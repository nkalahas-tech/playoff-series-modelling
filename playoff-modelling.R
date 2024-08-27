
#install.packages("ggplot2", repos = "http://cran.us.r-project.org")
#install.packages("plotly", repos = "http://cran.us.r-project.org")

library(ggplot2)
library(plotly)


#all of the regular season data for all teams
regular_season_data <- team_data %>% filter(gametype == 2)
regular_season_data

#getting the mean(aver age) of their team stats based on season and team
average_stats <- regular_season_data %>%
  group_by(season, off_team) %>%
  summarise(across(fg2made:shotattemptpoints,  mean, na.rm = TRUE)) %>%
  rename(team = off_team)
average_stats

# Calculate wins and losses for each team based on season and team name
team_records <- regular_season_data %>%
  # Wins for offensive team
  group_by(season, off_team) %>%
  summarise(wins = sum(off_win), losses = sum(def_win)) %>%
  rename(team = off_team) %>%
  # Wins for defensive team
  bind_rows(
    regular_season_data %>%
      group_by(season, def_team) %>%
      summarise(wins = sum(def_win), losses = sum(off_win)) %>%
      rename(team = def_team)
  ) %>%
  # Summarize total wins for each team
  group_by(season, team) %>%
  summarise(total_wins = sum(wins)/2, total_losses = sum(losses)/2)


# Display the number of wins for each team during the regular season
print(team_records)

#creation of four crucial features (Offensive rating, defensive rating, offensive eFG perent, and defensive eFG percent)

#offensive team rating 
ORTG_df_new <- regular_season_data %>%
  group_by(season, off_team, off_team_name) %>%
  rename(team = off_team) %>%
  arrange(season) %>%
  summarise(summed_offpoints = sum(points),
            summed_offpossessions = sum(possessions),
            ORTG = summed_offpoints/(summed_offpossessions/100)) %>%
  rename(team_name = off_team_name)
ORTG_df_new

#defensive team rating 
DRTG_df_new <- regular_season_data %>%
  group_by(season, def_team, def_team_name) %>%
  rename(team = def_team) %>%
  arrange(season) %>%
  summarise(summed_defpoints = sum(points),
            summed_defpossessions = sum(possessions),
            DRTG = summed_defpoints/(summed_defpossessions/100))%>%
  rename(team_name = def_team_name)
DRTG_df_new

#combining all the offensive teams stats from the whole seasons 
combined_team_data_off2 <- team_data %>%
  group_by(season, off_team) %>%
  rename(team= off_team) %>%
  summarise(
    total_fgmade = sum(fgmade),
    total_fg3made = sum(fg3made),
    total_fgattempted = sum(fgattempted)
  )

#Offensive eFG column creation
combined_team_data_off2$offensive_eFG_percent = (combined_team_data_off2$total_fgmade + 0.5*combined_team_data_off2$total_fg3made) * 100 / combined_team_data_off2$total_fgattempted

#combining all the defensive teams stats from the whole seasons 
combined_team_data_def2 <- team_data %>%
  group_by(season, def_team) %>%
  rename(team=def_team) %>%
  summarise(
    total_fgmade_allowed = sum(fgmade),
    total_fg3made_allowed = sum(fg3made),
    total_fgattempted_allowed = sum(fgattempted)
  )

#Defensive eFG column creation
combined_team_data_def2$defensive_eFG_percent = (combined_team_data_def2$total_fgmade_allowed + 0.5*combined_team_data_def2$total_fg3made_allowed) * 100 / combined_team_data_def2$total_fgattempted_allowed


#install all the necessary packages needed for models 
#install.packages("randomForest", dependencies = T)
#install.packages("caret")
library(caret)
library(randomForest)

#installing glue for string interpolation
#install.packages("glue")
library(glue)

#getting the playoff teams head to head record against each other in the regular season depending on which playoff year we are on
h2h_record <- regular_season_data %>%
  mutate(series_id = if_else(off_team < def_team, 
                             paste(off_team, def_team, sep = "-"), 
                             paste(def_team, off_team, sep = "-"))) %>%
  group_by(season, off_team, series_id) %>%
  rename(team = off_team) %>%
  summarize(total_win_againstopponent_regszn = sum(off_win), total_loss_againstopponent_regszn = sum(def_win))
h2h_record

#creation of our datasets and defining multiple features for our model to use (only using past round 1 series instead of other rounds as our eventual goal is to predict round 1 series for 2023)
distinct_round1_seriess <- round1_series %>%
  group_by(season, series_id, off_team, def_team) %>%
  summarise(total_games_in_series = n(), wins_in_series = sum(off_win), games_at_home = sum(off_home, na.rm = TRUE)) %>%
  rename(team = off_team) %>%
  left_join(team_records, by=c("season", "team")) %>%
  left_join(average_stats, by=c("season", "team")) %>%
  left_join(ORTG_df_new, by=c("season", "team")) %>%
  left_join(DRTG_df_new, by=c("season", "team")) %>%
  left_join(combined_team_data_off2, by = c("season", "team"))%>%
  left_join(h2h_record, by = c("season", "team", "series_id")) %>%
  mutate(NRTG = ORTG - DRTG, winner = ifelse(wins_in_series == 4, TRUE, FALSE))
distinct_round1_seriess

##########################################################################################################################################################################################################################################
#Prediction for who the Winner will be in all the previous playoff years before 2023 (the data that is already given)

#splitting the data into training and testing data 
set.seed(123)
trainIndex <- createDataPartition(distinct_round1_seriess$winner, p = .6, 
                                  list = FALSE, 
                                  times = 1)
trainData <- distinct_round1_seriess[trainIndex, ]
testData <- distinct_round1_seriess[-trainIndex, ]


#Using logistic regression for guessing/predicting the winner (only two classes to predict from)
logitModel <- glm(winner ~ total_wins + total_losses + NRTG + offensive_eFG_percent, data = trainData, family = binomial)
summary(logitModel)

#using random forest trees to predict winner 
rfmodel <- randomForest(formula = winner ~ total_wins + total_losses + NRTG, data = trainData, ntree = 500, mtry = 3, importance = TRUE)

#adding columns to show the predicted probabilities for each team to win the series 
testData$predicted_prob_with_lg <- predict(logitModel, newdata = testData, type = "response")
testData$predicted_prob_with_rf <- predict(rfmodel, newdata = testData, interval = "confidence")


#outputting the string showing what chance each team has to win the series in another dataframe 
output <- testData %>%
  mutate(opposing_team_probability = 1 - predicted_prob_with_lg, 
         final_output = glue("{team} has a {predicted_prob_with_lg*100} chance to win and {def_team} has a {opposing_team_probability* 100} chance."))


##########################################################################################################################################################################################################################################

#Prediction for Amount of Games the Series Goes 

#Using random forest (a multi-class classification model)
#splitting the data into training and testing datasets and giving indices 
trainIndex2 <- createDataPartition(distinct_round1_seriess$total_games_in_series, p = .8, 
                                   list = FALSE, 
                                   times = 1)
trainData2 <- distinct_round1_seriess[trainIndex2, ]
testData2 <- distinct_round1_seriess[-trainIndex2, ]

#omitting all NA values from the training data 
trainData2 <- na.omit(trainData2)
testData2 <- na.omit(testData2)

#converting all types to numeric if it can be a factor 
trainData2 <- trainData2 %>%
  mutate_if(is.factor, as.numeric)
testData2 <- testData2 %>%
  mutate_if(is.factor, as.numeric)

#the predicted amount of games the series goes to (creation of that column)
rfModelGames <- randomForest(as.factor(total_games_in_series) ~ ., data = trainData2, ntree = 100)
testData2$predicted_num_games <- predict(rfModelGames, newdata = testData2)
confusionMatrix(as.factor(testData2$predicted_num_games), as.factor(testData2$total_games_in_series))

#around 71 percent accuracy with the predicted number of games using random forest and all of the features in the dataset


######For the 2023 playoffs first round ONLY(we do not know about future rounds yet)######################################################################################################################################################################################################################################

#reading in the 2023 first round playoffs csv file 
current_season_playoffs<- read_csv("~/Documents/2023season_playoffs.csv")

#joining all the previous dataframes (team records, team offensive rating, team defensive rating, head to head records, combined team data offensively, combined team data defensively) into one bssed on common columns 
current_season_playoffs <- current_season_playoffs %>%
  group_by(season, off_team) %>%
  rename(team = off_team) %>%
  left_join(team_records, by=c("season", "team")) %>%
  left_join(average_stats, by=c("season", "team")) %>%
  left_join(ORTG_df_new, by=c("season", "team")) %>%
  left_join(DRTG_df_new, by=c("season", "team")) %>%
  left_join(combined_team_data_off2, by = c("season", "team")) %>%
  left_join(h2h_record, by = c("season", "team", "series_id")) %>%
  mutate(NRTG = ORTG - DRTG)
current_season_playoffs


#predicting probabilities and the winner with the already existing logistic regression model 
current_season_playoffs$predicted_prob_with_logr <- predict(logitModel, newdata = current_season_playoffs, type = "response")
current_season_playoffs <- current_season_playoffs %>%
  group_by(series_id) %>%
  mutate(predicted_winner_with_logr = case_when(is.na(lag(predicted_prob_with_logr)) ~ predicted_prob_with_logr > lead(predicted_prob_with_logr), TRUE ~ predicted_prob_with_logr > lag(predicted_prob_with_logr)))


#predicting probabilities and the winner with the already made random forest model 
current_season_playoffs$predicted_prob_with_rf <- predict(rfmodel, newdata = current_season_playoffs, interval = "confidence")
current_season_playoffs <- current_season_playoffs %>%
  group_by(series_id) %>%
  mutate(predicted_with_rf = case_when(is.na(lag(predicted_prob_with_rf)) ~ predicted_prob_with_rf > lead(predicted_prob_with_rf), TRUE ~ predicted_prob_with_rf > lag(predicted_prob_with_rf)))



###########Observation 1: ###############
#The model starts by reading the current season playoff data and joining it with various historical and statistical data sets (team records, offensive and defensive ratings, head-to-head records, and combined team data) based on common columns such as season, team, and series ID.
#The combined data includes important metrics like Offensive Rating (ORTG), Defensive Rating (DRTG), and Net Rating (NRTG, calculated as ORTG - DRTG).
#The logistic regression model (logitModel) is used to predict the probabilities of each team winning a game based on the combined dataset.
#The probabilities are calculated, and a decision is made for each series by comparing the probabilities for each team.
#Similarly, a random forest model (rfmodel) is used to predict the winning probabilities.
#This model also outputs the predicted probabilities, which are then compared to determine the predicted winner for each series.
#For both models, the predicted probabilities are compared within each series to identify the team with the higher probability of winning.
#The predictions are then used to label the predicted winner for each series.

###########Observation 2: ################
# My model's strengths were that it had comprehensive data integration, used multiple models, and had great insights. Firstly, I believe that the model leverages a wide array of data accounting for everything, including team records, advanced metrics, and head-to-head records, providing a holistic view of each team's performance.
#With the usage of multiple models, using both logistic regression and random forest models, the approach combines the strengths of both linear and non-linear predictive modeling techniques, potentially increasing the robustness of predictions.
#It also used contextual insights using important metrics like Offensive and Defensive Ratings, and Net Ratings provide context-specific insights that are crucial for predicting game outcomes.
#My model's weaknesses were the following: 
#Data Dependency: The accuracy of the model heavily depends on the quality and completeness of the historical data. Missing or inaccurate data can significantly impact predictions.
#Overfitting: The random forest model, while powerful, is prone to overfitting, especially with limited data. This means it might perform well on historical data but less so on new, unseen data.
#Complexity and Interpretability: The random forest model is a black-box model, making it difficult to interpret and understand the decision-making process compared to the logistic regression model.

###########Observation 3: ##################
#If given more time and data, I would focus on incorporating more data from additional seasons, including player-level statistics and more granular in-game data to improve the model’s predictive power. Model tuning is also important as I have to perform extensive hyperparameter tuning for the random forest model to minimize overfitting and improve generalization to new data.
#I would have also combined predictions from multiple models (beyond logistic regression and random forest) using ensemble techniques to potentially improve prediction accuracy.
#If there was a possiblity, I would have also created new features that capture more complex interactions between players and teams, such as player injuries, in-game strategies, and coaching styles.

########### Observation 4: ###################
p <- ggplot(current_season_playoffs, aes(x = current_season_playoffs$series_id, y = current_season_playoffs$predicted_prob_with_logr, group = team, color = team)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "NBA Playoff Teams' Chances of Advancing to Next Round",
       x = "Playoff Series",
       y = "Probability of Advancing",
       color = "Team") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert ggplot to plotly for interactivity
p <- ggplotly(p)
print(p)

