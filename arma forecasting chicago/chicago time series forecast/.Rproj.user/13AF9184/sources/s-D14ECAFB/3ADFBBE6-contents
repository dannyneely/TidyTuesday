library(tidyverse)
library(tidytuesdayR)


tt = tidytuesdayR::tt_load('2020-02-04')
attendance <- tt$attendance
games <- tt$games
standings <- tt$standings

#EDA
attendance_joined <- attendance %>% 
  left_join(standings, by  = c('year','team_name','team'))

#check each team and attendance with playoffs or not
#boxplot
attendance_joined %>% 
  filter(!(is.na(weekly_attendance))) %>% 
  mutate(team_name = fct_reorder(team_name, weekly_attendance)) %>% 
  ggplot(aes(weekly_attendance, team_name, fill = playoffs))+
  geom_boxplot(outlier.alpha = 0.5)


#histograms overlayed of each team and their margtin of victory
attendance_joined %>% 
  distinct(team_name, year, margin_of_victory, playoffs) %>% 
  ggplot(aes(margin_of_victory, fill = playoffs)) +
  geom_histogram(position = 'identity',alpha = 0.5)

#time series
attendance_joined %>%
  mutate(week = factor(week)) %>%
  ggplot(aes(week, weekly_attendance, fill = week)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.5) +
  labs(
    x = "Week of NFL season",
    y = "Weekly NFL game attendance"
  )

#build dataset for modelling
#each team has a week where they don't play, so remove those
attendance_df <- attendance_joined %>% 
  filter(!is.na(weekly_attendance)) %>% 
  select(
    weekly_attendance, team_name, year, week,
    margin_of_victory, strength_of_schedule, playoffs
  )

#train model
library(tidymodels)

#strata splits the data evenly according to one of my variables
#make the data even between the people who did and didn't make playoffs
attendance_split <- attendance_df %>% 
  initial_split(strata = playoffs )

attendance_train <- training(attendance_split)
attendance_test <- testing(attendance_split)


#first setup model spec. what kind of model do you want to train?
lm_spec <- linear_reg() %>% 
  #train lm
  set_engine(engine = 'lm') 

#fit model
lm_fit <- lm_spec %>% 
  fit(weekly_attendance~., data = attendance_train) 


#random forest model
#can set classification or regression
rf_spec <- rand_forest(mode = 'regression') %>% 
  #train rf
  set_engine(engine = 'ranger') 

#fit model
rf_fit <- rf_spec %>% 
  fit(weekly_attendance~., data = attendance_train) 


 #evaluate model

#predict 
results_train <- lm_fit %>%
  predict(new_data = attendance_train) %>%
  mutate(
    truth = attendance_train$weekly_attendance,
    model = "lm"
  ) %>%
  bind_rows(rf_fit %>%
              predict(new_data = attendance_train) %>%
              mutate(
                truth = attendance_train$weekly_attendance,
                model = "rf"
              ))

results_test <- lm_fit %>%
  predict(new_data = attendance_test) %>%
  mutate(
    truth = attendance_test$weekly_attendance,
    model = "lm"
  ) %>%
  bind_rows(rf_fit %>%
              predict(new_data = attendance_test) %>%
              mutate(
                truth = attendance_test$weekly_attendance,
                model = "rf"
              ))


#get metrics.
results_train %>%
  group_by(model) %>%
  rmse(truth = truth, estimate = .pred)

results_test %>%
  group_by(model) %>%
  rmse(truth = truth, estimate = .pred)


#lower rmse for RF model on training data, but gets much worse on testing data. 
#meaning we overfit the data.

#visualize results
results_test %>%
  mutate(train = "testing") %>%
  bind_rows(results_train %>%
              mutate(train = "training")) %>%
  ggplot(aes(truth, .pred, color = model)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.5) +
  facet_wrap(~train) +
  labs(
    x = "Truth",
    y = "Predicted attendance",
    color = "Type of model"
  )


#improve model
#use resampling on training data

#We can resample the training set to produce an estimate of how the model will perform. 
#Let’s divide our training set nfl_train into folds (say, 10) 
#and fit 10 versions of our model (each one trained on nine folds and evaluated on one heldout fold). 
#Then let’s measure how well our model(s) performs. 
#The function vfold_cv() creates folds for cross-validation, 
#the function fit_resamples() fits models to resamples such as these (to measure performance), 
#and then we can collect_metrics() from the result.

set.seed(1234)
nfl_folds <- vfold_cv(attendance_train, strata = playoffs)

rf_res <- fit_resamples(
  rf_spec,
  weekly_attendance ~ .,
  nfl_folds,
  control = control_resamples(save_pred = TRUE)
)

#gets metrics and summarises
rf_res %>%
  collect_metrics()


rf_res %>%
  unnest(.predictions) %>%
  ggplot(aes(weekly_attendance, .pred, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.5) +
  labs(
    x = "Truth",
    y = "Predicted game attendance",
    color = NULL
  )
