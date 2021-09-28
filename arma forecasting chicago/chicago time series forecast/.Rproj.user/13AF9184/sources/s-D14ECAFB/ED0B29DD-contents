library(tidyverse)
library(tidytuesdayR)
library(skimr)
library(scales)

tt <- tidytuesdayR::tt_load('2020-02-18')
food_consumption <- tt$food_consumption

head(food)


library(countrycode)
library(janitor)

#preprocessing
food <- food_consumption %>%
  select(-co2_emmission) %>%
  pivot_wider(
    names_from = food_category,
    values_from = consumption
  ) %>%
  clean_names() %>%
  mutate(continent = countrycode(
    country,
    origin = "country.name",
    destination = "continent"
  )) %>%
  mutate(asia = case_when(
    continent == "Asia" ~ "Asia",
    TRUE ~ "Other"
  )) %>%
  select(-country, -continent) %>%
  mutate_if(is.character, factor)

food

#pairs pot 
library(GGally)
ggscatmat(food, columns = 1:11, color = "asia", alpha = 0.7)



#modelling
#tuning hyperparameters
library(tidymodels)

#bootstrapping draws with replacement
set.seed(1234)
food_boot <- bootstraps(food, times = 30)
food_boot


#mtry - the number of predictors that would be included with each split when creating the tree models
#trees - want to make sure you have enough (not big impact)
#min_n - how many data points in a node before splitting
rf_spec <- rand_forest(
  mode = "classification",
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_engine("ranger")

rf_spec


#this is training a RF model of every combination of mtry and min_n, 30 times (from bootstrapped samples)
rf_grid <- tune_grid(
  rf_spec,
  asia~., 
  resamples = food_boot
  )

#tested from 2 mtry (minimum number of predictors) to 11 mtry (all predictors)
#we had 130 datapoints, adn tested from 4-37
rf_grid %>% 
  collect_metrics()

#tell me the best model, by specified metric
rf_grid %>% 
  show_best("roc_auc")
