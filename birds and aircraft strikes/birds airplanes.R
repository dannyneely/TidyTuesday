library(tidyverse)
library(skimr)


train_raw <- read_csv('C:/Users/Denny/OneDrive - Bit Fry Game Studios, Inc/bitfry/R/tidytuesday/birds and aircraft strikes/train.csv'
  , guess_max = 1e5) %>%
  #creating a factor
  mutate(damaged = case_when(
    damaged > 0 ~ "damage",
    TRUE ~ "no damage"
  ))
test_raw <- read_csv('C:/Users/Denny/OneDrive - Bit Fry Game Studios, Inc/bitfry/R/tidytuesday/birds and aircraft strikes/test.csv')

#Wow skim!
skimr::skim(train)


#For numeric predictors, I often like to make a pairs plot for EDA.

library(GGally)

train_raw %>%
  select(damaged, incident_year, height, speed, distance) %>%
  ggpairs(columns = 2:5, aes(color = damaged, alpha = 0.5))


#For categorical predictors, plots like these can be useful. 
#Notice especially that NA values look like they may be informative so we likely don't want to throw them out.

train_raw %>%
  select(
    damaged, precipitation, visibility, engine_type,
    flight_impact, flight_phase, species_quantity
  ) %>%
  pivot_longer(precipitation:species_quantity) %>%
  ggplot(aes(y = value, fill = damaged)) +
  geom_bar(position = "fill") +
  facet_wrap(vars(name), scales = "free", ncol = 2) +
  labs(x = NULL, y = NULL, fill = NULL)

#using select variables for modelling
bird_df <- train_raw %>%
  select(
    damaged, flight_impact, precipitation,
    visibility, flight_phase, engines, incident_year,
    incident_month, species_id, engine_type,
    aircraft_model, species_quantity, height, speed
  )

#build model
library(tidymodels)

set.seed(123)
bird_folds <- vfold_cv(train_raw, v = 5, strata = damaged)
bird_folds

#evaluated on log loss, so let's create a metric set for that metric plus a few others for demonstration purposes.

bird_metrics <- metric_set(mn_log_loss, accuracy, sensitivity, specificity)

#step_novel accounts for if there is a new factor level in the testing set. will assign as 'new'
#step other pools infrequently occurring values into a category together. 
#step unknown assigns missing values (NA) to unknowns.
#step impute imputes all NA with median
#step_zv is zero variance filter, removes all features that only contain one value
bird_rec <- recipe(damaged ~ ., data = bird_df) %>%
  step_novel(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_zv(all_predictors())


bird_rec

#recipe has been setup, but not run yet.
#if you want to estiamte which categories need to be pooled together, or see where the NAs are replaced, then use prep()
#tells you what operations were performed with each step
bird_rec %>% prep()

#if you want to see what the results look like after prepping, use bake()
bird_rec %>% prep %>% bake(new_data = NULL)

#IMBALANCED DATASET
#a lot more times plane was not damaged, than damaged
bird_df %>% count(damaged)


library(baguette)
#create simple decision tree model,
#bag 25 times
bag_spec <-
  bag_tree(min_n = 10) %>%
  set_engine("rpart", times = 25) %>%
  set_mode("classification")
#create workflow for imbalanced dataset
imb_wf <-
  workflow() %>%
  add_recipe(bird_rec) %>%
  add_model(bag_spec)
#fit one time
imb_fit <- fit(imb_wf, data = bird_df)
#shows variable importance
imb_fit

#fit with resampling
#setup parallel backend
doParallel::registerDoParallel()
set.seed(123)
#for the 5 cross validation folds, 
imb_rs <-
  fit_resamples(
    imb_wf,
    resamples = bird_folds,
    metrics = bird_metrics
  )

collect_metrics(imb_rs)
#very low sensitivity (predicted positive, actually positive)
#means it could not predict damaged planes well, because of the imb dataset.

#account for class imbalance

library(themis)

#all categorical features must be made dummy
#smote does upsampling with KNN 
bal_rec <- bird_rec %>%
  step_dummy(all_nominal_predictors()) %>%
  step_smote(damaged)

bal_wf <-
  workflow() %>%
  add_recipe(bal_rec) %>%
  add_model(bag_spec)

set.seed(234)
bal_rs <-
  fit_resamples(
    bal_wf,
    resamples = bird_folds,
    metrics = bird_metrics
  )

collect_metrics(bal_rs)
#accuracy got worse, log-loss got worse.

