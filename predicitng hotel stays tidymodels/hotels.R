library(tidyverse)
library(tidytuesdayR)
library(skimr)
library(scales)

tt <- tidytuesdayR::tt_load('2020-02-11')
hotels <- tt$hotels

head(hotels)

#mutate case when
hotel_stays <- hotels %>% filter(is_canceled ==0) %>% 
  mutate(children = case_when(children + babies > 0 ~ 'children',
                              TRUE ~ 'none'),
         required_car_parking_spaces = case_when(required_car_parking_spaces > 0 ~ 'parking',
                                                 TRUE ~ 'none')) %>% 
  select(-is_canceled, -reservation_status, -babies)

hotel_stays %>% count(children)

skim(hotel_stays)

#count number of chuldren by month and hotel type
hotel_stays %>% 
  mutate(arrival_date_month = factor(arrival_date_month,
         levels = month.name)) %>% 
  count(hotel, arrival_date_month, children) %>% 
  group_by(hotel, children) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(arrival_date_month, prop, fill = children))+
  geom_col(position = 'dodge')+
  scale_y_continuous(labels = percent_format())+
  facet_wrap(~hotel, nrow =2)



#count number of chuldren by required car parking spaces and hotel type
hotel_stays %>% 
  count(hotel, required_car_parking_spaces, children) %>% 
  group_by(hotel, children) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(required_car_parking_spaces, prop, fill = children))+
  geom_col(position = 'dodge')+
  scale_y_continuous(labels = percent_format())+
  facet_wrap(~hotel, nrow =2)

#pairs plot
library(GGally)
hotel_stays %>% 
  select(children, adr, required_car_parking_spaces, total_of_special_requests) %>% 
  ggpairs(mapping = aes(color = children))


#modelling with recipes
#create model df
#if something is a char, change to factor
hotels_df <- hotel_stays %>%
  select(
    children, hotel, arrival_date_month, meal, adr, adults,
    required_car_parking_spaces, total_of_special_requests,
    stays_in_week_nights, stays_in_weekend_nights
  ) %>%
  mutate_if(is.character, factor)

#train test split
library(tidymodels)
set.seed(1234)
hotels_split <- initial_split(hotels_df)
hotel_train <- training(hotels_split)
hotel_test<- testing(hotels_split)

#recipe:
#first declare what is being predicted, then add recipe steps
hotel_rec <- recipe(children~., data = hotel_train) %>% 
  #imbalanced dataset, downsample majority class
  step_downsample(children) %>% 
  #change all factors to dummy vars. not for outcomes (children)
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  #remove anything with 0 variance
  step_zv(all_numeric()) %>% 
  #center and scale 
  step_normalize(all_numeric()) %>% 
  #prep the recipe
  prep()

hotel_rec

#juice the recipe. retrieves output that has been downsampled
juice(hotel_rec)

juice(hotel_rec) %>% count(children)

#recipe can be applied to any dataset. (training and testing)
#use bake and apply to new data
test_proc <- bake(hotel_rec, new_data = hotel_test)


#training model
knn_spec <- nearest_neighbor() %>% 
  #KNN model. sensitive to scaling and standardizing. important that we normalized
  set_engine('kknn') %>% 
  #classifying
  set_mode('classification')


#fit KNN model on training data
#send the juiced recipe
knn_fit <- knn_spec %>% 
  fit(children~., data = juice(hotel_rec))
knn_fit

#decision tree model
#not very sensitive to centering and scaling
tree_spec <- decision_tree() %>% 
  set_engine('rpart') %>% 
  set_mode('classification')

#fit tree model
tree_fit <- tree_spec %>% 
  fit(children~., data = juice(hotel_rec))
tree_fit
#can view nodes, leaves, splits


#evaluate models
#monte carlo cross validation
#make 90 percent set, evaluate on 10 percent set
validation_splits <- mc_cv(juice(hotel_rec), prop = 0.9, strata = children)
#25 splits
validation_splits

#fit 25 times
#checking spec to see what engine and mode to fit with
#fitting a model saying children, explained by all predictors. 
#data that it is splitting on are the validation splits
#fits on split one, and evaluates on holdout 10%. repeat 25x
#feed model, then predictor, then splits, then save preds so we can make ROC curve
knn_res <- fit_resamples(
  knn_spec,
  children ~ .,
  validation_splits,
  control = control_resamples(save_pred = TRUE)
)

#get the metrics
knn_res %>% 
  collect_metrics()


#do same thing for tree model

tree_res <- fit_resamples(
  tree_spec,
  children ~ .,
  validation_splits,
  control = control_resamples(save_pred = TRUE)
)

#get the metrics
tree_res %>% 
  collect_metrics()


#visualize evaluations
#ROC plot
knn_res %>%
  unnest(.predictions) %>%
  mutate(model = "kknn") %>%
  bind_rows(tree_res %>%
              unnest(.predictions) %>%
              mutate(model = "rpart")) %>%
  group_by(model) %>%
  roc_curve(children, .pred_children) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_line(size = 1.5) +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  )

#confusion matrix
knn_conf <- knn_res %>%
  unnest(.predictions) %>%
  conf_mat(children, .pred_class)

knn_conf


#fit on test data
knn_fit %>%
  predict(new_data = test_proc, type = "prob") %>%
  mutate(truth = hotel_test$children) %>%
  roc_auc(truth, .pred_children)


#var imp
library(vip)
tree_res %>%
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 5)
