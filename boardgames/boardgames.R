library(tidyverse)
library(scales)
library(tidymodels)
library(textrecipes)
boardgames <- read_csv("tidytuesday/boardgames/train.csv") %>% 
  unite(category, starts_with('category'), sep = ', ', na.rm=TRUE)
validation <- read_csv("tidytuesday/boardgames/test.csv") %>% 
  unite(category, starts_with('category'), sep = ', ', na.rm=TRUE)


View(boardgames)
str(boardgames)

#train test split
train_test_split <- initial_split(boardgames, 0.8)
train = training(train_test_split) %>% 
  filter(year >0)
test = testing(train_test_split)

#histogram of target variable (geek rating)
train %>% 
  ggplot(aes(geek_rating))+
  geom_histogram(bins = 50)

#boxplot with min players and rating
train %>% 
  ggplot(aes(min_players, geek_rating, group = min_players))+
  geom_boxplot()

#scatter with num votes and rating
train %>% 
  ggplot(aes(num_votes, geek_rating))+
  geom_point()+
  scale_x_log10()

#scatter with num votes and rating
summary(train$year)
train %>% 
  group_by(year) %>% 
  summarise(median_rating = median(geek_rating),
            n = n()) %>% 
  filter(n > 20) %>% 
  ggplot(aes(year, median_rating))+
  geom_line()

#categorical variables
train %>% 
  separate_rows(mechanic, sep = ', ') %>%
  count(mechanic, sort = TRUE)

sd(train$geek_rating)
#0.49 is a dummy predicitve model

#corplot with numeric features
cor_boardgmaes<- train %>% 
  select(where(is.numeric)) %>% 
  cor()
library(heatmaply)
heatmaply_cor(cor_boardgmaes)


#MODELLING!
#we're always using rmse for measuring
mset <- metric_set(rmse)
#create 10 folds, can train and test on different folds
train_fold = train %>% 
  vfold_cv(10)


#start with lin reg
#recipes are a lot of code, but great because they are powerful
#and you can add/remove components easily

#first create a spec for the model, then begin cleaning through recipe
#glmnet is penalized regression
lin_spec <- linear_reg() %>% 
  set_engine('glmnet')

#base 2 for easy interpretation, offset 1 if there were any 0s.
linear_recipe <- recipe(geek_rating~owned + num_votes+avg_time+
                        min_players+max_players+
                          year + mechanic + designer+
                          age + category, data = train) %>% 
  step_log(owned, num_votes,avg_time,base = 2, offset = 1) %>% 
  step_mutate(max_players = pmin(max_players, 30)) %>% 
  #for non-linear trend with year, add splines
  #use deg_free = tune() for finding optimal df that lowers rmse. we found 5
  step_ns(year, deg_free = 5) %>% 
  #take words from comma separated mechanic column. 
  #regex so token isn't a word, but the full mechanic
  step_tokenize(mechanic,designer, category,token = 'regex', options = list(pattern = ', ')) %>% 
  #top 20 tokens
  step_tokenfilter(designer, max_tokens = tune()) %>% 
  #create a column for each mechanic token. now one hot encoded for top 20
  step_tf(mechanic, designer,category)

#now fit model
lin_workflow <- workflow() %>% 
  add_recipe(linear_recipe) %>% 
  add_model(lin_spec)


#run the linear model on the 10 folds, and fit them
cv <- lin_workflow %>% 
  fit_resamples(train_fold)

#view metrics on each
cv %>% 
  collect_metrics()






#using grid if we want to find the df for year
tuned <- lin_workflow %>%
  tune_grid(train_fold,
            grid = crossing(max_tokens= c(3,10,30,100),
            penalty = 10^seq(-7,-2,.1)),
            metrics = mset)

#prep and juice means on the training data, actually apply the cleaning methods
#now mechanic is a list of tokens
linear_recipe %>% 
  prep() %>% 
  juice()

#plot to find optimal df
cv %>%
  autoplot()


#RANDOM FOREST
set.seed(2021)
train_fold5 = train %>% 
  vfold_cv(5)
rf_spec = rand_forest('regression',
            mtry = tune(),
            trees = tune()) %>% 
  set_engine('ranger') %>% 
  set_args(importance = 'impurity')

#useless features are bad for tree models. in linear models, no problem (worst will happen is overfit)
rf_rec <- recipe(geek_rating~owned + num_votes+avg_time+
                          min_players+max_players+
                          year + mechanic + designer, data = train) %>% 
  step_tokenize(mechanic,designer, category, token = 'regex', options = list(pattern = ', ')) %>% 
  #only taking top 10 mechanics and desginers and categories
  step_tokenfilter(designer, mechanic, category, max_tokens =tune()) %>% 
  #create a column for each mechanic token. now one hot encoded for top 20
  step_tf(mechanic, designer, category)

rf_wflow <- workflow() %>% 
  add_recipe(rf_rec) %>% 
  add_model(rf_spec)

#use tune() in the recipe for whatever you want to tune
rf_tune <- rf_wflow %>% 
  tune_grid(train_fold,
            grid = crossing(trees = 200,
              mtry = 20,
              max_tokens = c(10,30)),
            metrics = mset)
autoplot(rf_tune)

rf_tune %>% 
  collect_metrics() %>% 
  arrange(mean)


#USE ON TEST SET
rf_wflow %>% 
  #once you have the hyperparameters you want from tuning
  finalize_workflow(list(trees = 300, mtry =4)) %>% 
  #train on the training set, test on the test set
  last_fit(train_test_split) %>% 
  collect_metrics()

#ensemble method combining linear regression and random forest
#complicated code LOL using stacks package



##############################
#XGBoost
#this has the full working model and tuning workflow
set.seed(2021)
train_fold5 <- train %>% 
  vfold_cv(5)

#spec
xg_spec = boost_tree('regression',
                      mtry = 4,
                     trees = tune(),
                     learn_rate = tune(),
                      ) %>% 
  set_engine('xgboost')

#recipe
xg_rec <- recipe(geek_rating~owned + num_votes+avg_time+
                   +max_players+ year +age, data = train) 


#workflow
xg_wflow <- workflow() %>% 
  add_recipe(xg_rec) %>% 
  add_model(xg_spec)

#tuning
#use tune() in the recipe for whatever you want to tune
xg_tune <- xg_wflow %>% 
  tune_grid(train_fold,
            grid = crossing(trees = c(50,100,150),
            learn_rate = c(0.05, 0.1, 0.15, 0.2)),
            metrics = mset
            )



autoplot(xg_tune)

#finalize on the workflow with hyperparameters
xg_test <- xg_wflow %>% 
  finalize_workflow(list(trees = 3000,
                    learn_rate = 0.01)) %>% 
  last_fit(train_test_split)

#0.172 rmse 
xg_test %>% 
  collect_metrics() 

#fit model to full data
xg_fit <- xg_wflow %>% 
  finalize_workflow(list(trees = 3000,
                         learn_rate = 0.01)) %>% 
  fit(boardgames)
  
#then predict on the holdout set (untouched)
#this combines the predictions from using the model and the game id, for submission to kaggle
xg_fit %>% 
  predict(validation) %>% 
  bind_cols(validation %>% select(game_id)) %>% 
  select(game_id, geek_rating = .pred)


  



