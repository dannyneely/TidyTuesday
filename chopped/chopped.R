library(tidytuesdayR)
library(tidyr)
library(scales)
library(tidyverse)
library(glue)
library(tidytext)
library(janitor)
chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')
head(chopped)

table(chopped$season)

#view episode ratings
chopped%>%
  ggplot(aes(x = episode_rating))+
  geom_histogram()

#one very low rating
chopped%>%
  select(episode_rating, episode_name)%>%
  arrange((episode_rating))

#rating of episodes over time
chopped%>%
  filter(!is.na(episode_rating))%>%
  ggplot(aes(x = series_episode, y = episode_rating))+
  geom_point(aes(color = factor(season)))+
  geom_text(aes(label = episode_name), check_overlap = TRUE)+
  geom_line(alpha = 0.5, color = 'gray')+
  theme(legend. = 'none')

#rating of episodes by season
chopped%>%
  filter(!is.na(episode_rating))%>%
  group_by(season)%>%
  summarize(n = n(), avg_rating = mean(episode_rating))%>%
  ggplot(aes(x = season, y = avg_rating))+
  geom_line()+
  geom_point(aes(size = n))

#top episiodes, with glue (season.episode)
chopped %>%
  arrange(desc(episode_rating))%>%
  head(20)%>%
  mutate(name = glue("{ season }.{ season_episode } { episode_name } "),
    name = fct_reorder(name, episode_rating))%>%
  ggplot(aes(x = episode_rating, y = name))+
  geom_point()

#ingredients
#combining 3 columns into 1 and naming each with the column header in a row
#separating each row of ingredients into its own row. now each ingredient is in a row with a course labelling what course it is in
#janitor clean names makes column names usable
ingredients = chopped%>%
  select(season, season_episode, series_episode, episode_rating, episode_name,appetizer:dessert)%>%
  pivot_longer(cols = c(appetizer, entree, dessert),
               names_to=  'course',
               values_to = 'ingredient')%>%
  separate_rows(ingredient, sep = ', ')%>%
  #fct_relevel orders the courses for the legend sequence
  mutate(course=  fct_relevel(course, c('appetizer', 'entree')))%>%
  janitor::clean_names()

ingredients%>%
  count(ingredient, sort = TRUE)

#count ingredients
#because there are so many, use fct_lump to take everything greater than a value, and anything less as 'other'
#reversing the order of course in the chart to go AED
#reversing the order in the legend to go AED
ingredients%>%
  count(course, ingredient,sort = TRUE)%>%
  filter(fct_lump(ingredient, 20, w=  n)!='Other')%>%
  mutate(ingredient = fct_reorder(ingredient, n, sum),
         course = fct_rev(course))%>%
  ggplot(aes(x = n, y = ingredient))+
  geom_col(aes(fill = course))+
  labs(title = 'most common ingredients by episode')+
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))

#facet count of ingredient by episode
ingredients%>%
  count(course, ingredient,sort = TRUE)%>%
  filter(fct_lump(ingredient, 20, w=  n)!='Other')%>%
  mutate(reorder_within(ingredient, n, course))%>%
  ggplot(aes(x = n, y = ingredient, fill = course))+
  geom_col()+
  facet_wrap(~course)



library(widyr)

#adds a n column to count how many episodes an ingredient appears
#find what ingredients frequently appear together through correlation
ingredient_correlations = ingredients%>%
  add_count(ingredient)%>%
  filter(n >=8)%>%
  pairwise_cor(ingredient, series_episode, sort = TRUE)

#count the number of times each ingredient appears with another
#for this dataset, not that relevant since ingredients are purposfully not paired together repeatedly
ingredients%>%
  add_count(ingredient)%>%
  filter(n >=8)%>%
  pairwise_count(ingredient, series_episode, sort = TRUE)

#do any ingredients ever appear with each other in the same course across episodes? NO
ingredients%>%
  add_count(ingredient)%>%
  filter(n >=8)%>%
  unite(episode_course, series_episode, course)%>%
  pairwise_count(ingredient, episode_course, sort = TRUE)

#make a network diagram from the correlations
library(ggraph)
library(tidygraph)
ingredient_correlations%>%
  head(75)%>%
  ggraph(layout = 'fr')+
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label = name), repel = TRUE)


#are there ingredients that got more popular in later seasons?
#this shows avg season appearance for ingredients, 6 earliest and latest appearing ingredients 
early_late_ingredients= ingredients%>%
  add_count(ingredient)%>%
  filter(n >=8)%>%
  group_by(ingredient)%>%
  summarise(first_season= min(season),
            avg_season = mean(season),
            last_season = max(season),
            n_appearances = n())%>%
  arrange(desc(avg_season))%>%
  slice(c(1:6), tail(row_number()))

#shows ingredients the show gave up on, and ones that started later
ingredients%>%
  add_count(ingredient)%>%
  filter(n >=8)%>%
  semi_join(early_late_ingredients, by = 'ingredient')%>%
  mutate(ingredient = fct_reorder(ingredient, season))%>%
  ggplot(aes(season,ingredient))+
    geom_boxplot()


#model what ingredients are popular?
#which ingredients lead to good or bad episode rating?

#makes dummies from if the ingredient was in the episode for a particular course
ingredients_wide = ingredients%>%
  add_count(ingredient)%>%
  filter(n >=8)%>%
  select(season,series_episode, episode_rating, course, ingredient)%>%
  mutate(value =  1)%>%
  pivot_wider(names_from = ingredient,
              values_from = value,
              values_fill = list(value= 0))%>%
  select(-series_episode)

#linear model
lm(episode_rating~season, data = ingredients_wide)%>%
  summary()

#build glm 
library(tidymodels)

#split data
set.seed(2020)
split_data = ingredients_wide%>%
  filter(!is.na(episode_rating))%>%
  initial_split()
training_set = training(split_data)
testing_set = testing(split_data)


#cross validate
cv_samples = training_set%>%
  vfold_cv(v = 2)
  
#random forest grid search
rf_spec = rand_forest(mode = 'regression',mtry = tune())%>%
  set_engine('ranger')

parameter_search = rf_spec%>%
  tune_grid(episode_rating~. , resamples = cv_samples)

#create model with optimal hyperparameters
model = rand_forest(mode = 'regression',mtry = 3, trees = 500)%>%
  set_engine('ranger')%>%
  fit(episode_rating~., training_set)
model

#pred and evaluate with rmse
predict(model, testing_set)%>%
  bind_cols(testing_set)%>%
  rmse(.pred, episode_rating)


#ML
library(tidymodels)
set.seed(2020)
train_test_split <- initial_split(chopped)
train = training(train_test_split)
test = testing(train_test_split)
mset = metric_set(rmse)


#dummy model:
sd(train$episode_rating, na.rm = T)

train_fold = vfold_cv(train, v = 20)

doParallel::registerDoParallel(cores = 4)
#linear model first

#tokenizing for judges: (NOT SIGNIFCANT THROUGH TUNING)
# step_mutate(judge = paste(judge1, judge2, judge3, sep = ';')) %>% 
#   step_tokenize(judge, token = 'regex', options = list(pattern = ';')) %>% 
#   #top 10 judge, filter out judges that are rare
#   step_tokenfilter(judge, max_tokens = 10) %>% 
#   #binary column for judge, now each judge is a 0 or 1 column for if they appeared
#   step_tf(judge) 
  
  
lm_rec <- recipe(episode_rating~series_episode + 
                   contestant1_info+contestant2_info+contestant3_info+contestant4_info, data = train) %>% 
  #once i find the df i like from tuning, replace tune()
  step_ns(series_episode, deg_free = 7) %>% 
  #tokenizing for contestants
  step_mutate(contestant_info = paste(contestant1_info, contestant2_info, contestant3_info, contestant4_info, sep = ' ')) %>% 
  step_tokenize(contestant_info) %>% 
  #remove stopwords from contestants
  step_stopwords(contestant_info) %>% 
  #top 10 judge, filter out judges that are rare
  step_tokenfilter(contestant_info, 3) %>% 
  #step_tf does binary column for contestant, now each judge is a 0 or 1 column for if they appeared
  step_tf(contestant_info) %>% 
  step_select(-c(contestant1_info, contestant2_info, contestant3_info, contestant4_info))

#view the recipe output
lm_rec %>% 
  prep() %>% 
  juice() 
#can see from new contestant tokenizing, do they have the words sous, executive, chef, etc.

#can add penalty term with glmnet
lm_model <- linear_reg(penalty = tune()) %>% 
  set_engine('glmnet') 

#create workflow (add recipe and model)
lm_wflow <- workflow() %>% 
  add_recipe(lm_rec) %>% 
  add_model(lm_model)

#check metrics
lm_wflow %>% 
  fit(train) %>% 
    extract_model() %>% 
    summary()
  

#tuning to find degrees of freedom, or max tokens to use
tuned <- lm_wflow %>% 
  tune_grid(train_fold,
            metrics = mset,
            grid = crossing(max_tokens = c(1,3,5,10,20),
                            penalty = 10 ^ seq(-7,-0.5,0.05)))

tuned %>% autoplot() 

#finalize recipe
lm_rec %>% 
  finalize_recipe(list(max_tokens = 3)) %>% 
  prep() %>% 
  juice()

#select best model
lm_wflow %>% 
  finalize_workflow(select_best(tuned)) %>% 
  fit(train) %>% 
  extract_model() %>% 
  tidy() %>% 
  filter(str_detect(term, 'info')) %>% 
  ggplot(aes(lambda, estimate, color = term)) +
  geom_line() +
  scale_x_log10()

#no improvement from judges, tiny improvements from 3 tokens on contestants, and food tokens do nothing



