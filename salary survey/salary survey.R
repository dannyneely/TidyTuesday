library(tidyverse)
library(scales)
library(tidytuesdayR)
library(lubridate)
theme_set(theme_light())
tt =tt_load('2021-05-18')
tt

#sort ages by least to greatest, including ' under 18'
#sort experiences with
#remove spaces in titles with hyphens
#fct_collapse brings two levels together since we have 2 categories that prefer not to answer
#coalasece replaces missing values
survey <- tt$survey %>%
  mutate(timestamp = mdy_hms(timestamp),
         age_category = fct_relevel(fct_reorder(how_old_are_you, parse_number(how_old_are_you)), "under 18"),
         experience_overall = str_replace(overall_years_of_professional_experience, " - ", "-"),
         experience_overall = fct_reorder(experience_overall, parse_number(experience_overall)),
         experience_in_field = str_replace(years_of_experience_in_field, " - ", "-"),
         experience_in_field = fct_reorder(experience_in_field, parse_number(experience_in_field))) %>%
  mutate(gender = fct_collapse(coalesce(gender, "Other or prefer not to answer"), "Other or prefer not to answer" = c("Other or prefer not to answer", "Prefer not to answer")),
         race = fct_lump(coalesce(race, "Other"), 4)) 

#check age groups
survey %>% 
  count(age_category)

####################
#EDA DIFFERENT SALARIES
######################


#survey USD
survey_usd <- survey %>% 
  filter(currency == 'USD',
         annual_salary > 10000,
         annual_salary < 1e6) %>% 
  mutate(state = str_remove(state, ", .*")) #only take the first state that people mention in multiple answers
#hist
survey_usd %>% 
  ggplot(aes(annual_salary))+
  geom_histogram()+
  scale_x_log10(labels= dollar_format())

#function to make count by group, and median
summarize_salary <- function(tbl){
  tbl %>% 
  summarise(n = n(),
            median_salary = median(annual_salary)) %>% 
    arrange(desc(n))
}

plot_categorical <- function(tbl, column, n_levels = 9, reorder = TRUE) {
  lumped_tbl <- tbl %>%
    filter(!is.na({{ column }})) %>%
    mutate({{ column }} := fct_lump({{ column }}, n_levels))

    #some times you don't want to reorder, and instead go by age bins order
  if (reorder) {
    lumped_tbl <- lumped_tbl %>%
      mutate({{ column }} := fct_reorder({{ column }}, annual_salary))
  }
  #group by, summarise, and barplot
  lumped_tbl %>%
    group_by({{ column }}) %>%
    summarize_salary() %>%
    ggplot(aes(median_salary, {{ column }})) +
    geom_col() +
    scale_x_continuous(labels = dollar_format()) +
    labs(x = "Median salary")
}

#check each feature to see how it affects salary
#barplot by state
survey_usd %>%
  plot_categorical(state)
#barplot by industry
survey_usd %>%
  plot_categorical(industry)
#barplot by job title
survey_usd %>%
  plot_categorical(job_title, n_levels = 10)
#barplot by experience
survey_usd %>%
  plot_categorical(experience_overall, reorder = FALSE)
#barplot by experience in field
survey_usd %>%
  plot_categorical(experience_in_field, reorder = FALSE)
#barplot by age
survey_usd %>%
  plot_categorical(age_category, reorder = FALSE)
#barplot by gender
survey_usd %>%
  plot_categorical(gender)
#barplot by race
survey_usd %>%
  plot_categorical(race, n_levels = 4)


##############
#ANOVA
#############
#check each feature to see how much variance is explained
#there's variance within each group and variance between each group
#what percentage of the variance does each group explain?
survey_usd %>% 
  filter(!is.na(experience_overall)) %>% 
  ggplot(aes(annual_salary, experience_overall))+
  geom_boxplot()+
  scale_x_log10()

#log2 is most interpretable. the log transformations do not change the models, only how to interpret the coefficients.
#we want to look at statistical signficance relative to the intercept
#how confident are we that someone with 5 yrs exp has a greater salary than someone with 1 year exp?

library(broom)
lm(log2(annual_salary)~ experience_overall, data = survey_usd) %>% 
  summary()
#R^2 is 0.06, 6 percent variance explained by experience overall

lm(log2(annual_salary)~ experience_in_field, data = survey_usd) %>% 
  summary()
#R^2 is 0.11, 11 percent variance explained by experience in field
#R^2 adj takes into account number of categories, but 21000 data points won't change this so much. which is why both R^2 are so close

#compares based on first category in feature (administrative assistant for job title)
#administrative assistant is the lowest making job title (at 10 levels), so any other job titles are looking at how much more salary they get
#a 1 in 'Estimate' means twice the salary as an administrative assistant
survey_usd %>% 
  mutate(job_title = fct_lump(job_title, 10)) %>% 
lm(log2(annual_salary)~ job_title, data = .) %>% 
  summary()

#combine all groups to look at interactions
#variance explained by each feature
survey_usd %>% 
  mutate(job_title = fct_lump(job_title, 10),
         state = fct_lump(state, 10),
         industry = fct_lump(industry, 10)) %>% 
  lm(log2(annual_salary)~ job_title + industry+state+experience_in_field+gender+race, data = .) %>% 
  anova() %>% 
  tidy() %>% 
  mutate(pct_variance = sumsq/sum(sumsq))


########################
#machine learning
########################
#tidymodels
library(tidymodels)

set.seed(2021)
#train test split
survey_usd_split <- initial_split(survey_usd)
survey_usd_train <- training(survey_usd_split)
survey_usd_test<- testing(survey_usd_split)

#create recipe
#target variable and what features
#mutate salary to be log transformed
#step_other - if rate of occurrence for a feature is lower than the threshold - throw in 'Other'
#replace missing values as 'unknown'
#dummify categorical variables
rec <- survey_usd_train %>%
  recipe(annual_salary ~ job_title + state + city + experience_in_field + gender + race + industry +
           highest_level_of_education_completed) %>%
  step_novel(all_nominal()) %>%
  step_unknown(job_title, industry, state, city, highest_level_of_education_completed) %>%
  step_log(annual_salary, base = 2) %>%
  step_other(job_title, industry, state, city, threshold = tune())



#fit linear model
training_cv <- vfold_cv(survey_used_train)

#create plausible grid of values for thresholds
#for each fold, run one of each of the 5 thresholds
threshold_grid <- crossing(threshold = c(.001, .003, .01, .03, .1))

linear_model_cv_tune_threshold <- linear_reg() %>%
  set_engine("lm") %>%
  tune_grid(rec, training_cv, grid = threshold_grid)

linear_model_cv_tune_threshold %>%
  autoplot() +
  scale_x_log10()

#update recipe with new parameters to change
rec_with_threshold <- rec %>%
  finalize_recipe(list(threshold = .001))



doParallel::registerDoParallel(cores = 4)

rf_grid <- crossing(mtry = c(2, 3, 4, 5),
                    trees = c(30, 100, 200, 300))

training_cv5 <- vfold_cv(survey_usd_train, v = 5)

linear_model_cv <- linear_reg() %>%
  set_engine("lm") %>%
  fit_resamples(rec_with_threshold, training_cv5)

#create random forest model, compare it to linear
#tune for the number of splits and number of trees
random_forest_tune <- rand_forest(mode = "regression", mtry = tune(), trees = tune()) %>%
  set_engine("ranger") %>%
  tune_grid(rec_with_threshold, training_cv5, grid = rf_grid,
            control = control_grid(verbose = TRUE))

linear_model_metrics <- linear_model_cv %>%
  collect_metrics()
linear_model_metrics

random_forest_tune %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  ggplot(aes(trees, mean, color = factor(mtry))) +
  geom_line() +
  geom_hline(yintercept = linear_model_metrics$mean[2], lty = 2) +
  labs(y = "R Squared",
       color = "# of splits",
       x = "# of trees")
