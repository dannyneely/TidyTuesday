#predict how many points an athlete will score

library(tidyverse)
library(tidytuesdayR)
library(skimr)

tt <- tt_load("2020-03-03")
game_goals <- tt$game_goals
season_goals <- tt$season_goals
top250 <- tt$top_250

skim(game_goals)


#trend of shots taken and goals scored
#more shots taken, more goals scored
game_goals %>% 
  group_by(player) %>% 
  summarise(total_goals = sum(goals),
            total_assists = sum(assists),
            total_shots = sum(shots)) %>% 
  ggplot(aes(total_shots, total_goals))+
  geom_point()+
  geom_smooth()

#count each player and their aggregate their stats
#each player and their shots per goal
game_goals %>% 
  group_by(player) %>% 
  summarise(total_goals = sum(goals),
            total_assists = sum(assists),
            total_shots = sum(shots)) %>% 
  mutate(score_pct = total_shots / total_goals) %>% 
  arrange(desc(score_pct)) %>% 
  ggplot(aes(player, score_pct, size = total_goals))+
  geom_point()

#analyze bivariate relationships between numeric features
game_goals %>% 
  select(assists, goals, shots) %>% 
  gather(key = 'key', value = 'value', -goals) %>% 
  ggplot(aes(value, goals, color = key))+
  geom_jitter(alpha=  0.5)+
  facet_wrap(~key, scales=  'free' )


#too many players to build (42 models) so we bin by 10 and classify
#rank every 42 players (1-42)
model_data <- game_goals %>% 
  group_by(player) %>% 
  mutate(total_goals = sum(goals)) %>% 
  ungroup() %>% 
  #rank
  mutate(rank = dense_rank(-total_goals),
         #rank every 10
         rank = floor(rank/10)+1) %>% 
#clean age (years and days
  separate(age, into = c('years','days')) %>% 
  mutate(age = as.integer(years)+as.numeric(days)/365) %>% 
  select(-years,-days,-total_goals, -season) %>% group_by(player) %>% 
  group_by(rank) %>% 
  #standardize x - xbar
  mutate(standardized_game_num = game_num - mean(game_num)) %>% 
  ungroup() %>% 
  select(-player, -game_num) %>% 
  select(rank, goals, everything())


#4 models for top 40% 4 bins   
library(broom)
#when you do + 0, you set intercept specifically for 0
#dummy variables have contribution to the intercetp
model_data %>% 
  group_by(rank) %>% 
  do(linear_model = lm(goals~. + 0, data = .)) %>% 
  tidy(linear_model) %>% 
  filter(p.value < .05) %>% 
  ggplot(aes(reorder(term, estimate), estimate, fill = rank)) +
  geom_col()+
  facet_wrap(~rank, scales=  'free_y')
  



          