# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!
library(janitor)
tuesdata <- tidytuesdayR::tt_load('2020-12-15')
tuesdata <- tidytuesdayR::tt_load(2020, week = 51)


ninja_warrior <- tuesdata$ninja_warrior%>%
mutate(round_stage= str_remove(round_stage,' \\(Regional/City\\)'))

janitor::clean_names(ninja_warrior)

head(ninja_warrior)
str(ninja_warrior)

library(tidyverse)
library(scales)

#find number of missing in each column
sapply(ninja_warrior, function(x) sum(is.na(x)))

table(ninja_warrior$season)
table(ninja_warrior$location)
table(ninja_warrior$round_stage)
table(ninja_warrior$obstacle_name)
table(ninja_warrior$obstacle_order)

ninja_warrior%>%
  count(round_stage, sort = TRUE)

#counts of each season
ggplot(ninja_warrior, aes(x = season))+
  geom_bar(fill = 'lightblue', bins = 15)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  labs(x = "Season",
       y = "Deaths",
       fill = "",
       title = "Counts of each season")+
  theme_minimal()

#counts of each round stage
ggplot(ninja_warrior, aes(x = round_stage))+
  geom_histogram(stat = 'count',fill = 'lightblue')+
  labs(x = "Round Stage",
       y = "Deaths",
       fill = "",
       title = "Counts of each round stage")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

#counts of each obtacle in qualifying and finals
#log odds to see if certain obstacles appear more in qualifying or finals
#plot with factor reordered 
library(tidylo)
ninja_warrior%>%
  filter(round_stage %in% c('Qualifying', 'Finals'))%>%
  count(round_stage, obstacle_name, sort = TRUE)%>%
  bind_log_odds(round_stage, obstacle_name,n)%>%
  arrange(desc(log_odds_weighted))%>%
  filter(round_stage == 'Finals')%>%
  top_n(12, abs(log_odds_weighted))%>%
  mutate(obstacle_name = fct_reorder(obstacle_name,log_odds_weighted))%>%
  ggplot(aes(log_odds_weighted, obstacle_name))+
  geom_col()


#see if certain obstacles show up more often in particular rounds 
 ninja_warrior%>%
  filter(round_stage %in% c('Qualifying','Finals'))%>%
   unite(season_location, season, location, remove = FALSE)%>%
   group_by(round_stage)%>%
   mutate(total_rounds = n_distinct(season_location))%>%
   group_by(round_stage, obstacle_name)%>%
  summarize(avg_position = mean(obstacle_order),
            n_rounds = n(),
            pct_rounds = n_rounds / first(total_rounds))%>%
   arrange(desc(n_rounds))

 
 #What does a typical course look like? Find most common obstacles per round
 ninja_warrior%>%
   filter(round_stage == 'Finals')%>%
   add_count(obstacle_order, round_stage, name = 'round_stage_total')%>%
   filter(round_stage_total >=10)%>%
   mutate(obstacle_name = fct_lump(obstacle_name, 10))%>%
  mutate(obstacle_name = fct_reorder(obstacle_name, obstacle_order))%>%
  count(round_stage_total,obstacle_name, obstacle_order)%>%
   ggplot(aes(obstacle_order,n/round_stage_total, fill = obstacle_name))+
   geom_col(width = 1)+
   scale_x_continuous(breaks = 1:10)+
   scale_y_continuous(labels = percent)+
   labs(x = 'Step',
        y = '% of courses',
        fill = 'Obstacle')
 

#common obstacles per round boxplot
 library(glue)
 ninja_warrior%>%
   filter(round_stage == 'Finals')%>%
   add_count(obstacle_order, round_stage, name = 'round_stage_total')%>%
   filter(round_stage_total >=10)%>%
   add_count(obstacle_name, name = 'obstacle_total')%>%
   #mutate(obstacle_name = glue('{ obstacle_name } ({ obstacle_total})'))%>%
   mutate(obstacle_name = fct_lump(obstacle_name, 10))%>%
   mutate(obstacle_name = fct_reorder(obstacle_name, obstacle_order))%>%
   count(round_stage_total,obstacle_name, obstacle_order)%>%
   ggplot(aes(obstacle_order, obstacle_name))+
   geom_boxplot()+
   scale_x_continuous(breaks = 1:10)+
   labs(x = 'Step',
        y = '% of courses',
        fill = 'Obstacle')
