library(tidytuesdayR)
library(tidyverse)
library(scales)
library(tidytext)
theme_set(theme_light())
coffee <- tt_load('2020-07-07')$coffee_ratings%>%
  filter(total_cup_points > 0)
head(coffee) #tons of columns

#EDA count columns
coffee%>%
  count(species)

#barplot of top producing countries
coffee%>%
  count(country = fct_lump(country_of_origin,12), sort = TRUE)%>%
  filter(!is.na(country))%>%
  mutate(country = fct_reorder(country, n))%>%
  ggplot(aes( n,country))+
  geom_col()



coffee%>%
  count(producer, sort = TRUE) #lots of NA

#mean of columns ignoring NA
coffee_ratings = coffee%>%
  summarise(across(everything(), ~mean(!is.na(.))))%>% #take mean of each column, ignoring NA
  gather()%>% #lets you sort in view
  View()


coffee_lumped = coffee%>%
  filter(!is.na(variety),
         total_cup_points>10)%>% #must have overall score greater than 10
  mutate(variety = fct_lump(variety, 12), sort = TRUE)#only have less than 10 varieties

#boxplot- variety by score
coffee_lumped%>%
  mutate(variety = fct_reorder(variety, total_cup_points))%>%
  ggplot(aes(total_cup_points, variety))+
  geom_boxplot()

#boxplot - country by score
coffee%>%
  mutate(country = fct_lump(country_of_origin,12),
country = fct_reorder(country, total_cup_points))%>%
  ggplot(aes( total_cup_points,country))+
  geom_boxplot()

#histogram
coffee_lumped%>%
  ggplot(aes(total_cup_points, fill = variety))+
  geom_histogram(binwidth = 2)+
  facet_wrap(~variety, scale='free_y')+
  theme(legend.position = 'none')

#ridge plot - very cool!
coffee_lumped%>%
  ggplot(aes(x = total_cup_points, y = variety, group = variety, fill = variety))+
  geom_density_ridges()+
  theme(legend.position = 'none')



#lollipop plot, what features matter most for rating
coffee %>% 
  select(aroma:cupper_points) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() %>% 
  mutate(key = str_replace(key, "_", " ") %>% str_to_title()) %>% 
  mutate(key = fct_reorder(key, value)) %>% 
  ggplot(aes(x = key, y = value, color = key)) + 
  geom_point(size = 5) + 
  geom_segment(aes(x = key, xend = key, y = value, yend = 0)) + 
  theme(legend.position = "none") + 
  ylab("") + 
  xlab("") + 
  coord_flip() + 
  labs(title = "Avg Flavour Profile")


#interesting dimensions:
#country, variety, company

coffee_metrics = coffee%>%
  mutate(coffee_id = row_number())%>%
  select(coffee_id,total_cup_points,variety,company,country_of_origin,altitude_mean_meters,
         aroma:moisture)%>%
  pivot_longer(aroma:cupper_points, names_to = 'metric', # pivot to show all attribute values 
               values_to  = 'value')

#aggregating to total_cup_points
 coffee_metrics%>%
   group_by(coffee_id, total_cup_points)%>%
   summarise(total = sum(value)) #figuring out where total_cup_points comes from
 
#ridgeplot of each metric and their values
coffee_metrics%>%
  mutate(metric = fct_reorder(metric, value))%>%
  ggplot(aes(value, metric))+
  geom_density_ridges() #almost always 10 for top 3 metrics

#confirming distribution of metrics
coffee_metrics%>%
  group_by(metric)%>%
  summarise(mean = mean(value),
            sd = sd(value))%>%
  arrange(desc(mean))

#find correlations among columns
library(widyr)
library(ggraph)
library(igraph)
correlations <- coffee_metrics%>%
  pairwise_cor(metric, coffee_id, value, sort = TRUE)

#plot
correlations%>%
  head(60)%>%
  graph_from_data_frame()%>%
  ggraph()+
  geom_edge_link(aes(edge_alpha = correlation))+
  geom_node_point()+
  geom_node_text(aes(label= name), repel = TRUE)

#correlation in dimensions
coffee_metrics%>%
  group_by(metric)%>%
  mutate(centered = value - mean(value))%>%
  ungroup()%>%
  widely_svd(metric, coffee_id, centered)%>%
  filter(dimension<=4)%>%
  mutate(tidytext::reorder_within(metric, value, dimension))%>%
  ggplot(aes(value, metric))+
  geom_col()+
  scale_y_reordered()+
  facet_wrap(~dimension, scales = 'free_y')

#altitudes
coffee_ratings%>%
  filter(altitude_mean_meters >=2000)%>%
  select(altitude_mean_meters, altitude)%>%
  arrange(altitude_mean_meters)

#look at correlations between qualities and altitudes

#scatterplot of altitude and score
coffee_metrics%>%
  filter(altitude_mean_meters<=10000)%>% #if higher than 3000, just stop
  mutate(altitude_mean_meters = pmin(altitude_mean_meters, 3000))%>%
  ggplot(aes(altitude_mean_meters, total_cup_points))+
  geom_point()+
  geom_smooth(method = 'lm')
  
#correlations with altitude, and create linear model
  coffee_metrics%>%
    filter(altitude_mean_meters<=10000)%>% 
    mutate(altitude_mean_meters = pmin(altitude_mean_meters, 3000), #if higher than 3000, just stop
           km = altitude_mean_meters/1000)%>% #convert to km
    group_by(metric)%>%
    summarise(correlation = cor(km, value),
              model = list(lm(value~km)))%>% #model for each metric
    mutate(tidied = map(model, broom::tidy, conf.int = TRUE))%>% #expand the lists
    unnest(tidied)%>% #unnest
    filter(term == 'km')%>% #remove intercept models
    ungroup()%>%
    mutate(metric = fct_reorder(metric, estimate))%>%
    ggplot(aes(estimate, metric, color = p.value < 0.05))+ #plotting slope increase of altitude/score
    geom_point()+
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.1 )+ #add CI bars
    labs(x  = 'evaluation of coffee',
         y = 'each kilometer of altitude contributes this much to score (95%CI)')
  
  

   