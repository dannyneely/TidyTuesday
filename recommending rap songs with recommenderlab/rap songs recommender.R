library(tidyverse)
library(tidytuesdayR)

tt <- tidytuesdayR::tt_load('2020-04-14')
polls <- tt$polls

#create recommendation engine
#convert to matrix
#make it a binary matrix
library(recommenderlab)

rap_matrix <- polls %>% 
  select(critic_name, title) %>% 
  mutate(n = 1) %>% 
  arrange(title) %>% 
  pivot_wider(names_from = 'title', values_from = 'n', values_fill = list(n = 0)) %>% 
  select(-critic_name) %>% 
  as.matrix() %>% 
  as('binaryRatingMatrix')


#create schema
#80-20 split
#given is how many items to give. we are doing all except for 1
#e.g, if a critic has their top 5 best rap songs of all time, use 4 of them to try and predict the 5th
training_schema <- evaluationScheme(rap_matrix, method = 'split', train = 0.8, given = -1)

#differnet model types
UBCF_model <- evaluate(training_schema, method = 'UBCF', type = 'topNList', n = 5)
IBCF_model <- evaluate(training_schema, method = 'IBCF', type = 'topNList', n = 5)

UBCF_model %>% avg()
IBCF_model %>% avg()
#TPR shows that 4% of the time it will predict the 5th song correctly

#parameter tuning to create a better model
tune_engines = function(schema, parameters){
  UBCF_model <- evaluate(schema, method = 'UBCF', type = 'topNList', n = 5, param = list(nn = parameters))
  IBCF_model <- evaluate(schema, method = 'IBCF', type = 'topNList', n = 5, param = list(k = parameters))
  
  UBCF_model %>% 
    avg() %>%  
    as.tibble() %>% 
    mutate(model = 'UBCF') %>% 
    rbind(IBCF_model %>% 
            avg() %>% 
            as.tibble() %>% 
            mutate(model = 'IBCF')) %>% 
    return()
  
  
}

#need to have 10 nearest critics
tune_grid <- tibble(parameters = c(3,5,10))
tune_grid %>% 
  mutate(results = map(parameters, -tune_engines(training_schema, .x))) %>% 
  unnest()

# 5 neighbors is best 

ubcf_final_model <- Recommender(getData(training_schema, 'train'), 'UBCF', param = list(nn =5 ))
predictions = predict(ubcf_final_model, getData(training_schema, 'known'), type = 'topNList')
calcPredictionAccuracy(predictions,getData(training_schema, 'unknown'), given = -1 )

rec_engine <-  Recommender(rap_matrix, 'UBCF', param = list(nn = 5))
rec_engine

andrew_songs <- polls %>% 
  select(title) %>% 
  distinct() %>% 
  arrange(title) %>% 
  filter(title %in% c('All of the Lights','Alright',"Bitch Don't Kill my Vibe", "m.a.a.d. city",
                      "changes")) %>% 
  rbind(polls %>% select(title) %>% distinct()) %>% 
  count(title) %>% 
  mutate(n = n-1) %>% 
  pivot_wider(names_from = 'title',values_from = 'n', values_fill =list(n = 0)) %>% 
  as.matrix() %>% 
  as('binaryRatingMatrix')

predict(rec_engine, andrew_songs) %>% 
  as('list') %>% 
  as.data.frame
