library(tidytuesdayR)
library(tidyverse)
library(scales)
library(tidytext)
library(lubridate)
theme_set(theme_light())
tt <- tidytuesdayR::tt_load('2021-04-20')

netflix <- tt$netflix_titles
str(netflix)
head(netflix)

#data cleaning
#durations have different units, so separate out 
netflix <- netflix %>%
  separate(duration, c("duration", "duration_units"), sep = " ", convert = TRUE) %>%
  mutate(date_added = mdy(date_added),
         year_added = year(date_added),
         mature = rating %in% c("TV-MA", "R", "NC-17"))


#################################################
#looking at movies/shows by year
netflix %>% 
  count(decade = 10 * (release_year%/%10), type) 
  
  
netflix %>% 
  ggplot(aes(release_year, fill = type))+
  geom_histogram()+
  facet_wrap(~type, ncol = 1, scales= 'free_y')

netflix %>% 
  count(decade = 10 * (release_year%/%10), type) %>% 
  group_by(type) %>% 
  mutate(percent = n/sum(n)) %>% 
  ggplot(aes(decade, percent, color  = type))+
  geom_line()
#################################################


#movie durations over time
netflix %>% 
  filter(type == 'Movie') %>% 
  mutate(decade = 10*(release_year %/%10)) %>% 
  ggplot(aes(decade, duration, group = decade))+
  geom_boxplot()

#################################################
#genres
#separate out different genres, each genre into its own row
netflix %>% 
  separate_rows(listed_in, sep = ', ') %>%
  group_by(type, listed_in) %>% 
  summarise(n = n(),
            median_duration = median(duration)) %>% 
arrange(desc(n))
#################################################
#look at date added

#how many added per year?
netflix %>% 
  filter(!is.na(date_added)) %>% 
  count() 

#look at recent years only
netflix %>%
  filter(!is.na(date_added)) %>%
  count(year_added, type) %>%
  ggplot(aes(year_added, n, fill = type)) +
  geom_area()

#ratings by year
netflix %>%
  filter(year_added >= 2015) %>%
  filter(!is.na(date_added), !is.na(rating)) %>%
  mutate(rating = fct_lump(rating, 5)) %>%
  ungroup() %>%
  count(type, year_added, rating) %>%
  group_by(type, year_added) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(year_added, percent, fill = rating)) +
  geom_area() +
  facet_wrap(~ type)

#ratings by country
#error bar plot with jefferys conf int (follows beta dist.)
#percent of titles that are rated R, for TV and movie, by country
netflix %>%
  filter(!is.na(rating), !is.na(country)) %>%
  group_by(type, country = fct_lump(country, 9)) %>%
  summarize(n_mature = sum(rating %in% c("R", "TV-MA", "NC-17")),
            n = n(),
            .groups = "drop") %>%
  mutate(pct_mature = n_mature / n,
         conf_low = qbeta(.025, n_mature + .5, n - n_mature + .5),
         conf_high = qbeta(.975, n_mature + .5, n - n_mature + .5)) %>%
  ggplot(aes(pct_mature, country, color = type)) +
  geom_point(aes(size = n)) +
  geom_errorbar(aes(xmin = conf_low, xmax = conf_high), width = .1) +
  scale_x_continuous(labels = percent) +
  expand_limits(x = 0) +
  labs(x = "% of titles that are R/TV-MA")

#################################################
#text cleanup
library(tidytext)
library(snakecase)
library(tidylo)
#snake case puts everything to_lower, and underscores for spaces
words_unnested <- netflix %>% 
  unnest_tokens(word, description) %>% 
  anti_join(stop_words, by = 'word')

words_unnested %>% 
  count(type, word, sort = TRUE) %>% 
  mutate(type = to_snake_case(type)) %>% 
  spread(type,n, fill = 0 ) %>% 
  mutate(total = movie + tv_show) %>% 
  arrange(desc(total)) %>% 
  head(100) %>% 
  ggplot(aes(movie, tv_show))+
  geom_point()+
  geom_text(aes(label = word),hjust=  1, vjust = 1)
  scale_x_log10()+
  scale_y_log10()
  
#find relative ratio
#word that occurs most in tv/movie
words_unnested %>%
  count(type, word) %>% 
  bind_log_odds(type, word, n) %>% 
  arrange(desc(log_odds_weighted)) %>% 
  group_by(type) %>% 
  top_n(10, log_odds_weighted) %>% 
  ungroup() %>% 
  mutate(word = fct_reorder( word, log_odds_weighted)) %>% 
  ggplot(aes(log_odds_weighted,word))+
  geom_col()+
  facet_wrap(~type, scales = 'free_y')

#what words often appear with other words
#words that appear in at least 40 titles
#plot words that appear together, at least 0.1 correlation
library(widyr)
library(tidygraph)
library(ggraph)
words_unnested %>%
  distinct(type, title, word) %>%
  add_count(word, name = "word_total") %>%
  filter(word_total >= 40) %>%
  pairwise_cor(word, title, sort = TRUE) %>% 
  filter(correlation >=0.1) %>% 
  igraph::graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation)) +
  geom_node_point() +
  geom_node_text(aes(label = name),
                 repel = TRUE) +
  theme(legend.position = "none")
  

#what words appear often together for each genre?
#first, check that each title only appears once for each word.
#only words that appear at least 25 times aross all genres
#how often does a word appear in one genre than by random chance?
word_genre_log_odds <- words_unnested %>%
  distinct(type, title, word, genre = listed_in) %>%
  add_count(word, name = "word_total") %>%
  filter(word_total >= 25) %>%
  separate_rows(genre, sep = ", ") %>%
  filter(fct_lump(genre, 9) != "Other") %>%
  count(genre, word) %>%
  bind_log_odds(genre, word, n)

#visualize
#reorder within every genre
word_genre_log_odds %>%
  group_by(genre) %>%
  top_n(10, log_odds_weighted) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, log_odds_weighted, genre)) %>%
  ggplot(aes(log_odds_weighted, word, fill = genre)) +
  geom_col() +
  facet_wrap(~ genre, scales = "free_y") +
  scale_y_reordered() +
  theme(legend.position = "none") +
  labs(x = "Log-odds of word's specificity to genre",
       y = "")

#modelling with lasso regression
#predict the rating it would get based off the words in the description
#filter out any words that dont appear in at least 30 movies
#adds the total count for every time the word appears
word_ratings <- words_unnested %>%
  count(type, title, rating, word) %>%
  filter(!is.na(rating)) %>%
  add_count(word, name = "word_total") %>%
  filter(word_total >= 30)


library(glmnet)
library(broom)

#add more features to help with mature prediction
#gather diretor, cast, and genre
other_features <- netflix %>%
  select(title, director, cast, genre = listed_in, country) %>%
  gather(feature_type, feature, director, cast, genre, country) %>%
  filter(!is.na(feature)) %>%
  separate_rows(feature, sep = ", ") %>%
  mutate(feature_type = str_to_title(feature_type)) %>%
  unite(feature, feature_type, feature, sep = ": ") %>%
  add_count(feature, name = "feature_count") %>%
  filter(feature_count >= 10)

feature_matrix <- word_ratings %>%
  mutate(feature = paste("Description:", word)) %>%
  bind_rows(other_features) %>%
  cast_sparse(title, feature)

#match the rownames to the title, use as target variable 
#trues and falses for if the movie is mature
y <- netflix$mature[match(rownames(feature_matrix), netflix$title)]

#predict with cross validation
mod <- cv.glmnet(feature_matrix, y, family = "binomial")

#fit the model with a linear term for each word
#as penalty parameter changes. with a smaller penalty, we start overfitting
#sweet spot is where lambda pushes error down (between 393 and 202)
plot(mod)

#words like comedian and stand up are most influential in pushing towards mature (standup comedy)
#filter for where lambda is within one standard deviation
#positive estimates make the word more likely to be mature, negative is less likely
#visualize top 20, absolute of value (negative or positive towards a mature rating)
mod$glmnet.fit %>%
  tidy() %>%
  separate(term, c("feature_type", "feature"), sep = ": ") %>%
  filter(lambda == mod$lambda.1se) %>%
  top_n(60, abs(estimate)) %>%
  mutate(feature = fct_reorder(feature, estimate)) %>%
  ggplot(aes(estimate, feature, fill = feature_type)) +
  geom_col() +
  labs(x = "Coefficient: does this make the title more likely to be TV-MA/R?",
       y = "",
       fill = "Feature Type")

