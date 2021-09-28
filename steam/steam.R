library(tidyverse)
library(scales)
library(lubridate)
theme_set(theme_light())
tt =tt_load('2021-03-16')

#create date column that is a date field with lubridate
games = tt$games%>%
  mutate(avg_peak_perc = parse_number(avg_peak_perc)) %>%  
           mutate(date = ymd(paste(year, month, 1))) %>% 
           filter(date > min(date)) %>% 
           mutate(month = fct_reorder(month, month(date))) #reorder as months occur throughout the year

#function to summarise at different levels
summarise_games = function(tbl){
  tbl%>%
    summarise(median_avg = median(avg),
              median_peak = median(peak)) #we're using medians because of all the extreme values in some games. 
}

#simple eda plots
games%>%
  count(date)%>%
  ggplot(aes(date, n))+
  geom_col()

games%>%
  filter(gamename == 'Dota 2')%>%
  arrange(desc(avg))%>%
  View()

#total peak players per month
games%>%
  group_by(gamename)%>%
  summarise(peak_sum = sum(peak))%>%
  arrange(desc(peak_sum))%>%
  head(10)%>%
  ggplot(aes(peak_sum, reorder(gamename, peak_sum),))+
  geom_col(aes(fill = gamename))+
  scale_x_continuous(label = comma)+
  theme(axis.text.x = element_text( hjust = 1))+
  labs(x = 'Game',
       y = 'Sum of Peak Players Every Month')

#pubg rise
games%>%
  filter(gamename %in% c("PLAYERUNKNOWN'S BATTLEGROUNDS",
                         "Counter-Strike: Global Offensive",
                         "Dota 2"),
         year == 2017)%>%
  group_by(gamename)%>%
  ggplot(aes(date, avg))+
  geom_line(aes(color = gamename))+
  scale_y_continuous(label = comma)
#pubg fall
games%>%
  filter(gamename %in% c("PLAYERUNKNOWN'S BATTLEGROUNDS",
                         "Counter-Strike: Global Offensive",
                         "Dota 2"),
         year == 2018)%>%
  group_by(gamename)%>%
  ggplot(aes(date, avg))+
  geom_line(aes(color = gamename))+
  scale_y_continuous(label = comma)

#pubg life compared to dota 2 and csgo
games%>%
  filter(gamename %in% c("PLAYERUNKNOWN'S BATTLEGROUNDS",
                         "Counter-Strike: Global Offensive",
                         "Dota 2"))%>%
  group_by(gamename)%>%
  ggplot(aes(date, avg))+
  geom_line(aes(color = gamename))+
  scale_y_continuous(label = comma)

#largest peaks plot
games%>%
  arrange(desc(peak))%>%
  head(20)%>%
  ggplot(aes(x = gamename, peak))+
  geom_col(aes(fill = gamename))
  scale_x_continuous(label = comma)+
  theme(axis.text.x = element_text( hjust = 1))+
  labs(x = 'Game',
       y = 'Peak Players Every Month')

#coronavirus spike for average number of concurrent players
games%>%
  group_by(date)%>%
  summarise_games%>%
  ggplot(aes(date, median_avg))+
  geom_line()+
  expand_limits(y = 0)

#looking at before covid spike, check for seasonality
#spikes in june/july for summer, and dec-jan for winter 
games%>%
  filter(date < '2020-01-01')%>%
  group_by(month)%>%
  summarise_games%>%
  ggplot(aes(month, median_avg))+
  geom_line(group=1)+
  expand_limits(y = 0)+
  labs(title = 'Seasonal Trend for Steam Games',
       subtitle = 'From 2012 - 2020',
       x = 'Month',
       y = 'Median Average of Concurrent Players')

#looking at each year and how their months perform, including 2020 covid year
games%>%
  group_by(year, month)%>%
  summarise_games%>%
  ggplot(aes(month, median_avg))+
  geom_line(aes(color = factor(year), group = year))+
  expand_limits(y = 0)+
  labs(title = 'Seasonal Trend for Steam Games',
       subtitle = 'From 2012 - 2021',
       x = 'Month',
       y = 'Median Average of Concurrent Players')

#looking at median peak concurrent players by each month/year
games%>%
  group_by(date)%>%
  summarise_games()%>%
  ggplot(aes(date, median_peak))+
  geom_line()+
  expand_limits(y = 0)


#distribution of game popularity
games%>%
  filter(date == max(date))%>%
  ggplot(aes(avg))+
  geom_histogram()+
  scale_x_log10(label = comma,
                breaks=  10^seq(0,5))+
  labs(x = 'Average Number of Players')


#world of warships averaged 11,574 players, but peaked at 353413 (in february 2021)
games%>%
  filter(date == max(date),#looks at feb 2021
         avg >= 100)%>% #must have more than 100 players average
  arrange(avg_peak_perc) #smallest share of average to peak. 


#world of warships has a 30:1 peak to average ratio. 30 times more players in the peak than average.
games%>%
  filter(date == '2020-12-01',
         avg >=1000)%>%
  arrange(avg_peak_perc)%>%
  ggplot(aes(avg, 1/avg_peak_perc, label = gamenamegeom_bar))+
  geom_point()+
  geom_text(vjust = 1, hjust = 1, check_overlap = TRUE)+
  scale_x_log10(labels = comma)+
  scale_y_log10(labels = comma)+
  labs(x = 'Average Number of Players in Feb 2021',
       y = 'ratio of peak:average')

 #interactive            
plotly::ggplotly(peak_avg_ratio_plot)


#look at games for all time, not just by month
games%>%
  filter(avg >=1000)%>%
  arrange(avg_peak_perc)%>%
  ggplot(aes(avg, 1/avg_peak_perc, label = gamename))+
  geom_point()+
  geom_text(vjust = 1, hjust = 1, check_overlap = TRUE)+
  scale_x_log10(labels = comma)+
  scale_y_log10(labels = comma)+
  labs(x = 'Average Number of Players',
       y = 'ratio of peak:average')


#looking at most popular games

#each game average players over time
games%>%
  filter(fct_lump(gamename,20,w = avg) != 'Other')%>%#takes top 12 games by total average players. anything else is 'other'
  mutate(gamename = fct_reorder(gamename, -avg))%>%
  ggplot(aes(x = date, y = avg))+
  geom_line()+
  expand_limits(y = 0)+
  scale_y_continuous(label = comma)+
  facet_wrap(~gamename, scales = 'free_y')+
  labs(x = 'Date',y = 'Average Number of Players per Month')



#what became popular because of covid? before and after lockdown (february to april)
apr_feb_ratios = games%>%
  filter(year == 2020,
         avg > 100)%>%
  select(gamename, month, avg, peak)%>%
  pivot_wider(names_from = month, values_from = c(avg,peak))%>%
  mutate(jan_feb_ratio =  avg_February/avg_January)%>% #comparing before and after - Jan to Feb
  mutate(feb_apr_ratio =  avg_April/avg_February)%>% #comparing before and after Feb - April (lockdown)
  select(gamename, contains('January'),
         contains('April'), 
         contains('February'), 
         jan_feb_ratio,
         feb_apr_ratio)%>% #only selecting specific months
  filter(avg_January >= 100,
         avg_February >= 100) %>% 
  arrange(desc(feb_apr_ratio))

#plotted biggest covid spike games 
games %>% 
  inner_join(apr_feb_ratios %>% 
  top_n(12, feb_apr_ratio), by = 'gamename') %>%  #inner join first, then get top 12 otherwise duplicate games are output
  mutate(gamename = fct_reorder(gamename, -avg))%>%
  complete(gamename, date, fill = list(avg= 0)) %>%  #fills in missing month data with 0, so weird diagonal line is gone
  filter(date >= '2018-01-01') %>% 
  ggplot(aes(x = date, y = avg))+
  geom_line()+
  geom_vline(xintercept = as.Date("2020-03-01"), #annotate lockdown
             lty = 2, color = 'red') +
  expand_limits(y = 0)+
  scale_y_continuous(label = comma)+
  facet_wrap(~gamename, scales = 'free_y')+
  labs(x = 'Date',y = 'Average Number of Players per Month')
  
  
#plague inc (can make a function to run for any game)
  games%>%
    filter(str_detect(gamename,'Wolcen'))%>%
    ggplot(aes(x = date, y = avg))+
    geom_line()+
    expand_limits(y = 0)+
    scale_y_continuous(label = comma)+
    labs(title = 'Plague Inc. Covid Effect',
         x = 'Date',
         y = 'Average Players per Month')
