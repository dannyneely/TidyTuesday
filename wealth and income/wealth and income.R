library(tidyverse)
library(scales)


tt <- tidytuesdayR::tt_load("2021-02-09")
head(tt)

#make one EDA graph for each dataset
#make function for the plotting
plot_by_race <-  function(df, col, labels = dollar, ...){
  df%>%
    mutate(race = fct_reorder(race, -{{ col }}, last))%>% #fct_reorder labels the legend by the last value occuring in the line
    ggplot(aes(x = year, y = {{ col }}, color = race, ...))+
    geom_line()+
    expand_limits(y = 0)+
    scale_y_continuous(label = labels)
}
  
  
tt$lifetime_earn%>%
  ggplot(aes(lifetime_earn, race, fill = gender))+
  geom_col(position = 'dodge')+
  scale_x_continuous(label = dollar)

tt$student_debt%>%
  plot_by_race(loan_debt_pct, labels = percent)+
  labs(y = '% of families with student loan debt')

tt$student_debt%>%
  plot_by_race(loan_debt)+
  labs(y = 'Average student loan debt ')

tt$retirement%>%
  plot_by_race(retirement)

tt$home_owner%>%
  plot_by_race(home_owner_pct, labels = percent)+
  labs(y = 'home ownership percentage')

tt$race_wealth%>%
  plot_by_race(wealth_family, lty = type)+
  facet_wrap(~type, scales = 'free_y')+ #separates a different y-axis scale for each facet chart
  labs(y = 'family wealth 2016 (dollars)')

tt$income_time%>%
  spread(percentile, income_family)%>%
  ggplot(aes(x = year, y = `50th`, ymin = `10th`, ymax = `90th`))+
  geom_line()+
  geom_ribbon(alpha = 0.2)+
  expand_limits(y=0)+
  scale_y_continuous(label = dollar)+
  labs(y = 'family income (median with 10th and 90th percentiles)')

library(plotly)  
#faceting different races with the quintiles.
#interactive with plotly
plt = tt$income_limits%>%
  filter(dollar_type == '2019 Dollars',
         !str_detect(race,'or in Combination') #removing duplicate race
         )%>%
  distinct(race, year, income_quintile, .keep_all = TRUE)%>%
  mutate(income_quintile = fct_reorder(income_quintile, -income_dollars))%>% #reorder legend
  ggplot(aes(x = year, y = income_dollars, color = income_quintile))+
  geom_line()+
  facet_wrap(~race)+
  scale_y_continuous(label = dollar)+
  labs(y = 'income quntile')

ggplotly(plt)


#income share area plot
tt$income_aggregate%>%
  filter(income_quintile != 'Top 5%',
         !str_detect(race, 'or in Combination'))%>%
  mutate(income_share = income_share/100,
         income_quintile = fct_inorder(income_quintile)
         )%>%
  ggplot(aes(year, income_share, fill = income_quintile))+
  geom_area()+
  facet_wrap(~race)+
  scale_y_continuous(label = percent)+
  labs(y = '% of income ', 
       fill = 'Income Quintile') #labels the legend

#distribution of income over time
tt$income_distribution%>%
  filter(income_bracket != '$200,000 and over',
         !str_detect(race, 'or in Combination'))%>%
  mutate(income_distribution = income_distribution/100,
         income_bracket = fct_inorder(income_bracket)
  )%>%
  ggplot(aes(year, income_distribution, fill = income_bracket))+
  geom_area()+
  facet_wrap(~race)+
  scale_y_continuous(label = percent)+
  labs(y = '% of income ', 
       fill = 'Income Bracket') #labels the legend


