library(tidyverse)
library(scales)
theme_set(theme_light())

tt = tidytuesdayR::tt_load('2021-03-02')
View(tt)
colnames(tt$youtube)
#remove columns
tt$youtube = tt$youtube%>%
  select(-favorite_count)%>%
  mutate(brand = fct_recode(brand, Hyundai = 'Hynudai')) #change mispelling typo for all instances

#count of each brand
tt$youtube%>%
  count(brand, sort=TRUE)%>%
  mutate(brand = fct_reorder(brand, n))%>%
  ggplot( aes( n, brand))+
  geom_col()

#count of each brand over time
tt$youtube%>%
  ggplot( aes(year, fill = brand))+
  geom_bar()+
  facet_wrap(~brand)+
  theme(legend.position = 'none')


#view count histogram
tt$youtube%>%
  ggplot(aes(view_count))+
  geom_histogram(binwidth = 0.2)+
  scale_x_log10(label = comma)

#boxplot view counts over time
tt$youtube%>%
  ggplot( aes(year, view_count, group = year))+
  geom_boxplot()+
  scale_x_log10() #one very high year of view counts, 2012

#2012 doritos ad
tt$youtube%>%
  arrange(desc(view_count))%>%
  select(year, brand, view_count)%>%
  head(10)


#lineplot median view counts over time. size of points is how many ads that year
tt$youtube%>%
  filter(!is.na(view_count))%>%
  group_by(year)%>%
  summarise(n = n(),
            median_views = median(view_count)
            )%>%
  arrange(desc(median_views))%>% 
  ggplot( aes(year, median_views))+
  geom_point(aes(size = n))+
  geom_line()+
  scale_y_log10() #high median view count for 2017

#what's that 2017 video?
tt$youtube%>%
  filter(year == 2017)%>%
  arrange(desc(view_count))%>%
  View()


#view count and brand boxplot.
#can use 'fill' to check for any column
tt$youtube%>%
  filter(!is.na(view_count))%>%
  mutate(brand = fct_reorder(brand, view_count))%>%
  ggplot(aes(view_count, brand, fill = use_sex))+
  geom_boxplot()+
  scale_x_log10(label = comma)

#all counts compared to the number of views
tt$youtube%>%
  gather(metric, value, contains('_count'))%>% #make everything with a count into a single column, plus n
  ggplot(aes(value))+
  geom_histogram(binwidth = 0.2)+
  scale_x_log10(label = comma)+
  facet_wrap(~metric)


#different qualities for a successful ad
tt$youtube%>%
  gather(category, value, funny:use_sex)%>%
  ggplot(aes(category, view_count, fill = value))+
  geom_boxplot()+
  scale_y_log10(labels = comma) #no real difference.

#differences in median view counts depending on quality
tt$youtube%>%
  filter(!is.na(view_count))%>%
  gather(category, value, funny:use_sex)%>%
  group_by(category, value)%>%
  summarise(n = n(), median_view_count = median(view_count))%>%
  ggplot(aes(x =category, y = median_view_count, fill = value ))+
  geom_col(position = 'dodge') #patriotic and danger have higher view counts

#correlation between quality and view count
tt$youtube%>%
  filter(!is.na(view_count))%>%
  gather(category, value, funny:use_sex)%>%
  group_by(category)%>%
  summarise(n = n(), 
            median_view_count = median(view_count),
            correlation = cor(value, log(view_count)+1))%>% #add 1 in case of 0's
  arrange(desc(correlation))

#linear model of quality and view count
summary(lm(log2(view_count)~animals+celebrity+danger+funny+patriotic+
             show_product_quickly+use_sex,data = tt$youtube)) #nothing significant.


#look at each quality over time
tt$youtube%>%
  filter(!is.na(view_count))%>%
  gather(category, value, funny:use_sex)%>%
  group_by(category = str_to_title(str_replace_all(category,"_", " ")), year)%>% #make categories readable
  summarize(pct = mean(value))%>%
  ggplot(aes(x = year, y = pct, color = category))+
  geom_line()+
  facet_wrap(~category)+
  scale_y_continuous(label = percent)+
  theme(legend.position = 'none') #big upward trend in celebrities. every ad in 2020 had a celeb.

#log reg model
summary(glm(celebrity~year, data = tt$youtube, family = 'binomial'))

#model for each predictor
coefs = tt$youtube%>%
  gather(category, value, funny:use_sex)%>%
  group_by(category)%>%
  summarise(model = list(glm(value~year, family = 'binomial')))%>% #makes a model for each
  mutate(td = map(model,broom::tidy))%>%
  unnest(td)%>%
  filter(term != '(Intercept)')%>%
  arrange(desc(estimate))

#compare brands and what categories of ads they used by percent
by_brand_category <- tt$youtube%>%
  gather(category, value, funny:use_sex)%>%
  group_by(brand, category)%>%
  summarise(pct = mean(value))%>%
  ungroup()

#% of brand's ads that have particular quality. percents will go above 100 because an ad can have multiple qualities
library(tidytext)
by_brand_category%>%
  mutate(brand = reorder_within(brand , pct, category))%>%
  ggplot(aes(pct,brand, fill = category))+
  geom_col()+
  scale_x_continuous(label = percent)+
  facet_wrap(~category, scales = 'free_y')+
  scale_y_reordered()


#same type of plot, but by brand. easy to compare brands
by_brand_category%>%mutate(category = reorder_within(category , pct, brand))%>%
  ggplot(aes(pct,category))+
  geom_col()+
  scale_y_reordered()+
  scale_x_continuous(label = percent)+
  facet_wrap(~brand, scales = 'free_y')

#tile plot for heatmap of percentages
by_brand_category%>%
  ggplot(aes(x = category, y = brand, fill = pct))+
  geom_tile()+
  scale_fill_gradient2(low = 'blue', high = 'red', midpoint = 0.5)


#LOOKING AT WHAT PEOPLE LIKED AND DIDN'T LIKE#
#geom text label plot
tt$youtube%>%
  mutate(dislike_pct=  dislike_count / (like_count + dislike_count),
         like_ratio = like_count / dislike_count,
         like_dislike_total = like_count + dislike_count)%>%
  filter(!is.na(dislike_pct),
         like_dislike_total >= 1000)%>%
  select(year, brand, title, description,view_count,like_count, dislike_pct, dislike_count,like_ratio)%>%
  arrange(desc(view_count))%>%
  ggplot(aes(x = view_count, y =dislike_pct))+
  geom_point(aes(color = brand))+ #color by brand
  geom_text(aes(label = str_trunc(title,65)),
                check_overlap = TRUE,
                vjust=  1, hjust = 1, size = 2)+ #label points 
  scale_x_log10(label = comma)+
    scale_y_continuous(labels = percent)

