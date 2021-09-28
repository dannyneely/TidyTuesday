library(tidyverse)
library(tidytuesdayR)
library(scales)
theme_set(theme_light())


tt <- tt_load("2021-06-08")

fishing <- tt$fishing %>%
  filter(values >= 0) %>%
  mutate(species = str_replace(str_to_title(species), "([^s])s$", "\\1"))

#function to get counts of each species, and total production of each species
summarize_fishing <- function(tbl) {
  tbl %>%
    summarize(n_obs = n(),
              total_production = sum(values, na.rm = TRUE)) %>%
    arrange(desc(total_production))
}

#check function
fishing %>%
  group_by(species) %>%
  summarize_fishing()

#basic histogram. add 1 for 0 values
fishing %>%
  ggplot(aes(values + 1)) +
  geom_histogram() +
  scale_x_log10()

#count of each lake
fishing %>%
  count(lake)

#count of each species. 44 species
fishing %>%
  count(species, sort = TRUE)

#barplot of production per decade
fishing %>%
  group_by(decade = 10 * year %/% 10) %>%
  summarize_fishing() %>%
  ggplot(aes(decade, total_production)) +
  geom_col() +
  scale_y_continuous(labels = comma_format()) +
  labs(x = "Decade",
       y = "Total production per decade (thousands of pounds)")


# top 15 species only, lump everything else. weighted by values
#reorder and descending for greatest count first
#area plot and facet by species
fishing %>%
  mutate(species = fct_lump(species, 15, w = values),
         species = fct_reorder(species, values, sum, .desc = TRUE)) %>%
  group_by(decade = 10 * year %/% 10,
           species) %>%
  summarize_fishing() %>%
  ggplot(aes(decade, total_production, fill = species)) +
  geom_area() +
  scale_y_continuous(labels = comma_format()) +
  facet_wrap(~ species) +
  theme(legend.position = "none") +
  labs(x = "Decade",
       y = "Total production per decade (thousands of pounds)",
       title = "Production of 9 common fish in Great Lakes over time")

#take each species and the year it peaked in
fishing %>%
  group_by(species, year) %>%
  summarize_fishing() %>%
  summarize(total_production = sum(total_production),
            peak_year = year[which.max(total_production)]) %>%
  arrange(desc(total_production)) %>%
#  head(25) %>%
  mutate(species = fct_reorder(species, peak_year)) %>%
  ggplot(aes(peak_year, species)) +
  geom_point(aes(size = total_production)) +
  scale_size_continuous(labels = comma_format()) +
  labs(x = "Year of peak production",
       y = "",
       size = "All-time production")

#production of fish per decade, for each LAKE this time
fishing %>%
  mutate(lake = fct_reorder(lake, values, sum, .desc = TRUE)) %>%
  group_by(decade = 10 * year %/% 10, lake) %>%
  summarize_fishing() %>%
  ggplot(aes(decade, total_production, fill = lake)) +
  geom_area() +
  scale_y_continuous(labels = comma_format()) +
  facet_wrap(~ lake) +
  theme(legend.position = "none") +
  labs(x = "Decade",
       y = "Total production per decade (thousands of pounds)",
       title = "Production across the Great Lakes over time")


#species per lake
by_lake_species <- fishing %>%
  filter(lake != "Saint Clair") %>%
  mutate(species = fct_lump(species, 20, w = values),
         species = fct_reorder(species, values, sum),
         lake = fct_reorder(lake, values, sum, .desc = TRUE)) %>%
  group_by(lake, species) %>%
  summarize_fishing()

#hetmap of each lake and what species are produced there
by_lake_species %>%
  ggplot(aes(lake, species, fill = total_production)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", high = "darkblue",
                       labels = comma_format()) +
  expand_limits(fill = 0) +
  theme(panel.grid = element_blank()) +
  labs(x = "Lake",
       y = "Species",
       fill = "All-time Production")

#same heatmap, but by percentage
#normalized, so all columns add to 1
by_lake_species %>%
  group_by(lake) %>%
  mutate(pct = total_production / sum(total_production)) %>%
  ggplot(aes(lake, species, fill = pct)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", high = "darkblue",
                       labels = percent_format(accuracy = 1)) +
  expand_limits(fill = 0) +
  theme(panel.grid = element_blank()) +
  labs(x = "Lake",
       y = "Species",
       fill = "% of lake's production")


#forecasting!

#looking at just one specis first-  yellow perch
fishing %>%
  filter(species == "Yellow Perch") %>%
  group_by(year, species) %>%
  summarize_fishing() %>%
  ggplot(aes(year, total_production)) +
  geom_line()

#each species by year
by_year_species <- fishing %>%
  mutate(year = as.Date(paste0(year, "-01-01"))) %>%
  group_by(year, species) %>%
  summarize_fishing() %>%
  ungroup() %>%
  select(-n_obs)

#forecasting libraries
library(timetk)
library(forecast)
library(sweep)
library(lubridate)



yellow_perch <- by_year_species %>%
  filter(species == "Yellow Perch")

#turn into a time series object
time_series <- yellow_perch %>%
  tk_ts(start = min(year(.$year)), freq = 1)

#fit time series with trend and seasonanlity 
ets_mod <- time_series %>%
  ets()

ets_mod %>%
  sw_tidy()

ets_mod %>%
  sw_augment() %>%
  ggplot(aes(index, .actual)) +
  geom_line() +
  geom_line(aes(y = .fitted), color = "blue") +
  labs(x = "Year",
       y = "Total production of Yellow Perch")

fcast <- ets_mod %>%
  forecast(h = 10) 

fcast_holt <- holt(time_series)

fcast_ses <- ses(time_series)

autoplot(time_series) +
  autolayer(fcast_holt, series="Holt", PI = FALSE) +
  autolayer(fcast_ses, series="SES", PI = FALSE)



time_series <- by_year_species %>%
  filter(species == "Cisco") %>%
  select(-species) %>%
  tk_ts(start = min(year(.$year)), freq = 1)


forecasts <- by_year_species %>%
  mutate(species = fct_lump(species, 8)) %>%
  group_by(species, year) %>%
  summarize(total_production = sum(total_production), .groups = "drop") %>%
  nest(data = c(-species)) %>%
  mutate(time_series = map(data, ~ tk_ts(., start = min(year(.$year)), freq = 1))) %>%
  mutate(holt = map(time_series, holt, h = 20),
         ses = map(time_series, ses, h = 20))


forecasts %>%
  mutate(forecast_sweep = map(holt, sw_sweep)) %>%
  unnest(forecast_sweep) %>%
  ggplot(aes(index, total_production)) +
  geom_line() +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80), alpha = .2) +
  facet_wrap(~ species)


fcast_holt <- holt(time_series, h = 50)

fcast_holt_damped <- holt(time_series, damped = TRUE, h = 50)

fcast_ses <- ses(time_series, h = 50)

autoplot(time_series) +
  autolayer(fcast_holt, series="Holt", PI = FALSE) +
  autolayer(fcast_holt_damped, series="Holt (Damped)", PI = FALSE) +
  autolayer(fcast_ses, series="SES", PI = FALSE)