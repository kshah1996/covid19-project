library(tidyverse)

global_confirmed <- read.csv("covid19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", header = TRUE)

# Group by country, summing number of deaths
global_confirmed <- global_confirmed %>%
  select(-c(Province.State, Lat, Long)) %>%
  group_by(Country.Region) %>%
  summarise_all(funs(sum))