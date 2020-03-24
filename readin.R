library(tidyverse)
select <- dplyr::select

confirmed <- read.csv("covid19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", header = TRUE)
deaths <- read.csv("covid19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", header = TRUE)

# Storing column names for dates columns
dates_confirmed <- names(confirmed)[-c(1,2,3,4)]
dates_deaths <- names(confirmed)[-c(1,2,3,4)]

# Number of cases needed to begin baseline time point
cases_threshold <- 50

# Groups by country, summing number of confirmed cases. Gathers date variables into a single
# column for date. Converts dates to date format. Creates new_cases column by lagging over
# total_cases.
global_confirmed <- confirmed %>%
  select(-c(Province.State, Lat, Long)) %>%
  group_by(Country.Region) %>%
  summarise_all(funs(sum)) %>%
  gather(key = "date", value = "total_cases", dates_confirmed) %>%
  mutate(date = as.Date(gsub("[.]", "/",gsub("X", "", date)), format = "%m/%d/%y")) %>%
  arrange(Country.Region, date) %>%
  group_by(Country.Region) %>%
  mutate(new_cases = total_cases - lag(total_cases))

# Similar to above, but for deaths
global_deaths <- deaths %>%
  select(-c(Province.State, Lat, Long)) %>%
  group_by(Country.Region) %>%
  summarise_all(funs(sum)) %>%
  gather(key = "date", value = "total_deaths", dates_deaths) %>%
  mutate(date = as.Date(gsub("[.]", "/",gsub("X", "", date)), format = "%m/%d/%y")) %>%
  arrange(Country.Region, date) %>%
  group_by(Country.Region) %>%
  mutate(new_deaths = total_deaths - lag(total_deaths))

# Joins confirmed case and deaths data. Converts dates to day 0, 1, 2, etc. past specified
# case threshold. 
dat <- global_confirmed %>%
  left_join(global_deaths, by = c("Country.Region", "date")) %>%
  mutate(keep = ifelse(total_cases >= 50, "yes", "no")) %>%
  group_by(Country.Region, keep) %>%
  mutate(day = row_number()-1) %>%
  filter(keep == "yes") %>%
  ungroup() %>%
  select(Country.Region, date, day, total_cases, new_cases, total_deaths, new_deaths)
