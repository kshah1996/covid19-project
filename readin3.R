library(tidyverse)
dataset_folder <- c("country_data/")
datasets <- c("ghs.csv",
              "API_SP.POP.65UP.TO.ZS_DS2_en_csv_v2_887563/API_SP.POP.65UP.TO.ZS_DS2_en_csv_v2_887563.csv",
              "API_SP.URB.TOTL.IN.ZS_DS2_en_csv_v2_888117/API_SP.URB.TOTL.IN.ZS_DS2_en_csv_v2_888117.csv",
              "API_SP.POP.TOTL_DS2_en_csv_v2_936048/API_SP.POP.TOTL_DS2_en_csv_v2_936048.csv")

covid_dat <- readRDS("dat.rds")
# GHS Data
dat <- read_csv(paste(dataset_folder,datasets[1],sep=""))
dat <- dat %>% rename(Country.Region = Country)
dat[which((dat$Country.Region)=="United States"),1] = "US"
dat[which((dat$Country.Region)=="South Korea"),1] = "Korea, South"
covid_dat <- left_join(covid_dat, dat, by= "Country.Region")

# World Bank Data
dat <- read_csv(paste(dataset_folder,datasets[2],sep=""),skip=4)
dat <- dat %>% select("Country Name", "2018")
names(dat)[names(dat) == "Country Name"] <- "Country.Region"
names(dat)[names(dat) == "2018"] <- "AgeGEQ65"
dat[which((dat$Country.Region)=="United States"),1] = "US"
dat[which((dat$Country.Region)=="Korea, Rep."),1] = "Korea, South"
covid_dat <- left_join(covid_dat, dat, by= "Country.Region")

dat <- read_csv(paste(dataset_folder,datasets[3],sep=""),skip=4)
dat <- dat %>% select("Country Name", "2018")
names(dat)[names(dat) == "Country Name"] <- "Country.Region"
names(dat)[names(dat) == "2018"] <- "UrbanPop"
dat[which((dat$Country.Region)=="United States"),1] = "US"
dat[which((dat$Country.Region)=="Korea, Rep."),1] = "Korea, South"
covid_dat <- left_join(covid_dat, dat, by= "Country.Region")

# Addition of total population data from World Bank
dat <- read_csv(paste(dataset_folder, datasets[4], sep=""), skip=4)
dat <- dat %>% select("Country Name", "2018")
names(dat)[names(dat) == "Country Name"] <- "Country.Region"
names(dat)[names(dat) == "2018"] <- "TotalPop"
dat[which((dat$Country.Region)=="United States"),1] = "US"
dat[which((dat$Country.Region)=="Korea, Rep."),1] = "Korea, South"
covid_dat <- left_join(covid_dat, dat, by= "Country.Region")

saveRDS(covid_dat, "dat3.rds")
