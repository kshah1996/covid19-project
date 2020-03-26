# Basic S-curve modeling, without taking other countries' information into account

library(tidyverse)
library(stats)

dat <- readRDS("dat.rds")

# Works well when a country has finished its curve
china <- filter(dat, Country.Region == "China")
fit <- nls(total_cases ~ SSlogis(day, Asym, xmid, scal), data = china)
plot(china$total_cases ~ china$day)
lines(seq(0, 80, length.out = 100), predict(fit, newdata = data.frame(day = seq(0, 80, length.out = 100))))

# Does not work well when a country has an incomplete curve
# E.g. China, but using only its first 10 days for future predictions
china2 <- filter(dat, Country.Region == "China" & day < 10)
fit2 <- nls(total_cases ~ SSlogis(day, Asym, xmid, scal), data = china2)
plot(china$total_cases ~ china$day)
lines(seq(0, 80, length.out = 100), predict(fit2, newdata = data.frame(day = seq(0, 80, length.out = 100))))