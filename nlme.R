library(tidyverse)
library(stats)
library(lme4)
library(nlme)

dat <- readRDS("dat.rds")
dat_old <- readRDS("dat_old.rds")

# Functional form of new COVID cases
bellcurve.model <- function(d, mu, var, x) {
  f <- d*exp(-((x - mu)^2) / (2*var))
  return(f)
}

# NLME model using old dataset
baseModel <- nlme(new_cases ~ bellcurve.model(d, mu, var, x = day), 
                  data = dat_old, 
                  fixed = list(d ~ 1, mu ~ 1, var ~ 1),
                  random = d + mu + var ~ 1|Country.Region,
                  start = list(fixed = c(1000, 20, 20)),
                  na.action = na.omit)

# Updates to increase accuracy of the model
nestedModel <- update(baseModel, fixed = list(d ~ 1, mu ~ 1, var ~ 1), start = fixef(baseModel))
nestedModel2 <- update(nestedModel, fixed = list(d ~ 1, mu ~ 1, var ~ 1), start = fixef(nestedModel))

# Fixed effects of the model parameters
summary(nestedModel2)

# Random effects of the model parameters by country
ranef(nestedModel2)

# Data manipulation to create USA predictions in the next step
usa <- filter(dat_old, Country.Region == "US")
preds <- predict(nestedModel2)
preds_dat <- data.frame(country = attr(preds, "names"), pred = preds)
preds_dat_usa <- preds_dat %>% 
  filter(country == "US") 
preds_dat_usa <- preds_dat_usa %>%
  mutate(day = as.numeric(rownames(preds_dat_usa)) - 1)

# Model has great predictions on USA data up till March 25, predicting a normal curve with peak new cases at day 29
plot(usa$day, usa$new_cases)
lines(preds_dat_usa$day, preds_dat_usa$pred)


#### Now with new data, the model does not run
baseModel2 <- nlme(new_cases ~ bellcurve.model(d, mu, var, x = day), 
                  data = dat, 
                  fixed = list(d ~ 1, mu ~ 1, var ~ 1),
                  random = d + mu + var ~ 1|Country.Region,
                  start = list(fixed = c(1000, 20, 20)),
                  na.action = na.omit)
