# Basic S-curve modeling, without taking other countries' information into account

library(tidyverse)
library(stats)
library(lme4)
library(nlme)

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


# Random Effects Attempt (will need to convert from linear to generalized)

# Random intercept and slope
fm2 <- lmer(log(total_cases) ~ day + (day | Country.Region), data = dat)
summary(fm2)
ranef(fm2) # Country estimates of random effect variations

## Poisson GLMM

glmm1 <- glmer(total_cases ~ day + (day | Country.Region), data = dat, family = poisson)
ranef(glmm1)

# NL Mixed Effects
library(nlme)
data <- groupedData(total_cases ~ day | Country.Region, data=dat)
#initVals <- getInitial(y ~ SSlogis(t, Asym, xmid, scal), data = data)
baseModel<- nlme(y ~ SSlogis(day, Asym, xmid, scal),
                 data = data,
                 fixed = list(Asym ~ 1, xmid ~ 1, scal ~ 1),
                 random = Asym + xmid + scal ~ 1,
                 start = c(Asym=50000, xmid = 20, scal=0.1)
)

## NLS
southkorea <- filter(dat, Country.Region == "Korea, South")
usa <- filter(dat, Country.Region == "US")
ggplot(data = southkorea, aes(x = day, y = new_cases)) + geom_line()

bellcurve.model <- function(d, mu, var, x) {
  f <- d*exp(-((x - mu)^2) / (2*var))
  return(f)
}

m1 <- nls(new_cases ~ bellcurve.model(d, mu, var, x = day), start = list(d = 800, mu = 15, var = 25), data = southkorea, trace = TRUE, lower = list(d = 1, mu = 1, var = 0))
plot(southkorea$day, southkorea$new_cases)
lines(southkorea$day, predict(m1))

m2 <- nls(new_cases ~ bellcurve.model(d, mu, var, x = day), start = list(d = 5000, mu = 15, var = 25), data = filter(dat, Country.Region == "US"), trace = TRUE, algorithm = "port", lower = list(d = 1, mu = 1, var = 0))
plot(usa$day, usa$new_cases)
lines(usa$day, predict(m2))

# NLME

baseModel <- nlme(new_cases ~ bellcurve.model(d, mu, var, x = day), 
                  data = dat, 
                  fixed = list(d ~ 1, mu ~ 1, var ~ 1),
                  random = d + mu + var ~ 1|Country.Region,
                  start = list(fixed = c(1000, 20, 20)),
                  na.action = na.omit)

nestedModel <- update(baseModel, fixed = list(d ~ 1, mu ~ 1, var ~ 1), start = fixef(baseModel))
nestedModel2 <- update(nestedModel, fixed = list(d ~ 1, mu ~ 1, var ~ 1), start = fixef(nestedModel))
nestedModel3 <- update(nestedModel2, fixed = list(d ~ 1, mu ~ 1, var ~ 1), start = fixef(nestedModel2))

preds <- predict(nestedModel2)
preds_dat <- data.frame(country = attr(preds, "names"), pred = preds)

