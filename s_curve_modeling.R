# Basic S-curve modeling, without taking other countries' information into account

library(tidyverse)
library(stats)
library(lme4)

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






###

library(nlme)
data <- groupedData(total_cases ~ day | Country.Region, data=dat)
#initVals <- getInitial(y ~ SSlogis(t, Asym, xmid, scal), data = data)
baseModel<- nlme(y ~ SSlogis(day, Asym, xmid, scal),
                 data = data,
                 fixed = list(Asym ~ 1, xmid ~ 1, scal ~ 1),
                 random = Asym + xmid + scal ~ 1,
                 start = c(Asym=50000, xmid = 20, scal=0.1)
)
