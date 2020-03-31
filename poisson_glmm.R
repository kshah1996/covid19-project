library(tidyverse)
library(stats)
library(lme4)

dat <- readRDS("dat.rds")

## Poisson GLMM Model
glmm1 <- glmer(total_cases ~ day + (day | Country.Region), data = dat, family = poisson)
ranef(glmm1)

## Negative Binomial Model 
glmm2 <- glmer.nb(total_cases~day + (day | Country.Region), data = dat)
ranef(glmm2)

## Simple Graphs for a Single Country
country = 21           # Country ID in data set
country_name = 'China' # Country Name
## Only Day as Covariate, Includes Random Slope for Day and Intercept, Outcome: Total Cases
glmm1 <- glmer(total_cases ~ day + (day | Country.Region), data = dat, family = poisson)
rand_ef = ranef(glmm1)$Country.Region[country,]
fix_ef = fixef(glmm1)
coef = c(fix_ef[[1]]+rand_ef[[1]],fix_ef[[2]]+rand_ef[[2]])
dat2 <- dat %>% filter(Country.Region==country_name)
ggplot(data=dat2)+geom_line(aes(x=day,y=total_cases))+geom_line(aes(x=day,y=exp(coef[1]+coef[2]*day)))
## Day and Day^2 as Covariates, Includes Random Slope for Day and Intercept, Outcome: Total Cases
dat <- dat %>% mutate(day2 = day^2)
glmm1 <- glmer(total_cases ~ day + day2 + (day | Country.Region), data = dat, family = poisson)
rand_ef = ranef(glmm1)$Country.Region[country,]
fix_ef = fixef(glmm1)
coef = c(fix_ef[[1]]+rand_ef[[1]],fix_ef[[2]]+rand_ef[[2]],fix_ef[[3]])
dat2 <- dat %>% filter(Country.Region==country_name)
ggplot(data=dat2)+geom_line(aes(x=day,y=total_cases))+geom_line(aes(x=day,y=exp(coef[1]+coef[2]*day+coef[3]*day^2)))
## Day, Day^2, and Day^3 as Covariates, Includes Random Slope for Day and Intercept, Outcome: Total Cases
dat <- dat %>% mutate(day3 = day^3)
glmm1 <- glmer(total_cases ~ day + day2 + day3 + (day | Country.Region), data = dat, family = poisson)
rand_ef = ranef(glmm1)$Country.Region[country,]
fix_ef = fixef(glmm1)
coef = c(fix_ef[[1]]+rand_ef[[1]],fix_ef[[2]]+rand_ef[[2]],fix_ef[[3]],fix_ef[[4]])
dat2 <- dat %>% filter(Country.Region==country_name)
ggplot(data=dat2)+geom_line(aes(x=day,y=total_cases))+geom_line(aes(x=day,y=exp(coef[1]+coef[2]*day+coef[3]*day^2+coef[4]*day^3)))
## Only Day as Covariate, Includes Random Slope for Day and Intercept, Outcome: New Cases
glmm1 <- glmer(new_cases ~ day + (day | Country.Region), data = dat, family = poisson)
rand_ef = ranef(glmm1)$Country.Region[country,]
fix_ef = fixef(glmm1)
coef = c(fix_ef[[1]]+rand_ef[[1]],fix_ef[[2]]+rand_ef[[2]])
dat2 <- dat %>% filter(Country.Region==country_name)
ggplot(data=dat2)+geom_line(aes(x=day,y=new_cases))+geom_line(aes(x=day,y=exp(coef[1]+coef[2]*day)))
## Day and Day^2 as Covariates, Includes Random Slope for Day and Intercept, Outcome: Total Cases
dat <- dat %>% mutate(day2 = day^2)
glmm1 <- glmer(new_cases ~ day + day2 + (day | Country.Region), data = dat, family = poisson)
rand_ef = ranef(glmm1)$Country.Region[country,]
fix_ef = fixef(glmm1)
coef = c(fix_ef[[1]]+rand_ef[[1]],fix_ef[[2]]+rand_ef[[2]],fix_ef[[3]])
dat2 <- dat %>% filter(Country.Region==country_name)
ggplot(data=dat2)+geom_line(aes(x=day,y=new_cases))+geom_line(aes(x=day,y=exp(coef[1]+coef[2]*day+coef[3]*day^2)))
