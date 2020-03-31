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