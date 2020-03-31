library(tidyverse)
library(stats)
library(lme4)

dat <- readRDS("dat.rds")

glmm1 <- glmer(total_cases ~ day + (day | Country.Region), data = dat, family = poisson)
ranef(glmm1)
