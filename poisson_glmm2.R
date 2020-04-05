library(tidyverse)
library(stats)
library(lme4)
dat <- readRDS("dat2.rds")

dat <- dat %>% mutate(day2 = day^2) %>% drop_na(AgeGEQ65) %>% drop_na(UrbanPop)

glmm1 <- glmer(new_cases ~ day + day2 + GHS_Score + AgeGEQ65 + UrbanPop + (day | Country.Region), data = dat, family = poisson)

country = 103
country_name = 'US'
rand_ef = ranef(glmm1)$Country.Region[country,]
fix_ef = fixef(glmm1)
coef = c(fix_ef[[1]]+rand_ef[[1]],fix_ef[[2]]+rand_ef[[2]],fix_ef[[3]],fix_ef[[4]],fix_ef[[5]],fix_ef[[6]])
dat2 <- dat %>% 
  filter(Country.Region==country_name) %>% 
  mutate(model=exp(coef[1]+coef[2]*day+coef[3]*day^2+coef[[4]]*GHS_Score+fix_ef[[5]]*AgeGEQ65+fix_ef[[6]]*UrbanPop))
ggplot(data=dat2)+geom_line(aes(x=day,y=new_cases))+geom_line(aes(x=day,y=model))


country = 23
country_name = 'China'
rand_ef = ranef(glmm1)$Country.Region[country,]
fix_ef = fixef(glmm1)
coef = c(fix_ef[[1]]+rand_ef[[1]],fix_ef[[2]]+rand_ef[[2]],fix_ef[[3]],fix_ef[[4]],fix_ef[[5]],fix_ef[[6]])
dat2 <- dat %>% 
  filter(Country.Region==country_name) %>% 
  mutate(model=exp(coef[1]+coef[2]*day+coef[3]*day^2+coef[[4]]*GHS_Score+fix_ef[[5]]*AgeGEQ65+fix_ef[[6]]*UrbanPop))
ggplot(data=dat2)+geom_line(aes(x=day,y=new_cases))+geom_line(aes(x=day,y=model))

country = 54
country_name = 'Korea, South'
rand_ef = ranef(glmm1)$Country.Region[country,]
fix_ef = fixef(glmm1)
coef = c(fix_ef[[1]]+rand_ef[[1]],fix_ef[[2]]+rand_ef[[2]],fix_ef[[3]],fix_ef[[4]],fix_ef[[5]],fix_ef[[6]])
dat2 <- dat %>% 
  filter(Country.Region==country_name) %>% 
  mutate(model=exp(coef[1]+coef[2]*day+coef[3]*day^2+coef[[4]]*GHS_Score+fix_ef[[5]]*AgeGEQ65+fix_ef[[6]]*UrbanPop))
ggplot(data=dat2)+geom_line(aes(x=day,y=new_cases))+geom_line(aes(x=day,y=model))


glmm2 <- glmer.nb(new_cases ~ day + day2 + GHS_Score + AgeGEQ65 + UrbanPop + (day | Country.Region), data = dat)
country = 23
country_name = 'China'
rand_ef = ranef(glmm2)$Country.Region[country,]
fix_ef = fixef(glmm2)
coef = c(fix_ef[[1]]+rand_ef[[1]],fix_ef[[2]]+rand_ef[[2]],fix_ef[[3]],fix_ef[[4]],fix_ef[[5]],fix_ef[[6]])
dat2 <- dat %>% 
  filter(Country.Region==country_name) %>% 
  mutate(model=exp(coef[1]+coef[2]*day+coef[3]*day^2+coef[[4]]*GHS_Score+fix_ef[[5]]*AgeGEQ65+fix_ef[[6]]*UrbanPop))
ggplot(data=dat2)+geom_line(aes(x=day,y=new_cases))+geom_line(aes(x=day,y=model))

