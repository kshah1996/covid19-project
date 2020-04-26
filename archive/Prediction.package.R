library(tidyverse)
library(stats)
library(lme4)
library(mvtnorm)
library(dplyr)
library(ggplot2)

#' Prediction of Country Covid-19 New Cases 
#' 
#' This function creates a graph for user specified country that displays the
#' predicted number of new cases based on the.
#' 
#' @param country_name A character input corresponding to the country being graphed
#' @param tday number of days from April 3rd 2020 to the day you want to predict
#'
#' @return a graph displaying the number of new cases for specified country in black and the GLMM model of new cases in red 
#' 
#' 
#' 
#' @export


## data processing
dat = readRDS("dat2.rds")
# remove na data
dat <- dat %>% mutate(day2 = day^2) %>% drop_na(GHS_Score) %>% drop_na(AgeGEQ65) %>% drop_na(UrbanPop)
# modify china new_cases day 0 since it was NA previously
dat[402,5]=548
dat$ID <- dat %>% group_indices(Country.Region)

for (i in 1:max(dat$ID)) {
    if (sum(dat$ID==i) < 5) {
        dat<- dat[!(dat$ID==i),]
    }
}

dat$ID <- dat %>% group_indices(Country.Region)

# unique country list
order = unique(dat$Country.Region)

suppressWarnings(glmm1 <- glmer(new_cases ~ day + day2 + GHS_Score + AgeGEQ65 
                                + UrbanPop + (day | Country.Region), data = dat, family = poisson))
fix_glmer <- fixef(glmm1)

gamma <- read.table("longleaf/glmm_mwg_rw_gamma_1.txt", header = F, skip = 1)
gamma2 <- as.matrix(gamma[,2:3])
M <- 1000
mean(gamma2[(49*M+1):(50*M),1])
mean(gamma2[(49*M+1):(50*M),2])

fix_mwg <- c(0.844,0.202,-0.005,0.028,0.010,-0.001)

tday = 48
GHS_Score <- ((dat %>% filter(Country.Region==country_name))[1,])$GHS_Score
AgeGEQ65 <- ((dat %>% filter(Country.Region==country_name))[1,])$AgeGEQ65
UrbanPop <- ((dat %>% filter(Country.Region==country_name))[1,])$UrbanPop
pred <- tibble(int=rep(1,tday)) %>% 
    add_column(day=(1:tday)) %>% 
    add_column(day2=(1:tday)^2)%>% 
    add_column(GHS_Score=rep(GHS_Score,tday))%>% 
    add_column(AgeGEQ65=rep(AgeGEQ65,tday))%>% 
    add_column(UrbanPop=rep(UrbanPop,tday))
pred <- pred %>%
    mutate(model_glmer=exp(coef_glmer[1]+coef_glmer[2]*day+coef_glmer[3]*day^2+coef_glmer[4]*GHS_Score+coef_glmer[5]*AgeGEQ65+coef_glmer[6]*UrbanPop)) %>% 
    mutate(model_mwg=exp(coef_mwg[1]+coef_mwg[2]*day+coef_mwg[3]*day^2+coef_mwg[4]*GHS_Score+coef_mwg[5]*AgeGEQ65+coef_mwg[6]*UrbanPop))
newdat = readRDS("dat.rds")
newdat = newdat %>% filter(Country.Region==country_name)

ggplot()+
    geom_line(data=pred[1:40,],aes(x=day,y=model_mwg), col= "red") +
    geom_line(data=pred[40:tday,],aes(x=day,y=model_mwg), col= "red", linetype="dashed") +
    geom_line(data=newdat[1:40,], aes(x=day,y=new_cases))+
    geom_line(data=newdat[40:48,], aes(x=day,y=new_cases), linetype="dashed")

print(pred)


