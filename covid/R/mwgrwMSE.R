library(tidyverse)
library(stats)
library(lme4)
library(mvtnorm)
#' Calculation of MSE
#' 
#' This function calculates the MSE for a specific country or every country based on their
#' predictions for the 8 days following April, 03, 2020.
#' 
#' @param country_name A character input corresponding to the country, if left empty the function will calculate the MSE for all countries together
#' @param glmer_MSE A boolean determining if the MSE should be calculated for the MCEM GLMM or the glmer function
#' 
#' @return The MSE given the parameters specified
#' 
#' @examples 
#' 
#' mwgrwMSE()
#' mwgrwMSE(country_name='US')
#' 
#' @export
mwgrwMSE <- function(country_name = NULL, glmer_MSE = FALSE){

  ## more data processing
  # dat = readRDS("dat2.rds")
  dat = covid2
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
  
  glmm1 <- glmer(new_cases ~ day + day2 + GHS_Score + AgeGEQ65 + UrbanPop + (day | Country.Region), data = dat, family = poisson)
  fix_glmer <- fixef(glmm1)
  
  # gamma <- read.table("longleaf/glmm_mwg_rw_gamma_1.txt", header = F, skip = 1)
  gamma <- gamma_ll
  gamma2 <- as.matrix(gamma[,2:3])
  M <- 1000
  
  fix_mwg <- c(0.844,0.202,-0.005,0.028,0.010,-0.001)
  
  MSE <- 0
  if(is_empty(country_name)){
    for(i in 1:99){
      country = i
      country_name = order[i]
      
      ran_glmer <- c((ranef(glmm1)$Country.Region[country,])[[1]],(ranef(glmm1)$Country.Region[country,])[[2]],0,0,0,0)
      ran_mwg <- c(mean(gamma2[((country-1)*M+1):(country*M),1]),mean(gamma2[((country-1)*M+1):(country*M),2]),0,0,0,0)
      coef_glmer <- fix_glmer + ran_glmer
      coef_mwg <- fix_mwg + ran_mwg
      
      dat2 <- dat %>% filter(Country.Region==country_name)
      
      
      tday = dim(dat2)[1]+8
      tdayp = tday-7
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
      #newdat = readRDS("dat.rds")
      newdat = covid1
      newdat = newdat %>% filter(Country.Region==country_name)
      if(!glmer_MSE){
        MSE <- MSE + sum((pred[tdayp:tday,]$model_mwg-newdat[tdayp:tday,]$new_cases)^2)
      }else{
        MSE <- MSE + sum((pred[tdayp:tday,]$model_glmer-newdat[tdayp:tday,]$new_cases)^2)
      }
    }
    MSE <- MSE/(length(order)*8)
    return(MSE)
  }else{
    country = which(order==country_name)
    ran_glmer <- c((ranef(glmm1)$Country.Region[country,])[[1]],(ranef(glmm1)$Country.Region[country,])[[2]],0,0,0,0)
    ran_mwg <- c(mean(gamma2[((country-1)*M+1):(country*M),1]),mean(gamma2[((country-1)*M+1):(country*M),2]),0,0,0,0)
    coef_glmer <- fix_glmer + ran_glmer
    coef_mwg <- fix_mwg + ran_mwg
    
    dat2 <- dat %>% filter(Country.Region==country_name)
    
    
    tday = dim(dat2)[1]+8
    tdayp = tday-7
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
    #newdat = readRDS("dat.rds")
    newdat = covid1
    newdat = newdat %>% filter(Country.Region==country_name)
    if(!glmer_MSE){
      MSE <- MSE + sum((pred[tdayp:tday,]$model_mwg-newdat[tdayp:tday,]$new_cases)^2)
    }else{
      MSE <- MSE + sum((pred[tdayp:tday,]$model_glmer-newdat[tdayp:tday,]$new_cases)^2)
    }
    MSE <- MSE/8
    return(MSE)
  }

}