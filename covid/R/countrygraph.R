#' Generation of a graph for any country's number of new COVID-19 cases overlayed with Generalized Linear Mixed Model
#' 
#' This function creates a graph for user specified country that displays number of new cases
#' of COVID-19 from their baseline day of 50 cases confirmed until April 3, 2020 in red.
#' An option to generate the predictions for the next 8 days is present as well.
#' Generalized Linear Mixed Model is graphed over the same period to show accuracy of 
#' model in explaining number of new cases for that country.
#' 
#' @param Country_Name A character input corresponding to the country being graphed
#' @param prediction A boolean input to generate the predictions over the next 8 days
#' @param Pred_Day A specified number of Days past April 3rd the user wants to predict. Default is 8 days; only used with prediction is TRUE
#' @param glmer_results A boolean input to display results from glmer in blue
#' 
#' @return A graph displaying the number of new cases for specified country in black and the GLMM model of new cases in red if prediction is specified as true, the dotted lines are the predictions.
#' 
#' @examples 
#' 
#' countrygraph(Country_Name='US')
#' countrygraph(Country_Name='US', prediction = TRUE)
#' countrygraph(Country_Name='China')
#' countrygraph(Country_Name='China', prediction = TRUE)
#' 
#' @export
countrygraph <- function(Country_Name, prediction = FALSE, Pred_Day=NULL, glmer_results = FALSE){
  
  #ERROR CHECK#
  
  #Check Country_Name is a character
  if(class(Country_Name)!="character")
    stop("'Country_Name' must be a character input")
  
  #Check Pred_Day is an integer variable
  if(class(Pred_Day)!="numeric" || (class(Pred_Day)=="numeric" && Pred_Day != floor(Pred_Day)))
    stop("'Pred_Day' must be an integer input")
  
  #________________________________________#
  
  
  #FUNCTION#
  
  # Read in data
  dat = readRDS("data/dat2.rds")
  
  # Remove NA data
  dat <- dat %>% mutate(day2 = day^2) %>% drop_na(GHS_Score) %>% drop_na(AgeGEQ65) %>% drop_na(UrbanPop)
  
  # Modify china new_cases day 0 since it was NA previously
  dat[402,5]=548
  dat$ID <- dat %>% group_indices(Country.Region)
  
  # Remove countries with less than 5 cases
  for (i in 1:max(dat$ID)) {
    if (sum(dat$ID==i) < 5) {
      dat<- dat[!(dat$ID==i),]
    }
  }
  dat$ID <- dat %>% group_indices(Country.Region)
  
  # Create unique country list
  order = unique(dat$Country.Region)
  
  # Read in GLMM model results
  gamma <- read.table("longleaf/glmm_mwg_rw_gamma_1.txt", header = F, skip = 1)
  gamma2 <- as.matrix(gamma[,2:3])
  glmm1 <- suppressWarnings(glmer(new_cases ~ day + day2 + GHS_Score + AgeGEQ65 + UrbanPop + (day | Country.Region), data = dat, family = poisson))
  fix_glmer <- fixef(glmm1)
  
  # Find Country ID based on user specified Country Name
  country_info <- dat %>% 
    filter(Country.Region==Country_Name)
  
  country_ID = country_info$ID[1]
  
  # Set fixed effects for GLMM
  fix_mwg <- c(0.844,0.202,-0.005,0.028,0.010,-0.001)
  
  # Get random effects for GLMM
  M <- 1000
  ran_mwg <- c(mean(gamma2[((country_ID-1)*M+1):(country_ID*M),1]),mean(gamma2[((country_ID-1)*M+1):(country_ID*M),2]),0,0,0,0)
  ran_glmer <- c((ranef(glmm1)$Country.Region[country_ID,])[[1]],(ranef(glmm1)$Country.Region[country_ID,])[[2]],0,0,0,0)
  
  
  #Combine fixed and random effects for GLMM
  coef_mwg <- fix_mwg + ran_mwg
  coef_glmer <- fix_glmer + ran_glmer
  #Create graph for specified country
  dat2 <- dat %>% 
    filter(Country.Region==Country_Name) %>% 
    mutate(model_mwg=exp(coef_mwg[1]+coef_mwg[2]*day+coef_mwg[3]*day^2+coef_mwg[4]*GHS_Score+coef_mwg[5]*AgeGEQ65+coef_mwg[6]*UrbanPop)) %>% 
    mutate(model_glmer=exp(coef_glmer[1]+coef_glmer[2]*day+coef_glmer[3]*day^2+coef_glmer[4]*GHS_Score+coef_glmer[5]*AgeGEQ65+coef_glmer[6]*UrbanPop))
  # New data with 8 new days
  newdat = readRDS("dat.rds")
  newdat = newdat %>% filter(Country.Region==Country_Name)
  
  tday = dim(dat2)[1]+8
  tdayp = tday-7
  GHS_Score <- ((dat %>% filter(Country.Region==Country_Name))[1,])$GHS_Score
  AgeGEQ65 <- ((dat %>% filter(Country.Region==Country_Name))[1,])$AgeGEQ65
  UrbanPop <- ((dat %>% filter(Country.Region==Country_Name))[1,])$UrbanPop
  pred <- tibble(int=rep(1,tday)) %>% 
    add_column(day=(1:tday)) %>% 
    add_column(day2=(1:tday)^2)%>% 
    add_column(GHS_Score=rep(GHS_Score,tday))%>% 
    add_column(AgeGEQ65=rep(AgeGEQ65,tday))%>% 
    add_column(UrbanPop=rep(UrbanPop,tday))
  pred <- pred %>%
    mutate(model_glmer=exp(coef_glmer[1]+coef_glmer[2]*day+coef_glmer[3]*day^2+coef_glmer[4]*GHS_Score+coef_glmer[5]*AgeGEQ65+coef_glmer[6]*UrbanPop)) %>% 
    mutate(model_mwg=exp(coef_mwg[1]+coef_mwg[2]*day+coef_mwg[3]*day^2+coef_mwg[4]*GHS_Score+coef_mwg[5]*AgeGEQ65+coef_mwg[6]*UrbanPop))
  
  graph_newcases <- NULL
  if(!prediction){
    if(!glmer_results){
      graph_newcases <- ggplot(data=dat2)+
        #True number of new cases
        geom_line(aes(x=day,y=new_cases))+
        #GLMM number of new cases
        geom_line(aes(x=day,y=model_mwg), col= "red")+
        
        labs(title=Country_Name, y="New Cases", x="Days since baseline 50 cases")
    }else{
      graph_newcases <- ggplot(data=dat2)+
        #True number of new cases
        geom_line(aes(x=day,y=new_cases))+
        #GLMM number of new cases
        geom_line(aes(x=day,y=model_mwg), col= "red")+
        #GLMER number of new cases
        geom_line(aes(x=day,y=model_glmer), col= "blue")
        labs(title=Country_Name, y="New Cases", x="Days since baseline 50 cases")
    }

  }else if (!is_empty(Pred_Day)) {
      tday = dim(dat2)[1]+Pred_Day
      tdayp = tday-(Pred_Day-1)
      
      pred <- tibble(int=rep(1,tday)) %>% 
        add_column(day=(1:tday)) %>% 
        add_column(day2=(1:tday)^2)%>% 
        add_column(GHS_Score=rep(GHS_Score,tday))%>% 
        add_column(AgeGEQ65=rep(AgeGEQ65,tday))%>% 
        add_column(UrbanPop=rep(UrbanPop,tday))
      pred <- pred %>%
        mutate(model_glmer=exp(coef_glmer[1]+coef_glmer[2]*day+coef_glmer[3]*day^2+coef_glmer[4]*GHS_Score+coef_glmer[5]*AgeGEQ65+coef_glmer[6]*UrbanPop)) %>% 
        mutate(model_mwg=exp(coef_mwg[1]+coef_mwg[2]*day+coef_mwg[3]*day^2+coef_mwg[4]*GHS_Score+coef_mwg[5]*AgeGEQ65+coef_mwg[6]*UrbanPop))
      if(!glmer_results){
        graph_newcases <- ggplot()+
          geom_line(data=pred[1:tdayp,],aes(x=day,y=model_mwg), col= "red") +
          # Predictions
          geom_line(data=pred[tdayp:tday,],aes(x=day,y=model_mwg), col= "red", linetype="dashed") +
          geom_line(data=newdat[1:tdayp,], aes(x=day,y=new_cases))+
          # Predictions
          geom_line(data=newdat[tdayp:min(tday,dim(dat2)[1]+8),], aes(x=day,y=new_cases), linetype="dashed")+
          labs(title=Country_Name, y="New Cases", x="Days since baseline 50 cases")
      }else{
        graph_newcases <- ggplot()+
          # glmm
          geom_line(data=pred[1:tdayp,],aes(x=day,y=model_mwg), col= "red") +
          # Predictions
          geom_line(data=pred[tdayp:tday,],aes(x=day,y=model_mwg), col= "red", linetype="dashed") +
          # glmer
          geom_line(data=pred[1:tdayp,],aes(x=day,y=model_glmer), col= "blue") +
          # Predictions
          geom_line(data=pred[tdayp:tday,],aes(x=day,y=model_glmer), col= "blue", linetype="dashed") +
          
          geom_line(data=newdat[1:tdayp,], aes(x=day,y=new_cases))+
          # Predictions
          geom_line(data=newdat[tdayp:min(tday,dim(dat2)[1]+8),], aes(x=day,y=new_cases), linetype="dashed")+
          labs(title=Country_Name, y="New Cases", x="Days since baseline 50 cases")
      }

  }else{
    if(!glmer_results){
      graph_newcases <- ggplot()+
        geom_line(data=pred[1:tdayp,],aes(x=day,y=model_mwg), col= "red") +
        # Predictions
        geom_line(data=pred[tdayp:tday,],aes(x=day,y=model_mwg), col= "red", linetype="dashed") +
        geom_line(data=newdat[1:tdayp,], aes(x=day,y=new_cases))+
        # Predictions
        geom_line(data=newdat[tdayp:tday,], aes(x=day,y=new_cases), linetype="dashed")+
        labs(title=Country_Name, y="New Cases", x="Days since baseline 50 cases")
    }else{
      graph_newcases <- ggplot()+
        # glmm
        geom_line(data=pred[1:tdayp,],aes(x=day,y=model_mwg), col= "red") +
        # Predictions
        geom_line(data=pred[tdayp:tday,],aes(x=day,y=model_mwg), col= "red", linetype="dashed") +
        # glmer
        geom_line(data=pred[1:tdayp,],aes(x=day,y=model_glmer), col= "blue") +
        # Predictions
        geom_line(data=pred[tdayp:tday,],aes(x=day,y=model_glmer), col= "blue", linetype="dashed") +
        
        geom_line(data=newdat[1:tdayp,], aes(x=day,y=new_cases))+
        # Predictions
        geom_line(data=newdat[tdayp:tday,], aes(x=day,y=new_cases), linetype="dashed")+
        labs(title=Country_Name, y="New Cases", x="Days since baseline 50 cases")
    }
  }
  return(graph_newcases)
}