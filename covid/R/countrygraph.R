#' Generation of a graph for any country's number of new COVID-19 cases overlayed with Generalized Linear Mixed Model
#' 
#' This function creates a graph for user specified country that displays number of new cases
#' of COVID-19 from their baseline day of 50 cases confirmed until April 3, 2020.
#' An option to generate the predictions for the next 8 days is present as well.
#' Generalized Linear Mixed Model is graphed over the same period to show accuracy of 
#' model in explaining number of new cases for that country.
#' 
#' @param Country_Name A character input corresponding to the country being graphed
#' @param prediction A boolean input to generate the predictions over the next 8 days
#' @param Pred_Day Number of Days from April 3rd the user wants to predict. default is 8 days
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
countrygraph <- function(Country_Name, prediction = FALSE, Pred_Day=NULL){
  
  #ERROR CHECK#
  
  #Check f is a factor variable with two balanced levels
  if(class(Country_Name)!="character")
    stop("'Country_Name' must be a character input")
  
  
  #________________________________________#
  
  
  #FUNCTION#
  
  # Read in data
  dat = readRDS("dat2.rds")
  
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
  
  
  # Find Country ID based on user specified Country Name
  country_info <- dat %>% 
    filter(Country.Region==Country_Name)
  
  country_ID = country_info$ID[1]
  
  # Set fixed effects for GLMM
  fix_mwg <- c(0.844,0.202,-0.005,0.028,0.010,-0.001)
  
  # Get random effects for GLMM
  ran_mwg <- c(mean(gamma2[((country_ID-1)*M+1):(country_ID*M),1]),mean(gamma2[((country_ID-1)*M+1):(country_ID*M),2]),0,0,0,0)
  
  #Combine fixed and random effects for GLMM
  coef_mwg <- fix_mwg + ran_mwg
  
  #Create graph for specified country
  dat2 <- dat %>% 
    filter(Country.Region==Country_Name) %>% 
    mutate(model_mwg=exp(coef_mwg[1]+coef_mwg[2]*day+coef_mwg[3]*day^2+coef_mwg[4]*GHS_Score+coef_mwg[5]*AgeGEQ65+coef_mwg[6]*UrbanPop))
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
    graph_newcases <- ggplot(data=dat2)+
      #True number of new cases
      geom_line(aes(x=day,y=new_cases))+
      #GLMM number of new cases
      geom_line(aes(x=day,y=model_mwg), col= "red")+
      
      labs(title=Country_Name, y="New Cases", x="Days since baseline 50 cases")
  }else{
    graph_newcases <- ggplot()+
      geom_line(data=pred[1:tdayp,],aes(x=day,y=model_mwg), col= "red") +
      # Predictions
      geom_line(data=pred[tdayp:tday,],aes(x=day,y=model_mwg), col= "red", linetype="dashed") +
      geom_line(data=newdat[1:tdayp,], aes(x=day,y=new_cases))+
      # Predictions
      geom_line(data=newdat[tdayp:tday,], aes(x=day,y=new_cases), linetype="dashed")+
      labs(title=Country_Name, y="New Cases", x="Days since baseline 50 cases")
    if (!is_empty(Pred_Day)) {
      pred <- tibble(int=rep(1,Pred_Day)) %>% 
        add_column(day=(1:Pred_Day)) %>% 
        add_column(day2=(1:Pred_Day)^2)%>% 
        add_column(GHS_Score=rep(GHS_Score,Pred_Day))%>% 
        add_column(AgeGEQ65=rep(AgeGEQ65,Pred_Day))%>% 
        add_column(UrbanPop=rep(UrbanPop,Pred_Day))
      pred <- pred %>%
        mutate(model_glmer=exp(coef_glmer[1]+coef_glmer[2]*day+coef_glmer[3]*day^2+coef_glmer[4]*GHS_Score+coef_glmer[5]*AgeGEQ65+coef_glmer[6]*UrbanPop)) %>% 
        mutate(model_mwg=exp(coef_mwg[1]+coef_mwg[2]*day+coef_mwg[3]*day^2+coef_mwg[4]*GHS_Score+coef_mwg[5]*AgeGEQ65+coef_mwg[6]*UrbanPop))
      
      graph_newcases <- ggplot()+
        geom_line(data=pred[1:tdayp,],aes(x=day,y=model_mwg), col= "red") +
        # Predictions
        geom_line(data=pred[tdayp:Pred_Day,],aes(x=day,y=model_mwg), col= "red", linetype="dashed") +
        geom_line(data=newdat[1:tdayp,], aes(x=day,y=new_cases))+
        # Predictions
        geom_line(data=newdat[tdayp:tday,], aes(x=day,y=new_cases), linetype="dashed")+
        labs(title=Country_Name, y="New Cases", x="Days since baseline 50 cases")
    }
  }
  return(graph_newcases)
}