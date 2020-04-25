#' Generation of a graph for any country's number of new COVID-19 cases overlayed with a Random Forest Model
#' 
#' This function creates a graph through a random forest prediction framework for any 
#' user-specified country that displays number of new cases
#' of COVID-19 from their baseline day of 50 cases confirmed until April 3, 2020,
#' with further predictions specified by the user.
#' 
#' @param Country_Name A character input corresponding to the country being graphed
#' @param Pred_Day A specified number of Days past from April 3rd the user wants to predict. Default is 8 days.
#' 
#' @return A graph displaying the number of new cases for specified country predicted through a random forest model.
#' 
#' @examples
#' getRF("US", 20)
#' getRF("Korea, South")
#' 
#' @export
getRF <- function(Country_Name, Pred_Day = 8) {
  
  # Read in data
  dat2 <- covid2 %>%
    mutate(new_cases = replace(new_cases, Country.Region == "China" & day==0, 0))
  
  #ERROR CHECK#
  
  #Check Country_Name is a character
  if(class(Country_Name)!="character")
    stop("'Country_Name' must be a character input")
  
  #Check Country_Name valid
  if (!(Country_Name %in% dat2$Country.Region))
    stop("'Country_Name' must be a valid country")
  
  #Check Pred_Day is an integer variable
  if(!is_empty(Pred_Day) && (class(Pred_Day)!="numeric" || (class(Pred_Day)=="numeric" && Pred_Day != floor(Pred_Day))))
    stop("'Pred_Day' must be an integer input")
  
  #________________________________________#
  
  # Run RF model on full dataset
  dat2_rf <- dat2 %>% select(Country.Region, day, GHS_Score, AgeGEQ65, UrbanPop, new_cases) %>%
    drop_na()
  dat2_rf_X <- dat2_rf %>% select(day, GHS_Score, AgeGEQ65, UrbanPop)
  dat2_rf_Y <- dat2_rf %>% select(new_cases)
  library(randomForest)
  rf1 <- randomForest::randomForest(x = dat2_rf_X, y = dat2_rf$new_cases)
  
  # Dataset with RF predictions
  dat2_rf_preds <- dat2_rf %>% mutate(pred = predict(rf1))
  dat2_rf_preds_country <- dat2_rf_preds %>% filter(Country.Region == Country_Name)
  
  # Manually create dataset for future predictions
  day <- 0:(max(dat2_rf_preds_country$day) + Pred_Day)
  GHS_Score <- dat2_rf_preds_country$GHS_Score[1]
  AgeGEQ65 <- dat2_rf_preds_country$AgeGEQ65[1]
  UrbanPop <- dat2_rf_preds_country$UrbanPop[1]
  
  # Finalize dataframe with future predictions
  prediction_df <- data.frame(day, GHS_Score, AgeGEQ65, UrbanPop)
  prediction_df$pred <- predict(rf1, prediction_df)
  prediction_df$new_cases <- c(dat2_rf_preds_country$new_cases, rep(NA, Pred_Day))
  
  # Plot country's cases and predictions
  ggplot(prediction_df, aes(x = day)) +
    geom_point(aes(y = new_cases)) +
    geom_line(aes(y = pred)) +
    labs(x = "Day", y = "New Cases") + 
    ggtitle(paste0(Country_Name, " Predictions (RF)"))
}