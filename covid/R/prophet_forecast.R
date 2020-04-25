#' Application of Prophet methods for covid-19 total case prediction
#' 
#' This function creates a prophet model  and predicts future cases for individual countries in the
#' John's Hopkins COVID-19 database. The dataset must also include a capacity population in order to
#' properly run the logistic modeling. The function prints a graph of previously reported cases, the 
#' model determined by prophet, and future predictions as well as a data.frame with predicted cases 
#' included.
#' 
#' @param country A character input corresponding to the country modeled
#' 
#' @param data A data.frame input with country name, total cases reported, date, and total population. 
#' This can be the dataset for all countries or an individual country
#' 
#' @param numPred A numeric input which indicates how many days in the futute predictions should be made.
#'
#' @return A graph displaying the model, previous cases, and predictions and a dataset which adds predicted 
#' counts onto data input to the function
#' 
#' @examples 
#' 
#' forecast("US", dat3, 7)
#' forecast("Korea, S", dat3, 7)
#' 
#' @importFrom prophet prophet
#' @importFrom prophet make_future_dataframe
#' @importFrom prophet predict
#' @importFrom prophet plot
#' 
#' @export

forecast <- function(country, data, numPred){
  #ERROR CHECK#
  #Check country is a string variable
  if(class(country)!="character")
    stop("'country' must be a character input")
  #Check data is a data.frame
  if(class(data)!="data.frame")
    stop("'data' must be a data frame input")
  #Check numPred is a numeric
  if(class(numPred)!="numeric")
    stop("'numPred' must be a numeric input")
  #Check if country listed is in the given dataset
  if ((country %in% unique(data$Country.Region))==FALSE)
    stop("'country' not listed in dataset")
  #________________________________________#
  
  dat <- data[data$Country.Region == country, ]
  prophet_dat <- as.data.frame(dat$date)
  prophet_dat$y <- as.numeric(dat$total_cases)
  prophet_dat$cap <- as.numeric(dat$TotalPop*.01)
  
  colnames(prophet_dat) <- c("ds", "y", "cap")
  
  now <- prophet::prophet(prophet_dat, growth= "logistic")
  future <- prophet::make_future_dataframe(now, periods=numPred)
  future$cap <- rep(prophet_dat[1,3], length(future$ds))
  
  forecast <- prophet::predict(now, future, )
  print(prophet::plot(now, forecast, plot_cap=F, uncertainty = T, ylabel = country) +  prophet::add_changepoints_to_plot(now))
  return(forecast)
}

#dat = readRDS("data/dat2.rds")


