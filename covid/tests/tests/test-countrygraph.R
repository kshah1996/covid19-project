library(covid)


test_that("simple warning for bad input" , {
    dat <- readRDS("dat2.rds")
    
    expect_error(countrygraph(21, prediction = FALSE, Pred_Day = 8), "'Country_Name' must be a character input")
    expect_error(countrygraph('US', prediction=FALSE, Pred_Day = twelve), "'Pred_Day' must be an integer input")

})
