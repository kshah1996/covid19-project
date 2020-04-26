library(covid)

context("test-countrygraph")

test_that("simple warning for bad input" , {
    dat = covid2
    
    expect_error(countrygraph(21, prediction = FALSE, Pred_Day = 8), "'Country_Name' must be a character input")
    expect_error(countrygraph('US', prediction=FALSE, Pred_Day = twelve), "'Pred_Day' must be an integer input")

})
