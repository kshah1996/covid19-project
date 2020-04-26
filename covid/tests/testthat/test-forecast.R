library(covid)
library(testthat)

context("test-forecast")

test_that("simple warning for bad input",{
  dat <- covid3
  
  expect_error(forecast("USA", dat, 7))
  expect_error(forecast("US", dat, "7.6"))
  expect_error(forecast(US, dat, 7))
})
