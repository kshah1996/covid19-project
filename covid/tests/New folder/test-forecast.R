library(covid)

context("test-forecast")

test_that("simple warning for bad input"){
  dat <- readRDS("dat3.rds")
  
  expect_error(forecast("USA", dat, as.integer(7)))
  expect_error(forecast("US", dat, 7.6))
  expect_error(forecast(US, dat, as.integer(7)))
}
