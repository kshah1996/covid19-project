library(covid)
library(testthat)

context("test-mwgrw")

test_that("simple warning for bad input",{
  dat <- covid3
  
  expect_error(mwgrw(Sigma_gamma = diag(1,3), Z=c(1,2)))
  expect_error(mwgrw(M=1000.5))
  expect_error(mwgrw(M=-1000))
})
