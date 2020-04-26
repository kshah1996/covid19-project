library(covid)
library(testthat)

context("test-getRF")

test_that("simple warning for bad input",{
  expect_error(getRF("USA"))
  expect_error(getRF("US", -3))
  expect_error(getRF("US", 4.5))
})
