context("getPosteriorMode")

test_that("functions stops on miscelaneous errors",{
  # obj1 <- list()
  # expect_error(getPosteriorMode(obj1))

  obj2 <- NULL
  expect_error(getPosteriorMode(obj2))

  obj3 <- c("string1", "string2")
  expect_error(getPosteriorMode(obj3))

  })
