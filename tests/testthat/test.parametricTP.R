context("parametricTP")

test_that("function stops when not using an isotopeData class object", {
  object_list <- list()
  expect_error(parametricTP(object_list))
})
