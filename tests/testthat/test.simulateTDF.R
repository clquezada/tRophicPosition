context("simulateTDF")

test_that("function returns a character list", {
  #expect_equal(typeof(simulateTDF()), "list")
  expect_is(simulateTDF(), "list")
})

test_that("function returns a two vector object", {
  expect_length(simulateTDF(), 2)
})

test_that("function returns a named list, whose 1st object is deltaN", {
  expect_identical(names(simulateTDF()[1]), "deltaN")
})

test_that("function returns a named list, whose 2nd object is deltaC", {
  expect_identical(names(simulateTDF()[2]), "deltaC")
})
