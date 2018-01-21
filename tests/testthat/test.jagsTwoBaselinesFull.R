context("jagsTwoBaselinesFull")

test_that("function return a character class", {
  expect_equal(typeof(jagsTwoBaselinesFull()), "character")
})

test_that("function return a ihnerited twoBaselinesFull class", {
  expect_equal(class(jagsTwoBaselinesFull())[2], "twoBaselinesFull")
})
