context("jagsTwoBaselines")

test_that("function return a character class", {
  expect_equal(typeof(jagsTwoBaselines()), "character")
})

test_that("function return a ihnerited twoBaselines class", {
  expect_equal(class(jagsTwoBaselines())[2], "twoBaselines")
})
