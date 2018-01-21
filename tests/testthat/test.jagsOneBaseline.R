context("jagsOneBaseline")

test_that("function return a character class", {
  expect_equal(typeof(jagsOneBaseline()), "character")
})

test_that("function return a ihnerited oneBaseline class", {
  expect_equal(class(jagsOneBaseline())[2], "oneBaseline")
})
