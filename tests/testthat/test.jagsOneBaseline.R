context("jagsOneBaseline")

test_that("function return a character class", {
  expect_equal(typeof(jagsOneBaseline()), "character")
})

test_that("function return a ihnerited oneBaseline class", {
  expect_equal(class(jagsOneBaseline())[2], "oneBaseline")
})

test_that("function send a warning/error when priors are not correctly written",
          {
            expect_warning(jagsOneBaseline(TP = "dnorm(3, 1"))

            expect_warning(jagsOneBaseline(TP = "dnorm(3, 1"),
                           muB = "duni(3,3)")

            expect_warning(jagsOneBaseline(sigmaB = "dnorn(3, 1)"))

            expect_warning(jagsOneBaseline(TP = "dnorm(3, 1)",
                                             sigmaDeltaN = "dbeta(1,1"))

            expect_error(jagsOneBaseline(TP = "dnorm(3, 1)",
                                           sigma = "dbeta(1, 1)",
                                           lambda = "5"))
          })
