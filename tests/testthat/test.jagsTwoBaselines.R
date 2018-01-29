context("jagsTwoBaselines")

test_that("function return a character class", {
  expect_equal(typeof(jagsTwoBaselines()), "character")
})

test_that("function return a ihnerited oneBaseline class", {
  expect_equal(class(jagsTwoBaselines())[2], "twoBaselines")
})

test_that("function send a warning/error when priors are not correctly written",
          {
            expect_warning(jagsTwoBaselines(TP = "dnorm(3, 1"))

            expect_warning(jagsTwoBaselines(TP = "dnorm(3, 1"),
                           muB = "duni(3,3)")

            expect_warning(jagsTwoBaselines(sigmaB = "dnorn(3, 1)"))

            expect_warning(jagsTwoBaselines(TP = "dnorm(3, 1)",
                                           sigmaDeltaN = "dbeta(1,1"))

            expect_warning(jagsTwoBaselines(TP = "dnorm(3, 1)",
                                            sigmaDeltaN = "dbeta(1, 1)",
                                            alpha = "dbeta(1, 2"))

            expect_error(jagsTwoBaselines(TP = "dnorm(3, 1)",
                                         sigma = "dbeta(1, 1)",
                                         lambda = "5"))
          })
