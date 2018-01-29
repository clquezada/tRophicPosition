context("jagsBayesianModel")

test_that("function return a character class", {
  expect_equal(typeof(jagsBayesianModel()), "character")
})
########################
# oneBaseline model
########################

test_that("function return a ihnerited oneBaseline class", {
  expect_equal(class(jagsBayesianModel(model = "oneBaseline"))[2],
               "oneBaseline")
})

test_that("function send a warning/error when priors are not correctly written",
          {
            expect_warning(jagsBayesianModel(TP = "dnorm(3, 1"),
                           model = "oneBaseline")

            expect_warning(jagsBayesianModel(TP = "dnorm(3, 1"),
                           muB = "duni(3,3)",
                           model = "oneBaseline")

            expect_warning(jagsBayesianModel(sigmaB = "dnorn(3, 1)",
                                             model = "oneBaseline"))

            expect_warning(jagsBayesianModel(TP = "dnorm(3, 1)",
                                             sigmaDeltaN = "dbeta(1,1",
                                             model = "oneBaseline"))

            expect_error(jagsBayesianModel(TP = "dnorm(3, 1)",
                                           sigma = "dbeta(1, 1)",
                                           lambda = "5",
                                           model = "oneBaseline"))
          })

########################
# twoBaselines model
########################

test_that("function return a character class", {
  expect_equal(typeof(jagsBayesianModel(model = "twoBaselines")), "character")
})

test_that("function return a ihnerited twoBaselines class", {
  expect_equal(class(jagsBayesianModel(model = "twoBaselines"))[2],
               "twoBaselines")
})

test_that("function send a warning/error when priors are not correctly written",
          {
            expect_warning(jagsBayesianModel(TP = "dnorm(3, 1"),
                           model = "twoBaselines")

            expect_warning(jagsBayesianModel(TP = "dnorm(3, 1"),
                           alpha = "duni(3,3)",
                           model = "twoBaselines")

            expect_warning(jagsBayesianModel(alpha = "dnorn(3, 1)",
                                             model = "twoBaselines"))

            expect_warning(jagsBayesianModel(TP = "dnorm(3, 1)",
                                             alpha = "dbeta(1,1",
                                             model = "twoBaselines"))

            expect_error(jagsBayesianModel(TP = "dnorm(3, 1)",
                                           alpha = "dbeta(1, 1)",
                                           lambda = "5",
                                           model = "twoBaselines"))
          })

########################
# twoBaselinesFull model
########################

test_that("function return a character class", {
  expect_equal(typeof(jagsBayesianModel(model = "twoBaselinesFull")), "character")
})

test_that("function return a ihnerited twoBaselinesFull class", {
  expect_equal(class(jagsBayesianModel(model = "twoBaselinesFull"))[2],
               "twoBaselinesFull")
})

test_that("function send a warning/error when priors are not correctly written",
          {
            expect_warning(jagsBayesianModel(TP = "dnorm(3, 1"),
                           model = "twoBaselinesFull")

            expect_warning(jagsBayesianModel(TP = "dnorm(3, 1"),
                           alpha = "duni(3,3)",
                           model = "twoBaselinesFull")

            expect_warning(jagsBayesianModel(alpha = "dnorn(3, 1)",
                                             model = "twoBaselinesFull"))

            expect_warning(jagsBayesianModel(TP = "dnorm(3, 1)",
                                             alpha = "dbeta(1,1",
                                             model = "twoBaselinesFull"))

            expect_error(jagsBayesianModel(TP = "dnorm(3, 1)",
                                           alpha = "dbeta(1, 1)",
                                           lambda = "5",
                                           model = "twoBaselinesFull"))
          })
