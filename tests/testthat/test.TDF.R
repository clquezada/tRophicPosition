context("TDF")

test_that("functions stops on miscelaneous errors",{

  expect_error(TDF(author = "Posts"))

  expect_warning(TDF(author = "Post", element = "both", type = "all"))

  expect_error(TDF(author = "Mccutchan"))

  expect_error(TDF(author = "McCutchan", element = "both", type = "alll"))

})
