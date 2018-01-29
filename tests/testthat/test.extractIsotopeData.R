context("extractIsotopeData")

test_that("functions stops on miscelaneous errors",{

  data("Bilagay")

  expect_error(extractIsotopeData(Bilagay, b1 = "Benthic_BL",
                                  b2 = "Pelagic_BL", baselineColumn = "FG",
                                  consumersColumn = "Spp",
                                  groupsColumn = "Location",
                                  d13C = "d13c", d15N = "d15N"))

  expect_error(extractIsotopeData(Bilagay, b1 = "Benthic_BL",
                                  b2 = "Pelagic_BL", baselineColumn = "FG",
                                  consumersColumn = "Spp",
                                  groupsColumn = "Location",
                                  d13C = "d13C", d15N = "d15n"))

  expect_error(extractIsotopeData(Bilagay, b1 = "Benthic_BL",
                                  b2 = "Pelagic_BL", baselineColumn = "NULL",
                                  consumersColumn = "Spp",
                                  groupsColumn = "Location",
                                  d13C = "d13C", d15N = "d15N"))

  expect_error(extractIsotopeData(Bilagay, b1 = "Benthic_BL",
                                  b2 = "Pelagic_BL", baselineColumn = "FG",
                                  consumersColumn = "NULL",
                                  groupsColumn = "Location",
                                  d13C = "d13C", d15N = "d15N"))
})
