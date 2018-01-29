context("loadIsotopeData")

test_that("functions stops on miscelaneous errors",{

  data(Bilagay)

  expect_error(
  loadIsotopeData(df = Bilagay, consumer = "Bilagay", consumersColumn = "FG",
                 group = c("CHI", "COL"), groupsColumn = "Location",
                 b1 = "Benthic_BL", b2 = "Pelagic_BL", baselineColumn = "NULL")
  )

  expect_error(
    loadIsotopeData(df = Bilagay, consumer = "Bilagay", consumersColumn = NULL,
                    group = c("CHI", "COL"), groupsColumn = "Location",
                    b1 = "Benthic_BL", b2 = "Pelagic_BL", baselineColumn = "FG")
  )

  expect_message(
    loadIsotopeData(df = Bilagay, species = "Bilagay", consumersColumn = "FG",
                    group = c("CHI", "COL"), groupsColumn = "Location",
                    b1 = "Benthic_BL", b2 = "Pelagic_BL", baselineColumn = "FG")
  )
  })
