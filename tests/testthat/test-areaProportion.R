test_that("areaProportion.R: the area proportion is not correctly calculated.", {
  library(data.table)
  library(testthat)
  thedata <- data.table(bearing = c(seq(0, 359, by = 45), 30),
                        distance = c(rep(c(20, 28.2847), 4), 5))

  thedata[, calculatedProportion := areaProportion(bearing, distance, radius = 10,
                                                   baseShape = "rectangle",
                                                   baseCorners = list(c(-20, 20), c(20, 20),
                                                                      c(20, -20), c(-20, -20)))]
  thedata[, calculatedProportion := round(calculatedProportion, 2)]

  expect_equal(thedata$calculatedProportion,
               c(rep(c(0.5, 0.25), 4), 1))
  rm(thedata)
  thedata <- data.table(bearing = c(sample(1:360, 10), 0),
                        distance = c(rep(20, 10), 0))

  thedata[, calculatedProportion := areaProportion(bearing, distance, radius = 20,
                                                   baseShape = "circle",
                                                   baseRadius = 20)]
  thedata[, calculatedProportion := round(calculatedProportion, 2)]

  expect_equal(thedata$calculatedProportion,
               c(rep(0.39, 10), 1))

})
