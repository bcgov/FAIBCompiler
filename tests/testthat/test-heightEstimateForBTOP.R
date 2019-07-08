test_that("heightEstimateForBTOP.R: tree height for broken top trees is not correctly estimated.", {
  library(data.table)
  library(testthat)
  projectedH <- -5
  expect_error(heightEstimateForBTOP_H(projectedH))
  projectedH <- as.numeric(NA)
  expect_warning(heightEstimateForBTOP_H(projectedH))
  projectedH <- sample(1:100000, size = 1000, replace = TRUE)
  estimatedH <- heightEstimateForBTOP_H(projectedH)
  expect_equal(projectedH, estimatedH)
  
  ### check error
  expect_error(heightEstimateForBTOP_D(heightBTOP = as.numeric(NA),
                                       DIBBTOP = 10,
                                       DBH = 15,
                                       taperEquationForm = "KBEC",
                                       FIZorBEC = "ESSF",
                                       species = "B",
                                       volMultiplier = 1,
                                       SASOriginal = FALSE))
  a <- sample(1:50, size = 1)
  expect_error(heightEstimateForBTOP_D(heightBTOP = as.numeric(-a),
                                       DIBBTOP = 10,
                                       DBH = 15,
                                       taperEquationForm = "KBEC",
                                       FIZorBEC = "ESSF",
                                       species = "B",
                                       volMultiplier = 1,
                                       SASOriginal = FALSE))
  
  expect_error(heightEstimateForBTOP_D(heightBTOP = as.numeric(10),
                                       DIBBTOP = as.numeric(NA),
                                       DBH = 15,
                                       taperEquationForm = "KBEC",
                                       FIZorBEC = "ESSF",
                                       species = "B",
                                       volMultiplier = 1,
                                       SASOriginal = FALSE))
  
  expect_error(heightEstimateForBTOP_D(heightBTOP = as.numeric(10),
                                       DIBBTOP = as.numeric(-a),
                                       DBH = 15,
                                       taperEquationForm = "KBEC",
                                       FIZorBEC = "ESSF",
                                       species = "B",
                                       volMultiplier = 1,
                                       SASOriginal = FALSE))
  
  expect_error(heightEstimateForBTOP_D(heightBTOP = as.numeric(10),
                                       DIBBTOP = as.numeric(5),
                                       DBH = as.numeric(NA),
                                       taperEquationForm = "KBEC",
                                       FIZorBEC = "ESSF",
                                       species = "B",
                                       volMultiplier = 1,
                                       SASOriginal = FALSE))
  
  expect_error(heightEstimateForBTOP_D(heightBTOP = as.numeric(10),
                                       DIBBTOP = as.numeric(5),
                                       DBH = -a,
                                       taperEquationForm = "KBEC",
                                       FIZorBEC = "ESSF",
                                       species = "B",
                                       volMultiplier = 1,
                                       SASOriginal = FALSE))
  
  ## check warnning
  # height check
  a <- sample(seq(0.1, 1.3, by = 0.1), size = 1)
  expect_warning(heightEstimateForBTOP_D(heightBTOP = a,
                                         DIBBTOP = 5,
                                         DBH = 10,
                                         taperEquationForm = "KBEC",
                                         FIZorBEC = "ESSF",
                                         species = "B",
                                         volMultiplier = 1,
                                         SASOriginal = FALSE))
  
  a <- sample(seq(60.1, 100, by = 0.1), size = 1)
  expect_warning(heightEstimateForBTOP_D(heightBTOP = a,
                                         DIBBTOP = 5,
                                         DBH = 10,
                                         taperEquationForm = "KBEC",
                                         FIZorBEC = "ESSF",
                                         species = "B",
                                         volMultiplier = 1,
                                         SASOriginal = FALSE))
  
  a <- sample(seq(1.4, 60, by = 0.1), size = 1)
  DBH <- sample(seq(4, 100, by = 0.1), size = 1)
  dib <- DBH+sample(seq(0.1, 3.9, by = 0.1), size = 1)
  expect_warning(heightEstimateForBTOP_D(heightBTOP = a,
                                         DIBBTOP = dib,
                                         DBH = DBH,
                                         taperEquationForm = "KBEC",
                                         FIZorBEC = "ESSF",
                                         species = "B",
                                         volMultiplier = 1,
                                         SASOriginal = FALSE))
  
  #### check the estimated Height
  ## for species AC in various bec zones
  estimatedH <- round(heightEstimateForBTOP_D(heightBTOP = c(16.6, 11.2, 9.4, 4.6, 16.5, 6.4, 24.2, 3.4),
                                              DIBBTOP = c(35.51, 1.87, 7.48, 1.87, 37.38, 1.87, 26.17, 9.35),
                                              DBH = c(50.4, 11.5, 18.7, 7.5, 102.1, 11.7, 84, 15.6),
                                              taperEquationForm = "KBEC",
                                              FIZorBEC = c("BWBS", "CWH", "ESSF", "ICH", "IDF", "MS", "SBS", "SWB"),
                                              species = "AC",
                                              volMultiplier = 1,
                                              SASOriginal = FALSE), 1)
  expect_equal(estimatedH, 
               c(48.4, 12.2, 14.0,  5.6, 25.8,  7.7, 33.7,  7.7)) ## sas esimates 48.6 12.2 14.1  5.8 25.9  7.8 33.8  7.8
  
  ## for species AT in various bec zones
  estimatedH <- round(heightEstimateForBTOP_D(heightBTOP = c(9.7, 1.8, 14, 17, 20.9, 1.4, 1.9, 1.7),
                                              DIBBTOP = c(2.8, 24.3, 2.8, 26.17, 5.61, 18.69, 8.41, 16.82),
                                              DBH = c(10.2, 26.5, 12.8, 55.8, 41, 20.8, 10.2, 21),
                                              taperEquationForm = "KBEC",
                                              FIZorBEC = c("BWBS", "ESSF", "ICH", "IDF", "MS", "SBPS", "SBS", "SWB"),
                                              species = "AT",
                                              volMultiplier = 1,
                                              SASOriginal = FALSE), 1)
  expect_equal(estimatedH,
               c(11.9, 16.4, 16.1, 27.2, 23.7, 16.7, 8.5, 6.6)) 
  ## sas outputs 11.9 16.5 16.1 27.2 23.8 16.7  8.5  6.7
  
  ## for species B in various bec zones
  estimatedH <- round(heightEstimateForBTOP_D(heightBTOP = c(4.4, 3.9, 7.9, 7.3, 17, 29.8, 
                                                             20.4, 5.5, 2.3, 12.4, 4.6),
                                              DIBBTOP = c(16.82, 10.28, 43.93, 10.28, 20.56, 
                                                          3.74, 5.61, 5.61, 8.41, 3.74, 2.8),
                                              DBH = c(29.1, 16.4, 51.1, 26.8, 30.2, 32.5,
                                                      51, 9.4, 11, 19.3, 7.8),
                                              taperEquationForm = "KBEC",
                                              FIZorBEC = c("AT", "BWBS", "CWH", "ESSF", "ICH",
                                                           "IDF", "MH", "MS", "SBPS", "SBS", "SWB"),
                                              species = "B",
                                              volMultiplier = 1,
                                              SASOriginal = FALSE), 1)
  expect_equal(estimatedH, 
               c(c(9.7, 7.4, 66.5, 10.4, 39.6, 31.0, 21.9, 10.3, 4.9, 14.0, 5.7))) 
  ## sas outputs 9.9  7.4 66.7 10.5 39.6 31.1 22.0 10.4  5.0 14.1  5.7
  
  ## for species C in various bec zones
  estimatedH <- round(heightEstimateForBTOP_D(heightBTOP = c(22.7, 1.5, 13.8, 25.2, 33.1, 1.7),
                                              DIBBTOP = c(4.67, 107.48, 5.61, 2.8, 2.8, 2.8),
                                              DBH = c(107.3, 122.6, 16.3, 38.7, 110.2, 5.5),
                                              taperEquationForm = "KBEC",
                                              FIZorBEC = c("CWH", "ESSF", "ICH", "IDF", "MH", "MS"),
                                              species = "C",
                                              volMultiplier = 1,
                                              SASOriginal = FALSE), 1)
  expect_equal(estimatedH,
               c(23.8,  2.5, 16.9, 26.9, 34.0,  2.1)) # 23.9  2.5 17.0 27.0 34.0  2.3
  
  ## for species D in various bec zones
  estimatedH <- round(heightEstimateForBTOP_D(heightBTOP = c(9.9, 8.8, 4.4, 2.1, 1.4, 2.6),
                                              DIBBTOP = c(15.89, 6.54, 7.48, 13.08, 4.67, 4.67),
                                              DBH = c(32.8, 13.1, 12, 14.1, 5.1, 7.9),
                                              taperEquationForm = "KBEC",
                                              FIZorBEC = c("CDF", "CWH", "ESSF", "IDF", "MH", "SBS"),
                                              species = "D",
                                              volMultiplier = 1,
                                              SASOriginal = FALSE), 1)
  expect_equal(estimatedH,
               c(16.6, 13.5,  8.7, 66.5, 66.5,  4.7)) ## sas outputs 16.7 13.5  8.8 66.7 51.1  4.6
  
  
  ## for species E in various bec zones
  estimatedH <- round(heightEstimateForBTOP_D(heightBTOP = c(2, 2.9, 1.6, 12.8, 16, 3.4, 3, 7.5, 1.5),
                                              DIBBTOP = c(8.41, 4.67, 11.21, 5.61, 7.48, 6.54, 
                                                          5.61, 14.95, 2.8),
                                              DBH = c(10.8, 7.2, 13.8, 18.6, 23.9, 10.2, 6.1, 23.9, 4.1),
                                              taperEquationForm = "KBEC",
                                              FIZorBEC = c("BWBS", "CWH", "ESSF", "ICH", "IDF",
                                                           "MS", "SBPS", "SBS", "SWB"),
                                              species = "E",
                                              volMultiplier = 1,
                                              SASOriginal = FALSE), 1)
  expect_equal(estimatedH,
               c(6.7,  6.9,  3.9, 16.8, 21.2,  8.2, 66.5, 17.5,  3.0)) 
  ## sas outputs 6.8  7.0  4.0 16.9 21.4  8.2 66.7 17.6  3.0
  
  
  ## for species F in various bec zones
  estimatedH <- round(heightEstimateForBTOP_D(heightBTOP = c(9.3, 22.5, 5.4, 10.9, 13.4, 6.4, 11.1, 
                                                             10.3, 5.4, 2.3, 9, 4.3),
                                              DIBBTOP = c(14.95, 7.48, 46.73, 1.87, 30.84, 1.1, 
                                                          24.3, 1.87, 3.74, 11.21, 23.36, 35.51),
                                              DBH = c(18, 45.5, 57, 33.4, 41.6, 9.1, 54.1, 
                                                      11.9, 9.9, 15.6, 32.8, 43.5),
                                              taperEquationForm = "KBEC",
                                              FIZorBEC = c("BWBS", "CDF", "CWH", "ESSF", "ICH", "IDF", "MH", 
                                                           "MS", "PP", "SBPS", "SBS", "SWB"),
                                              species = "F",
                                              volMultiplier = 1,
                                              SASOriginal = FALSE), 1)
  expect_equal(estimatedH,
               c(66.5, 25.0, 66.5, 11.5, 45.8,  7.0, 17.4, 11.3,  7.7,  8.4, 29.7, 66.5))
  ## sas outputs 66.7 25.1 66.7 11.5 45.9  7.0 17.6 11.4  7.8  8.4 29.7 66.7
  
  ## for species H in various bec zones
  estimatedH <- round(heightEstimateForBTOP_D(heightBTOP = c(16.7, 12, 2.2, 1.6, 33.3, 9.9, 1.7, 9.7),
                                              DIBBTOP = c(9.35, 18.69, 3.74, 4.67, 7.48, 10.28, 12.15, 14.02),
                                              DBH = c(62.8, 43.1, 6.2, 6, 68.7, 38, 13.5, 35),
                                              taperEquationForm = "KBEC",
                                              FIZorBEC = c("AT", "CDF", "CWH", "ESSF", "ICH", "MH",
                                                           "MS", "SBS"),
                                              species = "H",
                                              volMultiplier = 1,
                                              SASOriginal = FALSE), 1)
  expect_equal(estimatedH,
               c(18.9, 18.1,  3.5,  3.1, 35.8, 12.8, 66.5, 14.5)) ## 
  ## sas outputs 18.9 18.2  3.6  3.1 35.9 13.0 51.9 14.5
  
  ## for species L in various bec zones
  estimatedH <- round(heightEstimateForBTOP_D(heightBTOP = c(20, 37.4, 7.2, 28.3, 6.5, 11.3),
                                              DIBBTOP = c(2.8, 10.28, 26.17, 3.74, 1.1, 13.08),
                                              DBH = c(17.1, 74.1, 31.2, 43.3, 5, 62),
                                              taperEquationForm = "KBEC",
                                              FIZorBEC = c("BWBS", "ESSF", "ICH", "IDF", "MS", "SBS"),
                                              species = "L",
                                              volMultiplier = 1,
                                              SASOriginal = FALSE), 1)
  expect_equal(estimatedH,
               c(21.5, 41.0, 66.5, 30.0,  7.6, 13.3))
  ## sas outputs 21.6 41.0 66.7 30.0  7.6 13.3
  
  ## for species MB in various bec zones
  estimatedH <- round(heightEstimateForBTOP_D(heightBTOP = 15.8,
                                              DIBBTOP = 23.36,
                                              DBH = 92.7,
                                              taperEquationForm = "KBEC",
                                              FIZorBEC = "CWH",
                                              species = "MB",
                                              volMultiplier = 1,
                                              SASOriginal = FALSE), 1)
  expect_equal(estimatedH, 22) ## sas outputs 22.1
  
  ## for species PA in various bec zones
  estimatedH <- round(heightEstimateForBTOP_D(heightBTOP = c(1.9, 1.9, 19.5, 2.7),
                                              DIBBTOP = c(4.67, 48.60, 2.80, 1.10),
                                              DBH = c(33.6, 54.3, 50.0, 5.8),
                                              taperEquationForm = "KBEC",
                                              FIZorBEC = c("AT", "ESSF", "MH", "MS"),
                                              species = "PA",
                                              volMultiplier = 1,
                                              SASOriginal = FALSE), 1)
  expect_equal(estimatedH, 
               c(2.0, 5.9, 20.3, 3.1)) ## sas outputs 2.0 6.0 20.4 3.1
  
  
  ## for species PL in various bec zones
  estimatedH <- round(heightEstimateForBTOP_D(heightBTOP = c(8.6, 20.6, 15.6, 6.5, 8.9,
                                                             9.6, 9.1, 11.1, 4.9, 6.7),
                                              DIBBTOP = c(1.87, 2.8, 5.61, 3.74, 8.41, 15.89,
                                                          1.1, 1.87, 33.64, 3.74),
                                              DBH = c(16.7, 31.2, 15.2, 14.3, 10.7, 
                                                      21.6, 6.4, 12.6, 38.2, 15),
                                              taperEquationForm = "KBEC",
                                              FIZorBEC = c("AT", "BWBS", "CWH", "ESSF", "ICH",
                                                           "IDF", "MS", "SBPS", "SBS", "SWB"),
                                              species = "PL",
                                              volMultiplier = 1,
                                              SASOriginal = FALSE), 1)
  expect_equal(estimatedH,
               c(9.2, 21.2, 18.5,  7.9, 20.8, 21.1,  9.6, 11.2, 66.5,  7.2))
  ## sas outputs 9.4 21.3 18.7  8.0 20.8 21.1  9.7 11.4 66.7  7.3
  
  ## for species PW in various bec zones
  estimatedH <- round(heightEstimateForBTOP_D(heightBTOP = c(28.3, 4.3, 3.1, 12.2, 25.3, 33),
                                              DIBBTOP = c(14.02, 1.1, 8.41, 9.35, 28.04, 4.67),
                                              DBH = c(59.9, 6, 11.1, 26, 56.2, 52),
                                              taperEquationForm = "KBEC",
                                              FIZorBEC = c("CWH", "ESSF", "ICH", "IDF", "MH", "MS"),
                                              species = "PW",
                                              volMultiplier = 1,
                                              SASOriginal = FALSE), 1)
  expect_equal(estimatedH,
               c(32.9,  4.7,  6.6, 16.3, 41.2, 34.4)) 
  # sas outputs 33.0  4.9  6.6 16.4 41.2 34.4
  
  
  ## for species PY in various bec zones
  estimatedH <- round(heightEstimateForBTOP_D(heightBTOP = c(1.5, 3.4, 13.4),
                                              DIBBTOP = c(15.89, 31.78, 8.41),
                                              DBH = c(18.5, 35.0, 34.8),
                                              taperEquationForm = "KBEC",
                                              FIZorBEC = c("ICH", "IDF", "PP"),
                                              species = "PY",
                                              volMultiplier = 1,
                                              SASOriginal = FALSE), 1)
  expect_equal(estimatedH, 
               c(66.5, 39.6, 15.9)) ## sas outputs 32.8 39.6 16.0
  
  
  ## for species S in various bec zones
  estimatedH <- round(heightEstimateForBTOP_D(heightBTOP = c(19.9, 4.9, 4.1, 12, 4.7, 14.7,
                                                             36, 17.4, 9.7, 15.1, 3.5),
                                              DIBBTOP = c(9.35, 14.02, 55.14, 13.08, 14.02,
                                                          9.35, 3.74, 11.21, 1.1, 7.48, 7.48),
                                              DBH = c(21.7, 15.2, 62, 30.6, 18.6, 26.5, 
                                                      87.5, 23.1, 10.3, 25.8, 11.3),
                                              taperEquationForm = "KBEC",
                                              FIZorBEC = c("AT", "BWBS", "CWH", "ESSF", "ICH", 
                                                           "IDF", "MH", "MS", "SBPS", "SBS", "SWB"),
                                              species = "S",
                                              volMultiplier = 1,
                                              SASOriginal = FALSE), 1)
  expect_equal(estimatedH, 
               c(26.6, 43.5, 66.5, 17.8, 13.5, 19.5, 36.9, 25.2, 10.3, 18.7,  7.0))
  ## sas output 26.7 43.2 66.7 17.9 13.5 19.7 37.0 25.2 10.4 18.8  7.1
  
  ## for species Y in various bec zones
  estimatedH <- round(heightEstimateForBTOP_D(heightBTOP = c(1.5, 15.8, 2.4, 2.5),
                                              DIBBTOP = c(2.80, 20.56, 12.15, 12.15),
                                              DBH = c(4.0, 42.0, 19.1, 18.2),
                                              taperEquationForm = "KBEC",
                                              FIZorBEC = c("AT", "CWH", "ESSF", "MH"),
                                              species = "Y",
                                              volMultiplier = 1,
                                              SASOriginal = FALSE), 1)
  expect_equal(estimatedH,
               c(2.1, 24.4,  4.0,  4.6)) ## sas outputs 2.1 24.4  4.1  4.6
  
  
  
})