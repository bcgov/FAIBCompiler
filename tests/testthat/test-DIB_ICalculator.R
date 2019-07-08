test_that("DIB_ICalculator.R: diameter inside bark is not correctly calculated.", {
  library(data.table)
  library(testthat)

  ### check warning for Height, outpus NA
  expect_warning(DIB_ICalculator(taperEquationForm = "KBEC",
                                 FIZorBEC = "PP",
                                 species = "B",
                                 height_I = 5,
                                 heightTotal = sample(c(NA, 0), size = 1),
                                 DBH = 5,
                                 volMultiplier = 1))
  expect_equal(suppressWarnings(DIB_ICalculator(taperEquationForm = "KBEC",
                                                FIZorBEC = "PP",
                                                species = "B",
                                                height_I = 5,
                                                heightTotal = sample(c(NA, 0), size = 1),
                                                DBH = 5,
                                                volMultiplier = 1)),
               as.numeric(NA))

  ### check warning for DBH, outpus NA
  expect_warning(DIB_ICalculator(taperEquationForm = "KBEC",
                                 FIZorBEC = "PP",
                                 species = "B",
                                 height_I = 5,
                                 heightTotal = 10,
                                 DBH = sample(c(NA, 0), size = 1),
                                 volMultiplier = 1))
  expect_equal(suppressWarnings(DIB_ICalculator(taperEquationForm = "KBEC",
                                                FIZorBEC = "PP",
                                                species = "B",
                                                height_I = 5,
                                                heightTotal = 10,
                                                DBH = sample(c(NA, 0), size = 1),
                                                volMultiplier = 1)),
               as.numeric(NA))

  ### check warning for height_i > total height, outpus NA
  expect_warning(DIB_ICalculator(taperEquationForm = "KBEC",
                                 FIZorBEC = "PP",
                                 species = "B",
                                 height_I = 15,
                                 heightTotal = 10,
                                 DBH = 10,
                                 volMultiplier = 1))
  expect_equal(suppressWarnings(DIB_ICalculator(taperEquationForm = "KBEC",
                                                FIZorBEC = "PP",
                                                species = "B",
                                                height_I = 15,
                                                heightTotal = 10,
                                                DBH = 10,
                                                volMultiplier = 1)),
               as.numeric(NA))

  estimatedDIB <- suppressWarnings(round(DIB_ICalculator(taperEquationForm = "KBEC",
                                                         FIZorBEC = c("BWBS", "CDF", "CDF"),
                                                         species = "AC",
                                                         height_I = 1.4, ## this is corresponding to DIB_BH in sas compiler
                                                         ## as the sas compiler using 1.4 in calculating
                                                         ## DIB_BH
                                                         heightTotal = c(32.6, 12.7, 0),
                                                         DBH = c(99.4, 16.2, 10),
                                                         volMultiplier = 1), 4))
  expect_equal(estimatedDIB,
               c(88.5768, 15.2838, NA))



  #### check the estimated DIB for 16 species
  ## for species AC in various bec zones
  estimatedDIB <- round(DIB_ICalculator(taperEquationForm = "KBEC",
                                        FIZorBEC = c("BWBS", "CDF", "CWH", "ESSF", "ICH",
                                                     "IDF", "MS",
                                                     "PP", "SBPS", "SBS", "SWB"),
                                        species = "AC",
                                        height_I = 1.4, ## this is corresponding to DIB_BH in sas compiler
                                        ## as the sas compiler using 1.4 in calculating
                                        ## DIB_BH
                                        heightTotal = c(32.6, 12.7, 45, 24.2, 37.1, 44.3,
                                                        20.6, 33, 30.1,
                                                        39.5, 15.3),
                                        DBH = c(99.4, 16.2, 191.5, 44.1, 121.6, 114.2, 31.6, 55.8,
                                                57.4, 145, 24.8),
                                        volMultiplier = 1), 4)
  expect_equal(estimatedDIB,
               c(88.5768, 15.2838, 173.6924, 38.7407, 108.5873, 109.9922,
                 29.2956, 52.6335, 54.1782, 126.609, 21.0556)) ## The later part is from sas compiler

  ## for species AT in various bec zones
  estimatedDIB <- round(DIB_ICalculator(taperEquationForm = "KBEC",
                                        FIZorBEC = c("AT", "BWBS", "CWH", "ESSF", "ICH", "IDF", "MS",
                                                     "PP", "SBPS", "SBS", "SWB"),
                                        species = "AT",
                                        height_I = 1.4, ## this is corresponding to DIB_BH in sas compiler
                                        ## as the sas compiler using 1.4 in calculating
                                        ## DIB_BH
                                        heightTotal = c(27.7, 32.1, 25.7, 25.6, 31.2, 21.2, 31.7,
                                                        5.8, 27.3,
                                                        33, 21.4),
                                        DBH = c(37, 81.5, 43.9, 40.1, 58.9, 61.5, 55.2, 16, 57.1,
                                                77.2, 32.6),
                                        volMultiplier = 1), 4)
  expect_equal(estimatedDIB,
               c(34.9165, 79.1221, 42.8118, 39.0334, 59.2008, 58.4637,
                 53.0551, 13.6587, 52.0982, 70.9857, 30.1339))

  ## for species B in various bec zones
  estimatedDIB <- round(DIB_ICalculator(taperEquationForm = "KBEC",
                                        FIZorBEC = c("AT", "BWBS", "CDF", "CWH", "ESSF", "ICH", "IDF",
                                                     "MH", "MS",
                                                     "SBPS", "SBS", "SWB"),
                                        species = "B",
                                        height_I = 1.4, ## this is corresponding to DIB_BH in sas compiler
                                        ## as the sas compiler using 1.4 in calculating
                                        ## DIB_BH
                                        heightTotal = c(23.9, 27.8, 30, 56.5, 39, 43.8, 30.6, 54.8,
                                                        39.9, 20.5, 37.5,
                                                        11.6),
                                        DBH = c(81.6, 56.8, 35.5, 147.3, 92, 95.6,
                                                66.1, 127.8, 93, 22, 103.3,
                                                44),
                                        volMultiplier = 1), 4)
  expect_equal(estimatedDIB,
               c(71.6069, 53.4621, 33.6486, 142.4649, 88.1287, 91.2637, 64.1968,
                 122.2297, 89.9853, 21.3482, 98.2528, 40.2907))


  ## for species C in various bec zones
  estimatedDIB <- round(DIB_ICalculator(taperEquationForm = "KBEC",
                                        FIZorBEC = c("AT", "CDF", "CWH", "ESSF", "ICH", "IDF",
                                                     "MH", "MS", "PP",
                                                     "SBPS", "SBS"),
                                        species = "C",
                                        height_I = 1.4, ## this is corresponding to DIB_BH in sas compiler
                                        ## as the sas compiler using 1.4 in calculating
                                        ## DIB_BH
                                        heightTotal = c(16, 38.5, 51, 31.4, 33, 39.3,
                                                        30.6, 31, 4.1, 13.1, 27.3),
                                        DBH = c(18.2, 79.2, 341.3, 140.6, 226, 108.6,
                                                129.4, 62.4, 6.7, 9.2,
                                                68.1),
                                        volMultiplier = 1), 4)
  expect_equal(estimatedDIB,
               c(17.6753, 75.2183, 304.0391, 131.7553, 203.3313, 102.8913, 118.1729,
                 59.8681, 5.9023, 8.9068, 63.9218))

  ## for species D in various bec zones
  estimatedDIB <- round(DIB_ICalculator(taperEquationForm = "KBEC",
                                        FIZorBEC = c("CDF", "CWH", "ESSF", "ICH",
                                                     "IDF", "MH", "MS", "SBS"),
                                        species = "D",
                                        height_I = 1.4, ## this is corresponding to DIB_BH in sas compiler
                                        ## as the sas compiler using 1.4 in calculating
                                        ## DIB_BH
                                        heightTotal = c(43.2, 34.3, 4.4, 17.9, 20.2, 4, 8, 10.5),
                                        DBH = c(57.3, 81, 8.3, 19.4, 34.4, 4.9, 10, 12.6),
                                        volMultiplier = 1), 4)
  expect_equal(estimatedDIB,
               c(55.9791, 80.8138, 7.8185, 18.6049, 33.6427, 4.4833, 9.4875,
                 12.0164))


  ## for species E in various bec zones
  estimatedDIB <- round(DIB_ICalculator(taperEquationForm = "KBEC",
                                        FIZorBEC = c("BWBS", "CWH", "ESSF", "ICH", "IDF",
                                                     "MS", "SBPS", "SBS", "SWB"),
                                        species = "E",
                                        height_I = 1.4, ## this is corresponding to DIB_BH in sas compiler
                                        ## as the sas compiler using 1.4 in calculating
                                        ## DIB_BH
                                        heightTotal = c(18.1, 26, 23, 24.3, 26.2, 16.9, 13.1, 23.9, 5.8),
                                        DBH = c(44.5, 56.5, 41.7, 73.8, 55, 26.6, 11.5, 66, 5.4),
                                        volMultiplier = 1), 4)
  expect_equal(estimatedDIB,
               c(43.8479, 56.4658, 40.9834, 75.9898, 52.1811, 25.4127, 10.4705,
                 66.4063, 4.6033))



  ## for species F in various bec zones
  estimatedDIB <- round(DIB_ICalculator(taperEquationForm = "KBEC",
                                        FIZorBEC = c("AT", "BWBS", "CDF", "CWH",
                                                     "ESSF", "ICH", "IDF", "MH", "MS",
                                                     "PP", "SBPS", "SBS", "SWB"),
                                        species = "F",
                                        height_I = 1.4, ## this is corresponding to DIB_BH in sas compiler
                                        ## as the sas compiler using 1.4 in calculating
                                        ## DIB_BH
                                        heightTotal = c(3.8, 19.3, 49.1, 3, 54.1,
                                                        38.9, 38, 30, 36.7, 22.5, 37.1, 49,
                                                        16.7),
                                        DBH = c(8.2, 33.2, 188.4, 240, 113, 115.6,
                                                139.7, 117.9, 138.1, 91.3,
                                                134.3, 130.2, 24.2),
                                        volMultiplier = 1), 4)
  expect_equal(estimatedDIB,
               c(6.8466, 28.7675, 146.8871, 143.5101, 98.5498, 98.5763, 113.5813,
                 91.7572, 109.008, 72.7844, 110.0211, 110.4554, 20.9267))


  ## for species H in various bec zones
  estimatedDIB <- round(DIB_ICalculator(taperEquationForm = "KBEC",
                                        FIZorBEC = c("AT", "CDF", "CWH", "ESSF", "ICH",
                                                     "IDF", "MH", "MS", "SBS"),
                                        species = "H",
                                        height_I = 1.4, ## this is corresponding to DIB_BH in sas compiler
                                        ## as the sas compiler using 1.4 in calculating
                                        ## DIB_BH
                                        heightTotal = c(34.6, 23.3, 52.3, 25.2,
                                                        53.8, 22.5, 36.2, 12.7, 27.1),
                                        DBH = c(99.1, 42.1, 189.9, 110, 113.3,
                                                40.5, 127.5, 27.1, 95.3),
                                        volMultiplier = 1), 4)
  expect_equal(estimatedDIB,
               c(90.089, 38.9181, 174.3518, 103.6548, 102.6671, 37.9874, 111.9426,
                 24.5784, 92.5496)) ##


  ## for species L in various bec zones
  estimatedDIB <- round(DIB_ICalculator(taperEquationForm = "KBEC",
                                        FIZorBEC = c("AT", "BWBS", "ESSF", "ICH", "IDF", "MS", "SBS"),
                                        species = "L",
                                        height_I = 1.4, ## this is corresponding to DIB_BH in sas compiler
                                        ## as the sas compiler using 1.4 in calculating
                                        ## DIB_BH
                                        heightTotal = c(14.2, 20, 36, 39.6, 32.8, 34.6, 16.3),
                                        DBH = c(27.8, 43, 81.7, 86.4, 64, 85.8, 37.8),
                                        volMultiplier = 1), 4)
  expect_equal(estimatedDIB,
               c(25.92, 41.2437, 64.8479, 73.9604, 53.1383, 70.0625, 32.6558
               ))


  ## for species MB in various bec zones
  estimatedDIB <- round(DIB_ICalculator(taperEquationForm = "KBEC",
                                        FIZorBEC = c("AT", "CDF", "CWH", "ICH", "IDF", "SBS"),
                                        species = "MB",
                                        height_I = 1.4, ## this is corresponding to DIB_BH in sas compiler
                                        ## as the sas compiler using 1.4 in calculating
                                        ## DIB_BH
                                        heightTotal = c(16.8, 31.8, 28.1, 9.2, 11.8, 7.3),
                                        DBH = c(23.1, 60, 100.7, 11.7, 17.9, 6.2),
                                        volMultiplier = 1), 4)
  expect_equal(estimatedDIB,
               c(22.172, 63.619, 104.5426, 10.1968, 16.2272, 5.2173))

  ## for species PA in various bec zones
  estimatedDIB <- round(DIB_ICalculator(taperEquationForm = "KBEC",
                                        FIZorBEC = c("AT", "ESSF", "ICH", "MH", "MS"),
                                        species = "PA",
                                        height_I = 1.4, ## this is corresponding to DIB_BH in sas compiler
                                        ## as the sas compiler using 1.4 in calculating
                                        ## DIB_BH
                                        heightTotal = c(18, 18.4, 12.7, 10.2, 9.4),
                                        DBH = c(51.3, 75.2, 28.4, 29.7, 17.5),
                                        volMultiplier = 1), 4)
  expect_equal(estimatedDIB,
               c(50.4824, 75.2814, 27.194, 28.4506, 16.3715))


  ## for species PL in various bec zones
  estimatedDIB <- round(DIB_ICalculator(taperEquationForm = "KBEC",
                                        FIZorBEC = c("AT", "BWBS", "CDF", "CWH",
                                                     "ESSF", "ICH", "IDF", "MH", "MS",
                                                     "SBPS", "SBS", "SWB"),
                                        species = "PL",
                                        height_I = 1.4, ## this is corresponding to DIB_BH in sas compiler
                                        ## as the sas compiler using 1.4 in calculating
                                        ## DIB_BH
                                        heightTotal = c(14.6, 30.7, 11.6, 31.2, 21.6,
                                                        39.8, 30.5, 13.4, 24.4, 23.5,
                                                        31.7, 10.2),
                                        DBH = c(28.2, 52.9, 46, 84.3, 66.2, 66.4,
                                                54.3, 34.4, 53.3, 53.1, 71.6,
                                                40.3),
                                        volMultiplier = 1), 4)
  expect_equal(estimatedDIB,
               c(26.5618, 51.4707, 43.6146, 80.1865, 63.2863, 63.2796, 52.4181,
                 32.3565, 49.6075, 50.8692, 69.961, 39.9555))


  ## for species PW in various bec zones
  estimatedDIB <- round(DIB_ICalculator(taperEquationForm = "KBEC",
                                        FIZorBEC = c("CDF", "CWH", "ESSF", "ICH", "IDF", "MH", "MS"),
                                        species = "PW",
                                        height_I = 1.4, ## this is corresponding to DIB_BH in sas compiler
                                        ## as the sas compiler using 1.4 in calculating
                                        ## DIB_BH
                                        heightTotal = c(26.8, 36.7, 42.7, 37.7, 39.3, 39.4, 30),
                                        DBH = c(36.9, 77.8, 112.9, 102, 58.1, 75.5, 80.6),
                                        volMultiplier = 1), 4)
  expect_equal(estimatedDIB,
               c(35.5368, 75.6815, 104.4669, 94.0637, 56.5361, 70.9877, 75.843))



  ## for species PY in various bec zones
  estimatedDIB <- round(DIB_ICalculator(taperEquationForm = "KBEC",
                                        FIZorBEC = c("ICH", "IDF", "PP"),
                                        species = "PY",
                                        height_I = 1.4, ## this is corresponding to DIB_BH in sas compiler
                                        ## as the sas compiler using 1.4 in calculating
                                        ## DIB_BH
                                        heightTotal = c(40.9, 30.1, 41.9),
                                        DBH = c(89.8, 96.6, 110.2),
                                        volMultiplier = 1), 4)
  expect_equal(estimatedDIB,
               c(82.8036, 90.0052, 105.1527))


  ## for species S in various bec zones
  estimatedDIB <- round(DIB_ICalculator(taperEquationForm = "KBEC",
                                        FIZorBEC = c("AT", "BWBS", "CDF", "CWH",
                                                     "ESSF", "ICH", "IDF", "MH", "MS",
                                                     "PP", "SBPS", "SBS", "SWB"),
                                        species = "S",
                                        height_I = 1.4, ## this is corresponding to DIB_BH in sas compiler
                                        ## as the sas compiler using 1.4 in calculating
                                        ## DIB_BH
                                        heightTotal = c(33.8, 33, 49.7, 55.2, 39.9, 44.1,
                                                        39.3, 34.5, 41.9, 17.6, 32,
                                                        34.3, 25.4),
                                        DBH = c(85.1, 74.8, 74.2, 284, 113.8, 133,
                                                79.7, 160.6, 105.8, 38.5,
                                                58.6, 116.1, 62.4),
                                        volMultiplier = 1), 4)
  expect_equal(estimatedDIB,
               c(82.4539, 73.2947, 72.7973, 270.5925, 110.6431, 127.4502, 80.1027,
                 154.3443, 103.0301, 37.5221, 56.3192, 113.0208, 59.5934))


  ## for species Y in various bec zones
  estimatedDIB <- round(DIB_ICalculator(taperEquationForm = "KBEC",
                                        FIZorBEC = c("AT", "CWH", "ESSF", "ICH", "MH"),
                                        species = "Y",
                                        height_I = 1.4, ## this is corresponding to DIB_BH in sas compiler
                                        ## as the sas compiler using 1.4 in calculating
                                        ## DIB_BH
                                        heightTotal = c(4.5, 52, 18.1, 4.5, 25.8),
                                        DBH = c(23.2, 190, 47.2, 6.6, 132.4),
                                        volMultiplier = 1), 4)
  expect_equal(estimatedDIB,
               c(21.2429, 172.5651, 44.5048, 6.4103, 119.6112))



})
