test_that("annualGrowthRateCalculator.R: annual growth rate is not correctly calculated.", {
  library(data.table)
  library(testthat)
  ## test warning
  expect_warning(annualGrowthRateCalculator(boredDiameter = 10,
                                            growthIncrement = 20,
                                            growthYear = 5,
                                            barkThickness = as.numeric(NA)))
  ## test error
  expect_error(annualGrowthRateCalculator(boredDiameter = 10,
                                          growthIncrement = 20,
                                          growthYear = 5,
                                          barkThickness = -5))
  
  expect_error(annualGrowthRateCalculator(boredDiameter = 10,
                                          growthIncrement = -5,
                                          growthYear = 5,
                                          barkThickness = 5))
  ## check missing barkThickness, in which case it is assigned with 0.05
  boredDiameter <- sample(1:100, size = 10)
  growthIncrement <- sample(1:20, size = 10)
  growthYear <- sample(c(5, 10), size = 10, replace = TRUE)
  barkThickness1 <- c(NA, seq(0.1, 0.6, by = 0.1), NA, NA, NA)
  barkThickness2 <- c(0.05, seq(0.1, 0.6, by = 0.1), 0.05, 0.05, 0.05)
  growthrate1 <- suppressWarnings(annualGrowthRateCalculator(boredDiameter = boredDiameter,
                                                             growthIncrement = growthIncrement,
                                                             growthYear = growthYear,
                                                             barkThickness = barkThickness1))
  growthrate2 <- annualGrowthRateCalculator(boredDiameter = boredDiameter,
                                            growthIncrement = growthIncrement,
                                            growthYear = growthYear,
                                            barkThickness = barkThickness2)
  expect_equal(growthrate1, growthrate2)
  
  ## check the outputs against sas outputs
  randomsasoutputs <- data.table(CLSTR_ID = c("DBO1-0086-Q 1", "DDCX-0050-OO1", 
                                              "0201-0016-QO1", "0141-0053-TO1", "3251-0015-O 1", "LGMA-0119-Q 1", 
                                              "0111-0028-QR1", "3433-0012-DO1", "3252-0549-Q 1", "0223-0268-MO1", 
                                              "0231-0207-QO1", "3011-0104-QO1", "4792-0021-MO1", "CFP2-0110-YO1", 
                                              "3061-0029-DO1", "024S-0244-ZO1", "DDC1-0082-OO1", "0241-0237-ZO1", 
                                              "0111-0070-QO1", "DLL1-0094-QO1"), 
                                 PLOT = c("I", "I", "N", "I", "S", "N", "N", "N", 
                                          "I", "I", "I", "I", "I", "I", "N", "I", "I", "I", "E", "I"), 
                                 TREE_NO = c("005", 
                                             "002", "008", "002", "003", "003", "002", "008", "005", "529", 
                                             "001", "001", "161", "748", "007", "099", "012", "005", "003", 
                                             "004"), 
                                 BARK_THK = c(3, 
                                              2, 1.565, 6, 7, 66, 25, 17, 10, 0.84, 3.135, 3, 0.55, 0.275, 
                                              12, 11, 2, 5, 24, 6), 
                                 BNG_DIAM = c(13.7, 
                                              8.3, 31.3, 19.7, 28.2, 64.7, 34.2, 57, 39.3, 16.8, 62.7, 
                                              6.3, 11, 5.5, 38.4, 33.5, 12, 22.8, 36.7, 17.2), 
                                 GROW_5YR = c(3, 3, 3, 7, 5, 2, 3, 3, 6, 10, 1, 
                                              16, 14, 15, 11, 8, 5, 2, 2, 2), 
                                 GROW_10Y = c(7, 7, 5, 14, 12, 4, 7, 6, 12, 18, 
                                              2, 26, 32, 25, 26, 23, 9, 4, 4, 4), 
                                 GROW_20Y = c(16, 20, 11, 26, 24, 9, 12, 12, 22, 
                                              46, 6, NA, NA, NA, 54, 42, 18, 7, 11, 8), 
                                 RATE_5 = c(0.01893, 0.0321, 0.007852, 0.031978, 
                                            0.015327, 0.003124, 0.008339, 0.004513, 0.013166, 0.052583, 
                                            0.001292, 0.390509, 0.126242, 0.377487, 0.025544, 0.02121, 
                                            0.036719, 0.007435, 0.00506, 0.010179), 
                                 RATE_10 = c(0.022862, 0.039783, 0.006582, 0.033367, 
                                             0.018941, 0.003136, 0.009875, 0.004539, 0.01339, 0.049994, 
                                             0.001294, 0.626975, 0.193868, 0.650166, 0.031693, 0.032302, 
                                             0.0343, 0.007506, 0.005093, 0.010311), 
                                 RATE_20 = c(0.028404, 0.07314, 0.007392, 0.033551, 
                                             0.019932, 0.003564, 0.008614, 0.004591, 0.012631, 0.083887, 
                                             0.001954, NA, NA, NA, 0.036311, 0.031741, 0.037855, 0.00666, 
                                             0.007171, 0.010592))
  
  # CLSTR_ID PLOT TREE_NO BARK_THK BNG_DIAM GROW_5YR GROW_10Y GROW_20Y
  # 1: DBO1-0086-Q 1    I     005    3.000     13.7        3        7       16
  # 2: DDCX-0050-OO1    I     002    2.000      8.3        3        7       20
  # 3: 0201-0016-QO1    N     008    1.565     31.3        3        5       11
  # 4: 0141-0053-TO1    I     002    6.000     19.7        7       14       26
  # 5: 3251-0015-O 1    S     003    7.000     28.2        5       12       24
  # 6: LGMA-0119-Q 1    N     003   66.000     64.7        2        4        9
  # 7: 0111-0028-QR1    N     002   25.000     34.2        3        7       12
  # 8: 3433-0012-DO1    N     008   17.000     57.0        3        6       12
  # 9: 3252-0549-Q 1    I     005   10.000     39.3        6       12       22
  # 10: 0223-0268-MO1    I     529    0.840     16.8       10       18       46
  # 11: 0231-0207-QO1    I     001    3.135     62.7        1        2        6
  # 12: 3011-0104-QO1    I     001    3.000      6.3       16       26       NA
  # 13: 4792-0021-MO1    I     161    0.550     11.0       14       32       NA
  # 14: CFP2-0110-YO1    I     748    0.275      5.5       15       25       NA
  # 15: 3061-0029-DO1    N     007   12.000     38.4       11       26       54
  # 16: 024S-0244-ZO1    I     099   11.000     33.5        8       23       42
  # 17: DDC1-0082-OO1    I     012    2.000     12.0        5        9       18
  # 18: 0241-0237-ZO1    I     005    5.000     22.8        2        4        7
  # 19: 0111-0070-QO1    E     003   24.000     36.7        2        4       11
  # 20: DLL1-0094-QO1    I     004    6.000     17.2        2        4        8
  # RATE_5  RATE_10  RATE_20
  # 1: 0.018930 0.022862 0.028404
  # 2: 0.032100 0.039783 0.073140
  # 3: 0.007852 0.006582 0.007392
  # 4: 0.031978 0.033367 0.033551
  # 5: 0.015327 0.018941 0.019932
  # 6: 0.003124 0.003136 0.003564
  # 7: 0.008339 0.009875 0.008614
  # 8: 0.004513 0.004539 0.004591
  # 9: 0.013166 0.013390 0.012631
  # 10: 0.052583 0.049994 0.083887
  # 11: 0.001292 0.001294 0.001954
  # 12: 0.390509 0.626975       NA
  # 13: 0.126242 0.193868       NA
  # 14: 0.377487 0.650166       NA
  # 15: 0.025544 0.031693 0.036311
  # 16: 0.021210 0.032302 0.031741
  # 17: 0.036719 0.034300 0.037855
  # 18: 0.007435 0.007506 0.006660
  # 19: 0.005060 0.005093 0.007171
  # 20: 0.010179 0.010311 0.010592
  
  randomsasoutputs[,':='(r_RATE_5 = round(annualGrowthRateCalculator(boredDiameter = BNG_DIAM,
                                                               growthIncrement = GROW_5YR,
                                                               growthYear = 5,
                                                               barkThickness = BARK_THK), 6),
                         r_RATE_10 = round(annualGrowthRateCalculator(boredDiameter = BNG_DIAM,
                                                               growthIncrement = GROW_10Y,
                                                               growthYear = 10,
                                                               barkThickness = BARK_THK), 6),
                         r_RATE_20 = round(annualGrowthRateCalculator(boredDiameter = BNG_DIAM,
                                                               growthIncrement = GROW_20Y,
                                                               growthYear = 20,
                                                               barkThickness = BARK_THK), 6))]
  expect_equal(randomsasoutputs$RATE_5, randomsasoutputs$r_RATE_5)
  expect_equal(randomsasoutputs$RATE_10, randomsasoutputs$r_RATE_10)
  expect_equal(randomsasoutputs$RATE_20, randomsasoutputs$r_RATE_20)
  
})