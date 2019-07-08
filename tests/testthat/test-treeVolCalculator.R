test_that("treeVolCalculator.R: diameter inside bark is not correctly calculated.", {
  library(data.table)
  library(testthat)
  
  ### check error message when BTOPEstimateType is not among NA, 1, 2, 3
  expect_error(treeVolCalculator(FIZorBEC = "PP",
                                 species = "B",
                                 height = sample(2.1:100, size = 1),
                                 DBH = sample(4.1:400, size = 1),
                                 taperEquationForm = "KBEC",
                                 volMultiplier = 1,
                                 stumpHeight = 0.3,
                                 breastHeight = 1.3,
                                 UTOPDIB = as.numeric(10),
                                 BTOPEstimateType = as.integer(sample(4: 100, size = 1)), ## this is test part
                                 BTOPHeight = 10.5,
                                 BTOPDIB = 9.5,
                                 logLengthMatrix = data.table(log_l_1 = 2.5, log_l_2 = 3.5),
                                 logMinLength = 0.1))
  
  
  ### check warning message when tree height is not available NA, 0 or > 90m or <=1.4
  expect_warning(treeVolCalculator(FIZorBEC = "PP",
                                   species = "B",
                                   height = c(0, NA, 1.4, 90.1),
                                   ## this is test part
                                   DBH = c(30.5, 50, 30.5, 50),
                                   taperEquationForm = "KBEC",
                                   volMultiplier = 1,
                                   stumpHeight = 0.3,
                                   breastHeight = 1.3,
                                   UTOPDIB = as.numeric(10),
                                   BTOPEstimateType = c(NA, 1L, NA, 2L),
                                   BTOPHeight = c(NA, 10.5, NA, 20),
                                   BTOPDIB = c(NA, 9.5, NA, 10.5),
                                   logLengthMatrix = data.table(log_l_1 = 2.5, log_l_2 = 3.5),
                                   logMinLength = 0.1))
  estimatedVol <- suppressWarnings(treeVolCalculator(FIZorBEC = "PP",
                                                     species = "B",
                                                     height = c(0, NA, 1.4, 90.1),
                                                     ## this is test part
                                                     DBH = c(30.5, 50, 30.5, 50),
                                                     taperEquationForm = "KBEC",
                                                     volMultiplier = 1,
                                                     stumpHeight = 0.3,
                                                     breastHeight = 1.3,
                                                     UTOPDIB = as.numeric(10),
                                                     BTOPEstimateType = c(NA, 1L, NA, 2L),
                                                     BTOPHeight = c(NA, 10.5, NA, 20),
                                                     BTOPDIB = c(NA, 9.5, NA, 10.5),
                                                     logLengthMatrix = data.table(log_l_1 = 2.5, log_l_2 = 3.5),
                                                     logMinLength = 0.1))
  expect_equal(estimatedVol$VOL_WSV, rep(as.numeric(NA), 4))
  expect_equal(estimatedVol$VOL_BELOW_UTOP, rep(as.numeric(NA), 4))
  expect_equal(estimatedVol$VOL_ABOVE_UTOP, rep(as.numeric(NA), 4))
  
  
  ### check warning message when DBH is not available NA, 0 or > 1000cm or <=1cm
  expect_warning(treeVolCalculator(FIZorBEC = "PP",
                                   species = "B",
                                   height = rep(50.5, 4),
                                   ## this is test part
                                   DBH = c(0, NA, 1, 1000.1),
                                   taperEquationForm = "KBEC",
                                   volMultiplier = 1,
                                   stumpHeight = 0.3,
                                   breastHeight = 1.3,
                                   UTOPDIB = as.numeric(10),
                                   BTOPEstimateType = c(NA, 1L, NA, 2L),
                                   BTOPHeight = c(NA, 10.5, NA, 20),
                                   BTOPDIB = c(NA, 9.5, NA, 10.5),
                                   logLengthMatrix = data.table(log_l_1 = 2.5, log_l_2 = 3.5),
                                   logMinLength = 0.1))
  estimatedVol <- suppressWarnings(treeVolCalculator(FIZorBEC = "PP",
                                                     species = "B",
                                                     height = rep(50.5, 4),
                                                     ## this is test part
                                                     DBH = c(0, NA, 1, 1000.1),
                                                     taperEquationForm = "KBEC",
                                                     volMultiplier = 1,
                                                     stumpHeight = 0.3,
                                                     breastHeight = 1.3,
                                                     UTOPDIB = as.numeric(10),
                                                     BTOPEstimateType = c(NA, 1L, NA, 2L),
                                                     BTOPHeight = c(NA, 10.5, NA, 20),
                                                     BTOPDIB = c(NA, 9.5, NA, 10.5),
                                                     logLengthMatrix = data.table(log_l_1 = 2.5, log_l_2 = 3.5),
                                                     logMinLength = 0.1))
  expect_equal(estimatedVol$VOL_WSV, rep(as.numeric(NA), 4))
  expect_equal(estimatedVol$VOL_BELOW_UTOP, rep(as.numeric(NA), 4))
  expect_equal(estimatedVol$VOL_ABOVE_UTOP, rep(as.numeric(NA), 4))
  
  
  ### check warning message when broken top height more than tree height
  expect_warning(treeVolCalculator(FIZorBEC = "PP",
                                   species = "B",
                                   height = 30,
                                   ## this is test part
                                   DBH = 33,
                                   taperEquationForm = "KBEC",
                                   volMultiplier = 1,
                                   stumpHeight = 0.3,
                                   breastHeight = 1.3,
                                   UTOPDIB = 10,
                                   BTOPEstimateType = 1L,
                                   BTOPHeight = 31,
                                   BTOPDIB = 3.5,
                                   logLengthMatrix = data.table(log_l_1 = 2.5, log_l_2 = 3.5),
                                   logMinLength = 0.1))
  estimatedVol <- suppressWarnings(treeVolCalculator(FIZorBEC = "PP",
                                                     species = "B",
                                                     height = 30,
                                                     ## this is test part
                                                     DBH = 33,
                                                     taperEquationForm = "KBEC",
                                                     volMultiplier = 1,
                                                     stumpHeight = 0.3,
                                                     breastHeight = 1.3,
                                                     UTOPDIB = 10,
                                                     BTOPEstimateType = 1L,
                                                     BTOPHeight = 31,
                                                     BTOPDIB = 3.5,
                                                     logLengthMatrix = data.table(log_l_1 = 2.5, log_l_2 = 3.5),
                                                     logMinLength = 0.1))
  expect_equal(estimatedVol$VOL_WSV, rep(as.numeric(NA), 1))
  expect_equal(estimatedVol$VOL_BELOW_UTOP, rep(as.numeric(NA), 1))
  expect_equal(estimatedVol$VOL_ABOVE_UTOP, rep(as.numeric(NA), 1))
  
  
 
  
  #### check the estimated DIB for 16 species, the expect part is from SAS compiler
  ## for species AC in various bec zones
  estimatedVol <- round(treeVolCalculator(FIZorBEC = c("BWBS", "CDF", "CWH", "ESSF", "ICH", 
                                                       "IDF", "MS", "PP", "SBPS", "SBS", "SWB"), ## 11 bec zones
                                          species = "AC",
                                          height = c(32.6, 12.7, 45, 24.2, 37.1, 44.3, 20.6,
                                                     33, 30.1, 39.5, 15.3),
                                          ## this is test part
                                          DBH = c(99.4, 16.2, 191.5, 44.1, 121.6, 114.2, 31.6,
                                                  55.8, 57.4, 145, 24.8),
                                          logMinLength = 0.1), 4)
  expect_equal(estimatedVol$VOL_WSV, 
               c(7.3988, 0.1208, 38.3967, 1.1754, 11.803, 14.3739,
                 0.592, 2.8142, 2.684, 18.1557, 0.2422))
  expect_equal(estimatedVol$VOL_BELOW_UTOP, 
               c(7.1563, 0.0982, 37.4792, 1.1195, 11.4491, 13.9814,
                 0.5543, 2.7193, 2.5854, 17.5295, 0.2148))
  expect_equal(estimatedVol$VOL_ABOVE_UTOP, 
               c(0.0088, 0.0166, 0.0067, 0.0122, 0.0089, 0.0115,
                 0.0137, 0.0133, 0.0121, 0.0089, 0.0144))
  
  
  ## for species AT in various bec zones
  estimatedVol <- round(treeVolCalculator(FIZorBEC = c("AT", "BWBS", "CWH", "ESSF", "ICH", "IDF", 
                                                       "MS", "PP", "SBPS", "SBS", "SWB"), ## 11 bec zones
                                          species = "AT",
                                          height = c(27.7, 32.1, 25.7, 25.6, 31.2, 21.2, 31.7,
                                                       5.8, 27.3, 33, 21.4),
                                          ## this is test part
                                          DBH = c(37, 81.5, 43.9, 40.1, 58.9, 61.5, 55.2,
                                                  16, 57.1, 77.2, 32.6),
                                          logMinLength = 0.1), 4)
  expect_equal(estimatedVol$VOL_WSV, 
               c(1.1958, 6.0291, 1.628, 1.3201, 3.6477, 2.3634,
                 2.8916, 0.0507, 2.5016, 5.3679, 0.6901))
  expect_equal(estimatedVol$VOL_BELOW_UTOP, 
               c(1.1453, 5.8141, 1.566, 1.2677, 3.5408, 2.2602,
                 2.7898, 0.0361, 2.4071, 5.1985, 0.6499)) 
  expect_equal(estimatedVol$VOL_ABOVE_UTOP, 
               c(0.0127, 0.0091, 0.0104, 0.0117, 0.0095, 0.008, 0.0115, 0.0085, 
                 0.0098, 0.0093, 0.0123))
  expect_equal(estimatedVol$HT_UTOP, 
               c(22.5, 28.2, 21.7, 20.8, 27.5, 18, 27, 2.9, 23.5, 29.4, 16.5))
  
  
  
  ## for species B in various bec zones
  estimatedVol <- round(treeVolCalculator(FIZorBEC = c("AT", "BWBS", "CDF", "CWH", "ESSF", "ICH", "IDF", "MH", "MS", 
                                                      "SBPS", "SBS", "SWB"), 
                                                species = "B",
                                                height = c(23.9, 27.8, 30, 56.5, 39, 43.8, 30.6, 54.8, 39.9, 20.5, 37.5, 
                                                           11.6),
                                                ## this is test part
                                                DBH = c(81.6, 56.8, 35.5, 147.3, 92, 95.6, 66.1, 127.8, 93, 22, 103.3, 
                                                        44),
                                          logMinLength = 0.1), 4)
  expect_equal(estimatedVol$VOL_WSV, 
               c(3.5493, 2.5022, 1.3215, 33.3932, 8.7476, 10.3629, 3.5901, 23.2474, 
                 8.6277, 0.3239, 10.1885, 0.6786))
  expect_equal(estimatedVol$VOL_BELOW_UTOP, 
               c(3.3635, 2.3932, 1.2726, 32.5793, 8.489, 10.0439, 3.4388, 22.6934, 
                 8.323, 0.2891, 9.8525, 0.6246))  
  expect_equal(estimatedVol$VOL_ABOVE_UTOP, 
               c(0.0087, 0.0113, 0.0142, 0.0072, 0.0095, 0.0101, 0.0116, 0.0094, 
                 0.0117, 0.0216, 0.0093, 0.0066))
  expect_equal(estimatedVol$HT_UTOP, 
               c(20.4, 24, 25.7, 54.6, 36, 40.6, 26.7, 52.1, 36.3, 13.3, 34.4, 
                 9.3))
  
  

  
  
  ## for species C in various bec zones
  estimatedVol <- round(treeVolCalculator(FIZorBEC = c("AT", "CDF", "CWH", "ESSF", "ICH", "IDF", "MH", "MS", "PP", 
                                                       "SBPS", "SBS"), 
                                          species = "C",
                                          height = c(16, 38.5, 51, 31.4, 33, 39.3, 30.6, 31, 4.1, 13.1, 27.3),
                                          ## this is test part
                                          DBH = c(18.2, 79.2, 341.3, 140.6, 226, 108.6, 129.4, 62.4, 6.7, 9.2, 
                                                  68.1),
                                          logMinLength = 0.1), 4)
  expect_equal(estimatedVol$VOL_WSV, 
               c(0.1881, 5.6775, 100.3954, 13.7104, 33.3385, 10.3893, 10.9779, 
                 3.2014, 0.0081, 0.0421, 3.1536))
  expect_equal(estimatedVol$VOL_BELOW_UTOP, 
               c(0.1575, 5.4027, 95.4425, 12.8389, 31.2899, 9.8899, 10.3458, 
                 3.0248, 0, 0.0017, 2.9726))  
  expect_equal(estimatedVol$VOL_ABOVE_UTOP, 
               c(0.0198, 0.0113, 0.0053, 0.0071, 0.0051, 0.0095, 0.007, 0.011, 
                 0.007, 0.0376, 0.0094))
  expect_equal(estimatedVol$HT_UTOP, 
               c(9.6, 34.4, 49.2, 28.8, 31.4, 35.5, 28, 26.7, NA, 0.5, 23.5))
  
  
  ## for species D in various bec zones
  estimatedVol <- round(treeVolCalculator(FIZorBEC = c("CDF", "CWH", "ESSF", "ICH", "IDF", "MH", "MS", "SBS"), 
                                          species = "D",
                                          height = c(43.2, 34.3, 4.4, 17.9, 20.2, 4, 8, 10.5),
                                          ## this is test part
                                          DBH = c(57.3, 81, 8.3, 19.4, 34.4, 4.9, 10, 12.6),
                                          logMinLength = 0.1), 4)
  expect_equal(estimatedVol$VOL_WSV, 
               c(4.4282, 6.2212, 0.0139, 0.2349, 0.76, 0.0046, 0.029, 0.0585))
  expect_equal(estimatedVol$VOL_BELOW_UTOP, 
               c(4.3051, 5.9806, 0, 0.2067, 0.7128, 0, 0.0052, 0.0322))  
  expect_equal(estimatedVol$VOL_ABOVE_UTOP, 
               c(0.0137, 0.0102, 0.0116, 0.0179, 0.0113, 0.0037, 0.021, 0.022))
  expect_equal(estimatedVol$HT_UTOP, 
               c(38, 29.9, NA, 11.5, 15.5, NA, 0.9, 3.3))
  
  
  ## for species E in various bec zones
  estimatedVol <- round(treeVolCalculator(FIZorBEC = c("BWBS", "CWH", "ESSF", "ICH", "IDF", "MS", "SBPS", "SBS", "SWB"), 
                                          species = "E",
                                          height = c(18.1, 26, 23, 24.3, 26.2, 16.9, 13.1, 23.9, 5.8),
                                          ## this is test part
                                          DBH = c(44.5, 56.5, 41.7, 73.8, 55, 26.6, 11.5, 66, 5.4),
                                          logMinLength = 0.1), 4)
  expect_equal(estimatedVol$VOL_WSV, 
               c(1.0039, 2.2803, 1.1269, 3.676, 2.1963, 0.347, 0.0524, 2.746, 0.0053))
  expect_equal(estimatedVol$VOL_BELOW_UTOP, 
               c(0.9254, 2.1461, 1.0542, 3.4414, 2.0892, 0.3112, 0.0161, 2.565, 0))  
  expect_equal(estimatedVol$VOL_ABOVE_UTOP, 
               c(0.0103, 0.0117, 0.013, 0.0094, 0.0106, 0.0145, 0.0331, 0.0106, 0.0044))
  expect_equal(estimatedVol$HT_UTOP, 
               c(13.4, 20.8, 17.4, 19.9, 21.6, 10.8, 2.1, 18.9, NA))
  
  
  
  ## for species F in various bec zones
  estimatedVol <- round(treeVolCalculator(FIZorBEC = c("AT", "BWBS", "CDF", "CWH", "ESSF", "ICH", "IDF", "MH", "MS", 
                                                       "PP", "SBPS", "SBS", "SWB"), 
                                          species = "F",
                                          height = c(3.8, 19.3, 49.1, 3, 54.1, 38.9, 38, 30, 36.7,
                                                     22.5, 37.1, 49, 16.7),
                                          ## this is test part
                                          DBH = c(8.2, 33.2, 188.4, 240, 113, 115.6, 139.7, 117.9, 138.1, 91.3, 
                                                  134.3, 130.2, 24.2),
                                          logMinLength = 0.1), 4)
  expect_equal(estimatedVol$VOL_WSV, 
               c(0.0109, 0.5638, 33.06, 3.7958, 14.7176, 11.0353, 15.4216, 8.6544, 
                 12.6437, 4.0623, 13.8621, 17.9763, 0.2723))
  expect_equal(estimatedVol$VOL_BELOW_UTOP, 
               c(0, 0.5239, 32.3135, 3.2533, 14.319, 10.6729, 14.9867, 8.3801, 
                 12.1613, 3.8955, 13.4401, 17.5383, 0.2428))  
  expect_equal(estimatedVol$VOL_ABOVE_UTOP, 
               c(0.0089, 0.0125, 0.0053, 0.0013, 0.0104, 0.0086, 0.0072, 0.0053, 
                 0.0079, 0.0062, 0.007, 0.0083, 0.0153))
  expect_equal(estimatedVol$HT_UTOP, 
               c(NA, 14.9, 47.5, 2.9, 50.6, 35.8, 35.5, 28.3, 34, 20.2, 34.8, 
                 46.2, 11.5))
  
  
  ## for species H in various bec zones
  estimatedVol <- round(treeVolCalculator(FIZorBEC = c("AT", "CDF", "CWH", "ESSF", "ICH", "IDF", "MH", "MS", "SBS"), 
                                          species = "H",
                                          height = c(34.6, 23.3, 52.3, 25.2, 53.8, 22.5, 36.2, 12.7, 27.1),
                                          ## this is test part
                                          DBH = c(99.1, 42.1, 189.9, 110, 113.3, 40.5, 127.5, 27.1, 95.3),
                                          logMinLength = 0.1), 4)
  expect_equal(estimatedVol$VOL_WSV, 
               c(9.096, 1.2649, 45.2408, 9.075, 17.6007, 1.041, 14.1334, 0.2936, 
                 6.4903))
  expect_equal(estimatedVol$VOL_BELOW_UTOP, 
               c(8.8191, 1.2096, 44.0818, 8.7584, 17.1871, 0.9901, 13.6581, 
                 0.2689, 6.2546))  
  expect_equal(estimatedVol$VOL_ABOVE_UTOP, 
               c(0.0084, 0.0109, 0.0069, 0.0053, 0.0096, 0.0124, 0.008, 0.0102, 
                 0.0073))
  expect_equal(estimatedVol$HT_UTOP, 
               c(31.9, 19.3, 50.1, 23.7, 50.8, 17.7, 33.6, 9.1, 24.9))
  
  
  ## for species L in various bec zones
  estimatedVol <- round(treeVolCalculator(FIZorBEC = c("AT", "BWBS", "ESSF", "ICH", "IDF", "MS", "SBS"), 
                                          species = "L",
                                          height = c(14.2, 20, 36, 39.6, 32.8, 34.6, 16.3),
                                          ## this is test part
                                          DBH = c(27.8, 43, 81.7, 86.4, 64, 85.8, 37.8),
                                          logMinLength = 0.1), 4)
  expect_equal(estimatedVol$VOL_WSV, 
               c(0.3206, 1.0076, 5.005, 6.7152, 3.0181, 5.4286, 0.6481))
  expect_equal(estimatedVol$VOL_BELOW_UTOP, 
               c(0.285, 0.9322, 4.8377, 6.5223, 2.9119, 5.2456, 0.6055))  
  expect_equal(estimatedVol$VOL_ABOVE_UTOP, 
               c(0.0123, 0.0116, 0.0094, 0.0104, 0.0112, 0.0089, 0.0089))
  expect_equal(estimatedVol$HT_UTOP, 
               c(9.6, 15.5, 33, 36.1, 28.7, 31.6, 13.2))
  
  
  ## for species MB in various bec zones
  estimatedVol <- round(treeVolCalculator(FIZorBEC = c("AT", "CDF", "CWH", "ICH", "IDF", "SBS"), 
                                          species = "MB",
                                          height = c(16.8, 31.8, 28.1, 9.2, 11.8, 7.3),
                                          ## this is test part
                                          DBH = c(23.1, 60, 100.7, 11.7, 17.9, 6.2),
                                          logMinLength = 0.1), 4)
  expect_equal(estimatedVol$VOL_WSV, 
               c(0.2591, 3.2733, 7.5219, 0.0371, 0.1075, 0.0088))
  expect_equal(estimatedVol$VOL_BELOW_UTOP, 
               c(0.2257, 3.139, 7.1713, 0.0117, 0.0804, 0))  
  expect_equal(estimatedVol$VOL_ABOVE_UTOP, 
               c(0.0179, 0.013, 0.0085, 0.0218, 0.0181, 0.0078))
  expect_equal(estimatedVol$HT_UTOP, 
               c(10.2, 26.4, 24.5, 1.5, 5.4, NA))
  
  ## for species PA in various bec zones
  estimatedVol <- round(treeVolCalculator(FIZorBEC = c("AT", "ESSF", "ICH", "MH", "MS"), 
                                          species = "PA",
                                          height = c(18, 18.4, 12.7, 10.2, 9.4),
                                          ## this is test part
                                          DBH = c(51.3, 75.2, 28.4, 29.7, 17.5),
                                          logMinLength = 0.1), 4)
  expect_equal(estimatedVol$VOL_WSV, 
               c(1.6118, 3.5713, 0.3674, 0.3372, 0.1081))
  expect_equal(estimatedVol$VOL_BELOW_UTOP, 
               c(1.5068, 3.3448, 0.3318, 0.2996, 0.0863))  
  expect_equal(estimatedVol$VOL_ABOVE_UTOP, 
               c(0.0067, 0.0053, 0.0076, 0.006, 0.0115))
  expect_equal(estimatedVol$HT_UTOP, 
               c(15.5, 16.5, 9.8, 7.9, 5.6))
  
  
  ## for species PL in various bec zones
  estimatedVol <- round(treeVolCalculator(FIZorBEC = c("AT", "BWBS", "CDF", "CWH", "ESSF", "ICH", "IDF", "MH", "MS", 
                                                       "SBPS", "SBS", "SWB"), 
                                          species = "PL",
                                          height = c(14.6, 30.7, 11.6, 31.2, 21.6, 39.8, 30.5, 13.4, 24.4, 23.5, 
                                                     31.7, 10.2),
                                          ## this is test part
                                          DBH = c(28.2, 52.9, 46, 84.3, 66.2, 66.4, 54.3, 34.4, 53.3, 53.1, 71.6, 
                                                  40.3),
                                          logMinLength = 0.1), 4)
  expect_equal(estimatedVol$VOL_WSV, 
               c(0.3755, 2.8319, 0.7989, 5.8842, 2.6953, 5.1468, 2.9913, 0.501, 
                 2.0502, 2.1259, 4.8838, 0.7395))
  expect_equal(estimatedVol$VOL_BELOW_UTOP, 
               c(0.3421, 2.7326, 0.7343, 5.6533, 2.5485, 4.9935, 2.8923, 0.4583, 
                 1.959, 2.0357, 4.7114, 0.6918))  
  expect_equal(estimatedVol$VOL_ABOVE_UTOP, 
               c(0.0108, 0.0097, 0.0054, 0.0087, 0.0077, 0.0105, 0.0094, 0.0085, 
                 0.0088, 0.0078, 0.0085, 0.0034))
  expect_equal(estimatedVol$HT_UTOP, 
               c(10.9, 27.6, 9.6, 28.4, 19, 36.5, 27.5, 10.4, 21.3, 21.6, 29.1, 
                 9.1))
  
  
  ## for species PW in various bec zones
  estimatedVol <- round(treeVolCalculator(FIZorBEC = c("CDF", "CWH", "ESSF", "ICH", "IDF", "MH", "MS"), 
                                          species = "PW",
                                          height = c(26.8, 36.7, 42.7, 37.7, 39.3, 39.4, 30),
                                          ## this is test part
                                          DBH = c(36.9, 77.8, 112.9, 102, 58.1, 75.5, 80.6),
                                          logMinLength = 0.1), 4)
  expect_equal(estimatedVol$VOL_WSV, 
               c(1.2396, 6.7636, 13.8732, 10.2554, 4.1466, 6.1836, 5.4344))
  expect_equal(estimatedVol$VOL_BELOW_UTOP, 
               c(1.1857, 6.5499, 13.435, 9.9153, 4.0115, 5.9817, 5.2191))  
  expect_equal(estimatedVol$VOL_ABOVE_UTOP, 
               c(0.0125, 0.0085, 0.0083, 0.0081, 0.0123, 0.0109, 0.0087))
  expect_equal(estimatedVol$HT_UTOP, 
               c(22.7, 33.9, 40.3, 35.3, 35.5, 36.1, 27.2))
  
  
  
  ## for species PY in various bec zones
  estimatedVol <- round(treeVolCalculator(FIZorBEC = c("ICH", "IDF", "PP"), 
                                          species = "PY",
                                          height = c(40.9, 30.1, 41.9),
                                          ## this is test part
                                          DBH = c(89.8, 96.6, 110.2),
                                          logMinLength = 0.1), 4)
  expect_equal(estimatedVol$VOL_WSV, 
               c(9.2779, 7.7999, 15.7351))
  expect_equal(estimatedVol$VOL_BELOW_UTOP, 
               c(9.0459, 7.555, 15.395))  
  expect_equal(estimatedVol$VOL_ABOVE_UTOP, 
               c(0.0088, 0.0067, 0.0076))
  expect_equal(estimatedVol$HT_UTOP, 
               c(37.9, 27.8, 39.3))
  
  
  ## for species S in various bec zones
  estimatedVol <- round(treeVolCalculator(FIZorBEC = c("AT", "BWBS", "CDF", "CWH", "ESSF", "ICH", "IDF", "MH", "MS", 
                                                       "PP", "SBPS", "SBS", "SWB"), 
                                          species = "S",
                                          height = c(33.8, 33, 49.7, 55.2, 39.9, 44.1, 39.3, 34.5, 41.9, 17.6, 32, 
                                                     34.3, 25.4),
                                          ## this is test part
                                          DBH = c(85.1, 74.8, 74.2, 284, 113.8, 133, 79.7, 160.6, 105.8, 38.5, 
                                                  58.6, 116.1, 62.4),
                                          logMinLength = 0.1), 4)
  expect_equal(estimatedVol$VOL_WSV, 
               c(6.4434, 4.9682, 7.5939, 95.1472, 12.9506, 18.4689, 6.9388, 
                 22.0064, 11.9596, 0.8336, 3.0752, 11.9827, 2.59))
  expect_equal(estimatedVol$VOL_BELOW_UTOP, 
               c(6.1886, 4.7723, 7.3725, 92.0353, 12.4831, 17.7795, 6.6764, 
                 21.1248, 11.5309, 0.7782, 2.9548, 11.5015, 2.4666))  
  expect_equal(estimatedVol$VOL_ABOVE_UTOP, 
               c(0.009, 0.0105, 0.0128, 0.0056, 0.0084, 0.0087, 0.0111, 0.0055, 
                 0.0094, 0.0101, 0.0119, 0.0077, 0.01))
  expect_equal(estimatedVol$HT_UTOP, 
               c(30.7, 29.6, 45.6, 53.5, 37.1, 41.3, 35.5, 32.7, 38.8, 14, 28.4, 
                 31.9, 22.1))
  
  
  ## for species Y in various bec zones
  estimatedVol <- round(treeVolCalculator(FIZorBEC = c("AT", "CWH", "ESSF", "ICH", "MH"), 
                                          species = "Y",
                                          height = c(4.5, 52, 18.1, 4.5, 25.8),
                                          ## this is test part
                                          DBH = c(23.2, 190, 47.2, 6.6, 132.4),
                                          logMinLength = 0.1), 4)
  expect_equal(estimatedVol$VOL_WSV, 
               c(0.1086, 40.2361, 1.2411, 0.0103, 11.518))
  expect_equal(estimatedVol$VOL_BELOW_UTOP, 
               c(0.0885, 38.7122, 1.159, 0, 10.9347))  
  expect_equal(estimatedVol$VOL_ABOVE_UTOP, 
               c(0.0037, 0.0076, 0.008, 0.0083, 0.0048))
  expect_equal(estimatedVol$HT_UTOP, 
               c(3.2, 49.4, 15.1, NA, 24))
  
  
  ### test for calculating log volume (randomly selected in sas outputs)
  testdata <- data.table(CLSTR_ID = c("0131-0031-NO1", "DSC2-0035-Z 1", 
                                      "DME1-0055-NO1", "3052-0016-QO1", "KOL1-0020-BO1"),
                         PLOT = c("S", "W", "W", "E", "S"),
                         TREE_NO = c("005", "008", "002", "003", "002"),
                         SP0 = c("AT", "S", "PL", "F", "PW"),
                         BEC = c("ESSF", "CDF", "IDF", "SBS", "IDF"),
                         DBH = c(40.1, 74.2, 54.3, 130.2, 58.1),
                         HEIGHT = c(25.6, 49.7, 30.5, 49, 39.3),
                         LO_LG_0 = c(0.3, 0.3, 0.3, 0.3, 0.3),
                         LO_LG_1 = c(10.7, 9.7, 9.7, 7.7, 39), 
                         LO_LG_2 = c(14.6, 14, 11, 15, NA), 
                         LO_LG_3 = c(NA, 25.7, 9.5, 16, NA), 
                         LO_LG_4 = c(NA, NA, NA, 10, NA),
                         VOL_WSV = c(1.3201, 7.5939, 2.9913, 17.9763, 4.1466),
                         LOG_V_0 = c(0.0407, 0.2086, 0.0896, 0.4298, 0.1228), 
                         LOG_V_1 = c(0.9610, 3.1253, 1.6847, 6.0981, 4.0238),
                         LOG_V_2 = c(0.3184, 2.7071, 1.026, 7.3153, NA),
                         LOG_V_3 = c(NA, 1.5529, 0.1910, 3.8236, NA),
                         LOG_V_4 = c(NA, NA, NA, 0.3095, NA),
                         LOG_VM_0 = c(0, 0, 0, 0, 0), 
                         LOG_VM_1 = c(0.9610, 3.1253, 1.6847, 6.0981, 4.0115),  
                         LOG_VM_2 = c(0.3067, 2.7071, 1.0260, 7.3153, NA), 
                         LOG_VM_3 = c(NA, 1.5401, 0.1816, 3.8236, NA),  
                         LOG_VM_4 = c(NA, NA, NA, 0.3013, NA),
                         LOG_UTOP = c(2, 3, 3, 4, 1),
                         LOG_D_0 = c(41.5694, 94.0810, 61.6620, 135.0562, 72.1917),
                         LOG_D_1 = c(26.9634, 55.5009, 40.7390, 88.1556, 0),
                         LOG_D_2 = c(0, 43.1822, 25.8273, 69.3335, NA),  
                         LOG_D_3 = c(NA, 0, 0, 34.0682, NA), 
                         LOG_D_4 = c(NA, NA, NA, 0, NA))
  
  estimatedVol <- round(treeVolCalculator(FIZorBEC = testdata$BEC, 
                                          species = testdata$SP0,
                                          height = testdata$HEIGHT,
                                          ## this is test part
                                          DBH = testdata$DBH,
                                          logLengthMatrix = testdata[,paste("LO_LG_", 0:4, sep = ""), with = FALSE],
                                          logMinLength = 0.1), 4)
  
  expect_equal(estimatedVol$VOL_WSV, testdata$VOL_WSV) ## whole volume
  expect_equal(estimatedVol$LOG_V_0, testdata$LOG_V_0) ## log volume
  expect_equal(estimatedVol$LOG_V_1, testdata$LOG_V_1) ## the first log
  expect_equal(estimatedVol$LOG_V_2, testdata$LOG_V_2) ## the second log
  expect_equal(estimatedVol$LOG_V_3, testdata$LOG_V_3) ## the third log
  expect_equal(estimatedVol$LOG_V_4, testdata$LOG_V_4) ## the forth log

  expect_equal(estimatedVol$LOG_VM_0, testdata$LOG_VM_0) ## for the merchantable volume by each log
  expect_equal(estimatedVol$LOG_VM_1, testdata$LOG_VM_1)
  expect_equal(estimatedVol$LOG_VM_2, testdata$LOG_VM_2)
  expect_equal(estimatedVol$LOG_VM_3, testdata$LOG_VM_3)
  expect_equal(estimatedVol$LOG_VM_4, testdata$LOG_VM_4)

  expect_equal(estimatedVol$LOG_D_0, testdata$LOG_D_0) ## for the diameter at the top of each log
  expect_equal(estimatedVol$LOG_D_1, testdata$LOG_D_1)
  expect_equal(estimatedVol$LOG_D_2, testdata$LOG_D_2)
  expect_equal(estimatedVol$LOG_D_3, testdata$LOG_D_3)
  expect_equal(estimatedVol$LOG_D_4, testdata$LOG_D_4)

  expect_equal(estimatedVol$LOG_UTOP, testdata$LOG_UTOP)  ## for the log number in which the utop (10cm dib) is
  
  
  #### test for broken top trees (randomly selected 6 observations: 3 type 1 and 3 type 3)
  testdata <- data.table(CLSTR_ID = c("0101-0032-QO1", "0402-0011-YO1", "0141-0076-TO1", 
                                      "DSC2-0110-Z 1", "CMI1-0453-FO1", "CMI5-0274-FO1"), 
                         PLOT = c("I", "I", "I", "I", "I", "I"), 
                         TREE_NO = c("001", "088", "002", "005", "047", "003"), 
                         SP0 = c("H", "AT", "S", "F", "B", "AT"), 
                         BEC = c("CWH", "BWBS", "SBS", "CWH", "ESSF", "BWBS"),
                         DBH = c(85.8, 29.1, 11.3, 17.7, 8.9, 15.1),
                         HT = c(30.3, 20.5, 13., 15, 6, 15), 
                         DIB_BTOP_EST = c(44.8598, 16.8224, 6.5421, 
                                          2.1294, 4.6735, 12.8601),
                         HT_BTOP_EST = c(16.5, 11.2, 7.5, 14.3, 3.9, 2.2), 
                         BTOP_ESTIMATE_TYPE = c(1L, 1L, 1L, 3L, 3L, 3L),
                         VOL_BELOW_UTOP = c(5.9294, 0.4976, 0.0170, 
                                            0.1182, 0, 0.0771), 
                         VOL_ABOVE_UTOP = c(0.0085, 0.0129, 0.0373, 
                                            0.0206, 0.0162, 0.0257), 
                         VOL_BELOW_BTOP = c(5.1229, 0.4317, 0.0464,
                                            0.1387, 0.0148, 0.0264),
                         VOL_ABOVE_BTOP = c(0.8150, 0.0788, 0.0079, 
                                            0.0001, 0.0013, 0.0765), 
                         HT_UTOP = c(27.4, 15.4, 2.2, 8.9, NA, 7))
                         
  estimatedVol <- round(treeVolCalculator(FIZorBEC = testdata$BEC, 
                                          species = testdata$SP0,
                                          height = testdata$HT,
                                          ## this is test part
                                          DBH = testdata$DBH,
                                          BTOPEstimateType = testdata$BTOP_ESTIMATE_TYPE,
                                          BTOPHeight = testdata$HT_BTOP_EST,
                                          BTOPDIB = testdata$DIB_BTOP_EST,
                                          logMinLength = 0.1), 4)
   expect_equal(estimatedVol$VOL_BELOW_UTOP, 
                testdata$VOL_BELOW_UTOP)  
   expect_equal(estimatedVol$VOL_ABOVE_UTOP, 
                testdata$VOL_ABOVE_UTOP)
   expect_equal(estimatedVol$VOL_BELOW_BTOP, 
                testdata$VOL_BELOW_BTOP)  
   expect_equal(estimatedVol$VOL_ABOVE_BTOP, 
                testdata$VOL_ABOVE_BTOP)
   expect_equal(estimatedVol$HT_UTOP, 
                testdata$HT_UTOP)
   rm(testdata, estimatedVol)
   
   #### test for broken top trees with log (randomly selected 6 observations: 3 type 1 and 3 type 3)
   testdata <- data.table(CLSTR_ID = c("0131-0026-QO1", "DMHM-0238-FO1", "0231-0301-QO1", 
                                       "CMI4-0498-FO1", "4721-0004-Q 1", "DPG1-0063-DO1"),
                          PLOT = c("I", "I", "I", "I", "I", "I"),    
                          TREE_NO = c("007", "005", "011", "130", "007", "001"),
                          SP0 = c("F", "PL", "S", "S", "B", "B"), 
                          BEC = c("ICH", "SBPS", "SBS", "SWB", "MS", "ESSF"),
                          DBH = c(15, 5.5, 7.8, 9.4, 5.9, 34),
                          HT = c(13.1, 5.6, 12.9, 10, 9, 22),
                          DIB_BTOP_EST = c(3.7383, 3.7383, 3.7383, 1.9980, 0.2727, 7.8203), 
                          HT_BTOP_EST = c(10.9, 3.9, 8.9, 9.3, 8.7, 18.8),
                          BTOP_ESTIMATE_TYPE = c(1L, 1L, 1L, 3L, 3L, 3L),
                          LO_LG_0 = c(0.3, 0.3, 0.3, 0.3, 0.3, 0.3), 
                          LO_LG_1 = c(10.6, 3.6, 8.6, 9, 8.4, 3.7), 
                          LO_LG_2 = c(2.2, 1.7, 4, 0.7, 0.3, 14.8), 
                          LO_LG_3 = c(NA, NA, NA, NA, NA, 3.2), 
                          LO_LG_4 = rep(as.numeric(NA), 6), 
                          LO_LG_5 = rep(as.numeric(NA), 6), 
                          LO_LG_6 = rep(as.numeric(NA), 6), 
                          LO_LG_7 = rep(as.numeric(NA), 6), 
                          LOG_V_0 = c(0.0051, 0.0005, 0.0015,
                                      0.0020, 0.0008, 0.0314),  
                          LOG_V_1 = c(0.0829, 0.0065, 0.0245,
                                      0.0304, 0.0102, 0.2953),
                          LOG_V_2 = c(0.0007, 0.0009, 0.0017, 0.0001, 0, 0.4629),
                          LOG_V_3 = c(NA, NA, NA, NA, NA, 0.0051),
                          LOG_V_4 = rep(as.numeric(NA), 6),
                          LOG_V_5 = rep(as.numeric(NA), 6),
                          LOG_V_6 = rep(as.numeric(NA), 6),
                          LOG_V_7 = rep(as.numeric(NA), 6),
                          LOG_VM_0 = c(0, 0, 0, 0, 0, 0), 
                          LOG_VM_1 = c(0.0579, NA, NA, NA, NA, 0.2953), 
                          LOG_VM_2 = c(NA, NA, NA, NA, NA, 0.4543), 
                          LOG_VM_3 = rep(as.numeric(NA), 6),
                          LOG_VM_4 = rep(as.numeric(NA), 6),
                          LOG_VM_5 = rep(as.numeric(NA), 6),
                          LOG_VM_6 = rep(as.numeric(NA), 6),
                          LOG_VM_7 = rep(as.numeric(NA), 6),
                          LOG_BTOP = c(1, 1, 1, 1, 1, 2))
  
   estimatedVol <- round(treeVolCalculator(FIZorBEC = testdata$BEC, 
                                           species = testdata$SP0,
                                           height = testdata$HT,
                                           DBH = testdata$DBH,
                                           BTOPEstimateType = testdata$BTOP_ESTIMATE_TYPE,
                                           BTOPHeight = testdata$HT_BTOP_EST,
                                           BTOPDIB = testdata$DIB_BTOP_EST,
                                           logLengthMatrix = testdata[, paste("LO_LG_", 0:7, sep = ""), with = FALSE],
                                           logMinLength = 0.1), 4)
   expect_equal(estimatedVol$LOG_V_0, testdata$LOG_V_0) ## log volume
   expect_equal(estimatedVol$LOG_V_1, testdata$LOG_V_1) ## the first log
   expect_equal(estimatedVol$LOG_V_2, testdata$LOG_V_2) ## the second log
   expect_equal(estimatedVol$LOG_V_3, testdata$LOG_V_3) ## the third log
   expect_equal(estimatedVol$LOG_V_4, testdata$LOG_V_4) ## the forth log
   expect_equal(estimatedVol$LOG_VM_0, testdata$LOG_VM_0) ## for the merchantable volume by each log
   expect_equal(estimatedVol$LOG_VM_1, testdata$LOG_VM_1)
   expect_equal(estimatedVol$LOG_VM_2, testdata$LOG_VM_2)
   expect_equal(estimatedVol$LOG_VM_3, testdata$LOG_VM_3)
   expect_equal(estimatedVol$LOG_VM_4, testdata$LOG_VM_4)
   expect_equal(estimatedVol$LOG_BTOP, testdata$LOG_BTOP)  ## for the log number in which the btop (10cm dib) is
   
   
  
})
