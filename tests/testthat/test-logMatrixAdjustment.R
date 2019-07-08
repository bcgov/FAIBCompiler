test_that("logMatrixAdjustment.R: log length is not adjusted correctly", {
  library(data.table)
  library(testthat)
  logmatrix <- matrix(data = c(rep(NA, 10), 
                               c(0.3, rep(NA, 8), 0.6),
                               c(3, rep(NA, 8), 0.6),
                               c(0.2, rep(NA, 8), 0.6),
                               c(rep(3, 3), rep(NA, 6), 9.3),
                               c(rep(3, 3), rep(NA, 6), 9),
                               c(rep(3, 3), rep(NA, 6), 10),
                               c(rep(1, 9), 9.3),
                               c(rep(1, 9), 9),
                               c(rep(1, 9), 10),
                               c(4, 0.2, 6, 8, rep(NA, 5), 18.5)),
                      ncol = 10, byrow = TRUE)
  logmatrix <- data.table(data.frame(logmatrix))
  names(logmatrix) <- c(paste("LG_", 1:9, sep = ""), "HEIGHT")
  #     LG_1 LG_2 LG_3 LG_4 LG_5 LG_6 LG_7 LG_8 LG_9 HEIGHT
  # 1:   NA   NA   NA   NA   NA   NA   NA   NA   NA     NA ## for warning test
  # 2:  0.3   NA   NA   NA   NA   NA   NA   NA   NA    0.6 ## for one log test
  # 3:  3.0   NA   NA   NA   NA   NA   NA   NA   NA    0.6 ## for reducing length for log one test
  # 4:  0.2   NA   NA   NA   NA   NA   NA   NA   NA    0.6 ## for adding one log test
  # 5:  3.0  3.0    3   NA   NA   NA   NA   NA   NA    9.3 ## for three logs test
  # 6:  3.0  3.0    3   NA   NA   NA   NA   NA   NA    9.0 ## for three logs test and reducing length for last log
  # 7:  3.0  3.0    3   NA   NA   NA   NA   NA   NA   10.0 ## for three logs test and reducing length for last log
  # 8:  1.0  1.0    1    1    1    1    1    1    1    9.3 ## for maxlog test
  # 9:  1.0  1.0    1    1    1    1    1    1    1    9.0 ## for maxlog and reducing length for last log 
  # 10:  1.0  1.0    1    1    1    1    1    1    1   10.0 ## for maxlog and adding length for last log
  # 11:  4.0  0.2    6    8   NA   NA   NA   NA   NA   18.5 ## will be used for adjust length when smaller than min loglength
  expect_warning(logMatrixAdjustment(logLengthMatrix = logmatrix[,1:9],
                                     height = logmatrix$HEIGHT,
                                     stumpHeight = 0.3,
                                     logMinLength = 0.1,
                                     logDefaultLength = 5)) ## warning test passed
  adjustedlog1 <- suppressWarnings(logMatrixAdjustment(logLengthMatrix = logmatrix[,1:9],
                                                       height = logmatrix$HEIGHT,
                                                       stumpHeight = 0.3,
                                                       logMinLength = 0.1,
                                                       logDefaultLength = 5)) 
  adjustedlog1_ref <- data.table(VOL_TREE_MAX_LOGS = c(NA, 1, 1, 2, 3, 3, 4, 9, 9, 9, 4), 
                                 LOG_L_0 = c(NA, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3), 
                                 LOG_L_1 = c(NA, 0.3, 0.3, 0.2, 3, 3, 3, 1, 1, 1, 4), 
                                 LOG_L_2 = c(NA, NA, NA, 0.1, 3, 3, 3, 1, 1, 1, 0.2), 
                                 LOG_L_3 = c(NA, NA, NA, NA, 3, 2.7, 3, 1, 1, 1, 6), 
                                 LOG_L_4 = c(NA, NA, NA, NA, NA, NA, 0.7, 1, 1, 1, 8), 
                                 LOG_L_5 = c(NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, NA), 
                                 LOG_L_6 = c(NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, NA), 
                                 LOG_L_7 = c(NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, NA), 
                                 LOG_L_8 = c(NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, NA), 
                                 LOG_L_9 = c(NA, NA, NA, NA, NA, NA, NA, 1, 0.7, 1.7, NA))
  # VOL_TREE_MAX_LOGS LOG_L_0 LOG_L_1 LOG_L_2 LOG_L_3 LOG_L_4 LOG_L_5 LOG_L_6 LOG_L_7 LOG_L_8 LOG_L_9
  # 1:                NA      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
  # 2:                 1     0.3     0.3      NA      NA      NA      NA      NA      NA      NA      NA
  # 3:                 1     0.3     0.3      NA      NA      NA      NA      NA      NA      NA      NA
  # 4:                 2     0.3     0.2     0.1      NA      NA      NA      NA      NA      NA      NA
  # 5:                 3     0.3     3.0     3.0     3.0      NA      NA      NA      NA      NA      NA
  # 6:                 3     0.3     3.0     3.0     2.7      NA      NA      NA      NA      NA      NA
  # 7:                 4     0.3     3.0     3.0     3.0     0.7      NA      NA      NA      NA      NA
  # 8:                 9     0.3     1.0     1.0     1.0     1.0       1       1       1       1     1.0
  # 9:                 9     0.3     1.0     1.0     1.0     1.0       1       1       1       1     0.7
  # 10:                 9     0.3     1.0     1.0     1.0     1.0       1       1       1       1     1.7
  # 11:                 4     0.3     4.0     0.2     6.0     8.0      NA      NA      NA      NA      NA
  
  expect_true(all.equal(adjustedlog1, adjustedlog1_ref))
  
  ## adjust by minloglength
  adjustedlog2 <- suppressWarnings(logMatrixAdjustment(logLengthMatrix = logmatrix[,1:9],
                                                       height = logmatrix$HEIGHT,
                                                       stumpHeight = 0.3,
                                                       logMinLength = 1,
                                                       logDefaultLength = 5))
  adjustedlog2_ref <- data.table(VOL_TREE_MAX_LOGS = c(NA, 1, 1, 1, 3, 3, 4, 9, 9, 9, 4), 
                                 LOG_L_0 = c(NA, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3), 
                                 LOG_L_1 = c(NA, 0.3, 0.3, 0.3, 3, 3, 3, 1, 1, 1, 4), 
                                 LOG_L_2 = c(NA, NA, NA, NA, 3, 3, 3, 1, 1, 1, 5), 
                                 LOG_L_3 = c(NA, NA, NA, NA, 3, 2.7, 3, 1, 1, 1, 6), 
                                 LOG_L_4 = c(NA, NA, NA, NA, NA, NA, 0.7, 1, 1, 1, 3.2), 
                                 LOG_L_5 = c(NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, NA), 
                                 LOG_L_6 = c(NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, NA), 
                                 LOG_L_7 = c(NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, NA), 
                                 LOG_L_8 = c(NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, NA), 
                                 LOG_L_9 = c(NA, NA, NA, NA, NA, NA, NA, 1, 0.7, 1.7, NA))
  expect_true(all.equal(adjustedlog2, adjustedlog2_ref))
})