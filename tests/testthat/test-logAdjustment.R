test_that("logAdjustment.R: log length is not correctly adjusted.", {
  library(data.table)
  library(testthat)

  ## 1.
  testtreedata <- data.table(BTOP = c(NA, NA, "D"),
                             MEAS_INTENSE = c("FULL", "FULL", "ENHANCED"),
                             HEIGHT = c(NA, NA, 7),
                             HT = c(NA, NA, 10),
                             # HT_BTOP = c(NA, NA, 7),
                             NO_LOGS = c(NA, 2, 2),
                             LOG_L_1 = c(NA, 5, NA),
                             LOG_L_2 = c(NA, 7, NA))
  testtreedata[, paste("LOG_L_", 3:9, sep = "") := NA]
  testtreedata[, paste("LOG_G_", 1:9, sep = "") := as.character(NA)]
  testtreedata[, paste("LOG_S_", 1:9, sep = "") := as.numeric(NA)]

  # print(testtreedata[,.(BTOP, HEIGHT, HT, NO_LOGS, LOG_L_1, LOG_L_2)])
  #     BTOP HEIGHT HT NO_LOGS LOG_L_1 LOG_L_2
  # 1:   NA     NA NA      NA      NA      NA ## for testing log length and tree height information is not available
  # 2:   NA     NA NA       2       5       7 ## for testing log is available and height is not
  # 3:    D      7 10       2      NA      NA ## for log is not available and height is
  treedata_adjusted <- logAdjustment(treeData = testtreedata)
  expect_equal(treedata_adjusted[,.(BTOP, HEIGHT, HT, NO_LOGS, LOG_L_1, LOG_L_2,
                                    LOGADJUST, LOG_L_0)],
               data.table(BTOP = c(NA, NA, "D"),
                          HEIGHT = c(NA, NA, 7),
                          HT = c(NA, 12, 10),
                          NO_LOGS = c(NA, 2, 2),
                          LOG_L_1 = c(NA, 4.7, 6.7),
                          LOG_L_2 = c(NA, 7, 3),
                          LOGADJUST = c("FAIL", "PASS", "PASS"),
                          LOG_L_0 = c(NA, 0.3, 0.3)))
  rm(testtreedata, treedata_adjusted)


  ## nonbroken top trees
  testtreedata <- data.table(uniObs = 1:6,
                             BTOP = c(NA, NA, NA, NA, NA, NA),
                             MEAS_INTENSE = c(rep("FULL", 3), rep("ENHANCED", 3)),
                             HEIGHT = c(30, 30, 30, 50, 50, 50),
                             HT = c(30, 30, 30, 50, 50, 50),
                             # HT_BTOP = c(NA, NA, 7),
                             NO_LOGS = c(1, 1, 1, 3, 3, 3),
                             LOG_L_1 = c(30, 25, 35, 30, 30, 30),
                             LOG_L_2 = c(NA, NA, NA, 10, 10, 10),
                             LOG_L_3 = c(NA, NA, NA, 10, 5, 15))
  ## print(testtreedata)
  #     BTOP HEIGHT HT NO_LOGS LOG_L_1 LOG_L_2 LOG_L_3
  # 1:   NA     30 30       1      30      NA      NA ## exact match
  # 2:   NA     30 30       1      25      NA      NA ## total log length less than tree height
  # 3:   NA     30 30       1      35      NA      NA ## total log length more than tree height
  # 4:   NA     50 50       3      30      10      10 ## exact match
  # 5:   NA     50 50       3      30      10       5 ## total log length less than tree height
  # 6:   NA     50 50       3      30      10      15 ## total log length more than tree height

  testtreedata[, paste("LOG_L_", 4:9, sep = "") := NA]
  testtreedata[, paste("LOG_G_", 1:9, sep = "") := as.character(NA)]
  testtreedata[, paste("LOG_S_", 1:9, sep = "") := as.numeric(NA)]

  treedata_adjusted <- logAdjustment(treeData = testtreedata)
  # print(treedata_adjusted[order(uniObs),.(uniObs, HEIGHT, HT, NO_LOGS, LOG_L_0, LOG_L_1,
  #                                         LOG_L_2, LOG_L_3)])
  # uniObs HEIGHT HT NO_LOGS LOG_L_0 LOG_L_1 LOG_L_2 LOG_L_3
  # 1:      1     30 30       1     0.3    29.7      NA      NA ## GOOD
  # 2:      2     30 30       1     0.3    29.7      NA      NA ## GOOD
  # 3:      3     30 30       1     0.3    29.7      NA      NA ## GOOD
  # 4:      4     50 50       3     0.3    29.7      10      10 ## GOOD
  # 5:      5     50 50       2     0.3    29.7      10       0 ## DO NOT MAKE SENSE NEED CONFIRM WITH RENE OR GETA
  # 6:      6     50 50       3     0.3    29.7      10      10 ## GOOD
  expect_equal(treedata_adjusted[order(uniObs),.(HEIGHT, HT, NO_LOGS, LOG_L_0, LOG_L_1,
                                                 LOG_L_2, LOG_L_3)],
               data.table(HEIGHT = c(30, 30, 30, 50, 50, 50),
                          HT = c(30, 30, 30, 50, 50, 50),
                          NO_LOGS = c(1, 1, 1, 3, 2, 3),
                          LOG_L_0 = rep(0.3, 6),
                          LOG_L_1 = rep(29.7, 6),
                          LOG_L_2 = c(NA, NA, NA, 10, 10, 10),
                          LOG_L_3 = c(NA, NA, NA, 10, 0, 10)))
  rm(testtreedata, treedata_adjusted)



  ## broken top trees
  testtreedata <- data.table(uniObs = 1:6,
                             BTOP = c("D", "H", "D", "H", "D", "H"),
                             MEAS_INTENSE = c(rep("FULL", 3), rep("ENHANCED", 3)),
                             HEIGHT = c(30, 30, 30, 50, 50, 50),
                             HT = c(35, 36, 37, 55, 56, 57),
                             # HT_BTOP = c(NA, NA, 7),
                             NO_LOGS = c(2, 2, 2, 4, 4, 4),
                             LOG_L_1 = c(30, 25, 32, 30, 25, 12),
                             LOG_L_2 = c(NA, NA, NA, 10, 10, 20),
                             LOG_L_3 = c(NA, NA, NA, 10, 10, 20))
  testtreedata[, paste("LOG_L_", 4:9, sep = "") := NA]
  testtreedata[, paste("LOG_G_", 1:9, sep = "") := as.character(NA)]
  testtreedata[, paste("LOG_S_", 1:9, sep = "") := as.numeric(NA)]

  ## print(testtreedata)
  #     BTOP HEIGHT HT NO_LOGS LOG_L_1 LOG_L_2 LOG_L_3
  # 1:   D     30 35       2      30      NA      NA ## exact match between broken height and log length
  # 2:   H     30 36       2      25      NA      NA ## log 1 length less than tree broken height
  # 3:   D     30 37       2      32      NA      NA ## log 1 length more than tree broken height (currently not be implemented)
  # 4:   H     50 55       4      30      10      10 ## exact match between broken height and log length
  # 5:   D     50 56       4      25      10      10 ## total log1-3 length less than tree broken height
  # 6:   H     50 57       4      12      20      20 ## total log1-3 length more than tree broken height

  treedata_adjusted <- logAdjustment(treeData = testtreedata)
  # print(treedata_adjusted[order(uniObs),.(uniObs, HEIGHT, HT, NO_LOGS, LOG_L_0, LOG_L_1,
  #                                         LOG_L_2, LOG_L_3, LOG_L_4)])
  #      uniObs HEIGHT HT NO_LOGS LOG_L_0 LOG_L_1 LOG_L_2 LOG_L_3 LOG_L_4
  # 1:      1     30 35       2     0.3    29.7       5      NA      NA # good
  # 2:      2     30 36       2     0.3    29.7       6      NA      NA # good
  # 3:      3     30 37       2     0.3    29.7       7      NA      NA # good
  # 4:      4     50 55       4     0.3    29.7      10      10       5 # good
  # 5:      5     50 56       4     0.3    24.7      10      15       6 # good
  # 6:      6     50 57       4     0.3    11.7      20      18       7 # good
  expect_equal(treedata_adjusted[order(uniObs),.(BTOP, HEIGHT, HT, NO_LOGS, LOG_L_0, LOG_L_1,
                                                 LOG_L_2, LOG_L_3, LOG_L_4)],
               data.table(BTOP = c("D", "H", "D", "H", "D", "H"),
                          HEIGHT = c(30, 30, 30, 50, 50, 50),
                          HT = c(35, 36, 37, 55, 56, 57),
                          NO_LOGS = c(2, 2, 2, 4, 4, 4),
                          LOG_L_0 = rep(0.3, 6),
                          LOG_L_1 = c(rep(29.7, 4), 24.7, 11.7),
                          LOG_L_2 = c(5:7, 10, 10, 20),
                          LOG_L_3 = c(NA, NA, NA, 10, 15, 18),
                          LOG_L_4 = c(NA, NA, NA, 5:7)))
  rm(testtreedata, treedata_adjusted)



  ## similar to nonbroken top trees, test for a special case where the difference is same as length
  ## of last log
  testtreedata <- data.table(uniObs = 1:2,
                             BTOP = c("D", "H"),
                             MEAS_INTENSE = c("FULL", "ENHANCED"),
                             HEIGHT = c(30, 30),
                             HT = c(35, 36),
                             # HT_BTOP = c(NA, NA, 7),
                             NO_LOGS = c(2, 3),
                             LOG_L_1 = c(15, 20),
                             LOG_L_2 = c(NA, 5),
                             LOG_L_3 = c(NA, NA))
  # print(testtreedata)
  #     uniObs BTOP HEIGHT HT NO_LOGS LOG_L_1 LOG_L_2 LOG_L_3
  # 1:      1    D     30 35       2      15      NA      NA # diff = 30-15 == 15 (length of last log)
  # 2:      2    H     30 36       3      20       5      NA # diff = 30-25 == 5 (length of last log)
  testtreedata[, paste("LOG_L_", 4:9, sep = "") := NA]
  testtreedata[, paste("LOG_G_", 1:9, sep = "") := as.character(NA)]
  testtreedata[, paste("LOG_S_", 1:9, sep = "") := as.numeric(NA)]
  treedata_adjusted <- logAdjustment(treeData = testtreedata)
  # print(treedata_adjusted[order(uniObs),.(uniObs, HEIGHT, HT, NO_LOGS, LOG_L_0, LOG_L_1,
  #                                         LOG_L_2, LOG_L_3)])
  #      uniObs HEIGHT HT NO_LOGS LOG_L_0 LOG_L_1 LOG_L_2 LOG_L_3
  # 1:      1     30 35       1     0.3     4.7      NA      NA ## does not make sense
  # 2:      2     30 36       2     0.3    19.7       6      NA ## does not make sense

  expect_equal(treedata_adjusted[order(uniObs),.(HEIGHT, HT, NO_LOGS, LOG_L_0, LOG_L_1,
                                                 LOG_L_2, LOG_L_3)],
               data.table(HEIGHT = c(30, 30),
                          HT = c(35, 36),
                          NO_LOGS = 1:2,
                          LOG_L_0 = 0.3,
                          LOG_L_1 = c(4.7, 19.7),
                          LOG_L_2 = c(NA, 6),
                          LOG_L_3 = as.numeric(NA)))

  rm(testtreedata, treedata_adjusted)

  ## comprehensive testing
  testtreedata <- data.table(uniObs = 1:6,
                             BTOP = c(NA, NA, NA, NA, "D", "H"),
                             MEAS_INTENSE = c(rep("FULL", 3), rep("ENHANCED", 3)),
                             HEIGHT = c(NA, 30, 30, 30, 30, 30),
                             HT = c(NA, 30, 30, 30, 35, 36),
                             # HT_BTOP = c(NA, NA, 7),
                             NO_LOGS = c(NA, 2, 3, 3, 2, 3),
                             LOG_L_1 = c(NA, 15, 0.2, 5, 10, 20),
                             LOG_L_2 = c(NA, 15, 19.8, 7, NA, 6),
                             LOG_L_3 = c(NA, NA, 10, 16, NA, NA))
  #     uniObs BTOP HEIGHT HT NO_LOGS LOG_L_1 LOG_L_2 LOG_L_3
  # 1:      1   NA     NA NA      NA      NA      NA      NA
  # 2:      2   NA     30 30       2    15.0    15.0      NA
  # 3:      3   NA     30 30       3     0.2    19.8      10
  # 4:      4   NA     30 30       3     5.0     7.0      16
  # 5:      5    D     30 35       2    10.0      NA      NA
  # 6:      6    H     30 36       3    20.0     6.0      NA

  testtreedata[, paste("LOG_L_", 4:9, sep = "") := NA]
  testtreedata[, paste("LOG_G_", 1:9, sep = "") := as.character(NA)]
  testtreedata[, paste("LOG_S_", 1:9, sep = "") := as.numeric(NA)]
  treedata_adjusted <- logAdjustment(treeData = testtreedata)

   print(treedata_adjusted[order(uniObs),.(uniObs, HEIGHT, HT, NO_LOGS, LOG_L_0, LOG_L_1,
                                           LOG_L_2, LOG_L_3, LOGADJUST)])
  #    uniObs HEIGHT HT NO_LOGS LOG_L_0 LOG_L_1 LOG_L_2 LOG_L_3     LOGADJUST
  # 1:      1     NA NA      NA      NA      NA      NA      NA          FAIL
  # 2:      2     30 30       2     0.3    14.7    15.0      NA          PASS
  # 3:      3     30 30       3      NA     0.2    19.8      10 SMALLFIRSTLOG
  # 4:      4     30 30       3     0.3     4.7     7.0      18          PASS
  # 5:      5     30 35       2     0.3    29.7     5.0      NA          PASS
  # 6:      6     30 36       3     0.3    19.7    10.0       6          PASS

  expect_equal(treedata_adjusted[order(uniObs),.(HEIGHT, HT, NO_LOGS, LOG_L_0, LOG_L_1,
                                                 LOG_L_2, LOG_L_3, LOGADJUST)],
               data.table(HEIGHT = c(NA, 30, 30, 30, 30, 30),
                          HT = c(NA, 30, 30, 30, 35, 36),
                          NO_LOGS = c(NA, 2, 3, 3, 2, 3),
                          LOG_L_0 = c(NA, 0.3, NA, 0.3, 0.3, 0.3),
                          LOG_L_1 = c(NA, 14.7, 0.2, 4.7, 29.7, 19.7),
                          LOG_L_2 = c(NA, 15, 19.8, 7, 5, 10),
                          LOG_L_3 = c(NA, NA, 10, 18, NA, 6),
                          LOGADJUST = c("FAIL", "PASS", "SMALLFIRSTLOG", "PASS",
                                        "PASS", "PASS")))

  ## for H enhanced trees, those trees theoritically do not need to adjust,
  ## however, the function allows to do so to make all trees that at entrance to
  ## volume have same data formats
  testtreedata <- data.table(uniObs = 1:6,
                             BTOP = c(NA, NA, NA, NA, NA, NA),
                             MEAS_INTENSE = c(rep("H-ENHANCED", 3)),
                             HEIGHT = seq(10, 50, length = 6),
                             HT = NA,
                             # HT_BTOP = c(NA, NA, 7),
                             NO_LOGS = c(1, 1, 1, 3, 3, 3),
                             LOG_L_1 = c(30, 25, 35, 30, 30, 30),
                             LOG_L_2 = c(NA, NA, NA, 10, 10, 10),
                             LOG_L_3 = c(NA, NA, NA, 10, 5, 15))
  ## print(testtreedata)
  #     BTOP HEIGHT HT NO_LOGS LOG_L_1 LOG_L_2 LOG_L_3
  # 1:   NA     10 NA       1      30      NA      NA ## exact match
  # 2:   NA     18 NA       1      25      NA      NA ## total log length less than tree height
  # 3:   NA     26 NA       1      35      NA      NA ## total log length more than tree height
  # 4:   NA     34 NA       3      30      10      10 ## exact match
  # 5:   NA     42 NA       3      30      10       5 ## total log length less than tree height
  # 6:   NA     50 NA       3      30      10      15 ## total log length more than tree height

  testtreedata[, paste("LOG_L_", 4:9, sep = "") := NA]
  testtreedata[, paste("LOG_G_", 1:9, sep = "") := as.character(NA)]
  testtreedata[, paste("LOG_S_", 1:9, sep = "") := as.numeric(NA)]

  treedata_adjusted <- logAdjustment(treeData = testtreedata)

  expect_equal(treedata_adjusted[order(uniObs),.(HEIGHT, HT, NO_LOGS, LOG_L_0, LOG_L_1,
                                                 LOG_L_2, LOG_L_3)],
               data.table(HEIGHT = seq(10, 50, length = 6),
                          HT = NA,
                          NO_LOGS = 1,
                          LOG_L_0 = rep(0.3, 6),
                          LOG_L_1 = seq(9.7, 49.7, length.out = 6),
                          LOG_L_2 = as.numeric(NA),
                          LOG_L_3 = as.numeric(NA)))
  rm(testtreedata, treedata_adjusted)
})
