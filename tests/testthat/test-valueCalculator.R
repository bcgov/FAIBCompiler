test_that("valueCalculator.R: tree value is not correctly calculated.", {
  library(data.table)
  library(testthat)
  ## check the error
  # 1. number of row is right, ncol is not right
  logvolmatrix <- data.table(log_v_0 = 1:5,
                             log_v_1 = 5:9,
                             log_v_2 = c(5:7, NA, NA)) ## has 5 rows and 3 columns
  merchlogmatrix <- data.table(log_vm_1 = 1:5,
                               log_vm_2 = 5:9,
                               log_vm_3 = 7:11) ## has 5 rows and 3 columns
  soundtable <- data.table(log_s_1 = rep(100, 5),
                           log_s_2 = rep(80, 5),
                           log_s_3 = rep(70, 5),
                           log_s_4 = rep(60, 5)) ## has 5 rows and 4 columns
  expect_error(valueCalculator(species = "AC", 
                               grossVolMatrix = logvolmatrix,
                               grossMerchVolMatrix = merchlogmatrix,
                               callGradeMatrix = soundtable))
  # 2. number of row is wrong and number of col is right
  logvolmatrix <- data.table(log_v_0 = 1:5,
                             log_v_1 = 5:9,
                             log_v_2 = c(5:7, NA, NA)) ## has 5 rows and 3 columns
  merchlogmatrix <- data.table(log_vm_1 = 1:3,
                               log_vm_2 = 5:7) ## has 5 rows and 3 columns
  soundtable <- data.table(log_s_1 = rep(100, 5),
                           log_s_2 = rep(80, 5)) ## has 5 rows and 4 columns
  expect_error(valueCalculator(species = "AC",
                               grossVolMatrix = logvolmatrix,
                               grossMerchVolMatrix = merchlogmatrix,
                               callGradeMatrix = soundtable))
 
  ## check the outputs along with sas compiler
  ## 
  randomsasoutputs <- data.table(CLSTR_ID = c("0071-0079-NO1", "CMI2-0425-FR1", 
                                              "0172-1911-MO1", "0242-0086-NO1", "0091-0118-DO1", "0221-0070-TO1", 
                                              "DPG1-0010-NO1", "DPG1-0124-DO1", "DME1-0032-QR1", "011M-0082-MR1"), 
                                 PLOT = c("I", "I", "I", "N", "I", "W", "W", "N", 
                                          "I", "I"), 
                                 TREE_NO = c("007", 
                                             "060", "194", "001", "001", "004", "002", "005", "004", "201"),
                                 SPECIES = c("SE", "FDI", "BL", "SX", "BL", "PY", "SE", "AT", 
                                             "PY", "FDI"),
                                 LOG_V_0 = c(0.115653255733009, 
                                             0.0683438028704155, 0.0203226570323606, 0.199506032292159, 
                                             0.0316514639234829, 0.238202640191485, 0.0920843487373005, 
                                             0.0279606601119462, 0.120399650145645, 0.0767425813663651),
                                 LOG_V_1 = c(1.99186066371631, 1.31566450371338, 
                                             0.270473606918279, 1.83203738785607, 0.787142840916972, 4.3374748058904, 
                                             1.37638078511191, 0.136673116479434, 1.67919378465936, 1.1048730845388), 
                                 LOG_V_2 = c(0.723002021451179, 
                                             0.445775066064147, 0.136142504066935, 1.66396381054031, 0.308896881360363, 
                                             2.00638726831731, 0.424587812779612, 0.585443272877791, 0.700402139851381, 
                                             1.13831789282572), 
                                 LOG_V_3 = c(0.722456018914301, 
                                             0.501792009737348, 0.0737936007500049, 1.74683186506489, 
                                             0.0239241721384739, 1.21782510878648, 0.344345381186035, 
                                             0.0996147915024272, 0.414942323557599, 0.461132223528808), 
                                 LOG_V_4 = c(NA_real_, NA_real_, NA_real_, NA_real_, 
                                             NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), 
                                 LOG_VM_1 = c(1.99186066371631, 
                                              1.31566450371338, 0.270473606918279, 1.83203738785607, 0.787142840916972, 
                                              4.3374748058904, 1.37638078511191, 0.136673116479434, 1.67919378465936, 
                                              1.1048730845388),
                                 LOG_VM_2 = c(0.723002021451179, 
                                              0.445775066064147, 0.136142504066935, 1.66396381054031, 0.308896881360363, 
                                              2.00638726831731, 0.424587812779612, 0.585443272877791, 0.700402139851381, 
                                              1.13831789282572), 
                                 LOG_VM_3 = c(0.708624127050974, 
                                              0.490191592317167, 0.0581736172535729, 1.73616458656735, 
                                              0.0062246980089177, 1.21110968435012, 0.333184581791034, 
                                              0.0859368095607724, 0.408667733071952, 0.449892147685851), 
                                 LOG_VM_4 = c(NA_real_, NA_real_, NA_real_, NA_real_, 
                                              NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), 
                                 LOG_G_1 = c("H", 
                                                "I", "J", "H", "J", "H", "I", "Y", "I", "H"), 
                                 LOG_G_2 = c("J", "U", "U", "I", "U", "U", "U", 
                                             "U", "U", "J"), 
                                 LOG_G_3 = c("U", "X", "Y", "U", "Y", "Y", "Y", 
                                             "Y", "Y", "X"), 
                                 LOG_G_4 = c("", "", "", "", "", "", "", "", "", ""), 
                                 VAL_NET = c(302.22533536152, 
                                             177.327773624792, 25.675541131859, 404.928832382611, 62.205517862466, 
                                             421.514168354895, 137.830552611847, 28.7605913300878, 109.768589200405, 
                                             279.54065458192), 
                                 VAL_MER = c(301.60290022767, 177.037763189288, 
                                             24.9726418745196, 404.448804850222, 61.4090415266359, 421.379859866168, 
                                             137.328316639072, 28.2818619621299, 109.643097390692, 279.259652685847), 
                                 LOG_C_1 = c(219.104673008794, 
                                             144.723095408472, 16.2284164150967, 201.524112664168, 47.2285704550183, 
                                             346.997984471232, 103.228558883393, 4.7835590767802, 83.9596892329681, 
                                             171.255328103514), 
                                 LOG_C_2 = c(50.6101415015825, 
                                             20.0598779728866, 6.12641268301206, 124.797285790523, 13.9003596612163, 
                                             50.1596817079328, 19.1064515750825, 20.4905145507227, 17.5100534962845, 
                                             96.7570208901862), 
                                 LOG_C_3 = c(32.5105208511435, 
                                             12.5448002434337, 3.32071203375022, 78.60743392792, 1.07658774623133, 
                                             24.3565021757296, 15.4955421533716, 3.48651770258495, 8.29884647115197, 
                                             11.5283055882202), 
                                 LOG_C_4 = c(NA_real_, 
                                             NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, 
                                             NA_real_, NA_real_, NA_real_))
  
  # CLSTR_ID PLOT TREE_NO    LOG_V_0   LOG_V_1   LOG_V_2    LOG_V_3 LOG_V_4  LOG_VM_1
  # 1: 0071-0079-NO1    I     007 0.11565326 1.9918607 0.7230020 0.72245602      NA 1.9918607
  # 2: CMI2-0425-FR1    I     060 0.06834380 1.3156645 0.4457751 0.50179201      NA 1.3156645
  # 3: 0172-1911-MO1    I     194 0.02032266 0.2704736 0.1361425 0.07379360      NA 0.2704736
  # 4: 0242-0086-NO1    N     001 0.19950603 1.8320374 1.6639638 1.74683187      NA 1.8320374
  # 5: 0091-0118-DO1    I     001 0.03165146 0.7871428 0.3088969 0.02392417      NA 0.7871428
  # 6: 0221-0070-TO1    W     004 0.23820264 4.3374748 2.0063873 1.21782511      NA 4.3374748
  # 7: DPG1-0010-NO1    W     002 0.09208435 1.3763808 0.4245878 0.34434538      NA 1.3763808
  # 8: DPG1-0124-DO1    N     005 0.02796066 0.1366731 0.5854433 0.09961479      NA 0.1366731
  # 9: DME1-0032-QR1    I     004 0.12039965 1.6791938 0.7004021 0.41494232      NA 1.6791938
  # 10: 011M-0082-MR1    I     201 0.07674258 1.1048731 1.1383179 0.46113222      NA 1.1048731
  # LOG_VM_2    LOG_VM_3 LOG_VM_4 LOG_G_1 LOG_G_2 LOG_G_3 LOG_G_4   VAL_NET   VAL_MER
  # 1: 0.7230020 0.708624127       NA       H       J       U         302.22534 301.60290
  # 2: 0.4457751 0.490191592       NA       I       U       X         177.32777 177.03776
  # 3: 0.1361425 0.058173617       NA       J       U       Y          25.67554  24.97264
  # 4: 1.6639638 1.736164587       NA       H       I       U         404.92883 404.44880
  # 5: 0.3088969 0.006224698       NA       J       U       Y          62.20552  61.40904
  # 6: 2.0063873 1.211109684       NA       H       U       Y         421.51417 421.37986
  # 7: 0.4245878 0.333184582       NA       I       U       Y         137.83055 137.32832
  # 8: 0.5854433 0.085936810       NA       Y       U       Y          28.76059  28.28186
  # 9: 0.7004021 0.408667733       NA       I       U       Y         109.76859 109.64310
  # 10: 1.1383179 0.449892148       NA       H       J       X         279.54065 279.25965
  # LOG_C_1    LOG_C_2   LOG_C_3 LOG_C_4
  # 1: 219.104673  50.610142 32.510521      NA
  # 2: 144.723095  20.059878 12.544800      NA
  # 3:  16.228416   6.126413  3.320712      NA
  # 4: 201.524113 124.797286 78.607434      NA
  # 5:  47.228570  13.900360  1.076588      NA
  # 6: 346.997984  50.159682 24.356502      NA
  # 7: 103.228559  19.106452 15.495542      NA
  # 8:   4.783559  20.490515  3.486518      NA
  # 9:  83.959689  17.510053  8.298846      NA
  # 10: 171.255328  96.757021 11.528306      NA
  
  netvols <- valueCalculator(species = randomsasoutputs$SPECIES,
                             grossVolMatrix = randomsasoutputs[, paste("LOG_V_", 0:4, sep = ""), with = FALSE],
                             grossMerchVolMatrix = randomsasoutputs[, paste("LOG_VM_", 1:4, sep = ""), with = FALSE],
                             callGradeMatrix = randomsasoutputs[, paste("LOG_G_", 1:4, sep = ""), with = FALSE])
  ## FOR MERCHANTABLE VALUE
  expect_equal(netvols$VAL_MER, randomsasoutputs$VAL_MER)
  ## FOR VOLUME VALUE
  expect_equal(netvols$VAL_NET, randomsasoutputs$VAL_NET)
  ## FOR EACH LOG
  expect_equal(netvols$LOG_C_1, randomsasoutputs$LOG_C_1)
  expect_equal(netvols$LOG_C_2, randomsasoutputs$LOG_C_2)
  expect_equal(netvols$LOG_C_3, randomsasoutputs$LOG_C_3)
  expect_equal(netvols$LOG_C_4, randomsasoutputs$LOG_C_4)
  
  
  
})