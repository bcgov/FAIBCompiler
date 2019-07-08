test_that("netVolumeCalculator.R: total net volume is not correctly calculated.", {
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
  expect_error(netVolumeCalculator(grossVolMatrix = logvolmatrix,
                                   grossMerchVolMatrix = merchlogmatrix,
                                   netFactorMatrix = soundtable))
  # 2. number of row is wrong and number of col is right
  logvolmatrix <- data.table(log_v_0 = 1:5,
                             log_v_1 = 5:9,
                             log_v_2 = c(5:7, NA, NA)) ## has 5 rows and 3 columns
  merchlogmatrix <- data.table(log_vm_1 = 1:3,
                               log_vm_2 = 5:7) ## has 5 rows and 3 columns
  soundtable <- data.table(log_s_1 = rep(100, 5),
                           log_s_2 = rep(80, 5)) ## has 5 rows and 4 columns
  expect_error(netVolumeCalculator(grossVolMatrix = logvolmatrix,
                                   grossMerchVolMatrix = merchlogmatrix,
                                   netFactorMatrix = soundtable))
  ## check missing 
  # 1. missing grossVolMatrix
  merchlogmatrix <- data.table(log_vm_1 = 1:5,
                               log_vm_2 = 5:9) 
  soundtable <- data.table(log_s_1 = rep(100, 5),
                           log_s_2 = rep(80, 5)) 
  netvols <- netVolumeCalculator(grossMerchVolMatrix = merchlogmatrix,
                                 netFactorMatrix = soundtable)
  expect_equal(netvols$VOL_NET, rep(0, 5)) ## total net volume should be 0
  # 2. missing grossMerchVolMatrix
  logvolmatrix <- data.table(log_v_0 = 1:5,
                             log_v_1 = 5:9,
                             log_v_2 = c(5:7, NA, NA))
  soundtable <- data.table(log_s_1 = rep(100, 5),
                           log_s_2 = rep(80, 5))
  netvols <- netVolumeCalculator(grossVolMatrix = logvolmatrix,
                                 netFactorMatrix = soundtable)
  expect_equal(netvols$VOL_NETM, rep(0, 5)) ## total net merchantable volume should be 0
  # 3. missing netFactorMatrix, default is 100
  logvolmatrix <- data.table(log_v_0 = 1:5,
                             log_v_1 = 5:9,
                             log_v_2 = c(5:7, NA, NA))
  # log_v_0 log_v_1 log_v_2
  # 1:       1       5       5 ## sum 11
  # 2:       2       6       6 ## sum 14
  # 3:       3       7       7 ## sum 17
  # 4:       4       8      NA ## sum 12
  # 5:       5       9      NA ## sum 14
  merchlogmatrix <- data.table(log_vm_1 = 11:15,
                               log_vm_2 = 15:19) 
  # log_vm_1 log_vm_2
  # 1:       11       15 ## sum 26
  # 2:       12       16 ## sum 28
  # 3:       13       17 ## sum 30
  # 4:       14       18 ## sum 32
  # 5:       15       19 ## sum 34
  
  netvols <- netVolumeCalculator(grossVolMatrix = logvolmatrix,
                                 grossMerchVolMatrix =  merchlogmatrix)
  expect_equal(netvols$VOL_NET, c(11, 14, 17, 12, 14))
  expect_equal(netvols$VOL_NETM, c(26, 28, 30, 32, 34))
  
  ## check the outputs along with sas compiler
  ## 
  randomsasoutputs <- data.table(CLSTR_ID = c("DME1-0015-TO1", "3471-0060-DO1", 
                                             "3233-0008-OR1", "DAR2-0075-QO1", "DLL1-0122-Q 1", "4791-0027-NO1", 
                                             "3471-0085-NO1", "0222-0132-QO1", "3233-0098-NO1", "4701-0019-QO1"), 
                                PLOT = c("I", "I", "I", "I", "I", "E", "N", "I", "E", "N"),
                                TREE_NO = c("007", "190", "001", "006", "005", "004", "009", "002", "008", "008"),
                                LOG_V_0 = c(0.0263937121083685, 0.0558437896715244, 0.1372710526282, 0.161758575305153, 0.199839244940211, 
                                            0.177951797578219, 0.132231271911848, 0.063166631832148, 
                                            0.0944535578549054, 0.359025311630049), 
                                LOG_V_1 = c(0.374263721199455, 
                                            0.816875171810426, 2.05134993767047, 2.54955686910342, 1.80787396175441, 
                                            2.96626538566636, 2.29546297048104, 1.01327433254549, 0.907124781656755, 
                                            5.24519234468211),
                                LOG_V_2 = c(0.104848023327086, 
                                            0.699368020720212, 0.672634858706612, 1.21754218393219, 0.272798981144663, 
                                            1.18126510996936, 1.22809306802828, 0.32902011036264, 0.521546199170711, 
                                            8.61426020320531), 
                                LOG_V_3 = c(0.109452683516522, 
                                            0.336698018374717, 0.741850461283993, 0.559192046479827, 
                                            1.10872291994219, 1.66715748410779, 0.847882207671338, 0.00371100232733636, 
                                            1.16400224162801, 2.39078583203937), 
                                LOG_V_4 = c(NA, NA, 0.379500532304309, NA, 1.11666862811339, 
                                            NA, NA, NA, NA, 1.55768349040926), 
                                LOG_VM_1 = c(0.374263721199455, 0.816875171810426, 
                                             2.05134993767047, 2.54955686910342, 1.80787396175441, 2.96626538566636, 
                                             2.29546297048104, 1.01327433254549, 0.907124781656755, 5.24519234468211),
                                LOG_VM_2 = c(0.104848023327086, 
                                             0.699368020720212, 0.672634858706612, 1.21754218393219, 0.272798981144663, 
                                             1.18126510996936, 1.22809306802828, 0.321425912307692, 0.521546199170711, 
                                             8.61426020320531), 
                                LOG_VM_3 = c(0.0987970266543662, 
                                             0.325252800027074, 0.741850461283993, 0.547939452695515, 
                                             1.10872291994219, 1.65528116925591, 0.835549050355266, NA, 
                                             1.15137606170672, 2.39078583203937), 
                                LOG_VM_4 = c(NA, NA, 0.366825048596119, NA, 1.10931278208351, 
                                             NA, NA, NA, NA, 1.54901282282335), 
                                LOG_S_1 = c(100, 100, 96, 100, 100, 94, 100, 100, 
                                            100, 100), 
                                LOG_S_2 = c(100, 
                                            100, 100, 100, 100, 90, 100, 100, 100, 100), 
                                LOG_S_3 = c(100, 100, 100, 100, 100, 100, 100, 
                                            100, 100, 100), 
                                LOG_S_4 = c(NA, 
                                            NA, 100, NA, 100, NA, NA, NA, NA, 100), 
                                VOL_NET = c(0.614958140151431, 1.90878500057688, 
                                            3.89506200298164, 4.48804967482059, 4.50590373589486, 5.68586023533012, 
                                            4.50366951809251, 1.40917207706761, 2.68712678031038, 18.1669471819661), 
                                VOL_NETM = c(0.577908771180906, 
                                             1.84149599255771, 3.75060630875038, 4.31503850573113, 4.29870864492476, 
                                             5.50670923075472, 4.35910508886459, 1.33470024485318, 2.58004704253418, 
                                             17.7992512027501))
  # CLSTR_ID PLOT TREE_NO    LOG_V_0   LOG_V_1   LOG_V_2     LOG_V_3   LOG_V_4  LOG_VM_1
  # 1: DME1-0015-TO1    I     007 0.02639371 0.3742637 0.1048480 0.109452684        NA 0.3742637
  # 2: 3471-0060-DO1    I     190 0.05584379 0.8168752 0.6993680 0.336698018        NA 0.8168752
  # 3: 3233-0008-OR1    I     001 0.13727105 2.0513499 0.6726349 0.741850461 0.3795005 2.0513499
  # 4: DAR2-0075-QO1    I     006 0.16175858 2.5495569 1.2175422 0.559192046        NA 2.5495569
  # 5: DLL1-0122-Q 1    I     005 0.19983924 1.8078740 0.2727990 1.108722920 1.1166686 1.8078740
  # 6: 4791-0027-NO1    E     004 0.17795180 2.9662654 1.1812651 1.667157484        NA 2.9662654
  # 7: 3471-0085-NO1    N     009 0.13223127 2.2954630 1.2280931 0.847882208        NA 2.2954630
  # 8: 0222-0132-QO1    I     002 0.06316663 1.0132743 0.3290201 0.003711002        NA 1.0132743
  # 9: 3233-0098-NO1    E     008 0.09445356 0.9071248 0.5215462 1.164002242        NA 0.9071248
  # 10: 4701-0019-QO1    N     008 0.35902531 5.2451923 8.6142602 2.390785832 1.5576835 5.2451923
  # LOG_VM_2   LOG_VM_3 LOG_VM_4 LOG_S_1 LOG_S_2 LOG_S_3 LOG_S_4    VOL_NET   VOL_NETM
  # 1: 0.1048480 0.09879703       NA     100     100     100      NA  0.6149581  0.5779088
  # 2: 0.6993680 0.32525280       NA     100     100     100      NA  1.9087850  1.8414960
  # 3: 0.6726349 0.74185046 0.366825      96     100     100     100  3.8950620  3.7506063
  # 4: 1.2175422 0.54793945       NA     100     100     100      NA  4.4880497  4.3150385
  # 5: 0.2727990 1.10872292 1.109313     100     100     100     100  4.5059037  4.2987086
  # 6: 1.1812651 1.65528117       NA      94      90     100      NA  5.6858602  5.5067092
  # 7: 1.2280931 0.83554905       NA     100     100     100      NA  4.5036695  4.3591051
  # 8: 0.3214259         NA       NA     100     100     100      NA  1.4091721  1.3347002
  # 9: 0.5215462 1.15137606       NA     100     100     100      NA  2.6871268  2.5800470
  # 10: 8.6142602 2.39078583 1.549013     100     100     100     100 18.1669472 17.7992512
  
  
  netvols <- netVolumeCalculator(grossVolMatrix = randomsasoutputs[, paste("LOG_V_", 0:4, sep = ""), with = FALSE],
                                 grossMerchVolMatrix = randomsasoutputs[, paste("LOG_VM_", 1:4, sep = ""), with = FALSE],
                                 netFactorMatrix = randomsasoutputs[, paste("LOG_S_", 1:4, sep = ""), with = FALSE])
  
  
  expect_equal(netvols$VOL_NET, randomsasoutputs$VOL_NET)
  expect_equal(netvols$VOL_NETM, randomsasoutputs$VOL_NETM)
  
  
})