test_that("assignChangeComponent.R: assign component change at tree level.", {
  library(data.table)
  library(testthat)

  samples <- data.table(SITE_IDENTIFIER = 1234567,
                        VISIT_NUMBER = 1:3,
                        SAMP_TYP = "V")

  treelist <- data.table(SITE_IDENTIFIER = 1234567,
                         VISIT_NUMBER = 1:3,
                         PLOT = "I",
                         DIAMETER = 7:9,
                         TREE_NUMBER = 1,
                         TREE_EXTANT_CODE = "L",
                         LVD_EDIT = as.character(NA),
                         MSMT_MISSING_EDIT = as.character(NA),
                         DIAMETER_EDIT = as.character(NA),
                         MEASUREMENT_ANOMALY_CODE = as.character(NA),
                         OUT_OF_PLOT_IND = "N",
                         STOP = as.character(NA))
  treelist_withchange <- assignChangeComponent(treelist = treelist,
                                               samples = samples)
  expect_equal(nrow(treelist_withchange), 0) # as no change of component smried for variable plots
  rm(samples, treelist_withchange)
  samples <- data.table(SITE_IDENTIFIER = 1234567,
                        VISIT_NUMBER = 1:3,
                        SAMP_TYP = "F")
  treelist_withchange <- assignChangeComponent(treelist = treelist,
                                               samples = samples)
  expect_equal(treelist_withchange$COMPONENT_CHANGE,
               c(NA, "S", "S")) # as no change of component smried for variable plots
  rm(treelist, treelist_withchange)


  treelist <- data.table(SITE_IDENTIFIER = 1234567,
                         VISIT_NUMBER = 1:3,
                         PLOT = "I",
                         DIAMETER = 7:9,
                         TREE_NUMBER = 1,
                         TREE_EXTANT_CODE = "L",
                         LVD_EDIT = as.character(NA),
                         MSMT_MISSING_EDIT = as.character(NA),
                         DIAMETER_EDIT = as.character(NA),
                         MEASUREMENT_ANOMALY_CODE = as.character(NA),
                         OUT_OF_PLOT_IND = "Y",
                         STOP = as.character(NA)) # all out of plot trees, should be removed
  treelist_withchange <- assignChangeComponent(treelist = treelist,
                                               samples = samples)
  expect_equal(nrow(treelist_withchange), 0) # as no change of component smried for variable plots
  rm(treelist, treelist_withchange)

  treelist <- data.table(SITE_IDENTIFIER = 1234567,
                         VISIT_NUMBER = 1:3,
                         PLOT = "I",
                         DIAMETER = 7:9,
                         TREE_NUMBER = 1,
                         TREE_EXTANT_CODE = "L",
                         LVD_EDIT = as.character(NA),
                         MSMT_MISSING_EDIT = as.character(NA),
                         DIAMETER_EDIT = as.character(NA),
                         MEASUREMENT_ANOMALY_CODE = "Z",
                         OUT_OF_PLOT_IND = "N",
                         STOP = as.character(NA)) # all out of plot trees, should be removed
  treelist_withchange <- assignChangeComponent(treelist = treelist,
                                               samples = samples)
  expect_equal(nrow(treelist_withchange), 0) # as no change of component smried for variable plots
  rm(treelist, treelist_withchange)


  # for ingrowth tree and survival tree
  treelist <- data.table(SITE_IDENTIFIER = 1234567,
                         VISIT_NUMBER = 2:3,
                         PLOT = "I",
                         DIAMETER = 8:9,
                         TREE_NUMBER = 1,
                         TREE_EXTANT_CODE = "L",
                         LVD_EDIT = as.character(NA),
                         MSMT_MISSING_EDIT = as.character(NA),
                         DIAMETER_EDIT = as.character(NA),
                         MEASUREMENT_ANOMALY_CODE = as.character(NA),
                         OUT_OF_PLOT_IND = "N",
                         STOP = as.character(NA)) # all out of plot trees, should be removed
  treelist_withchange <- assignChangeComponent(treelist = treelist,
                                               samples = samples)
  expect_equal(treelist_withchange$COMPONENT_CHANGE, c("I", "S")) # as no change of component smried for variable plots
  rm(treelist, treelist_withchange)

  # for ingrowth tree and mortality tree
  treelist <- data.table(SITE_IDENTIFIER = 1234567,
                         VISIT_NUMBER = 2:3,
                         PLOT = "I",
                         DIAMETER = 8:9,
                         TREE_NUMBER = 1,
                         TREE_EXTANT_CODE = c("L", "D"),
                         LVD_EDIT = as.character(NA),
                         MSMT_MISSING_EDIT = as.character(NA),
                         DIAMETER_EDIT = as.character(NA),
                         MEASUREMENT_ANOMALY_CODE = as.character(NA),
                         OUT_OF_PLOT_IND = "N",
                         STOP = as.character(NA)) # all out of plot trees, should be removed
  treelist_withchange <- assignChangeComponent(treelist = treelist,
                                               samples = samples)
  expect_equal(treelist_withchange$COMPONENT_CHANGE, c("I", "M")) # as no change of component smried for variable plots
  rm(treelist, treelist_withchange, samples)


  # for ingrowth tree and mortality tree and dead
  samples <- data.table(SITE_IDENTIFIER = 1234567,
                        VISIT_NUMBER = 1:5,
                        SAMP_TYP = "F")
  treelist <- data.table(SITE_IDENTIFIER = 1234567,
                         VISIT_NUMBER = 2:4,
                         PLOT = "I",
                         DIAMETER = c(8:9, 10),
                         TREE_NUMBER = 1,
                         TREE_EXTANT_CODE = c("L", "D", "D"),
                         LVD_EDIT = as.character(NA),
                         MSMT_MISSING_EDIT = as.character(NA),
                         DIAMETER_EDIT = as.character(NA),
                         MEASUREMENT_ANOMALY_CODE = as.character(NA),
                         OUT_OF_PLOT_IND = "N",
                         STOP = as.character(NA)) # all out of plot trees, should be removed
  treelist_withchange <- assignChangeComponent(treelist = treelist,
                                               samples = samples)
  expect_equal(treelist_withchange$COMPONENT_CHANGE, c("I", "M", "D")) # as no change of component smried for variable plots
  rm(treelist, treelist_withchange)

  # for ingrowth tree and harvest tree
  treelist <- data.table(SITE_IDENTIFIER = 1234567,
                         VISIT_NUMBER = 2:4,
                         PLOT = "I",
                         DIAMETER = c(8:9, 10),
                         TREE_NUMBER = 1,
                         TREE_EXTANT_CODE = c("L", "D", "D"),
                         LVD_EDIT = as.character(NA),
                         MSMT_MISSING_EDIT = as.character(NA),
                         DIAMETER_EDIT = as.character(NA),
                         MEASUREMENT_ANOMALY_CODE = c(NA, "H", "H"),
                         OUT_OF_PLOT_IND = "N",
                         STOP = as.character(NA)) # all out of plot trees, should be removed
  treelist_withchange <- assignChangeComponent(treelist = treelist,
                                               samples = samples)
  expect_equal(treelist_withchange$COMPONENT_CHANGE, c("I", "H", "H")) # as no change of component smried for variable plots
  rm(treelist, treelist_withchange)

  # for drop tree
  treelist <- data.table(SITE_IDENTIFIER = 1234567,
                         VISIT_NUMBER = 2:4,
                         PLOT = "I",
                         DIAMETER = c(8:9, 10),
                         TREE_NUMBER = 1,
                         TREE_EXTANT_CODE = c("L", "L", "L"),
                         LVD_EDIT = as.character(NA),
                         MSMT_MISSING_EDIT = as.character(NA),
                         DIAMETER_EDIT = as.character(NA),
                         MEASUREMENT_ANOMALY_CODE = c(NA, NA, "D"),
                         OUT_OF_PLOT_IND = "N",
                         STOP = as.character(NA)) # all out of plot trees, should be removed
  treelist_withchange <- assignChangeComponent(treelist = treelist,
                                               samples = samples)
  expect_equal(treelist_withchange$COMPONENT_CHANGE, c("I", "S", "DROP")) # as no change of component smried for variable plots
  rm(treelist, treelist_withchange, samples)

  # for multiple trees
  samples <- rbind(data.table(SITE_IDENTIFIER = 1234567,
                        VISIT_NUMBER = 1:5,
                        SAMP_TYP = "F"),
                   data.table(SITE_IDENTIFIER = 1234568,
                              VISIT_NUMBER = 1:7,
                              SAMP_TYP = "F"))

  treelist <- rbind(data.table(SITE_IDENTIFIER = 1234567,
                         VISIT_NUMBER = 1:3,
                         PLOT = "I",
                         DIAMETER = c(8:9, 10),
                         TREE_NUMBER = 1,
                         TREE_EXTANT_CODE = c("L", "L", "L"),
                         LVD_EDIT = as.character(NA),
                         MSMT_MISSING_EDIT = as.character(NA),
                         DIAMETER_EDIT = as.character(NA),
                         MEASUREMENT_ANOMALY_CODE = c(NA, NA, "D"),
                         OUT_OF_PLOT_IND = "N",
                         STOP = as.character(NA)), # all out of plot trees, should be removed

                    data.table(SITE_IDENTIFIER = 1234568,
                               VISIT_NUMBER = 2:6,
                               PLOT = "I",
                               DIAMETER = 20:24,
                               TREE_NUMBER = 2,
                               TREE_EXTANT_CODE = c("L", "L", "D", "D", "D"),
                               LVD_EDIT = as.character(NA),
                               MSMT_MISSING_EDIT = as.character(NA),
                               DIAMETER_EDIT = as.character(NA),
                               MEASUREMENT_ANOMALY_CODE = as.character(NA),
                               OUT_OF_PLOT_IND = "N",
                               STOP = as.character(NA)))
  treelist <- treelist[sample(1:nrow(treelist), size = nrow(treelist)),]

  treelist_withchange <- assignChangeComponent(treelist = treelist,
                                               samples = samples)
  setkey(treelist_withchange, NULL)
  expect_equal(treelist_withchange[,.(SITE_IDENTIFIER,
                                      VISIT_NUMBER,
                                      TREE_NO,
                                      LV_D,
                                      COMPONENT_CHANGE)],
               data.table(SITE_IDENTIFIER = c(rep(1234567, 3), rep(1234568, 5)),
                          VISIT_NUMBER = c(1:3, 2:6),
                          TREE_NO = c(rep(1, 3), rep(2, 5)),
                          LV_D = c(rep("L", 5), rep("D", 3)),
                          COMPONENT_CHANGE = c(NA, "S", "DROP", "I", "S", "M", "D", "D"))) # as no change of component smried for variable plots
})
