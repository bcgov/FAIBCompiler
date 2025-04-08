test_that("treemsmtEditing.R: tree measurement editing.", {
  library(data.table)
  library(testthat)
  site_visits <- data.table(SITE_IDENTIFIER = 1234567,
                            VISIT_NUMBER = 1:3,
                            VISIT_TYPE = "REP",
                            TYPE_CD = "PSP")
  # some good msmts
  tree <- data.table(SITE_IDENTIFIER = 1234567,
                     PLOT = "I",
                     TREE_NUMBER = 1,
                     VISIT_NUMBER = 1:3,
                     TREE_EXTANT_CODE = c("L", "L", "L"),
                     DIAMETER = c(7, 8, 9),
                     DIAMETER_MEASMT_HEIGHT = 1.3,
                     OUT_OF_PLOT_IND = "N",
                     TREE_CLASS_CODE = c(1, 1, 1),
                     LENGTH = NA,
                     BROKEN_TOP_IND = "N",
                     CROWN_CLASS_CODE = "D",
                     TREE_SPECIES_CODE = c("AC", "AC", "AC"),
                     TREE_MEASUREMENT_COMMENT = as.character(NA),
                     TREE_SUPPRESSION_CODE = as.character(NA),
                     SUITABLE_FOR_HEIGHT_IND = "Y",
                     VETERAN_IND = "N",
                     RESIDUAL_IND = "N",
                     TREE_STANCE_CODE = "S",
                     SUITABLE_FOR_SITE_INDEX_IND = "Y",
                     SUITABLE_FOR_AGE_IND = "Y",
                     AGE_REPRESENTATIVE_IND = "Y",
                     MEASUREMENT_ANOMALY_CODE = as.character(NA),
                     STEM_MAP_BEARING = 160,
                     STEM_MAP_DISTANCE = 5)
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$LVD_EDIT, rep(as.character(NA), 3))
  expect_equal(tree_editted$DIAMETER_EDIT, rep(as.character(NA), 3))
  expect_equal(tree_editted$MSMT_MISSING_EDIT, rep(as.character(NA), 3))
  expect_equal(tree_editted$SP_EDIT, rep(as.character(NA), 3))
  expect_equal(tree_editted$OUT_OF_PLOT_EDIT, rep(as.character(NA), 3))
  rm(tree, tree_editted)

  tree <- data.table(SITE_IDENTIFIER = 1234567,
                     PLOT = "I",
                     TREE_NUMBER = 1,
                     VISIT_NUMBER = 1:2,
                     TREE_EXTANT_CODE = c("L", "D"),
                     DIAMETER = c(7, 8),
                     DIAMETER_MEASMT_HEIGHT = 1.3,
                     OUT_OF_PLOT_IND = "N",
                     TREE_CLASS_CODE = c(1, 4),
                     LENGTH = NA,
                     BROKEN_TOP_IND = "N",
                     CROWN_CLASS_CODE = "D",
                     TREE_SPECIES_CODE = c("AC", "AC"),
                     TREE_MEASUREMENT_COMMENT = as.character(NA),
                     TREE_SUPPRESSION_CODE = as.character(NA),
                     SUITABLE_FOR_HEIGHT_IND = "Y",
                     VETERAN_IND = "N",
                     RESIDUAL_IND = "N",
                     TREE_STANCE_CODE = "S",
                     SUITABLE_FOR_SITE_INDEX_IND = "Y",
                     SUITABLE_FOR_AGE_IND = "Y",
                     AGE_REPRESENTATIVE_IND = "Y",
                     MEASUREMENT_ANOMALY_CODE = as.character(NA),
                     STEM_MAP_BEARING = 160,
                     STEM_MAP_DISTANCE = 5,
                     CMI_WALKTHROUGH_CODE = as.character(NA))
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$LVD_EDIT, rep(as.character(NA), 2))
  expect_equal(tree_editted$DIAMETER_EDIT, c(rep(as.character(NA), 2)))
  expect_equal(tree_editted$MSMT_MISSING_EDIT,
               c(rep(as.character(NA), 1), "Stop msmt as Dead Less than Ten cm"))
  expect_equal(tree_editted$SP_EDIT, rep(as.character(NA), 2))
  expect_equal(tree_editted$STOP, c(NA, "S_DLT"))
  rm(tree_editted)
  ## for nonPSP
  tree_editted <- treemsmtEditing(compilationType = "nonPSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$LVD_EDIT, rep(as.character(NA), 2))
  expect_equal(tree_editted$DIAMETER_EDIT, c(rep(as.character(NA), 2)))
  expect_equal(tree_editted$MSMT_MISSING_EDIT,
               c(rep(as.character(NA), 1), "Missing at tail. Not found assumed, STOP S_N added"))
  expect_equal(tree_editted$SP_EDIT, rep(as.character(NA), 2))
  expect_equal(tree_editted$STOP, c(NA, "S_N"))
  rm(tree, tree_editted)

  tree <- data.table(SITE_IDENTIFIER = 1234567,
                     PLOT = "I",
                     TREE_NUMBER = 1,
                     VISIT_NUMBER = 1:2,
                     TREE_EXTANT_CODE = c("L", "D"),
                     DIAMETER = c(11, 12),
                     DIAMETER_MEASMT_HEIGHT = 1.3,
                     OUT_OF_PLOT_IND = "N",
                     TREE_CLASS_CODE = c(1, 4),
                     LENGTH = NA,
                     BROKEN_TOP_IND = "N",
                     CROWN_CLASS_CODE = "D",
                     TREE_SPECIES_CODE = c("AC", "AC"),
                     TREE_MEASUREMENT_COMMENT = as.character(NA),
                     TREE_SUPPRESSION_CODE = as.character(NA),
                     SUITABLE_FOR_HEIGHT_IND = "Y",
                     VETERAN_IND = "N",
                     RESIDUAL_IND = "N",
                     TREE_STANCE_CODE = "S",
                     SUITABLE_FOR_SITE_INDEX_IND = "Y",
                     SUITABLE_FOR_AGE_IND = "Y",
                     AGE_REPRESENTATIVE_IND = "Y",
                     MEASUREMENT_ANOMALY_CODE = as.character(NA),
                     STEM_MAP_BEARING = 160,
                     STEM_MAP_DISTANCE = 5,
                     CMI_WALKTHROUGH_CODE = as.character(NA))
  tree_editted_PSP <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  tree_editted_nonPSP <- treemsmtEditing(compilationType = "nonPSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted_PSP$LVD_EDIT, rep(as.character(NA), 2))
  expect_equal(tree_editted_nonPSP$LVD_EDIT, rep(as.character(NA), 2))
  expect_equal(tree_editted_PSP$DIAMETER_EDIT, c(rep(as.character(NA), 2)))
  expect_equal(tree_editted_nonPSP$DIAMETER_EDIT, c(rep(as.character(NA), 2)))
  expect_equal(tree_editted_PSP$MSMT_MISSING_EDIT,
               c(rep(as.character(NA), 1), "Missing at tail. Not found assumed, STOP S_N added"))
  expect_equal(tree_editted_nonPSP$MSMT_MISSING_EDIT,
               c(rep(as.character(NA), 1), "Missing at tail. Not found assumed, STOP S_N added"))
  expect_equal(tree_editted_PSP$SP_EDIT, rep(as.character(NA), 2))
  expect_equal(tree_editted_nonPSP$SP_EDIT, rep(as.character(NA), 2))
  expect_equal(tree_editted_PSP$STOP, c(NA, "S_N"))
  expect_equal(tree_editted_nonPSP$STOP, c(NA, "S_N"))
  rm(tree, tree_editted_PSP, tree_editted_nonPSP)

  tree <- data.table(SITE_IDENTIFIER = 1234567,
                     PLOT = "I",
                     TREE_NUMBER = 1,
                     VISIT_NUMBER = 2:3,
                     TREE_EXTANT_CODE = c("L", "D"),
                     DIAMETER = c(7, 8),
                     DIAMETER_MEASMT_HEIGHT = 1.3,
                     OUT_OF_PLOT_IND = "N",
                     TREE_CLASS_CODE = c(1, 4),
                     LENGTH = NA,
                     BROKEN_TOP_IND = "N",
                     CROWN_CLASS_CODE = "D",
                     TREE_SPECIES_CODE = c("AC", "AC"),
                     TREE_MEASUREMENT_COMMENT = as.character(NA),
                     TREE_SUPPRESSION_CODE = as.character(NA),
                     SUITABLE_FOR_HEIGHT_IND = "Y",
                     VETERAN_IND = "N",
                     RESIDUAL_IND = "N",
                     TREE_STANCE_CODE = "S",
                     SUITABLE_FOR_SITE_INDEX_IND = "Y",
                     SUITABLE_FOR_AGE_IND = "Y",
                     AGE_REPRESENTATIVE_IND = "Y",
                     MEASUREMENT_ANOMALY_CODE = as.character(NA),
                     STEM_MAP_BEARING = 160,
                     STEM_MAP_DISTANCE = 5)
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$LVD_EDIT, rep(as.character(NA), 2))
  expect_equal(tree_editted$DIAMETER_EDIT, rep(as.character(NA), 2))
  expect_equal(tree_editted$MSMT_MISSING_EDIT, rep(as.character(NA), 2))
  expect_equal(tree_editted$SP_EDIT, rep(as.character(NA), 2))
  rm(tree, tree_editted)


  tree <- data.table(SITE_IDENTIFIER = 1234567,
                     PLOT = "I",
                     TREE_NUMBER = 1,
                     VISIT_NUMBER = 1:3,
                     TREE_EXTANT_CODE = c("L", "L", NA),
                     DIAMETER = c(7, 8, NA),
                     DIAMETER_MEASMT_HEIGHT = 1.3,
                     OUT_OF_PLOT_IND = "N",
                     TREE_CLASS_CODE = c(1, 1, NA),
                     LENGTH = NA,
                     BROKEN_TOP_IND = "N",
                     CROWN_CLASS_CODE = "D",
                     TREE_SPECIES_CODE = c("B", "D", "AC"),
                     TREE_MEASUREMENT_COMMENT = as.character(NA),
                     TREE_SUPPRESSION_CODE = as.character(NA),
                     SUITABLE_FOR_HEIGHT_IND = "Y",
                     VETERAN_IND = "N",
                     RESIDUAL_IND = "N",
                     TREE_STANCE_CODE = "S",
                     SUITABLE_FOR_SITE_INDEX_IND = "Y",
                     SUITABLE_FOR_AGE_IND = "Y",
                     AGE_REPRESENTATIVE_IND = "Y",
                     MEASUREMENT_ANOMALY_CODE = as.character(NA),
                     STEM_MAP_BEARING = 160,
                     STEM_MAP_DISTANCE = 5)
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$DIAMETER, c(7, 8, 8))
  expect_equal(tree_editted$TREE_EXTANT_CODE, c("L", "L", "D"))
  expect_equal(tree_editted$LVD_EDIT[3],
               "Missing at tail. added D")
  expect_equal(tree_editted$DIAMETER_EDIT[3],
               "Diameter assinged based on previous msmt")
  expect_equal(tree_editted$TREE_SPECIES_CODE, c("AC", "AC", "AC"))
  expect_equal(tree_editted$SP_EDIT, c(rep("Species changed based on last msmt", 2), NA) )
  rm(tree, tree_editted)

  tree <- data.table(SITE_IDENTIFIER = 1234567,
                     PLOT = "I",
                     TREE_NUMBER = 2,
                     VISIT_NUMBER = 1:3,
                     TREE_EXTANT_CODE = c("L", "D", "L"),
                     DIAMETER = c(7, NA, 9),
                     DIAMETER_MEASMT_HEIGHT = 1.3,
                     OUT_OF_PLOT_IND = "N",
                     TREE_CLASS_CODE = c(1, NA, 1),
                     LENGTH = NA,
                     BROKEN_TOP_IND = "N",
                     CROWN_CLASS_CODE = "D",
                     TREE_SPECIES_CODE = "AC",
                     TREE_MEASUREMENT_COMMENT = as.character(NA),
                     TREE_SUPPRESSION_CODE = as.character(NA),
                     SUITABLE_FOR_HEIGHT_IND = "Y",
                     VETERAN_IND = "N",
                     RESIDUAL_IND = "N",
                     TREE_STANCE_CODE = "S",
                     SUITABLE_FOR_SITE_INDEX_IND = "Y",
                     SUITABLE_FOR_AGE_IND = "Y",
                     AGE_REPRESENTATIVE_IND = "Y",
                     MEASUREMENT_ANOMALY_CODE = as.character(NA),
                     STEM_MAP_BEARING = 160,
                     STEM_MAP_DISTANCE = 5)
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$DIAMETER, c(7, 8, 9))
  expect_equal(tree_editted$TREE_EXTANT_CODE, c("L", "L", "L"))
  expect_equal(tree_editted$LVD_EDIT[2],
               "Corrected to L as found alive later")
  expect_equal(tree_editted$DIAMETER_EDIT[2],
               "Missing. assigned based on mean of prev and next diameters")
  rm(tree, tree_editted)

  tree <- data.table(SITE_IDENTIFIER = 1234567,
                     PLOT = "I",
                     TREE_NUMBER = 2,
                     VISIT_NUMBER = 1:3,
                     TREE_EXTANT_CODE = c("L", NA, "L"),
                     DIAMETER = c(7, NA, 9),
                     DIAMETER_MEASMT_HEIGHT = 1.3,
                     OUT_OF_PLOT_IND = "N",
                     TREE_CLASS_CODE = c(1, NA, 1),
                     LENGTH = NA,
                     BROKEN_TOP_IND = "N",
                     CROWN_CLASS_CODE = "D",
                     TREE_SPECIES_CODE = "AC",
                     TREE_MEASUREMENT_COMMENT = as.character(NA),
                     TREE_SUPPRESSION_CODE = as.character(NA),
                     SUITABLE_FOR_HEIGHT_IND = "Y",
                     VETERAN_IND = "N",
                     RESIDUAL_IND = "N",
                     TREE_STANCE_CODE = "S",
                     SUITABLE_FOR_SITE_INDEX_IND = "Y",
                     SUITABLE_FOR_AGE_IND = "Y",
                     AGE_REPRESENTATIVE_IND = "Y",
                     MEASUREMENT_ANOMALY_CODE = as.character(NA),
                     STEM_MAP_BEARING = 160,
                     STEM_MAP_DISTANCE = 5)
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$DIAMETER, c(7, 8, 9))
  expect_equal(tree_editted$TREE_EXTANT_CODE, c("L", "L", "L"))
  expect_equal(tree_editted$LVD_EDIT[2],
               "Missing. added L as found alive later")
  expect_equal(tree_editted$DIAMETER_EDIT[2],
               "Missing. assigned based on mean of prev and next diameters")
  rm(tree, tree_editted)

  # test missing
  tree <- data.table(SITE_IDENTIFIER = 1234567,
                     PLOT = "I",
                     TREE_NUMBER = 2,
                     VISIT_NUMBER = c(1, 3),
                     TREE_EXTANT_CODE = c("L", "L"),
                     DIAMETER = c(7, 9),
                     DIAMETER_MEASMT_HEIGHT = 1.3,
                     OUT_OF_PLOT_IND = "N",
                     TREE_CLASS_CODE = c(1, 1),
                     LENGTH = NA,
                     BROKEN_TOP_IND = "N",
                     CROWN_CLASS_CODE = "D",
                     TREE_SPECIES_CODE = "AC",
                     TREE_MEASUREMENT_COMMENT = as.character(NA),
                     TREE_SUPPRESSION_CODE = as.character(NA),
                     SUITABLE_FOR_HEIGHT_IND = "Y",
                     VETERAN_IND = "N",
                     RESIDUAL_IND = "N",
                     TREE_STANCE_CODE = "S",
                     SUITABLE_FOR_SITE_INDEX_IND = "Y",
                     SUITABLE_FOR_AGE_IND = "Y",
                     AGE_REPRESENTATIVE_IND = "Y",
                     MEASUREMENT_ANOMALY_CODE = as.character(NA),
                     STEM_MAP_BEARING = 160,
                     STEM_MAP_DISTANCE = 5)
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$DIAMETER, c(7, 8, 9))
  expect_equal(tree_editted$TREE_EXTANT_CODE, c("L", "L", "L"))
  expect_equal(tree_editted$LVD_EDIT, rep(as.character(NA), 3))
  expect_equal(tree_editted$DIAMETER_EDIT[2],
               "Missing. assigned based on mean of prev and next diameters")
  expect_equal(tree_editted$MSMT_MISSING_EDIT[2],
               "Missing in between. added")
  rm(tree, tree_editted)

  tree <- data.table(SITE_IDENTIFIER = 1234567,
                     PLOT = "I",
                     TREE_NUMBER = 2,
                     VISIT_NUMBER = c(1, 3),
                     TREE_EXTANT_CODE = c("L", "D"),
                     DIAMETER = c(7, NA),
                     DIAMETER_MEASMT_HEIGHT = 1.3,
                     OUT_OF_PLOT_IND = "N",
                     TREE_CLASS_CODE = c(1, 4),
                     LENGTH = NA,
                     BROKEN_TOP_IND = "N",
                     CROWN_CLASS_CODE = "D",
                     TREE_SPECIES_CODE = "AC",
                     TREE_MEASUREMENT_COMMENT = as.character(NA),
                     TREE_SUPPRESSION_CODE = as.character(NA),
                     SUITABLE_FOR_HEIGHT_IND = "Y",
                     VETERAN_IND = "N",
                     RESIDUAL_IND = "N",
                     TREE_STANCE_CODE = "S",
                     SUITABLE_FOR_SITE_INDEX_IND = "Y",
                     SUITABLE_FOR_AGE_IND = "Y",
                     AGE_REPRESENTATIVE_IND = "Y",
                     MEASUREMENT_ANOMALY_CODE = as.character(NA),
                     STEM_MAP_BEARING = 160,
                     STEM_MAP_DISTANCE = 5)
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$DIAMETER, c(7, 7, 7))
  expect_equal(tree_editted$TREE_EXTANT_CODE, c("L", "D", "D"))
  expect_equal(tree_editted$LVD_EDIT, rep(as.character(NA), 3))
  expect_equal(tree_editted$DIAMETER_EDIT[2:3],
               c("Missing. assigned based on mean of prev and next diameters",
               "Diameter assinged based on previous msmt"))
  expect_equal(tree_editted$MSMT_MISSING_EDIT[2],
               "Missing in between. added")
  rm(tree, tree_editted)



  tree <- data.table(SITE_IDENTIFIER = 1234567,
                     PLOT = "I",
                     TREE_NUMBER = 2,
                     VISIT_NUMBER = c(1, 2, 3),
                     TREE_EXTANT_CODE = c("L", "L", NA),
                     DIAMETER = c(7, 8, NA),
                     DIAMETER_MEASMT_HEIGHT = 1.3,
                     OUT_OF_PLOT_IND = "N",
                     TREE_CLASS_CODE = c(1, 2, NA),
                     LENGTH = NA,
                     BROKEN_TOP_IND = "N",
                     CROWN_CLASS_CODE = "D",
                     TREE_SPECIES_CODE = "AC",
                     TREE_MEASUREMENT_COMMENT = as.character(NA),
                     TREE_SUPPRESSION_CODE = as.character(NA),
                     SUITABLE_FOR_HEIGHT_IND = "Y",
                     VETERAN_IND = "N",
                     RESIDUAL_IND = "N",
                     TREE_STANCE_CODE = "S",
                     SUITABLE_FOR_SITE_INDEX_IND = "Y",
                     SUITABLE_FOR_AGE_IND = "Y",
                     AGE_REPRESENTATIVE_IND = "Y",
                     MEASUREMENT_ANOMALY_CODE = as.character(NA),
                     STEM_MAP_BEARING = 160,
                     STEM_MAP_DISTANCE = 5)
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$DIAMETER, c(7, 8, 8))
  expect_equal(tree_editted$TREE_EXTANT_CODE, c("L", "L", "D"))
  expect_equal(tree_editted$LVD_EDIT, c(rep(as.character(NA), 2),
                                        "Missing at tail. added D"))
  expect_equal(tree_editted$DIAMETER_EDIT[3],
               "Diameter assinged based on previous msmt")
  rm(tree, tree_editted)

  tree <- data.table(SITE_IDENTIFIER = 1234567,
                     PLOT = "I",
                     TREE_NUMBER = 2,
                     VISIT_NUMBER = c(1, 2),
                     TREE_EXTANT_CODE = c("L", "L"),
                     DIAMETER = c(7, 8),
                     DIAMETER_MEASMT_HEIGHT = 1.3,
                     OUT_OF_PLOT_IND = "N",
                     TREE_CLASS_CODE = c(1, 2),
                     LENGTH = NA,
                     BROKEN_TOP_IND = "N",
                     CROWN_CLASS_CODE = "D",
                     TREE_SPECIES_CODE = "AC",
                     TREE_MEASUREMENT_COMMENT = as.character(NA),
                     TREE_SUPPRESSION_CODE = as.character(NA),
                     SUITABLE_FOR_HEIGHT_IND = "Y",
                     VETERAN_IND = "N",
                     RESIDUAL_IND = "N",
                     TREE_STANCE_CODE = "S",
                     SUITABLE_FOR_SITE_INDEX_IND = "Y",
                     SUITABLE_FOR_AGE_IND = "Y",
                     AGE_REPRESENTATIVE_IND = "Y",
                     MEASUREMENT_ANOMALY_CODE = as.character(NA),
                     STEM_MAP_BEARING = 160,
                     STEM_MAP_DISTANCE = 5)
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$DIAMETER, c(7, 8))
  expect_equal(tree_editted$TREE_EXTANT_CODE, c("L", "L"))
  expect_equal(tree_editted$MSMT_MISSING_EDIT,
               c(NA, "Missing at tail. Not found assumed, STOP S_N added"))
  rm(tree, tree_editted)

  tree <- data.table(SITE_IDENTIFIER = 1234567,
                     PLOT = "I",
                     TREE_NUMBER = 2,
                     VISIT_NUMBER = c(1, 2, 3),
                     TREE_EXTANT_CODE = c("L", "L", "L"),
                     DIAMETER = c(7, 8, 9),
                     DIAMETER_MEASMT_HEIGHT = 1.3,
                     OUT_OF_PLOT_IND = "N",
                     TREE_CLASS_CODE = c(1, 2, 2),
                     LENGTH = NA,
                     BROKEN_TOP_IND = c("N", "Y", "N"),
                     CROWN_CLASS_CODE = "D",
                     TREE_SPECIES_CODE = "AC",
                     TREE_MEASUREMENT_COMMENT = as.character(NA),
                     TREE_SUPPRESSION_CODE = as.character(NA),
                     SUITABLE_FOR_HEIGHT_IND = "Y",
                     VETERAN_IND = "N",
                     RESIDUAL_IND = "N",
                     TREE_STANCE_CODE = "S",
                     SUITABLE_FOR_SITE_INDEX_IND = "Y",
                     SUITABLE_FOR_AGE_IND = "Y",
                     AGE_REPRESENTATIVE_IND = "Y",
                     MEASUREMENT_ANOMALY_CODE = as.character(NA),
                     STEM_MAP_BEARING = 160,
                     STEM_MAP_DISTANCE = 5)
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$BROKEN_TOP_IND, c("N", "Y", "Y"))
  expect_equal(tree_editted$BTOP_EDIT[3],
               "Change to Y based on previous msmt")
  rm(tree, tree_editted)

  site_visits <- data.table(SITE_IDENTIFIER = 1234567,
                            VISIT_NUMBER = 1:4,
                            VISIT_TYPE = "REP",
                            TYPE_CD = "PSP")
  ## check stop due to Dead fallen
  tree <- data.table(SITE_IDENTIFIER = 1234567,
                     PLOT = "I",
                     TREE_NUMBER = 1,
                     VISIT_NUMBER = 1:4,
                     TREE_EXTANT_CODE = c("L", "D", "D", "D"),
                     DIAMETER = c(7, 8, 9, 10),
                     DIAMETER_MEASMT_HEIGHT = 1.3,
                     OUT_OF_PLOT_IND = "N",
                     TREE_CLASS_CODE = c(1, 1, 1, 1),
                     LENGTH = NA,
                     BROKEN_TOP_IND = "N",
                     CROWN_CLASS_CODE = "D",
                     TREE_SPECIES_CODE = c("AC", "AC", "AC", "AC"),
                     TREE_MEASUREMENT_COMMENT = as.character(NA),
                     TREE_SUPPRESSION_CODE = as.character(NA),
                     SUITABLE_FOR_HEIGHT_IND = "Y",
                     VETERAN_IND = "N",
                     RESIDUAL_IND = "N",
                     TREE_STANCE_CODE = c("S", "S", "F", "F"),
                     SUITABLE_FOR_SITE_INDEX_IND = "Y",
                     SUITABLE_FOR_AGE_IND = "Y",
                     AGE_REPRESENTATIVE_IND = "Y",
                     MEASUREMENT_ANOMALY_CODE = as.character(NA),
                     STEM_MAP_BEARING = 160,
                     STEM_MAP_DISTANCE = 5)
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$STOP, c(NA, NA, "S_DF"))
  rm(tree, tree_editted)

  ## harvest
  tree <- data.table(SITE_IDENTIFIER = 1234567,
                     PLOT = "I",
                     TREE_NUMBER = 1,
                     VISIT_NUMBER = 1:4,
                     TREE_EXTANT_CODE = c("L", "L", "D", "D"),
                     DIAMETER = c(7, 8, 9, 10),
                     DIAMETER_MEASMT_HEIGHT = 1.3,
                     OUT_OF_PLOT_IND = "N",
                     TREE_CLASS_CODE = c(1, 1, 1, 1),
                     LENGTH = NA,
                     BROKEN_TOP_IND = "N",
                     CROWN_CLASS_CODE = "D",
                     TREE_SPECIES_CODE = c("AC", "AC", "AC", "AC"),
                     TREE_MEASUREMENT_COMMENT = as.character(NA),
                     TREE_SUPPRESSION_CODE = as.character(NA),
                     SUITABLE_FOR_HEIGHT_IND = "Y",
                     VETERAN_IND = "N",
                     RESIDUAL_IND = "N",
                     TREE_STANCE_CODE = c("S", "S", NA, NA),
                     SUITABLE_FOR_SITE_INDEX_IND = "Y",
                     SUITABLE_FOR_AGE_IND = "Y",
                     AGE_REPRESENTATIVE_IND = "Y",
                     MEASUREMENT_ANOMALY_CODE = c(NA, NA, "H", "H"),
                     STEM_MAP_BEARING = 160,
                     STEM_MAP_DISTANCE = 5)
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$STOP, c(NA, "S_H"))
  rm(tree, tree_editted)

  tree <- data.table(SITE_IDENTIFIER = 1234567,
                     PLOT = "I",
                     TREE_NUMBER = 1,
                     VISIT_NUMBER = 1:4,
                     TREE_EXTANT_CODE = c("L", "L", "D", "L"),
                     DIAMETER = c(7, 8, 9, 10),
                     DIAMETER_MEASMT_HEIGHT = 1.3,
                     OUT_OF_PLOT_IND = "N",
                     TREE_CLASS_CODE = c(1, 1, 1, 1),
                     LENGTH = NA,
                     BROKEN_TOP_IND = "N",
                     CROWN_CLASS_CODE = "D",
                     TREE_SPECIES_CODE = c("AC", "AC", "AC", "AC"),
                     TREE_MEASUREMENT_COMMENT = as.character(NA),
                     TREE_SUPPRESSION_CODE = as.character(NA),
                     SUITABLE_FOR_HEIGHT_IND = "Y",
                     VETERAN_IND = "N",
                     RESIDUAL_IND = "N",
                     TREE_STANCE_CODE = c("S", "S", NA, NA),
                     SUITABLE_FOR_SITE_INDEX_IND = "Y",
                     SUITABLE_FOR_AGE_IND = "Y",
                     AGE_REPRESENTATIVE_IND = "Y",
                     MEASUREMENT_ANOMALY_CODE = c(NA, NA, "H", NA),
                     STEM_MAP_BEARING = 160,
                     STEM_MAP_DISTANCE = 5)
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$STOP, c(NA, "S_H"))
  rm(tree, tree_editted)

  ## not found
  tree <- data.table(SITE_IDENTIFIER = 1234567,
                     PLOT = "I",
                     TREE_NUMBER = 1,
                     VISIT_NUMBER = 1:4,
                     TREE_EXTANT_CODE = c("L", "L", "D", "D"),
                     DIAMETER = c(7, 8, 9, 10),
                     DIAMETER_MEASMT_HEIGHT = 1.3,
                     OUT_OF_PLOT_IND = "N",
                     TREE_CLASS_CODE = c(1, 1, 1, 1),
                     LENGTH = NA,
                     BROKEN_TOP_IND = "N",
                     CROWN_CLASS_CODE = "D",
                     TREE_SPECIES_CODE = c("AC", "AC", "AC", "AC"),
                     TREE_MEASUREMENT_COMMENT = as.character(NA),
                     TREE_SUPPRESSION_CODE = as.character(NA),
                     SUITABLE_FOR_HEIGHT_IND = "Y",
                     VETERAN_IND = "N",
                     RESIDUAL_IND = "N",
                     TREE_STANCE_CODE = c("S", "S", NA, NA),
                     SUITABLE_FOR_SITE_INDEX_IND = "Y",
                     SUITABLE_FOR_AGE_IND = "Y",
                     AGE_REPRESENTATIVE_IND = "Y",
                     MEASUREMENT_ANOMALY_CODE = c(NA, NA, "N", "N"),
                     STEM_MAP_BEARING = 160,
                     STEM_MAP_DISTANCE = 5)
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$STOP, c(NA, "S_N"))
  rm(tree, tree_editted)

  ## not found and come back and not found again
  tree <- data.table(SITE_IDENTIFIER = 1234567,
                     PLOT = "I",
                     TREE_NUMBER = 1,
                     VISIT_NUMBER = 1:4,
                     TREE_EXTANT_CODE = c("L", "D", "L", "D"),
                     DIAMETER = c(7, 8, 9, 10),
                     DIAMETER_MEASMT_HEIGHT = 1.3,
                     OUT_OF_PLOT_IND = "N",
                     TREE_CLASS_CODE = c(1, 1, 1, 1),
                     LENGTH = NA,
                     BROKEN_TOP_IND = "N",
                     CROWN_CLASS_CODE = "D",
                     TREE_SPECIES_CODE = c("AC", "AC", "AC", "AC"),
                     TREE_MEASUREMENT_COMMENT = as.character(NA),
                     TREE_SUPPRESSION_CODE = as.character(NA),
                     SUITABLE_FOR_HEIGHT_IND = "Y",
                     VETERAN_IND = "N",
                     RESIDUAL_IND = "N",
                     TREE_STANCE_CODE = c("S", NA, "S", NA),
                     SUITABLE_FOR_SITE_INDEX_IND = "Y",
                     SUITABLE_FOR_AGE_IND = "Y",
                     AGE_REPRESENTATIVE_IND = "Y",
                     MEASUREMENT_ANOMALY_CODE = c(NA, "N", NA, "N"),
                     STEM_MAP_BEARING = 160,
                     STEM_MAP_DISTANCE = 5)
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$STOP, c(NA, NA, "S_N"))
  rm(tree, tree_editted)

  ## out of plot indicator for Z trees
  tree <- data.table(SITE_IDENTIFIER = 1234567,
                     PLOT = "I",
                     TREE_NUMBER = 1,
                     VISIT_NUMBER = 1:4,
                     TREE_EXTANT_CODE = c("L", "L", "L", "L"),
                     DIAMETER = c(7, 8, 9, 10),
                     DIAMETER_MEASMT_HEIGHT = 1.3,
                     OUT_OF_PLOT_IND = c("Y", "N", "N", "Y"),
                     TREE_CLASS_CODE = 1,
                     LENGTH = NA,
                     BROKEN_TOP_IND = "N",
                     CROWN_CLASS_CODE = "D",
                     TREE_SPECIES_CODE = c("AC", "AC", "AC", "AC"),
                     TREE_MEASUREMENT_COMMENT = as.character(NA),
                     TREE_SUPPRESSION_CODE = as.character(NA),
                     SUITABLE_FOR_HEIGHT_IND = "Y",
                     VETERAN_IND = "N",
                     RESIDUAL_IND = "N",
                     TREE_STANCE_CODE = "S",
                     SUITABLE_FOR_SITE_INDEX_IND = "Y",
                     SUITABLE_FOR_AGE_IND = "Y",
                     AGE_REPRESENTATIVE_IND = "Y",
                     MEASUREMENT_ANOMALY_CODE = c(NA, NA, NA, "Z"),
                     STEM_MAP_BEARING = 160,
                     STEM_MAP_DISTANCE = 5)

  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$OUT_OF_PLOT_IND, c("Y", "Y", "Y", "Y"))
  expect_equal(tree_editted$OUT_OF_PLOT_EDIT,
               c(NA, "Corrected to Y due to Z tree",
                 "Corrected to Y due to Z tree", NA))
  rm(tree, tree_editted)

  ## not Z tree, but if see "Y" at a measurement, should be "Y" for previous msmt
  ## similar to a dropped measurement
  tree <- data.table(SITE_IDENTIFIER = 1234567,
                     PLOT = "I",
                     TREE_NUMBER = 1,
                     VISIT_NUMBER = 1:4,
                     TREE_EXTANT_CODE = c("L", "L", "L", "L"),
                     DIAMETER = c(7, 8, 9, 10),
                     DIAMETER_MEASMT_HEIGHT = 1.3,
                     OUT_OF_PLOT_IND = c("N", "N", "Y", "N"),
                     TREE_CLASS_CODE = 1,
                     LENGTH = NA,
                     BROKEN_TOP_IND = "N",
                     CROWN_CLASS_CODE = "D",
                     TREE_SPECIES_CODE = c("AC", "AC", "AC", "AC"),
                     TREE_MEASUREMENT_COMMENT = as.character(NA),
                     TREE_SUPPRESSION_CODE = as.character(NA),
                     SUITABLE_FOR_HEIGHT_IND = "Y",
                     VETERAN_IND = "N",
                     RESIDUAL_IND = "N",
                     TREE_STANCE_CODE = "S",
                     SUITABLE_FOR_SITE_INDEX_IND = "Y",
                     SUITABLE_FOR_AGE_IND = "Y",
                     AGE_REPRESENTATIVE_IND = "Y",
                     MEASUREMENT_ANOMALY_CODE = c(NA, NA, NA, NA),
                     STEM_MAP_BEARING = 160,
                     STEM_MAP_DISTANCE = 5)

  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$OUT_OF_PLOT_IND, c("Y", "Y", "Y", "N"))
  expect_equal(tree_editted$OUT_OF_PLOT_EDIT,
               c("Corrected to Y due out tree at a later measurement",
                 "Corrected to Y due out tree at a later measurement",
                 NA, NA))
  rm(tree, tree_editted)
  ## if saw a dropped msmt, the previous msmts should be marked as "Y"
  tree <- data.table(SITE_IDENTIFIER = 1234567,
                     PLOT = "I",
                     TREE_NUMBER = 1,
                     VISIT_NUMBER = 1:4,
                     TREE_EXTANT_CODE = c("L", "L", "L", "L"),
                     DIAMETER = c(7, 8, 9, 10),
                     DIAMETER_MEASMT_HEIGHT = 1.3,
                     OUT_OF_PLOT_IND = c("N", "N", "N", "N"),
                     TREE_CLASS_CODE = 1,
                     LENGTH = NA,
                     BROKEN_TOP_IND = "N",
                     CROWN_CLASS_CODE = "D",
                     TREE_SPECIES_CODE = c("AC", "AC", "AC", "AC"),
                     TREE_MEASUREMENT_COMMENT = as.character(NA),
                     TREE_SUPPRESSION_CODE = as.character(NA),
                     SUITABLE_FOR_HEIGHT_IND = "Y",
                     VETERAN_IND = "N",
                     RESIDUAL_IND = "N",
                     TREE_STANCE_CODE = "S",
                     SUITABLE_FOR_SITE_INDEX_IND = "Y",
                     SUITABLE_FOR_AGE_IND = "Y",
                     AGE_REPRESENTATIVE_IND = "Y",
                     MEASUREMENT_ANOMALY_CODE = c(NA, NA, "D", NA),
                     STEM_MAP_BEARING = 160,
                     STEM_MAP_DISTANCE = 5)

  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$STOP, c(NA, NA, "S_D", "RESUME"))
  expect_equal(tree_editted$OUT_OF_PLOT_IND, c("Y", "Y", "Y", "N"))
  expect_equal(tree_editted$OUT_OF_PLOT_EDIT,
               c("Corrected to Y due to dropped",
                 "Corrected to Y due to dropped",
                 "Corrected to Y due to dropped",
                 NA))
  rm(tree, tree_editted)
})
