test_that("treemsmtEditing.R: tree measurement editing.", {
  library(data.table)
  library(testthat)
  site_visits <- data.table(SITE_IDENTIFIER = 1234567,
                            VISIT_NUMBER = 1:3)
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
                     AGE_REPRESENTATIVE_IND = "Y")
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$LVD_EDIT, rep(as.character(NA), 3))
  expect_equal(tree_editted$DIAMETER_EDIT, rep(as.character(NA), 3))
  expect_equal(tree_editted$MSMT_MISSING_EDIT, rep(as.character(NA), 3))
  expect_equal(tree_editted$SP_EDIT, rep(as.character(NA), 3))
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
                     AGE_REPRESENTATIVE_IND = "Y")
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
                     AGE_REPRESENTATIVE_IND = "Y")
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
                     AGE_REPRESENTATIVE_IND = "Y")
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$DIAMETER, c(7, 8, 8))
  expect_equal(tree_editted$TREE_EXTANT_CODE, c("L", "L", "D"))
  expect_equal(tree_editted$LVD_EDIT[3],
               "Missing at last msmt, added dead")
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
                     AGE_REPRESENTATIVE_IND = "Y")
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$DIAMETER, c(7, 8, 9))
  expect_equal(tree_editted$TREE_EXTANT_CODE, c("L", "L", "L"))
  expect_equal(tree_editted$LVD_EDIT[2],
               "Change to L based on later msmt")
  expect_equal(tree_editted$DIAMETER_EDIT[2],
               "Diameter assigned based on mean of prev and next diameters")
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
                     AGE_REPRESENTATIVE_IND = "Y")
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$DIAMETER, c(7, 8, 9))
  expect_equal(tree_editted$TREE_EXTANT_CODE, c("L", "L", "L"))
  expect_equal(tree_editted$LVD_EDIT[2],
               "Missing, added based on next msmt")
  expect_equal(tree_editted$DIAMETER_EDIT[2],
               "Diameter assigned based on mean of prev and next diameters")
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
                     AGE_REPRESENTATIVE_IND = "Y")
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$DIAMETER, c(7, 8, 9))
  expect_equal(tree_editted$TREE_EXTANT_CODE, c("L", "L", "L"))
  expect_equal(tree_editted$LVD_EDIT, rep(as.character(NA), 3))
  expect_equal(tree_editted$DIAMETER_EDIT[2],
               "Diameter assigned based on mean of prev and next diameters")
  expect_equal(tree_editted$MSMT_MISSING_EDIT[2],
               "Missing in between, added")
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
                     AGE_REPRESENTATIVE_IND = "Y")
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$DIAMETER, c(7, 7, 7))
  expect_equal(tree_editted$TREE_EXTANT_CODE, c("L", "D", "D"))
  expect_equal(tree_editted$LVD_EDIT, rep(as.character(NA), 3))
  expect_equal(tree_editted$DIAMETER_EDIT[2:3],
               c("Diameter assigned based on mean of prev and next diameters",
               "Diameter assinged based on previous msmt"))
  expect_equal(tree_editted$MSMT_MISSING_EDIT[2],
               "Missing in between, added")
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
                     AGE_REPRESENTATIVE_IND = "Y")
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$DIAMETER, c(7, 8, 8))
  expect_equal(tree_editted$TREE_EXTANT_CODE, c("L", "L", "D"))
  expect_equal(tree_editted$LVD_EDIT, c(rep(as.character(NA), 2),
                                        "Missing at last msmt, added dead"))
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
                     AGE_REPRESENTATIVE_IND = "Y")
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$DIAMETER, c(7, 8, 8))
  expect_equal(tree_editted$TREE_EXTANT_CODE, c("L", "L", "D"))
  expect_equal(tree_editted$MSMT_MISSING_EDIT[3],
               "Missing at tail, added")
  expect_equal(tree_editted$DIAMETER_EDIT[3],
               "Diameter assinged based on previous msmt")
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
                     AGE_REPRESENTATIVE_IND = "Y")
  tree_editted <- treemsmtEditing(compilationType = "PSP",
                                  treemsmts = tree,
                                  sitevisits = site_visits)
  expect_equal(tree_editted$BROKEN_TOP_IND, c("N", "Y", "Y"))
  expect_equal(tree_editted$BTOP_EDIT[3],
               "Change to Y based on previous msmt")
  rm(tree, tree_editted)

})
