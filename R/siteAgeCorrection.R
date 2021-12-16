#' Bored age correction for multiple measurments based on measurement interval
#'
#' @description Correct the bored age if the difference of ages between two measurements does
#'              not match the years of measurement interval
#'
#' @param vih data.table, The table contains the site trees in the database.
#'
#' @return A site tree table that is equivalent to original vih,
#'         but with bored age corrected
#'
#'
#' @importFrom data.table data.table ':=' set rbindlist setnames setkey
#' @importFrom dplyr '%>%'
#' @export
#' @note 1) for the site trees with the age measurement code of PRE, the current age is corrected
#'          based on the previous bored age.
#'       2) for the site trees with multiple drills, the age of last measurement is used as a
#'          reference point, and the previous ages are adjusted.
#'       3) some ages of site trees are manuallly corrected due to complexity of measurements (see comments in the codes)
#' @docType methods
#' @rdname siteAgeCorrection
#'
#' @author Yong Luo
siteAgeCorrection<- function(vih){
  vih[, ':='(siteid = substr(CLSTR_ID, 1, 7),
             visit_no = substr(CLSTR_ID, 10, 10),
             meas_year = as.numeric(substr(MEAS_DT, 1, 4)))]

  ## identify the site trees in multiple measurements
  vih[, obsn := length(BARK_THK),
      by = c("siteid", "PLOT", "TREE_NO")]

  vih_multi <- vih[obsn > 1]
  vih_multi <- vih_multi[order(siteid, PLOT, TREE_NO, visit_no),]
  vih_multi[!is.na(BORE_AGE),
            ':='(BORAG_FL_temp = as.integer(NA),
                 TOTAL_AG_temp = as.integer(NA))]
  vih_multi[is.na(BORE_AGE) & !is.na(BORAG_FL),
            BORAG_FL_temp := BORAG_FL]
  vih_multi[is.na(BORE_AGE) & is.na(BORAG_FL),
            TOTAL_AG_temp := TOTAL_AG]

  ## identify ages copy from previous meas
  vih_multi[, ':='(meas_year_prev = shift(meas_year, type = "lag"),
                   grow5_prev = shift(GROW_5YR, type = "lag"),
                   meas_code_prev = shift(MEAS_COD, type = "lag"),
                   age_lab_prev = shift(BORE_AGE, type = "lag"),
                   age_field_prev = shift(BORAG_FL_temp, type = "lag"),
                   age_total_prev = shift(TOTAL_AG_temp, type = "lag")),
            by = c("siteid", "PLOT", "TREE_NO")]
  vih_multi[meas_year != meas_year_prev &
              (BORE_AGE == age_lab_prev |
                 age_field_prev == BORAG_FL_temp |
                 age_total_prev == TOTAL_AG_temp),
            MEAS_COD := "COPY"]

  ## remove the copy ages and pre measured ages
  vih_base <- vih_multi[!(MEAS_COD %in% c("PRE", "COPY")),
                        .(siteid, PLOT, TREE_NO, meas_year, BORAG_FL,
                          BORE_AGE, TOTAL_AG)]

  ## use the last measurement as the reference
  vih_base_bore_age <- vih_base[!is.na(BORE_AGE),
                                .(baseyear_bore_age = max(meas_year)),
                                by = c("siteid", "PLOT", "TREE_NO")]
  vih_base <- merge(vih_base,
                    vih_base_bore_age,
                    by = c("siteid", "PLOT", "TREE_NO"),
                    all.x = TRUE)

  vih_base_field_age <- vih_base[!is.na(BORAG_FL),
                                 .(baseyear_field_age = max(meas_year)),
                                 by = c("siteid", "PLOT", "TREE_NO")]
  vih_base <- merge(vih_base,
                    vih_base_field_age,
                    by = c("siteid", "PLOT", "TREE_NO"),
                    all.x = TRUE)

  vih_base_total_age <- vih_base[!is.na(TOTAL_AG),
                                 .(baseyear_total_age = max(meas_year)),
                                 by = c("siteid", "PLOT", "TREE_NO")]
  vih_base <- merge(vih_base,
                    vih_base_total_age,
                    by = c("siteid", "PLOT", "TREE_NO"),
                    all.x = TRUE)

  vih_base[!is.na(baseyear_bore_age),
           ':='(baseyear = baseyear_bore_age,
                age_corrected = "bore_age")]

  vih_base[is.na(baseyear) &
             !is.na(baseyear_field_age),
           ':='(baseyear = baseyear_field_age,
                age_corrected = "field_age")]

  vih_base[is.na(baseyear) &
             !is.na(baseyear_total_age),
           ':='(baseyear = baseyear_total_age,
                age_corrected = "total_age")]


  vih_base <- vih_base[meas_year == baseyear,
                       .(siteid, PLOT, TREE_NO, baseyear, age_corrected,
                         base_age_lab = BORE_AGE, ## first priority
                         base_age_field = BORAG_FL, ## second priority
                         base_age_total = TOTAL_AG)] ## third priority

  vih_base <- unique(vih_base, by = c("siteid", "PLOT", "TREE_NO"))



  vih_multi <- merge(vih_multi,
                     vih_base,
                     by = c("siteid", "PLOT", "TREE_NO"),
                     all.x = TRUE)

  ## examine age match
  vih_multi[, measure_interval := meas_year - baseyear]

  vih_multi[, ':='(age_lab_pred = measure_interval + base_age_lab,
                   age_field_pred = measure_interval + base_age_field,
                   age_total_pred = measure_interval + base_age_total)]

  vih_multi[age_corrected == "bore_age",
            age_lab_corrected := age_lab_pred]
  vih_multi[age_corrected == "field_age",
            age_field_corrected := age_field_pred]
  vih_multi[age_corrected == "total_age",
            age_total_corrected := age_total_pred]

  vih_multi[, ':='(age_lab_dif = age_lab_pred - BORE_AGE,
                   age_field_dif = age_field_pred - BORAG_FL,
                   age_total_dif = age_total_pred - TOTAL_AG)]

  ##
  vih_multi[age_corrected == "bore_age" &
              (age_lab_dif != 0 |
                 is.na(age_lab_dif)),
            age_dif_mark := 1]

  vih_multi[age_corrected == "field_age" &
              (age_field_dif != 0 |
                 is.na(age_field_dif)),
            age_dif_mark := 1]

  vih_multi[age_corrected == "total_age" &
              (age_total_dif != 0 |
                 is.na(age_total_dif)),
            age_dif_mark := 1]

  vih_multi[,':='(correct_mark = sum(age_dif_mark, na.rm = TRUE)),
            by = c("siteid", "PLOT", "TREE_NO")]

  vih_multi[, ':='(age_correct_meas = FALSE,
                   age_correct_tree = FALSE)]
  vih_multi[age_dif_mark == 1,
            age_correct_meas := TRUE]
  vih_multi[correct_mark > 0,
            age_correct_tree := TRUE]

  age_corrected_trees <- vih_multi[age_correct_tree == TRUE,
                                   .(CLSTR_ID, PLOT, TREE_NO, siteid, visit_no,
                                     meas_year, refer_year = baseyear, age_correct_meas,
                                     BORE_AGE_org = BORE_AGE,
                                     BORE_AGE_crt = age_lab_corrected,
                                     BORE_AGE_dif = age_lab_dif,
                                     BORAG_FL_org = BORAG_FL,
                                     BORAG_FL_crt = age_field_corrected,
                                     BORAG_FL_dif = age_field_dif,
                                     TOTAL_AG_org = TOTAL_AG,
                                     TOTAL_AG_crt = age_total_corrected,
                                     TOTAL_AG_dif = age_total_dif)]
  age_corrected_meas <- vih_multi[age_correct_meas == TRUE,
                                  .(CLSTR_ID, PLOT, TREE_NO,
                                    meas_year, refer_year = baseyear,
                                    BORE_AGE_org = BORE_AGE,
                                    BORE_AGE_crt = age_lab_corrected,
                                    BORE_AGE_dif = age_lab_dif,
                                    BORAG_FL_org = BORAG_FL,
                                    BORAG_FL_crt = age_field_corrected,
                                    BORAG_FL_dif = age_field_dif,
                                    TOTAL_AG_org = TOTAL_AG,
                                    TOTAL_AG_crt = age_total_corrected,
                                    TOTAL_AG_dif = age_total_dif)]

  ### check the corrected ages
  # 1) for the big difference
  age_corrected_trees_big_dif <- data.table::copy(age_corrected_trees)
  age_corrected_trees_big_dif[abs(BORE_AGE_dif) >= 50 |
                                abs(BORAG_FL_dif) >= 50 |
                                abs(TOTAL_AG_dif) >= 50,
                              big_age_dif := 1]
  age_corrected_trees_big_dif[, big_age_dif := sum(big_age_dif, na.rm = TRUE),
                              by = c("siteid", "PLOT", "TREE_NO")]
  age_corrected_trees_big_dif <- age_corrected_trees_big_dif[big_age_dif > 0,]

  # 2) for the missing age
  age_corrected_trees_missing_age <- data.table::copy(age_corrected_trees)
  age_corrected_trees_missing_age[(!is.na(BORE_AGE_crt) & is.na(BORE_AGE_org)) |
                                    (!is.na(BORAG_FL_crt) & is.na(BORAG_FL_org)) |
                                    (!is.na(TOTAL_AG_crt) & is.na(TOTAL_AG_org)),
                                  age_missing := 1]
  age_corrected_trees_missing_age[, age_missing := sum(age_missing, na.rm = TRUE),
                                  by = c("siteid", "PLOT", "TREE_NO")]
  age_corrected_trees_missing_age <- age_corrected_trees_missing_age[age_missing > 0,]

  # # 3) problimetic sites
  # problemsite <- vih[siteid == 1355611,
  #                    .(CLSTR_ID, PLOT, TREE_NO, SPECIES, meas_year, TREE_LEN,
  #                      BORE_AGE, BORAG_FL, TOTAL_AG)]
  age_corrected_meas[, ':='(meas_year.x = NULL,
                            meas_year.y = NULL)]
  return(list("age_corrected" = age_corrected_meas,
              "age_corrected_trees_big_dif" = age_corrected_trees_big_dif,
              "age_corrected_trees_missing_age" = age_corrected_trees_missing_age))
}
