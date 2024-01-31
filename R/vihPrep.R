#' to populate bored age for site trees using last measurements, set meas_cod = "FROM_LAST
#' and to correct bored age using last measurements, set meas_cod = *-ADJ_FROM_LAST
#'
#' @description This function is to populate bored age for site trees using last measurements, set meas_cod = "FROM_LAST
#' and to correct bored age using last measurements, set meas_cod = *-ADJ_FROM_LAST
#'
#' @param msmtInterval data.table, contains sample visits.
#' @param siteAgeTrees data.table, sample trees.
#'
#'
#' @return A data table
#'
#'
#' @importFrom data.table data.table ':=' set rbindlist setnames setkey
#' @importFrom dplyr '%>%'
#' @export
#' @docType methods
#' @rdname vihPrep
#'
#' @author Yong Luo
vihPrep <- function(msmtInterval,
                    siteAgeTrees){
  siteAgeTrees <- merge(siteAgeTrees, msmtInterval,
                        by = "CLSTR_ID",
                        all.x = TRUE)
  tree_last_msmt <- siteAgeTrees[(!is.na(BORED_HT) | !is.na(AGE_MEASURE_CODE)) &
                                   (!is.na(BORE_AGE_FLD) | !is.na(BORE_AGE_LAB)) &
                                   (!AGE_MEASURE_CODE %in% c("PRE", "NOC") | is.na(AGE_MEASURE_CODE)),
                                 .(tree_last_times = max(VISIT_NUMBER)),
                                 by = c("SITE_IDENTIFIER", "PLOT", "TREE_NO")]
  siteAgeTrees <- merge(siteAgeTrees,
                        tree_last_msmt,
                        by = c("SITE_IDENTIFIER", "PLOT", "TREE_NO"),
                        all.x = TRUE)
  siteAgeTrees_last <- siteAgeTrees[VISIT_NUMBER == tree_last_times,
                                    .(SITE_IDENTIFIER,
                                      PLOT,
                                      TREE_NO,
                                      MEAS_YR_REF = MEAS_YR,
                                      BORAG_FL_last = BORE_AGE_FLD,
                                      BORE_AGE_last = BORE_AGE_LAB,
                                      BORED_HT_last = BORED_HT)]
  siteAgeTrees_last[!is.na(BORAG_FL_last) & is.na(BORE_AGE_last),
                    lab_age_remove := TRUE]
  siteAgeTrees <- merge(siteAgeTrees,
                        siteAgeTrees_last,
                        by = c("SITE_IDENTIFIER",
                               "PLOT",
                               "TREE_NO"),
                        all.x = TRUE)
  siteAgeTrees[!is.na(BORE_AGE_LAB) & lab_age_remove == TRUE,
               BORE_AGE_LAB := NA]
  # choose last fld age over prevous lab age as the reference to correct
  siteAgeTrees[MEAS_YR == MEAS_YR_REF,
               BORED_AGE_FLAG := "Reference"]

  siteAgeTrees[is.na(BORED_HT) & is.na(AGE_MEASURE_CODE) &
                 MEAS_YR != MEAS_YR_REF,
               ':='(BORED_AGE_FLAG = "Added from Reference",
                    BORED_HT = BORED_HT_last,
                    BORE_AGE_FLD = BORAG_FL_last - (MEAS_YR_REF - MEAS_YR),
                    BORE_AGE_LAB = BORE_AGE_last - (MEAS_YR_REF - MEAS_YR))] # the age information is from last measurement
  siteAgeTrees[(!is.na(BORED_HT) | !is.na(AGE_MEASURE_CODE)) & ## have measurement flag
                 MEAS_YR != MEAS_YR_REF & # not from last measurement
                 is.na(BORED_AGE_FLAG), # not added
               ':='(BORE_AGE_adj = BORE_AGE_last - (MEAS_YR_REF - MEAS_YR),
                    BORE_FLD_adj = BORAG_FL_last - (MEAS_YR_REF - MEAS_YR))] # the age information is from last measurement
  siteAgeTrees[(BORE_AGE_LAB != BORE_AGE_adj |
                  is.na(BORE_AGE_LAB)) &
                 !is.na(BORE_AGE_adj) &
                 is.na(BORED_AGE_FLAG),
               ':='(BORED_AGE_FLAG = "LAB_AGE Corrected from Reference",
                    BORE_AGE_LAB = BORE_AGE_adj)]
  siteAgeTrees[BORED_AGE_FLAG == "LAB_AGE Corrected from Reference" &
                 (BORED_HT_last != BORED_HT | is.na(BORED_HT)),
               ':='(BORED_HT = BORED_HT_last, # bored ht set to reference
                    BORED_AGE_FLAG = "LAB_AGE Corrected from Reference, BORED_HT set to Reference")]
  siteAgeTrees[((BORE_AGE_FLD != BORE_FLD_adj |
                   is.na(BORE_AGE_FLD))) &
                 !is.na(BORE_FLD_adj) &
                 is.na(BORED_AGE_FLAG),
               ':='(BORED_AGE_FLAG = "FLD_AGE Corrected from Reference",
                    BORE_AGE_FLD = BORE_FLD_adj)]
  siteAgeTrees[BORED_AGE_FLAG == "FLD_AGE Corrected from Reference" &
                 (BORED_HT_last != BORED_HT | is.na(BORED_HT)),
               ':='(BORED_HT = BORED_HT_last, # bored ht set to reference
                    BORED_AGE_FLAG = "FLD_AGE Corrected from Reference, BORED_HT set to Reference")]

  siteAgeTrees[,':='(tree_last_times = NULL,
                     BORAG_FL_last = NULL,
                     BORE_AGE_last = NULL,
                     BORED_HT_last = NULL,
                     BORE_AGE_adj = NULL,
                     BORE_FLD_adj = NULL,
                     lab_age_remove = NULL)]
  siteAgeTrees[BORE_AGE_LAB <= 0, BORE_AGE_LAB := NA]
  siteAgeTrees[BORE_AGE_FLD <= 0, BORE_AGE_FLD := NA]
  siteAgeTrees[BORED_HT > 0.1 & !is.na(TOTAL_AG),
               TOTAL_AG := NA] # the bore height higher than 0.1 does not give total age

  siteAgeTrees_noref <- siteAgeTrees[is.na(MEAS_YR_REF),
                                     .(lastmearyr = max(MEAS_YR)),
                                     by = c("SITE_IDENTIFIER", "PLOT", "TREE_NO")]
  siteAgeTrees <- merge(siteAgeTrees,
                        siteAgeTrees_noref,
                        by = c("SITE_IDENTIFIER", "PLOT", "TREE_NO"),
                        all.x = TRUE)
  siteAgeTrees[is.na(MEAS_YR_REF),
               MEAS_YR_REF := lastmearyr]
  siteAgeTrees[is.na(BORED_AGE_FLAG) & MEAS_YR == MEAS_YR_REF,
               BORED_AGE_FLAG := "Reference"]
  siteAgeTrees[, lastmearyr := NULL]
  return(siteAgeTrees)
}


