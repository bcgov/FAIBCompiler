## to populate bored age for site trees using last measurements, set meas_cod = "FROM_LAST
## and to correct bored age using last measurements, set meas_cod = *-ADJ_FROM_LAST

vihPrep <- function(msmtInterval,
                    siteAgeTrees){
  siteAgeTrees <- merge(siteAgeTrees, msmtInterval,
                        by = "CLSTR_ID",
                        all.x = TRUE)
  tree_last_msmt <- siteAgeTrees[!is.na(BORED_HT) | !is.na(MEAS_COD),
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
                                      BORAG_FL_last = BORAG_FL,
                                      BORE_AGE_last = BORE_AGE,
                                      BORED_HT_last = BORED_HT)]
  siteAgeTrees <- merge(siteAgeTrees,
                        siteAgeTrees_last,
                        by = c("SITE_IDENTIFIER",
                               "PLOT",
                               "TREE_NO"),
                        all.x = TRUE)
  siteAgeTrees[MEAS_YR == MEAS_YR_REF,
               BORED_AGE_FLAG := "REFERENCE"]

  siteAgeTrees[is.na(BORED_HT) & is.na(MEAS_COD) &
                 MEAS_YR != MEAS_YR_REF,
               ':='(BORED_AGE_FLAG = "ADDED_FROM_REFERENCE",
                    BORED_HT = BORED_HT_last,
                    BORAG_FL = BORAG_FL_last - (MEAS_YR_REF - MEAS_YR),
                    BORE_AGE = BORE_AGE_last - (MEAS_YR_REF - MEAS_YR))] # the age information is from last measurement
  siteAgeTrees[(!is.na(BORED_HT) | !is.na(MEAS_COD)) & ## have measurement flag
                 MEAS_YR != MEAS_YR_REF & # not from last measurement
                 BORED_AGE_FLAG != "ADDED_FROM_REFERENCE" & # not from last measurement
                 BORED_HT == BORED_HT_last, # have same measurement height
               ':='(BORE_AGE_adj = BORE_AGE_last - (MEAS_YR_REF - MEAS_YR))] # the age information is from last measurement

  siteAgeTrees[BORE_AGE != BORE_AGE_adj,
               ':='(BORED_AGE_FLAG = "CORRECTED_FROM_REFERENCE",
                    BORE_AGE = BORE_AGE_adj)]
  siteAgeTrees[,':='(tree_last_times = NULL,
                     BORAG_FL_last = NULL,
                     BORE_AGE_last = NULL,
                     BORED_HT_last = NULL,
                     BORE_AGE_adj = NULL)]
  siteAgeTrees[BORE_AGE <= 0, BORE_AGE := NA]
  siteAgeTrees[BORAG_FL <= 0, BORAG_FL := NA]
  return(siteAgeTrees)
}


