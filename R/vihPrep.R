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
  ### this is temp fix for the confict between who and lab age
  ## there should not have lab age for who, as no core is taken for who
  siteAgeTrees[AGE_MEASURE_CODE == "WHO" &
                 BORE_AGE_LAB > 0 &
                 is.na(BORE_AGE_FLD),
               ':='(BORE_AGE_FLD = BORE_AGE_LAB,
                    AGE_MOD_COMMENT = "no lab age for who, move it to field age")]
  siteAgeTrees[AGE_MOD_COMMENT == "no lab age for who, move it to field age",
               BORE_AGE_LAB := NA]
  siteAgeTrees[AGE_MEASURE_CODE == "WHO" &
                 !is.na(BORE_AGE_LAB) &
                 !is.na(BORE_AGE_FLD),
               ':='(BORE_AGE_LAB = NA,
                    AGE_MOD_COMMENT = "no lab age for who, remove it")]
  ## total age can not be taken above 0
  ## fix a. for both the total age and (field age or lab age) exist, use field age or lab age
  siteAgeTrees[!is.na(TOTAL_AG) & BORED_HT > 0.01 &
                 (!is.na(BORE_AGE_FLD) | !is.na(BORE_AGE_LAB)) &
                 AGE_MEASURE_CODE != "PRE",
               ':='(TOTAL_AG = NA,
                    AGE_MOD_COMMENT = "Total age can not taken above 0 height, removed")]
  # fix b. if no field age or lab age available, using total age for field age
  siteAgeTrees[!is.na(TOTAL_AG) & BORED_HT > 0.01 &
                 (is.na(BORE_AGE_FLD) & is.na(BORE_AGE_LAB)) &
                 AGE_MEASURE_CODE != "PRE",
               ':='(BORE_AGE_FLD = TOTAL_AG,
                    AGE_MOD_COMMENT = "Total age can not taken above 0 height, moved to field age")]
  siteAgeTrees[AGE_MOD_COMMENT == "Total age can not taken above 0 height, moved to field age",
               TOTAL_AG := NA]

  ## the below codes to determine the best age measurement for multiple measure
  # only one measurement
  siteAgeTrees[, unitreeid := paste0(SITE_IDENTIFIER, "_", PLOT, "_", TREE_NO)]
  siteAgeTrees[AGE_MEASURE_CODE == "PTH" & # for PTH trees
                 (BORE_AGE_FLD > 0 | # field age > 0
                    BORE_AGE_LAB > 0 | # or lab age > 0
                    TOTAL_AG > 0), # or total age  > 0
               ageinfor := "OK"]
  siteAgeTrees[AGE_MEASURE_CODE == "WHO" & # for WHO trees
                 (BORE_AGE_FLD > 0 | # field age present or total age present
                    TOTAL_AG > 0),
               ageinfor := "OK"]
  siteAgeTrees[AGE_MEASURE_CODE == "OUT" & # for OUT trees, same as PTH trees
                 (BORE_AGE_FLD > 0 | # field age > 0
                    BORE_AGE_LAB > 0 | # or lab age > 0
                    TOTAL_AG > 0), # or total age  > 0
               ageinfor := "OK"]
  siteAgeTrees[AGE_MEASURE_CODE == "NOP" & # for NOP trees
                 (BORE_AGE_FLD > 0 | # field age > 0
                    BORE_AGE_LAB > 0),  # or lab age > 0
               ageinfor := "OK"]
  siteAgeTrees[AGE_MEASURE_CODE %in% c("CRC", "ROT") & # for CRC and ROT trees
               (PRO_LEN > 0 & PRO_RING > 0 & BNG_DIAM > 0), # all pro_len, pro_ring and bng_diam
               ageinfor := "OK"]                            # needed for prorate
  siteAgeTrees[AGE_MEASURE_CODE == "PHY" & # for PHY trees
                 PHYS_AGE > 0, # phys_age must present
               ageinfor := "OK"]
  siteAgeTrees[AGE_MEASURE_CODE == "EST" & # for EST trees
                 BORE_AGE_FLD > 0, # estimate age must be from field
               ageinfor := "OK"]

  siteAgeTrees_good <- siteAgeTrees[ageinfor == "OK",]
  siteAgeTrees[, ageinfor := NULL]
  tree_only_one <- siteAgeTrees_good[,.(tree_last_time = max(VISIT_NUMBER),
                                   tree_visits = length(BORED_HT)),
                                by = "unitreeid"]
  tree_last_msmt <- tree_only_one[tree_visits == 1, .(unitreeid, tree_last_time)]
  ## based on Scott's recommandation the priority order is
  ## PTH>NOP>WHO>OUT>CRC>ROT>PHY
  ## for multiple trees
  ## 1. last pth is the most reliable source
  tree_last_pth <- siteAgeTrees_good[!(unitreeid %in% tree_last_msmt$unitreeid) &
                                  AGE_MEASURE_CODE == "PTH",
                                .(tree_last_time = max(VISIT_NUMBER)),
                                by = "unitreeid"]
  tree_last_msmt <- rbind(tree_last_msmt, tree_last_pth)
  ## 2. last nop wins the rest
  tree_last_nop <- siteAgeTrees_good[!(unitreeid %in% tree_last_msmt$unitreeid) &
                                       AGE_MEASURE_CODE == "NOP",
                                     .(tree_last_time = max(VISIT_NUMBER)),
                                     by = "unitreeid"]
  tree_last_msmt <- rbind(tree_last_msmt, tree_last_nop)

  ## 3. last WHO wins the rest
  tree_last_who <- siteAgeTrees_good[!(unitreeid %in% tree_last_msmt$unitreeid) &
                                  AGE_MEASURE_CODE == "WHO",
                                .(tree_last_time = max(VISIT_NUMBER)),
                                by = "unitreeid"]
  tree_last_msmt <- rbind(tree_last_msmt, tree_last_who)
  ## 4. last out wins the rest
  tree_last_out <- siteAgeTrees_good[!(unitreeid %in% tree_last_msmt$unitreeid) &
                                  AGE_MEASURE_CODE == "OUT",
                                .(tree_last_time = max(VISIT_NUMBER)),
                                by = "unitreeid"]
  tree_last_msmt <- rbind(tree_last_msmt, tree_last_out)
  ## 5. last crc wins the rest
  tree_last_crc <- siteAgeTrees_good[!(unitreeid %in% tree_last_msmt$unitreeid) &
                                  AGE_MEASURE_CODE == "CRC",
                                .(tree_last_time = max(VISIT_NUMBER)),
                                by = "unitreeid"]
  tree_last_msmt <- rbind(tree_last_msmt, tree_last_crc)
  ## 6. last ROT wins the rest
  tree_last_rot <- siteAgeTrees_good[!(unitreeid %in% tree_last_msmt$unitreeid) &
                                  AGE_MEASURE_CODE == "ROT",
                                .(tree_last_time = max(VISIT_NUMBER)),
                                by = "unitreeid"]
  tree_last_msmt <- rbind(tree_last_msmt, tree_last_rot)
  ## 7. last phy wins
  tree_last_phy <- siteAgeTrees_good[!(unitreeid %in% tree_last_msmt$unitreeid) &
                                  AGE_MEASURE_CODE == "PHY",
                                .(tree_last_time = max(VISIT_NUMBER)),
                                by = "unitreeid"]
  tree_last_msmt <- rbind(tree_last_msmt, tree_last_phy)

  ## 8. last est wins
  tree_last_est <- siteAgeTrees_good[!(unitreeid %in% tree_last_msmt$unitreeid) &
                                  AGE_MEASURE_CODE == "EST",
                                .(tree_last_time = max(VISIT_NUMBER)),
                                by = "unitreeid"]
  tree_last_msmt <- rbind(tree_last_msmt, tree_last_est)

  siteAgeTrees <- merge(siteAgeTrees,
                        tree_last_msmt,
                        by = c("unitreeid"),
                        all.x = TRUE)
  siteAgeTrees_last <- siteAgeTrees[VISIT_NUMBER == tree_last_time,
                                    .(unitreeid,
                                      AGE_MEASURE_CODE,
                                      MEAS_YR_REF = MEAS_YR,
                                      BOREAG_FD_last = BORE_AGE_FLD,
                                      AGE_CORE_MISSED_YEARS_FIELD,
                                      BOREAG_LB_last = BORE_AGE_LAB,
                                      AGE_CORE_MISSED_YEARS_LAB,
                                      TOTAL_AG_last = TOTAL_AG,
                                      PHYS_AGE_last = PHYS_AGE,
                                      BORED_HT_last = BORED_HT)]
  ### assign BORED_AGE_SOURCE here
  ### first from NOP trees
  ## for NOP trees, the age should be prorated
  siteAgeTrees_last[AGE_MEASURE_CODE == "NOP",
              BORED_AGE_SOURCE := "prorated_needed"]
  ## the age_bor should take order from
  ## 1. lab age + lab missing age
  ## 2. lab age + field missing age
  ## 3. lab age
  ## 4. field age + field missing age
  ## 5. field age
  siteAgeTrees_last[BOREAG_LB_last > 0 &
                AGE_CORE_MISSED_YEARS_LAB > 0 &
                BORED_AGE_SOURCE == "prorated_needed",
              ':='(BOREAG_LB_last = BOREAG_LB_last + AGE_CORE_MISSED_YEARS_LAB,
                   BORED_AGE_SOURCE = "Lab Prorated Age from NOP")]
  siteAgeTrees_last[BOREAG_LB_last > 0 &
                AGE_CORE_MISSED_YEARS_FIELD > 0 &
                BORED_AGE_SOURCE == "prorated_needed",
              ':='(BOREAG_LB_last = BOREAG_LB_last + AGE_CORE_MISSED_YEARS_FIELD,
                   BORED_AGE_SOURCE = "Lab Prorated Age from NOP")]
  siteAgeTrees_last[BOREAG_LB_last > 0 &
                BORED_AGE_SOURCE == "prorated_needed",
              ':='(BOREAG_LB_last = BOREAG_LB_last,
                   BORED_AGE_SOURCE = "Lab Prorated Age from NOP")]
  siteAgeTrees_last[BOREAG_FD_last > 0 &
                BORED_AGE_SOURCE == "prorated_needed",
              ':='(BOREAG_FD_last = BOREAG_FD_last + AGE_CORE_MISSED_YEARS_FIELD,
                   BORED_AGE_SOURCE = "Field Prorated Age from NOP")]
  ##
  ### this is a temp fix for PTH trees, when there is a missing age available
  siteAgeTrees_last[AGE_MEASURE_CODE == "PTH",
                    ':='(BOREAG_LB_last = BOREAG_LB_last + AGE_CORE_MISSED_YEARS_LAB,
                         BOREAG_FD_last = BOREAG_FD_last + AGE_CORE_MISSED_YEARS_FIELD)]
  ## for crc and rot, it only can be prorated
  siteAgeTrees_last[AGE_MEASURE_CODE %in% c("CRC", "ROT"),
                    BORED_AGE_SOURCE := "Prorated Age from CRC/ROT"]

  ## for crc and rot, it only can be prorated
  siteAgeTrees_last[AGE_MEASURE_CODE == "EST",
                    BORED_AGE_SOURCE := "Estimated Age"]

  siteAgeTrees_last[BOREAG_LB_last > 0 &
                      is.na(BORED_AGE_SOURCE),
              BORED_AGE_SOURCE := "Lab Age"]
  siteAgeTrees_last[BOREAG_FD_last > 0 &
                      is.na(BORED_AGE_SOURCE),
              BORED_AGE_SOURCE := "Field Age"]
  siteAgeTrees_last[TOTAL_AG_last > 0 &
                      is.na(BORED_AGE_SOURCE),
              BORED_AGE_SOURCE := "Total Age"]
  siteAgeTrees_last[PHYS_AGE_last > 0 &
                      is.na(BORED_AGE_SOURCE),
              BORED_AGE_SOURCE := "Physiologic Age"]

  siteAgeTrees_last[is.na(BORED_HT_last) &
                      BORED_AGE_SOURCE == "Total Age",
                    BORED_HT_last := 0]
  siteAgeTrees_last[is.na(BORED_HT_last),
                    BORED_HT_last := 1.3]

  siteAgeTrees_last[,':='(AGE_CORE_MISSED_YEARS_FIELD = NULL,
                          AGE_CORE_MISSED_YEARS_LAB = NULL,
                          AGE_MEASURE_CODE = NULL)]

  siteAgeTrees_last[!is.na(BOREAG_FD_last) & is.na(BOREAG_LB_last),
                    lab_age_remove := TRUE]
  siteAgeTrees <- merge(siteAgeTrees,
                        siteAgeTrees_last,
                        by = "unitreeid",
                        all.x = TRUE)
  siteAgeTrees <- siteAgeTrees[!is.na(MEAS_YR_REF) | !is.na(AGE_MEASURE_CODE)]
  siteAgeTrees[is.na(MEAS_YR_REF),
               ':='(BORED_AGE_FLAG = "No valid age infor for this tree",
                    BORED_AGE_SOURCE = "Not Applicable")]
  siteAgeTrees[!is.na(BORE_AGE_LAB) & lab_age_remove == TRUE,
               BORE_AGE_LAB := NA]

  # choose last fld age over prevous lab age as the reference to correct
  siteAgeTrees[VISIT_NUMBER == tree_last_time,
               BORED_AGE_FLAG := "Reference"]
  siteAgeTrees[is.na(BORED_AGE_FLAG),
               BORED_AGE_FLAG := "NA"]
  ## add populated trees
  siteAgeTrees[is.na(BORED_HT) & is.na(AGE_MEASURE_CODE) &
                 VISIT_NUMBER != tree_last_time &
                 !is.na(MEAS_YR_REF),
               ':='(AGE_MEASURE_CODE = "CALC")] # the age information is from last measurement

  ## for bored height adjustment
  siteAgeTrees[BORED_HT != BORED_HT_last |
                 is.na(BORED_HT) &
                 !is.na(MEAS_YR_REF),
               ':='(BORED_HT = BORED_HT_last,
                    BORED_HT_FLAG = "Mismatch/missing, set to reference BORED_HT")] # for the pre msmt, the bored height should be
                                          # adjusted to reference bored height

  siteAgeTrees[!is.na(MEAS_YR_REF), # all have reference
               ':='(BORE_AGE_adj = BOREAG_LB_last - (MEAS_YR_REF - MEAS_YR),
                    BORE_FLD_adj = BOREAG_FD_last - (MEAS_YR_REF - MEAS_YR),
                    TOTAL_AGE_adj = TOTAL_AG_last - (MEAS_YR_REF - MEAS_YR),
                    PHYS_AGE_adj = PHYS_AGE_last - (MEAS_YR_REF - MEAS_YR))] # the age information is from last measurement

  siteAgeTrees[BORED_AGE_FLAG == "Reference",
               ':='(BORE_AGE_LAB = BOREAG_LB_last,
                    BORE_AGE_FLD = BOREAG_FD_last,
                    TOTAL_AG = TOTAL_AG_last,
                    PHYS_AGE = PHYS_AGE_last)]
  ## only need to adjust lab age for below cases
  siteAgeTrees[BORED_AGE_FLAG != "Reference" &
                 BORED_AGE_SOURCE %in% c("Lab Age", "Lab Prorated Age from NOP") &
                 (BORE_AGE_LAB != BORE_AGE_adj |
                    is.na(BORE_AGE_LAB)),
               ':='(BORED_AGE_FLAG = "Lab age mismatch/missing Corrected from Reference",
                    BORE_AGE_LAB = BORE_AGE_adj)]
  ## only need to adjust field age for below cases
  siteAgeTrees[BORED_AGE_FLAG != "Reference" &
                 BORED_AGE_SOURCE %in% c("Field Age", "Field Prorated Age from NOP") &
                 (BORE_AGE_FLD != BORE_FLD_adj |
                    is.na(BORE_AGE_FLD)),
               ':='(BORED_AGE_FLAG = "Field age mismatch/missing Corrected from Reference",
                    BORE_AGE_FLD = BORE_FLD_adj)]
  ## only need to adjust total age for below cases
  siteAgeTrees[BORED_AGE_FLAG != "Reference" &
                 BORED_AGE_SOURCE == "Total Age" &
                 (TOTAL_AG != TOTAL_AGE_adj |
                     is.na(TOTAL_AG)),
               ':='(BORED_AGE_FLAG = "Total age mismatch/missing Corrected from Reference",
                    TOTAL_AG = TOTAL_AGE_adj)]
  ## only need to adjust phys age for below cases
  siteAgeTrees[BORED_AGE_FLAG != "Reference" &
                 BORED_AGE_SOURCE == "Physiologic Age" &
                 (PHYS_AGE != PHYS_AGE_adj |
                     is.na(PHYS_AGE)),
               ':='(BORED_AGE_FLAG = "Phys age mismatch/missing Corrected from Reference",
                    PHYS_AGE = PHYS_AGE_adj)]
  ## for the crc and rot the age will be adjusted after prorating

  siteAgeTrees[,':='(tree_last_time = NULL,
                     BOREAG_FD_last = NULL,
                     BOREAG_LB_last = NULL,
                     TOTAL_AG_last = NULL,
                     BORED_HT_last = NULL,
                     BORE_AGE_adj = NULL,
                     BORE_FLD_adj = NULL,
                     TOTAL_AGE_adj = NULL,
                     PHYS_AGE_adj = NULL,
                     lab_age_remove = NULL,
                     PHYS_AGE_last = NULL,
                     AGE_CORE_MISSED_YEARS_FIELD = NULL,
                     AGE_CORE_MISSED_YEARS_LAB = NULL)]
  siteAgeTrees[BORE_AGE_LAB <= 0,
               ':='(BORE_AGE_LAB = NA,
                    BORED_AGE_FLAG = "Adjusted lab age less than 0",
                    BORED_AGE_SOURCE = "Not Applicable")]
  siteAgeTrees[BORE_AGE_FLD <= 0,
               ':='(BORE_AGE_FLD = NA,
                    BORED_AGE_FLAG = "Adjusted field age less than 0",
                    BORED_AGE_SOURCE = "Not Applicable")]
  return(siteAgeTrees)
}


