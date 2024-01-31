#' Edit tree measurements for a repeatedly measured data.
#'
#'
#' @description This function takes tree-level measurements and edits live/dead codes,
#'              diameter, species, and add tree measurements if the measurements are missing.
#'
#' @param compilationType character, Compilation type, either \code{nonPSP} or \code{PSP}.
#' @param treemsmts data.table, Tree measurements. The table must contain unique tree id, i.e.,
#'                              \code{SITE_IDENTIFIER}, \code{PLOT}, and \code{TREE_NUMBER}, and
#'                              \code{VISIT_NUMBER}.
#'                              And key measurements including live/dead (\code(TREE_EXTANT_CODE)),
#'                              diameter information (\code{DIAMETER} and \code{DIAMETER_MEASMT_HEIGHT}),
#'                              \code{TREE_CLASS_CODE}, \code{BROKEN_TOP_IND}, \code{CROWN_CLASS_CODE},
#'                              and \code{TREE_SPECIES_CODE}.
#' @param sitevisits data.table, Site visit information including \code{SITE_IDENTIFIER} and \code{VISIT_NUMBER}.
#'
#' @return return a table after editing.
#' @note 1. For missing live/dead code, using the next live/dead code if it is present. Otherwise,
#'          assign dead (D).
#'       2. In the case a tree is observed dead at a visit and change to live in the next visit,
#'          the dead code (D) will be changed to live (L).
#'       3. If there is a missing measurement in between two visits. A measurement will be added
#'          with diameter assigned as mean of previous and next diameter and live/dead will be populated using
#'          next measurement.
#'       4. If there is a missing measurement at tail, i.e., a tree was seen live for a given visit but
#'          without reaching the last visit for a given site. A measurement for next visit will be added with
#'          diameter populated using previous diameter and live/dead code will be assigned as D.
#'       5. If species code changes during visits, the species code at last visit will be used to correct
#'          species code throughout all visits.
#'       6. If at a tree's last visit, there is no diameter information and it is marked as live tree.
#'          The live/dead code will be changed to D.
#'       7. If a tree is dead at a given visit and without diameter, and a tree was live at previous
#'          visit with diameter, the diameter at previous visit will be used to populate diameter at this
#'          visit.
#'       8. If a tree is broken top tree for a given visit, this tree must be broken top tree since
#'          then.
#'       9. If crown_class_code is missing at a visit and it was present at previous visit,
#'          the previous crown_class_code will be used to populate for this visit.
#'
#' @importFrom data.table ':='
#' @importFrom FAIBBase merge_dupUpdate
#' @importFrom data.table shift
#'
#'
#' @export
#' @docType methods
#' @rdname treemsmtEditing
#'
#' @author Yong Luo
treemsmtEditing <- function(compilationType,
                            treemsmts,
                            sitevisits){
  treemsmts[, unitreeid := paste0(SITE_IDENTIFIER, "-", PLOT, "-", TREE_NUMBER)]
  # Please note that this is the correction diameter in old sas codes,
  # realize this correction approach may have problem as
  # there may be an added decimal for current msmt, in the below example, the
  # current diameter could be 67
  # Therefore, I implemented a manual correction process based on observed
  # diameter from other msmts of a tree
  # *determine that previous measurement was incorrectly keypunched by missing the decimal;
  # *the last measurement is assumed to be correct, so start check from latest measurement;
  # *assessment is done by computing the ratio of current dbh to last dbh measurement;
  # *if previous dbh is over 9X greater than current dbh, then it is extemely likely that the decimal was missed;
  # *at previous measurement;
  # *eg., from 64cm to 6.7cm, where the ratio 64/6.7=9.6;
  treemsmts[, diam_new := dbhManualCorrection(treeID = unitreeid,
                                              visitNum = VISIT_NUMBER,
                                              diameterOrg = DIAMETER)]

  treemsmts[!is.na(diam_new), ':='(DIAMETER = diam_new,
                                   DIAMETER_EDIT = "Abnormal diameter, mannually corrected")]
  treemsmts[DIAMETER == 0,
            DIAMETER := NA]
  treemsmts[, ':='(diam_new = NULL)]
  # for z trees, always out of plot
  ztrees <- unique(treemsmts[MEASUREMENT_ANOMALY_CODE == "Z"]$unitreeid)
  treemsmts[unitreeid %in% ztrees,
            OUT_OF_PLOT_IND := "Y"]
  rm(ztrees)


  treemsmts <- merge(treemsmts,
                     sitevisits[,.(SITE_IDENTIFIER, VISIT_NUMBER, VISIT_TYPE)],
                     by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                     all.x = TRUE)
  treemsmts_rep <- treemsmts[VISIT_TYPE == "REP",]
  treemsmts_tmp <- treemsmts[VISIT_TYPE == "TMP",]
  rm(treemsmts)


  # for stem mapping, use the last available stem mapping information for
  # a given tree to populate stem mapping for other msmts
  stemmap <- treemsmts_rep[!is.na(STEM_MAP_BEARING) & !is.na(STEM_MAP_DISTANCE),
                           .(unitreeid, VISIT_NUMBER,
                             STEM_MAP_BEARING,
                             STEM_MAP_DISTANCE)]
  stemmap[, lastvisit := max(VISIT_NUMBER),
          by = "unitreeid"]
  stemmap <- stemmap[VISIT_NUMBER == lastvisit,
                     .(unitreeid,
                       visit_ref = lastvisit,
                       bearing_ref = STEM_MAP_BEARING,
                       distance_ref = STEM_MAP_DISTANCE)]
  treemsmts_rep <- merge(treemsmts_rep,
                         stemmap,
                         by = "unitreeid",
                         all.x = TRUE)
  rm(stemmap)
  gc()
  treemsmts_rep[!is.na(visit_ref) &
                  VISIT_NUMBER != visit_ref &
                  paste0(STEM_MAP_BEARING, STEM_MAP_DISTANCE) != paste0(bearing_ref, distance_ref),
                ':='(STEM_MAP_BEARING = bearing_ref,
                     STEM_MAP_DISTANCE = distance_ref,
                     STEM_MAP_EDIT = "Corrected with last available stem mapping")]
  treemsmts_rep[,':='(visit_ref = NULL,
                      bearing_ref = NULL,
                      distance_ref = NULL)]

  # for drop trees,
  # as discussed with Dan on 2023-10-19, when a treemsmt is marked as dropped
  # the msmts before and equal to this msmt will be marked as out of plot for compilation
  # so that these msmts will not be included into tally but for age, if age is available
  droppedmsmt <- treemsmts_rep[MEASUREMENT_ANOMALY_CODE == "D",
                               .(lastvisit = max(VISIT_NUMBER)),
                               by = "unitreeid"]
  treemsmts_rep <- merge(treemsmts_rep,
                         droppedmsmt,
                         by = "unitreeid",
                         all.x = TRUE)
  treemsmts_rep[VISIT_NUMBER <= lastvisit &
                  (OUT_OF_PLOT_IND == "N" | is.na(OUT_OF_PLOT_IND)),
                ':='(OUT_OF_PLOT_IND = "Y",
                     OUT_OF_PLOT_EDIT = "Corrected to Y due to dropped")]
  treemsmts_rep[, lastvisit := NULL]

  if(compilationType == "nonPSP"){
    # correct cmi walkthrough code based on communication with Dan on 2023-10-12
    trees_walkthrough <- unique(treemsmts_rep[!is.na(CMI_WALKTHROUGH_CODE),]$unitreeid)

    cmi_walkthrough <- treemsmts_rep[unitreeid %in% trees_walkthrough,
                                     .(unitreeid,
                                       VISIT_NUMBER,
                                       CMI_WALKTHROUGH_CODE)]

    cmi_walkthrough[, lastvisit := max(VISIT_NUMBER),
                    by = "unitreeid"]
    cmi_walkthrough <- cmi_walkthrough[VISIT_NUMBER == lastvisit,
                                       .(unitreeid,
                                         visit_ref = VISIT_NUMBER,
                                         walkth_ref = CMI_WALKTHROUGH_CODE)]
    treemsmts_rep <- merge(treemsmts_rep,
                           cmi_walkthrough,
                           by = "unitreeid",
                           all.x = TRUE)
    treemsmts_rep[VISIT_NUMBER < visit_ref &
                    (CMI_WALKTHROUGH_CODE != walkth_ref |
                       is.na(CMI_WALKTHROUGH_CODE)),
                  ':='(CMI_WALKTHROUGH_CODE = walkth_ref,
                       CMI_WALKTHROUGH_EDIT = "Corrected based on next msmt")]
    treemsmts_rep[,':='(visit_ref = NULL,
                        walkth_ref = NULL)]
    rm(trees_walkthrough, cmi_walkthrough)
  } else {
    treemsmts_rep[, CMI_WALKTHROUGH_EDIT := as.character(NA)]
  }
  firstdf <- treemsmts_rep[TREE_EXTANT_CODE == "D" &
                             TREE_STANCE_CODE == "F",
                           .(firstdf = min(VISIT_NUMBER)),
                           by = "unitreeid"]
  treemsmts_rep <- merge(treemsmts_rep,
                         firstdf,
                         by = "unitreeid",
                         all.x = TRUE)
  treemsmts_rep <- treemsmts_rep[VISIT_NUMBER <= firstdf | is.na(firstdf),]
  treemsmts_rep[TREE_EXTANT_CODE == "D" &
                  TREE_STANCE_CODE == "F",
                STOP := "S_DF"]
  treemsmts_rep[, firstdf := NULL]
  rm(firstdf)
  gc()

  # not found, Harvest and dropped trees
  NHDtrees <- unique(treemsmts_rep[MEASUREMENT_ANOMALY_CODE %in% c("N", "H", "D")]$unitreeid)
  treemsmts_rep_bad <- treemsmts_rep[(unitreeid %in% NHDtrees),
                                     .(unitreeid, MEASUREMENT_ANOMALY_CODE, VISIT_NUMBER)]
  treemsmts_rep_bad <- treemsmts_rep_bad[order(unitreeid, VISIT_NUMBER)]
  treemsmts_rep_bad[, visit_prev := shift(VISIT_NUMBER, type = "lag"),
                    by = "unitreeid"]

  treemsmts_rep_bad <- treemsmts_rep_bad[MEASUREMENT_ANOMALY_CODE %in% c("N", "H", "D"),
                                         .(unitreeid, MEASUREMENT_ANOMALY_CODE,
                                           VISIT_NUMBER, visit_prev)]
  treemsmts_rep_bad_last <- treemsmts_rep_bad[,.SD[which.min(VISIT_NUMBER)],
                                              by = "unitreeid"]
  setnames(treemsmts_rep_bad_last,
           c("MEASUREMENT_ANOMALY_CODE", "VISIT_NUMBER"),
           c("ANOMALY_CODE_last", "visit_last"))
  treemsmts_rep <- merge(treemsmts_rep,
                         treemsmts_rep_bad_last,
                         by = "unitreeid",
                         all.x = TRUE)
  # retain three sets of data
  # 1) unstopped trees, 2) stop trees before N,H,D, 3) back to tally trees after dropped
  treemsmts_rep <- treemsmts_rep[is.na(visit_last) | # this is unstopped trees
                                   VISIT_NUMBER < visit_last | # this is for stopped trees before N H D
                                   (VISIT_NUMBER > visit_last & ANOMALY_CODE_last == "D"), # this is for back to tally trees
  ]
  treemsmts_rep[VISIT_NUMBER == visit_prev,
                STOP := paste0("S_", ANOMALY_CODE_last)]
  treemsmts_rep[,':='(ANOMALY_CODE_last = NULL,
                      visit_last = NULL,
                      visit_prev = NULL)]
  rm(treemsmts_rep_bad, treemsmts_rep_bad_last, NHDtrees)
  gc()
  # 1. correct lv code
  # based on tree measurement comments
  treemsmts_rep[, TREE_MEASUREMENT_COMMENT := tolower(TREE_MEASUREMENT_COMMENT)]
  treemsmts_rep[grepl("live", TREE_MEASUREMENT_COMMENT) |
                  grepl("not dead", TREE_MEASUREMENT_COMMENT) |
                  grepl("not cut", TREE_MEASUREMENT_COMMENT) |
                  grepl("found tagged with no data.", TREE_MEASUREMENT_COMMENT) |
                  grepl("dying", TREE_MEASUREMENT_COMMENT) |
                  grepl("found w/ no data.", TREE_MEASUREMENT_COMMENT) |
                  grepl("tagged with no prev. measurement data.", TREE_MEASUREMENT_COMMENT) |
                  grepl("found w/no data.", TREE_MEASUREMENT_COMMENT),
                lvd_comment := "L"]

  treemsmts_rep[lvd_comment == "L" &
                  TREE_EXTANT_CODE == "D",
                ':='(TREE_EXTANT_CODE = "L",
                     LVD_EDIT = "Back to L based on tree measurement comments")] # any msmt before this visit and lv_d == "D" need to be corrected


  ## to deal with missing lv codes
  treemsmt_lastlive <- treemsmts_rep[TREE_EXTANT_CODE == "L",
                                     .(vst_last_live = max(VISIT_NUMBER)),
                                     by = "unitreeid"]
  treemsmts_rep <- merge(treemsmts_rep,
                         treemsmt_lastlive,
                         by = c("unitreeid"),
                         all.x = TRUE)
  treemsmts_rep[is.na(TREE_EXTANT_CODE) &
                  VISIT_NUMBER < vst_last_live,
                ':='(TREE_EXTANT_CODE = "L",
                     LVD_EDIT = "Missing, added L as found alive later")]
  treemsmts_rep[is.na(TREE_EXTANT_CODE) &
                  VISIT_NUMBER > vst_last_live,
                ':='(TREE_EXTANT_CODE = "D",
                     LVD_EDIT = "Missing at tail, added D")]
  treemsmts_rep[is.na(TREE_EXTANT_CODE) &
                  is.na(vst_last_live),
                ':='(TREE_EXTANT_CODE = "D",
                     LVD_EDIT = "Missing, added D as found dead earlier")]
  rm(treemsmt_lastlive)
  gc()

  ## correction 3, if a tree was dead at current msmt but live in the later msmts,
  # change this tree for current msmt as L
  treemsmts_rep[VISIT_NUMBER < vst_last_live &
                  TREE_EXTANT_CODE == "D",
                ':='(TREE_EXTANT_CODE = "L",
                     LVD_EDIT = "Corrected to L as found alive later")]
  treemsmts_rep[, ':='(vst_last_live = NULL,
                       lvd_comment = NULL)]


  # correction 4. if the last msmt do not have diameter information, assign lvd as D
  # realized that there are a lot trees do not have diameter at any msmt
  # so, to fix that, add one more condition: if a tree have diameter measured at previous msmts
  treemsmts_withdiameter <- treemsmts_rep[DIAMETER > 0,.(unitreeid, VISIT_NUMBER,
                                                         DIAMETER, DIAMETER_MEASMT_HEIGHT,
                                                         TREE_EXTANT_CODE)]
  treemsmts_withdiameter[, lastvisit := max(VISIT_NUMBER), by = "unitreeid"]
  treemsmts_withdiameter <- treemsmts_withdiameter[lastvisit == VISIT_NUMBER,
                                                   .(unitreeid, diameter_visit_last = VISIT_NUMBER,
                                                     diameter_last = DIAMETER,
                                                     diameter_ht_last = DIAMETER_MEASMT_HEIGHT)]
  treemsmts_rep <- merge(treemsmts_rep,
                         treemsmts_withdiameter,
                         by = "unitreeid",
                         all.x = TRUE)
  rm(treemsmts_withdiameter)
  gc()


  # lv_d = L, but without diameter msmt
  # and inplot tree
  # *identify if tree at very last measurement is missing a diameter measurement, then assume it is a dead fallen tree;
  # correction a, if this tree is live, set to dead, use last diameter as current diameter
  treemsmts_rep[VISIT_NUMBER > diameter_visit_last & is.na(DIAMETER) &
                  TREE_EXTANT_CODE == "L" & OUT_OF_PLOT_IND == "N",
                ':='(TREE_EXTANT_CODE = "D",
                     TREE_CLASS_CODE = 4,
                     DIAMETER = diameter_last,
                     DIAMETER_MEASMT_HEIGHT = diameter_ht_last,
                     LVD_EDIT = "Change to D as missing diameter msmt")]
  treemsmts_rep[,':='(diameter_visit_last = NULL,
                      diameter_last = NULL,
                      diameter_ht_last = NULL)]

  # for PSP only
  if(compilationType == "PSP"){
    # correction c,
    # *check for outlier ht/dbh ratios;
    # *if measured heights are more than 3X DBH, then assume height decimal placement error, divide by 10;
    # if ld_fill(c) in ("L","I","V") and htmeas_fill(c) > 0 and dbh_fill(c) > 0 then do;
    # if htmeas_fill(c)/dbh_fill(c) >= 3 then do;
    # htmeas_fill(c) = htmeas_fill(c) / 10;
    # tree_err11 = "_extrhtdbh";
    # end;
    # end;
    treemsmts_rep[LENGTH/DIAMETER >= 3,
                  ':='(LENGTH = LENGTH/10,
                       HEIGHT_EDIT = "Abnomal HEIGHT-DBH ratio, divided by 10")]
  } else {
    treemsmts_rep[SITE_IDENTIFIER == 3001342 &
                    TREE_NUMBER == 68 &
                    VISIT_NUMBER == 1,
                  ':='(LENGTH = 4.7,
                       HEIGHT_EDIT = "Best guess")] # best guess
    treemsmts_tmp[SITE_IDENTIFIER == 6006671 &
                    TREE_NUMBER == 8 &
                    VISIT_NUMBER == 1 &
                    PLOT == "I",
                  ':='(LENGTH = 3.8,
                       HEIGHT_EDIT = "Best guess")] # best guess
  }
  # correction d, if this tree is dead, and diameter is missing
  # and previous it is live with diameter
  treemsmts_rep <- treemsmts_rep[order(unitreeid, VISIT_NUMBER),]
  treemsmts_rep[, ':='(lvd_prev = shift(TREE_EXTANT_CODE, type = "lag"),
                       diam_prev = shift(DIAMETER, type = "lag"),
                       diam_ht_prev = shift(DIAMETER_MEASMT_HEIGHT, type = "lag")),
                by = "unitreeid"]

  treemsmts_rep[is.na(DIAMETER) & diam_prev > 0 & lvd_prev == "L" &
                  TREE_EXTANT_CODE == "D",
                ':='(DIAMETER = diam_prev,
                     DIAMETER_MEASMT_HEIGHT = diam_ht_prev,
                     DIAMETER_EDIT = "Diameter assinged based on previous msmt")]
  treemsmts_rep[,':='(lvd_prev = NULL,
                      diam_prev = NULL,
                      diam_ht_prev = NULL)]
  gc()
  # 2. add missing observation in between two msmt
  msmts_tree <- unique(treemsmts_rep[,.(unitreeid, SITE_IDENTIFIER, VISIT_NUMBER,
                                        intreemsmt = TRUE)])
  msmts_tree_range <- msmts_tree[, .(visit_first = min(VISIT_NUMBER),
                                     visit_last = max(VISIT_NUMBER)),
                                 by = "unitreeid"]
  sitevisits <- sitevisits[VISIT_TYPE == "REP",]
  sitevisits <- sitevisits[order(SITE_IDENTIFIER, VISIT_NUMBER),]
  sitevisits[, ':='(VISIT_NUMBER_next = shift(VISIT_NUMBER, type = "lead"),
                    VISIT_NUMBER_prev = shift(VISIT_NUMBER, type = "lag")),
             by = "SITE_IDENTIFIER"]
  msmts_tree_full <- unique(merge(msmts_tree[,.(SITE_IDENTIFIER, unitreeid)],
                                  sitevisits,
                                  by = c("SITE_IDENTIFIER"),
                                  allow.cartesian = TRUE))
  msmts_tree_full <- merge(msmts_tree_full,
                           msmts_tree,
                           by = c("SITE_IDENTIFIER", "unitreeid", "VISIT_NUMBER"),
                           all.x = TRUE)
  msmts_tree_full <- merge(msmts_tree_full,
                           msmts_tree_range,
                           by = "unitreeid",
                           all.x = TRUE)
  rm(msmts_tree, msmts_tree_range)
  msmt_missing_inbetween <- msmts_tree_full[is.na(intreemsmt) & VISIT_NUMBER > visit_first &
                                              VISIT_NUMBER < visit_last,
                                            .(unitreeid, VISIT_NUMBER_crt = VISIT_NUMBER, VISIT_NUMBER_next, VISIT_NUMBER_prev)]
  rm(msmts_tree_full)
  gc()

  # use all the same information from next msmt for a temporary solution
  msmt_missing_inbetween[, VISIT_NUMBER := VISIT_NUMBER_next]
  msmt_missing_inbetween_last <- msmt_missing_inbetween[,.(visit_missing_last = max(VISIT_NUMBER_crt)),
                                                        by = "unitreeid"]
  missing_inbetween_next <- merge(treemsmts_rep,
                                  msmt_missing_inbetween_last,
                                  by = c("unitreeid"),
                                  all.x = TRUE)
  missing_inbetween_next <- missing_inbetween_next[VISIT_NUMBER > visit_missing_last,]
  missing_inbetween_next[, visit_next := min(VISIT_NUMBER), by = "unitreeid"]
  missing_inbetween_next <- missing_inbetween_next[VISIT_NUMBER == visit_next,]
  missing_inbetween_next[,':='(visit_missing_last = NULL,
                               visit_next = NULL,
                               VISIT_NUMBER = NULL)]
  treemsmts_missing_inbetween <- merge(msmt_missing_inbetween[,.(unitreeid, VISIT_NUMBER = VISIT_NUMBER_crt)],
                                       missing_inbetween_next,
                                       by = "unitreeid",
                                       all.x = TRUE)
  rm(msmt_missing_inbetween, missing_inbetween_next)
  treemsmts_missing_inbetween[,':='(DIAMETER = NA,
                                    LENGTH = NA,
                                    BORING_AGE = NA,
                                    AGE_CORE_MISSED_YEARS_FIELD = NA,
                                    MICROSCOPE_AGE = NA,
                                    AGE_CORE_MISSED_YEARS_LAB = NA,
                                    AGE_CORRECTION = NA,
                                    TOTAL_AGE = NA,
                                    RADIAL_INCREMENT_LAST_10_YR = NA,
                                    RADIAL_INCREMENT_LAST_5_YR = NA,
                                    RADIAL_INCREMENT_LAST_20_YR = NA,
                                    PRORATE_LENGTH = NA,
                                    AGE_MEASURE_CODE = NA,
                                    AGE_MEASMT_HEIGHT = NA,
                                    MSMT_MISSING_EDIT = "Missing in between, added")]
  treemsmts_rep <- rbind(treemsmts_rep, treemsmts_missing_inbetween,
                         fill = TRUE)
  rm(treemsmts_missing_inbetween)
  gc()

  # 3. add missing observation at tail
  treemsmts_rep[, visit_tree_last := max(VISIT_NUMBER),
                by = "unitreeid"]
  sitevisits_last <- sitevisits[,.(visit_site_last = max(VISIT_NUMBER)),
                                by = "SITE_IDENTIFIER"]
  treemsmts_rep <- merge(treemsmts_rep, sitevisits_last,
                         by = "SITE_IDENTIFIER",
                         all.x = TRUE)
  # tree's last visit have not reached the site's last visit, and
  # there is no stop sign
  # lv_d = L at the last visit of this tree and with diameter msmt
  treemsmts_missing_tail <- treemsmts_rep[visit_tree_last == VISIT_NUMBER &
                                            visit_site_last != visit_tree_last &
                                            is.na(STOP),]
  treemsmts_missing_tail <- merge(treemsmts_missing_tail,
                                  sitevisits[,.(SITE_IDENTIFIER, VISIT_NUMBER, VISIT_NUMBER_next)],
                                  by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                                  all.x = TRUE)
  treemsmts_missing_tail[, ':='(VISIT_NUMBER = VISIT_NUMBER_next, # add next visit
                                TREE_EXTANT_CODE = "D", # force to dead
                                TREE_CLASS_CODE = 4,
                                LENGTH = NA,
                                MSMT_MISSING_EDIT = "Missing at tail, added",
                                DIAMETER_EDIT = "Diameter assinged based on previous msmt")]

  treemsmts_missing_tail[,':='(BORING_AGE = NA,
                               AGE_CORE_MISSED_YEARS_FIELD = NA,
                               MICROSCOPE_AGE = NA,
                               AGE_CORE_MISSED_YEARS_LAB = NA,
                               AGE_CORRECTION = NA,
                               TOTAL_AGE = NA,
                               RADIAL_INCREMENT_LAST_10_YR = NA,
                               RADIAL_INCREMENT_LAST_5_YR = NA,
                               RADIAL_INCREMENT_LAST_20_YR = NA,
                               PRORATE_LENGTH = NA,
                               AGE_MEASURE_CODE = NA,
                               AGE_MEASMT_HEIGHT = NA)]
  treemsmts_rep <- rbind(treemsmts_rep,
                         treemsmts_missing_tail,
                         fill = TRUE)
  rm(treemsmts_missing_tail)
  gc()
  treemsmts_rep[, ':='(visit_tree_last = NULL,
                       visit_site_last = NULL,
                       VISIT_NUMBER_next = NULL)]
  # for missing diameter msmt height
  treemsmts_rep[is.na(DIAMETER_MEASMT_HEIGHT) &
                  !is.na(DIAMETER),
                ':='(DIAMETER_MEASMT_HEIGHT = 1.3,
                     DIAM_MSMT_HT_EDIT = "Missing diameter msmt height, 1.3 added")]

  treemsmts_rep_missingD <- treemsmts_rep[is.na(DIAMETER),
                                          .(unitreeid, VISIT_NUMBER)]
  treemsmts_rep_withD <- treemsmts_rep[!is.na(DIAMETER) & unitreeid %in% treemsmts_rep_missingD$unitreeid,
                                       .(unitreeid, VISIT_NUMBER_withD = VISIT_NUMBER,
                                         DIAMETER)]
  treemsmts_rep_withD <- merge(treemsmts_rep_withD,
                               treemsmts_rep_missingD,
                               by = "unitreeid")
  treemsmts_rep_withD_prev <- treemsmts_rep_withD[VISIT_NUMBER_withD < VISIT_NUMBER,
                                                  .(unitreeid, VISIT_NUMBER_withD,
                                                    VISIT_NUMBER,
                                                    DIAMETER_prev = DIAMETER)]
  treemsmts_rep_withD_prev <- treemsmts_rep_withD_prev[, lastvst := max(VISIT_NUMBER_withD),
                                                       by = "unitreeid"]
  treemsmts_rep_withD_prev <- treemsmts_rep_withD_prev[VISIT_NUMBER_withD == lastvst,
                                                       .(unitreeid, VISIT_NUMBER, DIAMETER_prev)]

  treemsmts_rep_withD_next <- treemsmts_rep_withD[VISIT_NUMBER_withD > VISIT_NUMBER,
                                                  .(unitreeid, VISIT_NUMBER_withD,
                                                    VISIT_NUMBER,
                                                    DIAMETER_next = DIAMETER)]
  treemsmts_rep_withD_next <- treemsmts_rep_withD_next[, fstvst := min(VISIT_NUMBER_withD),
                                                       by = "unitreeid"]
  treemsmts_rep_withD_next <- treemsmts_rep_withD_next[VISIT_NUMBER_withD == fstvst,
                                                       .(unitreeid, VISIT_NUMBER, DIAMETER_next)]

  treemsmts_rep_for_missingD <- merge(treemsmts_rep_withD_prev,
                                      treemsmts_rep_withD_next,
                                      by = c("unitreeid", "VISIT_NUMBER"),
                                      all = TRUE)
  treemsmts_rep_for_missingD[!is.na(DIAMETER_prev) &
                               !is.na(DIAMETER_next),
                             ':='(DIAMETER_final = (DIAMETER_prev + DIAMETER_next)/2,
                                  DIAMETER_EDIT_final = "Missing, assigned based on mean of prev and next diameters")]
  treemsmts_rep_for_missingD[!is.na(DIAMETER_prev) &
                               is.na(DIAMETER_next),
                             ':='(DIAMETER_final = DIAMETER_prev,
                                  DIAMETER_EDIT_final = "Missing, assinged based on previous msmt")]
  treemsmts_rep_for_missingD[is.na(DIAMETER_prev) &
                               !is.na(DIAMETER_next),
                             ':='(DIAMETER_final = DIAMETER_next,
                                  DIAMETER_EDIT_final = "Missing, assinged based on next msmt")]

  treemsmts_rep <- merge(treemsmts_rep,
                         treemsmts_rep_for_missingD[,.(unitreeid, VISIT_NUMBER,
                                                       DIAMETER_final,
                                                       DIAMETER_EDIT_final)],
                         by = c("unitreeid", "VISIT_NUMBER"),
                         all.x = TRUE)
  treemsmts_rep[!is.na(DIAMETER_final),
                ':='(DIAMETER = DIAMETER_final,
                     DIAMETER_EDIT = DIAMETER_EDIT_final,
                     DIAMETER_MEASMT_HEIGHT = 1.3)]
  treemsmts_rep[,':='(DIAMETER_final = NULL,
                      DIAMETER_EDIT_final = NULL)]
  treemsmts_rep[is.na(DIAMETER) & DIAMETER_MEASMT_HEIGHT == 1.3,
                DIAMETER_EDIT := "Missing, no clue to assign"]
  rm(treemsmts_rep_missingD, treemsmts_rep_withD, treemsmts_rep_withD_prev,
     treemsmts_rep_withD_next, treemsmts_rep_for_missingD)
  ## 4. if a tree was broken top tree, this tree is always be btop trees in sequential msmt
  ### force trees that have length of 1.4 or less as broken top trees
  treemsmts_rep[LENGTH %<=% 1.4 & BROKEN_TOP_IND == "N" &
                  DIAMETER >= 3,
                ':='(BROKEN_TOP_IND = "Y",
                     BTOP_EDIT = "Corrected to Y as height <= 1.4 and DBH >= 3")]
  tree_btop <- treemsmts_rep[BROKEN_TOP_IND == "Y",
                             .(unitreeid, VISIT_NUMBER, BROKEN_TOP_IND)]
  if(nrow(tree_btop) > 0){
    tree_btop[, visit_ref := min(VISIT_NUMBER), by = "unitreeid"]
    tree_btop <- tree_btop[visit_ref == VISIT_NUMBER,
                           .(unitreeid, visit_ref, btop_ref = BROKEN_TOP_IND)]
    treemsmts_rep <- merge(treemsmts_rep,
                           tree_btop,
                           by = "unitreeid",
                           all.x = TRUE)
    treemsmts_rep[VISIT_NUMBER > visit_ref &
                    BROKEN_TOP_IND %in% c("N", NA),
                  ':='(BROKEN_TOP_IND = "Y",
                       BTOP_EDIT = "Change to Y based on previous msmt")]
    treemsmts_rep[, ':='(btop_ref = NULL,
                         visit_ref = NULL)]
  }

  if(compilationType == "PSP"){
    ## this is for PSP only
    # 5. *fill in crown class code moving forward;
    # if compress(index
    #             _fill(k)) ~= "" and compress(crncls_fill(k+1)) = "" then do;
    # crncls_fill(k+1) = crncls_fill(k);
    # end;
    ## first bring tree suppression code
    treemsmts_rep[TREE_SUPPRESSION_CODE == "Y" &
                    CROWN_CLASS_CODE != "S",
                  ':='(CROWN_CLASS_CODE = "S",
                       CRCL_EDIT = "Corrected to S based on tree suppression code")]
    treemsmts_rep[TREE_SUPPRESSION_CODE == "Y" &
                    is.na(CROWN_CLASS_CODE),
                  ':='(CROWN_CLASS_CODE = "S",
                       CRCL_EDIT = "Missing, assigned to S based on tree suppression code")]

    treeid_no_crcl <- unique(treemsmts_rep[is.na(CROWN_CLASS_CODE)]$unitreeid)
    treemsmts_good <- treemsmts_rep[!(unitreeid %in% treeid_no_crcl),]
    treemsmts_bad <- treemsmts_rep[(unitreeid %in% treeid_no_crcl),]
    treemsmts_bad <- treemsmts_bad[order(unitreeid, VISIT_NUMBER),]


    treemsmts_bad[, crcl_prev := shift(CROWN_CLASS_CODE, type = "lag"),
                  by = "unitreeid"]
    while (nrow(treemsmts_bad[is.na(CROWN_CLASS_CODE) & !is.na(crcl_prev),]) > 0) {
      treemsmts_bad[is.na(CROWN_CLASS_CODE) & !is.na(crcl_prev),
                    ':='(CROWN_CLASS_CODE = crcl_prev,
                         CRCL_EDIT = "Missing, assigned based on previous msmt")]
      treemsmts_bad[, crcl_prev := shift(CROWN_CLASS_CODE, type = "lag"),
                    by = "unitreeid"]
    }
    treemsmts_bad[, crcl_prev := NULL]
    # is still missing fill it with next available crow class code
    treelist_withcrcl <- treemsmts_bad[!is.na(CROWN_CLASS_CODE),
                                       .(unitreeid,
                                         VISIT_NUMBER, CROWN_CLASS_CODE)]
    if(nrow(treelist_withcrcl) > 0){
      treelist_withcrcl[, firstVisit := min(VISIT_NUMBER),
                        by = c("unitreeid")]
      treelist_withcrcl <- treelist_withcrcl[VISIT_NUMBER == firstVisit,
                                             .(unitreeid,
                                               visit_ref = VISIT_NUMBER,
                                               CROWN_CLASS_CODE_ref = CROWN_CLASS_CODE)]
      treemsmts_bad <- merge(treemsmts_bad,
                             treelist_withcrcl,
                             by = c("unitreeid"),
                             all.x = TRUE)
      treemsmts_bad[VISIT_NUMBER < visit_ref & is.na(CROWN_CLASS_CODE),
                    ':='(CROWN_CLASS_CODE = CROWN_CLASS_CODE_ref,
                         CRCL_EDIT = "Missing, assigned based on next available crown CR_CL")]
      treemsmts_bad[,':='(visit_ref = NULL,
                          CROWN_CLASS_CODE_ref = NULL)]

    }
    treemsmts_bad[is.na(CROWN_CLASS_CODE),
                  CRCL_EDIT := "Missing, no clue to add"]
    treemsmts_rep <- rbind(treemsmts_good, treemsmts_bad, fill = TRUE)
    rm(treemsmts_good, treemsmts_bad, treelist_withcrcl)
  } else {
    treemsmts_rep[, CRCL_EDIT := as.character(NA)]
  }

  # 6. correct species based on last observed species
  sp_last <- treemsmts_rep[,.(unitreeid,
                              VISIT_NUMBER, TREE_SPECIES_CODE)]
  sp_last[, lastvisit := max(VISIT_NUMBER),
          by = "unitreeid"]
  sp_last <- sp_last[VISIT_NUMBER == lastvisit,
                     .(unitreeid, sp_last = TREE_SPECIES_CODE)]
  treemsmts_rep <- merge(treemsmts_rep,
                         sp_last,
                         by = "unitreeid",
                         all.x = TRUE)
  treemsmts_rep[TREE_SPECIES_CODE != sp_last,
                ':='(TREE_SPECIES_CODE = sp_last,
                     SP_EDIT = "Species changed based on last msmt")]
  rm(sp_last)
  gc()
  treemsmts_rep[, sp_last := NULL]
  treemsmts_rep[TREE_SPECIES_CODE %in% c("XH", "Z", "ZH"),
                TREE_SPECIES_CODE := "X"]


  if(compilationType == "PSP"){

    # 8. fill missing ht suit
    treeid_no_suitht <- unique(treemsmts_rep[is.na(SUITABLE_FOR_HEIGHT_IND)]$unitreeid)
    treemsmts_good <- treemsmts_rep[!(unitreeid %in% treeid_no_suitht),]
    treemsmts_bad <- treemsmts_rep[(unitreeid %in% treeid_no_suitht),]
    treemsmts_bad <- treemsmts_bad[order(unitreeid, VISIT_NUMBER),]
    treemsmts_bad[, ht_suit_prev := shift(SUITABLE_FOR_HEIGHT_IND, type = "lag"),
                  by = "unitreeid"]
    while (nrow(treemsmts_bad[is.na(SUITABLE_FOR_HEIGHT_IND) & !is.na(ht_suit_prev),]) > 0) {
      treemsmts_bad[is.na(SUITABLE_FOR_HEIGHT_IND) & !is.na(ht_suit_prev),
                    ':='(SUITABLE_FOR_HEIGHT_IND = ht_suit_prev,
                         SUIT_HT_EDIT = "Missing, assigned based on previous ht_suit")]
      treemsmts_bad[, ht_suit_prev := shift(SUITABLE_FOR_HEIGHT_IND, type = "lag"),
                    by = "unitreeid"]
    }
    treemsmts_bad[, ht_suit_prev := NULL]
    treemsmts_rep <- rbind(treemsmts_good, treemsmts_bad, fill = TRUE)
    rm(treemsmts_good, treemsmts_bad)

    # *step 1 to identify trees that should be dropped, ie., tagged trees subsequently determined to be outside;
    # *plot perimeter, and therefore should be dropped.  reclassify these trees as tree class 99, so they can be dropped from tree list;
    # *across all measurements (subsequent process in age2);
    # if missed_dropped_fallen_cd = "D" then do;
    # tr_class = "9";
    # end;
    # else if missed_dropped_fallen_cd = "F" then do;
    # tr_class = "4";
    # if compress(dead_standing_or_down) = "" then dead_standing_or_down = "F";
    # end;
    treemsmts_rep[VETERAN_IND == "Y" &
                    RESIDUAL_IND == "N",
                  ':='(RESIDUAL_IND = "Y",
                       RESIDUAL_EDIT = "Corrected to Y based on veteran_ind")]
    treemsmts_rep[VETERAN_IND == "Y" &
                    is.na(RESIDUAL_IND),
                  ':='(RESIDUAL_IND = "Y",
                       RESIDUAL_EDIT = "Missing, assigned to Y based on veteran_ind")]
    treemsmts_rep[RESIDUAL_IND == "Y" &
                    is.na(SUITABLE_FOR_HEIGHT_IND),
                  ':='(SUITABLE_FOR_HEIGHT_IND = "N",
                       SUIT_HT_EDIT = "Missing, N assigned due to residual tree")]
    treemsmts_rep[RESIDUAL_IND == "Y" &
                    SUITABLE_FOR_HEIGHT_IND == "Y",
                  ':='(SUITABLE_FOR_HEIGHT_IND = "N",
                       SUIT_HT_EDIT = "Corrected to N due to residual tree")]
    treemsmts_rep[TREE_EXTANT_CODE == "D" &
                    is.na(SUITABLE_FOR_HEIGHT_IND),
                  ':='(SUITABLE_FOR_HEIGHT_IND = "N",
                       SUIT_HT_EDIT = "Missing, N assigned due to dead tree")]
    treemsmts_rep[TREE_EXTANT_CODE == "D" &
                    SUITABLE_FOR_HEIGHT_IND == "Y",
                  ':='(SUITABLE_FOR_HEIGHT_IND = "N",
                       SUIT_HT_EDIT = "Corrected to N due to dead tree")]
    treemsmts_rep[BROKEN_TOP_IND == "Y" &
                    is.na(SUITABLE_FOR_HEIGHT_IND),
                  ':='(SUITABLE_FOR_HEIGHT_IND = "N",
                       SUIT_HT_EDIT = "Missing, N assigned due to btop tree")]
    treemsmts_rep[BROKEN_TOP_IND == "Y" &
                    SUITABLE_FOR_HEIGHT_IND == "Y",
                  ':='(SUITABLE_FOR_HEIGHT_IND = "N",
                       SUIT_HT_EDIT = "Corrected to N due to btop tree")]
    treemsmts_rep[TREE_STANCE_CODE == "F" &
                    SUITABLE_FOR_HEIGHT_IND == "Y",
                  ':='(SUITABLE_FOR_HEIGHT_IND = "N",
                       SUIT_HT_EDIT = "Corrected to N due to fallen tree")]
    treemsmts_rep[TREE_STANCE_CODE == "F" &
                    is.na(SUITABLE_FOR_HEIGHT_IND),
                  ':='(SUITABLE_FOR_HEIGHT_IND = "N",
                       SUIT_HT_EDIT = "Missing, assigned N due to fallen tree")]
    treemsmts_rep[is.na(SUITABLE_FOR_HEIGHT_IND),
                  ':='(SUITABLE_FOR_HEIGHT_IND = "Y",
                       SUIT_HT_EDIT = "Missing, assigned Y due to no sign of unsuit")]
    # *define a site tree suitability flag, based on a selection of damage codes, suitability for taking heights;
    # *cores with pith reached or pith estimated, and assessment of age suppression;
    # *for site tree screening, the sisuitability flag is used, modified under age2 dataset based on additional criteria;
    # *conversation with khardy 2013-mar-26, ;
    # if top_damage = "Y" or tree_suitable_for_ht = "N" or substr(pith_code,1,1) in ("R") or suppression_ind in ("Y") or tr_class in ("5","6") then do;
    # *for this definition, if not suitable for si, then also not suitable for ht, if missing;
    # sitree_suit = "N";
    # if tree_suitable_for_ht = "" then tree_suitable_for_ht = "N";
    # end;
    # else do;
    # *for this definition, if suitable for si, and htsuit flag is missing, then assume also suitable for ht;
    # sitree_suit = "Y";
    # if tree_suitable_for_ht = "" then tree_suitable_for_ht = "Y";
    # end;
    treemsmts_rep[SUITABLE_FOR_HEIGHT_IND == "N" &
                    SUITABLE_FOR_SITE_INDEX_IND == "Y",
                  ':='(SUITABLE_FOR_SITE_INDEX_IND = "N",
                       SUIT_SI_EDIT = "Corrected to N as not suit for HT")]
    treemsmts_rep[SUITABLE_FOR_HEIGHT_IND == "N" &
                    is.na(SUITABLE_FOR_SITE_INDEX_IND),
                  ':='(SUITABLE_FOR_SITE_INDEX_IND = "N",
                       SUIT_SI_EDIT = "Missing, assigned to N as not suit for HT")]

    treemsmts_rep[(substr(AGE_MEASURE_CODE, 1, 1) %in% c("R", "C") |
                     SUITABLE_FOR_AGE_IND == "N" |
                     AGE_REPRESENTATIVE_IND == "N") &
                    is.na(SUITABLE_FOR_SITE_INDEX_IND),
                  ':='(SUITABLE_FOR_SITE_INDEX_IND = "N",
                       SUIT_SI_EDIT = "Missing, assigned to N as not suit for age")]

    treemsmts_rep[(substr(AGE_MEASURE_CODE, 1, 1) %in% c("R", "C") |
                     SUITABLE_FOR_AGE_IND == "N" |
                     AGE_REPRESENTATIVE_IND == "N") &
                    SUITABLE_FOR_SITE_INDEX_IND == "Y",
                  ':='(SUITABLE_FOR_SITE_INDEX_IND = "N",
                       SUIT_SI_EDIT = "Corrected to N as not suit for age")]
    treemsmts_rep[(CROWN_CLASS_CODE %in% c("S", "I") |
                     TREE_SUPPRESSION_CODE == "Y") &
                    SUITABLE_FOR_SITE_INDEX_IND == "Y",
                  ':='(SUITABLE_FOR_SITE_INDEX_IND = "N",
                       SUIT_SI_EDIT = "Corrected to N as not dominant or codominant tree")]
    treemsmts_rep[(CROWN_CLASS_CODE %in% c("S", "I") |
                     TREE_SUPPRESSION_CODE == "Y") &
                    is.na(SUITABLE_FOR_SITE_INDEX_IND),
                  ':='(SUITABLE_FOR_SITE_INDEX_IND = "N",
                       SUIT_SI_EDIT = "Missing, assigned to N as not dominant or codominant tree")]
    treemsmts_rep[is.na(SUITABLE_FOR_SITE_INDEX_IND),
                  ':='(SUITABLE_FOR_SITE_INDEX_IND = "Y",
                       SUIT_SI_EDIT = "Missing, assigned to Y as no clue")]
  } else {
    # for nonpsp
    treemsmts_rep[,':='(SUIT_HT_EDIT = as.character(NA),
                        SUIT_SI_EDIT = as.character(NA),
                        RESIDUAL_EDIT = as.character(NA))]
    # if a tree is a site tree and no diameter information
    # the broken top ind should be assigned with ""
    # pls refer the conversion with Dan on 2023-10-18
    treemsmts_rep[is.na(DIAMETER) &
                    (!is.na(AGE_MEASMT_HEIGHT) | !is.na(AGE_MEASURE_CODE)),
                  BROKEN_TOP_IND := ""]
  }
  treemsmts <- rbind(treemsmts_rep, treemsmts_tmp, fill = TRUE)
  treemsmts[,":="(VISIT_TYPE = NULL,
                  unitreeid = NULL)]
  return(treemsmts)
}
