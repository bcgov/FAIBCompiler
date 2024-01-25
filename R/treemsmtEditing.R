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
  firstdf <- treemsmts[TREE_EXTANT_CODE == "D" &
                         TREE_STANCE_CODE == "F",
                       .(firstdf = min(VISIT_NUMBER)),
                       by = "unitreeid"]
  treemsmts <- merge(treemsmts,
                     firstdf,
                     by = "unitreeid",
                     all.x = TRUE)
  treemsmts <- treemsmts[VISIT_NUMBER <= firstdf | is.na(firstdf),]
  treemsmts[TREE_EXTANT_CODE == "D" &
              TREE_STANCE_CODE == "F",
            STOP := "S_DF"]
  treemsmts[, firstdf := NULL]

  # not found, Harvest and dropped trees
  NHDtrees <- unique(treemsmts[MEASUREMENT_ANOMALY_CODE %in% c("N", "H", "D")]$unitreeid)
  treemsmts_bad <- treemsmts[(unitreeid %in% NHDtrees),
                             .(unitreeid, MEASUREMENT_ANOMALY_CODE, VISIT_NUMBER)]
  treemsmts_bad <- treemsmts_bad[order(unitreeid, VISIT_NUMBER)]
  treemsmts_bad[, visit_prev := shift(VISIT_NUMBER, type = "lag"),
                by = "unitreeid"]

  treemsmts_bad <- treemsmts_bad[MEASUREMENT_ANOMALY_CODE %in% c("N", "H", "D"),
                                 .(unitreeid, MEASUREMENT_ANOMALY_CODE,
                                   VISIT_NUMBER, visit_prev)]
  treemsmts_bad_last <- treemsmts_bad[,.SD[which.min(VISIT_NUMBER)],
                                      by = "unitreeid"]
  setnames(treemsmts_bad_last,
           c("MEASUREMENT_ANOMALY_CODE", "VISIT_NUMBER"),
           c("ANOMALY_CODE_last", "visit_last"))
  treemsmts <- merge(treemsmts,
                     treemsmts_bad_last,
                     by = "unitreeid",
                     all.x = TRUE)
  # retain three sets of data
  # 1) unstopped trees, 2) stop trees before N,H,D, 3) back to tally trees after dropped
  treemsmts <- treemsmts[is.na(visit_last) | # this is unstopped trees
                           VISIT_NUMBER < visit_last | # this is for stopped trees before N H D
                           (VISIT_NUMBER > visit_last & ANOMALY_CODE_last == "D"), # this is for back to tally trees
                         ]
  treemsmts[VISIT_NUMBER == visit_prev,
            STOP := paste0("S_", ANOMALY_CODE_last)]
  treemsmts[,':='(ANOMALY_CODE_last = NULL,
                  visit_last = NULL,
                  visit_prev = NULL)]


  # 1. correct lv code
  ## if missing lv codes, use next lvd to populate,
  trees_missinglvd <- treemsmts[is.na(TREE_EXTANT_CODE),
                                .(unitreeid, VISIT_NUMBER)]
  trees_missinglvd[, times := length(VISIT_NUMBER), by = "unitreeid"]
  trees_missinglvd_morethan1 <- trees_missinglvd[times > 1,
                                                 .(unitreeid, VISIT_NUMBER,
                                                   lvd_new = "D",
                                                   LVD_EDIT = "Missing, added dead as no lvd next")]
  trees_missinglvd_only1 <- trees_missinglvd[times == 1,
                                             .(unitreeid,
                                               VISIT_NUMBER = VISIT_NUMBER+1)]
  trees_missinglvd_only1 <- merge(trees_missinglvd_only1,
                                  treemsmts[,.(unitreeid, VISIT_NUMBER,
                                               lvd_new = TREE_EXTANT_CODE)],
                                  by = c("unitreeid", "VISIT_NUMBER"),
                                  all.x = TRUE)
  trees_missinglvd_only1[!is.na(lvd_new),
                         LVD_EDIT := "Missing, added based on next msmt"]
  trees_missinglvd_only1[is.na(lvd_new),
                         ':='(lvd_new = "D",
                              LVD_EDIT = "Missing at last msmt, added dead")]
  trees_missinglvd_only1[, VISIT_NUMBER := VISIT_NUMBER-1]
  trees_missinglvd <- rbind(trees_missinglvd_morethan1,
                            trees_missinglvd_only1)
  treemsmts <- merge(treemsmts,
                     trees_missinglvd,
                     by = c("unitreeid", "VISIT_NUMBER"),
                     all.x = TRUE)
  treemsmts[is.na(TREE_EXTANT_CODE),
            TREE_EXTANT_CODE := lvd_new]
  treemsmts[, lvd_new := NULL]
  rm(trees_missinglvd, trees_missinglvd_morethan1, trees_missinglvd_only1)
  gc()

  treemsmts[, TREE_MEASUREMENT_COMMENT := tolower(TREE_MEASUREMENT_COMMENT)]
  treemsmts[grepl("live", TREE_MEASUREMENT_COMMENT) |
              grepl("not dead", TREE_MEASUREMENT_COMMENT) |
              grepl("not cut", TREE_MEASUREMENT_COMMENT) |
              grepl("found tagged with no data.", TREE_MEASUREMENT_COMMENT) |
              grepl("dying", TREE_MEASUREMENT_COMMENT) |
              grepl("found w/ no data.", TREE_MEASUREMENT_COMMENT) |
              grepl("tagged with no prev. measurement data.", TREE_MEASUREMENT_COMMENT) |
              grepl("found w/no data.", TREE_MEASUREMENT_COMMENT),
            lvd_comment := "L"]
  if(nrow(treemsmts[lvd_comment == "L",]) > 0){
    trees_live_comment <- treemsmts[lvd_comment == "L",
                                    .(confirm_live_visit = max(VISIT_NUMBER)),
                                    by = "unitreeid"]
    treemsmts <- merge(treemsmts,
                       trees_live_comment,
                       by = "unitreeid",
                       all.x = TRUE)
    treemsmts[VISIT_NUMBER <= confirm_live_visit &
                TREE_EXTANT_CODE == "D",
              ':='(TREE_EXTANT_CODE = "L",
                   LVD_EDIT = "Back to L based on tree measurement comments")] # any msmt before this visit and lv_d == "D" need to be corrected
    # to L
    treemsmts[, ':='(lvd_comment = NULL,
                     confirm_live_visit  = NULL)]
    rm(trees_live_comment)
  }
  ## correction 3, if a tree was dead at current msmt but live in the later msmts,
  # change this tree for current msmt as L
  trees_live <- treemsmts[TREE_EXTANT_CODE == "L",
                          .(lastlive = max(VISIT_NUMBER)),
                          by = "unitreeid"]
  treemsmts <- merge(treemsmts,
                     trees_live,
                     by = "unitreeid",
                     all.x = TRUE)
  treemsmts[VISIT_NUMBER < lastlive &
              TREE_EXTANT_CODE == "D",
            ':='(TREE_EXTANT_CODE = "L",
                 LVD_EDIT = "Change to L based on later msmt")]
  treemsmts[, ':='(lastlive = NULL)]

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

  # correction 4. if the last msmt do not have diameter information, assign lvd as D
  # realized that there are a lot trees do not have diameter at any msmt
  # so, to fix that, add one more condition: if a tree have diameter measured at previous msmts
  treemsmts_withdiameter <- treemsmts[DIAMETER > 0,.(unitreeid, VISIT_NUMBER,
                                                     DIAMETER, DIAMETER_MEASMT_HEIGHT,
                                                     TREE_EXTANT_CODE)]
  treemsmts_withdiameter[, lastvisit := max(VISIT_NUMBER), by = "unitreeid"]
  treemsmts_withdiameter <- treemsmts_withdiameter[lastvisit == VISIT_NUMBER,
                                                   .(unitreeid, diameter_visit_last = VISIT_NUMBER,
                                                     diameter_last = DIAMETER,
                                                     diameter_ht_last = DIAMETER_MEASMT_HEIGHT)]
  treemsmts <- merge(treemsmts,
                     treemsmts_withdiameter,
                     by = "unitreeid",
                     all.x = TRUE)
  rm(treemsmts_withdiameter)
  gc()


  # lv_d = L, but without diameter msmt
  # and inplot tree
  # *identify if tree at very last measurement is missing a diameter measurement, then assume it is a dead fallen tree;
  # correction a, if this tree is live, set to dead, use last diameter as current diameter
  treemsmts[VISIT_NUMBER > diameter_visit_last & is.na(DIAMETER) &
              TREE_EXTANT_CODE == "L" & OUT_OF_PLOT_IND == "N",
            ':='(TREE_EXTANT_CODE = "D",
                 TREE_CLASS_CODE = 4,
                 DIAMETER = diameter_last,
                 DIAMETER_MEASMT_HEIGHT = diameter_ht_last,
                 LVD_EDIT = "Change to D as missing diameter msmt")]
  treemsmts[,':='(diameter_visit_last = NULL,
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
    treemsmts[LENGTH/DIAMETER >= 3,
              ':='(LENGTH = LENGTH/10,
                   HEIGHT_EDIT = "Abnomal HEIGHT-DBH ratio, divided by 10")]
  } else {
    treemsmts[SITE_IDENTIFIER == 3001342 &
                TREE_NUMBER == 68 &
                VISIT_NUMBER == 1,
              ':='(LENGTH = 4.7,
                   HEIGHT_EDIT = "Best guess")] # best guess
    treemsmts[SITE_IDENTIFIER == 6006671 &
                TREE_NUMBER == 8 &
                VISIT_NUMBER == 1 &
                PLOT == "I",
              ':='(LENGTH = 3.8,
                   HEIGHT_EDIT = "Best guess")] # best guess
  }
  # correction d, if this tree is dead, and diameter is missing
  # and previous it is live with diameter
  treemsmts <- treemsmts[order(unitreeid, VISIT_NUMBER),]
  treemsmts[, ':='(lvd_prev = shift(TREE_EXTANT_CODE, type = "lag"),
                   diam_prev = shift(DIAMETER, type = "lag"),
                   diam_ht_prev = shift(DIAMETER_MEASMT_HEIGHT, type = "lag")),
            by = "unitreeid"]

  treemsmts[is.na(DIAMETER) & diam_prev > 0 & lvd_prev == "L" &
              TREE_EXTANT_CODE == "D",
            ':='(DIAMETER = diam_prev,
                 DIAMETER_MEASMT_HEIGHT = diam_ht_prev,
                 DIAMETER_EDIT = "Diameter assinged based on previous msmt")]
  treemsmts[,':='(lvd_prev = NULL,
                  diam_prev = NULL,
                  diam_ht_prev = NULL)]
  gc()
  # 2. add missing observation in between two msmt
  msmts_tree <- unique(treemsmts[,.(unitreeid, SITE_IDENTIFIER, VISIT_NUMBER,
                                    intreemsmt = TRUE)])
  msmts_tree_range <- msmts_tree[, .(visit_first = min(VISIT_NUMBER),
                                     visit_last = max(VISIT_NUMBER)),
                                 by = "unitreeid"]
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
  treemsmts_missing_inbetween <- merge(treemsmts,
                                       msmt_missing_inbetween,
                                       by = c("unitreeid", "VISIT_NUMBER"))
  rm(msmt_missing_inbetween)
  treemsmts_missing_inbetween[, ':='(VISIT_NUMBER = VISIT_NUMBER_crt, # get back to the right visit number
                                     LENGTH = NA, # set tree height as NA
                                     MSMT_MISSING_EDIT = "Missing in between, added")]
  treemsmts_missing_inbetween_prev <- treemsmts_missing_inbetween[,.(unitreeid,
                                                                     VISIT_NUMBER = VISIT_NUMBER_prev,
                                                                     VISIT_NUMBER_crt)]
  treemsmts_missing_inbetween_prev <- merge(treemsmts_missing_inbetween_prev,
                                            treemsmts[,.(unitreeid, VISIT_NUMBER,
                                                         DIAMETER_prev = DIAMETER)],
                                            by = c("unitreeid", "VISIT_NUMBER"))
  treemsmts_missing_inbetween_prev[, VISIT_NUMBER := VISIT_NUMBER_crt]
  treemsmts_missing_inbetween <- merge(treemsmts_missing_inbetween,
                                       treemsmts_missing_inbetween_prev,
                                       by = c("unitreeid", "VISIT_NUMBER"),
                                       all.x = TRUE)
  treemsmts_missing_inbetween[!is.na(DIAMETER) & !is.na(DIAMETER_prev),
                              ':='(DIAMETER = (DIAMETER+DIAMETER_prev)/2,
                                   DIAMETER_EDIT = "Diameter assigned based on mean of prev and next diameters")]
  treemsmts_missing_inbetween[is.na(DIAMETER) & !is.na(DIAMETER_prev),
                              ':='(DIAMETER = DIAMETER_prev,
                                   DIAMETER_EDIT = "Diameter assinged based on previous msmt")]
  treemsmts_missing_inbetween[, ':='(DIAMETER_prev = NULL,
                                     VISIT_NUMBER_crt.x = NULL,
                                     VISIT_NUMBER_next = NULL,
                                     VISIT_NUMBER_prev = NULL,
                                     VISIT_NUMBER_crt.y = NULL)]
  treemsmts_missing_inbetween[,':='(BORING_AGE = NA,
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
  treemsmts <- rbind(treemsmts, treemsmts_missing_inbetween,
                     fill = TRUE)
  rm(treemsmts_missing_inbetween, treemsmts_missing_inbetween_prev)
  gc()

  # 3. add missing observation at tail
  treemsmts[, visit_tree_last := max(VISIT_NUMBER),
            by = "unitreeid"]
  sitevisits_last <- sitevisits[,.(visit_site_last = max(VISIT_NUMBER)),
                                by = "SITE_IDENTIFIER"]
  treemsmts <- merge(treemsmts, sitevisits_last,
                     by = "SITE_IDENTIFIER",
                     all.x = TRUE)
  # tree's last visit have not reached the site's last visit, and
  # there is no stop sign
  # lv_d = L at the last visit of this tree and with diameter msmt
  treemsmts_missing_tail <- treemsmts[visit_tree_last == VISIT_NUMBER &
                                        visit_site_last != visit_tree_last &
                                        !is.na(DIAMETER) & TREE_EXTANT_CODE == "L" &
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
  treemsmts <- rbind(treemsmts,
                     treemsmts_missing_tail,
                     fill = TRUE)
  rm(treemsmts_missing_tail)
  gc()
  treemsmts <- treemsmts[order(unitreeid, VISIT_NUMBER),]
  treemsmts[, ':='(visit_tree_last = NULL,
                   visit_site_last = NULL,
                   VISIT_NUMBER_next = NULL)]

  ## 4. if a tree was broken top tree, this tree is always be btop trees in sequential msmt
  ### force trees that have length of 1.4 or less as broken top trees
  treemsmts[LENGTH %<=% 1.4 & BROKEN_TOP_IND == "N" &
              DIAMETER >= 3,
            ':='(BROKEN_TOP_IND = "Y",
                 BTOP_EDIT = "Corrected to Y as height <= 1.4 and DBH >= 3")]
  tree_btop <- treemsmts[BROKEN_TOP_IND == "Y",
                         .(unitreeid, VISIT_NUMBER, BROKEN_TOP_IND)]
  if(nrow(tree_btop) > 0){
    tree_btop[, visit_ref := min(VISIT_NUMBER), by = "unitreeid"]
    tree_btop <- tree_btop[visit_ref == VISIT_NUMBER,
                           .(unitreeid, visit_ref, btop_ref = BROKEN_TOP_IND)]
    treemsmts <- merge(treemsmts,
                       tree_btop,
                       by = "unitreeid",
                       all.x = TRUE)
    treemsmts[VISIT_NUMBER > visit_ref &
                BROKEN_TOP_IND %in% c("N", NA),
              ':='(BROKEN_TOP_IND = "Y",
                   BTOP_EDIT = "Change to Y based on previous msmt")]
    treemsmts[, ':='(btop_ref = NULL,
                     visit_ref = NULL)]
  }

  treeid_no_diam <- unique(treemsmts[is.na(DIAMETER) &
                                       !is.na(DIAMETER_MEASMT_HEIGHT) &
                                       LENGTH %>>% 1.3]$unitreeid)
  treemsmts_good <- treemsmts[!(unitreeid %in% treeid_no_diam),]
  treemsmts_bad <- treemsmts[(unitreeid %in% treeid_no_diam),]
  treemsmts_bad <- treemsmts_bad[order(unitreeid, VISIT_NUMBER),]
  treemsmts_bad[, ':='(diam_prev = shift(DIAMETER, type = "lag"),
                       diam_ht_prev = shift(DIAMETER_MEASMT_HEIGHT, type = "lag"),
                       diam_ht_next = shift(DIAMETER_MEASMT_HEIGHT, type = "lead"),
                       diam_next = shift(DIAMETER, type = "lead")),
                by = "unitreeid"]
  treemsmts_bad[is.na(DIAMETER) & diam_ht_prev == diam_ht_next &
                  !is.na(diam_prev) & !is.na(diam_next),
                ':='(DIAMETER = (diam_prev + diam_next)/2,
                     DIAMETER_MEASMT_HEIGHT = diam_ht_prev,
                     DIAMETER_EDIT = "Diameter assigned based on mean of prev and next diameters")]
  treemsmts_bad[is.na(DIAMETER) &
                  !is.na(diam_prev) & is.na(diam_next),
                ':='(DIAMETER = diam_prev,
                     DIAMETER_MEASMT_HEIGHT = diam_ht_prev,
                     DIAMETER_EDIT = "Diameter assinged based on previous msmt")]
  treemsmts_bad[,':='(diam_prev = NULL,
                      diam_next = NULL,
                      diam_ht_prev = NULL,
                      diam_ht_next = NULL)]
  treemsmts_bad[is.na(DIAMETER),
                DIAMETER_EDIT := "Missing diameter, no clue to assign"]
  treemsmts <- rbind(treemsmts_good, treemsmts_bad)
  rm(treemsmts_good, treemsmts_bad, tree_btop, treeid_no_diam,
     trees_live)

  if(compilationType == "PSP"){
    ## this is for PSP only
    # 5. *fill in crown class code moving forward;
    # if compress(index
    #             _fill(k)) ~= "" and compress(crncls_fill(k+1)) = "" then do;
    # crncls_fill(k+1) = crncls_fill(k);
    # end;
    ## first bring tree suppression code
    treemsmts[TREE_SUPPRESSION_CODE == "Y" &
                CROWN_CLASS_CODE != "S",
              ':='(CROWN_CLASS_CODE = "S",
                   CRCL_EDIT = "Corrected to S based on tree suppression code")]
    treemsmts[TREE_SUPPRESSION_CODE == "Y" &
                is.na(CROWN_CLASS_CODE),
              ':='(CROWN_CLASS_CODE = "S",
                   CRCL_EDIT = "Missing, assigned to S based on tree suppression code")]

    treeid_no_crcl <- unique(treemsmts[is.na(CROWN_CLASS_CODE)]$unitreeid)
    treemsmts_good <- treemsmts[!(unitreeid %in% treeid_no_crcl),]
    treemsmts_bad <- treemsmts[(unitreeid %in% treeid_no_crcl),]
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
    treemsmts <- rbind(treemsmts_good, treemsmts_bad, fill = TRUE)
    rm(treemsmts_good, treemsmts_bad, treelist_withcrcl)
  } else {
    treemsmts[, CRCL_EDIT := as.character(NA)]
  }

  # 6. correct species based on last observed species
  sp_last <- treemsmts[,.(unitreeid,
                          VISIT_NUMBER, TREE_SPECIES_CODE)]
  sp_last[, lastvisit := max(VISIT_NUMBER),
          by = "unitreeid"]
  sp_last <- sp_last[VISIT_NUMBER == lastvisit,
                     .(unitreeid, sp_last = TREE_SPECIES_CODE)]
  treemsmts <- merge(treemsmts,
                     sp_last,
                     by = "unitreeid",
                     all.x = TRUE)
  treemsmts[TREE_SPECIES_CODE != sp_last,
            ':='(TREE_SPECIES_CODE = sp_last,
                 SP_EDIT = "Species changed based on last msmt")]
  rm(sp_last)
  gc()
  treemsmts[, sp_last := NULL]
  treemsmts[TREE_SPECIES_CODE %in% c("XH", "Z", "ZH"),
            TREE_SPECIES_CODE := "X"]

  # 7. for missing diameter msmt height
  treemsmts[is.na(DIAMETER_MEASMT_HEIGHT) &
              !is.na(DIAMETER),
            ':='(DIAMETER_MEASMT_HEIGHT = 1.3,
                 DIAM_MSMT_HT_EDIT = "Missing diameter msmt height, 1.3 added")]
  if(compilationType == "PSP"){

    # 8. fill missing ht suit
    treeid_no_suitht <- unique(treemsmts[is.na(SUITABLE_FOR_HEIGHT_IND)]$unitreeid)
    treemsmts_good <- treemsmts[!(unitreeid %in% treeid_no_suitht),]
    treemsmts_bad <- treemsmts[(unitreeid %in% treeid_no_suitht),]
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
    treemsmts <- rbind(treemsmts_good, treemsmts_bad, fill = TRUE)
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
    treemsmts[VETERAN_IND == "Y" &
                RESIDUAL_IND == "N",
              ':='(RESIDUAL_IND = "Y",
                   RESIDUAL_EDIT = "Corrected to Y based on veteran_ind")]
    treemsmts[VETERAN_IND == "Y" &
                is.na(RESIDUAL_IND),
              ':='(RESIDUAL_IND = "Y",
                   RESIDUAL_EDIT = "Missing, assigned to Y based on veteran_ind")]
    treemsmts[RESIDUAL_IND == "Y" &
                is.na(SUITABLE_FOR_HEIGHT_IND),
              ':='(SUITABLE_FOR_HEIGHT_IND = "N",
                   SUIT_HT_EDIT = "Missing, N assigned due to residual tree")]
    treemsmts[RESIDUAL_IND == "Y" &
                SUITABLE_FOR_HEIGHT_IND == "Y",
              ':='(SUITABLE_FOR_HEIGHT_IND = "N",
                   SUIT_HT_EDIT = "Corrected to N due to residual tree")]
    treemsmts[TREE_EXTANT_CODE == "D" &
                is.na(SUITABLE_FOR_HEIGHT_IND),
              ':='(SUITABLE_FOR_HEIGHT_IND = "N",
                   SUIT_HT_EDIT = "Missing, N assigned due to dead tree")]
    treemsmts[TREE_EXTANT_CODE == "D" &
                SUITABLE_FOR_HEIGHT_IND == "Y",
              ':='(SUITABLE_FOR_HEIGHT_IND = "N",
                   SUIT_HT_EDIT = "Corrected to N due to dead tree")]
    treemsmts[BROKEN_TOP_IND == "Y" &
                is.na(SUITABLE_FOR_HEIGHT_IND),
              ':='(SUITABLE_FOR_HEIGHT_IND = "N",
                   SUIT_HT_EDIT = "Missing, N assigned due to btop tree")]
    treemsmts[BROKEN_TOP_IND == "Y" &
                SUITABLE_FOR_HEIGHT_IND == "Y",
              ':='(SUITABLE_FOR_HEIGHT_IND = "N",
                   SUIT_HT_EDIT = "Corrected to N due to btop tree")]
    treemsmts[TREE_STANCE_CODE == "F" &
                SUITABLE_FOR_HEIGHT_IND == "Y",
              ':='(SUITABLE_FOR_HEIGHT_IND = "N",
                   SUIT_HT_EDIT = "Corrected to N due to fallen tree")]
    treemsmts[TREE_STANCE_CODE == "F" &
                is.na(SUITABLE_FOR_HEIGHT_IND),
              ':='(SUITABLE_FOR_HEIGHT_IND = "N",
                   SUIT_HT_EDIT = "Missing, assigned N due to fallen tree")]
    treemsmts[is.na(SUITABLE_FOR_HEIGHT_IND),
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
    treemsmts[SUITABLE_FOR_HEIGHT_IND == "N" &
                SUITABLE_FOR_SITE_INDEX_IND == "Y",
              ':='(SUITABLE_FOR_SITE_INDEX_IND = "N",
                   SUIT_SI_EDIT = "Corrected to N as not suit for HT")]
    treemsmts[SUITABLE_FOR_HEIGHT_IND == "N" &
                is.na(SUITABLE_FOR_SITE_INDEX_IND),
              ':='(SUITABLE_FOR_SITE_INDEX_IND = "N",
                   SUIT_SI_EDIT = "Missing, assigned to N as not suit for HT")]

    treemsmts[(substr(AGE_MEASURE_CODE, 1, 1) %in% c("R", "C") |
                 SUITABLE_FOR_AGE_IND == "N" |
                 AGE_REPRESENTATIVE_IND == "N") &
                is.na(SUITABLE_FOR_SITE_INDEX_IND),
              ':='(SUITABLE_FOR_SITE_INDEX_IND = "N",
                   SUIT_SI_EDIT = "Missing, assigned to N as not suit for age")]

    treemsmts[(substr(AGE_MEASURE_CODE, 1, 1) %in% c("R", "C") |
                 SUITABLE_FOR_AGE_IND == "N" |
                 AGE_REPRESENTATIVE_IND == "N") &
                SUITABLE_FOR_SITE_INDEX_IND == "Y",
              ':='(SUITABLE_FOR_SITE_INDEX_IND = "N",
                   SUIT_SI_EDIT = "Corrected to N as not suit for age")]
    treemsmts[(CROWN_CLASS_CODE %in% c("S", "I") |
                 TREE_SUPPRESSION_CODE == "Y") &
                SUITABLE_FOR_SITE_INDEX_IND == "Y",
              ':='(SUITABLE_FOR_SITE_INDEX_IND = "N",
                   SUIT_SI_EDIT = "Corrected to N as not dominant or codominant tree")]
    treemsmts[(CROWN_CLASS_CODE %in% c("S", "I") |
                 TREE_SUPPRESSION_CODE == "Y") &
                is.na(SUITABLE_FOR_SITE_INDEX_IND),
              ':='(SUITABLE_FOR_SITE_INDEX_IND = "N",
                   SUIT_SI_EDIT = "Missing, assigned to N as not dominant or codominant tree")]
    treemsmts[is.na(SUITABLE_FOR_SITE_INDEX_IND),
              ':='(SUITABLE_FOR_SITE_INDEX_IND = "Y",
                   SUIT_SI_EDIT = "Missing, assigned to Y as no clue")]
  } else {
    # for nonpsp
    treemsmts[,':='(SUIT_HT_EDIT = as.character(NA),
                    SUIT_SI_EDIT = as.character(NA),
                    RESIDUAL_EDIT = as.character(NA))]
    # if a tree is a site tree and no diameter information
    # the broken top ind should be assigned with ""
    # pls refer the conversion with Dan on 2023-10-18
    treemsmts[is.na(DIAMETER) &
                (!is.na(AGE_MEASMT_HEIGHT) | !is.na(AGE_MEASURE_CODE)),
              BROKEN_TOP_IND := ""]
  }
  return(treemsmts)
}
