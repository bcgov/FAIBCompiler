#' Edit tree measurements for a repeatedly measured data.
#'
#'
#' @description This function takes tree-level measurements and edits live/dead codes,
#'              diameter, species, and add tree measurements if the measurements are missing.
#'
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
#'
#'
#' @export
#' @docType methods
#' @rdname treemsmtEditing
#'
#' @author Yong Luo
treemsmtEditing <- function(treemsmts,
                            sitevisits){
  treemsmts[, unitreeid := paste0(SITE_IDENTIFIER, "-", PLOT, "-", TREE_NUMBER)]
  # 1. correct lv code
  ## if missing lv codes, use next lvd to populate,
  trees_attention <- unique(treemsmts[is.na(TREE_EXTANT_CODE)]$unitreeid)
  treemsmts_bad <- treemsmts[unitreeid %in% trees_attention,]
  treemsmts_good <- treemsmts[!(unitreeid %in% trees_attention),]

  treemsmts_bad <- treemsmts_bad[order(unitreeid, VISIT_NUMBER),]
  treemsmts_bad[, ':='(lvd_next = shift(TREE_EXTANT_CODE, type = "lead")),
                by = "unitreeid"]
  ## correction 1, if there is no next lvd information
  treemsmts_bad[is.na(TREE_EXTANT_CODE) & is.na(lvd_next),
                ':='(TREE_EXTANT_CODE = "D",
                     LVD_EDIT = "Missing, added dead as no lvd next")]
  ## correction 2, use next msmt to populate if it is present
  while(nrow(treemsmts_bad[is.na(TREE_EXTANT_CODE) & !is.na(lvd_next),]) > 0){
    treemsmts_bad[is.na(TREE_EXTANT_CODE) & !is.na(lvd_next),
                  ':='(TREE_EXTANT_CODE = lvd_next,
                       LVD_EDIT = "Missing, added based on next msmt")]
    treemsmts_bad[, ':='(lvd_next = shift(TREE_EXTANT_CODE, type = "lead")),
                  by = "unitreeid"]
  }
  treemsmts_bad[, ':='(lvd_next = NULL)]
  treemsmts <- rbind(treemsmts_good, treemsmts_bad,
                     fill = TRUE)
  rm(treemsmts_bad, treemsmts_good)
  gc()

  ## correction 3, if a tree was dead at current msmt but live in the later msmts,
  # change this tree for current msmt as L
  treemsmts <- treemsmts[order(unitreeid, VISIT_NUMBER),]
  treemsmts[, lvd_next := shift(TREE_EXTANT_CODE, type = "lead"),
            by = "unitreeid"]
  while(nrow(treemsmts[TREE_EXTANT_CODE == "D" & lvd_next == "L"]) > 0){
    treemsmts[TREE_EXTANT_CODE == "D" & lvd_next == "L",
              ':='(TREE_EXTANT_CODE = "L",
                   LVD_EDIT = "Change to L based on later msmt")]
    treemsmts[, lvd_next := shift(TREE_EXTANT_CODE, type = "lead"),
              by = "unitreeid"]
  }
  treemsmts[, lvd_next := NULL]

  # correction 4. if the last msmt do not have diameter infomation, assign lvd as D
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

  # correction b, if this tree is dead, and diameter is missing
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
  msmt_missing_inbetween <- msmts_tree_full[is.na(intreemsmt) & VISIT_NUMBER > visit_first &
                                              VISIT_NUMBER < visit_last,
                                            .(unitreeid, VISIT_NUMBER_crt = VISIT_NUMBER, VISIT_NUMBER_next, VISIT_NUMBER_prev)]


  # use all the same information from next msmt for a temporary solution
  msmt_missing_inbetween[, VISIT_NUMBER := VISIT_NUMBER_next]
  treemsmts_missing_inbetween <- merge(treemsmts,
                                       msmt_missing_inbetween,
                                       by = c("unitreeid", "VISIT_NUMBER"))
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

  treemsmts <- rbind(treemsmts, treemsmts_missing_inbetween,
                     fill = TRUE)

  # 3. add missing observation at tail
  treemsmts[, visit_tree_last := max(VISIT_NUMBER),
            by = "unitreeid"]
  sitevisits_last <- sitevisits[,.(visit_site_last = max(VISIT_NUMBER)),
                                by = "SITE_IDENTIFIER"]
  treemsmts <- merge(treemsmts, sitevisits_last,
                     by = "SITE_IDENTIFIER",
                     all.x = TRUE)
  # tree's last visit have not reached the site's last visit, and
  # lv_d = L at the last visit of this tree and with diameter msmt
  treemsmts_missing_tail <- treemsmts[visit_tree_last == VISIT_NUMBER &
                                        visit_site_last != visit_tree_last &
                                        !is.na(DIAMETER) & TREE_EXTANT_CODE == "L",]
  treemsmts_missing_tail <- merge(treemsmts_missing_tail,
                                  sitevisits[,.(SITE_IDENTIFIER, VISIT_NUMBER, VISIT_NUMBER_next)],
                                  by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                                  all.x = TRUE)
  treemsmts_missing_tail[, ':='(VISIT_NUMBER = VISIT_NUMBER_next, # add next visit
                                TREE_EXTANT_CODE = "D", # force to dead
                                LENGTH = NA,
                                MSMT_MISSING_EDIT = "Missing at tail, added",
                                DIAMETER_EDIT = "Diameter assinged based on previous msmt")]

  treemsmts <- rbind(treemsmts,
                     treemsmts_missing_tail,
                     fill = TRUE)
  treemsmts <- treemsmts[order(unitreeid, VISIT_NUMBER),]
  treemsmts[, ':='(visit_tree_last = NULL,
                   visit_site_last = NULL,
                   VISIT_NUMBER_next = NULL)]

  ## 4. if a tree was broken top tree, this tree is always be btop trees in sequential msmt
  treemsmts[, btop_prev := shift(BROKEN_TOP_IND, type = "lag"),
            by = "unitreeid"]
  while(nrow(treemsmts[BROKEN_TOP_IND == "N" & btop_prev == "Y"]) > 0){
    treemsmts[BROKEN_TOP_IND == "N" & btop_prev == "Y",
              ':='(BROKEN_TOP_IND = "Y",
                   BTOP_EDIT = "Change to Y based on previous msmt")]
    treemsmts[, btop_prev := shift(BROKEN_TOP_IND, type = "lag"),
              by = "unitreeid"]
  }
  treemsmts[, btop_prev := NULL]

  treemsmts[, ':='(diam_prev = shift(DIAMETER, type = "lag"),
                   diam_ht_prev = shift(DIAMETER_MEASMT_HEIGHT, type = "lag"),
                   diam_ht_next = shift(DIAMETER_MEASMT_HEIGHT, type = "lead"),
                   diam_next = shift(DIAMETER, type = "lead")),
            by = "unitreeid"]
  treemsmts[is.na(DIAMETER) & diam_ht_prev == diam_ht_next &
              !is.na(diam_prev) & !is.na(diam_next),
            ':='(DIAMETER = (diam_prev + diam_next)/2,
                 DIAMETER_MEASMT_HEIGHT = diam_ht_prev,
                 DIAMETER_EDIT = "Diameter assigned based on mean of prev and next diameters")]
  treemsmts[is.na(DIAMETER) &
              !is.na(diam_prev) & is.na(diam_next),
            ':='(DIAMETER = diam_prev,
                 DIAMETER_MEASMT_HEIGHT = diam_ht_prev,
                 DIAMETER_EDIT = "Diameter assinged based on previous msmt")]
  treemsmts[,':='(diam_prev = NULL,
                  diam_next = NULL,
                  diam_ht_prev = NULL,
                  diam_ht_next = NULL)]


  # 5. *fill in crown class code moving forward;
  # if compress(index
  #             _fill(k)) ~= "" and compress(crncls_fill(k+1)) = "" then do;
  # crncls_fill(k+1) = crncls_fill(k);
  # end;
  treemsmts[, crcl_prev := shift(CROWN_CLASS_CODE, type = "lag"),
            by = "unitreeid"]
  while (nrow(treemsmts[is.na(CROWN_CLASS_CODE) & !is.na(crcl_prev),]) > 0) {
    treemsmts[is.na(CROWN_CLASS_CODE) & !is.na(crcl_prev),
              ':='(CROWN_CLASS_CODE = crcl_prev,
                   CRCL_EDIT = "CR_CL added based on previous msmt")]
    treemsmts[, crcl_prev := shift(CROWN_CLASS_CODE, type = "lag"),
              by = "unitreeid"]
  }
  treemsmts[, crcl_prev := NULL]
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
  treemsmts[, sp_last := NULL]
  treemsmts[TREE_SPECIES_CODE %in% c("XH", "Z", "ZH"),
            TREE_SPECIES_CODE := "X"]
  treemsmts[, unitreeid := NULL]
  return(treemsmts)
}
