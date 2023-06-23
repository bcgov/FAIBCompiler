#' Summarize site age data by cluster and cluster/species for PSP data
#'
#'
#' @description This function takes compiled site age tree data, an output of \code{\link{siteAgeCompiler}}, to
#'              derive mean age and height results. The compiled data must have breast height age,
#'              total age, and site index.
#'
#' @param cpldSiteAgeData data.table, Compiled site age tree data, an output of \code{\link{siteAgeCompiler}}.
#' @param treemsmt data.table, Tree measurement data.
#'
#' @return Two data tables: cl_ah is the age/height summary at cluster level and spc_ah is the age/height summary
#'         at cluster and species level
#'
#'
#' @importFrom data.table ':='
#' @importFrom FAIBBase merge_dupUpdate
#'
#'
#' @export
#' @docType methods
#' @rdname siteAgeSummary_PSP
#'
#' @author Yong Luo
siteAgeSummary_PSP <- function(cpldSiteAgeData,
                               treemsmt){
  compPath <- "D:/ISMC project/ISMC compiler/ISMC compiler G version/compilation_PSP_sa/" # loc
cpldSiteAgeData <- readRDS(file.path(compPath, "tree_ah2.rds"))

treemsmt_org <- readRDS(file.path(compPath, "treemsmt.rds"))

  treemsmt <- treemsmt_org[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER,
                          PLOT, TREE_NO,
                          SPECIES,
                          LV_D,
                          TAGGING_SECTOR_NO,
                          SITE_SECTOR_NO,
                          DBH,
                          HT_TOTAL,
                          HT_TOTAL_SOURCE,
                          RESIDUAL,
                          PLOT_AREA_MAIN,
                          BTOP, TREE_SUPPRESSION_CODE,
                          CROWN_CLASS_CODE,
                          TOP_HEIGHT_TREE_IND,
                          TREE_CLASS_CODE)]
  # to fill missing site sector,
  # 1) use the last observed site sector number to populate for a given tree
  # this is a revised method from sas codes
  # *fill in site sector no;
  # if sitesect_fill(j-1) ~= . and sitesect_fill(j) = . then
  # sitesect_fill(j) = sitesect_fill(j-1);
  # else if sitesect_fill(j-1) = . and sitesect_fill(j) ~= . then
  # sitesect_fill(j-1) = sitesect_fill(j);
  # else if sitesect_fill(j-1) ~= . and sitesect_fill(j) ~= . and
  # sitesect_fill(j-1) ~= sitesect_fill(j) then do;
  # tree_err18 = "_chgsitsec";
  # end;
  lastsitesector_tb <- treemsmt[!is.na(SITE_SECTOR_NO),
                                .(SITE_IDENTIFIER, PLOT, TREE_NO,
                                  VISIT_NUMBER, SITE_SECTOR_NO)]
  lastsitesector_tb[, visit_last := max(VISIT_NUMBER),
                    by = c("SITE_IDENTIFIER", "PLOT", "TREE_NO")]
  lastsitesector_tb <- lastsitesector_tb[visit_last == VISIT_NUMBER,
                                         .(SITE_IDENTIFIER, PLOT, TREE_NO,
                                           SITE_SECTOR_NO_last = SITE_SECTOR_NO)]
  treemsmt <- merge(treemsmt,
                    lastsitesector_tb,
                    by = c("SITE_IDENTIFIER", "PLOT", "TREE_NO"),
                    all.x = TRUE)
  treemsmt[is.na(SITE_SECTOR_NO) & !is.na(SITE_SECTOR_NO_last),
           SITE_SECTOR_NO := SITE_SECTOR_NO_last]

  treemsmt[, SITE_SECTOR_NO_last := NULL]
  # 2) based on tagging sector : site sector computed lookup table;
  # *for each sample id, based on the majority of trees
  # with valid tagging sector numbers and site sector numbers;
  # this method is original SAS method
  bestsitesector_tb <- treemsmt[TAGGING_SECTOR_NO > 0 &
                                  SITE_SECTOR_NO > 0,
                                .(nobsved = length(DBH)),
                                by = c("CLSTR_ID", "PLOT", "TAGGING_SECTOR_NO",
                                       "SITE_SECTOR_NO")]
  bestsitesector_tb <- bestsitesector_tb[order(CLSTR_ID, PLOT, TAGGING_SECTOR_NO,
                                               -nobsved),]
  bestsitesector_tb <- unique(bestsitesector_tb,
                              by = c("CLSTR_ID", "PLOT", "TAGGING_SECTOR_NO"))
  setnames(bestsitesector_tb, "SITE_SECTOR_NO", "SITE_SECTOR_NO_best")

  treemsmt <- merge(treemsmt,
                    bestsitesector_tb,
                    by = c("CLSTR_ID", "PLOT", "TAGGING_SECTOR_NO"),
                    all.x = TRUE)
  treemsmt[is.na(SITE_SECTOR_NO) & !is.na(SITE_SECTOR_NO_best),
           SITE_SECTOR_NO := SITE_SECTOR_NO_best]
  treemsmt[, nobsved := NULL]



  saveRDS(treemsmt,
          "treemsmt_forsector.rds")




  cpldSiteAgeData[, ':='(siteAgeTree = TRUE,
                  SPECIES = NULL,
                  RESIDUAL = NULL,
                  HT_TOTAL_SOURCE = NULL)]


  treelist_whole <- merge(treemsmt,
                          cpldSiteAgeData,
                          by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                          all.x = TRUE)

  treelist_whole[siteAgeTree == TRUE &
                   BORED_AGE_FLAG == "ADDED_FROM_REFERENCE" &
                   is.na(LV_D),
                 LV_D := "L"]
  treelist_whole <- treelist_whole[LV_D == "L" &
                                     RESIDUAL %in% c("N", NA),] # only for Live trees and nonresidual trees



  treelist_whole <- treelist_whole[!is.na(PLOT_AREA_MAIN),] # select plot with area
  treelist_whole[, MAXN_HTOP := round(PLOT_AREA_MAIN*100)] # determine how many trees for a given plot size for the smry
  treelist_whole[MAXN_HTOP == 0, MAXN_HTOP := 1]


  treelist_whole[, SECTOR_NO := ifelse(!is.na(SITE_SECTOR_NO) & SITE_SECTOR_NO > 0,
                                       SITE_SECTOR_NO, TAGGING_SECTOR_NO)]



  treelist_whole[, NO_SECTOR := length(unique(SECTOR_NO)),
                 by = c("CLSTR_ID", "PLOT")]
  # treelist_whole[NO_SECTOR > 1 & # more than 1 sector
  #                  is.na(SECTOR_NO) &
  #                  siteAgeTree == TRUE, # however, some of the trees do
  #                ]
  # treelist_whole[NO_SECTOR > 1 & # more than 1 sector
  #                  is.na(SECTOR_NO), # however, some of the trees do
  #                ]
  # treelist_whole[NO_SECTOR == 1 & # more than 1 sector
  #                  is.na(SECTOR_NO), # however, some of the trees do
  #                ]


  treelist_whole[NO_SECTOR > 0 & MAXN_HTOP == NO_SECTOR,
                 ':='(TAGGING_SECTOR_HTOP = SECTOR_NO,
                      NUM_HTOP_PERSEC = 1)]

  treelist_whole[NO_SECTOR > 0 & MAXN_HTOP > NO_SECTOR,
                 ':='(TAGGING_SECTOR_HTOP = SECTOR_NO,
                      NUM_HTOP_PERSEC = as.integer(MAXN_HTOP/NO_SECTOR))]

  treelist_whole[NO_SECTOR > 0 & MAXN_HTOP < NO_SECTOR,
                 ':='(TAGGING_SECTOR_HTOP = round(SECTOR_NO/(round(NO_SECTOR/MAXN_HTOP))),
                      NUM_HTOP_PERSEC = 1)]

  treelist_whole[NO_SECTOR > 0 & MAXN_HTOP < NO_SECTOR &
                   TAGGING_SECTOR_HTOP == 0,
                 TAGGING_SECTOR_HTOP := 1]

  treelist_whole <- treelist_whole[order(CLSTR_ID, PLOT, TAGGING_SECTOR_HTOP, SPECIES, -DBH),]
  treelist_whole[, DBH_SEC_ORDER := 1:(length(DBH)),
                 by = c("CLSTR_ID", "PLOT", "TAGGING_SECTOR_HTOP", "SPECIES")]
  treelist_whole <- treelist_whole[order(CLSTR_ID, PLOT, SPECIES, -DBH),]
  treelist_whole[, DBH_SAM_ORDER := 1:(length(DBH)),
                 by = c("CLSTR_ID", "PLOT", "SPECIES")]
  treelist_whole[NO_SECTOR == 1 & is.na(TAGGING_SECTOR_HTOP), # no tagging sector htop for this sample
                 DBH_SEC_ORDER := NA]
  treelist_whole_org <- data.table::copy(treelist_whole)

  treelist_whole <- data.table::copy(treelist_whole_org)

  ## to define suitable for si
  # 1. use what database has
  treelist_whole[, SUIT_SI_org := SUIT_SI]
  # 2. if suit_si missing, use original sas codes to define suit_si
  # *define a site tree suitability flag, based on a selection of damage codes,
  # suitability for taking heights;
  # *cores with pith reached or pith estimated, and assessment of age suppression;
  # *for site tree screening, the sisuitability flag is used, modified under age2 dataset based on additional criteria;
  # *conversation with khardy 2013-mar-26, ;
  # if top_damage = "Y" or tree_suitable_for_ht = "N" or pith_code in ("R") or
  # suppression_ind in ("Y") or tr_class in (5,6) then do;
  # *for this definition, if not suitable for si, then also not suitable for ht, if missing;
  # sitree_suit = "N";
  # if tree_suitable_for_ht = "" then tree_suitable_for_ht = "N";
  # end;
  # else do;
  # *for this definition, if suitable for si, and htsuit flag is missing,
  # then assume also suitable for ht;
  # sitree_suit = "Y";
  # if tree_suitable_for_ht = "" then tree_suitable_for_ht = "Y";
  # end;
  treelist_whole[is.na(SUIT_SI) & (BTOP == "Y" | SUIT_HT == "N" | substr(AGE_MEASURE_CODE, 1, 1) %in% c("R", "C") |
                   TREE_SUPPRESSION_CODE == "Y" | TREE_CLASS_CODE %in% c(5, 6)),
                 SUIT_SI := "N"]
  treelist_whole[is.na(SUIT_SI_org) & SUIT_SI == "N" & is.na(SUIT_HT),
                 SUIT_HT := "N"]
  treelist_whole[is.na(SUIT_SI), ':='(SUIT_SI = "Y")]
  treelist_whole[is.na(SUIT_HT), ':='(SUIT_HT = "Y")]




  treelist_whole[CROWN_CLASS_CODE %in% c("S", "I"),
                 SUIT_SI := "N"]
  treelist_whole[AGE_BH < 20 & HT_TOTAL_SOURCE == "Estimated based on DBH",
                 SUIT_SI := "N"]




  saveRDS(treelist_whole, "treelist_withsi_suit.rds")

  # *si method 1 based on selecting the largest suitable dbh
  # tree from each sector (where each sector is an approximation of 0.01ha);

  treelist_whole[DBH_SEC_ORDER > 0 &
                   DBH_SEC_ORDER <= NUM_HTOP_PERSEC &
                   SUIT_SI == "Y" &
                   !is.na(AGE_BH) & !is.na(AGE_TOT) & !is.na(SI_TREE)]

  smry_method1 <- treelist_whole[DBH_SEC_ORDER > 0 &
                                   DBH_SEC_ORDER <= NUM_HTOP_PERSEC &
                                   SUIT_SI == "Y" &
                                   !is.na(AGE_BH) & !is.na(AGE_TOT) & !is.na(SI_TREE),
                                 .(SI1 = mean(SI_TREE, na.rm = TRUE),
                                   AGE_BH1 = mean(AGE_BH, na.rm = TRUE),
                                   AGE_TOT1 = mean(AGE_TOT, na.rm = TRUE),
                                   HTOP1 = mean(HT_TOTAL, na.rm = TRUE),
                                   N_OBS_METHOD1 = length(DBH)),
                                 by = c("CLSTR_ID", "SPECIES")]

  # *method 2, based on selecting the largest suitable 100 topht trees/ha without sectoring (ie., 4 largest in 0.04ha, 8/0.08, etc);
  # *adjust by applying oscar garcias simple adjustment equation: 1.6 * A - 0.6, to account for bias;
  # *Theoretical arguments suggest the 1.6 A - 0.6 largests as a reasonable approximation (Garcia, O., Can.J.For.Res. 28: 1509-1517, 1998);
  treelist_whole[, MAXN_HTOP_ADJ := round((1.6 * MAXN_HTOP - 0.6))]
  smry_method2 <- treelist_whole[DBH_SAM_ORDER > 0 &
                                   DBH_SAM_ORDER <= MAXN_HTOP_ADJ &
                                   SUIT_SI == "Y" &
                                   !is.na(AGE_BH) & !is.na(AGE_TOT) & !is.na(SI_TREE),
                                 .(SI2 = mean(SI_TREE, na.rm = TRUE),
                                   AGE_BH2 = mean(AGE_BH, na.rm = TRUE),
                                   AGE_TOT2 = mean(AGE_TOT, na.rm = TRUE),
                                   HTOP2 = mean(HT_TOTAL, na.rm = TRUE),
                                   N_OBS_METHOD2 = length(DBH)),
                                 by = c("CLSTR_ID", "SPECIES")]
  # *prep for method 3, based on first computing average age and average calc height
  # of the 100 largest suitable dbh trees (4/0.04) by species;
  # *faib way;
  # *as of june 11, 2014, decided to relax age criteria, to be all available si suitable trees;
  # *otherwise, no tree data may get averaged here, if not the largest 4 dbh trees/0.04ha plot were bored for age;
  # *this may lower the average, but at least plots with aged tree data of si suitable trees will get a stand age assigned with this method;
  smry_method3 <- treelist_whole[DBH_SAM_ORDER > 0 &
                                   (DBH_SAM_ORDER <= MAXN_HTOP_ADJ |
                                   (SUIT_SI == "Y" & AGE_BH > 0)),
                                 .(AGE_BH = mean(AGE_BH, na.rm = TRUE),
                                   AGE_TOT = mean(AGE_TOT, na.rm = TRUE),
                                   HTOP = mean(HT_TOTAL, na.rm = TRUE)),
                                 by = c("CLSTR_ID", "SPECIES")]
  specieslkup <- lookup_species()
  smry_method3 <- merge(smry_method3,
                        unique(specieslkup[,.(SPECIES, SP_SINDEX)]),
                        by = "SPECIES",
                        all.x = TRUE)
  smry_method3[, sp_index := SIndexR_SpecMap(SP_SINDEX)]
  smry_method3[, ':='(gi_curve = SIndexR_DefGICurve(sp_index),
  si_curve = SIndexR_DefCurve(sp_index))]
  SI_GI_temp <- SIndexR_HtAgeToSI(curve = smry_method3$gi_curve,
                            age = smry_method3$AGE_BH,
                            ageType = 1,
                            height = smry_method3$HTOP,
                            estType = 1)
  SI_SI_temp <- SIndexR_HtAgeToSI(curve = smry_method3$si_curve,
                            age = smry_method3$AGE_BH,
                            ageType = 1,
                            height = smry_method3$HTOP,
                            estType = 1)


  smry_method3[, ':='(SI_GI = SI_GI_temp$output,
                      SI_SI = SI_SI_temp$output)]

  smry_method3[, SI3 := ifelse(SI_GI > 0 & AGE_BH > 10, SI_GI,
                               ifelse(SI_SI > 0 & AGE_BH > 10, SI_SI,
                                      NA))]
  smry_method3 <- smry_method3[,.(CLSTR_ID, SPECIES,
                                  AGE_BH3 = AGE_BH,
                                  AGE_TOT3 = AGE_TOT,
                                  HTOP3 = HTOP,
                                  SI3)]
  treelist_whole


}


