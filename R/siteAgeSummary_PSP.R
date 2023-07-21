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


  lasttaggsector_tb <- treemsmt[!is.na(TAGGING_SECTOR_NO),
                                .(SITE_IDENTIFIER, PLOT, TREE_NO,
                                  VISIT_NUMBER, TAGGING_SECTOR_NO)]
  lasttaggsector_tb[, visit_last := max(VISIT_NUMBER),
                    by = c("SITE_IDENTIFIER", "PLOT", "TREE_NO")]
  lasttaggsector_tb <- lasttaggsector_tb[visit_last == VISIT_NUMBER,
                                         .(SITE_IDENTIFIER, PLOT, TREE_NO,
                                           TAGGING_SECTOR_NO_last = TAGGING_SECTOR_NO)]
  treemsmt <- merge(treemsmt,
                    lasttaggsector_tb,
                    by = c("SITE_IDENTIFIER", "PLOT", "TREE_NO"),
                    all.x = TRUE)
  treemsmt[is.na(TAGGING_SECTOR_NO) & !is.na(TAGGING_SECTOR_NO_last),
           TAGGING_SECTOR_NO := TAGGING_SECTOR_NO_last]
  treemsmt[, TAGGING_SECTOR_NO_last := NULL]
  cpldSiteAgeData <- cpldSiteAgeData[,.(CLSTR_ID, PLOT, TREE_NO,
                                        AGE_BH, AGE_TOT, SI_TREE,
                                        AGE_MEASURE_CODE, BORED_AGE_FLAG,
                                        siteAgeTree = TRUE)]

  treelist_whole <- merge(treemsmt,
                          cpldSiteAgeData,
                          by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                          all.x = TRUE)

  treelist_whole[siteAgeTree == TRUE &
                   BORED_AGE_FLAG == "ADDED_FROM_REFERENCE" &
                   is.na(LV_D),
                 LV_D := "L"]

  treelist_whole <- treelist_whole[!is.na(PLOT_AREA_MAIN),] # select plot with area
  treelist_whole[, MAXN_HTOP := round(PLOT_AREA_MAIN*100)] # determine how many trees for a given plot size for the smry
  treelist_whole[MAXN_HTOP == 0, MAXN_HTOP := 1]

  treelist_whole[SITE_SECTOR_NO > 0,
                 SECTOR_NO := SITE_SECTOR_NO]
  treelist_whole[is.na(SECTOR_NO) & TAGGING_SECTOR_NO > 0,
                 SECTOR_NO := TAGGING_SECTOR_NO]
  # in the original sas code, it used max sector_no as the number of the sectors for a
  # given samp_id
  # this needs to be discussed
  treelist_whole[, MAX_SECTORS := max(SECTOR_NO, na.rm = TRUE),
                 by = "SITE_IDENTIFIER"]

  treelist_whole[MAXN_HTOP == MAX_SECTORS,
                 ':='(TAGGING_SECTOR_HTOP = SECTOR_NO,
                      NUM_HTOP_PERSEC = 1)]

  treelist_whole[MAXN_HTOP > MAX_SECTORS,
                 ':='(TAGGING_SECTOR_HTOP = SECTOR_NO,
                      NUM_HTOP_PERSEC = as.integer(MAXN_HTOP/MAX_SECTORS))]
  treelist_whole[MAXN_HTOP < MAX_SECTORS,
                 ':='(TAGGING_SECTOR_HTOP = round(SECTOR_NO/round(MAX_SECTORS/MAXN_HTOP)),
                      NUM_HTOP_PERSEC = 1)]
  treelist_whole[TAGGING_SECTOR_HTOP == 0, TAGGING_SECTOR_HTOP := 1]
  treelist_whole[is.na(NUM_HTOP_PERSEC), NUM_HTOP_PERSEC := 1]

  treelist_whole_live <- treelist_whole[LV_D == "L" & RESIDUAL == "N",
                                        .(SITE_IDENTIFIER, PLOT, TREE_NO, VISIT_NUMBER,
                                          TAGGING_SECTOR_HTOP, SPECIES, DBH)]
  treelist_whole_live <- treelist_whole_live[order(SITE_IDENTIFIER, PLOT, VISIT_NUMBER,
                                                   TAGGING_SECTOR_HTOP, SPECIES, -DBH),]
  treelist_whole_live[, DBH_SEC_ORDER := 1:length(DBH),
                      by = c("SITE_IDENTIFIER", "PLOT", "VISIT_NUMBER",
                             "TAGGING_SECTOR_HTOP", "SPECIES")]
  treelist_whole_live <- treelist_whole_live[order(SITE_IDENTIFIER, PLOT, VISIT_NUMBER,
                                                   SPECIES, -DBH),]
  treelist_whole_live[, DBH_SAM_ORDER := 1:length(DBH),
                      by = c("SITE_IDENTIFIER", "PLOT", "VISIT_NUMBER",
                             "SPECIES")]



  treelist_whole <- merge(treelist_whole,
                          treelist_whole_live[,.(SITE_IDENTIFIER, PLOT, TREE_NO,
                                                 VISIT_NUMBER, DBH_SEC_ORDER, DBH_SAM_ORDER)],
                          by = c("SITE_IDENTIFIER", "PLOT", "TREE_NO",
                                 "VISIT_NUMBER"),
                          all.x = TRUE)

  ## to define suitable for si
  # 1. use what database has
  treelist_whole[, SUIT_SI_org := SUIT_SI]
  treelist_whole[RESIDUAL == "Y" | LV_D == "D",
                 SUIT_SI := "N"]
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


  # *si method 1 based on selecting the largest suitable dbh
  # tree from each sector (where each sector is an approximation of 0.01ha);

  treelist_whole[DBH_SEC_ORDER > 0 &
                   DBH_SEC_ORDER <= NUM_HTOP_PERSEC &
                   SUIT_SI == "Y"]
  treelist_whole[, si_flag := ifelse(is.na(SI_TREE), 0,
                                     ifelse(SI_TREE > 0, 1,
                                            0))]
  treelist_whole[, agebh_flag := ifelse(is.na(AGE_BH), 0,
                                        ifelse(AGE_BH > 0, 1,
                                               0))]
  treelist_whole[, httop_flag := ifelse(is.na(HT_TOTAL), 0,
                                        ifelse(HT_TOTAL > 0, 1,
                                               0))]
  treelist_whole[DBH_SEC_ORDER > 0 &
                   DBH_SEC_ORDER <= NUM_HTOP_PERSEC &
                   SUIT_SI == "Y",
                 IN_METHOD1 := TRUE]
  smry_method1 <- treelist_whole[DBH_SEC_ORDER > 0 &
                                   DBH_SEC_ORDER <= NUM_HTOP_PERSEC &
                                   SUIT_SI == "Y",
                                 .(SI1 = mean(SI_TREE, na.rm = TRUE),
                                   AGE_BH1 = mean(AGE_BH, na.rm = TRUE),
                                   AGE_TOT1 = mean(AGE_TOT, na.rm = TRUE),
                                   HTOP1 = mean(HT_TOTAL, na.rm = TRUE),
                                   N_SI1 = sum(si_flag),
                                   N_AGE1 = sum(agebh_flag),
                                   N_HTOP1 = sum(httop_flag)),
                                 by = c("SITE_IDENTIFIER", "VISIT_NUMBER", "SPECIES")]

  # *method 2, based on selecting the largest suitable 100 topht trees/ha without sectoring (ie., 4 largest in 0.04ha, 8/0.08, etc);
  # *adjust by applying oscar garcias simple adjustment equation: 1.6 * A - 0.6, to account for bias;
  # *Theoretical arguments suggest the 1.6 A - 0.6 largests as a reasonable approximation (Garcia, O., Can.J.For.Res. 28: 1509-1517, 1998);
  treelist_whole[, MAXN_HTOP_ADJ := round((1.6 * MAXN_HTOP - 0.6))]
  treelist_whole[DBH_SAM_ORDER > 0 &
                   DBH_SAM_ORDER <= MAXN_HTOP_ADJ &
                   SUIT_SI == "Y",
                 IN_METHOD2 := TRUE]
  smry_method2 <- treelist_whole[DBH_SAM_ORDER > 0 &
                                   DBH_SAM_ORDER <= MAXN_HTOP_ADJ &
                                   SUIT_SI == "Y",
                                 .(SI2 = mean(SI_TREE, na.rm = TRUE),
                                   AGE_BH2 = mean(AGE_BH, na.rm = TRUE),
                                   AGE_TOT2 = mean(AGE_TOT, na.rm = TRUE),
                                   HTOP2 = mean(HT_TOTAL, na.rm = TRUE),
                                   N_SI2 = sum(si_flag),
                                   N_AGE2 = sum(agebh_flag),
                                   N_HTOP2 = sum(httop_flag)),
                                 by = c("SITE_IDENTIFIER", "VISIT_NUMBER", "SPECIES")]
  # *prep for method 3, based on first computing average age and average calc height
  # of the 100 largest suitable dbh trees (4/0.04) by species;
  # *faib way;
  # *as of june 11, 2014, decided to relax age criteria, to be all available si suitable trees;
  # *otherwise, no tree data may get averaged here, if not the largest 4 dbh trees/0.04ha plot were bored for age;
  # *this may lower the average, but at least plots with aged tree data of si suitable trees will get a stand age assigned with this method;
  treelist_whole[DBH_SAM_ORDER > 0 &
                   (DBH_SAM_ORDER <= MAXN_HTOP_ADJ |
                      (SUIT_SI == "Y" & AGE_BH > 0)),
                 IN_METHOD3 := TRUE]
  smry_method3 <- treelist_whole[DBH_SAM_ORDER > 0 &
                                   (DBH_SAM_ORDER <= MAXN_HTOP_ADJ |
                                      (SUIT_SI == "Y" & AGE_BH > 0)),
                                 .(AGE_BH = mean(AGE_BH, na.rm = TRUE),
                                   AGE_TOT = mean(AGE_TOT, na.rm = TRUE),
                                   HTOP = mean(HT_TOTAL, na.rm = TRUE),
                                   N_AGE3 = sum(agebh_flag),
                                   N_HTOP3 = sum(httop_flag)),
                                 by = c("SITE_IDENTIFIER", "VISIT_NUMBER", "SPECIES")]
  specieslkup <- lookup_species()
  smry_method3 <- merge(smry_method3,
                        unique(specieslkup[,.(SPECIES, SP_SINDEX)]),
                        by = "SPECIES",
                        all.x = TRUE)
  smry_method3[, sp_index := SIndexR::SIndexR_SpecMap(SP_SINDEX)]
  smry_method3[, ':='(gi_curve = SIndexR::SIndexR_DefGICurve(sp_index),
                      si_curve = SIndexR_DefCurve(sp_index))]
  SI_GI_temp <- SIndexR::SIndexR_HtAgeToSI(curve = smry_method3$gi_curve,
                                           age = smry_method3$AGE_BH,
                                           ageType = 1,
                                           height = smry_method3$HTOP,
                                           estType = 1)
  SI_SI_temp <- SIndexR::SIndexR_HtAgeToSI(curve = smry_method3$si_curve,
                                           age = smry_method3$AGE_BH,
                                           ageType = 1,
                                           height = smry_method3$HTOP,
                                           estType = 1)


  smry_method3[, ':='(SI_GI = SI_GI_temp$output,
                      SI_SI = SI_SI_temp$output)]

  smry_method3[, SI3 := ifelse(SI_GI > 0 & AGE_BH > 10, SI_GI,
                               ifelse(SI_SI > 0 & AGE_BH > 10, SI_SI,
                                      NA))]
  smry_method3 <- smry_method3[,.(SITE_IDENTIFIER, VISIT_NUMBER, SPECIES,
                                  AGE_BH3 = AGE_BH,
                                  AGE_TOT3 = AGE_TOT,
                                  HTOP3 = HTOP,
                                  SI3, N_AGE3, N_HTOP3)]
  maxnhtop <- treelist_whole[,.(MAXN_HTOP = max(MAXN_HTOP, na.rm = TRUE)),
                             by = c("SITE_IDENTIFIER", "PLOT")]
  maxnhtop <- maxnhtop[,.(MAXN_HTOP = sum(MAXN_HTOP)), by = "SITE_IDENTIFIER"]

  smry_method_all <- merge(smry_method1,
                           smry_method2,
                           by = c("SITE_IDENTIFIER", "VISIT_NUMBER", "SPECIES"),
                           all = TRUE)
  smry_method_all <- merge(smry_method_all,
                           smry_method3,
                           by = c("SITE_IDENTIFIER", "VISIT_NUMBER", "SPECIES"),
                           all = TRUE)

  smry_method_all <- merge(smry_method_all,
                           maxnhtop,
                           by = c("SITE_IDENTIFIER"),
                           all.x = TRUE)
  smry_method_all[,':='(PER_AGE1 = round(100*N_AGE1/MAXN_HTOP),
                        PER_AGE2 = round(100*N_AGE2/MAXN_HTOP),
                        PER_AGE3 = round(100*N_AGE3/MAXN_HTOP),
                        YRSFRM_50BH1 = abs(50 - AGE_BH1),
                        YRSFRM_50BH2 = abs(50 - AGE_BH2),
                        YRSFRM_50BH3 = abs(50 - AGE_BH3))]
  # *prioritize site index by measurement, to assign one standardized site index across all measurements;
  # *based on msmt that is closest to 50yrs breast height age, and has at least 50% of the maximum number of site trees available for si calculation;
  # *repeat for each si method;
  #
  # /************************/
  #   *method 1;
  # *first keep only measurements with at least 1 site tree sampled;
  si_sort1 <- smry_method_all[N_SI1 > 0,]
  si_sort1 <- si_sort1[order(SITE_IDENTIFIER, SPECIES, YRSFRM_50BH1),]
  # *next, pick first measurement closest to 50yrs bh;
  si_sort1 <- unique(si_sort1[,.(SITE_IDENTIFIER, SPECIES,
                                 SI_FINAL1 = SI1,
                                 SI_FINAL_SOURCE1 = VISIT_NUMBER,
                                 sort1 = TRUE)],
                     by = c("SITE_IDENTIFIER", "SPECIES"))
  # do the same for the rests
  si_sort2 <- smry_method_all[N_SI2 > 0,]
  si_sort2 <- si_sort2[order(SITE_IDENTIFIER, SPECIES, YRSFRM_50BH2),]
  si_sort2 <- unique(si_sort2[,.(SITE_IDENTIFIER, SPECIES,
                                 SI_FINAL2 = SI2,
                                 SI_FINAL_SOURCE2 = VISIT_NUMBER,
                                 sort2 = TRUE)],
                     by = c("SITE_IDENTIFIER", "SPECIES"))
  si_sort3 <- smry_method_all[N_AGE3 > 0,]
  si_sort3 <- si_sort3[order(SITE_IDENTIFIER, SPECIES, YRSFRM_50BH3),]
  si_sort3 <- unique(si_sort3[,.(SITE_IDENTIFIER, SPECIES,
                                 SI_FINAL3 = SI3,
                                 SI_FINAL_SOURCE3 = VISIT_NUMBER,
                                 sort3 = TRUE)],
                     by = c("SITE_IDENTIFIER", "SPECIES"))

  si_master <- unique(treelist_whole[,.(SITE_IDENTIFIER, VISIT_NUMBER, SPECIES)])
  si_master <- merge(si_master,
                     smry_method_all,
                     by = c("SITE_IDENTIFIER", "VISIT_NUMBER", "SPECIES"),
                     all.x = TRUE)
  si_master <- merge(si_master,
                     si_sort1,
                     by = c("SITE_IDENTIFIER", "SPECIES"),
                     all.x = TRUE)

  si_master <- merge(si_master,
                     si_sort2,
                     by = c("SITE_IDENTIFIER", "SPECIES"),
                     all.x = TRUE)

  si_master <- merge(si_master,
                     si_sort3,
                     by = c("SITE_IDENTIFIER", "SPECIES"),
                     all.x = TRUE)
  si_master <- si_master[sort1 == TRUE | sort2 == TRUE | sort3 == TRUE,]
  # equv si_allmethod3
  si_master[,':='(sort1 = NULL,
                  sort2 = NULL,
                  sort3 = NULL)]

  # *get leading species (by ba) site index;
  # *note, that the leading species is specific to a given measurement;
  # *leading species can change over subsequent measurements, so that the leading SI is species specific;
  # *if a given leading species has no SI site trees, then no leading SI will be available for that measurement, even though;
  # *si is standardized across all measurements;
  # summarize the leading and secondary species for a given msmt
  # based on BA of the trees with dbh>=4, and live trees
  ba_smry <- treelist_whole[volumeTree == TRUE &
                              DBH >= 4 & LV_D == "L",
                            .(ba_tot_sp = sum(BA_TREE)),
                            by = c("SITE_IDENTIFIER", "VISIT_NUMBER", "SPECIES")]
  ba_smry[, ba_tot := sum(ba_tot_sp),
          by = c("SITE_IDENTIFIER", "VISIT_NUMBER")]
  ba_smry[, BA_PCT := round(100*ba_tot_sp/ba_tot, 2)]

  ba_smry <- ba_smry[order(SITE_IDENTIFIER, VISIT_NUMBER, -BA_PCT),]
  ba_smry[, BA_PCT_RANK := 1:length(SPECIES), by = c("SITE_IDENTIFIER", "VISIT_NUMBER")]
  ba_smry[,':='(ba_tot_sp = NULL,
                ba_tot = NULL)]
  smry_by_sp <- merge(ba_smry,
                      si_master,
                      by = c("SITE_IDENTIFIER", "VISIT_NUMBER", "SPECIES"),
                      all.x = TRUE)
  smry_by_sp <- smry_by_sp[order(SITE_IDENTIFIER, VISIT_NUMBER, BA_PCT_RANK),
                           .(CLSTR_ID = paste0(SITE_IDENTIFIER, "-PSP", VISIT_NUMBER),
                             SITE_IDENTIFIER, VISIT_NUMBER, SPECIES,
                             BA_PCT, BA_PCT_RANK,
                             SI1, N_SI1,
                             SI2, N_SI2,
                             SI3, N_SI3 = N_AGE3,
                             AGE_BH1 = round(AGE_BH1),
                             N_AGE1,
                             AGE_BH2 = round(AGE_BH2),
                             N_AGE2,
                             AGE_BH3 = round(AGE_BH3),
                             N_AGE3,
                             AGE_TOT1 = round(AGE_TOT1),
                             AGE_TOT2 = round(AGE_TOT2),
                             AGE_TOT3 = round(AGE_TOT3),
                             HTOP1, N_HTOP1,
                             HTOP2, N_HTOP2,
                             HTOP3, N_HTOP3)]

  spcomp_4cm_lead <- ba_smry[BA_PCT_RANK == 1,
                             .(SITE_IDENTIFIER, VISIT_NUMBER, SPECIES, BA_PCT)]
  spcomp_4cm_lead <- merge(spcomp_4cm_lead,
                           si_master,
                           by = c("SITE_IDENTIFIER", "VISIT_NUMBER", "SPECIES"),
                           all.x = TRUE)
  names(spcomp_4cm_lead) <- paste0("LEAD_", names(spcomp_4cm_lead))
  setnames(spcomp_4cm_lead, c("LEAD_SITE_IDENTIFIER", "LEAD_VISIT_NUMBER", "LEAD_SPECIES"),
           c("SITE_IDENTIFIER", "VISIT_NUMBER", "LEAD_SP"))
  # for the second leading species
  spcomp_4cm_secd <- ba_smry[BA_PCT_RANK == 2,
                             .(SITE_IDENTIFIER, VISIT_NUMBER, SPECIES, BA_PCT)]
  spcomp_4cm_secd <- merge(spcomp_4cm_secd,
                           si_master,
                           by = c("SITE_IDENTIFIER", "VISIT_NUMBER", "SPECIES"),
                           all.x = TRUE)
  names(spcomp_4cm_secd) <- paste0("SECD_", names(spcomp_4cm_secd))
  setnames(spcomp_4cm_secd, c("SECD_SITE_IDENTIFIER", "SECD_VISIT_NUMBER", "SECD_SPECIES"),
           c("SITE_IDENTIFIER", "VISIT_NUMBER", "SECD_SP"))
  si_lead_secd <- merge(spcomp_4cm_lead,
                        spcomp_4cm_secd,
                        by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                        all = TRUE)
  si_lead_secd <- si_lead_secd[,.(CLSTR_ID = paste0(SITE_IDENTIFIER, "-PSP", VISIT_NUMBER),
                                  SITE_IDENTIFIER, VISIT_NUMBER,
                                  LEAD_SP, LEAD_BA_PCT,
                                  LEAD_SI1, LEAD_N_SI1,
                                  LEAD_SI2, LEAD_N_SI2,
                                  LEAD_SI3, LEAD_N_SI3 = LEAD_N_AGE3,
                                  LEAD_SI_FINAL1, LEAD_SI_FINAL_SOURCE1,
                                  LEAD_SI_FINAL2, LEAD_SI_FINAL_SOURCE2,
                                  LEAD_SI_FINAL3, LEAD_SI_FINAL_SOURCE3,
                                  LEAD_AGE_BH1 = round(LEAD_AGE_BH1),
                                  LEAD_N_AGE1,
                                  LEAD_AGE_BH2 = round(LEAD_AGE_BH2),
                                  LEAD_N_AGE2,
                                  LEAD_AGE_BH3 = round(LEAD_AGE_BH3),
                                  LEAD_N_AGE3,
                                  LEAD_AGE_TOT1 = round(LEAD_AGE_TOT1),
                                  LEAD_AGE_TOT2 = round(LEAD_AGE_TOT2),
                                  LEAD_AGE_TOT3 = round(LEAD_AGE_TOT3),
                                  LEAD_HTOP1, LEAD_N_HTOP1,
                                  LEAD_HTOP2, LEAD_N_HTOP2,
                                  LEAD_HTOP3, LEAD_N_HTOP3,
                                  SECD_SP, SECD_BA_PCT,
                                  SECD_SI1, SECD_N_SI1,
                                  SECD_SI2, SECD_N_SI2,
                                  SECD_SI3, SECD_N_SI3 = SECD_N_AGE3,
                                  SECD_SI_FINAL1, SECD_SI_FINAL_SOURCE1,
                                  SECD_SI_FINAL2, SECD_SI_FINAL_SOURCE2,
                                  SECD_SI_FINAL3, SECD_SI_FINAL_SOURCE3,
                                  SECD_AGE_BH1 = round(SECD_AGE_BH1),
                                  SECD_N_AGE1,
                                  SECD_AGE_BH2 = round(SECD_AGE_BH2),
                                  SECD_N_AGE2,
                                  SECD_AGE_BH3 = round(SECD_AGE_BH3),
                                  SECD_N_AGE3,
                                  SECD_AGE_TOT1 = round(SECD_AGE_TOT1),
                                  SECD_AGE_TOT2 = round(SECD_AGE_TOT2),
                                  SECD_AGE_TOT3 = round(SECD_AGE_TOT3))]
  # *some cases where leading species changes over measurements, but site tree data only available for
  # one species (eg., leading species at later msmts;
  #              *stand total age is based on leading species at establishment.  but if the leading species
  # at first measure doesnot have site tree data;
  #              *then need to have a surrogate age assigned;
  #              *site index stays specific to the species at given measure;
  #              *but a last resort total age assigned to sample id will be based on the leading species
  # at last measure, if no site tree data are available;
  #              *for the leading species at establishment;
  #              *this part address this;

  surage1 <- si_master[,.(SITE_IDENTIFIER, VISIT_NUMBER,
                          SPECIES, SPC1_LMEAS_BHAGE = round(AGE_BH3),
                          SPC1_LMEAS_TOTAGE = round(AGE_TOT3))]
  surage2 <- ba_smry[BA_PCT_RANK == 1,
                     .(SITE_IDENTIFIER, VISIT_NUMBER, SPECIES)]
  surage2[, lastvisit := max(VISIT_NUMBER),
          by = "SITE_IDENTIFIER"]
  surage2 <- surage2[VISIT_NUMBER == lastvisit,
                     .(SITE_IDENTIFIER, sp_lead = SPECIES)]

  surage1 <- merge(surage1,
                   surage2,
                   by = "SITE_IDENTIFIER",
                   all.x = TRUE)
  surage1 <- surage1[SPECIES == sp_lead,
                     .(SITE_IDENTIFIER, VISIT_NUMBER, SPECIES,
                       SPC1_LMEAS_BHAGE, SPC1_LMEAS_TOTAGE)]
  si_lead_secd <- merge(si_lead_secd,
                        surage1,
                        by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                        all.x = TRUE)
  # *note that since stand age is based on the average age
  # of the site trees of the leading species (using si method 1) at the first measurement;
  # *all subsequent measurements are based on this age.  so if the later
  # measurements have a different leading species, then the total age may no longer;
  # *be relevent.  at this time, total age is kept consistent with
  # measurement interval, and is assigned blank if the total age goes less than zero;
  si_lead_secd_standage <- si_lead_secd[,.(SITE_IDENTIFIER,
                                           VISIT_NUMBER,
                                           LEAD_AGE_TOT1,
                                           LEAD_AGE_BH1,
                                           LEAD_AGE_TOT2,
                                           LEAD_AGE_BH2,
                                           LEAD_AGE_TOT3,
                                           LEAD_AGE_BH3,
                                           SPC1_LMEAS_TOTAGE,
                                           SPC1_LMEAS_BHAGE,
                                           SECD_AGE_TOT3,
                                           SECD_AGE_BH3,
                                           LEAD_SP)]
  si_lead_secd_standage[, visit_ref := min(VISIT_NUMBER),
                        by = "SITE_IDENTIFIER"]
  si_lead_secd_standage <- si_lead_secd_standage[VISIT_NUMBER == visit_ref,]
  # *base stand age on average age from si method 1, 2, or 3, in that order of priority, depending whether a age is available for each si method;
  # *first use method 1 if available;
  si_lead_secd_standage[LEAD_AGE_TOT1 >= 4,
                        ':='(tot_age_ref = LEAD_AGE_TOT1,
                             bh_age_ref = LEAD_AGE_BH1,
                             STAND_AGE_SOURCE = "FIRSTM_METH1")]
  si_lead_secd_standage[LEAD_AGE_TOT2 >= 4 & is.na(STAND_AGE_SOURCE),
                        ':='(tot_age_ref = LEAD_AGE_TOT2,
                             bh_age_ref = LEAD_AGE_BH2,
                             STAND_AGE_SOURCE = "FIRSTM_METH2")]
  si_lead_secd_standage[LEAD_AGE_TOT3 >= 4 & is.na(STAND_AGE_SOURCE),
                        ':='(tot_age_ref = LEAD_AGE_TOT3,
                             bh_age_ref = LEAD_AGE_BH3,
                             STAND_AGE_SOURCE = "FIRSTM_METH3")]

  si_lead_secd_standage[SPC1_LMEAS_TOTAGE >= 4 & is.na(STAND_AGE_SOURCE),
                        ':='(tot_age_ref = SPC1_LMEAS_TOTAGE,
                             bh_age_ref = SPC1_LMEAS_BHAGE,
                             STAND_AGE_SOURCE = "LASTM_METH3")]
  si_lead_secd_standage[SECD_AGE_TOT3 >= 4 & is.na(STAND_AGE_SOURCE),
                        ':='(tot_age_ref = SECD_AGE_TOT3,
                             bh_age_ref = SECD_AGE_BH3,
                             STAND_AGE_SOURCE = "FIRSTM_METH3_S2")]
  si_lead_secd_standage[, LAST_SPC1 := LEAD_SP]
  si_lead_secd_standage <- si_lead_secd_standage[,.(SITE_IDENTIFIER,
                                                    visit_ref,
                                                    tot_age_ref,
                                                    bh_age_ref,
                                                    STAND_AGE_SOURCE,
                                                    LAST_SPC1)]
  si_lead_secd <- merge(si_lead_secd,
                        si_lead_secd_standage,
                        by = "SITE_IDENTIFIER",
                        all.x = TRUE)
  # to get sample msmt summaries
  sampmsmt <- unique(treemsmt[,.(SITE_IDENTIFIER, VISIT_NUMBER, MEAS_YR)])
  si_lead_secd <- merge(si_lead_secd,
                        sampmsmt[,.(SITE_IDENTIFIER, VISIT_NUMBER,
                                    MEAS_YR)],
                        by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                        all.x = TRUE)
  si_lead_secd[visit_ref == VISIT_NUMBER,
               MEAS_YR_ref := MEAS_YR]
  si_lead_secd[, MEAS_YR_ref := max(MEAS_YR_ref, na.rm = TRUE),
               by = "SITE_IDENTIFIER"]
  si_lead_secd[,':='(TOT_STAND_AGE = MEAS_YR - MEAS_YR_ref + tot_age_ref,
                     BH_STAND_AGE = MEAS_YR - MEAS_YR_ref + bh_age_ref)]
  si_lead_secd[,':='(SPECIES = NULL,
                     SPC1_LMEAS_BHAGE = NULL,
                     SPC1_LMEAS_TOTAGE = NULL,
                     tot_age_ref = NULL,
                     bh_age_ref = NULL,
                     LAST_SPC1 = NULL,
                     visit_ref = NULL,
                     MEAS_YR = NULL,
                     MEAS_YR_ref = NULL)]
  return(list(smry_by_clsp = smry_by_sp,
              smry_by_cl = si_lead_secd,
              tree_order = treelist_whole))
}


