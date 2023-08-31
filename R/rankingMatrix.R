#' This function is to calculate rating for faib sample sites and to rank them by rating values
#'
#'
#' @description To rank psp sample sites
#'
#'
#'
#' @param archivedPSPPath character, The path to the compiled PSP data, which is configured
#'                        and outputed from \code{ISMCCompiler}.
#' @param archivednonPSPPath character, The path to the compiled nonPSP data, which is configured
#'                        and outputed from \code{ISMCCompiler}.
#' @param useOldCellKey logical, Indicates if using the old cell key from original sas ranking. Default is \code{TRUE}.
#' @return A list of ranking tables including
#'         1) Psp_netdown_summary: net down summary
#'         2) Protected_psp_summary: a summary of protect psp sites by protect code
#'         3)	Remeasured_psp_summary: a summary of remeasured psp sites by last msmt year group
#'         4)	data_source: a description of data used for ranking
#'         5) Ranking_psp: the ranking for psp sample sites
#'         6)	Ranking_matrix: all the raw data and intermediate data used for calculating the rating values
#'
#' @importFrom data.table ':='
#' @importFrom reshape cast
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#' @note As the ISMCCompiler could not produce the same cell key as the previously used. The
#'       function allows using a lookup table to populate the cell key to reach consistency.
#'       However, the cell key is further updated using the site status code from ISMC.
#'
#'
#' @export rankingMatrix
#' @docType methods
#' @rdname rankingMatrix
#'
#' @author Xinjia(Bridget) Guo and Yong Luo
rankingMatrix <- function(archivedPSPPath,
                          archivednonPSPPath = NULL,
                          useOldCellKey = TRUE){
  sample_sites_org <- readRDS(file.path(archivedPSPPath,
                                        "compilation_PSP_db",
                                        "sample_site_header.rds")) %>%
    data.table

  sample_sites <- data.table::copy(sample_sites_org)

  #-----------------------------------------------------------------#
  # temporary updates to site access code, from chris and bryce, 2022-feb-11
  # revised version updated by dan turner 2022feb23
  update1 <- read.xlsx(file.path(archivedPSPPath,
                                 "compilation_coeff",
                                 "Boat Access_Updated_2022Feb24.xlsx")) %>%
    data.table
  update1 <- update1[,.(SITE_IDENTIFIER = Sample_ID,
                        site_access_code_update = gsub(" ", "",
                                                       toupper(Access.Type)))]
  update1 <- update1[, .(site_access_code_update = paste0(site_access_code_update,
                                                          collapse = ", ")),
                     by = "SITE_IDENTIFIER"]
  # need site identifier to link with excel workbook listing updated access description
  sample_sites <- merge(sample_sites, update1,
                        by = "SITE_IDENTIFIER",
                        all.x = TRUE)
  sample_sites[!is.na(site_access_code_update),
               SITE_ACCESS_CODE := site_access_code_update]
  sample_sites[, site_access_code_update := NULL]
  sample_sites <- sample_sites[SAMPLE_ESTABLISHMENT_TYPE %in% c("PSP_G", "PSP_R", "CMI", "VRI",
                                                                "YSM", "NFI", "CMO",
                                                                "T", "I", "EP", "FLT",
                                                                "VLT", "SUP"), ]
  ## this can be achieved in the sample site table
  sample_sites[!is.na(OWNER) & !is.na(SCHEDULE),
               OWN_SCHED := paste0(OWNER, "-", SCHEDULE)]
  # for these three sites, the own_sched mannually assigned as 40-N based
  # on original ranking
  sample_sites[SITE_IDENTIFIER %in% c(4023673, 4023674, 4023722),
               OWN_SCHED := "40-N"]
  sample_sites[, PRIVATE := ifelse(OWN_SCHED %in% c("40-N", "41-N", "52-N", "72-A",
                                                    "77-A", "79-A"),
                                   "Y",
                                   "N")]
  # for samples that have no ownership code assigned, assign VGIS based samples all as MOF owned
  sample_sites[, OWNER_org := OWNER]
  sample_sites[, OWNER := NULL]
  sample_sites[SAMPLE_ESTABLISHMENT_TYPE %in% c("VRI", "CMI", "NFI", "YSM", "CMO",
                                                "EP", "FLT", "VLY", "SUP"),
               OWNER := "MOF"]
  # for remaining samples with no ownership record, assume MOF
  sample_sites[is.na(OWNER), OWNER := "MOF"]

  #-----------------------------------------------------------#
  # get latest sample_site_visit
  sample_msmt_org <- readRDS(file.path(archivedPSPPath,
                                       "compilation_PSP_db",
                                       "sample_msmt_header.rds")) %>%
    data.table
  sample_msmt_org[, ':='(PROJ_ID = NULL,
                         SAMP_NO = NULL,
                         PROJ_AGE_1 = NULL,
                         PROJECTED_DATE = NULL)]
  sample_msmt <- data.table::copy(sample_msmt_org)

  # sample7 <- sample7[, !c('SITE_ACCESS_CODE')]
  # sample7[!is.na(SITE_ACCESS_CODE_UPDATE), SITE_ACCESS_CODE := SITE_ACCESS_CODE_UPDATE]


  sample_msmt[substr(MEAS_DT, 1, 4) %in% c("2020", "2021", "2022"),
              PLAN_YR := as.numeric(substr(MEAS_DT, 1, 4))]

  #-------------------------------------------------------------------#
  # base rating algorithm on psp attributes at the lowest compiled utilization limit

  smry_org <- readRDS(file.path(archivedPSPPath,
                                "compilation_PSP_db",
                                "Smries_volume_byCL.rds"))
  smry_org <- smry_org[, ':='(UTIL_FREQ = length(unique(UTIL)),
                              UTIL_MIN = min(UTIL)),
                       by = "CLSTR_ID"]
  # select summary based on minimum util at each visit
  smry_all <- unique(smry_org[UTIL == UTIL_MIN,],
                     by = "CLSTR_ID")
  smry_all <- merge(smry_all,
                    sample_msmt,
                    by = c("CLSTR_ID"),
                    all.x = TRUE)

  smry_all <- merge(smry_all,
                    sample_sites[,.(SITE_IDENTIFIER, SAMPLE_ESTABLISHMENT_TYPE,
                                    BEC_ZONE, BEC_SBZ, BEC_VAR)],
                    by = "SITE_IDENTIFIER",
                    all.x = TRUE)




  # plot area not populated for some fixed radius sample types
  # note, for those samples that are made up of multiple plots,
  # eg., 3 * 0.015ha plots on a transect, then the evaluation should be based on the ;
  # individual plot area, and not the combined area of all plots within a sample id;
  # this is the area of the individual plot. the total area of all plots in a sample id
  # would be (area_pm * no_plots);
  samp_plot <- readRDS(file.path(archivedPSPPath,
                                 "compilation_PSP_db",
                                 "sample_plot_header.rds"))
  smry_all <- merge(smry_all,
                    unique(samp_plot[,.(CLSTR_ID, PLOT_AREA_MAIN)],
                           by = "CLSTR_ID"),
                    by = "CLSTR_ID",
                    all.x = TRUE)

  smry_all[, AREA := ifelse(SAMPLE_ESTABLISHMENT_TYPE %in% c("CMI", "YSM", "NFI", "CMO", "FLT"),
                            0.04,
                            ifelse(SAMPLE_ESTABLISHMENT_TYPE == "SUP" & SAMP_TYP == "F",
                                   0.04,
                                   PLOT_AREA_MAIN))]


  smry_all[, DBH_LIMIT_TAG := ifelse(SAMPLE_ESTABLISHMENT_TYPE %in% c("CMI", "YSM", "NFI", "CMO","FLT"), 4.0,
                                     ifelse(SAMPLE_ESTABLISHMENT_TYPE == "SUP" & SAMP_TYP == "F",
                                            4.0,
                                            DBH_LIMIT_TAG))]

  smry_all[, STEMS_HA_L := STEMS_HA_LS + STEMS_HA_LF] # all live trees regardless of standing or falling


  smry_all[, AREA_PM_MIN := ifelse(round(AREA, digits = 2) >= 0.04 & !is.na(AREA),
                                   "Y", "N")]

  treelist_org <- readRDS(file.path(archivedPSPPath,
                                    "compilation_PSP_db",
                                    "treelist.rds")) %>%
    data.table

  treelist <- treelist_org[MEASUREMENT_ANOMALY_CODE != "PSP-TALLY" |
                                 is.na(MEASUREMENT_ANOMALY_CODE) &
                             VOLUME_TREE == "Y",]
  # compute an number of live trees per plot computed at the minimum dbh limit
  tree_count_live <- treelist_org[LV_D == "L" &
                                    VOLUME_TREE == "Y",
                                  .(TREE_COUNT = length(DBH)),
                                  by = "CLSTR_ID"]

  smry_all <- merge(smry_all,
                    tree_count_live,
                    by = "CLSTR_ID",
                    all.x = TRUE)



  # track calendar year
  smry_all[, CALEND_YR := as.numeric(substr(MEAS_DT, 1, 4))]

  age_smry <- readRDS(file.path(archivedPSPPath,
                                "compilation_PSP_db",
                                "Smries_siteAge_byCL.rds"))

  smry_all <- merge(smry_all,
                    age_smry[,.(CLSTR_ID, TOT_STAND_AGE,
                                LEAD_SI_FINAL1,
                                LEAD_SI_FINAL2,
                                LEAD_SI_FINAL3,
                                LEAD_HTOP1,
                                LEAD_HTOP2,
                                LEAD_HTOP3,
                                BH_STAND_AGE)],
                    by = "CLSTR_ID",
                    all.x = TRUE)
  # some attributes are defined based on first measurement, ie., at establishment
  # ie., species, age class, density, total number of trees.  site index, included here,
  # but not critical, since si was previously assigned based on the measurement closest to 50yr bh age;
  # bec variant and site series included here

  # ------- smry_1st codes -------------------------------------------#
  # keep only first measure record
  smry_1st <- smry_all[FIRST_MSMT == "Y",]

  # new 7 class age classes, as per 2019 strategic plan
  # bring tot stand age

  smry_1st[, ":="(TOT_STAND_AGE = as.numeric(TOT_STAND_AGE))]

  smry_1st[, AGE_STRATA := as.character(cut(TOT_STAND_AGE,
                                            breaks = c(seq(0, 240, by = 40),
                                                       max(as.numeric(TOT_STAND_AGE),
                                                           na.rm = TRUE)),
                                            labels = c("001-040YR",
                                                       "041-080YR",
                                                       "081_120YR",
                                                       "121_160YR",
                                                       "161_200YR",
                                                       "201_240YR",
                                                       ">240YR")))]

 # for psp ranking the species composition is summarized using live non-residual trees
 # and based on original species in the database

   treemsmts_raw <- readRDS(dir(file.path(archivedPSPPath,
                                 "compilation_PSP_raw"),
                               pattern = "TreeMeasurements.rds",
                               full.names = TRUE))
  treemsmts_raw <- treemsmts_raw[,.(SITE_IDENTIFIER,
                                    PLOT = PLOT_NUMBER,
                                    TREE_NO = TREE_NUMBER,
                                    VISIT_NUMBER,
                                    TREE_SPECIES_CODE)]
  treelist_org <- merge(treelist_org,
                        treemsmts_raw,
                        by = c("SITE_IDENTIFIER", "PLOT",
                               "TREE_NO", "VISIT_NUMBER"),
                        all.x = TRUE)
  ba_smry <- treelist_org[VOLUME_TREE == "Y" &
                            LV_D == "L" &
                            S_F %in% c("S", NA) &
                            RESIDUAL_IND %in% c("N", NA) &
                            (MEASUREMENT_ANOMALY_CODE != "PSP-TALLY" |
                               is.na(MEASUREMENT_ANOMALY_CODE)),
                          .(BA_SP = sum(BA_TREE)),
                          by = c("CLSTR_ID", "TREE_SPECIES_CODE")]
  ba_smry[, SP_PCT_BA_LS := round(100*BA_SP/sum(BA_SP), 2),
          by = "CLSTR_ID"]
  ## correct some species codes
  ba_smry[TREE_SPECIES_CODE == "B",
          TREE_SPECIES_CODE := "BL"]
  ba_smry[TREE_SPECIES_CODE == "H",
          TREE_SPECIES_CODE := "HW"]
  ba_smry[TREE_SPECIES_CODE == "S",
          TREE_SPECIES_CODE := "SX"]
  ba_smry[TREE_SPECIES_CODE == "W",
          TREE_SPECIES_CODE := "SX"]
  ba_smry[TREE_SPECIES_CODE == "L",
          TREE_SPECIES_CODE := "LW"]
  ba_smry[TREE_SPECIES_CODE == "SW",
          TREE_SPECIES_CODE := "SX"]


  # vol_smry_cs <- readRDS(file.path(archivedPSPPath,
  #                                  "compilation_PSP_db",
  #                                  "Smries_volume_byCLSP.rds"))
  leadingspecies <- ba_smry[order(CLSTR_ID, -SP_PCT_BA_LS),
                                .(CLSTR_ID,
                                  SPC1 = TREE_SPECIES_CODE,
                                  SP_PCT_BA_LS)]
  leadingspecies <- unique(leadingspecies,
                           by = c("CLSTR_ID"))

  # species strata
  # use 80% species as per 2019 strategic plan

  leadingspecies[, SPC_STRATA := ifelse(SP_PCT_BA_LS > 80,
                                        sprintf("%sPURE", SPC1),
                                        ifelse(SP_PCT_BA_LS >0 & SP_PCT_BA_LS <= 80,
                                               sprintf("%sMIX", SPC1),
                                               NA))]
  smry_1st <- merge(smry_1st,
                    leadingspecies,
                    by = c("CLSTR_ID"),
                    all.x = TRUE)
  # density strata
  smry_1st[, DEN_STRATA := ifelse(STEMS_HA_L > 5000,
                                  ">5000SPH",
                                  ifelse(STEMS_HA_L > 1000 & STEMS_HA_L <= 5000,
                                         "1001-5000SPH",
                                         ifelse(STEMS_HA_L > 0 & STEMS_HA_L <= 1000,
                                                "<=1000SPH", NA)))]

  # site index
  # decide on source of available site index estimates
  smry_1st[, ":="(LEAD_SI1 = as.numeric(LEAD_SI_FINAL1),
                  LEAD_SI2 = as.numeric(LEAD_SI_FINAL2),
                  LEAD_SI3 = as.numeric(LEAD_SI_FINAL3),
                  LEAD_SI4 = as.numeric(NA))]

  smry_1st[, SI_USE := ifelse(LEAD_SI1 < 1 | is.na(LEAD_SI1),
                              ifelse(LEAD_SI2 < 1 | is.na(LEAD_SI2),
                                     ifelse(LEAD_SI3 < 1 | is.na(LEAD_SI3),
                                            ifelse(LEAD_SI4 < 1 | is.na(LEAD_SI4), NA,
                                                   LEAD_SI4),
                                            LEAD_SI3),
                                     LEAD_SI2),
                              LEAD_SI1)]

  smry_1st[, SI_STRATA := ifelse(SI_USE > 0 & SI_USE <= 12.5, "<=12.5M",
                                 ifelse(SI_USE > 12.5 & SI_USE <= 17.5, "12.6-17.5M",
                                        ifelse(SI_USE > 17.5 & SI_USE <= 22.5, "17.6-22.5M",
                                               ifelse(SI_USE > 22.5 & SI_USE <= 27.5, "22.6-27.5M",
                                                      ifelse(SI_USE > 27.5 & SI_USE <= 32.5, "27.6-32.5M",
                                                             ifelse(SI_USE > 32.5, ">32.5M", NA))))))]

  # bec strata
  # new bec strata classification based on subzone, as per 2019 strategic plan
  smry_1st[, BEC_STRATA := ifelse(!is.na(BEC_ZONE) & !is.na(BEC_SBZ),
                                  paste0(BEC_ZONE, BEC_SBZ), NA)]
  smry_1st[, REG := ifelse(BEC_ZONE %in% c("CWH", "CDF", "MH", "CMA"),
                           "C", "I")]
  # site series strata

  smry_1st <- smry_1st[,.(SITE_IDENTIFIER,
                          AGE_STRATA,
                          SPC_STRATA,
                          DEN_STRATA,
                          SI_STRATA,
                          BEC_STRATA,
                          TREE_COUNT_FST_MSMT = TREE_COUNT,
                          SI_USE,
                          REG,
                          SPC1)]
  sample_sites <- merge(sample_sites,
                        smry_1st,
                        by = "SITE_IDENTIFIER",
                        all.x = TRUE)

  sample_sites[, ':='(SS_STRATA = BGC_SS_GRD,
                      BGC_SS_GRD_FST_MSMT = BGC_SS_GRD)]

  # some attributes are defined based on last measurement only
  # i.e. treatment, disturbance history
  #------- smry_last --------------------------------------------------------#

  # keep only last measure
  smry_last <- smry_all[LAST_MSMT == "Y",]
  setnames(smry_last, "CALEND_YR", "CALEND_YR_LAST")
  smry_last <- smry_last[,.(SITE_IDENTIFIER,
                            CALEND_YR_LAST,
                            STEM_MAPPED_SAMPLE,
                            AREA,
                            AREA_PM_MIN,
                            NO_PLOTS,
                            TOT_STAND_AGE,
                            LEAD_SI_FINAL1,
                            LEAD_SI_FINAL2,
                            LEAD_SI_FINAL3,
                            LEAD_HTOP1,
                            LEAD_HTOP2,
                            LEAD_HTOP3,
                            BH_STAND_AGE,
                            TREE_COUNT_LAST_MSMT = TREE_COUNT)]

  # treatment strata
  sample_sites[, TRT_STRATA := ifelse(TREATMENT == "THINNED" & !is.na(TREATMENT), "T",
                                      "U")]
  sample_sites <- merge(sample_sites,
                        smry_last,
                        by = "SITE_IDENTIFIER",
                        all.x = TRUE)
  # status strata
  sample_sites[, STS_STRATA := ifelse(SITE_STATUS_CODE != "A", "X",
                                      SITE_STATUS_CODE)]
  #-----------------------------------------------#
  # create cell matrix definitions
  sample_sites[, CELL_KEY := paste(STS_STRATA, # from last measurement
                                   TRT_STRATA, # from last measurement
                                   BEC_STRATA, # from first measurement
                                   SPC_STRATA, # from first measurement
                                   AGE_STRATA, # from first measurement
                                   DEN_STRATA, # from first measurement
                                   SI_STRATA, # from first measurement
                                   sep = "_")]

  # cell completeness flag. samples with incomplete criteria cannot be reasonably assigned a cell class

  sample_sites[, CELL_COMPLETE := ifelse(is.na(AGE_STRATA) |
                                           is.na(SPC_STRATA) |
                                           is.na(DEN_STRATA) |
                                           is.na(SI_STRATA) |
                                           is.na(BEC_STRATA),
                                         "N", "Y")]

  sample_sites[CELL_COMPLETE == "N",
               CELL_KEY := "MISS_STRATA"]

    oldcellkeys <- fread(file.path(archivedPSPPath,
                                   "compilation_coeff", "cellkeylookup.csv"))
  if(useOldCellKey){
    sample_sites <- merge(sample_sites,
                          oldcellkeys[,.(SITE_IDENTIFIER, SAMP_ID, CELL_KEY_org,
                                         PROTECT_CODE,
                                         CELL_COMPLETE_org, STS_STRATA_org,
                                         TRT_STRATA_org, BEC_STRATA_org,
                                         SPC_STRATA_org, AGE_STRATA_org,
                                         DEN_STRATA_org, SI_STRATA_org)],
                          by = "SITE_IDENTIFIER",
                          all.x = TRUE)
    sample_sites[, ':='(CELL_KEY_new = CELL_KEY,
                      CELL_COMPLETE_new = CELL_COMPLETE,
                      STS_STRATA_new = STS_STRATA,
                      TRT_STRATA_new = TRT_STRATA,
                      BEC_STRATA_new = BEC_STRATA,
                      SPC_STRATA_new = SPC_STRATA,
                      AGE_STRATA_new = AGE_STRATA,
                      DEN_STRATA_new = DEN_STRATA,
                      SI_STRATA_new = SI_STRATA)]
    sample_sites[!is.na(CELL_KEY_org),
                 ':='(CELL_COMPLETE = CELL_COMPLETE_org,
                      TRT_STRATA = TRT_STRATA_org,
                      BEC_STRATA = BEC_STRATA_org,
                      SPC_STRATA = SPC_STRATA_org,
                      AGE_STRATA = AGE_STRATA_org,
                      DEN_STRATA = DEN_STRATA_org,
                      SI_STRATA = SI_STRATA_org)]
    sample_sites[, CELL_KEY := paste(STS_STRATA, # from last measurement
                                     TRT_STRATA, # from last measurement
                                     BEC_STRATA, # from first measurement
                                     SPC_STRATA, # from first measurement
                                     AGE_STRATA, # from first measurement
                                     DEN_STRATA, # from first measurement
                                     SI_STRATA, # from first measurement
                                     sep = "_")]
  } else {
    sample_sites <- merge(sample_sites,
                          oldcellkeys[,.(SITE_IDENTIFIER, SAMP_ID,
                                         PROTECT_CODE)],
                          by = "SITE_IDENTIFIER",
                          all.x = TRUE)
  }

  # need to get access note data, as part of assessment of keeping a sample
  # ie., a sample needs to have either a gps coordinate or access notes, otherwise do not keep
  ac_ismc <- readRDS(file.path(archivedPSPPath,
                                "compilation_PSP_sa",
                                "siteaccessnotes.rds"))
  ac1 <- merge(sample_sites,
               ac_ismc[,.(SITE_IDENTIFIER, ACCESS_NOTES)],
               by = "SITE_IDENTIFIER",
               all.x = TRUE)


  # create access coord flag, and access notes flag
  ac1[, ACCESS_COORD := ifelse(IP_UTM > 0 & IP_NRTH > 0 & IP_EAST > 0,
                               "Y", "N")]
  ac1[is.na(ACCESS_COORD), ACCESS_COORD := "N"]


  ac1[, ACCESS_NOTE := ifelse(!is.na(ACCESS_NOTES), "Y", "N")]

  ac1[, ACCESS := ifelse(ACCESS_COORD == "N" & ACCESS_NOTE == "N",
                         "N",
                         ifelse(ACCESS_COORD == "Y" | ACCESS_NOTE == "Y",
                                "Y", NA))]
  ac1 <- ac1[,.(SITE_IDENTIFIER,
                ACCESS_COORD,
                ACCESS_NOTE,
                ACCESS)]
  sample_sites <- merge(sample_sites,
                        ac1,
                        by = "SITE_IDENTIFIER",
                        all.x = TRUE)


  # modify access flag, to drop boat access samples
  # and also to drop psps in parks

  sample_sites[ACCESS == "Y" & OWN_SCHED == "51-N",
               SITE_ACCESS_CODE := ifelse(!is.na(SITE_ACCESS_CODE),
                                          paste("NAT_PARK", SITE_ACCESS_CODE, sep = "_"),
                                          "NAT_PARK")]

  sample_sites[ACCESS == "Y" & OWN_SCHED == "51-N",
               ACCESS := "N"]

  sample_sites[ACCESS == "Y" &
                 SITE_ACCESS_CODE %in% c("BOAT", "BOAT/HELI") &
                 (OWN_SCHED != "51-N" | is.na(OWN_SCHED)) &
                 CALEND_YR_LAST <= 2021,
               ACCESS := "N"]


  #-------------------------------------#
  smry_all[, ':='(VHA_WSV_L = VHA_WSV_LF + VHA_WSV_LS,
                  VHA_MER_L = VHA_MER_LF + VHA_MER_LS)]

  mastertable <- unique(smry_all[,.(SITE_IDENTIFIER)])

  interestcols <- c("PERIOD", "WSVHA_L", "WSVHA_V",
                    "TOT_STAND_AGE", "MEAS_YR", "DBH_LIMIT_TAG",
                    "TREE_COUNT")
  maxVisit <- max(smry_all$VISIT_NUMBER)
  # need attention
  # what is wsvha_l and wsvha_v
  interestcols <- c("PERIOD", "VHA_WSV_L", # whole stem vol for all live trees
                    "VHA_MER_L", # merchantable vol for all live trees
                    "TOT_STAND_AGE", "MEAS_YR", "DBH_LIMIT_TAG",
                    "TREE_COUNT")

  for (indicolname in interestcols) {
    indidata <- smry_all[,c("SITE_IDENTIFIER", "VISIT_NUMBER", indicolname),
                         with = FALSE]

    indidata %<>%
      melt(id = c("SITE_IDENTIFIER", "VISIT_NUMBER")) %>%
      reshape::cast(SITE_IDENTIFIER~VISIT_NUMBER) %>%
      setDT()

    if(indicolname == "PERIOD"){
      prefix <- "MINT_"
    } else if (indicolname == "VHA_WSV_L"){
      prefix <- "VHA_WSV_L_"
    } else if (indicolname == "VHA_MER_L"){
      prefix <- "VHA_MER_L_"
    } else if (indicolname == "TOT_STAND_AGE"){
      prefix <- "TAGE_"
    } else if (indicolname == "MEAS_YR"){
      prefix <- "MEASYR_"
    } else if (indicolname == "DBH_LIMIT_TAG"){
      prefix <- "DBHLIM_"
    } else if (indicolname == "TREE_COUNT"){
      prefix <- "NTREES_"
    }

    names(indidata)[2:(maxVisit+1)] <- paste0(prefix, names(indidata)[2:(maxVisit+1)])
    mastertable <- merge(mastertable,
                         indidata,
                         by = "SITE_IDENTIFIER",
                         all.x = TRUE)
  }

  #-------------------------------------------------------------#
  # get num hts;

  # treelist_org <- read.xlsx(file.path(wkdir,
  #                              "sasds",
  #                              "psp_tree_all_small.xlsx")) %>%
  #   data.table

  treelist_nhts <- treelist[HT_TOTAL_SOURCE == "Field measured" &
                                  SUIT_HT == "Y" &
                                  LV_D == "L",
                                .(CLSTR_ID, HEIGHT, SUIT_HT, LV_D)]

  treelist_nhts <- merge(treelist_nhts,
                         sample_msmt[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER)],
                         by = c("CLSTR_ID"),
                         all.x = TRUE)
  numhts <- treelist_nhts[!is.na(HEIGHT),
                          .(NHTS = length(HEIGHT)),
                          by = c("SITE_IDENTIFIER", "VISIT_NUMBER")]
  numhts %<>%
    melt(id = c("SITE_IDENTIFIER", "VISIT_NUMBER")) %>%
    cast(SITE_IDENTIFIER~VISIT_NUMBER) %>%
    setDT()
  colnames(numhts)[2:(maxVisit+1)] <- paste0("NHTS_", colnames(numhts)[2:(maxVisit+1)])

  mastertable <- merge(mastertable,
                       numhts,
                       by = "SITE_IDENTIFIER",
                       all.x = TRUE)

  psp4b <- merge(sample_sites[,.(SITE_IDENTIFIER,
                                 CALEND_YR_LAST,
                                 MEAS_YR_LAST,
                                 NO_MEAS)],
                 mastertable,
                 by = "SITE_IDENTIFIER",
                 all.x = TRUE)
  #-----------------------------------------------------------#\
  # update ratings with planned remeasures for the upcoming planning season

  psp4b[, ":=" (PLAN_YR = as.numeric(CALEND_YR_LAST),
                MEAS_YR_LAST = as.numeric(MEAS_YR_LAST),
                NO_MEAS = as.numeric(NO_MEAS))]

  psp4b_simp <- psp4b[, c("SITE_IDENTIFIER", "PLAN_YR", "MEAS_YR_LAST", "NO_MEAS",
                          paste0("MINT_", 1:(maxVisit)),
                          paste0("NTREES_", 1:(maxVisit)),
                          paste0("NHTS_", 1:(maxVisit)),
                          paste0("DBHLIM_", 1:(maxVisit))),
                      with = FALSE]

  psp4b_simp[, YR_CALC := PLAN_YR - MEAS_YR_LAST]
  no_meas_in <- sort(unique(psp4b_simp[PLAN_YR >= 2020, ]$NO_MEAS))

  #View(psp4b_simp[SAMP_ID == "01001 G000518", .(SAMP_ID, YR_CALC,
  #                                              PLAN_YR, MEAS_YR_LAST)])

  for(i in no_meas_in){
    setnames(psp4b_simp,
             paste0(c("MINT_", "NTREES_", "NHTS_", "DBHLIM_"), i),
             c("MINT_crt", "NTREES_crt", "NHTS_crt", "DBHLIM_crt"))
    setnames(psp4b_simp,
             paste0(c("MINT_", "NTREES_", "NHTS_", "DBHLIM_"), (i-1)),
             c("MINT_prv", "NTREES_prv", "NHTS_prv", "DBHLIM_prv"))

    psp4b_simp[PLAN_YR >= 2020 & NO_MEAS == i, MINT_crt := YR_CALC]
    psp4b_simp[PLAN_YR >= 2020 & NO_MEAS == i, NTREES_crt := NTREES_prv]
    psp4b_simp[PLAN_YR >= 2020 & NO_MEAS == i, NHTS_crt := NHTS_prv]
    psp4b_simp[PLAN_YR >= 2020 & NO_MEAS == i, DBHLIM_crt := DBHLIM_prv]

    setnames(psp4b_simp,
             c("MINT_crt", "NTREES_crt", "NHTS_crt", "DBHLIM_crt"),
             paste0(c("MINT_", "NTREES_", "NHTS_", "DBHLIM_"), i))

    setnames(psp4b_simp,
             c("MINT_prv", "NTREES_prv", "NHTS_prv", "DBHLIM_prv"),
             paste0(c("MINT_", "NTREES_", "NHTS_", "DBHLIM_"), (i-1)))

  }

  psp4b <- psp4b[, c(paste0("MINT_", 1:maxVisit),
                     paste0("NTREES_", 1:maxVisit),
                     paste0("NHTS_", 1:maxVisit),
                     paste0("DBHLIM_", 1:maxVisit)) := NULL]

  psp4b_simp[, ':='(PLAN_YR = NULL,
                    MEAS_YR_LAST = NULL,
                    NO_MEAS = NULL) ]
  psp4b <- merge(psp4b, psp4b_simp,
                 by = "SITE_IDENTIFIER")

  # reassign last measurement year to the planned measurement year

  psp4b[,
        ":=" (MEAS_YR_LAST = NULL,
              CALEND_YR_LAST = NULL,
              NO_MEAS = NULL)]
  sample_sites <- merge(sample_sites,
                        psp4b,
                        by = "SITE_IDENTIFIER",
                        all.x = TRUE)
  psp5 <- data.table::copy(sample_sites)
  # assign coefficients

  psp5[, ":=" (a = 2.069,
               b = 66.660,
               c = 3.18,
               d = 0.0704,
               e = 0.05,
               f = 0.989,
               g = 0.0614,
               h = -0.171)]

  # compute rating components
  # plot size
  # given prism VRI plots a 1.0, as this test is not relevant for prism plots
  psp5[, P1 := ifelse(SAMPLE_ESTABLISHMENT_TYPE %in% "VRI", 1.0,
                      ifelse(AREA > 0 & !is.na(AREA),
                             exp(-exp(a-(b*AREA))),
                             0))]

  # total number of trees is evaluated, based on first msmt, to be consistent
  # with original fpc ranking standards

  # evaluation of total number of trees should be based on the individual plot
  # for those samples with multiple plots in a given sample id;
  # use the average density across all plots as the basis

  psp5[, NO_PLOTS := as.numeric(NO_PLOTS)]

  psp5[, P2 := ifelse(NTREES_1 > 0 & !is.na(NTREES_1),
                      exp(-exp(c-(d*(NTREES_1/NO_PLOTS)))),
                      0)]

  # two of age, htop, si
  # get top height and si source, use priority list

  psp5[, LEAD_HTOP := ifelse(LEAD_HTOP1 > 0 & !is.na(LEAD_HTOP1),
                             LEAD_HTOP1,
                             ifelse(LEAD_HTOP2 > 0 & !is.na(LEAD_HTOP2),
                                    LEAD_HTOP2,
                                    ifelse(LEAD_HTOP3 > 0 & !is.na(LEAD_HTOP3),
                                           LEAD_HTOP3,
                                           NA)))]

  psp5[, ":=" (TOT_STAND_AGE = as.numeric(TOT_STAND_AGE),
               LEAD_HTOP = as.numeric(LEAD_HTOP),
               BH_STAND_AGE = as.numeric(BH_STAND_AGE),
               BUFFER_RAD = as.numeric(NA))]

  psp5[, P3 := ifelse(BH_STAND_AGE > 0 & LEAD_HTOP > 0
                      & !is.na(BH_STAND_AGE) & !is.na(LEAD_HTOP),
                      1.0,
                      ifelse(TOT_STAND_AGE > 0 & LEAD_HTOP > 0
                             & !is.na(TOT_STAND_AGE) & !is.na(LEAD_HTOP),
                             0.95,
                             ifelse(BH_STAND_AGE > 0 & SI_USE > 0
                                    & !is.na(BH_STAND_AGE) & !is.na(SI_USE),
                                    0.90,
                                    ifelse(TOT_STAND_AGE > 0 & SI_USE > 0
                                           & !is.na(TOT_STAND_AGE) & !is.na(SI_USE),
                                           0.85,
                                           0))))]

  # stem mapping of all trees

  psp5[, P4 := ifelse(STEM_MAPPED_SAMPLE == TRUE,
                      1.0, 0)]

  # activity status assume missing is "A"
  psp5[, P5 := ifelse(SITE_STATUS_CODE == "A" | is.na(SITE_STATUS_CODE),
                      1.0, 0)]
  # presence of buffer
  # as per Anya's comment on 2023-04-21
  # buffer is no longer used for ranking process
  psp5[, P6 := 0]

  # by measurement ranks
  psp5[MINT_1 %in% c(0, NA), MINT_1 := 10]
  # for single-measurement samples, need to have a default interval
  psp5_simp <- psp5[,c("SITE_IDENTIFIER",
                       paste0("NTREES_", 1:maxVisit),
                       paste0("MINT_", 1:maxVisit),
                       paste0("NHTS_", 1:maxVisit),
                       paste0("DBHLIM_", 1:maxVisit),
                       letters[1:8],
                       paste0("P", 1:6)),
                    with = FALSE]

  psp5_simp[, 2:ncol(psp5_simp)] <- lapply(2:ncol(psp5_simp),
                                           function(x)
                                             as.numeric(psp5_simp[[x]]))
  psp5_simp[, MT := 0]
  for (j in 1:maxVisit) {
    setnames(psp5_simp,
             paste0(c("NTREES_", "MINT_", "NHTS_", "DBHLIM_"), j),
             c("NTREES", "MINT", "NHTS", "DBHLIM"))
    psp5_simp[NTREES > 0, M1 := (1+(e*MINT)) * exp(-e*MINT)]

    psp5_simp[NTREES > 0 &
                NHTS == NTREES,
              M2 := 1]

    psp5_simp[NTREES > 0 & NHTS >= 6 &
                is.na(M2),
              M2 := (1 - f*exp(-g*NHTS))]

    psp5_simp[NTREES > 0 & is.na(M2),
              M2 := (0.4*(1-f*exp(-g*6)))]

    psp5_simp[NTREES > 0,
              M3 := (exp(h*DBHLIM))]


    psp5_simp[NTREES > 0,
              MT := MT + (5 * MINT * M1 * M2 * M3)]

    psp5_simp[,c("NTREES", "MINT", "NHTS", "DBHLIM", "M1",
                 "M2", "M3") := NULL]

  }

  psp5_simp[, RATING := round(30*P1 + 30*P2 + 20*P3 + 5*P4 + 5*P5 + 20*P6 + MT,
                              3)]

  psp5 <- merge(psp5,
                psp5_simp[,.(SITE_IDENTIFIER, MT, RATING)],
                by = "SITE_IDENTIFIER",
                all.x = TRUE)

  #-------------------------------------------------#
  # prep for ranking
  # this will rank only active Growth natural MOF and Industry owned,
  # in crown forest, >=0.04ha, with access info, measured since 1996
  # determine priority samples per cell_key for psp growth naturals only samples
  ranking_psp <- psp5[SAMPLE_ESTABLISHMENT_TYPE %in% c("PSP_G", "PSP_R"),
                      .(CELL_KEY, CELL_COMPLETE, RATING, SITE_IDENTIFIER,
                        MGMT_UNIT, OWNER, MEAS_YR_FIRST, MEAS_YR_LAST,
                        TOTAL_PERIOD, ACCESS, STS_STRATA, OWN_SCHED,
                        AREA, NO_MEAS)]
  ranking_psp_proc <- psp5[SAMPLE_ESTABLISHMENT_TYPE %in% c("PSP_G", "PSP_R") &
                             STS_STRATA == "A" &
                             PRIVATE == "N" & AREA_PM_MIN == "Y" &
                             ACCESS == "Y" & MEAS_YR_LAST >= 1996 &
                             CELL_COMPLETE == "Y",]
  ranking_psp_proc <- ranking_psp_proc[order(CELL_KEY, -RATING),]
  ranking_psp_proc[,':='(RANK_PSP = 1:length(RATING),
                         NO_SITE_PER_CELL_PSP = length(RATING)),
                   by = "CELL_KEY"]
  ranking_psp_proc[CELL_COMPLETE == "N",
                   ':='(RANK_PSP = NA,
                        NO_SITE_PER_CELL_PSP = NA)]
  ranking_psp <- merge(ranking_psp,
                       ranking_psp_proc[,.(SITE_IDENTIFIER, RANK_PSP, NO_SITE_PER_CELL_PSP)],
                       by = "SITE_IDENTIFIER",
                       all.x = TRUE)
  ranking_psp <- ranking_psp[order(CELL_KEY, RANK_PSP),]

  # this will rank all PSPs regardless of ownership, in crown forest,
  # >=0.04ha, with access info, measured since 1996
  # rank by CELL_KEY
  # determine priority samples per cell_key for all samples
  ranking_all <- psp5[SAMPLE_ESTABLISHMENT_TYPE %in% c("PSP_G", "PSP_R", "VRI",
                                                       "CMI", "YSM", "CMO",
                                                       "EP", "FLT", "VLT", "SUP"),
                      .(CELL_KEY, CELL_COMPLETE, RATING, SITE_IDENTIFIER,
                        MGMT_UNIT, OWNER, MEAS_YR_FIRST, MEAS_YR_LAST,
                        TOTAL_PERIOD, ACCESS, STS_STRATA, OWN_SCHED,
                        AREA, NO_MEAS)]
  ranking_all_proc <- psp5[STS_STRATA == "A" &
                             SAMPLE_ESTABLISHMENT_TYPE %in% c("PSP_G", "PSP_R", "VRI",
                                                              "CMI", "YSM", "CMO",
                                                              "EP", "FLT", "VLT", "SUP") &
                             PRIVATE == "N" & (AREA_PM_MIN == "Y" | SAMPLE_ESTABLISHMENT_TYPE == "VRI") &
                             ACCESS == "Y" & MEAS_YR_LAST >= 1996 & CELL_COMPLETE == "Y",]
  ranking_all_proc <- ranking_all_proc[order(CELL_KEY, -RATING),]
  ranking_all_proc[,':='(RANK_ALL = 1:length(RATING),
                         NO_SITE_PER_CELL_ALL = length(RATING)),
                   by = "CELL_KEY"]
  ranking_all_proc[CELL_COMPLETE == "N",
                   ':='(RANK_ALL = NA,
                        NO_SITE_PER_CELL_ALL = NA)]
  ranking_all <- merge(ranking_all,
                       ranking_all_proc[,.(SITE_IDENTIFIER, RANK_ALL, NO_SITE_PER_CELL_ALL)],
                       by = "SITE_IDENTIFIER",
                       all.x = TRUE)
  ranking_all <- ranking_all[order(CELL_KEY, RANK_ALL),]
  psp5 <- merge(psp5,
                ranking_psp[,.(SITE_IDENTIFIER, RANK_PSP, NO_SITE_PER_CELL_PSP)],
                by = "SITE_IDENTIFIER",
                all.x = TRUE)
  psp5 <- merge(psp5,
                ranking_all[,.(SITE_IDENTIFIER, RANK_ALL, NO_SITE_PER_CELL_ALL)],
                by = "SITE_IDENTIFIER",
                all.x = TRUE)
  psp5[, ':='(PROTECT_PSP = "N")]
  psp5[PROTECT_CODE == "C" & !is.na(RANK_PSP),
       ':='(PROTECT_PSP = "Y")]
  psp5[RANK_PSP == 1 &
         NO_MEAS > 1 &
         is.na(PROTECT_CODE),
       ':='(PROTECT_PSP = "Y",
            PROTECT_CODE = "A")]

  psp5[RANK_PSP == 1 &
         NO_MEAS == 1 &
         BEC_ZONE %in% c("BWBS", "ESSF") &
         is.na(PROTECT_CODE),
       ':='(PROTECT_PSP = "Y",
            PROTECT_CODE = "B")]
  ## procect_code = C, species psps, need to figure out

  psp5[RANK_PSP == 2 &
         NO_MEAS > 1 &
         is.na(PROTECT_CODE),
       ':='(PROTECT_PSP = "Y",
            PROTECT_CODE = "D")]
  psp5[RANK_PSP %in% c(3, 4, 5) &
         NO_MEAS > 1 &
         TOTAL_PERIOD >= 30 &
         is.na(PROTECT_CODE),
       ':='(PROTECT_PSP = "Y",
            PROTECT_CODE = "E")]
  psp5[PROTECT_CODE == "A",
       PROTECT_CRITERIA := "Rank1_multiple_msmt"]
  psp5[PROTECT_CODE == "B",
       PROTECT_CRITERIA := "Rank1_single_msmt"]
  psp5[PROTECT_CODE == "C",
       PROTECT_CRITERIA := "New_DR_CW_Projects"]
  psp5[PROTECT_CODE == "D",
       PROTECT_CRITERIA := "Rank2_multiple_msmt"]
  psp5[PROTECT_CODE == "E",
       PROTECT_CRITERIA := "30yr_plus_period"]
  psp5[substr(MEAS_DT_LAST, 1, 4) <= 1995,
       LAST_MSMT_GRP := "<=1995"]
  psp5[substr(MEAS_DT_LAST, 1, 4) >= 1996 & substr(MEAS_DT_LAST, 1, 4) <= 2000,
       LAST_MSMT_GRP := "1996_2000"]
  psp5[substr(MEAS_DT_LAST, 1, 4) >= 2001 & substr(MEAS_DT_LAST, 1, 4) <= 2010,
       LAST_MSMT_GRP := "2001_2010"]
  psp5[substr(MEAS_DT_LAST, 1, 4) >= 2011,
       LAST_MSMT_GRP := ">=2011"]
  ## to produce netdown table
  netdown_table <- NULL
  netdown1 <- data.table::copy(psp5)
  set(netdown1, , c(paste0("VHA_WSV_L_", 1:maxVisit),
                    paste0("VHA_MER_L_", 1:maxVisit),
                    paste0("TAGE_", 1:maxVisit),
                    paste0("MEASYR_", 1:maxVisit),
                    paste0("MINT_", 1:maxVisit),
                    paste0("NTREES_", 1:maxVisit),
                    paste0("NHTS_", 1:maxVisit),
                    paste0("DBHLIM_", 1:maxVisit),
                    paste0("P", 1:6), "MT",
                    letters[1:8]), NULL)
  netdown_table <- rbind(netdown_table,
                      data.table(`Retention Criteria` = "Total # Ground Samples @ FAIB",
                                 `Removal Criteria` = as.character(NA),
                                 `# Sample Removed` = as.numeric(NA),
                                 `# Sample Remaining` = nrow(netdown1)))
  sampleRemain <- nrow(netdown1)

  netdown1 <- netdown1[SAMPLE_ESTABLISHMENT_TYPE %in% c("PSP_R", "PSP_G"),]
  netdown_table <- rbind(netdown_table,
                      data.table(`Retention Criteria` = "G & R sample types",
                                 `Removal Criteria` = "CMI, VRI, YSM, & Other",
                                 `# Sample Removed` = sampleRemain - nrow(netdown1),
                                 `# Sample Remaining` = nrow(netdown1)))
  sampleRemain <- nrow(netdown1)

  netdown1 <- netdown1[SITE_STATUS_CODE == c("A"),]
  netdown_table <- rbind(netdown_table,
                      data.table(`Retention Criteria` = "Active & Pest-damaged samples",
                                 `Removal Criteria` = "Inactive, destroyed, damaged",
                                 `# Sample Removed` = sampleRemain - nrow(netdown1),
                                 `# Sample Remaining` = nrow(netdown1)))
  sampleRemain <- nrow(netdown1)

  netdown1 <- netdown1[PRIVATE == "N",]
  netdown_table <- rbind(netdown_table,
                      data.table(`Retention Criteria` = "Crown Forest",
                                 `Removal Criteria` = "Private land, IR",
                                 `# Sample Removed` = sampleRemain - nrow(netdown1),
                                 `# Sample Remaining` = nrow(netdown1)))
  sampleRemain <- nrow(netdown1)
  library(fpCompare)
  netdown1 <- netdown1[round(AREA, 2) %>=% 0.04,]
  netdown_table <- rbind(netdown_table,
                      data.table(`Retention Criteria` = "Individual plot size >= 0.04ha",
                                 `Removal Criteria` = "Plot size < 0.04ha",
                                 `# Sample Removed` = sampleRemain - nrow(netdown1),
                                 `# Sample Remaining` = nrow(netdown1)))
  sampleRemain <- nrow(netdown1)

  netdown1[SITE_ACCESS_CODE == "BOAT" &
             ACCESS == "N",
           remove := TRUE]
  netdown1 <- netdown1[is.na(remove),]
  netdown_table <- rbind(netdown_table,
                      data.table(`Retention Criteria` = "Restricted access samples",
                                 `Removal Criteria` = "Boat accessible only",
                                 `# Sample Removed` = sampleRemain - nrow(netdown1),
                                 `# Sample Remaining` = nrow(netdown1)))
  sampleRemain <- nrow(netdown1)

  netdown1[grepl(pattern = "NAT_PARK",
                 SITE_ACCESS_CODE) &
             ACCESS == "N",
           remove := TRUE]
  netdown1 <- netdown1[is.na(remove),]
  netdown_table <- rbind(netdown_table,
                      data.table(`Retention Criteria` = "Restricted access samples",
                                 `Removal Criteria` = "In National Parks",
                                 `# Sample Removed` = sampleRemain - nrow(netdown1),
                                 `# Sample Remaining` = nrow(netdown1)))
  sampleRemain <- nrow(netdown1)


  netdown1[is.na(SITE_ACCESS_CODE) &
             ACCESS == "N",
           remove := TRUE]
  netdown1 <- netdown1[is.na(remove),]
  netdown_table <- rbind(netdown_table,
                      data.table(`Retention Criteria` = "Need location coordinates or access notes",
                                 `Removal Criteria` = "No access info available",
                                 `# Sample Removed` = sampleRemain - nrow(netdown1),
                                 `# Sample Remaining` = nrow(netdown1)))
  sampleRemain <- nrow(netdown1)



  netdown1 <- netdown1[MEAS_YR_LAST >= 1996,]
  netdown_table <- rbind(netdown_table,
                      data.table(`Retention Criteria` = "Last measured >= 1996",
                                 `Removal Criteria` = "Last measured < 1996",
                                 `# Sample Removed` = sampleRemain - nrow(netdown1),
                                 `# Sample Remaining` = nrow(netdown1)))
  sampleRemain <- nrow(netdown1)

  netdown1 <- netdown1[CELL_COMPLETE == "Y",]
  netdown_table <- rbind(netdown_table,
                      data.table(`Retention Criteria` = "All 7 matrix cell attributes populated",
                                 `Removal Criteria` = "Missing cell attributes",
                                 `# Sample Removed` = sampleRemain - nrow(netdown1),
                                 `# Sample Remaining` = nrow(netdown1)))
  sampleRemain <- nrow(netdown1)

  netdown1 <- netdown1[PROTECT_PSP == "Y",]
  netdown_protect <- data.table::copy(netdown1)
  netdown_table <- rbind(netdown_table,
                      data.table(`Retention Criteria` = "Total PSPs to protect",
                                 `Removal Criteria` = "Not in any protect criteria",
                                 `# Sample Removed` = sampleRemain - nrow(netdown1),
                                 `# Sample Remaining` = nrow(netdown1)))
  sampleRemain <- nrow(netdown1)

  netdown1 <- netdown1[PROTECT_CODE %in% c("A", "B", "C"),]
  netdown_table <- rbind(netdown_table,
                      data.table(`Retention Criteria` = "Total PSPs to re-measure",
                                 `Removal Criteria` = "protect criteria: D,E",
                                 `# Sample Removed` = sampleRemain - nrow(netdown1),
                                 `# Sample Remaining` = nrow(netdown1)))
  sampleRemain <- nrow(netdown1)

  protect_sites <- data.table::copy(netdown_protect)
  detail_of_psp_protect_list <- netdown_protect[,.(`# Samples` = length(RATING)),
                                                by = "PROTECT_CODE"]
  detail_of_psp_protect_list[PROTECT_CODE == "A",
                             PROTECT_CODE_DESCRIPTION := "Rank 1 PSPs with at least two measurements"]
  detail_of_psp_protect_list[PROTECT_CODE == "B",
                             PROTECT_CODE_DESCRIPTION := "Rank 1 PSPs with a single measurement (BWBS & ESSF zones only)"]
  detail_of_psp_protect_list[PROTECT_CODE == "C",
                             PROTECT_CODE_DESCRIPTION := "Special Project PSPs regardless of rank or # measurements"]
  detail_of_psp_protect_list[PROTECT_CODE == "D",
                             PROTECT_CODE_DESCRIPTION := "Rank 2 PSPs with at least two measurements"]
  detail_of_psp_protect_list[PROTECT_CODE == "E",
                             PROTECT_CODE_DESCRIPTION := "Rank 3-5 PSPs with at least 30years of a total re-measurement period"]
  detail_of_psp_protect_list <- rbind(detail_of_psp_protect_list[order(PROTECT_CODE),
                                                           .(PROTECT_CODE, PROTECT_CODE_DESCRIPTION,
                                                             `# Samples`)],
                                      data.table(PROTECT_CODE = "Total # protected PSPs",
                                                 PROTECT_CODE_DESCRIPTION = "",
                                                 `# Samples` = nrow(netdown_protect)))

  remeasured_sites <- data.table::copy(netdown1)
  detail_of_psp_remeas_psp <- rbind(data.table(`Last measurement year` = "Last measured between 1996 - 2000",
                                               `# Samples` = nrow(netdown1[LAST_MSMT_GRP == "1996_2000"])),
                                    data.table(`Last measurement year` = "Last measured between 2001 - 2010",
                                               `# Samples` = nrow(netdown1[LAST_MSMT_GRP == "2001_2010"])),
                                    data.table(`Last measurement year` = "Last measured after 2010",
                                               `# Samples` = nrow(netdown1[LAST_MSMT_GRP == ">=2011"])),
                                    data.table(`Last measurement year` = "Total # remeasured psps",
                                               `# Samples` = nrow(netdown1)))
  ranking_psp <- merge(ranking_psp,
                       psp5[,.(SITE_IDENTIFIER, SAMP_ID,
                               PROTECT_PSP, PROTECT_CODE, PROTECT_CRITERIA,
                               LAST_MSMT_GRP)],
                       by = "SITE_IDENTIFIER",
                       all.x = TRUE)

  ismcdownloadtime <- dir(file.path(archivedPSPPath, "compilation_PSP_raw"), pattern = "TreeMeasurements.rds")
  ismcdownloadtime <- gsub("ISMC_PROD_", "", ismcdownloadtime)
  ismcdownloadtime <- gsub("_TreeMeasurements.rds", "", ismcdownloadtime)

  tsamap_time <- gsub("TSA_map", "",
                      dir(file.path(archivedPSPPath, "compilation_map"),
                          pattern = "TSA_map"))
  tsamap_time <- gsub(".rds", "", tsamap_time)
  becmap_time <- gsub("BEC_map", "",
                      dir(file.path(archivedPSPPath, "compilation_map"),
                          pattern = "BEC_map"))
  becmap_time <- gsub(".rds", "", becmap_time)
  tflmap_time <- gsub("TFL_map", "",
                      dir(file.path(archivedPSPPath, "compilation_map"),
                          pattern = "TFL_map"))
  tflmap_time <- gsub(".rds", "", tflmap_time)
  ownermap_time <- gsub("Ownership_map", "",
                        dir(file.path(archivedPSPPath, "compilation_map"),
                            pattern = "Ownership_map"))
  ownermap_time <- gsub(".rds", "", ownermap_time)

  datasource <- rbind(data.table(`Data Name` = "ISMC raw data",
                                 `Data Source` = "ISMC",
                                 `Download Time` = ismcdownloadtime),
                      data.table(`Data Name` = "TSA map",
                                 `Data Source` = "BCGW",
                                 `Download Time` = tsamap_time),
                      data.table(`Data Name` = "BEC map",
                                 `Data Source` = "BCGW",
                                 `Download Time` = becmap_time),
                      data.table(`Data Name` = "TFL map",
                                 `Data Source` = "BCGW",
                                 `Download Time` = tflmap_time),
                      data.table(`Data Name` = "OWN_SCHEDULE map",
                                 `Data Source` = "BCGW",
                                 `Download Time` = ownermap_time))
  datasource[`Data Name` == "ISMC raw data",
              `Compiled Date` := substr(`Download Time`, 1, 8)]
  return(list(psp_netdown_summary = netdown_table,
              protected_psp_summary = detail_of_psp_protect_list,
              remeasured_psp_summary = detail_of_psp_remeas_psp,
              ranking_psp = ranking_psp,
              datasource = datasource,
              ranking_all = ranking_all,# a place holder
              ranking_matrix = psp5))
}

