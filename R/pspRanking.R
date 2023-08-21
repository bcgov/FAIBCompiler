# generate ranking matrix

##

rankingMatrix <- function(compilationPath){
  browser()
  sample_sites_org <- readRDS(file.path(compilationPath,
                                        "compilation_PSP_db",
                                        "sample_site_header.rds")) %>%
    data.table

  sample_sites <- data.table::copy(sample_sites_org)

  #-----------------------------------------------------------------#
  # temporary updates to site access code, from chris and bryce, 2022-feb-11
  # revised version updated by dan turner 2022feb23
  update1 <- read.xlsx(file.path(compilationPath,
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
  sample_msmt_org <- readRDS(file.path(compilationPath,
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

  smry_org <- readRDS(file.path(compilationPath,
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
  samp_plot <- readRDS(file.path(compilationPath,
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

  smry_all[, STEMS_HA_L := STEMS_HA_LS + STEMS_HA_LF] # all live trees regardless of staning or falling


  smry_all[, AREA_PM_MIN := ifelse(round(AREA, digits = 2) >= 0.04 & !is.na(AREA),
                                   "Y", "N")]

  treelist_org <- readRDS(file.path(compilationPath,
                                    "compilation_PSP_db",
                                    "treelist.rds")) %>%
    data.table
  # compute an number of live trees per plot computed at the minimum dbh limit
  tree_count_live <- treelist_org[LV_D == "L",
                                  .(TREE_COUNT = length(DBH)),
                                  by = "CLSTR_ID"]

  smry_all <- merge(smry_all,
                    tree_count_live,
                    by = "CLSTR_ID",
                    all.x = TRUE)



  # track calendar year
  smry_all[, CALEND_YR := as.numeric(substr(MEAS_DT, 1, 4))]

  age_smry <- readRDS(file.path(compilationPath,
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

  vol_smry_cs <- readRDS(file.path(compilationPath,
                                   "compilation_PSP_db",
                                   "Smries_volume_byCLSP.rds"))
  leadingspecies <- vol_smry_cs[order(CLSTR_ID, UTIL, -SP_PCT_BA_LS),
                                .(CLSTR_ID, UTIL,
                                  SPC1 = SPECIES,
                                  SP_PCT_BA_LS)]
  leadingspecies <- unique(leadingspecies,
                           by = c("CLSTR_ID", "UTIL"))

  # species strata
  # use 80% species as per 2019 strategic plan

  leadingspecies[, SPC_STRATA := ifelse(SP_PCT_BA_LS > 80,
                                        sprintf("%sPURE", SPC1),
                                        ifelse(SP_PCT_BA_LS >0 & SP_PCT_BA_LS <= 80,
                                               sprintf("%sMIX", SPC1),
                                               NA))]
  smry_1st <- merge(smry_1st,
                    leadingspecies,
                    by = c("CLSTR_ID", "UTIL"),
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

  # need to get access note data, as part of assessment of keeping a sample
  # ie., a sample needs to have either a gps coordinate or access notes, otherwise do not keep
  ac_ismc <- read_rds(file.path(compilationPath,
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


  #------------------- smry_all cleanup ------------#


  #-----------------------------------------------#
  # create cell matrix definitions
  sample_sites[, CELL_KEY := paste(STS_STRATA,
                                   TRT_STRATA,
                                   BEC_STRATA,
                                   SPC_STRATA,
                                   AGE_STRATA,
                                   DEN_STRATA,
                                   SI_STRATA,
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
                 OWN_SCHED != "51-N",
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
      cast(SITE_IDENTIFIER~VISIT_NUMBER) %>%
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

  treelist_org[, HT_SUIT := "Y"] # need attention here, as ht_suit is not in treelist
  treelist_org[BTOP == "Y", HT_SUIT := "N"]
  treelist_org[RESIDUAL == "Y", HT_SUIT := "N"]
  treelist_org[LV_D == "D", HT_SUIT := "N"]
  treelist_org[S_F == "F", HT_SUIT := "N"]


  treelist_nhts <- treelist_org[HT_TOTAL_SOURCE == "Field measured" &
                                  HT_SUIT == "Y" &
                                  LV_D == "L",
                                .(CLSTR_ID, HEIGHT, HT_SUIT, LV_D)]

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
  return(list(ranking_matrix = psp5,
              ranking_psp = ranking_psp,
              ranking_all = ranking_all))
}

