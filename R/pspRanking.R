rm(list = ls())
library(data.table)
library(readxl)
library(tidyverse)
library(openxlsx)
library(dplyr)
library(reshape)

# this is a test from yong

# define wkdir
if(Sys.info()[["user"]] == "yluo"){
  wkdir <- "G:/!Workgrp/Inventory/PSP Height Estimate"
} else {
  wkdir <- "G:\\PSP Height Estimate"
}

rankingMatrix <- function(my_data_psp_sumry_rating){

  sample1 <- read.xlsx(file.path(wkdir,
                                 "lastversionSAS",
                                 "sample_site_header.xlsx")) %>%
    data.table


  sample2 <- data.table::copy(sample1)
  names(sample2) <- toupper(names(sample2))

  sample2[nchar(SAMPLE_SITE_NAME) == 11,
          ":=" (SAMP_ID = paste0(substr(SAMPLE_SITE_NAME, 1, 2),
                                 substr(SAMPLE_SITE_NAME, 4, 6),
                                 " ",
                                 substr(SAMPLE_SITE_NAME, 11, 11),
                                 "000",
                                 substr(SAMPLE_SITE_NAME, 8, 10)),
                SAMPLETYPE_ISMC = substr(SAMPLE_SITE_NAME, 11, 11))]

  sample2[nchar(SAMPLE_SITE_NAME) == 12,
          ":=" (SAMP_ID = paste0(substr(SAMPLE_SITE_NAME, 1, 2),
                                 substr(SAMPLE_SITE_NAME, 4, 6),
                                 substr(SAMPLE_SITE_NAME, 7, 7),
                                 substr(SAMPLE_SITE_NAME, 12, 12),
                                 "000",
                                 substr(SAMPLE_SITE_NAME, 9, 11)),
                SAMPLETYPE_ISMC = substr(SAMPLE_SITE_NAME, 12, 12))]
  sample2[is.na(BEC_VAR),
          BEC_VAR := ""]
  sample2[, BECLABEL := paste0(BEC_ZONE, BEC_SBZ, BEC_VAR)]
  sample2[, ':='(TSA_DESC = gsub("TSA", "", TSA_DESC))]
  sample2[, ':='(TSA_DESC = gsub(" ", "", TSA_DESC))]
  sample2[, TSA_DESC := sub("QueenCharlotte", "HaidaGwaii", TSA_DESC)]
  sample2[, MGMT_UNIT := paste0("TSA", TSA, "_", TSA_DESC)]



  # prep steps to define management unit

  sample2[, ":=" (TSA_DESC2 = gsub("TSA", "", TSA_DESC),
                  TFL_NO = gsub("TFL", "", TFL))]


  sample2[nchar(TFL_NO) == 1, TFL_NO := paste0("0", TFL_NO)]

  sample2[!is.na(IP_UTM), UTM_SOURCE_ISMC := "DGPS"]

  setnames(sample2,
           old = c("IP_UTM", "IP_NRTH", "IP_EAST", "OWNER"),
           new = c("UTM_ZONE_ISMC", "UTM_NORTHING_ISMC", "UTM_EASTING_ISMC", "OWN"))

  #-------------------------------------------------------------#

  # get mu lookup table, merge to get tfl licensee name

  mu_tfl <- read.xlsx(file.path(wkdir,
                                "lastversionSAS",
                                "tfl_lookup.xlsx")) %>%
    data.table

  names(mu_tfl) <- toupper(names(mu_tfl))

  sample3 <- merge(sample2, mu_tfl, by.x = "TFL_NO", by.y = "TFL", all.x = TRUE)

  # define management unit (either 1) TFL or 2) remaining TSA outside TFL)

  sample4 <- data.table::copy(sample3)

  sample4[, MGMT_UNIT_ISMC := ifelse(!is.na(TFL),
                                     paste0("TFL", TFL_NO, "_", TFL_DESC),
                                     ifelse(!is.na(TSA),
                                            paste0("TSA", TSA, "_", TSA_DESC3),
                                            NA))]
  sample4[, MGMT_UNIT_ISMC := gsub(" ", "", MGMT_UNIT_ISMC)]
  sample4 <- sample4[, !c("TFL_NO", "MU", "TSA_DESC2")]

  # create tsa list file for subsequent batch processing

  list1 <- unique(sample4[!is.na(TSA),
                   .(TSA_NO = TSA,
                     TSA_NAME = gsub(" ", "", TSA_DESC3))]) %>%
    arrange(TSA_NO)


  # export to text file
  write.table(list1, file.path(wkdir, "list.txt"),
              quote = FALSE,
              row.names = FALSE, col.names = FALSE)

  sample4 <- arrange(sample4, SITE_IDENTIFIER)

  #-----------------------------------------------------------#
  # get latest sample_site_visit

  sample_visit1 <- read.xlsx(file.path(wkdir,
                                       "lastversionSAS",
                                       "sample_msmt_header.xlsx")) %>%
    data.table

  sample_visit1 <- sample_visit1[, .(SITE_IDENTIFIER, VISIT_NUMBER, MEAS_DT)]

  sample_visit2 <- data.table::copy(sample_visit1)

  #sample_visit2[, ":=" (YR = substr(MEAS_DT, 1, 4),
  #                      MN = substr(MEAS_DT, 6, 7),
  #                      DY = substr(MEAS_DT, 9, 10),
  #                      MEAS_DT_ISMC = )]

  sample_visit2 <- sample_visit2 %>%
    arrange(SITE_IDENTIFIER, MEAS_DT)

  sample_visit3 <- sample_visit2 %>%
    group_by(SITE_IDENTIFIER) %>%
    slice(n()) %>% # select the last row for each group
    setDT()

  sample5 <- merge(sample4, sample_visit3, by = "SITE_IDENTIFIER")

  sample5[, YR := substr(MEAS_DT, 1, 4)]

  sample6 <- sample5[,.(SITE_IDENTIFIER, SAMPLE_SITE_NAME, SAMP_ID, MEAS_DT, YR, SAMPLETYPE_ISMC, OWN,
           SCHEDULE, TSA_ISMC = TSA, MGMT_UNIT_ISMC, UTM_ZONE_ISMC, UTM_NORTHING_ISMC,
           UTM_EASTING_ISMC, UTM_SOURCE_ISMC, SITE_STATUS_CODE, SITE_ACCESS_CODE,
           BEC_ZONE, BEC_SBZ, BEC_VAR)]

  #-----------------------------------------------------------------#

  # temporary updates to site access code, from chris and bryce, 2022-feb-11
  # revised version updated by dan turner 2022feb23

  update1 <- read.xlsx(file.path(wkdir,
                                 "lastversionSAS",
                                 "Boat Access_Updated_2022Feb24.xlsx")) %>%
    data.table

  update1 <- update1[,.(SAMP_ID = Sample.Number,
                        site_access_code_update = gsub(" ", "",toupper(Access.Type)))]

  # update1 <- update1[, .(SAMP_ID, site_access_code_update)]
  names(update1) <- toupper(names(update1))

  update1[, lofsitecode := nchar(SITE_ACCESS_CODE_UPDATE)]

  update1 %<>%
    group_by(SAMP_ID) %>%
    arrange(lofsitecode) %>%
    slice(n()) %>%
    setDT()
  update1 <- update1[,.(SAMP_ID, SITE_ACCESS_CODE_UPDATE)]

  # need site identifier to link with excel workbook listing updated access description

  sample6[, SITE_ACCESS_CODE := NULL]
  sample7 <- merge(sample6, update1, by = "SAMP_ID", all.x = TRUE)
  sample7[!is.na(SITE_ACCESS_CODE_UPDATE),
          SITE_ACCESS_CODE := SITE_ACCESS_CODE_UPDATE]

  # sample7 <- sample7[, !c('SITE_ACCESS_CODE')]
  # sample7[!is.na(SITE_ACCESS_CODE_UPDATE), SITE_ACCESS_CODE := SITE_ACCESS_CODE_UPDATE]

  planyr <- sample7[YR %in% c("2020", "2021", "2022"),]
  setnames(planyr,
           "YR", "PLAN_YR")

### after long weekend
  #-------------------------------------------------------------------#
  # base rating algorithm on psp attributes at the lowest compiled utilization limit
  psp_sumry_all2 <- fread(file.path(wkdir,
                                    "sasds",
                                    "psp_sumry_all2.csv"))
  psp_sumry_all2[psp_sumry_all2 == "." |
                   psp_sumry_all2 == ""|
                   psp_sumry_all2 == " "] = NA
  # psp_sumry_all2 <- psp_sumry_all2[!is.na(SAMP_ID),]
  names(psp_sumry_all2) <- toupper(names(psp_sumry_all2))
  psp_sumry_all2 <- merge(psp_sumry_all2,
                          sample2[,.(SAMP_ID, SITE_IDENTIFIER)],
                          by = "SAMP_ID",
                          all.x = TRUE)

  psp_sumry_all2_new <- psp_sumry_all2[!is.na(SITE_IDENTIFIER),]
  psp_sumry_all2_new[, VISIT_NUMBER := as.numeric(MEAS_NO) + 1]
  psp_sumry_all2_new[, MEAS_DT := NULL]
  psp_sumry_all2_new <- merge(psp_sumry_all2_new,
                              sample_visit2,
                              by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                              all.x = TRUE)
  psp_sumry_all2_new[, MEAS_DT := as.numeric(substr(MEAS_DT, 1, 4))]

  psp_sumry_all2_new[, DBHLIMIT_COMPILE := as.numeric(DBHLIMIT_COMPILE)]

  ut1 <- arrange(psp_sumry_all2_new, SAMP_ID, DBHLIMIT_COMPILE)

  ut2 <- ut1[!is.na(SAMP_ID), .(SAMP_ID, DBHLIMIT_COMPILE)]
  ut2 %<>%
    group_by(SAMP_ID) %>%
    summarise(FREQ = n(), MIN_UTIL = min(DBHLIMIT_COMPILE, na.rm = TRUE)) %>%
    setDT()

  # a <- psp_sumry_all2 %>%
  #  group_by(SAMP_ID) %>%
  #  filter(!row_number() == 1)

  # b <- ut2 %>%
  #   group_by(SAMP_ID) %>%
  #   filter(!row_number() == 1)

  # c <- planyr %>%
  #   group_by(SAMP_ID) %>%
  #   filter(!row_number() == 1)


  #------------------- preparing for merge -------------------------------------#
  psp_sumry_all2_new[,':='(UTM_SOURCE_ISMC = NULL,
                           UTM_ZONE_ISMC = NULL,
                           UTM_NORTHING_ISMC = NULL,
                           UTM_EASTING_ISMC = NULL,
                           SITE_ACCESS_CODE = NULL)]
  sample7_new <- data.table::copy(sample7)
  sample7_new[,':='(MEAS_DT = NULL,
                    SITE_IDENTIFIER = NULL)]
  psp1a <- merge(psp_sumry_all2_new,
                    sample7_new,
                    by = c("SAMP_ID"),
                    all.x = TRUE)

  psp1a <- merge(psp1a,
                    ut2,
                    by = c("SAMP_ID"),
                    all.x = TRUE)

  psp1a <- merge(psp1a,
                    planyr[,.(SAMP_ID, PLAN_YR)],
                    by = c("SAMP_ID"),
                    all.x = TRUE)





  #----------------- cleanup psp1a ------------------------------------------#

  rm(access1, list1, mu_tfl,
     sample_visit1, sample_visit2, sample_visit3, sample1, sample2,
     sample3, sample4, sample5, ut1, CURRENT_YEAR, YEAR_NUMERIC, YEAR_STRING)

  psp1a <- psp1a[, .(SITE_IDENTIFIER, SAMP_ID, PROJ_ID, SAMP_NO, TYPE_CD, SAMPLETYPE,
                     OWN_SCHED, OWN_SCHED_DESCRIP, PRIVATE, OWNER, OLD_OWNER, NEW_OWNER,
                     TSA, MGMT_UNIT, MGMT_UNIT_ISMC, PROJECT, BEC_SOURCE,
                     BGC_ZONE, BECLABEL, BECLABEL_GRD,
                     BGC_SS_GRD, MAP_TILE, POLYGON_NO, OPENING_NO, UTM_VERSION, UTM_SOURCE,
                     UTM_ZONE, UTM_EASTING, UTM_NORTHING, UTM_SOURCE_ISMC, UTM_ZONE_ISMC,
                     UTM_EASTING_ISMC, UTM_NORTHING_ISMC, UTM_SOURCE_GYS, UTM_ZONE_GYS,
                     UTM_EASTING_GYS, UTM_NORTHING_GYS, UTM_SOURCE_RECON, UTM_ZONE_RECON,
                     UTM_EASTING_RECON, UTM_NORTHING_RECON, UTM_SOURCE_REMEASOP1,
                     UTM_ZONE_REMEASOP1, UTM_EASTING_REMEASOP1, UTM_NORTHING_REMEASOP1,
                     UTM_SOURCE_REMEASOP2, UTM_ZONE_REMEASOP2, UTM_EASTING_REMEASOP2,
                     UTM_NORTHING_REMEASOP2, BCALB_X, BCALB_Y, LATITUDE, LONGITUDE,
                     ASPECT, SLOPE, SL_POS, ELEV, SAMP_STS,
                     LANDSAT_DISTURB_YR, MEAS_YR_FIRST, MEAS_YR_LAST,
                     CALEND_YR_LAST_HARDY, RECON_YR, RELEASE, NO_PLOTS, AREA_PM,
                     SHP_PM, RAD_PM, LEN_PM, WID_PM, NO_MEAS,
                     TOT_PERIOD, STEMSHA_LIV, WSVHA_LIV, SPC_LABEL_LIVE, TOT_STAND_AGE,
                     SAMPLETYPE_DUP, SITE_ACCESS_CODE, BAF_PM, PLOT_TYP, FIZ, BUFFER_RAD,
                     BUFFER_OK, STEM_MAPPED_IND,
                     STND_ORG, STND_STR, SEL_LGD, TREATMENT, TRT_CODE, OWN, SCHEDULE,
                     DBHLIMIT_TAG, MEAS_DT, MEAS_YR, MIN_UTIL, TSA_ISMC, BEC_ZONE,
                     BGC_SBZN, BEC_SBZ, BEC_VAR, MEAS_NO, MEAS_FIRST, SPC_LIVE1,
                     SPCPER_LIVE1, STEMSHA_L, LEAD_SI1, LEAD_SI2, LEAD_SI3,
                     LEAD_SI4, MEAS_LAST, SITE_STATUS_CODE, PERIOD, PLAN_YR,
                     CALEND_YR_LAST, LEAD_HTOP1, LEAD_HTOP2, LEAD_HTOP3,
                     BH_STAND_AGE, SAMPLETYPE_ISMC, DBHLIMIT_COMPILE, WSVHA_L, WSVHA_V)]

  #--------------------------------------------------------------------------#

  # temporary fix, to bring back sampletype to those samples that were never in oracle to begin with

  psp1a[is.na(SAMPLETYPE), SAMPLETYPE := SAMPLETYPE_ISMC]

  psp1a[!is.na(OWN) & !is.na(SCHEDULE), OWN_SCHED2 := paste0(OWN, "-", SCHEDULE)]
  psp1a[, PRIVATE := ifelse(OWN_SCHED2 %in% c("40-N", "41-N", "52-N", "72-A", "77-A", "79-A"),
                            "Y",
                            "N")]

  psp1a[, OWN_SCHED := NULL]
  setnames(psp1a, "OWN_SCHED2", "OWN_SCHED")

  # deletes records from tsas where no samples are present, these are from null records

  psp1a <- psp1a[!is.na(SAMP_ID), ]
  psp1a <- psp1a[SAMPLETYPE %in% c("G", "R", "CMI", "VRI",
                                   "YSM", "NFI", "CMO",
                                   "T", "I", "EP", "FLT",
                                   "VLT", "SUP"), ]

  # for samples that have no ownership code assigned, assign VGIS based samples all as MOF owned
  psp1a[SAMPLETYPE %in% c("VRI", "CMI", "NFI", "YSM", "CMO",
                          "EP", "FLT", "VLY", "SUP"),
        OWNER := "MOF"]
  # for remaining samples with no ownership record, assume MOF
  psp1a[is.na(OWNER), OWNER := "MOF"]

  # plot area not populated for some fixed radius sample types
  # note, for those samples that are made up of multiple plots, eg., 3 * 0.015ha plots on a transect, then the evaluation should be based on the ;
  # individual plot area, and not the combined area of all plots within a sample id;
  # this is the area of the individual plot. the total area of all plots in a sample id would be (area_pm * no_plots);

  psp1a[, AREA := ifelse(SAMPLETYPE %in% c("CMI", "YSM", "NFI", "CMO", "FLT"),
                         0.04,
                         ifelse(SAMPLETYPE == "SUP" & PLOT_TYP == "F",
                                0.04,
                                AREA_PM))]

  psp1a[, DBHLIMIT_TAG := ifelse(SAMPLETYPE %in% c("CMI", "YSM", "NFI", "CMO","FLT"), 4.0,
                                 ifelse(SAMPLETYPE == "SUP" & PLOT_TYP == "F",
                                        4.0,
                                        DBHLIMIT_TAG))]

  psp1a[,':='(AREA = as.numeric(AREA),
              STEMSHA_LIV = as.numeric(STEMSHA_LIV))]

  psp1a[, AREA_PM_MIN := ifelse(round(AREA, digits = 2) >= 0.04 & !is.na(AREA),
                                "Y", "N")]

  # compute an equivalent number of live trees per plot computed at the minimum dbh limit
  psp1a[, TREE_COUNT := STEMSHA_LIV * AREA]

  # track calendar year

  psp1a[, CALEND_YR := ifelse(!is.na(MEAS_DT),
                              substr(MEAS_DT, 1, 4),
                              MEAS_YR)]

  # base rating algorithm on compiled attributes at lowest utilization limit

  psp1a <- subset(psp1a, DBHLIMIT_COMPILE == MIN_UTIL)

  # update coordinates with ISMC source, to represent the most up to date information source
  # as of ismc prod updates to january 2022

  psp1a[(is.na(UTM_ZONE) & UTM_ZONE_ISMC > 0) |
          (UTM_ZONE > 0 & UTM_ZONE_ISMC > 0 & UTM_ZONE != UTM_ZONE_ISMC) |
          (UTM_NORTHING > 0 & UTM_NORTHING_ISMC > 0 & UTM_NORTHING != UTM_NORTHING_ISMC) |
          (UTM_EASTING > 0 & UTM_EASTING_ISMC > 0 & UTM_EASTING != UTM_EASTING_ISMC),
        ":=" (UTM_SOURCE = UTM_SOURCE_ISMC,
              UTM_VERSION = "ISMC",
              UTM_ZONE = UTM_ZONE_ISMC,
              UTM_NORTHING = UTM_NORTHING_ISMC,
              UTM_EASTING = UTM_EASTING_ISMC)]

  psp1a[, TSA_ISMC := as.numeric(TSA_ISMC)]

  psp1a[TSA != TSA_ISMC & !is.na(TSA_ISMC),
        ":=" (TSA = TSA_ISMC,
              TSA_SOURCE = "ISMC")]

  # use use ismc spatial intersect of bec classification for all psps with values present
  # that also have coordinates, otherwise use default reg/comp/bec lookup table

  psp1a[!is.na(BEC_ZONE) & UTM_VERSION == "ISMC",
        ":=" (BGC_ZONE = BEC_ZONE,
              BGC_SBZN = BEC_SBZ,
              BGC_VAR = BEC_VAR,
              BECLABEL = paste0(BEC_ZONE, BEC_SBZ, BEC_VAR),
              BEC_SOURCE = "ISMC")]

  psp1a[, MGMT_UNIT_GYS := MGMT_UNIT]
  psp1a[!is.na(MGMT_UNIT_ISMC),
        ":=" (MGMT_UNIT = MGMT_UNIT_ISMC,
              MGMT_UNIT_SOURCE = "ISMC")]

  # delete variables that haven't yet been created

  #psp1a <- psp1a[, !c("RATING",
  #                    "CALEND_YR_LAST",
  #                    "CELL_KEY1",
  #                    "RANK_PSP1",
  #                    "RANK_ALL1",
  #                    "MAX_PSP1",
  #                    "MAX_ALL1",
  #                    "CELL_COMPLETE")]

  psp1a <- arrange(psp1a, SAMP_ID, MEAS_NO)

  # some attributes are defined based on first measurement, ie., at establishment
  # ie., species, age class, density, total number of trees.  site index, included here,
  # but not critical, since si was previously assigned based on the measurement closest to 50yr bh age;
  # bec variant and site series included here

  # ------- psp1b codes -------------------------------------------#

  psp1b <- data.table::copy(psp1a)

  # keep only first measure record
  psp1b <- subset(psp1b, psp1b$MEAS_FIRST == "Y")

  # new 7 class age classes, as per 2019 strategic plan

  psp1b[, ":="(TOT_STAND_AGE = as.numeric(TOT_STAND_AGE))]

  psp1b[, AGE_STRATA := as.character(cut(TOT_STAND_AGE,
                                         breaks = c(seq(0, 240, by = 40), max(as.numeric(TOT_STAND_AGE), na.rm = TRUE)),
                                         labels = c("001-040YR",
                                                    "041-080YR",
                                                    "081_120YR",
                                                    "121_160YR",
                                                    "161_200YR",
                                                    "201_240YR",
                                                    ">240YR")))]

  # species strata
  # use 80% species as per 2019 strategic plan

  psp1b[, ":="(SPC1 = SPC_LIVE1,
               PCT1 = as.numeric(SPCPER_LIVE1))]

  psp1b[, SPC_STRATA := ifelse(PCT1 > 80,
                             sprintf("%sPURE", SPC1),
                             ifelse(PCT1 >0 & PCT1 <= 80,
                                    sprintf("%sMIX", SPC1),
                                    NA))]
  # density strata

  psp1b[,STEMSHA_L := as.numeric(psp1b$STEMSHA_L)]

  psp1b$DEN_STRATA <- ifelse(psp1b$STEMSHA_L > 5000,
                             ">5000SPH",
                             ifelse(psp1b$STEMSHA_L > 1000 & psp1b$STEMSHA_L <= 5000,
                                    "1001-5000SPH",
                                    ifelse(psp1b$STEMSHA_L > 0 & psp1b$STEMSHA_L <= 1000,
                                           "<=1000SPH", NA)))

  # site index
  psp1b$REG <- ifelse(psp1b$BGC_ZONE %in% c("CWH", "CDF", "MH", "CMA"),
                      "C", "I")

  # decide on source of available site index estimates

  psp1b[, ":="(LEAD_SI1 = as.numeric(LEAD_SI1),
               LEAD_SI2 = as.numeric(LEAD_SI2),
               LEAD_SI3 = as.numeric(LEAD_SI3),
               LEAD_SI4 = as.numeric(LEAD_SI4))]

  psp1b[, SI_USE := ifelse(LEAD_SI1 < 1 | is.na(LEAD_SI1),
                           ifelse(LEAD_SI2 < 1 | is.na(LEAD_SI2),
                                  ifelse(LEAD_SI3 < 1 | is.na(LEAD_SI3),
                                         ifelse(LEAD_SI4 < 1 | is.na(LEAD_SI4), NA,
                                                LEAD_SI4),
                                         LEAD_SI3),
                                  LEAD_SI2),
                           LEAD_SI1)]

  psp1b[, SI_STRATA := ifelse(SI_USE > 0 & SI_USE <= 12.5, "<=12.5M",
                              ifelse(SI_USE > 12.5 & SI_USE <= 17.5, "12.6-17.5M",
                                     ifelse(SI_USE > 17.5 & SI_USE <= 22.5, "17.6-22.5M",
                                            ifelse(SI_USE > 22.5 & SI_USE <= 27.5, "22.6-27.5M",
                                                   ifelse(SI_USE > 27.5 & SI_USE <= 32.5, "27.6-32.5M",
                                                          ifelse(SI_USE > 32.5, ">32.5M", NA))))))]

  # bec strata
  # new bec strata classification based on subzone, as per 2019 strategic plan

  psp1b[, BEC_STRATA := ifelse(!is.na(BGC_ZONE) & !is.na(BGC_SBZN),
                               paste0(BGC_ZONE, BGC_SBZN), NA)]

  # site series strata

  psp1b[, SS_STRATA := BGC_SS_GRD]
  psp1b <- psp1b[,.(SAMP_ID,
                    AGE_STRATA,
                    SPC_STRATA,
                    DEN_STRATA,
                    SI_STRATA,
                    BEC_STRATA,
                    SS_STRATA,
                    TREE_COUNT,
                    SI_USE,
                    REG,
                    SPC1,
                    BGC_SS_GRD)]

  # some attributes are defined based on last measurement only
  # i.e. treatment, disturbance history

  #------- psp1c --------------------------------------------------------#

  psp1c <- data.table::copy(psp1a)

  # keep only last measure
  psp1c <- subset(psp1c, psp1c$MEAS_LAST == "Y")

  # treatment strata
  psp1c[, TRT_STRATA := ifelse(TREATMENT == "THINNED", "T",
                             "U")]


  # status strata
  psp1c[, SAMP_STS := ifelse(SITE_STATUS_CODE == "IA" & !is.na(SITE_STATUS_CODE), "X",
                             ifelse(SAMPLETYPE %in% c("CMI", "VRI", "NFI", "YSM", "CMO", "EP", "SUP"), "A",
                                    ifelse(is.na(SAMP_STS), "A",
                                           ifelse(SAMP_ID == "71058 G000003" & SAMP_STS == "L", "P",
                                                  SAMP_STS))))]

  psp1c[, STS_STRATA := ifelse(SAMP_STS %in% c("A", "P", "Q"), "A",
                               "X")]

  setnames(psp1c, "CALEND_YR", "CALEND_YR_LAST")
  psp1c <- psp1c[,.(SAMP_ID,
                    TRT_STRATA,
                    SAMP_STS,
                    STS_STRATA,
                    CALEND_YR_LAST)]

  # psp1a %<>%
  #   arrange(SAMP_ID, MEAS_NO) %>%
  #   distinct()
  #
  # psp1b %<>%
  #   arrange(SAMP_ID) %>%
  #   distinct()
  #
  # psp1c %<>%
  #   arrange(SAMP_ID) %>%
  #   distinct()
  #
  # need to get access note data, as part of assessment of keeping a sample
  # ie., a sample needs to have either a gps coordinate or access notes, otherwise do not keep

  ac1 <- fread(file.path(wkdir, "sasds", "psp_access_notes.csv"))



  # "G:\\PSP Height Estimate\\sasds\\")
  ac1[ac1 == "." |
        ac1 == ""|
        ac1 == " "] = NA
  names(ac1) <- toupper(names(ac1))
  ac1 <- merge(ac1, sample2[,.(SITE_IDENTIFIER, SAMP_ID,
                               IP_UTM, IP_NRTH, IP_EAST)],
               by = "SAMP_ID",
               all.x = TRUE)
  ac1 <- ac1[!is.na(SITE_IDENTIFIER),]

  ac1[(UTM_ZONE == IP_UTM | (is.na(UTM_ZONE) & is.na(IP_UTM))) &
      (UTM_NORTHING == IP_NRTH | (is.na(UTM_NORTHING) & is.na(IP_NRTH))) &
      (UTM_EASTING == IP_EAST | (is.na(UTM_EASTING) & is.na(IP_EAST))),
      utm_same := "Y"]
  ac1[is.na(utm_same)]




  ac_ismc <- read_rds(file.path(wkdir, "siteaccessnotes.rds"))

  ac1 <- merge(ac1,
               ac_ismc[,.(SITE_IDENTIFIER, ACCESS_NOTES_ismc = ACCESS_NOTES)],
               by = "SITE_IDENTIFIER",
               all.x = TRUE)
  a <- ac1[is.na(ACCESS_NOTES_ismc) & !is.na(ACCESS_NOTES)]
  b <- ac1[!is.na(ACCESS_NOTES_ismc) & is.na(ACCESS_NOTES)]
  c <- ac1[is.na(ACCESS_NOTES_ismc) & is.na(ACCESS_NOTES)]
  d <- ac1[!is.na(ACCESS_NOTES_ismc) & !is.na(ACCESS_NOTES)]


  write.xlsx(a[,.(SITE_IDENTIFIER, SAMP_ID, ACCESS_NOTES_old_compiled = ACCESS_NOTES,
                  ACCESS_NOTES_ismc)],
             "missing_accessnotes_inISMC.xlsx")

  write.xlsx(b[,.(SITE_IDENTIFIER, SAMP_ID, ACCESS_NOTES_old_compiled = ACCESS_NOTES,
                  ACCESS_NOTES_ismc)],
             "populated_accessnotes_inISMC.xlsx")


  ac1[, ":="(UTM_ZONE = as.numeric(UTM_ZONE),
             UTM_NORTHING = as.numeric(UTM_NORTHING),
             UTM_EASTING = as.numeric(UTM_EASTING))]

  # create access coord flag, and access notes flag
  ac1[, ACCESS_COORD := ifelse(UTM_ZONE > 0 & UTM_NORTHING > 0 & UTM_EASTING > 0,
                               "Y", "N")]
  ac1[is.na(ACCESS_COORD), ACCESS_COORD := "N"]


  ac1[, ACCESS_NOTE := ifelse(!is.na(ACCESS_NOTES), "Y", "N")]


  # ac1[, ACCESS := "N"]
  # ac1[ACCESS_COORD == "Y" | ACCESS_NOTE == "Y",
  #     ACCESS := "Y"]
  ac1[, ACCESS := ifelse(ACCESS_COORD == "N" & ACCESS_NOTE == "N",
                         "N",
                         ifelse(ACCESS_COORD == "Y" | ACCESS_NOTE == "Y",
                                "Y", NA))]

  ac1 <- ac1[,.(SAMP_ID,
                ACCESS_COORD,
                ACCESS_NOTE,
                ACCESS)]

  ac1 <- arrange(ac1, SAMP_ID)

  #------------------- psp2a ----------------------#

  psp1a1 <- psp1a[, !c("SAMP_STS", "CALEND_YR_LAST")]
  setnames(psp1b, c("TREE_COUNT", "BGC_SS_GRD"),
           c("TREE_COUNT_FST_MSMT", "BGC_SS_GRD_FST_MSMT"))


  join_list2 <- list(psp1a1,
                     psp1b,
                     psp1c,
                     ac1)

  psp2a <- Reduce(function(x,y)
    merge(x, y, by = "SAMP_ID", all = TRUE),
    join_list2)

  #------------------- psp2a cleanup ------------#

  rm(join_list2, psp1a1)

  #-----------------------------------------------#

  psp2 <- data.table::copy(psp2a)

  # create cell matrix definitions

  psp2[, CELL_KEY1 := paste(STS_STRATA,
                            TRT_STRATA,
                            BEC_STRATA,
                            SPC_STRATA,
                            AGE_STRATA,
                            DEN_STRATA,
                            SI_STRATA,
                            sep = "_")]

  # cell completeness flag. samples with incomplete criteria cannot be reasonably assigned a cell class

  psp2[, CELL_COMPLETE := ifelse(is.na(AGE_STRATA) |
                                   is.na(SPC_STRATA) |
                                   is.na(DEN_STRATA) |
                                   is.na(SI_STRATA) |
                                   is.na(BEC_STRATA),
                                 "N", "Y")]

  # modify access flag, to drop boat access samples
  # and also to drop psps in parks

  psp2[ACCESS == "Y" & OWN_SCHED == "51-N",
       SITE_ACCESS_CODE := ifelse(!is.na(SITE_ACCESS_CODE),
                                  paste("NAT_PARK", SITE_ACCESS_CODE, sep = "_"),
                                  "NAT_PARK")]

  # psp2[grepl("NAT_PARK", SITE_ACCESS_CODE), ACCESS := "N"]

  psp2[ACCESS == "Y" & OWN_SCHED == "51-N",
       ACCESS := "N"]

  psp2[ACCESS == "Y" &
         SITE_ACCESS_CODE %in% c("BOAT", "BOAT/HELI") &
         OWN_SCHED != "51-N",
       ACCESS := "N"]


  #-------------------------------------#

  psp2_1 <- psp2[!is.na(MEAS_NO),]

  mastertable <- unique(psp2_1[,.(SAMP_ID)])
  interestcols <- c("PERIOD", "WSVHA_L", "WSVHA_V",
                    "TOT_STAND_AGE", "MEAS_YR", "DBHLIMIT_TAG",
                    "TREE_COUNT")

  for (indicolname in interestcols) {
    indidata <- psp2_1[,c("SAMP_ID", "MEAS_NO", indicolname),
                     with = FALSE]

    indidata %<>%
      melt(id = c("SAMP_ID", "MEAS_NO")) %>%
      cast(SAMP_ID~MEAS_NO) %>%
      setDT()

    if(indicolname == "PERIOD"){
      prefix <- "MINT_"
    } else if (indicolname == "WSVHA_L"){
      prefix <- "WSVL_"
    } else if (indicolname == "WSVHA_V"){
      prefix <- "WSVV_"
    } else if (indicolname == "TOT_STAND_AGE"){
      prefix <- "TAGE_"
    } else if (indicolname == "MEAS_YR"){
      prefix <- "MEASYR_"
    } else if (indicolname == "DBHLIMIT_TAG"){
      prefix <- "DBHLIM_"
    } else if (indicolname == "TREE_COUNT"){
      prefix <- "NTREES_"
    }

    names(indidata)[2:14] <- paste0(prefix, names(indidata)[2:14])
    mastertable <- merge(mastertable,
                         indidata,
                         by = "SAMP_ID",
                         all.x = TRUE)
  }

  #-------------------------------------------------------------#
  # get num hts;

  nhts1 <- read.xlsx(file.path(wkdir,
                               "sasds",
                               "psp_tree_all_small.xlsx")) %>%
    data.table
  names(nhts1) <- toupper(names(nhts1))
  # nhts1 <- merge(nhts1,
  #                sample2[,.(SITE_IDENTIFIER, SAMP_ID)],
  #                by = "SAMP_ID",
  #                all.x = TRUE)


  nhts1 <- nhts1[HT_MEAS > 0 & HT_SUIT != "N" & LD == "L",
                 .(SAMP_ID, MEAS_NO, HT_MEAS, HT_SUIT, LD)]

  nhts1 <- arrange(nhts1, SAMP_ID, MEAS_NO) # optional

  nhts2 <- nhts1[, .(SAMP_ID, MEAS_NO, HT_MEAS)]
  nhts2 %<>%
    group_by(SAMP_ID, MEAS_NO) %>%
    summarise(FREQ = n(), NHTS = sum(!is.na(HT_MEAS))) %>%
    setDT()

  numhts <- nhts2[, .(SAMP_ID, MEAS_NO, NHTS)]
  numhts %<>%
    melt(id = c("SAMP_ID", "MEAS_NO")) %>%
    cast(SAMP_ID~MEAS_NO) %>%
    setDT()
  colnames(numhts)[2:9] <- paste0("NHTS_", colnames(numhts)[2:9])

  #---------------------------------------------------------------#

  # stand attributes at last measurement

  psp3 <- data.table::copy(psp2)

  # psp3[, last_meas_no := max(MEAS_NO), by = "SAMP_ID"]
  # psp3_yl <- psp3[MEAS_NO == last_meas_no,]

  psp3 <- arrange(psp3, SAMP_ID)
  psp3 %<>%
    group_by(SAMP_ID) %>%
    slice(n()) %>%
    setDT()

  #-----------------------------------------------------------#

  join_list3 <- list(psp3, numhts, mastertable)

  psp4 <- Reduce(function(x,y)
    merge(x, y, by = "SAMP_ID", all.x = T),
    join_list3)

  #-------cleanup data tables used to create psp4-------------#

  rm(indidata, join_list3, nhts1, nhts2, indicolname,
     interestcols, prefix, psp2_1)

  #-----------------------------------------------------------#

  # update ratings with planned remeasures for the upcoming planning season

  psp4[, ":=" (PLAN_YR = as.numeric(PLAN_YR),
               MEAS_YR_LAST = as.numeric(MEAS_YR_LAST),
               NO_MEAS = as.numeric(NO_MEAS))]

  psp4b <- data.table::copy(psp4)

  psp4b_simp <- psp4b[, c("SAMP_ID", "PLAN_YR", "MEAS_YR_LAST", "NO_MEAS",
                          paste0("MINT_", 0:12),
                          paste0("NTREES_", 0:12),
                          paste0("NHTS_", 0:7),
                          paste0("DBHLIM_", 0:12)),
                      with = FALSE]
  psp4b_simp[, paste0("NHTS_", 8:12) := NA]
  psp4b_simp[, YR_CALC := as.character(PLAN_YR - MEAS_YR_LAST)]
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

  psp4b <- psp4b[, c(paste0("MINT_", 0:12),
                     paste0("NTREES_", 0:12),
                     paste0("NHTS_", 0:7),
                     paste0("DBHLIM_", 0:12)) := NULL]

  psp4b_simp[, ':='(PLAN_YR = NULL,
                    MEAS_YR_LAST = NULL,
                    NO_MEAS = NULL) ]
  psp4b <- merge(psp4b, psp4b_simp,
                 by = "SAMP_ID")

  # reassign last measurement year to the planned measurement year

  psp4b[PLAN_YR >= 2020,
        ":=" (MEAS_YR_LAST = PLAN_YR,
              CALEND_YR_LAST = PLAN_YR,
              STEM_MAPPED_IND = "Y")]

  psp5 <- data.table::copy(psp4b)

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

  psp5[, P1 := ifelse(SAMPLETYPE %in% "VRI", 1.0,
                      ifelse(AREA > 0 & !is.na(AREA),
                             exp(-exp(a-(b*AREA))),
                             0))]

  # total number of trees is evaluated, based on first msmt, to be consistent
  # with original fpc ranking standards

  # evaluation of total number of trees should be based on the individual plot
  # for those samples with multiple plots in a given sample id;
  # use the average density across all plots as the basis

  psp5[, NO_PLOTS := as.numeric(NO_PLOTS)]

  psp5[, P2 := ifelse(NTREES_0 > 0 & !is.na(NTREES_0),
                      exp(-exp(c-(d*(NTREES_0/NO_PLOTS)))),
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
               BUFFER_RAD = as.numeric(BUFFER_RAD))]

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

  psp5[, P4 := ifelse(STEM_MAPPED_IND == "Y" & !is.na(STEM_MAPPED_IND),
                      1.0, 0)]

  # activity status assume missing is "A"

  psp5[, P5 := ifelse(SAMP_STS == "A" | is.na(SAMP_STS),
                      1.0, 0)]

  # presence of buffer
  # as per Anya's comment on 2023-04-21
  # buffer is no longer used for ranking process
  psp5[, P6 := 0]

  # by measurement ranks

  psp5[, MT := 0]
  psp5[MINT_0 == 0, MINT_0 := 10]

  # for single-measurement samples, need to have a default interval

  psp5_simp <- psp5[,c("SAMP_ID",
                       paste0("NTREES_", 0:12),
                       paste0("MINT_", 0:12),
                       paste0("NHTS_", 0:7),
                       paste0("DBHLIM_", 0:12),
                       letters[1:8],
                       paste0("P", 1:6)),
                    with = FALSE]

  psp5_simp[, 2:ncol(psp5_simp)] <- lapply(2:ncol(psp5_simp),
                                           function(x)
                                             as.numeric(psp5_simp[[x]]))


  psp5_simp[, paste0("NHTS_", 8:12) := NA]

  psp5_simp[, MT := 0]

  for (j in 0:12) {
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

    psp5_simp[,c("NTREES", "MINT", "NHTS", "DBHLIM",
                 "M2", "M3") := NULL]

    psp5_simp[, RATING := 30*P1 + 30*P2 + 20*P3 + 5*P4 + 5*P5 + 20*P6 + MT]
  }

  psp5 <- merge(psp5,
                psp5_simp[,.(SAMP_ID, MT, M1, RATING)],
                by = "SAMP_ID",
                all.x = TRUE)

  #--------------clean up psp5 columns--------------------#

  psp5 <- psp5[, !c(letters[1:8],
                    paste0("P", 1:6),
                    paste0("NTREES_", 0:12),
                    paste0("MINT_", 0:12),
                    paste0("NHTS_", 0:7),
                    paste0("DBHLIM_", 0:12),
                    "MT.x",
                    "MT.y",
                    "M1"), with = FALSE]

  #--------------------------------------------------------#

  # redefine last measurement year

  psp6_simp <- psp5[, c("SAMP_ID",
                        paste0("MEASYR_", 0:12)),
                    with = FALSE]

  psp6_simp[, LAST_MEAS_YR := 0]
  psp6_simp[, 2:ncol(psp6_simp)] <- lapply(2:ncol(psp6_simp),
                                           function(x)
                                             as.numeric(psp6_simp[[x]]))

  for(i in 0:12){
    setnames(psp6_simp,
             paste0("MEASYR_", i),
             "MEASYR")

    psp6_simp[MEASYR > LAST_MEAS_YR,
              LAST_MEAS_YR := MEASYR]

    psp6_simp[, "MEASYR" := NULL]
  }

  psp6 <- data.table::copy(psp5)

  psp6 <- merge(psp6, psp6_simp[, .(SAMP_ID, LAST_MEAS_YR)], by = "SAMP_ID")

  #------------cleanup for-loop process and psp5 -------------#

  rm(psp4b_simp, psp5_simp, psp6_simp, i, j, no_meas_in)

  #-------------------------------------------------#

  # prep for ranking
  # this will rank only active Growth natural MOF and Industry owned,
  # in crown forest, >=0.04ha, with access info, measured since 1996

  psp6_mofgr <- psp6[SAMPLETYPE %in% c("G", "R") & STS_STRATA == "A" &
                       PRIVATE == "N" & AREA_PM_MIN == "Y" &
                       ACCESS == "Y" & MEAS_YR_LAST >= 1996 &
                       CELL_COMPLETE == "Y", ]

  # this will rank all PSPs regardless of ownership, in crown forest,
  # >=0.04ha, with access info, measured since 1996

  psp6_all <- psp6[STS_STRATA == "A" & SAMPLETYPE %in% c("G", "R", "VRI",
                                                         "CMI", "YSM", "CMO",
                                                         "EP", "FLT", "VLT", "SUP") &
                     PRIVATE == "N" & (AREA_PM_MIN == "Y" | SAMPLETYPE == "VRI") &
                     ACCESS == "Y" & MEAS_YR_LAST >= 1996 & CELL_COMPLETE == "Y"]

  #----------------------------------------------------------------#

  # rank by cell_key1
  # determine priority samples per cell_key for all samples

  psp6_all <- psp6_all %>%
    arrange(CELL_KEY1, desc(RATING))

  psp6_all <- psp6_all %>%
    group_by(CELL_KEY1) %>%
    mutate(RANK_ALL1 = row_number()) %>%
    arrange(SAMP_ID) %>%
    setDT()
  psp6_all <- psp6_all[,.(SAMP_ID, RANK_ALL1)]

  # determine priority samples per cell_key for psp growth naturals only samples

  psp6_mofgr <- psp6_mofgr %>%
    arrange(CELL_KEY1, desc(RATING))

  psp6_mofgr <- psp6_mofgr %>%
    group_by(CELL_KEY1) %>%
    mutate(RANK_PSP1 = row_number()) %>%
    arrange(SAMP_ID) %>%
    setDT()
  psp6_mofgr <- psp6_mofgr[,.(SAMP_ID, RANK_PSP1)]

  #-----------------------------------------------------------#

  join_list4 <- list(psp6, psp6_all, psp6_mofgr)
  psp6f <- Reduce(function(x,y)
    merge(x, y, all.x = T),
    join_list4)

  #-------cleanup data tables used to create psp6f-------------#

  rm(psp6_all, psp6_mofgr, join_list4)

  #------------------------------------------------------------#

  psp6f[, NUM_TREES_LIV := STEMSHA_LIV * as.numeric(AREA_PM)] # need to keep what has been done for this

  psp6f[, ":=" (BGC_ZONE_DUP = BGC_ZONE,
                STS_STRATA_DUP = STS_STRATA,
                TRT_STRATA_DUP = TRT_STRATA,
                BEC_STRATA_DUP = BEC_STRATA,
                SS_STRATA_DUP = SS_STRATA,
                SPC_STRATA_DUP = SPC_STRATA,
                SPC1_DUP = SPC1,
                AGE_STRATA_DUP = AGE_STRATA,
                DEN_STRATA_DUP = DEN_STRATA,
                SI_STRATA_DUP = SI_STRATA,
                RECON_YR_DUP = RECON_YR,
                MEAS_YR_LAST_DUP = MEAS_YR_LAST)]

  psp6f[CELL_KEY1 == "MISS_STRATA",
        ":=" (RANK_ALL1 = NA,
              RANK_PSP1 = NA)]

  # next, for each matrix cell, identify the maximum number of group samples in each cell
  # cell_key1

  psp6f <- psp6f %>%
    arrange(CELL_KEY1)

  max1 <- psp6f[, .(SAMP_ID, CELL_KEY1, RANK_PSP1, RANK_ALL1)]

  max1 <- max1 %>%
    group_by(CELL_KEY1) %>%
    summarise(FREQ = n(), MAX_PSP1 = max(RANK_PSP1, na.rm = T),
              MAX_ALL1 = max(RANK_ALL1, na.rm = T)) %>%
    setDT()

  max1[MAX_PSP1 == "-Inf", MAX_PSP1 := NA]
  max1[MAX_ALL1 == "-Inf", MAX_ALL1 := NA]

  psp71 <- merge(psp6f, max1, by = "CELL_KEY1", all = T)
  psp71 <- arrange(psp71, SAMP_ID)

  #---------------- ownsched -------------------------------#

  own1 <- read.xlsx(file.path(wkdir,
                              "lastversionSAS",
                              "own_sched_2018sep26.xlsx")) %>%
    data.table

  names(own1) <- toupper(names(own1))
  psp72 <- psp71[, OWN_SCHED_DESCRIP := NULL]

  psp73 <- merge(psp72, own1, by = "OWN_SCHED", all.x = TRUE)

  #-------------------------------------------------------#

  my_data_psp_sumry_rating <- data.table::copy(psp73)

  sumry1 <- my_data_psp_sumry_rating[, .(SITE_IDENTIFIER,
                                         SAMP_ID,
                                         PROJ_ID,
                                         SAMP_NO,
                                         TYPE_CD,
                                         SAMPLETYPE,
                                         OWN_SCHED,
                                         OWN_SCHED_DESCRIP,
                                         PRIVATE,
                                         OWNER,
                                         OLD_OWNER,
                                         NEW_OWNER,
                                         TSA,
                                         TSA_SOURCE,
                                         MGMT_UNIT,
                                         MGMT_UNIT_SOURCE,
                                         MGMT_UNIT_GYS,
                                         MGMT_UNIT_ISMC,
                                         PROJECT,
                                         BEC_SOURCE,
                                         BGC_ZONE,
                                         BECLABEL,
                                         BECLABEL_GRD,
                                         BGC_SS_GRD,
                                         MAP_TILE,
                                         POLYGON_NO,
                                         OPENING_NO,
                                         UTM_VERSION,
                                         UTM_SOURCE,
                                         UTM_ZONE,
                                         UTM_EASTING,
                                         UTM_NORTHING,
                                         UTM_SOURCE_ISMC,
                                         UTM_ZONE_ISMC,
                                         UTM_EASTING_ISMC,
                                         UTM_NORTHING_ISMC,
                                         UTM_SOURCE_GYS,
                                         UTM_ZONE_GYS,
                                         UTM_EASTING_GYS,
                                         UTM_NORTHING_GYS,
                                         UTM_SOURCE_RECON,
                                         UTM_ZONE_RECON,
                                         UTM_EASTING_RECON,
                                         UTM_NORTHING_RECON,
                                         UTM_SOURCE_REMEASOP1,
                                         UTM_ZONE_REMEASOP1,
                                         UTM_EASTING_REMEASOP1,
                                         UTM_NORTHING_REMEASOP1,
                                         UTM_SOURCE_REMEASOP2,
                                         UTM_ZONE_REMEASOP2,
                                         UTM_EASTING_REMEASOP2,
                                         UTM_NORTHING_REMEASOP2,
                                         BCALB_X,
                                         BCALB_Y,
                                         LATITUDE,
                                         LONGITUDE,
                                         ASPECT,
                                         SLOPE,
                                         SL_POS,
                                         ELEV,
                                         SAMP_STS,
                                         STS_STRATA,
                                         TRT_STRATA,
                                         BEC_STRATA,
                                         SS_STRATA,
                                         SPC_STRATA,
                                         AGE_STRATA,
                                         DEN_STRATA,
                                         SI_STRATA,
                                         LANDSAT_DISTURB_YR,
                                         MEAS_YR_FIRST,
                                         MEAS_YR_LAST,
                                         CALEND_YR_LAST,
                                         CALEND_YR_LAST_HARDY,
                                         RECON_YR,
                                         RELEASE,
                                         NO_PLOTS,
                                         RATING,
                                         CELL_KEY1,
                                         RANK_ALL1,
                                         RANK_PSP1,
                                         MAX_PSP1,
                                         MAX_ALL1,
                                         AREA_PM,
                                         AREA_PM_MIN,
                                         SHP_PM,
                                         RAD_PM,
                                         LEN_PM,
                                         WID_PM,
                                         AREA,
                                         NO_MEAS,
                                         TOT_PERIOD,
                                         STEMSHA_LIV,
                                         WSVHA_LIV,
                                         SPC_LABEL_LIVE,
                                         TOT_STAND_AGE,
                                         SPC1,
                                         SAMPLETYPE_DUP,
                                         BGC_ZONE_DUP,
                                         STS_STRATA_DUP,
                                         TRT_STRATA_DUP,
                                         BEC_STRATA_DUP,
                                         SS_STRATA_DUP,
                                         SPC_STRATA_DUP,
                                         SPC1_DUP,
                                         AGE_STRATA_DUP,
                                         DEN_STRATA_DUP,
                                         SI_STRATA_DUP,
                                         RECON_YR_DUP,
                                         MEAS_YR_LAST_DUP,
                                         ACCESS_COORD,
                                         ACCESS_NOTE,
                                         ACCESS,
                                         SITE_ACCESS_CODE,
                                         BAF_PM,
                                         PLOT_TYP,
                                         FIZ,
                                         BUFFER_RAD,
                                         BUFFER_OK,
                                         STEM_MAPPED_IND,
                                         STND_ORG,
                                         STND_STR,
                                         SEL_LGD,
                                         TREATMENT,
                                         TRT_CODE,
                                         CELL_COMPLETE)]

  sumry2 <- my_data_psp_sumry_rating[, c(paste0("WSVL_", 0:12),
                                         paste0("TAGE_", 0:12),
                                         paste0("MEASYR_", 0:12))]

  my_data_psp_sumry_rating <- cbind(sumry1, sumry2)

  my_data_psp_sumry_rating[, ":=" (SS_STRATA = as.numeric(SS_STRATA),
                                   SS_STRATA_DUP = as.numeric(SS_STRATA_DUP))]

  my_data_psp_sumry_rating <- arrange(my_data_psp_sumry_rating, SAMP_ID)

  return(my_data_psp_sumry_rating)

}

