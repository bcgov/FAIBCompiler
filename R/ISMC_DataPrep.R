#' prepare raw data from ismc for ISMCCompiler
#'
#'
#' @description This function is to prepare ismc data for compilation and save them
#'              in \code{outputPath} folder
#'
#' @param compilationType character, either \code{PSP} or \code{nonPSP}. If it is \code{PSP}, it
#'                               is consistent with original PSP compiler, otherwise, it
#'                               is consistent with VRI compiler.
#' @param inputPath character, Specifies the path that stores data from oracle data base.
#' @param outputPath character, Specifies the path to save your outputs. If missing, the current working
#'                   directory will be choose.
#' @param coeffPath character, Specifies the path where the crosswalk
#'                   table is stored.
#'
#' @return no item returned
#'
#' @importFrom data.table ':=' data.table melt
#' @importFrom dplyr '%>%'
#'
#' @rdname ISMC_DataPrep
#' @author Yong Luo
ISMC_DataPrep <- function(compilationType,
                          inputPath,
                          outputPath,
                          coeffPath){
  sitenavigation <- readRDS(dir(inputPath, pattern = "SiteNavigation.rds", full.names = TRUE)) %>%
    data.table
  sitenavigation <- sitenavigation[!is.na(UTM_ZONE) &
                                     !is.na( UTM_EASTING) &
                                     !is.na(UTM_NORTHING),]
  sitenavigation[, lastvisit := max(VISIT_NUMBER),
                 by = "SITE_IDENTIFIER"]
  actualcoord <- sitenavigation[VISIT_NUMBER == lastvisit,
                                .(SITE_IDENTIFIER,
                                  UTM_ZONE_act = UTM_ZONE,
                                  UTM_EASTING_act = UTM_EASTING,
                                  UTM_NORTHING_act = UTM_NORTHING,
                                  ELEVATION_act = ELEVATION,
                                  source_act = POINT_LOCATION_TYPE_CODE)]

  samplesites <- readRDS(dir(inputPath, pattern = "SampleSites.rds", full.names = TRUE)) %>%
    data.table

  samplesites <- samplesites[,.(SITE_IDENTIFIER, SAMPLE_SITE_NAME,
                                IP_UTM, IP_EAST, IP_NRTH,
                                UTM_SOURCE = COORDINATE_SOURCE_CODE,
                                CORRDINATE_SOURCE = POINT_LOCATION_TYPE_CODE,
                                IP_ELEV,
                                SAMPLING_REGION_NUMBER, COMPARTMENT_NUMBER,
                                FOREST_INVENTORY_ZONE_CD, STAND_ORIGIN_CODE,
                                SITE_STATUS_CODE, SITE_CONDITION_CODE,
                                PSP_TYPE = PSP_SAMPLE_SITE_TYPE_CODE)]
  samplesites <- merge(samplesites,
                       actualcoord,
                       by = "SITE_IDENTIFIER",
                       all.x = TRUE)
  samplesites[!is.na(UTM_ZONE_act) &
                !is.na(UTM_EASTING_act) &
                !is.na(UTM_NORTHING_act),
              ':='(IP_UTM = UTM_ZONE_act,
                   IP_EAST = UTM_EASTING_act,
                   IP_NRTH = UTM_NORTHING_act,
                   IP_ELEV = ELEVATION_act,
                   CORRDINATE_SOURCE = source_act)]
  samplesites[,':='(UTM_ZONE_act = NULL,
                    UTM_EASTING_act = NULL,
                    UTM_NORTHING_act = NULL,
                    ELEVATION_act = NULL,
                    source_act = NULL)]

  # for PSP, the I samples should be removed from compilation
  if(compilationType == "PSP"){
    samplesites <- samplesites[!(PSP_TYPE %in% c("I", "T")),] #
    ## see Rene's email on Nov. 22, 2022
  }

  if(nrow(samplesites) != length(unique(samplesites$SITE_IDENTIFIER))){
    warning("samplesites file: SITE_IDENTIFIER is not unique.")
  }
  SampleSiteVisits <- readRDS(dir(inputPath, pattern = "SampleSiteVisits.rds",
                                  full.names = TRUE)) %>%
    data.table
  SampleSiteVisits[, newMD := SAMPLE_SITE_VISIT_START_DATE + 60*60]
  # for nonPSP
  # extract suit_si from notes there are 7912 observations found, with 7900 have valid suit_si code, i.e., yes or no
  if(compilationType == "nonPSP"){
    samplesitevisitenotes <- SampleSiteVisits[,.(SITE_IDENTIFIER, VISIT_NUMBER, SAMPLE_SITE_VISIT_COMMENT)]
    samplesitevisitenotes[, SAMPLE_SITE_VISIT_COMMENT := toupper(gsub(" ", "", SAMPLE_SITE_VISIT_COMMENT))]
    samplesitevisitenotes <- samplesitevisitenotes[grepl("SITEINDEXTREESUITABILITY", SAMPLE_SITE_VISIT_COMMENT, fixed = TRUE), ]
    suit_si_from_notes <- NULL
    for(indirow in 1:nrow(samplesitevisitenotes)){
      indipositiontable <- data.table(start_position = as.numeric(gregexpr("SITEINDEXTREESUITABILITY",
                                                                           samplesitevisitenotes$SAMPLE_SITE_VISIT_COMMENT[indirow])[[1]]))

      indipositiontable[,':='(SITE_IDENTIFIER = samplesitevisitenotes$SITE_IDENTIFIER[indirow],
                              VISIT_NUMBER = samplesitevisitenotes$VISIT_NUMBER[indirow],
                              SAMPLE_SITE_VISIT_COMMENT = samplesitevisitenotes$SAMPLE_SITE_VISIT_COMMENT[indirow])]

      suit_si_from_notes <- rbind(suit_si_from_notes, indipositiontable)
    }
    suit_si_from_notes[, firstcut := substr(SAMPLE_SITE_VISIT_COMMENT, start_position+24, start_position+30)]
    suit_si_from_notes[, yesposition := unlist(lapply(firstcut, function(x){as.numeric(gregexpr("YES", x)[[1]])}))]
    suit_si_from_notes[, noposition := unlist(lapply(firstcut, function(x){as.numeric(gregexpr("NO", x)[[1]][1])}))]
    suit_si_from_notes[yesposition > 0, TREE_NO_yes := as.numeric(substr(firstcut, 1, yesposition-1))]
    suit_si_from_notes[noposition > 0, TREE_NO_no := as.numeric(substr(firstcut, 1, noposition-1))]
    suit_si_from_notes[(!is.na(TREE_NO_yes) & is.na(TREE_NO_no)), ':='(TREE_NUMBER = TREE_NO_yes,
                                                                       SUIT_SI_temp = "Y")]
    suit_si_from_notes[(is.na(TREE_NO_yes) & !is.na(TREE_NO_no)), ':='(TREE_NUMBER = TREE_NO_no,
                                                                       SUIT_SI_temp = "N")]
    suit_si_from_notes <- suit_si_from_notes[!is.na(TREE_NUMBER),.(SITE_IDENTIFIER, VISIT_NUMBER, TREE_NUMBER, SUIT_SI_temp)]
  }
  vi_a <- unique(SampleSiteVisits[,.(CLSTR_ID = NA,
                                     SITE_IDENTIFIER, PROJ_ID = PROJECT_NAME,
                                     PROJECT_DESCRIPTOR,
                                     STAND_DISTURBANCE_CODE, STAND_STRUCTURE_CODE,
                                     SAMPLE_BREAK_POINT, SAMPLE_BREAK_POINT_TYPE,
                                     STEM_MAP_REQD_IND,
                                     SELECTIVELY_LOGGED_IND,
                                     SAMP_NO = PROJECT_NUMBER, TYPE_CD = SAMPLE_SITE_PURPOSE_TYPE_CODE,
                                     VISIT_NUMBER,
                                     SAMPLE_SITE_PURPOSE_TYPE_DESCRIPTION,
                                     MEAS_DT = substr(newMD, 1, 10),
                                     IP_AZ_PN = NA,
                                     IP_DT_PN = NA, IP_AZ_GP = NA, IP_DT_GP = NA, IP_GPSID = NA)],
                 by = c("SITE_IDENTIFIER", "VISIT_NUMBER"))

  # manually correct measurement date
  ### for the sample site 4016893 and visit 3,
  ### the sample date is 2008-04-01 based on Chris
  ### see email on 2021-08-26
  vi_a[SITE_IDENTIFIER == 4016893 &
         VISIT_NUMBER == 3,
       MEAS_DT := "2008-04-01"]
  vi_a <- vi_a[SITE_IDENTIFIER %in% samplesites$SITE_IDENTIFIER,]
  vi_a <- merge(vi_a, samplesites,
                by = "SITE_IDENTIFIER",
                all.x = TRUE)
  ### for some sites, there are two sample site access codes (see emails on 2023-01-11)
  ### we need to combine access codes into one
  siteaccess <- SampleSiteVisits[!is.na(SITE_ACCESS_CODE),.(SITE_IDENTIFIER,
                                                            SITE_ACCESS_CODE, DESCRIPTION)]
  siteaccess_new <- siteaccess[, .(SITE_ACCESS_CODE = paste0(SITE_ACCESS_CODE, collapse = ", "),
                                   DESCRIPTION = paste0(DESCRIPTION, collapse = ", ")),
                               by = "SITE_IDENTIFIER"]
  vi_a <- merge(vi_a, siteaccess_new,
                by = "SITE_IDENTIFIER",
                all.x = TRUE)
  rm(samplesites, SampleSiteVisits, siteaccess_new, siteaccess)
  gc()
  vi_a[,CLSTR_ID := paste(SITE_IDENTIFIER, paste0(TYPE_CD, VISIT_NUMBER), sep = "-")]
  vi_a <- unique(vi_a)

  ## collect dbh_tagging_limit from sample measurement table
  samplemeasurement <- readRDS(dir(inputPath, pattern = "SampleMeasurements.rds",
                                   full.names = TRUE))
  samplemeasurement <- unique(samplemeasurement[!is.na(DBH_TAGGING_LIMIT), .(SITE_IDENTIFIER,
                                                                             VISIT_NUMBER,
                                                                             DBH_TAGGING_LIMIT)],
                              by = c("SITE_IDENTIFIER", "VISIT_NUMBER"))
  vi_a <- merge(vi_a,
                samplemeasurement,
                by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                all.x = TRUE)
  # ## assign dbh_tagging_limit for NAs
  # ## 1) 9.1cm for measurement year < 1980
  # ## 2) 7.5cm for measurement 1980<=year<=1990
  # ## 2) 4cm for measurement 1990 < year
  # ## based on Rene's previous compiler
  # vi_a[, DBH_TAGGING_LIMIT_org := DBH_TAGGING_LIMIT]
  # vi_a[is.na(DBH_TAGGING_LIMIT) &
  #        as.numeric(substr(MEAS_DT, 1, 4)) < 1980,
  #      DBH_TAGGING_LIMIT := 9.1]
  # vi_a[is.na(DBH_TAGGING_LIMIT) &
  #        as.numeric(substr(MEAS_DT, 1, 4)) >= 1980 &
  #        as.numeric(substr(MEAS_DT, 1, 4)) <= 1990,
  #      DBH_TAGGING_LIMIT := 7.5]
  # vi_a[is.na(DBH_TAGGING_LIMIT) &
  #        as.numeric(substr(MEAS_DT, 1, 4)) > 1990 &
  #        SAMPLE_BREAK_POINT == 2,
  #      DBH_TAGGING_LIMIT := 2]
  # vi_a[is.na(DBH_TAGGING_LIMIT) &
  #        as.numeric(substr(MEAS_DT, 1, 4)) > 1990,
  #      DBH_TAGGING_LIMIT := 4]
  ## hard code fix for site = 4042369 visit 3, the break point should be 4
  vi_a[SITE_IDENTIFIER == 4042369 & VISIT_NUMBER == 3,
       SAMPLE_BREAK_POINT := 4]
  ## hard code to fill missing samplebreak point
  vi_a[substr(MEAS_DT, 1, 4) >= 2020 &
         is.na(SAMPLE_BREAK_POINT),
       ':='(SAMPLE_BREAK_POINT = 4,
            SAMPLE_BREAK_POINT_TYPE = "D")]
  vi_a[SITE_IDENTIFIER == 4002144 &
         VISIT_NUMBER == 1,
       ':='(SAMPLE_BREAK_POINT = 4,
            SAMPLE_BREAK_POINT_TYPE = "D")]
  vi_a[SITE_IDENTIFIER == 4044744 &
         VISIT_NUMBER == 2,
       ':='(SAMPLE_BREAK_POINT = 4,
            SAMPLE_BREAK_POINT_TYPE = "D")]
  vi_a[SITE_IDENTIFIER == 4001722 &
         VISIT_NUMBER == 3,
       ':='(SAMPLE_BREAK_POINT = 4,
            SAMPLE_BREAK_POINT_TYPE = "D")]

  ## see communications between Dan and I on March 07, 2023
  vi_a[, DBH_TAGGING_LIMIT_org := DBH_TAGGING_LIMIT]
  vi_a[is.na(DBH_TAGGING_LIMIT),
       DBH_TAGGING_LIMIT := 2]
  vi_a[SAMPLE_BREAK_POINT < DBH_TAGGING_LIMIT,
       ':='(SAMPLE_BREAK_POINT = 4)]

  saveRDS(vi_a, file.path(outputPath, "vi_a.rds"))
  plotdetails <- readRDS(dir(inputPath, pattern = "PlotDetails.rds",
                             full.names = TRUE))

  plotdetails <- merge(plotdetails, vi_a[,.(SITE_IDENTIFIER, VISIT_NUMBER, TYPE_CD,
                                            CLSTR_ID)],
                       by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                       all.x = TRUE)
  plotdetails <- plotdetails[!is.na(CLSTR_ID),]
  # for nonPSP, a sample may contain multiple plots,
  # which is flagged in plot_category code,
  # for the psp, a sample also may contain multiple plots
  # which is flagged in plot number
  if(compilationType == "nonPSP"){
    vi_b <- plotdetails[PLOT_CATEGORY_CODE %in% c("IPC TD", "AUX S", "AUX W",
                                                  "AUX N", "AUX E"),
                        .(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, TYPE_CD,
                          PLOT = PLOT_CATEGORY_CODE,
                          V_BAF = VARIABLE_BAF, F_RAD = PLOT_RADIUS,
                          PLOT_AREA, PLOT_WIDTH, PLOT_LENGTH,
                          PARTIAL_PLOT_REASON_CODE, PLOT_SEGMENT_CODE,
                          PLOT_SLOPE, PLOT_ASPECT, PLOT_ELEVATION,
                          PLOT_SHAPE_CODE,
                          SMALL_TREE_SUBPLOT_RADIUS)]
    vi_b[PLOT == "IPC TD", PLOT := "I"]
    vi_b[PLOT != "I", PLOT := substr(PLOT, 5, 5)]
  } else {
    vi_b <- plotdetails[PLOT_CATEGORY_CODE %in% c("IPC TD", "AUX S", "AUX W",
                                                  "AUX N", "AUX E"),
                        .(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, TYPE_CD,
                          PLOT = PLOT_NUMBER,
                          V_BAF = VARIABLE_BAF, F_RAD = PLOT_RADIUS,
                          PLOT_AREA, PLOT_WIDTH, PLOT_LENGTH,
                          PARTIAL_PLOT_REASON_CODE, PLOT_SEGMENT_CODE,
                          PLOT_SLOPE, PLOT_ASPECT, PLOT_ELEVATION,
                          PLOT_SHAPE_CODE,
                          SMALL_TREE_SUBPLOT_RADIUS)]
  }
  vi_b_master <- unique(vi_b[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, TYPE_CD,
                                PLOT, V_BAF, F_RAD,
                                PLOT_AREA, PLOT_WIDTH, PLOT_LENGTH,
                                PLOT_SLOPE, PLOT_ASPECT, PLOT_ELEVATION,
                                PLOT_SHAPE_CODE, SMALL_TREE_SUBPLOT_RADIUS)],
                        by = c("CLSTR_ID", "PLOT"))
  fixplots <- vi_b[!is.na(F_RAD),]
  vi_b_master <- merge(vi_b_master,
                       fixplots[PARTIAL_PLOT_REASON_CODE == "BOUNDARY",
                                .(CLSTR_ID, PLOT, F_BDRY = TRUE)],
                       by = c("CLSTR_ID", "PLOT"),
                       all.x = TRUE)
  vi_b_master <- merge(vi_b_master,
                       fixplots[PARTIAL_PLOT_REASON_CODE == "SPLIT",
                                .(CLSTR_ID, PLOT, F_SPLT = TRUE)],
                       by = c("CLSTR_ID", "PLOT"),
                       all.x = TRUE)
  vi_b_master <- merge(vi_b_master,
                       fixplots[PLOT_SEGMENT_CODE == "FULL",
                                .(CLSTR_ID, PLOT, F_FULL = TRUE)],
                       by = c("CLSTR_ID", "PLOT"),
                       all.x = TRUE)
  vi_b_master <- merge(vi_b_master,
                       fixplots[PLOT_SEGMENT_CODE == "HALF",
                                .(CLSTR_ID, PLOT, F_HALF = TRUE)],
                       by = c("CLSTR_ID", "PLOT"),
                       all.x = TRUE)
  vi_b_master <- merge(vi_b_master,
                       fixplots[PLOT_SEGMENT_CODE == "QTR",
                                .(CLSTR_ID, PLOT, F_QRTR = TRUE)],
                       by = c("CLSTR_ID", "PLOT"),
                       all.x = TRUE)
  varplots <- vi_b[!is.na(V_BAF),]
  vi_b_master <- merge(vi_b_master,
                       varplots[PARTIAL_PLOT_REASON_CODE == "BOUNDARY",
                                .(CLSTR_ID, PLOT, V_BDRY = TRUE)],
                       by = c("CLSTR_ID", "PLOT"),
                       all.x = TRUE)
  vi_b_master <- merge(vi_b_master,
                       varplots[PARTIAL_PLOT_REASON_CODE == "SPLIT",
                                .(CLSTR_ID, PLOT, V_SPLT = TRUE)],
                       by = c("CLSTR_ID", "PLOT"),
                       all.x = TRUE)
  vi_b_master <- merge(vi_b_master,
                       varplots[PLOT_SEGMENT_CODE == "FULL",
                                .(CLSTR_ID, PLOT, V_FULL = TRUE)],
                       by = c("CLSTR_ID", "PLOT"),
                       all.x = TRUE)
  vi_b_master <- merge(vi_b_master,
                       varplots[PLOT_SEGMENT_CODE == "HALF",
                                .(CLSTR_ID, PLOT, V_HALF = TRUE)],
                       by = c("CLSTR_ID", "PLOT"),
                       all.x = TRUE)
  vi_b_master <- merge(vi_b_master,
                       varplots[PLOT_SEGMENT_CODE == "QTR",
                                .(CLSTR_ID, PLOT, V_QRTR = TRUE)],
                       by = c("CLSTR_ID", "PLOT"),
                       all.x = TRUE)
  rm(fixplots, varplots)
  vi_b_master[TYPE_CD %in% c("M", "Y", "F") &
                (F_HALF == TRUE | F_QRTR == TRUE),
              ':='(F_FULL = TRUE,
                   F_HALF = NA,
                   F_QRTR = NA)]
  vi_b_master[CLSTR_ID %in% vi_a[SAMPLE_BREAK_POINT == DBH_TAGGING_LIMIT]$CLSTR_ID,
              SMALL_TREE_SUBPLOT_RADIUS := 0]
  # since 2021, if subplot area is missing and plot is either sq or rectangle,
  # a 5.64 circle subplot will be used, as per communications with Chris on 2023-03-13
  vi_b_master[CLSTR_ID %in% vi_a[substr(MEAS_DT, 1, 4) > 2020,]$CLSTR_ID &
                !is.na(PLOT_WIDTH) &
                is.na(SMALL_TREE_SUBPLOT_RADIUS),
              SMALL_TREE_SUBPLOT_RADIUS := 5.64]

  ## use subplot area lookup table to populate subplot area when it is missing
  if(compilationType == "PSP"){
    subplotarea_lookup <- read.csv(file.path(coeffPath, "area_subplot_lookup.csv"))
    vi_b_master <- merge(vi_b_master,
                         subplotarea_lookup,
                         by = c("CLSTR_ID", "PLOT"),
                         all.x = TRUE)
  } else {
    vi_b_master[, AREA_PS := as.numeric(NA)]
  }
  saveRDS(vi_b_master, file.path(outputPath, "vi_b.rds"))

  vi_e <- unique(plotdetails[PLOT_CATEGORY_CODE %in% c("IPC SM",  "IPC ST"),
                             .(CLSTR_ID, PLOT = "I", F_RAD = PLOT_RADIUS, PLOT_CATEGORY_CODE)],
                 by = c("CLSTR_ID",  "PLOT_CATEGORY_CODE"))

  vi_e <- merge(vi_e,
                plotdetails[PARTIAL_PLOT_REASON_CODE == "BOUNDARY",
                            .(CLSTR_ID, PLOT_CATEGORY_CODE,
                              F_BDRY = "X")],
                by = c("CLSTR_ID", "PLOT_CATEGORY_CODE"),
                all.x = TRUE)
  vi_e <- merge(vi_e,
                plotdetails[PARTIAL_PLOT_REASON_CODE == "SPLIT",
                            .(CLSTR_ID, PLOT_CATEGORY_CODE,
                              F_SPLT = "X")],
                by = c("CLSTR_ID", "PLOT_CATEGORY_CODE"),
                all.x = TRUE)
  vi_e <- merge(vi_e,
                plotdetails[PLOT_SEGMENT_CODE == "FULL",
                            .(CLSTR_ID, PLOT_CATEGORY_CODE,
                              F_FULL = "X")],
                by = c("CLSTR_ID", "PLOT_CATEGORY_CODE"),
                all.x = TRUE)
  vi_e <- merge(vi_e,
                plotdetails[PLOT_SEGMENT_CODE == "HALF",
                            .(CLSTR_ID, PLOT_CATEGORY_CODE,
                              F_HALF = "X")],
                by = c("CLSTR_ID", "PLOT_CATEGORY_CODE"),
                all.x = TRUE)
  vi_e <- merge(vi_e,
                plotdetails[PLOT_SEGMENT_CODE == "QTR",
                            .(CLSTR_ID, PLOT_CATEGORY_CODE,
                              F_QRTR = "X")],
                by = c("CLSTR_ID", "PLOT_CATEGORY_CODE"),
                all.x = TRUE)
  vi_e[PLOT_CATEGORY_CODE == "IPC SM", PL_ORIG := "SML_TR"]
  vi_e[PLOT_CATEGORY_CODE == "IPC ST", PL_ORIG := "STUMP"]
  vi_e[F_FULL == "X", PLOT_WT := 1]
  vi_e[F_HALF == "X", PLOT_WT := 2]
  vi_e[F_QRTR == "X", PLOT_WT := 4]
  vi_e[,PLOT_CATEGORY_CODE := NULL]
  rm(plotdetails)
  saveRDS(vi_e, file.path(outputPath, "vi_e.rds"))


  treemeasurements <- readRDS(dir(inputPath, "TreeMeasurements.rds",
                                  full.names = TRUE)) %>%
    data.table


  # JD = common juniper
  # My guess is we consider Juniper spp as shrubs, not trees.
  # And as a shrub, we would ignore in our compilation.
  # Rene's email on 2022-09-09

  treemeasurements <- treemeasurements[TREE_SPECIES_CODE != "JD",]

  treemeasurements <- merge(treemeasurements,
                            unique(vi_a[,.(SITE_IDENTIFIER, VISIT_NUMBER, TYPE_CD,
                                           CLSTR_ID, MEAS_DT,
                                           SAMPLE_BREAK_POINT)],
                                   by = c("SITE_IDENTIFIER", "VISIT_NUMBER")),
                            by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                            all.x = TRUE)
  treemeasurements <- treemeasurements[!is.na(CLSTR_ID),]

  ## adjust dbh if a tree is from main plot to subplot if the sample break point does not change
  ## based on rene's suggestions on March 14
  treemeasurements_simp <- treemeasurements[!is.na(DIAMETER),.(SITE_IDENTIFIER, PLOT_NUMBER,
                                                               TREE_NUMBER, VISIT_NUMBER,
                                                               DIAMETER,
                                                               SAMPLE_BREAK_POINT)]

  treemeasurements_simp <- treemeasurements_simp[order(SITE_IDENTIFIER, PLOT_NUMBER, TREE_NUMBER,
                                                       VISIT_NUMBER),]
  needadjust <- NULL
  for(i in 1:(max(treemeasurements_simp$VISIT_NUMBER)-1)){
    treemeasurements_simp[DIAMETER %>=% SAMPLE_BREAK_POINT,
                          plot_crt := "M"]
    treemeasurements_simp[is.na(plot_crt),
                          plot_crt := "S"]
    treemeasurements_simp[, plot_first := shift(plot_crt, type = "lag"),
                          by = c("SITE_IDENTIFIER", "PLOT_NUMBER", "TREE_NUMBER")]
    treemeasurements_simp[, breakpoint_first := shift(SAMPLE_BREAK_POINT, type = "lag"),
                          by = c("SITE_IDENTIFIER", "PLOT_NUMBER", "TREE_NUMBER")]

    treemeasurements_simp[!is.na(plot_first), ':='(change = paste0(plot_first, "-", plot_crt))]

    treemeasurements_simp[change == "M-S" &
                            SAMPLE_BREAK_POINT %==% breakpoint_first &
                            VISIT_NUMBER == (i+1),
                          ':='(dbh_new = SAMPLE_BREAK_POINT,
                               DIAMETER = SAMPLE_BREAK_POINT)]
    needadjust <- rbind(needadjust,
                        treemeasurements_simp[!is.na(dbh_new),
                                              .(SITE_IDENTIFIER, PLOT_NUMBER,
                                                TREE_NUMBER, VISIT_NUMBER, dbh_new)])
    treemeasurements_simp <- treemeasurements_simp[VISIT_NUMBER != i,.(SITE_IDENTIFIER, PLOT_NUMBER,
                                                                       TREE_NUMBER, VISIT_NUMBER,
                                                                       DIAMETER,
                                                                       SAMPLE_BREAK_POINT)]

  }
  treemeasurements <- merge(treemeasurements,
                            needadjust,
                            by = c("SITE_IDENTIFIER", "PLOT_NUMBER",
                                   "TREE_NUMBER", "VISIT_NUMBER"),
                            all.x = TRUE)
  treemeasurements[!is.na(dbh_new),
                   DIAMETER := dbh_new]
  treemeasurements[,':='(SAMPLE_BREAK_POINT = NULL,
                         dbh_new = NULL)]
  rm(treemeasurements_simp, needadjust)
  gc()
  # for nonPSP
  if(compilationType == "nonPSP"){
    # remove A samples from vol and age compilation
    ## output A sample treelist
    ## after discussion with Dan and Chris
    treemeasurements_A_samples <- treemeasurements[TYPE_CD == "A",]
    saveRDS(treemeasurements_A_samples,
            file.path(outputPath, "treelist_A_samples.rds"))
    # for some A samples the trees are very small so that the DBH can not be taken
    # I populated diameter_msmt_ht and diameter and flagged this modification with
    # -SizeMOD in measurement_anomaly_code
    treemeasurements[TYPE_CD == "A" &
                       is.na(DIAMETER_MEASMT_HEIGHT) &
                       is.na(LENGTH),
                     LENGTH := 0]
    treemeasurements[TYPE_CD == "A" &
                       is.na(DIAMETER_MEASMT_HEIGHT) &
                       TREE_PLANTED_IND == "Y", # this is for eysm
                     ':='(DIAMETER_MEASMT_HEIGHT = 1.3,
                          DIAMETER = 7, # so that this can be summarized at 4 but not at 7.5cm util
                          LENGTH = 7 + LENGTH,
                          MEASUREMENT_ANOMALY_CODE = paste0(MEASUREMENT_ANOMALY_CODE,
                                                            "-SizeMOD"))]
    # merge suit_si_from_notes to all tree measurements
    treemeasurements <- merge(treemeasurements, suit_si_from_notes,
                              by = c("SITE_IDENTIFIER", "VISIT_NUMBER", "TREE_NUMBER"),
                              all.x = TRUE)
    treemeasurements[!is.na(SUIT_SI_temp),
                     SUITABLE_FOR_SITE_INDEX_IND := SUIT_SI_temp]
    treemeasurements[, SUIT_SI_temp := NULL]

    ### for the NFI samples the tree number changes
    ## the next few line to modify tree number and make them
    ## the same as the previous ones. the crosswalk table is prepared
    ## by Dan
    crosswalk <- read.csv(file.path(coeffPath,
                                    "NFI_TREE_CORRECTION.csv")) %>%
      data.table
    treemeasurements <- merge(treemeasurements,
                              crosswalk,
                              by = c("SITE_IDENTIFIER",
                                     "VISIT_NUMBER",
                                     "TREE_NUMBER"),
                              all.x = TRUE)
    treemeasurements[!is.na(TREE_NUMBER_future),
                     TREE_NUMBER_org := TREE_NUMBER]
    treemeasurements[!is.na(TREE_NUMBER_future),
                     TREE_NUMBER := TREE_NUMBER_future]
    treemeasurements[, TREE_NUMBER_future := NULL]
    treemeasurements[PLOT_CATEGORY_CODE == "IPC TD", PLOT := "I"]
    treemeasurements[PLOT_CATEGORY_CODE != "IPC TD",
                     PLOT := gsub("AUX ", "", PLOT_CATEGORY_CODE)]
  } else {
    # for PSP
    treemeasurements[, TREE_DETAIL_COMMENT := NULL]
    treemeasurements[,':='(BORING_AGE_org = BORING_AGE,
                           MICROSCOPE_AGE_org = MICROSCOPE_AGE,
                           AGE_MEASMT_HEIGHT_org = AGE_MEASMT_HEIGHT)]
    ## this is a temporary fix for new age in ISMC
    treemeasurements[BORING_AGE >= 0 & NEW_AGE > 0 &
                       BORING_AGE != NEW_AGE,
                     ':='(BORING_AGE = NEW_AGE,
                          new_age_replaced = TRUE)]
    treemeasurements[is.na(BORING_AGE) & NEW_AGE > 0,
                     ':='(BORING_AGE = NEW_AGE,
                          new_age_replaced = TRUE)]
    treemeasurements[MICROSCOPE_AGE >= 0 & NEW_AGE > 0 &
                       MICROSCOPE_AGE != NEW_AGE,
                     ':='(MICROSCOPE_AGE = NEW_AGE,
                          new_age_replaced = TRUE)]
    treemeasurements[is.na(MICROSCOPE_AGE) & NEW_AGE > 0,
                     ':='(MICROSCOPE_AGE = NEW_AGE,
                          new_age_replaced = TRUE)]
    treemeasurements[is.na(MICROSCOPE_AGE) & BORING_AGE > 0,
                     ':='(MICROSCOPE_AGE = BORING_AGE)]

    treemeasurements[, ':='(unitree_id = paste0(SITE_IDENTIFIER, "-",
                                                PLOT_NUMBER, "-", TREE_NUMBER),
                            last_grow_year = as.numeric(substr(MEAS_DT, 1, 4)),
                            cutdate = as.Date(paste0(substr(MEAS_DT, 1, 4), "-06-01")))]
    treemeasurements[, MEAS_DT :=  as.Date(MEAS_DT)]
    treemeasurements[MEAS_DT < cutdate,
                     last_grow_year := last_grow_year - 1]
    ## the first age adjustment is for microscope age
    lab_age_meas <- treemeasurements[!is.na(MICROSCOPE_AGE),
                                     .(unitree_id, MICROSCOPE_AGE,
                                       last_grow_year)]
    lab_age_meas[, ref_year_lab := max(last_grow_year),
                 by = "unitree_id"]
    lab_age_meas <- lab_age_meas[last_grow_year == ref_year_lab,
                                 .(unitree_id, ref_year_lab,
                                   ref_age_lab = MICROSCOPE_AGE)]

    fld_age_meas <- treemeasurements[!is.na(BORING_AGE),
                                     .(unitree_id, BORING_AGE, last_grow_year,
                                       AGE_MEASMT_HEIGHT)]
    fld_age_meas <- fld_age_meas[,ref_year_fld := max(last_grow_year),
                                 by = "unitree_id"]
    fld_age_meas <- fld_age_meas[last_grow_year == ref_year_fld,
                                 .(unitree_id, ref_year_fld,
                                   ref_age_fld = BORING_AGE,
                                   ref_boring_ht = AGE_MEASMT_HEIGHT)]

    treemeasurements <- merge(treemeasurements,
                              lab_age_meas,
                              by = "unitree_id",
                              all.x = TRUE)
    treemeasurements <- merge(treemeasurements,
                              fld_age_meas,
                              by = "unitree_id",
                              all.x = TRUE)

    treemeasurements[, ':='(adjusted_boring_age = (last_grow_year - ref_year_fld) + ref_age_fld,
                            adjusted_lab_age = (last_grow_year - ref_year_lab) + ref_age_lab)]
    # make adjustment for boring age
    treemeasurements[BORING_AGE > 0 & adjusted_boring_age > 0 &
                       BORING_AGE != adjusted_boring_age,
                     ':='(BORING_AGE = adjusted_boring_age,
                          boring_age_adjusted = TRUE)]
    # make adjustment for lab age
    treemeasurements[MICROSCOPE_AGE > 0 & adjusted_lab_age > 0 &
                       MICROSCOPE_AGE != adjusted_lab_age,
                     ':='(MICROSCOPE_AGE = adjusted_lab_age,
                          lab_age_adjusted = TRUE)]
    # make adjustment boring height
    treemeasurements[AGE_MEASMT_HEIGHT != ref_boring_ht,
                     ':='(AGE_MEASMT_HEIGHT = ref_boring_ht,
                          age_meas_ht_adjusted = TRUE)]
    age_adjusted_trees <- treemeasurements[new_age_replaced == TRUE |
                                             boring_age_adjusted == TRUE |
                                             lab_age_adjusted == TRUE |
                                             age_meas_ht_adjusted == TRUE,
                                           .(SITE_IDENTIFIER, VISIT_NUMBER,
                                             PLOT_NUMBER, TREE_NUMBER,
                                             BORING_AGE, BORING_AGE_org,
                                             MICROSCOPE_AGE, MICROSCOPE_AGE_org,
                                             AGE_MEASMT_HEIGHT, AGE_MEASMT_HEIGHT_org,
                                             NEW_AGE, new_age_replaced,
                                             boring_age_adjusted, lab_age_adjusted,
                                             age_meas_ht_adjusted,
                                             MEAS_DT,
                                             ref_year_fld,
                                             last_grow_year)]
    write.xlsx(age_adjusted_trees,
               file.path(outputPath, "age_adjusted_trees_forPSP.xlsx"),
               overwrite = TRUE)
    treemeasurements[,':='(unitree_id = NULL,
                           MEAS_DT = NULL,
                           last_grow_year = NULL,
                           new_age_replaced = NULL,
                           cutdate = NULL,
                           ref_year_lab = NULL,
                           ref_age_lab = NULL,
                           ref_year_fld = NULL,
                           ref_age_fld = NULL,
                           ref_boring_ht = NULL,
                           adjusted_lab_age = NULL,
                           boring_age_adjusted = NULL,
                           lab_age_adjusted = NULL,
                           age_meas_ht_adjusted = NULL,
                           BORING_AGE_org = NULL,
                           MICROSCOPE_AGE_org = NULL,
                           AGE_MEASMT_HEIGHT_org = NULL)]
    treemeasurements[, PLOT := PLOT_NUMBER]
  }
  sp_last <- treemeasurements[,.(SITE_IDENTIFIER, PLOT, TREE_NUMBER,
                                 VISIT_NUMBER, TREE_SPECIES_CODE)]
  sp_last <- sp_last[order(SITE_IDENTIFIER, PLOT, TREE_NUMBER, VISIT_NUMBER),]
  sp_last[, lastvisit := max(VISIT_NUMBER),
          by = c("SITE_IDENTIFIER", "PLOT", "TREE_NUMBER")]
  sp_last <- sp_last[VISIT_NUMBER == lastvisit,
                     .(SITE_IDENTIFIER, PLOT, TREE_NUMBER, sp_last = TREE_SPECIES_CODE)]
  treemeasurements <- merge(treemeasurements,
                            sp_last,
                            by = c("SITE_IDENTIFIER", "PLOT", "TREE_NUMBER"),
                            all.x = TRUE)
  treemeasurements[TREE_SPECIES_CODE != sp_last,
                   TREE_SPECIES_CODE := sp_last]
  rm(sp_last)
  # specieslookup <- lookup_species()
  # specieslookup <- unique(specieslookup[,.(SPECIES, SP0)],
  #                         by = "SPECIES")
  # setnames(specieslookup, "SPECIES", "TREE_SPECIES_CODE")
  # treemeasurements <- merge(treemeasurements, specieslookup,
  #                           by = "TREE_SPECIES_CODE",
  #                           all.x = TRUE)
  treemeasurements[TREE_SPECIES_CODE %in% c("XH", "Z", "ZH"),
                   TREE_SPECIES_CODE := "X"]
  vi_c <- treemeasurements[DIAMETER_MEASMT_HEIGHT == 1.3 &
                             DIAMETER >= 4 &
                             !is.na(LENGTH) &
                             OUT_OF_PLOT_IND == "N" &
                             (MEASUREMENT_ANOMALY_CODE %in% c(NA, "M", "D", "F", "H", "N") |
                                grepl("-SizeMOD", MEASUREMENT_ANOMALY_CODE)), ## non tally tree, can not used for volume, see scott's comments
                           .(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, TYPE_CD, PLOT,
                             TREE_NO = TREE_NUMBER, SPECIES = TREE_SPECIES_CODE,
                             DBH = DIAMETER, BROKEN_TOP_IND, DIAM_BTP = BROKEN_TOP_DIAMETER,
                             HEIGHT_TO_BREAK, TREE_PLANTED_IND,
                             CR_CL = CROWN_CLASS_CODE, TREE_LEN = LENGTH,
                             HT_PROJ = PROJECTED_HEIGHT,
                             HT_BRCH = HEIGHT_TO_LIVE_CROWN,
                             S_F = TREE_STANCE_CODE,
                             LV_D = TREE_EXTANT_CODE, WALKTHRU_STATUS = CMI_WALKTHROUGH_CODE,
                             BARK_PER = REMAINING_BARK_PERCENT,
                             SECTOR = TAGGING_SECTOR_NUMBER,
                             RESIDUAL = RESIDUAL_IND, WL_USE = WILDLIFE_USE_CODE,
                             WL_APPEA = TREE_APPEARANCE_CODE,
                             WL_CROWN = CROWN_CONDITION_CODE,
                             WL_BARK = BARK_RETENTION_CODE,
                             WL_WOOD = WOOD_CONDITION_CODE,
                             WL_LICHE = LICHEN_LOADING_RATING_CODE,
                             MEASUREMENT_ANOMALY_CODE,
                             TREE_CLASS_CODE)]
  # only nonPSP has log information
  if(compilationType == "nonPSP"){
    treelog <- readRDS(dir(inputPath, "TreeLogAssessments.rds",
                           full.names = TRUE)) %>%
      data.table
    treelog <- merge(treelog,
                     unique(vi_a[,.(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID)],
                            by = c("SITE_IDENTIFIER", "VISIT_NUMBER")),
                     by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                     all.x = TRUE)
    treelog[PLOT_CATEGORY_CODE == "IPC TD", PLOT_CATEGORY_CODE := "I"]
    treelog[PLOT_CATEGORY_CODE != "IPC TD",
            PLOT_CATEGORY_CODE := gsub("AUX ", "", PLOT_CATEGORY_CODE)]

    treelog <- treelog[,.(CLSTR_ID, PLOT = PLOT_CATEGORY_CODE, TREE_NO = TREE_NUMBER,
                          LOG_NUMBER, LOG_LEN = LOG_LENGTH,
                          LOG_GRD = LOG_GRADE_CODE,
                          LOG_SND = PERCENT_SOUND)]
    treelog[, NO_LOGS := length(LOG_NUMBER),
            by = c("CLSTR_ID", "PLOT", "TREE_NO")]
    treelog[, LOG_GRD := substr(LOG_GRD, 1, 1)]

    maxlength <- max(treelog$LOG_NUMBER)

    treelog <- reshape(treelog,
                       v.names = c("LOG_LEN", "LOG_SND", "LOG_GRD"),
                       timevar = "LOG_NUMBER",
                       idvar = c("CLSTR_ID", "PLOT", "TREE_NO", "NO_LOGS"),
                       direction = "wide")
    setnames(treelog,
             c(paste("LOG_LEN.", 1:maxlength, sep = ""),
               paste("LOG_GRD.", 1:maxlength, sep = ""),
               paste("LOG_SND.", 1:maxlength, sep = "")),
             c(paste("LOG_L_", 1:maxlength, sep = ""),
               paste("LOG_G_", 1:maxlength, sep = ""),
               paste("LOG_S_", 1:maxlength, sep = "")))

    treelog <- treelog[, c("CLSTR_ID", "PLOT", "TREE_NO", "NO_LOGS",
                           paste("LOG_L_", 1:maxlength, sep = ""),
                           paste("LOG_G_", 1:maxlength, sep = ""),
                           paste("LOG_S_", 1:maxlength, sep = "")),
                       with = FALSE]
    rm(maxlength)
    vi_c <- merge(vi_c, treelog,
                  by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                  all.x = TRUE)
    vi_c[BROKEN_TOP_IND == "Y" & !is.na(NO_LOGS),
         NO_LOGS := NO_LOGS+1L]
    brknologs <- sort(unique(vi_c[BROKEN_TOP_IND == "Y" & !is.na(NO_LOGS),]$NO_LOGS))
    for(i in brknologs){
      vi_c[BROKEN_TOP_IND == "Y" & NO_LOGS == i,
           paste("LOG", i, "_GRD", sep = "") := "N"]
      vi_c[BROKEN_TOP_IND == "Y" & NO_LOGS == i,
           paste("LOG", i, "_LEN", sep = "") := NA]
      vi_c[BROKEN_TOP_IND == "Y" & NO_LOGS == i,
           paste("LOG", i, "_SND", sep = "") := 0]
    }
    rm(brknologs, i, treelog)

    vi_c[is.na(NO_LOGS), needchangerows := TRUE]
    vi_c[needchangerows == TRUE, ':='(NO_LOGS = 1,
                                      LOG_G_1 = "*", # flag for h enhanced trees
                                      LOG_L_1 = TREE_LEN,
                                      LOG_S_1 = 100)]
    vi_c[needchangerows == TRUE & BROKEN_TOP_IND == "Y",
         ':='(LOG_G_2 = "N",
              LOG_L_2 = 0,
              LOG_S_2 = 0,
              NO_LOGS = 2)]
    vi_c[, ':='(needchangerows = NULL)]
  }
  saveRDS(vi_c, file.path(outputPath, "vi_c.rds"))


  treemeasurements[, AGE_METH := as.character(NA)]
  treemeasurements[RANDOM_TREE_IND == "Y", AGE_METH:= "RANDOM"]
  treemeasurements[LEADING_SPECIES_TREE_IND == "Y", AGE_METH:= "LEADSP"]
  treemeasurements[SECOND_SPECIES_TREE_IND == "Y", AGE_METH:= "SECNDSP"]
  treemeasurements[TOP_HEIGHT_TREE_IND == "Y", AGE_METH:= "TOP92"]
  treemeasurements[RESIDUAL_IND == "Y", AGE_METH:= "VETERAN"]
  treemeasurements[EXTRA_TREE_IND == "Y", AGE_METH:= "OUTPTSUB"]
  treemeasurements[OTHER_TREE_IND == "Y", AGE_METH:= "INPTSUB"]
  ## as explained by Scott and Rene, we need to adjust field age and lab age
  ## based on missed year from pith
  ## see email on August 25, 2021
  treemeasurements[is.na(AGE_CORE_MISSED_YEARS_FIELD),
                   AGE_CORE_MISSED_YEARS_FIELD := 0]
  treemeasurements[is.na(AGE_CORE_MISSED_YEARS_LAB),
                   AGE_CORE_MISSED_YEARS_LAB := 0]
  vi_h <- treemeasurements[!is.na(AGE_MEASMT_HEIGHT) | !is.na(AGE_MEASURE_CODE),
                           .(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, TYPE_CD, PLOT,
                             TREE_NO = TREE_NUMBER,  SPECIES = TREE_SPECIES_CODE,
                             CR_CL = CROWN_CLASS_CODE, PRO_RING = PRORATE_RING_COUNT,
                             BORAG_FL = BORING_AGE + AGE_CORE_MISSED_YEARS_FIELD,
                             BORE_AGE = MICROSCOPE_AGE + AGE_CORE_MISSED_YEARS_LAB,
                             AGE_CORR = AGE_CORRECTION,  TOTAL_AG = TOTAL_AGE,
                             PHYS_AGE = as.numeric(NA), GROW_20YR = RADIAL_INCREMENT_LAST_20_YR,
                             GROW_10YR = RADIAL_INCREMENT_LAST_10_YR,
                             GROW_5YR = RADIAL_INCREMENT_LAST_5_YR,
                             PRO_LEN = PRORATE_LENGTH, TREE_LEN = LENGTH,
                             BNG_DIAM = DIAMETER_AT_BORING_HEIGHT, BARK_THK = BARK_THICKNESS,
                             SUIT_TR = SUITABLE_FOR_AGE_IND, BORED_HT = AGE_MEASMT_HEIGHT,
                             SUIT_HT = SUITABLE_FOR_HEIGHT_IND,
                             SUIT_SI = SUITABLE_FOR_SITE_INDEX_IND,
                             PLOT_TYP = NA, # not sure
                             BARK_THKX = NA, # not sure
                             MEAS_COD = AGE_MEASURE_CODE,
                             RANDOM_TREE_IND, RESIDUAL_IND)]

  vi_h_th <- treemeasurements[!is.na(AGE_MEASMT_HEIGHT) | !is.na(AGE_MEASURE_CODE),
                              .(CLSTR_ID, PLOT,
                                TREE_NO = TREE_NUMBER,
                                TOP_HEIGHT_TREE_IND, LEADING_SPECIES_TREE_IND,
                                SECOND_SPECIES_TREE_IND,
                                EXTRA_TREE_IND, OTHER_TREE_IND,
                                OUT_OF_PLOT_IND)]
  vi_h_th[EXTRA_TREE_IND == "N" &
            OUT_OF_PLOT_IND == "Y" &
            OTHER_TREE_IND == "N",
          EXTRA_TREE_IND := "Y"]
  vi_h_th[, OUT_OF_PLOT_IND := NULL]
  vi_h_th <- melt(data = vi_h_th,
                  id.vars = c("CLSTR_ID", "PLOT", "TREE_NO"),
                  measure.vars = c("TOP_HEIGHT_TREE_IND", "LEADING_SPECIES_TREE_IND",
                                   "SECOND_SPECIES_TREE_IND",
                                   "EXTRA_TREE_IND", "OTHER_TREE_IND"))
  vi_h_th <- vi_h_th[value == "Y",]
  vi_h_th[, th_tree_tmp := substr(variable, 1, 1)]
  vi_h_th[variable == "EXTRA_TREE_IND", th_tree_tmp := "X"]
  vi_h_th[variable == "OTHER_TREE_IND", th_tree_tmp := "O"]
  vi_h_th <- unique(vi_h_th)

  vi_h_th <- vi_h_th[order(CLSTR_ID, PLOT, TREE_NO, th_tree_tmp),]
  vi_h_th[, nobs := length(th_tree_tmp),
          by = c("CLSTR_ID", "PLOT", "TREE_NO")]


  vi_h_th[nobs == 1, TH_TREE := th_tree_tmp]
  vi_h_th[nobs > 1, TH_TREE_tmp1 := paste0(th_tree_tmp, collapse = "_"),
          by = c("CLSTR_ID", "PLOT", "TREE_NO")]
  vi_h_th[TH_TREE_tmp1 %in% c("L_T", "S_T", "O_T", "N_T",
                              "T_V", "L_T_V", "O_T_V",
                              "S_T_V"), TH_TREE := "T"]
  vi_h_th[TH_TREE_tmp1 %in% c("T_X", "V_X"), TH_TREE := "X"]
  vi_h_th[TH_TREE_tmp1 %in% c("L_N"), TH_TREE := "N"]
  vi_h_th[TH_TREE_tmp1 %in% c("L_V"), TH_TREE := "L"]
  vi_h_th[TH_TREE_tmp1 %in% c("O_V"), TH_TREE := "O"]
  vi_h_th[TH_TREE_tmp1 %in% c("S_V"), TH_TREE := "S"]

  vi_h_th <- unique(vi_h_th[,.(CLSTR_ID, PLOT, TREE_NO, TH_TREE)],
                    by = c("CLSTR_ID", "PLOT", "TREE_NO"))
  vi_h_th[PLOT == "I" & TH_TREE == "T", TP_TREE := "T"]
  vi_h <- merge(vi_h, vi_h_th,
                by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                all.x = TRUE)
  vi_h[RANDOM_TREE_IND == "Y", RA_TREE := "R"]
  vi_h[, RANDOM_TREE_IND := NULL]
  vi_h[is.na(TH_TREE) & RESIDUAL_IND == "N" & is.na(RA_TREE),
       TH_TREE := "N"]
  vi_h[, RESIDUAL_IND := NULL]

  # for nonPSP
  if(compilationType == "nonPSP"){
    vi_h[, site_id := substr(CLSTR_ID, 1, 7)]
    vi_h_add <- vi_h[!is.na(BORED_HT),
                     .(BORED_HT_pre = unique(BORED_HT)),
                     by = c("site_id", "PLOT", "TREE_NO")]
    vi_h_add <- unique(vi_h_add, by = c("site_id", "PLOT", "TREE_NO"))
    vi_h <- merge(vi_h, vi_h_add,
                  by = c("site_id", "PLOT", "TREE_NO"),
                  all.x = TRUE)
    vi_h[is.na(BORED_HT) &
           !is.na(BORED_HT_pre),
         BORED_HT := BORED_HT_pre]
    vi_h[, ':='(site_id = NULL,
                BORED_HT_pre = NULL)]
    vi_h[is.na(BORED_HT), BORED_HT := 1.3] # temporary fix for the bored trees with bored height
    vi_h <- merge(vi_h,
                  unique(vi_a[,.(CLSTR_ID, MEAS_DT)],
                         by = "CLSTR_ID"),
                  by = "CLSTR_ID",
                  all.x = TRUE)
    agecorrecttable <- siteAgeCorrection(vih = vi_h)
    agecorrecttable <- agecorrecttable$age_corrected
    agecorrecttable[BORE_AGE_crt < 0,
                    BORE_AGE_crt := BORE_AGE_org]
    vi_h <- merge(vi_h,
                  agecorrecttable,
                  by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                  all.x = TRUE)
    vi_h[!is.na(BORE_AGE_crt),
         BORE_AGE := BORE_AGE_crt]
    vi_h[!is.na(BORAG_FL_crt),
         BORAG_FL := BORAG_FL_crt]
    vi_h[!is.na(TOTAL_AG_crt),
         TOTAL_AG := TOTAL_AG_crt]
    set(vi_h, , c("MEAS_DT", "refer_year",
                  "BORE_AGE_org", "BORE_AGE_crt", "BORE_AGE_dif", "BORAG_FL_org",
                  "BORAG_FL_crt", "BORAG_FL_dif", "TOTAL_AG_org", "TOTAL_AG_crt",
                  "TOTAL_AG_dif"),
        NULL)
  }
  saveRDS(vi_h, file.path(outputPath, "vi_h.rds"))
  rm(vi_h, vi_h_th)
  gc()
  if(compilationType == "nonPSP"){
    vi_i <- treemeasurements[DIAMETER_MEASMT_HEIGHT == 1.3 & is.na(LENGTH),
                             .(CLSTR_ID, PLOT, TREE_NO = TREE_NUMBER,
                               SPECIES = TREE_SPECIES_CODE,
                               DBH = DIAMETER,
                               LV_D = TREE_EXTANT_CODE,
                               S_F = TREE_STANCE_CODE,
                               MEASUREMENT_ANOMALY_CODE,
                               TREE_CLASS_CODE)]
  } else {
    vi_i <- treemeasurements[!is.na(DIAMETER) & is.na(LENGTH),
                             .(CLSTR_ID, PLOT,
                               TREE_NO = TREE_NUMBER,
                               SPECIES = TREE_SPECIES_CODE,
                               DBH = DIAMETER,
                               LV_D = TREE_EXTANT_CODE,
                               S_F = TREE_STANCE_CODE,
                               BROKEN_TOP_IND,
                               DIAM_BTP = BROKEN_TOP_DIAMETER,
                               HEIGHT_TO_BREAK,
                               HT_PROJ = PROJECTED_HEIGHT,
                               MEASUREMENT_ANOMALY_CODE,
                               TREE_CLASS_CODE)]
  }
  saveRDS(vi_i, file.path(outputPath, "vi_i.rds"))
  rm(vi_i)
  treeloss <- readRDS(dir(inputPath, "TreeLossIndicators.rds",
                          full.names = TRUE)) %>%
    data.table
  treeloss <- treeloss[SITE_IDENTIFIER %in% vi_a$SITE_IDENTIFIER,]
  if(compilationType == "nonPSP"){
    treeloss[PLOT_CATEGORY_CODE == "IPC TD", PLOT := "I"]
    treeloss[PLOT_CATEGORY_CODE != "IPC TD",
             PLOT := gsub("AUX ", "", PLOT_CATEGORY_CODE)]
  } else {
    treeloss[, PLOT := PLOT_NUMBER]
  }
  treeloss <- treeloss[order(SITE_IDENTIFIER, VISIT_NUMBER,
                             PLOT_CATEGORY_CODE, TREE_NUMBER, LOCATION_FROM),
                       .(SITE_IDENTIFIER, VISIT_NUMBER, PLOT, TREE_NUMBER,
                         LOSS_IN = TREE_LOSS_INDICATOR_CODE, # not sure
                         LOC_FROM = LOCATION_FROM,
                         LOC_TO = LOCATION_TO,
                         FREQ = FREQUENCY,
                         T_SIGN = NA,
                         F_SIGN = NA)]
  treeloss[, newseq := 1:length(LOC_FROM),
           by = c("SITE_IDENTIFIER", "VISIT_NUMBER",
                  "PLOT", "TREE_NUMBER")]
  newseqmax <- max(treeloss$newseq)
  treeloss[T_SIGN == "A", T_SIGN := "+"]
  treeloss[T_SIGN == "B", T_SIGN := "-"]
  treeloss[F_SIGN == "A", F_SIGN := "+"]
  treeloss[F_SIGN == "B", F_SIGN := "-"]
  treeloss[, maxseq := max(newseq),
           by = c("SITE_IDENTIFIER", "VISIT_NUMBER",
                  "PLOT", "TREE_NUMBER")]
  treeloss1 <- treeloss[maxseq == 1,
                        .(SITE_IDENTIFIER, VISIT_NUMBER,
                          PLOT, TREE_NUMBER,
                          LOSS_IN.1 = LOSS_IN,
                          LOC_FROM.1 = LOC_FROM,
                          LOC_TO.1 = LOC_TO,
                          FREQ.1 = FREQ,
                          T_SIGN.1 = T_SIGN,
                          F_SIGN.1 = F_SIGN)]

  treeloss2 <- treeloss[maxseq > 1,]
  treeloss2 <- reshape(data = treeloss2,
                       v.names = c("LOSS_IN", "LOC_FROM", "LOC_TO", "FREQ",
                                   "T_SIGN", "F_SIGN"),
                       timevar = "newseq",
                       idvar = c("SITE_IDENTIFIER", "VISIT_NUMBER",
                                 "PLOT", "TREE_NUMBER"),
                       direction = "wide")
  treeloss <- rbindlist(list(treeloss1, treeloss2),
                        fill = TRUE)
  setnames(treeloss,
           c(paste("LOSS_IN.", 1:newseqmax, sep = ""),
             paste("LOC_FROM.", 1:newseqmax, sep = ""),
             paste("LOC_TO.", 1:newseqmax, sep = ""),
             paste("T_SIGN.", 1:newseqmax, sep = ""),
             paste("F_SIGN.", 1:newseqmax, sep = ""),
             paste("FREQ.", 1:newseqmax, sep = "")),
           c(paste("LOSS", 1:newseqmax, "_IN", sep = ""),
             paste("LOC", 1:newseqmax, "_FRO", sep = ""),
             paste("LOC", 1:newseqmax, "_TO", sep = ""),
             paste("T_SIGN", 1:newseqmax, sep = ""),
             paste("F_SIGN", 1:newseqmax, sep = ""),
             paste("FREQ", 1:newseqmax, sep = "")))
  rm(newseqmax, treeloss1, treeloss2)
  gc()
  treedamage <- readRDS(dir(inputPath, "TreeDamageOccurrences.rds",
                            full.names = TRUE)) %>%
    data.table
  if(compilationType == "nonPSP"){
    treedamage[PLOT_CATEGORY_CODE == "IPC TD", PLOT := "I"]
    treedamage[PLOT_CATEGORY_CODE != "IPC TD",
               PLOT := gsub("AUX ", "", PLOT_CATEGORY_CODE)]
  } else {
    treedamage[, PLOT := PLOT_NUMBER]
  }


  treedamage <- treedamage[order(SITE_IDENTIFIER, VISIT_NUMBER,
                                 PLOT, TREE_NUMBER),
                           .(SITE_IDENTIFIER, VISIT_NUMBER, PLOT, TREE_NUMBER,
                             DAMG_NEW = DAMAGE_AGENT_CODE,
                             DAMG_OLD  = NA, # not sure
                             SEVERITY = SEVERITY_RATING_VALUE)]

  treedamage[, neworder := 1:length(DAMG_OLD),
             by = c("SITE_IDENTIFIER", "VISIT_NUMBER", "PLOT", "TREE_NUMBER")]
  maxneworder <- max(treedamage$neworder)
  treedamage <- reshape(data = treedamage,
                        v.names = c("DAMG_NEW", "DAMG_OLD", "SEVERITY"),
                        timevar = "neworder",
                        idvar = c("SITE_IDENTIFIER", "VISIT_NUMBER",
                                  "PLOT", "TREE_NUMBER"),
                        direction = "wide")
  setnames(treedamage,
           c(paste("DAMG_NEW.", 1:maxneworder, sep = ""),
             paste("DAMG_OLD.", 1:maxneworder, sep = ""),
             paste("SEVERITY.", 1:maxneworder, sep = "")),
           c(paste("DAM_AGN", LETTERS[1:maxneworder], sep = ""),
             paste("OLD_AGN", LETTERS[1:maxneworder], sep = ""),
             paste("SEV_", LETTERS[1:maxneworder], sep = "")))
  vi_d <- merge(treeloss, treedamage,
                by = c("SITE_IDENTIFIER", "VISIT_NUMBER",
                       "PLOT", "TREE_NUMBER"),
                all = TRUE)
  rm(treeloss, treedamage, maxneworder)
  vi_d <- merge(vi_d[, ind := TRUE],
                treemeasurements[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, TYPE_CD, PLOT,
                                    TREE_NUMBER, SPECIES = TREE_SPECIES_CODE,
                                    DISTANCE = STEM_MAP_DISTANCE, AZIMUTH = STEM_MAP_BEARING,
                                    OUT_OF_PLOT_IND, MEASUREMENT_ANOMALY_CODE)],
                by = c("SITE_IDENTIFIER", "VISIT_NUMBER", "PLOT", "TREE_NUMBER"),
                all = TRUE)
  vi_d <- vi_d[!is.na(CLSTR_ID),]
  vi_d <- vi_d[OUT_OF_PLOT_IND == "N",]
  vi_d <- vi_d[MEASUREMENT_ANOMALY_CODE %in% c(NA, "M", "D", "F", "H", "N"),]
  vi_d[, ':='(OUT_OF_PLOT_IND = NULL,
              MEASUREMENT_ANOMALY_CODE = NULL)]
  vi_d[!is.na(DISTANCE) & !is.na(AZIMUTH), STEM := TRUE]
  vi_d <- vi_d[ind == TRUE | STEM == TRUE,]
  vi_d[, ':='(ind = NULL)]
  setnames(vi_d, "TREE_NUMBER", "TREE_NO")
  saveRDS(vi_d, file.path(outputPath, "vi_d.rds"))
  rm(vi_d)
  gc()

  SmallLiveTreeTallies <- readRDS(dir(inputPath,
                                      pattern =  "SmallLiveTreeTallies.rds",
                                      full.names = TRUE))
  SmallLiveTreeTallies <- merge(SmallLiveTreeTallies,
                                unique(vi_a[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER)],
                                       by = c("SITE_IDENTIFIER", "VISIT_NUMBER")),
                                by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                                all.x = TRUE)
  SmallLiveTreeTallies <- SmallLiveTreeTallies[!is.na(CLSTR_ID),]
  SmallLiveTreeTallies[, low_bnd := as.numeric(SMALL_TREE_TALLY_CLASS_CODE)]
  vi_f <- SmallLiveTreeTallies[,.(CLSTR_ID, PLOT = "I", TREE_SPECIES_CODE,
                                  low_bnd, TOTAL = NUMBER_OF_TREES)]
  vi_f <- vi_f[,.(TOTAL = sum(TOTAL)),
               by = c("CLSTR_ID", "PLOT", "TREE_SPECIES_CODE", "low_bnd")]

  vi_f <- reshape(data = vi_f,
                  v.names = "TOTAL",
                  timevar = "low_bnd",
                  idvar = c("CLSTR_ID", "PLOT", "TREE_SPECIES_CODE"),
                  direction = "wide",
                  sep = "")
  totalnames <- names(vi_f)
  totalnames <- totalnames[substr(totalnames, 1, 5) == "TOTAL"]
  for (indiname in totalnames) {
    setnames(vi_f, indiname, "tempname")
    vi_f[is.na(tempname), tempname := 0]
    setnames(vi_f, "tempname", indiname)
  }
  # vi_f <- merge(vi_f, specieslookup,
  #               by = "TREE_SPECIES_CODE", all.x = TRUE)
  setnames(vi_f, "TREE_SPECIES_CODE", "SPECIES")
  saveRDS(vi_f, file.path(outputPath, "vi_f.rds"))


  stumptallies <- readRDS(dir(inputPath, pattern = "StumpTallies.rds",
                              full.names = TRUE))
  stumptallies <- merge(stumptallies,
                        unique(vi_a[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER)],
                               by = c("SITE_IDENTIFIER", "VISIT_NUMBER")),
                        by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                        all.x = TRUE)
  stumptallies <- stumptallies[!is.na(CLSTR_ID),]
  vi_g <- stumptallies[,.(CLSTR_ID, PLOT = "I", TREE_SPECIES_CODE,
                          FREQ = STUMP_OCCURRENCE_FREQUENCY, DIB = DIAMETER_INSIDE_BARK,
                          HEIGHT = STUMP_HEIGHT, PCT_SND = SOUND_WOOD_PERCENT,
                          WL_USE = WILDLIFE_USE_CODE, WL_WOOD = WOOD_CONDITION_CODE,
                          BARK_RET = BARK_RETENTION_CODE, ROOT_ROT = DAMAGE_AGENT_CODE)]
  # vi_g <- merge(vi_g, specieslookup,
  #               by = "TREE_SPECIES_CODE", all.x = TRUE)
  setnames(vi_g, "TREE_SPECIES_CODE", "SPECIES")
  saveRDS(vi_g, file.path(outputPath, "vi_g.rds"))
}
