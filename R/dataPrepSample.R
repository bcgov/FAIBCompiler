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
#' @importFrom data.table ':=' data.table melt shift
#' @importFrom dplyr '%>%'
#' @importFrom parallel detectCores makeCluster clusterExport parLapply stopCluster
#' @export
#' @docType methods
#' @rdname dataPrepSample
#' @author Yong Luo
dataPrepSample <- function(compilationType,
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
                                PSP_TYPE = PSP_SAMPLE_SITE_TYPE_CODE,
                                BGC_SS_GRD = SITE_SERIES)]
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
  siteaccessnotes <- readRDS(dir(inputPath, pattern = "AccessNotes.rds",
                                 full.names = TRUE)) %>%
    data.table
  siteaccessnotes <- siteaccessnotes[order(SITE_IDENTIFIER, ODOMETER_READING),]
  siteaccessnotes[, changepointaction := paste0("[@", ODOMETER_READING, "km] ",
                                                REMARKS)]
  siteaccessnotes <- siteaccessnotes[,.(ACCESS_NOTES = paste(changepointaction, collapse = ", ")),
                                     by = "SITE_IDENTIFIER"]
  saveRDS(siteaccessnotes,
          file.path(outputPath, "siteaccessnotes.rds"))

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
    suit_si_from_notes <- suit_si_from_notes[!is.na(TREE_NUMBER),
                                             .(SITE_IDENTIFIER, VISIT_NUMBER,
                                               TREE_NUMBER, SUIT_SI_temp)]
    saveRDS(suit_si_from_notes,
            file.path(outputPath, "suit_si_from_notes.rds"))
  }
  vi_a <- unique(SampleSiteVisits[,.(CLSTR_ID = NA,
                                     SITE_IDENTIFIER, PROJ_ID = PROJECT_NAME,
                                     PROJECT_DESCRIPTOR,
                                     STAND_DISTURBANCE_CODE, STAND_STRUCTURE_CODE,
                                     SAMPLE_BREAK_POINT, SAMPLE_BREAK_POINT_TYPE,
                                     STEM_MAP_REQD_IND,
                                     SELECTIVELY_LOGGED_IND,
                                     SAMP_NO = PROJECT_NUMBER,
                                     TYPE_CD = SAMPLE_SITE_PURPOSE_TYPE_CODE,
                                     VISIT_NUMBER,
                                     SAMPLE_SITE_PURPOSE_TYPE_DESCRIPTION,
                                     MEAS_DT = substr(newMD, 1, 10),
                                     IP_AZ_PN = NA,
                                     IP_DT_PN = NA, IP_AZ_GP = NA,
                                     IP_DT_GP = NA, IP_GPSID = NA)],
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
  if(compilationType == "PSP"){
    vi_a[is.na(DBH_TAGGING_LIMIT),
         DBH_TAGGING_LIMIT := 2]
    vi_a[SAMPLE_BREAK_POINT < DBH_TAGGING_LIMIT,
         ':='(SAMPLE_BREAK_POINT = 4)]
  } else {
    vi_a[, DBH_TAGGING_LIMIT := 4]
    vi_a[, SAMPLE_BREAK_POINT := 9] # trees between 4 and 8.9 are in subplot
  }

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
                                                  "AUX N", "AUX E") &
                          PLOT_NUMBER != 0, # 0 plot_number may be a mistake, remove it
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


}
