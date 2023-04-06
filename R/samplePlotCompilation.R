#' Compile sample and plot level information
#'
#'
#' @description This function is to compile sample and plot information.
#'
#' @param compilationType character, either \code{PSP} or \code{nonPSP}. If it is \code{PSP}, it
#'                               is consistent with original PSP compiler, otherwise, it
#'                               is consistent with VRI compiler.
#' @param dataSourcePath character, Specifies the path that contains prepared data from raw data.
#' @param mapPath character, Specifies the path dependent maps are stored.
#' @param coeffPath character, Specifies the path dependent coeffs are stored.
#' @return A data table that contains key information at cluster/plot level and compiler log file.
#'
#' @importFrom data.table ':='
#' @importFrom dplyr '%>%'
#' @importFrom FAIBBase merge_dupUpdate
#'
#' @export
#' @docType methods
#' @rdname samplePlotCompilation
#'
#' @author Yong Luo
samplePlotCompilation <- function(compilationType,
                                  dataSourcePath,
                                  mapPath,
                                  coeffPath){
  vi_a <- readRDS(file.path(dataSourcePath, "vi_a.rds"))
  vi_a <- vi_a[substr(PROJ_ID, 1, 3) != "DEV",]
  vi_a <- vi_a[substr(TYPE_CD, 1, 1) != "E", ]
  # The plots belong to LGMW project, which samples each polygon (a unit of sample)
  # that has one or more plots, however, the plot identity is not unique
  # remove these plot from further compilation
  vi_a <- vi_a[substr(TYPE_CD, 1, 1) != "W",] # double check with Bob and Rene
  # vi_a <- vi_a[!(PROJ_ID == "CAR1" & TYPE_CD == "N"),]
  vi_a <- updateSpatial(compilationType = compilationType,
                        samplesites = vi_a,
                        mapPath = mapPath)
  if(compilationType == "PSP"){
    # populate bec and tsa based on region number and compartment
    ## the bec zone with the most sites for a given region/compartment wins
    ## based on Rene's suggestions on March 14, 2023
    spatialAvailable <- unique(vi_a[!is.na(BEC),.(SITE_IDENTIFIER, BEC, BEC_SBZ, BEC_VAR,
                                           TSA, TSA_DESC, SAMPLING_REGION_NUMBER, COMPARTMENT_NUMBER)],
                               by = "SITE_IDENTIFIER")
    bec_avai <- spatialAvailable[, .(No_samples = length(SITE_IDENTIFIER)),
                     by = c("SAMPLING_REGION_NUMBER", "COMPARTMENT_NUMBER",
                            "BEC", "BEC_SBZ", "BEC_VAR")]
    bec_avai <- bec_avai[order(SAMPLING_REGION_NUMBER, COMPARTMENT_NUMBER, -No_samples),
                         .(SAMPLING_REGION_NUMBER, COMPARTMENT_NUMBER,
                           BEC_new = BEC,
                           BEC_SBZ_new = BEC_SBZ,
                           BEC_VAR_new = BEC_VAR)]
    bec_avai <- unique(bec_avai,
                       by = c("SAMPLING_REGION_NUMBER", "COMPARTMENT_NUMBER"))
    vi_a <- merge(vi_a,
                  bec_avai,
                  by = c("SAMPLING_REGION_NUMBER", "COMPARTMENT_NUMBER"),
                  all.x = TRUE)
    vi_a[is.na(BEC),
         ':='(BEC = BEC_new,
              BEC_SBZ = BEC_SBZ_new,
              BEC_VAR = BEC_VAR_new)]
    vi_a[is.na(BEC)]
    tsa_avai <- spatialAvailable[, .(No_samples = length(SITE_IDENTIFIER)),
                     by = c("SAMPLING_REGION_NUMBER", "COMPARTMENT_NUMBER",
                            "TSA", "TSA_DESC")]
    tsa_avai <- tsa_avai[order(SAMPLING_REGION_NUMBER, COMPARTMENT_NUMBER, -No_samples),
                         .(SAMPLING_REGION_NUMBER, COMPARTMENT_NUMBER,
                           TSA_new = TSA,
                           TSA_DESC_new = TSA_DESC)]
    tsa_avai <- unique(tsa_avai,
                       by = c("SAMPLING_REGION_NUMBER", "COMPARTMENT_NUMBER"))
    vi_a <- merge(vi_a,
                  tsa_avai,
                  by = c("SAMPLING_REGION_NUMBER", "COMPARTMENT_NUMBER"),
                  all.x = TRUE)
    vi_a[is.na(TSA),
         ':='(TSA = TSA_new,
              TSA_DESC = TSA_DESC_new)]
    vi_a[is.na(TSA)]
    #
    # previousSamples <- readRDS(file.path(mapPath, "spatiallookup_PSP.rds"))
    # previousSamples <- previousSamples$spatiallookup
    # names(previousSamples) <- paste0(names(previousSamples), "_prev")
    # setnames(previousSamples, "SITE_IDENTIFIER_prev", "SITE_IDENTIFIER")
    # samplesites_Loc <- unique(vi_a[,
    #                                .(SITE_IDENTIFIER,
    #                                  IP_UTM, IP_NRTH, IP_EAST)],
    #                           by = "SITE_IDENTIFIER")
    #
    # allsamples <- merge(previousSamples[, inprev := TRUE],
    #                     samplesites_Loc[, incurt := TRUE],
    #                     by = "SITE_IDENTIFIER",
    #                     all = TRUE)
    # allsamples[, unid := 1:nrow(allsamples)]
    #
    # samples_skip <- allsamples[(inprev == TRUE & incurt == TRUE) &
    #                              (IP_UTM_prev == IP_UTM |
    #                                 (is.na(IP_UTM_prev) & is.na(IP_UTM))) &
    #                              (IP_EAST_prev == IP_EAST |
    #                                 (is.na(IP_EAST_prev) & is.na(IP_EAST))) &
    #                              (IP_NRTH_prev == IP_NRTH |
    #                                 (is.na(IP_NRTH_prev) & is.na(IP_NRTH)))]
    samples_skip <- vi_a[!is.na(BEC),]

    samples_proc <- vi_a[!(SITE_IDENTIFIER %in% samples_skip$SITE_IDENTIFIER),]
    if(nrow(samples_proc) > 0){
      ## for PSP, some samples do not have good spatial coordinates, hence, causing
      ## missing spatial attributes
      samples_proc <- updateMissingSpAttribute(spatialtable = samples_proc,
                                               mapPath = mapPath,
                                               updateMethod = "fromRegionCompartMap")
    }
    vi_a <- rbindlist(list(samples_skip, samples_proc),
                      fill = TRUE)
    spatialLookups <- unique(vi_a[,.(SITE_IDENTIFIER, SAMP_POINT = SITE_IDENTIFIER,
                                     IP_UTM, IP_NRTH, IP_EAST, UTM_SOURCE, CORRDINATE_SOURCE, BC_ALBERS_X, BC_ALBERS_Y,
                                     Longitude, Latitude, BEC_ZONE = BEC, BEC_SBZ, BEC_VAR,
                                     TSA, TSA_DESC, FIZ, TFL, OWNER, SCHEDULE,
                                     PROJ_ID, SAMP_NO,
                                     SAMPLE_ESTABLISHMENT_TYPE = paste0("PSP_", PSP_TYPE), SAMPLE_SITE_NAME,
                                     SITE_STATUS_CODE, SITE_ACCESS_CODE, STAND_ORIGIN_CODE,
                                     STAND_DISTURBANCE_CODE, SEL_LGD = SELECTIVELY_LOGGED_IND)],
                             by = "SAMP_POINT")
  } else {
    spatialLookups <- unique(vi_a[,.(SITE_IDENTIFIER, SAMP_POINT = SITE_IDENTIFIER,
                                     IP_UTM, IP_NRTH, IP_EAST, UTM_SOURCE, CORRDINATE_SOURCE, BC_ALBERS_X, BC_ALBERS_Y,
                                     Longitude, Latitude, BEC_ZONE = BEC, BEC_SBZ, BEC_VAR,
                                     TSA, TSA_DESC, FIZ, TFL, OWNER, SCHEDULE,
                                     PROJ_ID, SAMP_NO,
                                     SAMPLE_SITE_NAME,
                                     SITE_STATUS_CODE, SITE_ACCESS_CODE, STAND_ORIGIN_CODE,
                                     STAND_DISTURBANCE_CODE, SEL_LGD = SELECTIVELY_LOGGED_IND)],
                             by = "SAMP_POINT")
  }
  vi_a <- vi_a[,.(CLSTR_ID,
                  SITE_IDENTIFIER,
                  VISIT_NUMBER,
                  BEC, FIZ,
                  MEAS_DT,
                  TYPE_CD,
                  SAMPLE_SITE_PURPOSE_TYPE_DESCRIPTION,
                  PROJ_ID,
                  SAMP_NO,
                  SAMPLE_BREAK_POINT,
                  SAMPLE_BREAK_POINT_TYPE,
                  DBH_LIMIT_TAG = DBH_TAGGING_LIMIT,
                  PROJECT_DESCRIPTOR)]
  mapsource <- data.table(mapFile = dir(mapPath, pattern = "_map"))
  spatialLookups <- list(spatiallookup = spatialLookups,
                         mapsource = mapsource)
  vi_a[, PRJ_GRP := prj_ID2Grp(PROJ_ID)]
  vi_a[!(BEC %in% c("AT","BWBS","CDF","CWH","ESSF","ICH","IDF","MH",
                    "MS","PP","SBPS","SBS","SWB","BG","BAFA","CMA","IMA")),
       BEC := prj_ID2BEC(PROJ_ID)]

  vi_a[is.na(FIZ) | FIZ == " ", FIZ := "E"]

  # vi_a <- merge(vi_a,
  #               SAVegComp[,.(SITE_IDENTIFIER, PROJ_AGE_1,
  #                            PROJECTED_Year = as.numeric(substr(PROJECTED_DATE, 1, 4)))],
  #               by = "SITE_IDENTIFIER",
  #               all.x = TRUE)
  # vi_a[, measYear := as.numeric(substr(MEAS_DT, 1, 4))]
  #
  # vi_a[, SA_VEGCOMP := measYear - PROJECTED_Year + PROJ_AGE_1]
  # vi_a[, ':='(PROJ_AGE_1 = NULL,
  #             PROJECTED_Year = NULL,
  #             measYear = NULL)]
  vi_b <- readRDS(file.path(dataSourcePath, "vi_b.rds")) %>% data.table
  vi_b <- vi_b[CLSTR_ID %in% vi_a$CLSTR_ID,]
  vi_b <- merge(vi_b, vi_a[,.(CLSTR_ID, PROJ_ID)],
                by = "CLSTR_ID",
                all.x = TRUE)
  sitetopography <- vi_b[,.(SITE_IDENTIFIER, VISIT_NUMBER,
                            ELEVATION = PLOT_ELEVATION,
                            ASPECT = PLOT_ASPECT,
                            SLOPE = PLOT_SLOPE)]
  sitetopography[, lastvisit := max(VISIT_NUMBER),
                 by = SITE_IDENTIFIER]
  sitetopography <- unique(sitetopography[VISIT_NUMBER == lastvisit,
                                   .(SITE_IDENTIFIER, ELEVATION,
                                     ASPECT, SLOPE)],
                           by = "SITE_IDENTIFIER")
  spatialLookups$spatiallookup <- merge(spatialLookups$spatiallookup,
                                        sitetopography,
                                        by = "SITE_IDENTIFIER",
                                        all.x = TRUE)

  # remove I from N samples in CAR1 project, as these N samples do not have
  # IPC, see communications with Rene and Chris on July 29, 2022
  vi_b <- vi_b[!(PROJ_ID == "CAR1" & TYPE_CD == "N" & PLOT == "I"),]
  vi_b <- unique(vi_b, by = c("CLSTR_ID", "PLOT"))
  # for variable area plot
  vi_b[V_BAF > 0 & V_FULL == TRUE, PLOT_WT := 1]
  vi_b[V_BAF > 0 & V_HALF == TRUE, PLOT_WT := 2]
  vi_b[V_BAF > 0 & V_QRTR == TRUE, PLOT_WT := 4]
  vi_b[V_BAF > 0, ':='(SAMP_TYP = "V",
                       PLOT_AREA_MAIN = as.numeric(NA),
                       BLOWUP_MAIN = V_BAF)]
  # for fixed area plot
  vi_b[is.na(V_BAF) & F_FULL == TRUE, PLOT_WT := 1]
  vi_b[is.na(V_BAF) & F_HALF == TRUE, PLOT_WT := 2]
  vi_b[is.na(V_BAF) & F_QRTR == TRUE, PLOT_WT := 4]

  # calculate main plot area
  #for circular plot
  vi_b[V_BAF %in% c(0, NA) &
         !is.na(F_RAD),
       ':='(SAMP_TYP = "F",
            PLOT_AREA_MAIN = (pi* F_RAD^2)/10000)]
  # for rectangle plot
  vi_b[V_BAF %in% c(0, NA) &
         !is.na(PLOT_WIDTH) &
         is.na(PLOT_AREA_MAIN),
       ':='(SAMP_TYP = "F",
            PLOT_AREA_MAIN = (PLOT_WIDTH* PLOT_LENGTH)/10000)]
  # for the plot that just have plot area
  vi_b[V_BAF %in% c(0, NA) &
         !is.na(PLOT_AREA) &
         is.na(PLOT_AREA_MAIN),
       ':='(SAMP_TYP = "F",
            PLOT_AREA_MAIN = PLOT_AREA)]
  # for subplot area
  vi_b[V_BAF %in% c(0, NA) &
         !(SMALL_TREE_SUBPLOT_RADIUS %in% c(NA, 0)),
       ':='(PLOT_AREA_SUBPLOT = (pi* SMALL_TREE_SUBPLOT_RADIUS^2) / 10000)]
  vi_b[is.na(PLOT_AREA_SUBPLOT) &
         SMALL_TREE_SUBPLOT_RADIUS == 0,
       ':='(PLOT_AREA_SUBPLOT = 0)]
  vi_b[is.na(PLOT_AREA_SUBPLOT) &
         !is.na(AREA_PS),
       ':='(PLOT_AREA_SUBPLOT = AREA_PS)] # area_ps is in hactre

  # for the fixed area plot, the blowup is 1/total plot area
  vi_b[SAMP_TYP == "F",
       ':='(BLOWUP_MAIN = 1/sum(PLOT_AREA_MAIN),
            BLOWUP_SUBPLOT = 1/sum(PLOT_AREA_SUBPLOT)),
       by = "CLSTR_ID"]
  vi_b[BLOWUP_SUBPLOT %in% c(Inf, NA),
       BLOWUP_SUBPLOT := 0]
  vi_b[, NO_PLOTS := length(PLOT), by = CLSTR_ID]
  vi_b[, PLOT_DED := 1L]
  vi_b <- merge(vi_b, vi_a[,.(CLSTR_ID, MEAS_DT)],
                by = "CLSTR_ID",
                all.x = TRUE)
  vi_b[TYPE_CD == "N" | (as.numeric(substr(MEAS_DT, 1, 4)) >= 2008) |
         (as.numeric(substr(MEAS_DT, 1, 4)) == 2007 & PROJ_ID %in% c("0141", "014M", "0091")),
       PLOT_DED := NO_PLOTS]
  vi_a <- merge(vi_a,
                unique(vi_b[,.(CLSTR_ID, SAMP_TYP, NO_PLOTS, PLOT_DED)],
                       by = "CLSTR_ID"),
                by = "CLSTR_ID")
  setnames(vi_a, "BEC", "BEC_ZONE")
  if(compilationType == "nonPSP"){
    allsample_ests <- dir(coeffPath, pattern = "sample_establishment_type")
    allsample_ests <- gsub("sample_establishment_type_", "", allsample_ests)
    allsample_ests <- gsub(".xlsx", "", allsample_ests)
    allsample_est_last <- max(as.numeric(allsample_ests))
    sample_est_1 <- openxlsx::read.xlsx(file.path(coeffPath,
                                                  paste0("sample_establishment_type_", allsample_est_last, ".xlsx")),
                                        sheet = "1_non_standard_site_identifier") %>%
      data.table
    sample_est_1 <- sample_est_1[,.(SITE_IDENTIFIER, SAMPLE_ESTABLISHMENT_TYPE)]

    sample_est_2 <- openxlsx::read.xlsx(file.path(coeffPath,
                                                  paste0("sample_establishment_type_", allsample_est_last, ".xlsx")),
                                        sheet = "2_non_standard_project") %>%
      data.table
    sample_est_2 <- sample_est_2[,.(PROJECT_NAME, TYPE_CD = SAMPLE_SITE_PURPOSE_TYPE_CODE,
                                    SAMPLE_ESTABLISHMENT_TYPE2 = SAMPLE_ESTABLISHMENT_TYPE)]

    sample_est_3 <- openxlsx::read.xlsx(file.path(coeffPath,
                                                  paste0("sample_establishment_type_", allsample_est_last, ".xlsx")),
                                        sheet = "3_standard") %>%
      data.table
    sample_est_3 <- sample_est_3[,.(TYPE_CD = sample_site_purpose_type_code,
                                    SAMPLE_ESTABLISHMENT_TYPE3 = SAMPLE_ESTABLISHMENT_TYPE)]
    site_visit1 <- vi_a[TYPE_CD != "N",]
    site_visit1 <- site_visit1[!(TYPE_CD == "B" &
                                   PROJ_ID == "KOL1"),]
    ## these are test sites
    site_visit1 <- site_visit1[substr(PROJ_ID, 1, 4) != "2019",]

    site_visit1[, VISIT_NUMBER_first := min(VISIT_NUMBER),
                by = "SITE_IDENTIFIER"]
    site_visit1 <- site_visit1[VISIT_NUMBER == VISIT_NUMBER_first,]
    site_visit1 <- merge(site_visit1,
                         sample_est_1,
                         by = "SITE_IDENTIFIER",
                         all.x = TRUE)

    site_visit1[, PROJECT_NAME := PROJ_ID]
    site_visit1 <- merge(site_visit1,
                         sample_est_2,
                         by = c("PROJECT_NAME", "TYPE_CD"),
                         all.x = TRUE)
    site_visit1[is.na(SAMPLE_ESTABLISHMENT_TYPE) &
                  !is.na(SAMPLE_ESTABLISHMENT_TYPE2),
                SAMPLE_ESTABLISHMENT_TYPE := SAMPLE_ESTABLISHMENT_TYPE2]
    site_visit1[, SAMPLE_ESTABLISHMENT_TYPE2 := NULL]
    site_visit1 <- merge(site_visit1,
                         sample_est_3,
                         by = c("TYPE_CD"),
                         all.x = TRUE)
    site_visit1[is.na(SAMPLE_ESTABLISHMENT_TYPE),
                SAMPLE_ESTABLISHMENT_TYPE := SAMPLE_ESTABLISHMENT_TYPE3]
    site_visit1[, SAMPLE_ESTABLISHMENT_TYPE3 := NULL]
    site_visit1[TYPE_CD == "A",
                SAMPLE_ESTABLISHMENT_TYPE := "EYSM"]
    site_visit1[TYPE_CD == "A" & PROJECT_DESCRIPTOR == "Forest Health Early YSM",
                SAMPLE_ESTABLISHMENT_TYPE := "FHYSM"]
    site_visit1 <- site_visit1[,.(SITE_IDENTIFIER, SAMPLE_ESTABLISHMENT_TYPE)]
    site_visit1[SITE_IDENTIFIER == "2104138",
                SAMPLE_ESTABLISHMENT_TYPE := "YNS"]
    spatialLookups$spatiallookup <- merge(spatialLookups$spatiallookup,
                                          site_visit1,
                                          by = "SITE_IDENTIFIER",
                                          all.x = TRUE)
  }
  vi_a[,':='(BEC_ZONE = NULL,
             FIZ = NULL)]
  return(list(spatiallookup = spatialLookups,
              samples = vi_a,
              plots = vi_b[,.(CLSTR_ID, PLOT, PLOT_WT, PLOT_AREA_MAIN, PLOT_AREA_SUBPLOT,
                              BLOWUP_MAIN, BLOWUP_SUBPLOT,
                              PLOT_SHAPE_CODE, F_RAD,
                              PLOT_WIDTH, PLOT_LENGTH, V_BAF, SMALL_TREE_SUBPLOT_RADIUS,
                              PLOT_SLOPE,	PLOT_ASPECT, PLOT_ELEVATION)]))
}

