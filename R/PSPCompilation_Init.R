#' Initiate PSP data in ISMC for the compilation
#'
#'
#' @description This function is to organize ismc PSP data
#'              for the sequential compilation.
#'
#' @param inputPath character, Specifies the path that stores data from oracle data base.
#' @param outputPath character, Specifies the path to save your outputs. If missing, the current working
#'                   directory will be choose.
#'
#' @return no item returned
#'
#' @importFrom data.table ':=' data.table melt
#' @importFrom dplyr '%>%'
#'
#' @rdname PSPCompilation_Init
#' @author Yong Luo
PSPCompilation_Init <- function(inputPath, outputPath){
  samplesites <- readRDS(dir(inputPath, pattern = "SampleSites.rds", full.names = TRUE)) %>%
    data.table
  samplesites[, ELEVATION := NULL]
  samplesites <- samplesites[,.(SITE_IDENTIFIER, SAMPLE_SITE_NAME,
                                IP_UTM = UTM_ZONE, IP_EAST = UTM_EASTING, IP_NRTH = UTM_NORTHING,
                                IP_ELEV = ELEVATION,
                                SAMPLING_REGION_NUMBER, COMPARTMENT_NUMBER,
                                FOREST_INVENTORY_ZONE_CD)]
  samplesites[, namelength := nchar(SAMPLE_SITE_NAME)]
  samplesites[, PSP_TYPE := substr(SAMPLE_SITE_NAME, namelength, namelength)]
  samplesites[, namelength := NULL]
  samplesites <- samplesites[PSP_TYPE != "I",]
  if(nrow(samplesites) != length(unique(samplesites$SITE_IDENTIFIER))){
    warning("samplesites file: SITE_IDENTIFIER is not unique.")
  }
  SampleSiteVisits <- readRDS(dir(inputPath, pattern = "SampleSiteVisits.rds",
                                  full.names = TRUE)) %>%
    data.table
  SampleSiteVisits[, newMD := SAMPLE_SITE_VISIT_START_DATE + 60*60]

  vi_a <- SampleSiteVisits[,.(CLSTR_ID = NA,
                              SITE_IDENTIFIER, PROJ_ID = PROJECT_NAME,
                              STAND_DISTURBANCE_CODE, STAND_STRUCTURE_CODE,
                              SAMPLE_BREAK_POINT, SAMPLE_BREAK_POINT_TYPE,
                              STEM_MAP_REQD_IND,
                              SAMP_NO = PROJECT_NUMBER, TYPE_CD = SAMPLE_SITE_PURPOSE_TYPE_CODE,
                              VISIT_NUMBER,
                              MEAS_DT = substr(newMD, 1, 10),
                              IP_AZ_PN = NA,
                              IP_DT_PN = NA, IP_AZ_GP = NA, IP_DT_GP = NA, IP_GPSID = NA)]
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
  rm(SampleSiteVisits)
  gc()
  vi_a[, TYPE_CD := paste0(TYPE_CD, VISIT_NUMBER)]
  vi_a[,CLSTR_ID := paste(SITE_IDENTIFIER, TYPE_CD, sep = "-")]
  saveRDS(vi_a, file.path(outputPath, "vi_a.rds"))
  plotdetails <- readRDS(dir(inputPath, pattern = "PlotDetails.rds",
                             full.names = TRUE))
  plotdetails <- plotdetails[SITE_IDENTIFIER %in% samplesites$SITE_IDENTIFIER,]
  plotdetails <- merge(plotdetails, vi_a[,.(SITE_IDENTIFIER, VISIT_NUMBER,
                                            CLSTR_ID)],
                       by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                       all.x = TRUE)
  ## for some PSP samples, there are multiple plots, which
  ## is reflected by plot number
  vi_b <- plotdetails[PLOT_CATEGORY_CODE %in% c("IPC TD", "AUX S", "AUX W",
                                                "AUX N", "AUX E"),
                      .(CLSTR_ID, PLOT = PLOT_NUMBER,
                        V_BAF = VARIABLE_BAF, F_RAD = PLOT_RADIUS,
                        PLOT_AREA, PLOT_WIDTH, PLOT_LENGTH,
                        PARTIAL_PLOT_REASON_CODE, PLOT_SEGMENT_CODE)]
  # vi_b[PLOT == "IPC TD", PLOT := "I"]
  # vi_b[PLOT != "I", PLOT := substr(PLOT, 5, 5)]
  gc()

  vi_b_master <- unique(vi_b[,.(CLSTR_ID, PLOT, V_BAF, F_RAD,
                                PLOT_AREA, PLOT_WIDTH, PLOT_LENGTH)],
                        by = c("CLSTR_ID", "PLOT"))
  fixplots <- vi_b[!is.na(F_RAD),]
  vi_b_master <- merge(vi_b_master,
                       fixplots[PARTIAL_PLOT_REASON_CODE == "BOUNDARY",
                                .(CLSTR_ID, PLOT, F_BDRY = TRUE)],
                       all.x = TRUE)
  vi_b_master <- merge(vi_b_master,
                       fixplots[PARTIAL_PLOT_REASON_CODE == "SPLIT",
                                .(CLSTR_ID, PLOT, F_SPLT = TRUE)],
                       all.x = TRUE)
  vi_b_master <- merge(vi_b_master,
                       fixplots[PLOT_SEGMENT_CODE == "FULL",
                                .(CLSTR_ID, PLOT, F_FULL = TRUE)],
                       all.x = TRUE)
  vi_b_master <- merge(vi_b_master,
                       fixplots[PLOT_SEGMENT_CODE == "HALF",
                                .(CLSTR_ID, PLOT, F_HALF = TRUE)],
                       all.x = TRUE)
  vi_b_master <- merge(vi_b_master,
                       fixplots[PLOT_SEGMENT_CODE == "QTR",
                                .(CLSTR_ID, PLOT, F_QRTR = TRUE)],
                       all.x = TRUE)
  varplots <- vi_b[!is.na(V_BAF),]
  vi_b_master <- merge(vi_b_master,
                       varplots[PARTIAL_PLOT_REASON_CODE == "BOUNDARY",
                                .(CLSTR_ID, PLOT, V_BDRY = TRUE)],
                       all.x = TRUE)
  vi_b_master <- merge(vi_b_master,
                       varplots[PARTIAL_PLOT_REASON_CODE == "SPLIT",
                                .(CLSTR_ID, PLOT, V_SPLT = TRUE)],
                       all.x = TRUE)
  vi_b_master <- merge(vi_b_master,
                       varplots[PLOT_SEGMENT_CODE == "FULL",
                                .(CLSTR_ID, PLOT, V_FULL = TRUE)],
                       all.x = TRUE)
  vi_b_master <- merge(vi_b_master,
                       varplots[PLOT_SEGMENT_CODE == "HALF",
                                .(CLSTR_ID, PLOT, V_HALF = TRUE)],
                       all.x = TRUE)
  vi_b_master <- merge(vi_b_master,
                       varplots[PLOT_SEGMENT_CODE == "QTR",
                                .(CLSTR_ID, PLOT, V_QRTR = TRUE)],
                       all.x = TRUE)
  rm(fixplots, varplots)
  saveRDS(vi_b_master, file.path(outputPath, "vi_b.rds"))
  rm(vi_b_master)
  gc()

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
  gc()
  saveRDS(vi_e, file.path(outputPath, "vi_e.rds"))
  rm(vi_e)
  treemeasurements <- readRDS(dir(inputPath, "TreeMeasurements.rds",
                                  full.names = TRUE)) %>%
    data.table
  treemeasurements[, TREE_DETAIL_COMMENT := NULL]
  treemeasurements[,':='(BORING_AGE_org = BORING_AGE,
                         MICROSCOPE_AGE_org = MICROSCOPE_AGE,
                         AGE_MEASMT_HEIGHT_org = AGE_MEASMT_HEIGHT)]
  treemeasurements <- merge(treemeasurements,
                            unique(vi_a[,.(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, MEAS_DT)],
                                   by = c("SITE_IDENTIFIER", "VISIT_NUMBER")),
                            by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                            all.x = TRUE)

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

  a <- treemeasurements[SITE_IDENTIFIER == 4000045 & TREE_NUMBER == 10,
    .(SITE_IDENTIFIER, VISIT_NUMBER,
      PLOT_NUMBER, TREE_NUMBER,
      BORING_AGE_adjusted = BORING_AGE, BORING_AGE_org,
      MICROSCOPE_AGE_adjusted = MICROSCOPE_AGE, MICROSCOPE_AGE_org,
      AGE_MEASMT_HEIGHT_adjusted = AGE_MEASMT_HEIGHT, AGE_MEASMT_HEIGHT_org,
      MEAS_DT,
      ref_year_fld,
      ref_age_fld,
      ref_year_lab,
      ref_age_lab,
      last_grow_year)
  ]
  b <- treemeasurements[SITE_IDENTIFIER == 4000014  & TREE_NUMBER == 2017,
    .(SITE_IDENTIFIER, VISIT_NUMBER,
      PLOT_NUMBER, TREE_NUMBER,
      BORING_AGE_adjusted = BORING_AGE, BORING_AGE_org,
      MICROSCOPE_AGE_adjusted = MICROSCOPE_AGE, MICROSCOPE_AGE_org,
      AGE_MEASMT_HEIGHT_adjusted = AGE_MEASMT_HEIGHT, AGE_MEASMT_HEIGHT_org,
      MEAS_DT,
      ref_year_fld,
      ref_age_fld,
      ref_year_lab,
      ref_age_lab,
      last_grow_year)
  ]

  c <- rbind(a, b)



  write.xlsx(age_adjusted_trees,
             file.path(outputPath, "age_adjusted_trees.xlsx"),
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
## end of fix

  treemeasurements <- treemeasurements[SITE_IDENTIFIER %in% samplesites$SITE_IDENTIFIER,]



  # treemeasurements[PLOT_CATEGORY_CODE == "IPC TD", PLOT_CATEGORY_CODE := "I"]
  # treemeasurements[PLOT_CATEGORY_CODE != "I",
  #                  PLOT_CATEGORY_CODE := gsub("AUX ", "", PLOT_CATEGORY_CODE)]
  specieslookup <- lookup_species()
  specieslookup <- unique(specieslookup[,.(SPECIES, SP0)],
                          by = "SPECIES")
  setnames(specieslookup, "SPECIES", "TREE_SPECIES_CODE")
  treemeasurements <- merge(treemeasurements, specieslookup,
                            by = "TREE_SPECIES_CODE",
                            all.x = TRUE)
  treemeasurements[TREE_SPECIES_CODE %in% c("XH", "Z", "ZH"),
                   TREE_SPECIES_CODE := "X"]


  vi_c <- treemeasurements[!is.na(DIAMETER) & !is.na(LENGTH) &
                             OUT_OF_PLOT_IND == "N" &
                             MEASUREMENT_ANOMALY_CODE %in% c(NA, "M", "D", "F", "H", "N"), ## non tally tree, can not used for volume, see scott's comments
                           .(CLSTR_ID, PLOT = PLOT_NUMBER,
                             TREE_NO = TREE_NUMBER, SPECIES = TREE_SPECIES_CODE, SP0,
                             DBH = DIAMETER, BROKEN_TOP_IND, DIAM_BTP = BROKEN_TOP_DIAMETER,
                             HEIGHT_TO_BREAK,
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
                             MEASUREMENT_ANOMALY_CODE)]

  #   treelog <- readRDS(dir(inputPath, "TreeLogAssessments.rds",
  #                          full.names = TRUE)) %>%
  #     data.table
  #   treelog <- merge(treelog,
  #                    unique(vi_a[,.(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID)],
  #                           by = c("SITE_IDENTIFIER", "VISIT_NUMBER")),
  #                    by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
  #                    all.x = TRUE)
  #   treelog[PLOT_CATEGORY_CODE == "IPC TD", PLOT_CATEGORY_CODE := "I"]
  #   treelog[PLOT_CATEGORY_CODE != "I",
  #           PLOT_CATEGORY_CODE := gsub("AUX ", "", PLOT_CATEGORY_CODE)]
  #
  #   treelog <- treelog[,.(CLSTR_ID, PLOT = PLOT_CATEGORY_CODE, TREE_NO = TREE_NUMBER,
  #                         LOG_NUMBER, LOG_LEN = LOG_LENGTH,
  #                         LOG_GRD = LOG_GRADE_CODE,
  #                         LOG_SND = PERCENT_SOUND)]
  #   treelog[, NO_LOGS := length(LOG_NUMBER),
  #           by = c("CLSTR_ID", "PLOT", "TREE_NO")]
  #   treelog[, LOG_GRD := substr(LOG_GRD, 1, 1)]
  #
  #   maxlength <- max(treelog$LOG_NUMBER)
  # browser()
  #   treelog <- reshape(treelog,
  #                      v.names = c("LOG_LEN", "LOG_SND", "LOG_GRD"),
  #                      timevar = "LOG_NUMBER",
  #                      idvar = c("CLSTR_ID", "PLOT", "TREE_NO", "NO_LOGS"),
  #                      direction = "wide")
  #   setnames(treelog,
  #            c(paste("LOG_LEN.", 1:maxlength, sep = ""),
  #              paste("LOG_GRD.", 1:maxlength, sep = ""),
  #              paste("LOG_SND.", 1:maxlength, sep = "")),
  #            c(paste("LOG", 1:maxlength, "_LEN", sep = ""),
  #              paste("LOG", 1:maxlength, "_GRD", sep = ""),
  #              paste("LOG", 1:maxlength, "_SND", sep = "")))
  #
  #   treelog <- treelog[, c("CLSTR_ID", "PLOT", "TREE_NO", "NO_LOGS",
  #                          paste("LOG", 1:maxlength, "_LEN", sep = ""),
  #                          paste("LOG", 1:maxlength, "_GRD", sep = ""),
  #                          paste("LOG", 1:maxlength, "_SND", sep = "")),
  #                      with = FALSE]
  #   rm(maxlength)
  #   vi_c <- merge(vi_c, treelog,
  #                 by = c("CLSTR_ID", "PLOT", "TREE_NO"),
  #                 all.x = TRUE)
  #   vi_c[BROKEN_TOP_IND == "Y" & !is.na(NO_LOGS),
  #        NO_LOGS := NO_LOGS+1L]
  #   brknologs <- sort(unique(vi_c[BROKEN_TOP_IND == "Y" & !is.na(NO_LOGS),]$NO_LOGS))
  #   for(i in brknologs){
  #     vi_c[BROKEN_TOP_IND == "Y" & NO_LOGS == i,
  #          paste("LOG", i, "_GRD", sep = "") := "N"]
  #     vi_c[BROKEN_TOP_IND == "Y" & NO_LOGS == i,
  #          paste("LOG", i, "_LEN", sep = "") := NA]
  #     vi_c[BROKEN_TOP_IND == "Y" & NO_LOGS == i,
  #          paste("LOG", i, "_SND", sep = "") := 0]
  #   }
  #   rm(brknologs, i, treelog)
  #   vi_c[is.na(NO_LOGS), needchangerows := TRUE]
  #   vi_c[needchangerows == TRUE, ':='(NO_LOGS = 1,
  #                                     LOG1_GRD = "*", # flag for h enhanced trees
  #                                     LOG1_LEN = TREE_LEN,
  #                                     LOG1_SND = 100)]
  #   vi_c[needchangerows == TRUE & BROKEN_TOP_IND == "Y",
  #        ':='(LOG2_GRD = "N",
  #             LOG2_LEN = 0,
  #             LOG2_SND = 0,
  #             NO_LOGS = 2)]
  #   vi_c[, ':='(needchangerows = NULL)]
  saveRDS(vi_c, file.path(outputPath, "vi_c.rds"))
  rm(vi_c)
  gc()

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
  vi_h <- treemeasurements[!is.na(AGE_MEASMT_HEIGHT),
                           .(CLSTR_ID, PLOT = PLOT_NUMBER,
                             TREE_NO = TREE_NUMBER,  SPECIES = TREE_SPECIES_CODE, SP0,
                             CR_CL = CROWN_CLASS_CODE, PRO_RING = PRORATE_RING_COUNT,
                             BORAG_FL = BORING_AGE + AGE_CORE_MISSED_YEARS_FIELD,
                             BORE_AGE = MICROSCOPE_AGE + AGE_CORE_MISSED_YEARS_LAB,
                             AGE_CORR = AGE_CORRECTION,  TOTAL_AG = TOTAL_AGE,
                             PHYS_AGE = PHYSIOLOGICAL_AGE, GROW_20YR = RADIAL_INCREMENT_LAST_20_YR,
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

  vi_h_th <- treemeasurements[!is.na(AGE_MEASMT_HEIGHT),
                              .(CLSTR_ID, PLOT = PLOT_NUMBER,
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
  saveRDS(vi_h, file.path(outputPath, "vi_h.rds"))
  rm(vi_h, vi_h_th)
  gc()

  vi_i <- treemeasurements[!is.na(DIAMETER) & is.na(LENGTH),
                           .(CLSTR_ID, PLOT = PLOT_NUMBER,
                             TREE_NO = TREE_NUMBER,
                             SPECIES = TREE_SPECIES_CODE, SP0,
                             DBH = DIAMETER,
                             LV_D = TREE_EXTANT_CODE,
                             S_F = TREE_STANCE_CODE,
                             BROKEN_TOP_IND,
                             DIAM_BTP = BROKEN_TOP_DIAMETER,
                             HEIGHT_TO_BREAK,
                             HT_PROJ = PROJECTED_HEIGHT)]
  saveRDS(vi_i, file.path(outputPath, "vi_i.rds"))
  rm(vi_i)
  treemeasurements <- treemeasurements[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, PLOT = PLOT_NUMBER,
                      TREE_NUMBER, SPECIES = TREE_SPECIES_CODE, SP0,
                      DISTANCE = STEM_MAP_DISTANCE, AZIMUTH = STEM_MAP_BEARING,
                      OUT_OF_PLOT_IND, MEASUREMENT_ANOMALY_CODE)]

  gc()
  treeloss <- readRDS(dir(inputPath, "TreeLossIndicators.rds",
                          full.names = TRUE)) %>%
    data.table
  treeloss <- treeloss[SITE_IDENTIFIER %in% samplesites$SITE_IDENTIFIER,]

  # vi_d <- treemeasurements[DIAMETER_MEASMT_HEIGHT == 1.3 &
  #                            OUT_OF_PLOT_IND == "N",
  #                          .(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, PLOT_CATEGORY_CODE,
  #                            TREE_NUMBER)]
  treeloss <- treeloss[order(SITE_IDENTIFIER, VISIT_NUMBER,
                             PLOT_NUMBER, TREE_NUMBER, LOCATION_FROM),
                       .(SITE_IDENTIFIER, VISIT_NUMBER, PLOT = PLOT_NUMBER, TREE_NUMBER,
                         LOSS_IN = TREE_LOSS_INDICATOR_CODE, # not sure
                         LOC_FROM = LOCATION_FROM,
                         LOC_TO = LOCATION_TO,
                         FREQ = FREQUENCY,
                         T_SIGN = NA,
                         F_SIGN = NA)]
  gc()
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
  treedamage <- treedamage[SITE_IDENTIFIER %in% samplesites$SITE_IDENTIFIER,]

  treedamage <- treedamage[order(SITE_IDENTIFIER, VISIT_NUMBER,
                                 PLOT_NUMBER, TREE_NUMBER),]

  treedamage <- treedamage[,.(SITE_IDENTIFIER, VISIT_NUMBER,
                              PLOT = PLOT_NUMBER, TREE_NUMBER,
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
                treemeasurements[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER,
                                    PLOT,
                                    TREE_NUMBER, SPECIES, SP0,
                                    DISTANCE, AZIMUTH,
                                    OUT_OF_PLOT_IND, MEASUREMENT_ANOMALY_CODE)],
                by = c("SITE_IDENTIFIER", "VISIT_NUMBER", "PLOT", "TREE_NUMBER"),
                all = TRUE)
  vi_d <- vi_d[OUT_OF_PLOT_IND == "N",]
  vi_d <- vi_d[MEASUREMENT_ANOMALY_CODE %in% c(NA, "M", "D", "F", "H", "N"),]
  vi_d[, ':='(OUT_OF_PLOT_IND = NULL,
              MEASUREMENT_ANOMALY_CODE = NULL)]
  vi_d[!is.na(DISTANCE) & !is.na(AZIMUTH), STEM := TRUE]
  vi_d <- vi_d[ind == TRUE | STEM == TRUE,]
  vi_d[, ':='(ind = NULL,
              SITE_IDENTIFIER = NULL,
              VISIT_NUMBER = NULL)]
  setnames(vi_d, c("TREE_NUMBER"),
           c("TREE_NO"))
  saveRDS(vi_d, file.path(outputPath, "vi_d.rds"))

  SmallLiveTreeTallies <- readRDS(dir(inputPath,
                                      pattern =  "SmallLiveTreeTallies.rds",
                                      full.names = TRUE))
  SmallLiveTreeTallies <- SmallLiveTreeTallies[SITE_IDENTIFIER %in% samplesites$SITE_IDENTIFIER,]
  SmallLiveTreeTallies <- merge(SmallLiveTreeTallies,
                                unique(vi_a[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER)],
                                       by = c("SITE_IDENTIFIER", "VISIT_NUMBER")),
                                by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                                all.x = TRUE)
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
  vi_f <- merge(vi_f, specieslookup,
                by = "TREE_SPECIES_CODE", all.x = TRUE)
  setnames(vi_f, "TREE_SPECIES_CODE", "SPECIES")
  saveRDS(vi_f, file.path(outputPath, "vi_f.rds"))


  stumptallies <- readRDS(dir(inputPath, pattern = "StumpTallies.rds",
                              full.names = TRUE))
  stumptallies <- stumptallies[SITE_IDENTIFIER %in% samplesites$SITE_IDENTIFIER,]
  stumptallies <- merge(stumptallies,
                        unique(vi_a[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER)],
                               by = c("SITE_IDENTIFIER", "VISIT_NUMBER")),
                        by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                        all.x = TRUE)
  vi_g <- stumptallies[,.(CLSTR_ID, PLOT = "I", TREE_SPECIES_CODE,
                          FREQ = STUMP_OCCURRENCE_FREQUENCY, DIB = DIAMETER_INSIDE_BARK,
                          HEIGHT = STUMP_HEIGHT, PCT_SND = SOUND_WOOD_PERCENT,
                          WL_USE = WILDLIFE_USE_CODE, WL_WOOD = WOOD_CONDITION_CODE,
                          BARK_RET = BARK_RETENTION_CODE, ROOT_ROT = DAMAGE_AGENT_CODE)]
  vi_g <- merge(vi_g, specieslookup,
                by = "TREE_SPECIES_CODE", all.x = TRUE)
  setnames(vi_g, "TREE_SPECIES_CODE", "SPECIES")
  saveRDS(vi_g, file.path(outputPath, "vi_g.rds"))
}
