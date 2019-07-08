# title GYS_oracle_org1
#
# description This function is to merge oracle and ascii data before the GYS compiler, the function below is modified
# from ld_gysfx_pt1_rene_10jun2017.sas
# The major changes from the original codes are
#   1. removed spatial attributes in GYS and updated from the most recent map
#   2. clearified the processes in sample_id, plot and tree_no order, as the original sas version creates
#      massive tables, and confuses the compilation.
#   3. removed some pre-compiled results as inputs
#
# param oracleSourcePath character, Specifies the path that stores data from oracle data base.
#                                    In VRI compiler, this should be the savePath for \code{\link{loadVGIS}}.
# param outputPath character, Specifies the path to save your outputs. If missing, the current working
#                   directory will be choosed.
#
# return no item returned
#
# importFrom data.table ':=' data.table copy
GYS_oracle_org1 <- function(oracleSourcePath, outputPath){
  r <- readRDS(file.path(oracleSourcePath, "rcl_area_validn_codes.rds"))
  r <- r[,.(RCL_AREA_VALIDN_SKEY, COMPT_LTR, COMPT, FIZ, REGION)]
  s <- readRDS(file.path(oracleSourcePath, "gys_sample.rds"))
  s <- s[,.(RCL_AREA_VALIDN_SKEY, ORACLE_ID_NO = SAMPLE_ID,
            INST = INSTALLATION, SAMPLE_TYPE_CODE, SAMPLE_NO)]
  init_samples <- merge(r, s, by = "RCL_AREA_VALIDN_SKEY")
  init_samples[, ST := substr(SAMPLE_TYPE_CODE, 1, 1)]
  init_samples[is.na(INST), INST := 0]
  init_samples[, ':='(REGION = sprintf("%02d", REGION),
                      COMPT = sprintf("%03d", COMPT),
                      INST = sprintf("%03d", INST),
                      SAMPLE_NO = sprintf("%03d", SAMPLE_NO))]
  init_samples[is.na(COMPT_LTR), COMPT_LTR := " "]
  init_samples[is.na(ST), ST := " "]
  init_samples[, SAMP_ID := paste0(REGION, COMPT, COMPT_LTR,
                                   ST, INST, SAMPLE_NO)]
  oracle_samp_id_lookup <- unique(init_samples[,.(ORACLE_ID_NO, SAMP_ID)],
                                  by = c("ORACLE_ID_NO", "SAMP_ID"))

  r <- init_samples[,.(SAMP_ID, ORACLE_ID_NO)]
  s <- readRDS(file.path(oracleSourcePath, "gys_sample.rds"))
  s[, newindex := SAMPLE_ID]
  r[, newindex := ORACLE_ID_NO]
  gys_sample <- merge(r, s, by = "newindex")
  gys_sample[, ':='(newindex = NULL,
                    ORACLE_ID_NO = NULL)]
  gys_sample <- gys_sample[substr(SAMP_ID, 7, 7) %in% c("G", "R", "T", "I")]
  saveRDS(gys_sample, file.path(outputPath, "gys_sample.rds"))
  rm(gys_sample, init_samples, r, s)
  gc()

  gys_SAMPLE_MEASUREMENT <- readRDS(file.path(oracleSourcePath,
                                              "gys_SAMPLE_MEASUREMENT.rds"))

  gys_SAMPLE_MEASUREMENT[,':='(MEAS_NO = sprintf("%02d", MEAS_NO),
                               ORACLE_ID_NO = SAMPLE_ID)]
  gys_SAMPLE_MEASUREMENT <- merge(gys_SAMPLE_MEASUREMENT, oracle_samp_id_lookup,
                                  by = "ORACLE_ID_NO", all.x = TRUE)
  gys_SAMPLE_MEASUREMENT[, SAMP_TYP := substr(SAMP_ID, 7, 7)]
  gys_SAMPLE_MEASUREMENT <- gys_SAMPLE_MEASUREMENT[order(SAMP_ID, MEAS_NO),]
  meas_id <- gys_SAMPLE_MEASUREMENT[,.(SAMPLE_MEASUREMENT_ID, SAMP_ID, MEAS_NO)]
  meas_id <- unique(meas_id, by = c("SAMPLE_MEASUREMENT_ID", "SAMP_ID", "MEAS_NO"))
  gys_SAMPLE_MEASUREMENT[, SAMPLE_ID := NULL]
  gys_SAMPLE_MEASUREMENT <- gys_SAMPLE_MEASUREMENT[substr(SAMP_ID, 7, 7) %in% c("G", "R", "T", "I"),]
  saveRDS(gys_SAMPLE_MEASUREMENT, file.path(outputPath, "gys_SAMPLE_MEASUREMENT.rds"))
  rm(gys_SAMPLE_MEASUREMENT)
  gc()

  s <- data.table::copy(oracle_samp_id_lookup)
  nt <- readRDS(file.path(oracleSourcePath, "access_note.rds"))
  nt[, ORACLE_ID_NO := SAMPLE_ID]
  gys_access_note <- merge(nt, s, by = "ORACLE_ID_NO")
  gys_access_note <- gys_access_note[order(SAMP_ID),]
  gys_access_note <- gys_access_note[substr(SAMP_ID, 7, 7) %in% c("G", "R", "T", "I"),]

  saveRDS(gys_access_note, file.path(outputPath, "gys_access_note.rds"))
  rm(nt, gys_access_note, s)
  gc()

  s <- data.table::copy(oracle_samp_id_lookup)
  pm <- readRDS(file.path(oracleSourcePath, "gys_plot.rds"))
  pm[, ORACLE_ID_NO := SAMPLE_ID]
  gys_plot <- merge(pm, s, by = "ORACLE_ID_NO")
  gys_plot <- gys_plot[order(SAMP_ID, PLOT_NO),]
  gys_plot[, PLOT_NO := sprintf("%02d", PLOT_NO)]
  plot_id <- gys_plot[,.(PLOT_ID, SAMP_ID, PLOT_NO)]
  gys_plot[, ORACLE_ID_NO := NULL]
  gys_plot <- gys_plot[substr(SAMP_ID, 7, 7) %in% c("G", "R", "T", "I")]
  saveRDS(gys_plot, file.path(outputPath, "gys_plot.rds"))
  rm(pm, gys_plot, s)
  gc()

  pm <- data.table::copy(plot_id)
  pl <- readRDS(file.path(oracleSourcePath, "gys_subplot.rds"))
  pl <- pl[,.(PLOT_ID, SUBPLOT_RADIUS, SUBPLOT_BASAL_AREA_FACTOR, SUBPLOT_PER_HECTARE_FACTOR,
              SUBPLOT_AREA, THIRD_PLOT_RADIUS, SAMPLE_MEASUREMENT_ID)]
  m <- data.table::copy(meas_id)
  m[, SAMP_ID := NULL]
  gys_subplot <- merge(pl, pm, by = "PLOT_ID")
  gys_subplot <- merge(gys_subplot, m, by = "SAMPLE_MEASUREMENT_ID")
  gys_subplot[, PLOT_NO := sprintf("%02s", PLOT_NO)]
  gys_subplot <- gys_subplot[order(SAMP_ID, PLOT_NO),]
  gys_subplot <- gys_subplot[substr(SAMP_ID, 7, 7) %in% c("G", "R", "T", "I"),]


  saveRDS(gys_subplot, file.path(outputPath, "gys_subplot.rds"))
  rm(gys_subplot, pl, m, pm)
  gc()

  gys_tree_measurement <- readRDS(file.path(oracleSourcePath, "gys_tree_measurement.rds"))
  tree_layer <- unique(gys_tree_measurement$LAYER_ID)

  gys_layer_species_assessment <- readRDS(file.path(oracleSourcePath, "gys_layer_species_assessment.rds"))
  spec_layer <- unique(gys_layer_species_assessment$LAYER_ID)
  layer_list <- unique(c(tree_layer, spec_layer))

  m <- data.table::copy(meas_id)
  l <- readRDS(file.path(oracleSourcePath, "gys_layer.rds"))
  too_many_layers <- merge(l, m[,.(SAMPLE_MEASUREMENT_ID, SAMP_ID, MEAS_NO)],
                           by = "SAMPLE_MEASUREMENT_ID")
  too_many_layers <- too_many_layers[substr(SAMP_ID, 7, 7) %in% c("G", "R", "T", "I"),]
  too_many_layers <- too_many_layers[order(SAMP_ID, MEAS_NO, LAYER_CODE),]

  gys_layer <- too_many_layers[LAYER_ID %in% layer_list,]

  layer_idx <- unique(gys_layer[,.(SAMP_ID, MEAS_NO, LAYER_ID, LAYER_CODE)],
                      by = c("SAMP_ID", "MEAS_NO", "LAYER_ID", "LAYER_CODE"))

  layer_code <- gys_layer[,.(LAYER_CODE, LAYER_ID)]
  layer_code <- layer_code[order(LAYER_CODE),]
  saveRDS(gys_layer, file.path(outputPath, "gys_layer.rds"))
  gys_tree_measurement <- gys_tree_measurement[,.(TREE_ID, CRN_CL = CROWN_CLASS_CODE,
                                                  HEIGHT_SOURCE_CODE, HT_DIAMETER_CURVE_USE_CODE,
                                                  HT_MEAS_STATUS = HT_MEASUREMENT_STATUS_CODE,
                                                  PITH = PITH_CODE, TR_CLASS = TREE_CLASS_CODE,
                                                  DBH = DIAM_AT_13M, SI_BHA50 = SITE_INDEX,
                                                  CURVE_OR_EST_HT, SPL_TRE = SUB_PLOT_TREE_IND,
                                                  DM_137_4 = DIAM_AT_137M, HT_MEAS = MEAS_HT,
                                                  HT_BRK = HT_TO_BREAK, HT_BORE = BORING_HT,
                                                  AGE_BORE = BORING_AGE, NEW_AGE, STUMP_DIAM, STUMP_HT,
                                                  STUMP_STATUS_CODE, TREE_SUITABLE_FOR_HT,
                                                  AGE_CORR = AGE_CORRECTION, AGE_TOT = TOTAL_AGE,
                                                  AGE_COMP_IND = COMPILATION_AGE_IND, TREE_MEASUREMENT_ID,
                                                  SAMPLE_MEASUREMENT_ID)]
  saveRDS(gys_tree_measurement, file.path(outputPath, "gys_tree_measurement.rds"))
  rm(gys_layer, l, too_many_layers, gys_tree_measurement, m,
     tree_layer, spec_layer)
  gc()

  gys_layer_species_assessment <- merge(gys_layer_species_assessment,
                                        layer_idx,
                                        by = "LAYER_ID")
  gys_layer_species_assessment <- gys_layer_species_assessment[substr(SAMP_ID, 7, 7) %in% c("G", "R", "T", "I"),]

  saveRDS(gys_layer_species_assessment,
          file.path(outputPath, "gys_layer_species_assessment.rds"))
  rm(gys_layer_species_assessment)
  gc()


  p <- data.table::copy(plot_id)
  tr <- readRDS(file.path(oracleSourcePath, "gys_tree.rds"))
  gys_tree <- merge(tr, p[,.(PLOT_ID, SAMP_ID, PLOT_NO)],
                    by = "PLOT_ID")
  gys_tree[, ':='(TREE_NO = sprintf("%04s", TREE_NO),
                  PLOT_NO = sprintf("%02s", PLOT_NO))]
  tree_idx <- unique(gys_tree[,.(SAMP_ID, PLOT_NO, TREE_NO, TREE_ID)],
                     by = c("SAMP_ID", "PLOT_NO", "TREE_NO", "TREE_ID"))
  gys_tree <- gys_tree[substr(gys_tree$SAMP_ID, 7, 7) %in% c("G", "R", "T", "I"),]


  saveRDS(gys_tree, file.path(outputPath, "gys_tree.rds"))
  rm(p, tr, gys_tree)
  gc()

  tr <- readRDS(file.path(oracleSourcePath, "gys_tree_measurement.rds"))
  p <- data.table::copy(tree_idx)
  tree_assess <- merge(tr[,.(TREE_ID, TREE_MEASUREMENT_ID, LAYER_ID,
                             SAMPLE_MEASUREMENT_ID)],
                       p, by = "TREE_ID")
  tree_assess <- tree_assess[order(LAYER_ID),]
  tree_assess <- tree_assess[substr(SAMP_ID, 7, 7) %in% c("G", "R", "T", "I"),]

  m <- data.table::copy(meas_id)
  tree_assess1 <- merge(tree_assess, m[,.(SAMPLE_MEASUREMENT_ID, MEAS_NO)],
                        by = "SAMPLE_MEASUREMENT_ID")
  tree_assess1 <- tree_assess1[order(LAYER_ID),]
  rm(tree_assess, m, p)
  gc()

  tree_meas_info <- merge(tree_assess1[,.(TREE_MEASUREMENT_ID, SAMP_ID, PLOT_NO,
                                          TREE_NO, MEAS_NO)], tr,
                          by = "TREE_MEASUREMENT_ID")
  rm(tr, tree_assess1)
  gc()

  tree_meas_info <- merge(tree_meas_info, layer_code, by = "LAYER_ID", all.x = TRUE)
  tree_meas_id <- tree_meas_info[,.(SAMP_ID, PLOT_NO, MEAS_NO, LAYER_CODE, TREE_NO,
                                    TREE_MEASUREMENT_ID)]
  saveRDS(tree_meas_info, file.path(outputPath, "tree_meas_info.rds"))
  rm(tree_meas_info)
  gc()

  path <- readRDS(file.path(oracleSourcePath, "gys_pathological_observation.rds"))
  gys_pathological_observation <- merge(path, tree_meas_id,
                                        by = "TREE_MEASUREMENT_ID",
                                        all.x = TRUE)
  gys_pathological_observation <- gys_pathological_observation[substr(SAMP_ID, 7, 7) %in% c("G", "R", "T", "I"),]
  gys_pathological_observation <- gys_pathological_observation[order(TREE_MEASUREMENT_ID),]

  saveRDS(gys_pathological_observation, file.path(outputPath, "gys_pathological_observation.rds"))
  rm(path)
  gc()




  gys_sample <- readRDS(file.path(outputPath, "gys_sample.rds"))
  setnames(gys_sample,
           c("BCGS_20K_MAP_TILE", "DATA_OWNER_CODE", "PER_HECTARE_FACTOR_1",
             "PLOTS_IN_SAMPLE_CNT", "SAMPLE_STATUS_CODE", "SAMPLING_METHOD_CODE",
             "INSTALLATION", "SPECIAL_SAMPLE_CODE", "STAND_ORIGIN_CODE",
             "TFL", "TSA_NO"),
           c("BCGS_MAP", "OWNER", "PHF_PM",
             "NO_PLOTS", "SAMP_STS", "SAMP_MTD",
             "INST", "SAMP_SCD", "STND_ORG",
             "_TFL", "TSA"))
  gys_sample[is.na(INST), INST := 0]
  gys_sample[, ':='(YR_EST = ESTABLISHMENT_YR,
                    SAMP_TYP = substr(SAMPLE_TYPE_CODE, 1, 1),
                    RG_PRJNO = as.character(REGIONAL_PROJECT_NO))]
  gys_sample[RG_PRJNO %in% c(0, NA), RG_PRJNO := ""]
  gys_sample[is.na(SPECIAL_CRUISE_NO), SPECIAL_CRUISE_NO := 0]
  gys_sample[, SPECIAL_CRUISE_SUBCODE := as.numeric(SPECIAL_CRUISE_SUBCODE)]
  gys_sample[is.na(SPECIAL_CRUISE_SUBCODE), SPECIAL_CRUISE_SUBCODE := 0]

  gys_sample[, ':='(PSYU = sprintf("%03d", SPECIAL_CRUISE_NO),
                    PSYU_BLK = SPECIAL_CRUISE_SUBCODE,
                    TFL = sprintf("%02d", `_TFL`),
                    RC = substr(SAMP_ID, 1, 5))]



  gys_sample[, TR_STD_YR := as.numeric(gsub(" ", "", TREATED_STAND_PLANTATION_YR))]
  gys_sample[TR_STD_YR == 0, TR_STD_YR := NA]
  gys_sample[, TR_AGE := as.numeric(gsub(" ", "", TREATED_STAND_AGE_OF_STOCK))]
  gys_sample[TR_AGE == 0, TR_AGE := NA]

  gys_sample[, STD_TR_PT1 := paste(TR_STD_YR, TR_AGE, sep = "")]
  gys_sample[, STD_TR_PT1 := gsub("NA", "", STD_TR_PT1)]
  gys_sample[STD_TR_PT1 == "", STD_TR_PT1 := NA]

  ### this is a typo in assigning plot type in original sas codes
  gys_sample[SAMP_TYP == "S" & PHF_PM > 0, PLOT_TYP := "F"]
  gys_sample[is.na(PLOT_TYP), PLOT_TYP := "V"]

  ## TFL, TSA AND PSYU SHOULD BE UPDATED FROM MAPS
  gys_sample_withloc <- gys_sample[!is.na(UTM_ZONE) & !is.na(UTM_EASTING) &
                                     !is.na(UTM_NORTHING) & UTM_ZONE %in% 8:11,
                                   .(SAMP_ID, UTM_ZONE, UTM_EASTING, UTM_NORTHING)]

  ## for bec zone
  loc_bec <- getSpatial(pointID = gys_sample_withloc$SAMP_ID,
                        zone = gys_sample_withloc$UTM_ZONE,
                        northing = gys_sample_withloc$UTM_NORTHING,
                        easting = gys_sample_withloc$UTM_EASTING,
                        spatialAttribute = "bec")
  names(loc_bec) <- c("SAMP_ID", "BEC", "BEC_SBZ", "BEC_VAR")
  loc_tsa <- getSpatial(pointID = gys_sample_withloc$SAMP_ID,
                        zone = gys_sample_withloc$UTM_ZONE,
                        northing = gys_sample_withloc$UTM_NORTHING,
                        easting = gys_sample_withloc$UTM_EASTING,
                        spatialAttribute = "tsa")
  names(loc_tsa) <- c("SAMP_ID", "TSA", "TSA_DESC")

  gys_sample[,':='(BEC_CODE_ID = NULL,
                   BEC_SITE_SERIES_ID = NULL,
                   BEC_SOURCE_CODE = NULL,
                   BEC_SITE_SERIES_CD = NULL,
                   TSA = NULL,
                   TSA_BLK = NULL,
                   BGC_ZONE = NULL,
                   BGC_SBZN = NULL,
                   BGC_VAR = NULL,
                   BGC_PHASE = NULL,
                   BGC_TRANSITION_IND = NULL)]
  gys_sample <- merge(gys_sample, loc_bec, by = "SAMP_ID", all.x = TRUE)
  gys_sample <- merge(gys_sample, loc_tsa, by = "SAMP_ID", all.x = TRUE)

  samples <- gys_sample[,.(SAMP_ID, UTM_ZONE, UTM_EASTING, UTM_NORTHING, OWNER, PSYU, PSYU_BLK, SAMP_MTD, SAMP_STS, STND_ORG, TSA, TSA_DESC,
                           TFL, YR_EST, RG_PRJNO, BCGS_MAP, BEC, BEC_SBZ, BEC_VAR, ECOSYSTEM_SUBPLOT_PHASE,
                           VALIDATION_LEVEL_CODE, TIEPOINT_UTM_EASTING, TIEPOINT_UTM_EASTING, TIEPOINT_UTM_NORTHING)]

  s_meas <- gys_sample[,.(SAMP_ID, OWNER, SAMP_SCD, STD_TR_PT1, PLOT_TYP, NO_PLOTS, SAMP_TYP)]
  saveRDS(s_meas, file.path(outputPath, "s_meas.rds"))


  gys_sample_measurement <- readRDS(file.path(outputPath, "gys_sample_measurement.rds"))
  setnames(gys_sample_measurement,
           c("AGE_HT_MIDPOINT", "DBH_CONVERTED_IND", "SAMPLE_MIN_DBH", "STAND_STRUCTURE_CODE",
             "IN_OUT_OF_PROV_IND", "AGENCY_TYPE_CODE", "SELECTIVELY_LOGGED_IND"),
           c("AGE_HT_M", "DBH_CNV", "DBH_SMIN", "STND_STR", "I_O_PROV", "AGENCY", "SEL_LGD"))
  gys_sample_measurement[SAMPLE_BREAK_POINT_IND == "D", DIAM_BKP := SAMPLE_BREAK_POINT]
  gys_sample_measurement[SAMPLE_BREAK_POINT_IND != "D", HT_TG_LM := SAMPLE_BREAK_POINT]
  gys_sample_measurement[, ':='(GROW_SN = sprintf("%04d", GROWING_SEASON_YR),
                                MEAS_DT = as.Date(MEAS_DATE))]
  gys_sample_measurement[,MEAS_YR := substr(as.character(MEAS_DT), 1, 4)]
  gys_sample_measurement[,GROW_SN2 := get_grow_season(MEAS_DT)]
  gys_sample_measurement <- gys_sample_measurement[order(SAMP_ID, MEAS_NO),]
  saveRDS(gys_sample_measurement, file.path(outputPath, "measurements.rds"))

  meas_samp <- gys_sample_measurement[,.(NO_MEAS = length(MEAS_YR),
                                         YR_MS1 = max(MEAS_YR)),
                                      by = "SAMP_ID"]

  meas_layer <- readRDS(file.path(outputPath, "gys_layer.rds"))
  meas_layer <- meas_layer[,.(SAMP_ID, LAYER = LAYER_CODE, MEAS_NO,
                              CC_AIR = CROWN_CLOSURE_AIR_PERCENT,
                              CC_GND = CROWN_CLOSURE_GROUND_PERCENT,
                              STEMS_FE = DENSITY_FIELD_EST,
                              LYR_STR = LAYER_STRUCTURE,
                              SITE_SCD = SPECIAL_SITE_CODE)]
  meas_layer <- meas_layer[order(SAMP_ID, LAYER, MEAS_NO),]


  ls <- readRDS(file.path(outputPath, "gys_layer_species_assessment.rds"))
  sp1_layer <- ls[LEADING_SPECIES_IND == "Y",
                  .(SAMP_ID, MEAS_NO, LAYER = LAYER_CODE, SPCA_BA = BASAL_AREA,
                    LSPA_PCT = BASAL_AREA_PERCENT, SI_BHA50 = LEADING_SPC_SITE_INDEX_BHA_50,
                    LEADING_SPC_AGE_RANGE_MIN, LEADING_SPC_AGE_RANGE_MAX,
                    AGEM_BHAGE = LEADING_SPC_MEAN_BREAST_HT_AGE,
                    HTM_SEF = LEADING_SPC_MEAN_HT, AGEM_SEF = LEADING_SPC_MEAN_TOTAL_AGE,
                    HT_TP_CV = LEADING_SPC_TOP_HT_CURVE, LEADING_QMD = QUADRATIC_MEAN_DIAM,
                    LSPA = SPECIES_CODE)]
  sp1_layer <- sp1_layer[order(SAMP_ID, LAYER, MEAS_NO, LSPA_PCT),]

  sp2_layer <- ls[SECOND_SPECIES_IND == "Y",
                  .(SAMP_ID, MEAS_NO, LAYER = LAYER_CODE, SPCB_BA = BASAL_AREA,
                    LSPB_PCT = BASAL_AREA_PERCENT,
                    LSPB = SPECIES_CODE)]
  sp2_layer <- sp2_layer[order(SAMP_ID, LAYER, MEAS_NO, LSPB_PCT),]

  sp1_layer <- sp1_layer[order(SAMP_ID, LAYER, MEAS_NO),]
  sp1_layerx <-unique(sp1_layer, by = c("SAMP_ID", "LAYER", "MEAS_NO"))


  sp1_layer[, NO_MEAS := length(LSPA_PCT),
            by = c("SAMP_ID", "LAYER", "MEAS_NO")]

  processtable <- sp1_layer[NO_MEAS != 1,]
  processtable[, processrow := 1:length(LSPA_PCT),
               by = c("SAMP_ID", "LAYER", "MEAS_NO")]
  processtable[, uniobs := 1:length(LSPA)]
  sp2_layerx <- processtable[0,]
  for(i in 1:(max(sp1_layer$NO_MEAS))){
    if(i == 1){
      basetable <- processtable[processrow == i,.(SAMP_ID, LAYER, MEAS_NO, LSPA_PCT, LSPA)]
    } else {
      comparetable <- processtable[processrow == i, .(SAMP_ID, LAYER, MEAS_NO,
                                                      LSPA_PCT_n = LSPA_PCT, LSPA_n = LSPA,
                                                      uniobs)]
      combinedtable <- merge(basetable, comparetable,
                             by = c("SAMP_ID", "LAYER", "MEAS_NO"),
                             all.x = TRUE)
      selectedrow <- combinedtable[LSPA_PCT != LSPA_PCT_n |
                                     LSPA != LSPA_n,]$uniobs
      sp2_layerx <- rbind(sp2_layerx, processtable[uniobs %in% selectedrow,])
    }
  }
  rm(processtable, i, basetable, combinedtable, comparetable, selectedrow)
  setnames(sp2_layerx, c("SPCA_BA", "LSPA_PCT", "LSPA", "AGEM_SEF", "HTM_SEF"),
           c("SPCB_BA", "LSPB_PCT", "LSPB", "AGE2M_SEF", "HT2M_SEF"))


  sp2_layer <- sp2_layer[order(SAMP_ID, LAYER, MEAS_NO),]
  sp2_layerx1 <- unique(sp2_layer,
                        by = c("SAMP_ID", "LAYER", "MEAS_NO"))
  sp2_layer[, ':='(obsseq = 1:length(LSPB_PCT),
                   totalobs = length(LSPB_PCT)),
            by = c("SAMP_ID", "LAYER", "MEAS_NO")]
  processtable <- sp2_layer[totalobs > 1,]
  processtable[, uniobs := 1:length(LSPB_PCT)]
  for(i in 1:(max(processtable$totalobs))){
    if(i == 1){
      basetable <- processtable[obsseq == i,
                                .(SAMP_ID, LAYER, MEAS_NO, LSPB_PCT)]
    } else {
      comparetable <- processtable[obsseq == i, .(SAMP_ID, LAYER, MEAS_NO,
                                                  LSPB_PCT_n = LSPB_PCT,
                                                  uniobs)]
      combinedtable <- merge(basetable, comparetable,
                             by = c("SAMP_ID", "LAYER", "MEAS_NO"),
                             all.x = TRUE)
      selectedrow <- processtable[uniobs %in% combinedtable[LSPB_PCT != LSPB_PCT_n,]$uniobs,]
      selectedrow[, ':='(obsseq = NULL,
                         totalobs = NULL,
                         uniobs = NULL)]
      sp2_layerx1 <- rbind(sp2_layerx1, selectedrow)
    }
  }
  rm(processtable, i, basetable, combinedtable, comparetable, selectedrow)
  sp2_layerx <- sp2_layerx[, names(sp2_layerx1), with = FALSE]
  sp2_layerx2 <- rbind(sp2_layerx1, sp2_layerx)
  sp2_layerx2 <- sp2_layerx2[order(SAMP_ID, LAYER, MEAS_NO),]
  sp2_layerx2 <- unique(sp2_layerx2,
                        by = c("SAMP_ID", "LAYER", "MEAS_NO"))

  link1 <- readRDS(file.path(outputPath, "gys_sample_measurement.rds"))
  link1 <- link1[,.(SAMP_ID, MEAS_NO)]

  link2 <- readRDS(file.path(outputPath, "gys_plot.rds"))
  link2 <- link2[,.(SAMP_ID, PLOT_NO)]

  link3 <- merge(link1, link2, by = "SAMP_ID", all.x = TRUE)
  rm(link1, link2)
  gc()

  subplot <- readRDS(file.path(outputPath, "gys_subplot.rds"))
  subplot[, fromdb := TRUE]
  subplot <- merge(link3, subplot,
                   by = c("SAMP_ID", "PLOT_NO", "MEAS_NO"),
                   all.x = TRUE)

  plotm <- readRDS(file.path(outputPath, "gys_plot.rds"))
  plotm <- plotm[,.(SAMP_ID, PLOT_ID, PLOT_NO, ASPECT = PLOT_ASPECT,
                    SL_POS = SLOPE_POSITION_CODE, SLOPE = PLOT_SLOPE,
                    ELEV = PLOT_ELEVATION, RAD_PM = PLOT_RADIUS,
                    AREA_PM = PLOT_AREA, BAF_PM = BASAL_AREA_FACTOR)]
  plotm <- plotm[order(SAMP_ID, PLOT_NO),]

  pl <- unique(subplot[,.(SAMP_ID, PLOT_ID, RAD_PS = SUBPLOT_RADIUS,
                          BAF_PS = SUBPLOT_BASAL_AREA_FACTOR,
                          PHF_PS = SUBPLOT_PER_HECTARE_FACTOR,
                          AREA_PS = SUBPLOT_AREA,
                          RAD_THIRD = THIRD_PLOT_RADIUS,
                          SAMPLE_MEASUREMENT_ID)],
               by = c("SAMP_ID", "PLOT_ID"))
  plot_final <- merge(plotm, pl, by = c("SAMP_ID", "PLOT_ID"))
  saveRDS(plot_final, file.path(outputPath, "plot.rds"))
  plot_info <- plot_final[,.(SAMP_ID, PLOT_NO, ASPECT, SL_POS, SLOPE,
                             ELEV, RAD_PM, AREA_PM, BAF_PM)]

  ## did not recode layer_info

  gys_pathological_observation <- readRDS(file.path(outputPath, "gys_pathological_observation.rds"))
  path <- unique(gys_pathological_observation[,.(TREE_MEASUREMENT_ID,
                                                 SAMP_ID, MEAS_NO, PLOT_NO, LAYER_CODE, TREE_NO)],
                 by = "TREE_MEASUREMENT_ID")
  blc <- gys_pathological_observation[PATHOLOGICAL_OBSRVTN_CODE == "BCNK",
                                      .(TREE_MEASUREMENT_ID,
                                        BLIND_CONK = PATHOLOGICAL_OBSRVTN_CODE,
                                        BLC = 1)]
  path <- merge(path, blc, by = "TREE_MEASUREMENT_ID", all.x = TRUE)
  path[is.na(BLC), BLC := 0]
  ck <- gys_pathological_observation[PATHOLOGICAL_OBSRVTN_CODE == "CNK",
                                     .(TREE_MEASUREMENT_ID,
                                       CONK = PATHOLOGICAL_OBSRVTN_CODE,
                                       CK = 1)]
  path <- merge(path, ck, by = "TREE_MEASUREMENT_ID", all.x = TRUE)
  path[is.na(CK), CK := 0]
  sc <- gys_pathological_observation[PATHOLOGICAL_OBSRVTN_CODE == "SCR",
                                     .(TREE_MEASUREMENT_ID,
                                       SCAR = PATHOLOGICAL_OBSRVTN_CODE,
                                       SC = 1)]
  path <- merge(path, sc, by = "TREE_MEASUREMENT_ID", all.x = TRUE)
  path[is.na(SC), SC := 0]

  fk <- gys_pathological_observation[PATHOLOGICAL_OBSRVTN_CODE == "FRK",
                                     .(TREE_MEASUREMENT_ID,
                                       FORK_OR_CROOK = PATHOLOGICAL_OBSRVTN_CODE,
                                       FK = 1)]
  path <- merge(path, fk, by = "TREE_MEASUREMENT_ID", all.x = TRUE)
  path[is.na(FK), FK := 0]

  fc <- gys_pathological_observation[PATHOLOGICAL_OBSRVTN_CODE == "FRS",
                                     .(TREE_MEASUREMENT_ID,
                                       FROST_CRACK = PATHOLOGICAL_OBSRVTN_CODE,
                                       FC = 1)]
  path <- merge(path, fc, by = "TREE_MEASUREMENT_ID", all.x = TRUE)
  path[is.na(FC), FC := 0]


  mt <- gys_pathological_observation[PATHOLOGICAL_OBSRVTN_CODE == "MIS",
                                     .(TREE_MEASUREMENT_ID,
                                       MISTLE_TOE = PATHOLOGICAL_OBSRVTN_CODE,
                                       MT = 1)]
  path <- merge(path, mt, by = "TREE_MEASUREMENT_ID", all.x = TRUE)
  path[is.na(MT), MT := 0]


  rb <- gys_pathological_observation[PATHOLOGICAL_OBSRVTN_CODE == "ROT",
                                     .(TREE_MEASUREMENT_ID,
                                       ROTTEN_BRANCH = PATHOLOGICAL_OBSRVTN_CODE,
                                       RB = 1)]
  path <- merge(path, rb, by = "TREE_MEASUREMENT_ID", all.x = TRUE)
  path[is.na(RB), RB := 0]


  dt <- gys_pathological_observation[PATHOLOGICAL_OBSRVTN_CODE == "DTOP",
                                     .(TREE_MEASUREMENT_ID,
                                       DEAD_TOP = PATHOLOGICAL_OBSRVTN_CODE,
                                       DT = 1)]
  path <- merge(path, dt, by = "TREE_MEASUREMENT_ID", all.x = TRUE)
  path[is.na(DT), DT := 0]



  bt <- gys_pathological_observation[PATHOLOGICAL_OBSRVTN_CODE == "BTOP",
                                     .(TREE_MEASUREMENT_ID,
                                       BROKEN_TOP = PATHOLOGICAL_OBSRVTN_CODE,
                                       BT = 1)]
  path <- merge(path, bt, by = "TREE_MEASUREMENT_ID", all.x = TRUE)
  path[is.na(BT), BT := 0]
  path[DT == 1 | BT == 1, dt_bt := 1]
  path[is.na(dt_bt), dt_bt := 0]
  path[, PATH_SEF := paste0(CK, BLC, SC, FK, FC, MT, RB, dt_bt)]
setnames(path, "LAYER_CODE", "LAYER")
  saveRDS(path, file.path(outputPath, "path.rds"))
  saveRDS(tree_idx, file.path(outputPath, "tree_idx.rds"))
  rm(list = ls())
  gc()
}
