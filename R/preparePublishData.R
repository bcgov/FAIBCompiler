# prepare publish data
preparePublishData <- function(compilationPaths,
                               compilationType){
  sampsites_org <- readRDS(file.path(compilationPaths$compilation_db,
                                     "sample_site_header.rds"))

  tflmap <- readRDS(dir(compilationPaths$compilation_map,
                        pattern = "TFL_map",
                        full.names = TRUE))
  tflmap_table <- data.table(data.frame(tflmap$map))
  tflmap_table <- unique(tflmap_table[,.(TFL = FOREST_FILE_ID, LICENCEE)])
  tflmap_table[, LICENCEE := gsub("Corporation", "", LICENCEE)]
  tflmap_table[, LICENCEE := gsub("Inc.", "", LICENCEE)]
  tflmap_table[, LICENCEE := gsub("Ltd.", "", LICENCEE)]
  tflmap_table[, LICENCEE := gsub("Ltd", "", LICENCEE)]
  tflmap_table[, LICENCEE := gsub("Co.", "", LICENCEE)]
  tflmap_table[, LICENCEE := gsub(" ", "", LICENCEE)]
  sampsites <- merge(sampsites_org,
                     tflmap_table,
                     by = "TFL",
                     all.x = TRUE)
  sampsites[!is.na(TFL), MGMT_UNIT := paste0(TFL, "_", LICENCEE)]
  sampsites[, TSA_DESC := gsub(" ", "", gsub(" TSA", "", TSA_DESC))]

  sampsites[is.na(MGMT_UNIT), MGMT_UNIT := paste0("TSA", TSA, "_", TSA_DESC)]


  ownermap <- readRDS(dir(compilationPaths$compilation_map,
                          pattern = "Ownership_map",
                          full.names = TRUE))
  ownermap_table <- data.table(data.frame(ownermap$map))
  ownermap_table <- unique(ownermap_table[,.(OWNER = OWN,
                                             SCHEDULE,
                                             OWN_SCHED_DESCRIP = OWNERSHIP_DESCRIPTION)])

  ownermap_table <- ownermap_table[,.(OWN_SCHED_DESCRIP = paste0(OWN_SCHED_DESCRIP, collapse = " or ")),
                                   by = c("OWNER", "SCHEDULE")]
  sampsites <- merge(sampsites,
                     ownermap_table,
                     by = c("OWNER", "SCHEDULE"),
                     all.x = TRUE)

  faib_header <- sampsites[,.(SITE_IDENTIFIER, BC_ALBERS_X, BC_ALBERS_Y, BEC_ZONE, BEC_SBZ,
                              BEC_VAR, FIZ, IP_UTM, IP_EAST, IP_NRTH, Latitude, Longitude,
                              OWNER, SCHEDULE,
                              SAMPLE_TYPE = SAMPLE_ESTABLISHMENT_TYPE,
                              TFL, TSA, TSA_DESC, MGMT_UNIT,
                              OWN_SCHED_DESCRIP,
                              GRID_BASE = NA, GRID_SIZE = NA)]
  write.csv(faib_header,
            file.path(compilationPaths$compilation_publish,
                      "faib_header.rds"),
            row.names = FALSE,
            na = "")

  sampvisits <- readRDS(file.path(compilationPaths$compilation_db,
                                  "sample_msmt_header.rds"))

  sampvisits <- merge(sampvisits,
                      sampsites_org[,.(SITE_IDENTIFIER, PROJ_AGE_1, PROJECTED_DATE,
                                       SAMPLE_SITE_NAME, SAMPLE_ESTABLISHMENT_TYPE)],
                      by = "SITE_IDENTIFIER",
                      all.x = TRUE)

  sampvisits[, ':='(visit_first = min(VISIT_NUMBER),
                    visit_last = max(VISIT_NUMBER)),
             by = "SITE_IDENTIFIER"]
  sampvisits[VISIT_NUMBER == visit_first,
             FIRST_MSMT := "Y"]
  sampvisits[is.na(FIRST_MSMT), FIRST_MSMT := "N"]

  sampvisits[VISIT_NUMBER == visit_last,
             LAST_MSMT := "Y"]
  sampvisits[is.na(LAST_MSMT), LAST_MSMT := "N"]

  sampvisits[,':='(sa_ref = PROJ_AGE_1,
                   year_ref = as.numeric(substr(PROJECTED_DATE, 1, 4)))]
  sampvisits[, PROJ_AGE_ADJ := sa_ref - (year_ref - MEAS_YR)]

  sampvisits_first <- sampvisits[FIRST_MSMT == "Y",
                                 .(SITE_IDENTIFIER, SAMPLE_ESTABLISHMENT_TYPE,
                                   PROJ_AGE_ADJ)]
  sampvisits_first[SAMPLE_ESTABLISHMENT_TYPE %in% c("CMI", "NFI", "SUP") &
                     PROJ_AGE_ADJ > 50,
                   MAT_MAIN_FM := "Y"]
  sampvisits_first[is.na(MAT_MAIN_FM),
                   MAT_MAIN_FM := "N"]

  sampvisits_first[SAMPLE_ESTABLISHMENT_TYPE %in% c("CMI", "NFI", "YSM") &
                     PROJ_AGE_ADJ <= 50 & PROJ_AGE_ADJ >= 15,
                   YSM_MAIN_FM := "Y"]
  sampvisits_first[is.na(YSM_MAIN_FM),
                   YSM_MAIN_FM := "N"]

  sampvisits_first[SAMPLE_ESTABLISHMENT_TYPE == "YNS" &
                     PROJ_AGE_ADJ <= 50 & PROJ_AGE_ADJ >= 15,
                   YSM_PILOT_FM := "Y"]
  sampvisits_first[is.na(YSM_PILOT_FM),
                   YSM_PILOT_FM := "N"]

  sampvisits <- merge(sampvisits,
                      sampvisits_first[,.(SITE_IDENTIFIER, MAT_MAIN_FM, YSM_MAIN_FM, YSM_PILOT_FM)],
                      by = "SITE_IDENTIFIER",
                      all.x = TRUE)

  sampvisits_last <- sampvisits[LAST_MSMT == "Y",
                                .(SITE_IDENTIFIER, SAMPLE_ESTABLISHMENT_TYPE,
                                  PROJ_AGE_ADJ)]
  sampvisits_last[SAMPLE_ESTABLISHMENT_TYPE %in% c("CMI", "NFI", "SUP") &
                    PROJ_AGE_ADJ > 50,
                  MAT_MAIN_LM := "Y"]
  sampvisits_last[is.na(MAT_MAIN_LM),
                  MAT_MAIN_LM := "N"]

  sampvisits_last[SAMPLE_ESTABLISHMENT_TYPE %in% c("CMI", "NFI", "YSM") &
                    PROJ_AGE_ADJ <= 50 & PROJ_AGE_ADJ >= 15,
                  YSM_MAIN_LM := "Y"]
  sampvisits_last[is.na(YSM_MAIN_LM),
                  YSM_MAIN_LM := "N"]
  sampvisits_last[SAMPLE_ESTABLISHMENT_TYPE == "YNS" &
                    PROJ_AGE_ADJ <= 50 & PROJ_AGE_ADJ >= 15,
                  YSM_PILOT_LM := "Y"]
  sampvisits_last[is.na(YSM_PILOT_LM),
                  YSM_PILOT_LM := "N"]

  sampvisits <- merge(sampvisits,
                      sampvisits_last[,.(SITE_IDENTIFIER, MAT_MAIN_LM, YSM_MAIN_LM, YSM_PILOT_LM)],
                      by = "SITE_IDENTIFIER",
                      all.x = TRUE)

  faib_sample_byvisit <- sampvisits[,.(CLSTR_ID, SITE_IDENTIFIER, SAMPLE_SITE_NAME,
                                       VISIT_NUMBER, FIRST_MSMT, LAST_MSMT,
                                       MEAS_DT, MEAS_YR, NO_PLOTS, PROJ_AGE_1, PROJECTED_DATE,
                                       PROJ_AGE_ADJ,
                                       SAMP_TYP, SAMPLE_SITE_PURPOSE_TYPE_CODE,
                                       SAMPLE_ESTABLISHMENT_TYPE,
                                       MAT_MAIN_FM, MAT_MAIN_LM,
                                       YSM_MAIN_FM, YSM_MAIN_LM,
                                       YSM_PILOT_FM, YSM_PILOT_LM)]
  write.csv(faib_sample_byvisit,
            file.path(compilationPaths$compilation_publish,
                      "faib_sample_byvisit.rds"),
            row.names = FALSE,
            na = "")

  volsmry <- readRDS(file.path(compilationPaths$compilation_db,
                               "Smries_volume_byCL.rds"))
  volsmry <- merge(volsmry,
                   sampvisits[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER)],
                   by = "CLSTR_ID",
                   all.x = TRUE)

  spcomp <- readRDS(file.path(compilationPaths$compilation_db,
                              "Smries_speciesComposition_byCL.rds"))

  volsmry <- merge(volsmry,
                   spcomp,
                   by = c("CLSTR_ID", "UTIL"),
                   all.x = TRUE)
  htsmry <- readRDS(file.path(compilationPaths$compilation_db,
                              "Smries_height_byCL.rds"))
  volsmry <- merge(volsmry,
                   htsmry,
                   by = c("CLSTR_ID"),
                   all.x = TRUE)

  faib_compiled_smeries <- volsmry[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, UTIL,
                                      BA_HA_DF, BA_HA_DS, BA_HA_LF, BA_HA_LS,
                                      STEMS_HA_DF, STEMS_HA_DS, STEMS_HA_LF, STEMS_HA_LS,
                                      VHA_MER_DF, VHA_MER_DS, VHA_MER_LF, VHA_MER_LS,
                                      VHA_WSV_DF, VHA_WSV_DS, VHA_WSV_LF, VHA_WSV_LS,
                                      VHA_NTWB_DF, VHA_NTWB_DS, VHA_NTWB_LF, VHA_NTWB_LS,
                                      VHA_NTWB_NVAF_DS, VHA_NTWB_NVAF_LS,
                                      QMD_DS, QMD_LS, SPB_CPCT_LS,
                                      HT_LRY1, HT_LRY2, HT_LRYALL,
                                      HT_MEAN1, HT_MEAN2, HT_MNALL)]

  write.csv(faib_compiled_smeries,
            file.path(compilationPaths$compilation_publish,
                      "faib_compiled_smeries.rds"),
            row.names = FALSE,
            na = "")

  volsmry_sp <- readRDS(file.path(compilationPaths$compilation_db,
                                  "Smries_volume_byCLSP.rds"))

  volsmry_sp <- merge(volsmry_sp,
                      sampvisits[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER)],
                      by = "CLSTR_ID",
                      all.x = TRUE)
  agesmry_sp <- readRDS(file.path(compilationPaths$compilation_db,
                                  "Smries_siteAge_byCLSP.rds"))

  volsmry_sp <- merge(volsmry_sp,
                      agesmry_sp,
                      by = c("CLSTR_ID", "SPECIES"),
                      all.x = TRUE)

  faib_compiled_spcsmries <- volsmry_sp[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER,
                                           UTIL, SPECIES,
                                           BA_HA_DF, BA_HA_DS, BA_HA_LF, BA_HA_LS,
                                           STEMS_HA_DF, STEMS_HA_DS, STEMS_HA_LF, STEMS_HA_LS,
                                           VHA_MER_DF, VHA_MER_DS, VHA_MER_LF, VHA_MER_LS,
                                           VHA_NTWB_DF, VHA_NTWB_DS, VHA_NTWB_LF, VHA_NTWB_LS,
                                           VHA_NTWB_NVAF_DS, VHA_NTWB_NVAF_LS,
                                           VHA_WSV_DF, VHA_WSV_DS, VHA_WSV_LF, VHA_WSV_LS,
                                           NVAF_L, NVAF_D, SP_PCT_BA_LS,
                                           QMD_DS, QMD_LS,
                                           AGEB_TLSO, AGET_TLSO, HT_TLSO,
                                           SI_M_TLSO, N_AG_TLSO, N_HT_TLSO)]
  write.csv(faib_compiled_spcsmries,
            file.path(compilationPaths$compilation_publish,
                      "faib_compiled_spcsmries.rds"),
            row.names = FALSE,
            na = "")

  treemsmt <- readRDS(file.path(compilationPaths$compilation_sa,
                                "treemeasurements.rds"))

  treemsmt[DIAMETER_MEASMT_HEIGHT %==% 1.3 &
             !is.na(DIAMETER),
           DBH := DIAMETER]
  treemsmt[!is.na(LENGTH),
           HEIGHT := LENGTH]
  treemsmt[!is.na(TREE_STANCE_CODE),
           TREE_STANCE_CODE := "S"]


  faib_tree_detail <- treemsmt[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, PLOT, TREE_NO = TREE_NUMBER,
                                  SPECIES, DBH, HEIGHT,
                                  LV_D = TREE_EXTANT_CODE,
                                  LVD_EDIT, DIAMETER_EDIT,
                                  HEIGHT_EDIT, MSMT_MISSING_EDIT,
                                  BTOP_EDIT, CRCL_EDIT, SP_EDIT, DIAM_MSMT_HT_EDIT,
                                  S_F = TREE_STANCE_CODE,
                                  OUT_OF_PLOT_IND,
                                  BROKEN_TOP_IND, CR_CL = CROWN_CLASS_CODE,
                                  HT_BRCH = HEIGHT_TO_LIVE_CROWN,
                                  RESIDUAL = RESIDUAL_IND,
                                  SECTOR = TAGGING_SECTOR_NUMBER,
                                  WALKTHRU_STATUS = CMI_WALKTHROUGH_CODE,
                                  AZIMUTH = STEM_MAP_BEARING,
                                  DISTANCE = STEM_MAP_DISTANCE)]

  treelist <- readRDS(file.path(compilationPaths$compilation_db,
                                "treelist.rds"))
  faib_tree_detail <- merge(faib_tree_detail,
                            treelist[,.(CLSTR_ID, PLOT, TREE_NO,
                                        MEAS_INTENSE, PHF_TREE, TREE_WT,
                                        BA_TREE, VOL_WSV, VOL_MER, VOL_NTWB)],
                            by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                            all.x = TRUE)

  vih <- readRDS(file.path(compilationPaths$compilation_db,
                           "compiled_vi_h.rds"))

  faib_tree_detail <- merge(faib_tree_detail,
                            vih[,.(CLSTR_ID, PLOT, TREE_NO, AGE_BH,
                                   AGE_TOT, BORED_AGE_FINAL, BORED_HT, BORED_AGE_SOURCE,
                                   SI_TREE, SUIT_TR, SUIT_HT, SUIT_SI, RA_TREE, TH_TREE)],
                            by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                            all.x = TRUE)

  vid <- readRDS(file.path(compilationPaths$compilation_db,
                           "compiled_vi_d.rds"))

  faib_tree_detail <- merge(faib_tree_detail,
                            vid[,.(CLSTR_ID, PLOT, TREE_NO,
                                   DAM_AGNA, DAM_AGNB, DAM_AGNC, DAM_AGND, DAM_AGNE,
                                   SEV_A, SEV_B, SEV_C, SEV_D, SEV_E)],
                            by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                            all.x = TRUE)

  compchange <- readRDS(file.path(compilationPaths$compilation_db,
                                  "component_change_treelevel.rds"))
  faib_tree_detail <- merge(faib_tree_detail,
                            compchange[,.(SITE_IDENTIFIER, VISIT_NUMBER, PLOT, TREE_NO,
                                          COMP_CHG = COMPONENT_CHANGE)],
                            by = c("SITE_IDENTIFIER", "VISIT_NUMBER", "PLOT", "TREE_NO"),
                            all.x = TRUE)
  faib_tree_detail <- merge(faib_tree_detail,
                            sampvisits[,.(CLSTR_ID, PERIOD)],
                            by = "CLSTR_ID",
                            all.x = TRUE)
  faib_tree_detail[COMP_CHG == "I",
                   PERIOD := 0]
  faib_tree_detail[OUT_OF_PLOT_IND == "Y",
                   MEAS_INTENSE := "OUT_OF_PLOT"]

  sampplot <- readRDS(file.path(compilationPaths$compilation_db,
                                "sample_plot_header.rds"))
  faib_tree_detail <- merge(faib_tree_detail,
                            sampplot[,.(CLSTR_ID, PLOT, PLOT_WT)],
                            by = c("CLSTR_ID", "PLOT"),
                            all.x = TRUE)
  faib_tree_detail[is.na(MEAS_INTENSE),
                   MEAS_INTENSE := "OUT_OF_PLOT"]

  faib_tree_detail[, OUT_OF_PLOT_IND := NULL]
  # there still two variables missing x y coord

  write.csv(faib_tree_detail,
            file.path(compilationPaths$compilation_publish,
                      "faib_tree_detail.rds"),
            row.names = FALSE,
            na = "")

}
