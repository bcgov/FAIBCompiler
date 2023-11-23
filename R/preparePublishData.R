#' Prepare the compiled data for publish
#' @description To prepare the compiled data for publish, currently supporting nonPSP part
#'
#' @param compilationPath character, The path to the compiled PSP data, which is configured
#'                        from \code{ISMCCompiler}.
#' @param publishPath character, The path to save prepared data.
#' @param compilationType character, Specifies the compilation type either \code{nonPSP} or code{PSP}.
#' @return no value returned. Instead, all the files will be saved into the \code{publishPath} including a readme file.
#' @importFrom data.table ':='
#' @importFrom openxlsx write.xlsx
#' @note The compilationPath must have all the outputs from \code{ISMCCompiler}.
#'
#'
#' @export preparePublishData
#' @docType methods
#' @rdname preparePublishData
#'
#' @author Yong Luo
preparePublishData <- function(compilationPath,
                               publishPath,
                               compilationType){
  if(dir.exists(publishPath)){
    unlink(publishPath, recursive = TRUE)
  }
  dir.create(publishPath)
  datadictionary_all <- read.xlsx(file.path(compilationPath,
                                            "compilation_coeff",
                                            "data_dictionary_master.xlsx")) %>%
    data.table
  datadictionary_all <- datadictionary_all[,.(Attribute = ColumnName,
                                              Description)]
  datadictionary_publish <- list()

  sampsites_org <- readRDS(file.path(compilationPath,
                                     paste0("compilation_", compilationType, "_db"),
                                     "sample_site_header.rds"))

  faib_header <- sampsites_org[,.(SITE_IDENTIFIER, TOTAL_PERIOD,
                                  BC_ALBERS_X, BC_ALBERS_Y, BEC_ZONE, BEC_SBZ,
                                  BEC_VAR, FIZ, IP_UTM, IP_EAST, IP_NRTH, Latitude, Longitude,
                                  OWNER, SCHEDULE,
                                  SAMPLE_ESTABLISHMENT_TYPE,
                                  TFL, TSA = as.numeric(TSA),
                                  TSA_DESC, MGMT_UNIT,
                                  OWN_SCHED_DESCRIP = OWNERSHIP_DESCRIPTION)]
  faib_header[, MGMT_UNIT := gsub("QueenCharlotte", "HaidaGwaii", MGMT_UNIT)]
  if(compilationType == "nonPSP"){ # only nonPSP needs site grid information
    gridlookup <- read.xlsx(file.path(compilationPath,
                                      "compilation_coeff",
                                      paste0(compilationType, "_site_grid.xlsx"))) %>%
      data.table
    # first priority goes to tfl
    gridlookup_tfl <- gridlookup[!is.na(TFL) & TFL != "All",
                                 .(SAMPLE_ESTABLISHMENT_TYPE, TFL,
                                   GRID_BASE, GRID_SIZE,
                                   grid_fill = "y")]
    faib_header <- merge(faib_header,
                         gridlookup_tfl,
                         by = c("SAMPLE_ESTABLISHMENT_TYPE", "TFL"),
                         all.x = TRUE)
    # second priority goes to tsa
    gridlookup_tsa <- gridlookup[!is.na(TSA) & TSA != "All",
                                 .(SAMPLE_ESTABLISHMENT_TYPE,
                                   TSA = as.numeric(TSA),
                                   GRID_BASE_tsa = GRID_BASE,
                                   GRID_SIZE_tsa = GRID_SIZE,
                                   grid_fill_tsa = "y")]
    faib_header <- merge(faib_header,
                         gridlookup_tsa,
                         by = c("SAMPLE_ESTABLISHMENT_TYPE", "TSA"),
                         all.x = TRUE)
    faib_header[is.na(grid_fill) & grid_fill_tsa == "y",
                ':='(GRID_BASE = GRID_BASE_tsa,
                     GRID_SIZE = GRID_SIZE_tsa,
                     grid_fill = grid_fill_tsa)]
    faib_header[,':='(GRID_BASE_tsa = NULL,
                      GRID_SIZE_tsa = NULL,
                      grid_fill_tsa = NULL)]
    # last priority goes to est type
    gridlookup_est <- gridlookup[TSA == "All" & TFL == "All",
                                 .(SAMPLE_ESTABLISHMENT_TYPE,
                                   GRID_BASE_est = GRID_BASE,
                                   GRID_SIZE_est = GRID_SIZE,
                                   grid_fill_est = "y")]
    faib_header <- merge(faib_header,
                         gridlookup_est,
                         by = c("SAMPLE_ESTABLISHMENT_TYPE"),
                         all.x = TRUE)
    faib_header[is.na(grid_fill) & grid_fill_est == "y",
                ':='(GRID_BASE = GRID_BASE_est,
                     GRID_SIZE = GRID_SIZE_est,
                     grid_fill = grid_fill_est)]
    faib_header[,':='(GRID_BASE_est = NULL,
                      GRID_SIZE_est = NULL,
                      grid_fill_est = NULL)]
    # for the else
    faib_header[is.na(grid_fill),
                ':='(GRID_BASE = "UNKN",
                     GRID_SIZE = "UNKN")]
    faib_header[, grid_fill := NULL]
  } else {
    faib_header[,
                ':='(GRID_BASE = as.character(NA),
                     GRID_SIZE = as.character(NA))]
  }

  sampvisits <- readRDS(file.path(compilationPath,
                                  paste0("compilation_", compilationType, "_db"),
                                  "sample_msmt_header.rds"))
  # remove N samples
  sampvisits <- sampvisits[SAMPLE_SITE_PURPOSE_TYPE_CODE != "N"]
  # remove B samples in the proj_id = "KOL1"
  sampvisits <- sampvisits[!(SAMPLE_SITE_PURPOSE_TYPE_CODE == "B" & PROJ_ID == "KOL1")]
  # remove C samples which are course woody debris samples
  sampvisits <- sampvisits[SAMPLE_SITE_PURPOSE_TYPE_CODE != "C"]


  faib_header <- faib_header[SITE_IDENTIFIER %in% sampvisits$SITE_IDENTIFIER,]
  write.csv(faib_header,
            file.path(publishPath,
                      "faib_header.csv"),
            row.names = FALSE,
            na = "")
  write.xlsx(faib_header,
             file.path(publishPath,
                       "faib_header.xlsx"))
  faib_header_dic <- data.table(Attribute = names(faib_header))
  faib_header_dic <- merge(faib_header_dic,
                           datadictionary_all,
                           by = "Attribute",
                           all.x = TRUE)
  datadictionary_publish[["faib_header"]] <- faib_header_dic

  # reassign first_msmt and last_msmt
  sampvisits[, ':='(minVisit = min(VISIT_NUMBER),
                    maxVisit = max(VISIT_NUMBER)),
             by = "SITE_IDENTIFIER"]
  sampvisits[,':='(FIRST_MSMT = "N",
                   LAST_MSMT = "N")]
  sampvisits[VISIT_NUMBER == minVisit,
             FIRST_MSMT := "Y"]
  sampvisits[VISIT_NUMBER == maxVisit,
             LAST_MSMT := "Y"]
  sampvisits[,':='(minVisit = NULL,
                   maxVisit = NULL)]

  sampvisits <- merge(sampvisits,
                      sampsites_org[,.(SITE_IDENTIFIER,
                                       SAMPLE_SITE_NAME, SAMPLE_ESTABLISHMENT_TYPE)],
                      by = "SITE_IDENTIFIER",
                      all.x = TRUE)
  sampvisits_first <- sampvisits[FIRST_MSMT == "Y",
                                 .(SITE_IDENTIFIER, SAMPLE_ESTABLISHMENT_TYPE,
                                   SA_VEGCOMP)]
  sampvisits_first[SAMPLE_ESTABLISHMENT_TYPE %in% c("CMI", "NFI", "SUP", "VRI") &
                     SA_VEGCOMP > 50,
                   MAT_MAIN_FM := "Y"]
  sampvisits_first[is.na(MAT_MAIN_FM),
                   MAT_MAIN_FM := "N"]

  sampvisits_first[SAMPLE_ESTABLISHMENT_TYPE %in% c("CMI", "NFI", "YSM") &
                     SA_VEGCOMP <= 50 & SA_VEGCOMP >= 15,
                   YSM_MAIN_FM := "Y"]
  sampvisits_first[is.na(YSM_MAIN_FM),
                   YSM_MAIN_FM := "N"]

  sampvisits_first[SAMPLE_ESTABLISHMENT_TYPE == "YNS" &
                     SA_VEGCOMP <= 50 & SA_VEGCOMP >= 15,
                   YSM_PILOT_FM := "Y"]
  sampvisits_first[is.na(YSM_PILOT_FM),
                   YSM_PILOT_FM := "N"]

  sampvisits <- merge(sampvisits,
                      sampvisits_first[,.(SITE_IDENTIFIER, MAT_MAIN_FM, YSM_MAIN_FM, YSM_PILOT_FM)],
                      by = "SITE_IDENTIFIER",
                      all.x = TRUE)

  sampvisits_last <- sampvisits[LAST_MSMT == "Y",
                                .(SITE_IDENTIFIER, SAMPLE_ESTABLISHMENT_TYPE,
                                  SA_VEGCOMP)]
  sampvisits_last[SAMPLE_ESTABLISHMENT_TYPE %in% c("CMI", "NFI", "SUP", "VRI") &
                    SA_VEGCOMP > 50,
                  MAT_MAIN_LM := "Y"]
  sampvisits_last[is.na(MAT_MAIN_LM),
                  MAT_MAIN_LM := "N"]

  sampvisits_last[SAMPLE_ESTABLISHMENT_TYPE %in% c("CMI", "NFI", "YSM") &
                    SA_VEGCOMP <= 50 & SA_VEGCOMP >= 15,
                  YSM_MAIN_LM := "Y"]
  sampvisits_last[is.na(YSM_MAIN_LM),
                  YSM_MAIN_LM := "N"]
  sampvisits_last[SAMPLE_ESTABLISHMENT_TYPE == "YNS" &
                    SA_VEGCOMP <= 50 & SA_VEGCOMP >= 15,
                  YSM_PILOT_LM := "Y"]
  sampvisits_last[is.na(YSM_PILOT_LM),
                  YSM_PILOT_LM := "N"]

  sampvisits <- merge(sampvisits,
                      sampvisits_last[,.(SITE_IDENTIFIER, MAT_MAIN_LM, YSM_MAIN_LM, YSM_PILOT_LM)],
                      by = "SITE_IDENTIFIER",
                      all.x = TRUE)
  sampvisits[SA_VEGCOMP < 0.1, SA_VEGCOMP := NA]
  faib_sample_byvisit <- sampvisits[,.(CLSTR_ID, SITE_IDENTIFIER, SAMPLE_SITE_NAME,
                                       VISIT_NUMBER, FIRST_MSMT, LAST_MSMT,
                                       MEAS_DT, MEAS_YR, PERIOD, NO_PLOTS, PROJ_AGE_1, PROJECTED_DATE,
                                       PROJ_AGE_ADJ = SA_VEGCOMP,
                                       SAMP_TYP, SAMPLE_SITE_PURPOSE_TYPE_CODE,
                                       SAMPLE_ESTABLISHMENT_TYPE,
                                       MAT_MAIN_FM, MAT_MAIN_LM,
                                       YSM_MAIN_FM, YSM_MAIN_LM,
                                       YSM_PILOT_FM, YSM_PILOT_LM)]
  write.csv(faib_sample_byvisit,
            file.path(publishPath,
                      "faib_sample_byvisit.csv"),
            row.names = FALSE,
            na = "")
  write.xlsx(faib_sample_byvisit,
             file.path(publishPath,
                       "faib_sample_byvisit.xlsx"))
  faib_sample_byvisit_dic <- data.table(Attribute = names(faib_sample_byvisit))
  faib_sample_byvisit_dic <- merge(faib_sample_byvisit_dic,
                                   datadictionary_all,
                                   by = "Attribute",
                                   all.x = TRUE)
  datadictionary_publish[["faib_sample_byvisit"]] <- faib_sample_byvisit_dic


  volsmry <- readRDS(file.path(compilationPath,
                               paste0("compilation_", compilationType, "_db"),
                               "Smries_volume_byCL.rds"))
  volsmry <- merge(volsmry,
                   sampvisits[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER)],
                   by = "CLSTR_ID",
                   all.x = TRUE)

  spcomp <- readRDS(file.path(compilationPath,
                              paste0("compilation_", compilationType, "_db"),
                              "Smries_speciesComposition_byCL.rds"))

  volsmry <- merge(volsmry,
                   spcomp,
                   by = c("CLSTR_ID", "UTIL"),
                   all.x = TRUE)
  if(compilationType == "nonPSP"){

    faib_compiled_smeries <- volsmry[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, UTIL,
                                        BA_HA_DF, BA_HA_DS, BA_HA_LF, BA_HA_LS,
                                        STEMS_HA_DF, STEMS_HA_DS, STEMS_HA_LF, STEMS_HA_LS,
                                        VHA_MER_DF, VHA_MER_DS, VHA_MER_LF, VHA_MER_LS,
                                        VHA_WSV_DF, VHA_WSV_DS, VHA_WSV_LF, VHA_WSV_LS,
                                        VHA_NTWB_DF, VHA_NTWB_DS, VHA_NTWB_LF, VHA_NTWB_LS,
                                        VHA_NTWB_NVAF_DS, VHA_NTWB_NVAF_LS,
                                        QMD_DS, QMD_LS, SPB_CPCT_LS)]
  } else {
    faib_compiled_smeries <- volsmry[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, UTIL,
                                        BA_HA_LIV, BA_HA_L, BA_HA_I, BA_HA_V, BA_HA_D,
                                        STEMS_HA_LIV, STEMS_HA_L, STEMS_HA_I, STEMS_HA_V,
                                        VHA_WSV_LIV, VHA_WSV_L, VHA_WSV_I, VHA_WSV_V, VHA_WSV_D,
                                        VHA_MER_LIV, VHA_MER_L, VHA_MER_I, VHA_MER_V, VHA_MER_D,
                                        VHA_DWB_LIV, VHA_DWB_L, VHA_DWB_I, VHA_DWB_V, VHA_DWB_D,
                                        QMD_LIV, QMD_L, QMD_I, QMD_V,
                                        SPB_CPCT_LIV, SPB_CPCT_LI)]

  }
  write.csv(faib_compiled_smeries,
            file.path(publishPath,
                      "faib_compiled_smeries.csv"),
            row.names = FALSE,
            na = "")
  write.xlsx(faib_compiled_smeries,
             file.path(publishPath,
                       "faib_compiled_smeries.xlsx"))

  faib_compiled_smeries_dic <- data.table(Attribute = names(faib_compiled_smeries))
  faib_compiled_smeries_dic <- merge(faib_compiled_smeries_dic,
                                     datadictionary_all,
                                     by = "Attribute",
                                     all.x = TRUE)
  datadictionary_publish[["faib_compiled_smeries"]] <- faib_compiled_smeries_dic

  htsmry <- readRDS(file.path(compilationPath,
                              paste0("compilation_", compilationType, "_db"),
                              "Smries_height_byCL.rds"))
  if(compilationType == "nonPSP"){

    faib_compiled_ht_smeries <- htsmry[,.(CLSTR_ID, HT_LRY1, HT_LRY2, HT_LRYALL,
                                          HT_MEAN1, HT_MEAN2, HT_MNALL)]
  } else {
    faib_compiled_ht_smeries <- data.table::copy(htsmry)
  }

  write.csv(faib_compiled_ht_smeries,
            file.path(publishPath,
                      "faib_compiled_smeries_ht.csv"),
            row.names = FALSE,
            na = "")
  write.xlsx(faib_compiled_ht_smeries,
             file.path(publishPath,
                       "faib_compiled_smeries_ht.xlsx"))

  faib_compiled_ht_smeries_dic <- data.table(Attribute = names(faib_compiled_ht_smeries))
  faib_compiled_ht_smeries_dic <- merge(faib_compiled_ht_smeries_dic,
                                        datadictionary_all,
                                        by = "Attribute",
                                        all.x = TRUE)
  datadictionary_publish[["faib_compiled_smeries_ht"]] <- faib_compiled_ht_smeries_dic

  volsmry_sp <- readRDS(file.path(compilationPath,
                                  paste0("compilation_", compilationType, "_db"),
                                  "Smries_volume_byCLSP.rds"))

  volsmry_sp <- merge(volsmry_sp,
                      sampvisits[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER)],
                      by = "CLSTR_ID",
                      all.x = TRUE)

  if(compilationType == "nonPSP"){
    faib_compiled_spcsmries <- volsmry_sp[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER,
                                             UTIL, SPECIES,
                                             BA_HA_DF, BA_HA_DS, BA_HA_LF, BA_HA_LS,
                                             STEMS_HA_DF, STEMS_HA_DS, STEMS_HA_LF, STEMS_HA_LS,
                                             VHA_MER_DF, VHA_MER_DS, VHA_MER_LF, VHA_MER_LS,
                                             VHA_NTWB_DF, VHA_NTWB_DS, VHA_NTWB_LF, VHA_NTWB_LS,
                                             VHA_NTWB_NVAF_DS, VHA_NTWB_NVAF_LS,
                                             VHA_WSV_DF, VHA_WSV_DS, VHA_WSV_LF, VHA_WSV_LS,
                                             NVAF_L, NVAF_D, SP_PCT_BA_LS,
                                             QMD_DS, QMD_LS)]
  } else {
    faib_compiled_spcsmries <- volsmry_sp[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, UTIL, SPECIES,
                                             BA_HA_LIV, BA_HA_L, BA_HA_I, BA_HA_V, BA_HA_D,
                                             STEMS_HA_LIV, STEMS_HA_L, STEMS_HA_I, STEMS_HA_V,
                                             VHA_WSV_LIV, VHA_WSV_L, VHA_WSV_I, VHA_WSV_V, VHA_WSV_D,
                                             VHA_MER_LIV, VHA_MER_L, VHA_MER_I, VHA_MER_V, VHA_MER_D,
                                             VHA_DWB_LIV, VHA_DWB_L, VHA_DWB_I, VHA_DWB_V, VHA_DWB_D,
                                             QMD_LIV, QMD_L, QMD_I, QMD_V,
                                             SP_PCT_BA_LI)]
  }
  write.csv(faib_compiled_spcsmries,
            file.path(publishPath,
                      "faib_compiled_spcsmries.csv"),
            row.names = FALSE,
            na = "")
  write.xlsx(faib_compiled_spcsmries,
             file.path(publishPath,
                       "faib_compiled_spcsmries.xlsx"))
  faib_compiled_spcsmries_dic <- data.table(Attribute = names(faib_compiled_spcsmries))
  faib_compiled_spcsmries_dic <- merge(faib_compiled_spcsmries_dic,
                                       datadictionary_all,
                                       by = "Attribute",
                                       all.x = TRUE)
  datadictionary_publish[["faib_compiled_spcsmries"]] <- faib_compiled_spcsmries_dic


  agesmry_sp <- readRDS(file.path(compilationPath,
                                  paste0("compilation_", compilationType, "_db"),
                                  "Smries_siteAge_byCLSP.rds"))


  if(compilationType == "nonPSP"){

    faib_compiled_siteage_spcsmries <- agesmry_sp[,.(CLSTR_ID, SPECIES, AGEB_TLSO, AGET_TLSO, HT_TLSO,
                                                     SI_M_TLSO, N_AG_TLSO, N_HT_TLSO)]
  } else {
    faib_compiled_siteage_spcsmries <- agesmry_sp[,.(CLSTR_ID, SPECIES,
                                                     AGE_BH1, AGE_BH2, AGE_BH3,
                                                     AGE_TOT1, AGE_TOT2, AGE_TOT3,
                                                     HTOP1, HTOP2, HTOP3,
                                                     SI1, SI2, SI3)]

  }
  write.csv(faib_compiled_siteage_spcsmries,
            file.path(publishPath,
                      "faib_compiled_spcsmries_siteage.csv"),
            row.names = FALSE,
            na = "")
  write.xlsx(faib_compiled_siteage_spcsmries,
             file.path(publishPath,
                       "faib_compiled_spcsmries_siteage.xlsx"))
  faib_compiled_siteage_spcsmries_dic <- data.table(Attribute = names(faib_compiled_siteage_spcsmries))
  faib_compiled_siteage_spcsmries_dic <- merge(faib_compiled_siteage_spcsmries_dic,
                                               datadictionary_all,
                                               by = "Attribute",
                                               all.x = TRUE)
  datadictionary_publish[["faib_compiled_spcsmries_siteage"]] <- faib_compiled_siteage_spcsmries_dic


  treemsmt <- readRDS(file.path(compilationPath,
                                paste0("compilation_", compilationType, "_sa"),
                                "treemeasurements.rds"))

  treemsmt[DIAMETER_MEASMT_HEIGHT %==% 1.3 &
             !is.na(DIAMETER),
           DBH := DIAMETER]
  treemsmt[!is.na(LENGTH),
           HEIGHT := LENGTH]
  treemsmt[is.na(TREE_STANCE_CODE),
           TREE_STANCE_CODE := "S"]
  faib_tree_detail <- treemsmt[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, PLOT, TREE_NO = TREE_NUMBER,
                                  SPECIES = TREE_SPECIES_CODE,
                                  DBH, HEIGHT,
                                  LV_D = TREE_EXTANT_CODE,
                                  S_F = TREE_STANCE_CODE,
                                  OUT_OF_PLOT_IND,
                                  BROKEN_TOP_IND, CR_CL = CROWN_CLASS_CODE,
                                  HT_BRCH = HEIGHT_TO_LIVE_CROWN,
                                  RESIDUAL = RESIDUAL_IND,
                                  SECTOR = TAGGING_SECTOR_NUMBER,
                                  WALKTHRU_STATUS = CMI_WALKTHROUGH_CODE,
                                  AZIMUTH = STEM_MAP_BEARING,
                                  DISTANCE = STEM_MAP_DISTANCE)]

  treelist <- readRDS(file.path(compilationPath,
                                paste0("compilation_", compilationType, "_db"),
                                "treelist.rds"))
  if(compilationType == "nonPSP"){

    faib_tree_detail <- merge(faib_tree_detail,
                              treelist[,.(CLSTR_ID, PLOT, TREE_NO,
                                          MEAS_INTENSE, PHF_TREE, TREE_WT,
                                          BA_TREE, VOL_WSV, VOL_MER, VOL_NTWB)],
                              by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                              all.x = TRUE)
  } else {
    faib_tree_detail <- merge(faib_tree_detail,
                              treelist[,.(CLSTR_ID, PLOT, TREE_NO,
                                          MEAS_INTENSE,
                                          HT_TOTAL,
                                          HEIGHT_SOURCE = HT_TOTAL_SOURCE,
                                          PHF_TREE, TREE_WT,
                                          BA_TREE, VOL_WSV, VOL_MER,
                                          VOL_DWB)],
                              by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                              all.x = TRUE)
    faib_tree_detail[, HEIGHT := HT_TOTAL]
    faib_tree_detail[, HT_TOTAL := NULL]
  }

  vih <- readRDS(file.path(compilationPath,
                           paste0("compilation_", compilationType, "_db"),
                           "compiled_vi_h.rds"))

  faib_tree_detail <- merge(faib_tree_detail,
                            vih[,.(CLSTR_ID, PLOT, TREE_NO, AGE_BH,
                                   AGE_TOT, BORED_AGE_FINAL, BORED_HT, BORED_AGE_SOURCE,
                                   SI_TREE, SUIT_TR, SUIT_HT, SUIT_SI, RA_TREE, TH_TREE)],
                            by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                            all.x = TRUE)

  vid <- readRDS(file.path(compilationPath,
                           paste0("compilation_", compilationType, "_db"),
                           "compiled_vi_d.rds"))

  faib_tree_detail <- merge(faib_tree_detail,
                            vid[,.(CLSTR_ID, PLOT, TREE_NO,
                                   DAM_AGNA, DAM_AGNB, DAM_AGNC, DAM_AGND, DAM_AGNE,
                                   SEV_A, SEV_B, SEV_C, SEV_D, SEV_E)],
                            by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                            all.x = TRUE)

  compchange <- readRDS(file.path(compilationPath,
                                  paste0("compilation_", compilationType, "_db"),
                                  "component_change_treelevel.rds"))
  faib_tree_detail <- merge(faib_tree_detail,
                            compchange[,.(SITE_IDENTIFIER, VISIT_NUMBER, PLOT, TREE_NO,
                                          COMP_CHG = COMPONENT_CHANGE)],
                            by = c("SITE_IDENTIFIER", "VISIT_NUMBER", "PLOT", "TREE_NO"),
                            all.x = TRUE)

  faib_tree_detail[OUT_OF_PLOT_IND == "Y",
                   MEAS_INTENSE := "OUT_OF_PLOT"]

  sampplot <- readRDS(file.path(compilationPath,
                                paste0("compilation_", compilationType, "_db"),
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
            file.path(publishPath,
                      "faib_tree_detail.csv"),
            row.names = FALSE,
            na = "")
  if(compilationType == "nonPSP"){
    write.xlsx(faib_tree_detail,
               file.path(publishPath,
                         "faib_tree_detail.xlsx"))
  } else {
    write.table(faib_tree_detail,
                file.path(publishPath,
                          "faib_tree_detail.txt"),
                row.names = FALSE,
                na = "")
  }

  faib_tree_detail_dic <- data.table(Attribute = names(faib_tree_detail))
  faib_tree_detail_dic <- merge(faib_tree_detail_dic,
                                datadictionary_all,
                                by = "Attribute",
                                all.x = TRUE)
  datadictionary_publish[["faib_tree_detail"]] <- faib_tree_detail_dic

  write.xlsx(datadictionary_publish,
             file.path(publishPath, "data_dictionary.xlsx"))
  ## prepare readme file
  compilation_raw <- file.path(compilationPath,
                               paste0("compilation_", compilationType, "_raw"))
  downloadtime <- dir(compilation_raw, pattern = "AccessNotes.rds")
  downloadtime <- paste0("Raw tree/sample data were downloaded from ISMC at ",
                         substr(downloadtime, 11, 14),
                         "-", substr(downloadtime, 15, 16),
                         "-", substr(downloadtime, 17, 18),
                         "-", substr(downloadtime, 20, 23),
                         ".")
  compiledate <- as.character(file.info(file.path(compilationPath,
                                                  paste0("compilation_", compilationType, "_db"),
                                                  "sample_site_header.rds"))$mtime)
  compiledate <- paste0("Data were compiled on ", substr(compiledate, 1, 10),
                        " using FAIBCompiler package at https://github.com/bcgov/FAIBCompiler.")
  compilation_map <- file.path(compilationPath,
                               paste0("compilation_map"))
  mapinfor <- readRDS(file.path(compilation_map,
                                paste0("spatiallookup_", compilationType, ".rds")))
  mapinfor <- mapinfor$mapsource

  mapinfor[, mapName := unlist(lapply(mapFile, function(s)unlist(strsplit(s, "_map"))[1]))]
  TSA_id <- "8daa29da-d7f4-401c-83ae-d962e3a28980"
  BEC_id <- "f358a53b-ffde-4830-a325-a5a03ff672c3"
  TFL_id <- "454f2153-efbd-4a6e-8966-a6d9755da9a6"
  FIZ_id <- "67e95c68-c1ef-4363-b351-0dfead151122"
  Ownership_id <- "5fc4e8ce-dd1d-44fd-af17-e0789cf65e4e"
  mapinfor[mapName == "TSA",
           website := paste0("https://catalogue.data.gov.bc.ca/dataset/", TSA_id)]
  mapinfor[mapName == "BEC",
           website := paste0("https://catalogue.data.gov.bc.ca/dataset/", BEC_id)]
  mapinfor[mapName == "TFL",
           website := paste0("https://catalogue.data.gov.bc.ca/dataset/", TFL_id)]
  mapinfor[mapName == "FIZ",
           website := paste0("https://catalogue.data.gov.bc.ca/dataset/", FIZ_id)]
  mapinfor[mapName == "Ownership",
           website := paste0("https://catalogue.data.gov.bc.ca/dataset/", Ownership_id)]

  mapinfor[, mapName := paste0("    ", mapName, " map")]
  mapinfor[, lastModifyDate := unlist(lapply(mapFile, function(s)unlist(strsplit(s, "_map"))[2]))]
  mapinfor[, lastModifyDate := gsub(".rds", "", lastModifyDate)]
  mapinfor[, lastModifyDate := paste0("last modified date was ",
                                      substr(lastModifyDate, 1, 4), "-",
                                      substr(lastModifyDate, 5, 6), "-",
                                      substr(lastModifyDate, 7, 8),
                                      ".")]
  mapinfor[, mapinfor := paste0(mapName, ": available at ", website, ", ",
                                lastModifyDate)]
  mapinfor <- paste(mapinfor$mapinfor, collapse = "\n ")
  publshdate <- paste0(" Data were prepared for publish on ", substr(Sys.time(), 1, 10), ".")
  vegcompyear <- substr(faib_sample_byvisit[!is.na(PROJECTED_DATE),]$PROJECTED_DATE[1], 1, 4)
  rank1layer <- paste0("    VegCompR1: available by searching vri ", vegcompyear,
                       " rank 1 layer at https://catalogue.data.gov.bc.ca/.")
  cat(publshdate, "\n",
      downloadtime, "\n",
      compiledate, "\n",
      "All the spatial maps were downloaded from BC Data Catalogue. \n",
      "Detailed availability and last modified dates were listed below: \n",
      mapinfor, "\n",
      rank1layer,
      file = file.path(publishPath, "readme.txt"))
}
