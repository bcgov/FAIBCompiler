#' ISMC compiler
#'
#'
#' @description This compiler is a general compiler to compile field data from either PSP or other
#'              programs.
#' @param compilationType character, either \code{PSP} or \code{nonPSP}. If it is \code{PSP}, it
#'                               is consistent with original PSP compiler, otherwise, it
#'                               is consistent with VRI compiler.
#' @param ismcUserName character, User name to access to ISMC database.
#' @param ismcPassword character, Password to access to ISMC database.
#' @param oracleEnv character, Specify which environment of ISMC database the data download from.
#'                             Currently, it supports 1) \code{INT} for intergration environment;
#'                             2) \code{TST} for test environment; 3) \code{PROD} for final production
#'                             environment.
#' @param bcgwUserName character, User name to access to bcgw database.
#' @param bcgwPassword character, Password to access to bcgw database.
#' @param compilationPath character, Specifies the path that stores all the data/processes. By specifying this,
#'                         four folders will be created to record all the data/processes. Specifically,
#'                         raw_from_oracle stores the data just after oracle and ascii without editing;
#'                         compilation_sa stores key data (not all) that after editing and before volume and age compilation;
#'                         compilation_db stores compiled results for volume and age compilation at both tree level
#'                         and cluater level;
#'                         Archive_YYYYMMDD achives all the data mentioned above for the future use or reference.
#' @param equation character, Specifies the taper equation that is used for compiler. Currently supports
#'                            BEC-based (\code{KBEC}) and FIZ-based (\code{KFIZ}).
#' @param walkThru logical, Speciefies whether the data had been collected using work through method. Default is \code{TRUE},
#'                          if it is not specified.
#' @param logMinLength numeric, Specifies minimum length of log when doing log length adjustment,
#'                              see \code{\link{logMatrixAdjustment}} for details. If missing 0.1 is used.
#' @param stumpHeight numeric, Stump height. If missing 0.3 is used.
#' @param breastHeight numeric, Breast height. If missing 1.3 is used.
#' @param UTOPDIB numeric, Threshold inside-bark diameter for merchantable volume. If missing, UTOPDIB is 10.
#' @param utilLevel numeric, Specifies utilization level in summrizing tree volumes at cluster and species level. Default is 4.
#' @param weirdUtil character, Specifies weird utilization in summarizing tree volumes at cluster and species level.
#'                             Default is \code{no}, if missing. Otherwise, a number should be provided.
#' @param recompile logical, Defines whether we want to recompile data using archived
#'                              raw data. Default is FALSE, which means the compiler needs to
#'                              download data from ISMC database. When it is \code{TRUE}, a folder will
#'                              be created in format of Archive_YYYYMMDD(archiveDate)_RecompYYYYMMDD(current date) to save the all the compilation.
#' @param archiveDate character, Defines on which archive date the raw data were downloaded.
#'                             These raw data will be used for recompilation. Format is YYYYMMDD.
#' @return This function compiles data and save outputs in \code{compilationPaths$compilation_db} and no file is returned.
#'
#' @importFrom data.table ':='
#' @importFrom fs dir_create dir_copy
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#' @references VRI compiler manual
#' @note
#'  Improvements include:
#'  \enumerate{
#'  \item specifies trees in B plots as height enhanced trees
#'  \item removes the sas-dependent sindex functions
#'  \item introduce the SIndexR package
#'  }
#'  Currently, the compiler supports compilation for the below sample types:
#'  \itemize{
#'  \item{\code{Q: }} {Regular VRI sample with five point clusters design}
#'  \item{\code{T: }} {??, sample protocol and sample design are same as Q samples}
#'  \item{\code{B: }} {??, same plot layout as Q sample, with height is measured for all trees and no call grading information}
#'  \item{\code{M: }} {CMI sample, with all trees have call grading information in the field}
#'  \item{\code{L: }} {LiDAR project, same plot layout and same design but without call grading information}
#'  \item{\code{Y: }} {YSM plots, population between 15 and 50 years}
#'  \item{\code{F: }} {NFI plots, trees measured all DBH, height and call grading}
#'  \item{\code{N: }} {NVAF plots}
#'  \item{\code{A: }} {VRI audit plots}
#'  }
#' @export
#' @docType methods
#' @rdname ISMCCompiler
#' @importFrom FAIBOracle loadISMC_bySampleType
#' @importFrom FAIBBase merge_dupUpdate
#' @importFrom openxlsx write.xlsx
#' @importFrom SIndexR SIndexR_VersionNumber
#'
#' @author Yong Luo
ISMCCompiler <- function(compilationType,
                         ismcUserName,
                         ismcPassword,
                         oracleEnv = "PROD",
                         bcgwUserName,
                         bcgwPassword,
                         compilationPath,
                         equation = "KBEC",
                         walkThru = TRUE,
                         logMinLength = 0.1,
                         stumpHeight = 0.3,
                         breastHeight = 1.3,
                         UTOPDIB = 10,
                         utilLevel = 4,
                         weirdUtil = "4",
                         recompile = FALSE,
                         archiveDate = as.character(NA)){
  if(!(compilationType %in% c("PSP", "nonPSP"))){
    stop("The compilationType must be either PSP or nonPSP")
  }

  cat(paste(Sys.time(), ": Prepare folders in compilation path.\n", sep = ""))
  compilationDate <- gsub("-", "", Sys.Date())
  compilationPaths <- compilerPathSetup_new(compilationPath,
                                            compilationDate,
                                            compilationType,
                                            recompile = recompile,
                                            archiveDate = archiveDate)

  cat(paste(Sys.time(), ": Check requirements for compilation:\n", sep = ""))
  checkMaps(mapPath = compilationPaths$compilation_map)
  todayDate <- as.Date(Sys.time())
  todayYear <- substr(todayDate, 1, 4)
  if(todayDate >= as.Date(paste0(todayYear, "-01-01")) &
     todayDate < as.Date(paste0(todayYear, "-03-31"))){
    compilationYear <- as.character(as.numeric(todayYear) - 1)
  } else {
    compilationYear <- todayYear
  }
  cat("    Check VOL~BA coefficients and ratios:.\n")
  if(file.exists(file.path(compilationPaths$compilation_coeff,
                           paste0("fixedCoefs", compilationYear, ".rds"))) &
     file.exists(file.path(compilationPaths$compilation_coeff,
                           paste0("randomCoefs", compilationYear, ".rds"))) &
     file.exists(file.path(compilationPaths$compilation_coeff,
                           paste0("ratios", compilationYear, ".rds")))){
    cat(paste0("        All coefficients and ratios for year ", compilationYear, " are checked.\n"))
    needNewCoffs <- FALSE
  } else {
    cat(paste0("        Coefficients and ratios for year ", compilationYear, " are not available\n"))
    cat("        and will be calculated by this compiler.\n")
    needNewCoffs <- TRUE
  }
  cat("    Check stand age table from vegcomp:.\n")
  if(!file.exists(file.path(compilationPaths$compilation_coeff,
                            paste0("stand_age_from_vegcomp_dan_", compilationYear, ".xlsx")))){
    stop(paste0("Ask Dan Turner to derive stand age table from vegcomp layer for ", compilationYear, ".\nAnd save it in coeff fold."))
  }
  if(recompile == FALSE){
    if(compilationType == "nonPSP"){
      sampletypes <- c("M", "Y", "L", "Q", "N", "Z", "D", "T",
                       "O", "F", "E", "C", "B", "A")
    } else {
      sampletypes <- "PSP"
    }
    if(!(toupper(oracleEnv) %in% c("INT", "TST", "PROD"))){
      stop("oracleEnv must be correctly specified from INT, TST and PROD.")
    }

    ### 2. load oracle data
    cat(paste(Sys.time(), ": Load data from ISMC database.\n", sep = ""))
    FAIBOracle::loadISMC_bySampleType(userName = ismcUserName,
                                      passWord = ismcPassword,
                                      env = toupper(oracleEnv),
                                      sampleType = sampletypes,
                                      savePath = compilationPaths$raw_from_oracle,
                                      saveFormat = "rds",
                                      overWrite = TRUE)
  } else {
    downloaddate <- dir(compilationPaths$raw_from_oracle, pattern = "AccessNotes.rds")
    downloaddate <- gsub("_AccessNotes.rds", "", downloaddate)
    cat(paste(Sys.time(), paste0(": The compiler recompiles existing raw data: ", downloaddate, "\n"), sep = ""))
  }
  cat(paste(Sys.time(), ": Prepare ISMC data for compilation.\n", sep = ""))
  ISMC_DataPrep(compilationType = compilationType,
                inputPath = compilationPaths$raw_from_oracle,
                outputPath = compilationPaths$compilation_sa,
                coeffPath = compilationPaths$compilation_coeff)

  cat(paste(Sys.time(), ": Compile sample and plot information.\n", sep = ""))
  samplePlotResults <- samplePlotCompilation(compilationType = compilationType,
                                             dataSourcePath = compilationPaths$compilation_sa,
                                             mapPath = compilationPaths$compilation_map,
                                             coeffPath = compilationPaths$compilation_coeff)

  cat(paste(Sys.time(), ": Update stand age from vegcomp layer.\n", sep = ""))
  sample_site_header <- updateSA_vegcomp(compilationType = compilationType,
                 coeffPath = compilationPaths$compilation_coeff,
                 bcgwUserName = bcgwUserName,
                 bcgwPassword = bcgwPassword,
                 sampleSites = samplePlotResults$spatiallookup$spatiallookup)
  saveRDS(sample_site_header,
          file.path(compilationPaths$compilation_db,
                    "sample_site_header.rds"))
  saveRDS(samplePlotResults$spatiallookup,
          file.path(compilationPaths$compilation_map,
                    paste0("spatiallookup_", compilationType, ".rds")))
  cat("    Saved spatial attribute table. \n")
  samples <- data.table::copy(samplePlotResults$samples)
  samples_tmp <- unique(samples[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, MEAS_DT,
                                   SAMPLE_SITE_PURPOSE_TYPE_CODE = TYPE_CD, SAMPLE_SITE_PURPOSE_TYPE_DESCRIPTION,
                                   SAMP_TYP, NO_PLOTS, PROJ_ID, SAMP_NO, SAMPLE_BREAK_POINT,
                                   SAMPLE_BREAK_POINT_TYPE, DBH_LIMIT_COUNT = NA, DBH_LIMIT_TAG)],
                        by = "CLSTR_ID")
  saveRDS(samples_tmp,
          file.path(compilationPaths$compilation_db, "sample_msmt_header.rds"))
  cat("    Saved compiled sample information. \n")
  saveRDS(samplePlotResults$plots,
          file.path(compilationPaths$compilation_db, "sample_plot_header.rds"))
  cat("    Saved compiled plot information. \n")

  cat(paste(Sys.time(), ": Correct species at tree level.\n", sep = ""))
  ## the species correct depends on BEC and BECsubzone, hence should be
  ## done after the bec zone information updated
  samples <- merge(samples,
                   sample_site_header[,.(SITE_IDENTIFIER,
                                         BEC_ZONE, BEC_SBZ, BEC_VAR,
                                         FIZ, TSA, PROJ_AGE_1,
                                         PROJECTED_Year = as.numeric(substr(PROJECTED_DATE, 1, 4)))],
                   by = "SITE_IDENTIFIER",
                   all.x = TRUE)
  samples[, measYear := as.numeric(substr(MEAS_DT, 1, 4))]
  samples[, SA_VEGCOMP := measYear - PROJECTED_Year + PROJ_AGE_1]
  samples[, ':='(PROJ_AGE_1 = NULL,
              PROJECTED_Year = NULL,
              measYear = NULL)]
  spCorr(BECInfor = samples[,.(CLSTR_ID, BEC_ZONE, BEC_SBZ, BEC_VAR)],
         dataSourcePath = compilationPaths$compilation_sa)
  cat(paste(Sys.time(), ": Compile tree-level WSV_VOL and MER_VOL for volume trees.\n", sep = ""))
  ## The volume trees are in two files: vi_c and vi_i
  ##
  ## the vi_c contains the trees of: 1) fully measured trees in IPC (trees have dbh, height and call grading)
  ##                             2) enhanced trees in auxi plots (trees have dbh, height and call grading)
  ##                             3) H-enhanced trees in auxi plots (trees have dbh, height)
  ##                             4) B-sample trees in fixed area lidar projects (trees have dbh, height)
  ## the vi_i contains trees in auxi plots without height information,
  ##
  samples <- merge(samplePlotResults$plots,
                   samples,
                   by = "CLSTR_ID",
                   all.x = TRUE)
  tree_ms1 <- vicPrep(compilationType = compilationType,
                      data.table::copy(samples),
                      compilationPaths$compilation_sa,
                      walkThru)
  tree_nonHT <- viiPrep(compilationType = compilationType,
                        clusterplotHeader = data.table::copy(samples),
                        dataSourcePath = compilationPaths$compilation_sa)

  ## vi_d contains call grading data for fully measured trees and enhanced trees
  tree_callGrading <- vidPrep(dataSourcePath = compilationPaths$compilation_sa)
  saveRDS(tree_callGrading$lossfactors_full,
          file.path(compilationPaths$compilation_db, "compiled_vi_d.rds"))
  tree_callGrading <- tree_callGrading$lossfactors_simp
  # assign measurement intensity
  alltrees <- assignMeasInt(compilationType = compilationType,
                            vic = tree_ms1,
                            vii = tree_nonHT,
                            vid = tree_callGrading)
  # treat broken top trees
  fullDimTrees <- alltrees$fullDimTrees
  HTEstimateMethod <-  "bestMEM"
  if(HTEstimateMethod == "bestMEM"){
    best_height_models <- read.csv(file.path(compilationPaths$compilation_coeff,
                                             "best_height_models.csv")) %>%
      data.table
    ## as suggested by Rene, as the quality of projected height is bad for PSPs,
    ## even though we have projected height for broken top trees,
    ## we should not use it
    if(compilationType == "PSP"){
      fullDimTrees[, HT_PROJ := NA] ## force projected height as NA for PSP
    }
    # ## force trees that have length of 1.4 or less as broken top trees
    fullDimTrees[HEIGHT %<=% 1.4 & BROKEN_TOP_IND == "N",
                 BROKEN_TOP_IND := "Y"]
    # for full dim trees, if field projected height is available,
    ## use this as HT_TOTAL
    fullDimTrees[BROKEN_TOP_IND == "Y" &
                   !is.na(HT_PROJ),
                 ':='(HT_TOTAL = round(FAIBBase::heightEstimateForBTOP_H(HT_PROJ), 1),
                      HT_TOTAL_SOURCE = "Field projected")]
    # this is the best height-dbh model in the mixed effect model forms
    fullDimTrees[BROKEN_TOP_IND == "Y" &
                   is.na(HT_PROJ),
                 ':='(HT_TOTAL = round(heightEstimate_byHeightModel(beczone = BEC_ZONE,
                                                                    subzone = BEC_SBZ,
                                                                    species = SPECIES,
                                                                    DBH = DBH,
                                                                    heightModels = best_height_models)),
                      HT_TOTAL_SOURCE = "Estimated based on DBH")]
    fullDimTrees[BROKEN_TOP_IND == "Y" & HT_BTOP %==% 1.3, HT_BTOP := 1.4]
    fullDimTrees[BROKEN_TOP_IND == "Y" & (HT_TOTAL < HEIGHT | is.na(HT_TOTAL)),
                 HT_TOTAL := HEIGHT]
  } else {
    # to allow using SAS routine to calculate Height based on DBH
  }
  fullDimTrees[is.na(HT_TOTAL),
               ':='(HT_TOTAL = HEIGHT,
                    HT_TOTAL_SOURCE = "Field measured")]

  # treat nonHT trees
  ## only PSP needs to calculate height for the volume calculation
  ## while nonPSP is used regression method for whole stem volumen estimate based on BA
  if(compilationType == "PSP"){
    nonHTTrees <- alltrees$nonHTTrees
    if(HTEstimateMethod == "bestMEM"){
      # this is the best height-dbh model in the mixed effect model forms
      nonHTTrees[,
                 ':='(HT_TOTAL = round(heightEstimate_byHeightModel(beczone = BEC_ZONE,
                                                                    subzone = BEC_SBZ,
                                                                    species = SPECIES,
                                                                    DBH = DBH,
                                                                    heightModels = best_height_models)),
                      HT_TOTAL_SOURCE = "Estimated based on DBH")]
    } else {
      # to allow using SAS routine to calculate Height based on DBH
    }
    voltrees <- rbindlist(list(fullDimTrees, nonHTTrees),
                          fill = TRUE)
  } else {
    voltrees <- fullDimTrees
  }
  # calculate whole stem volume and merchantable volume
  tree_ms6 <- grossVolCal_kozak(compilationType = compilationType,
                                fullDimTreeData = data.table::copy(voltrees),
                                logMinLength = logMinLength,
                                stumpHeight = stumpHeight,
                                breastHeight = breastHeight,
                                UTOPDIB = 10)
  tree_ms6[, WSV_VOL_SRCE := "Calculated"]
  if(compilationType == "nonPSP"){
    tree_ms6 <- rbindlist(list(tree_ms6, alltrees$nonHTTrees), fill = TRUE)
  }

  cat(paste(Sys.time(), ": Compile age trees.\n", sep = ""))
  ## vi_h data is the site age trees
  tree_ah1 <- readRDS(file.path(compilationPaths$compilation_sa, "vi_h.rds"))
  tree_ah1 <- merge(tree_ah1,
                    unique(samples[,.(CLSTR_ID,
                                      FIZ = as.character(FIZ))],
                           by = c("CLSTR_ID")),
                    by = c("CLSTR_ID"),
                    all.x = TRUE)
  tree_ah2 <- siteAgeCompiler(siteAgeData = data.table::copy(tree_ah1))
  tree_ah2_temp <- data.table::copy(tree_ah2)
  tree_ah2_temp[,c("FIZ", "BEC_ZONE", "SP0", "AGE_CORR",
                   "TOTAL_AG", "PHYS_AGE", "TREE_LEN",
                   "SI_SP", "BARK_PCT",
                   "AGE_SOURCE", "AGE_ADJUST_TO_BH",
                   "CR_CL") := NULL]
  saveRDS(tree_ah2_temp, file.path(compilationPaths$compilation_db, "compiled_vi_h.rds"))
  rm(tree_ah2_temp)
  siteAgeSummaries <- siteAgeSummary(tree_ah2)
  cl_ah <- siteAgeSummaries$cl_ah
  saveRDS(cl_ah, file.path(compilationPaths$compilation_db,
                           "Smries_siteAge_byCL.rds"))
  saveRDS(siteAgeSummaries$spc_ah,
          file.path(compilationPaths$compilation_db, "Smries_siteAge_byCLSP.rds"))
  rm(siteAgeSummaries, tree_ah1, tree_ah2)

  ######################
  ######################
  ### 5. start the decay, waste and breakage calculation for full/enhanced trees in vi_c
  cat(paste(Sys.time(), ": Compile DWB.\n", sep = ""))
  siteAgeTable <- FAIBBase::merge_dupUpdate(cl_ah[,.(CLSTR_ID, AT_M_TLS = AT_M_TLSO, AT_M_TXO)],
                                            unique(samples[,.(CLSTR_ID, PROJ_ID, SAMP_NO, TYPE_CD)],
                                                   by = "CLSTR_ID"),
                                            by = "CLSTR_ID",
                                            all.x = TRUE)
  tree_ms6 <- FAIBBase::merge_dupUpdate(tree_ms6,
                                        unique(samples[,.(CLSTR_ID, PROJ_ID,
                                                          TSA)],
                                               by = "CLSTR_ID"),
                                        by = "CLSTR_ID",
                                        all.x = TRUE)
  if(compilationType == "nonPSP"){
    fs::file_copy(file.path(compilationPaths$compilation_sa,
                            "treelist_A_samples.rds"),
                  file.path(compilationPaths$compilation_db,
                            "treelist_A_samples.rds"))
    tree_ms7 <- DWBCompiler(compilationType = compilationType,
                            treeMS = tree_ms6[MEAS_INTENSE %in% c("FULL", "ENHANCED"),],
                            siteAge = unique(siteAgeTable, by = "CLSTR_ID"),
                            treeLossFactors = tree_callGrading)
    tree_ms7[, NET_FCT_METHOD := "JF_reg"]
    tree_ms7 <- rbindlist(list(tree_ms7,
                               tree_ms6[MEAS_INTENSE %in% c("H-ENHANCED", "NON-ENHANCED"),]),
                          fill = TRUE)
  } else {
    tree_ms7 <- DWBCompiler(compilationType = compilationType,
                            treeMS = tree_ms6,
                            siteAge = unique(siteAgeTable, by = "CLSTR_ID"),
                            treeLossFactors = tree_callGrading)
    tree_ms7[,':='(TSA = NULL,
                   TYPE_CD = NULL,
                   NET_FCT_METHOD = "JF_reg")]
  }

  vi_c_sa <- readRDS(file.path(compilationPaths$compilation_sa,
                               "vi_c.rds"))
  tree_ms7[, ADJ_ID := NULL]
  tree_ms7 <- merge(tree_ms7,
                    vi_c_sa[,.(CLSTR_ID, PLOT, TREE_NO,
                               CR_CL, WALKTHRU_STATUS,
                               SECTOR, RESIDUAL,
                               HT_BRCH)],
                    by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                    all.x = TRUE)
  tree_ms7[DIB_STUMP < DIB_BH,
           DIB_STUMP := DIB_BH] # see rene's comment on this as per communication on April 19, 2022
  tree_ms7[(HEIGHT - HT_BRCH) < 0,
           HT_BRCH := NA]
  if(compilationType == "PSP"){
    prep_smy <- data.table::copy(tree_ms7)
  } else { # for nonPSP, save compiled vi_c

    tree_ms7_temp <- data.table::copy(tree_ms7)
    for (i in 1:9) {
      tree_ms7_temp[BROKEN_TOP_IND == "Y" &
                      LOG_BTOP == i,
                    c(paste0("LOG_L_", (i+1):9),
                      paste0("LOG_D_", (i+1):9)) := NA]
    }
    rm(i)
    tree_ms7_temp[BROKEN_TOP_IND == "Y" &
                    !is.na(LOG_BTOP),
                  NO_LOGS := LOG_BTOP]
    tree_ms7_temp[, c("SPECIES_ORG", "SP0", "HT_PROJ",
                      "DIAM_BTP", "FIZ",
                      "BEC", "BEC_ZONE", "BEC_SBZ", "BEC_VAR",
                      "BTOP",
                      "HT_BTOP", "LOGADJUST", "LOG_L_0",
                      "HT_UTOP", "VOL_PSP_MERCH", "VOL_TOP",
                      "VOL_BKT", "VOL_NET", "VOL_NETM",
                      "VAL_NET", "VAL_MER",
                      paste0("LOG_C_", 1:9),
                      "LOG_V_0", "LOG_VM_0",
                      "PROJ_ID", "AGE_DWB", "AGE_FLG",
                      "PATH_IND", "RISK_GRP",
                      "VOL_W2", "VOL_NTW2", "VOL_B", "VOL_D",
                      "VOL_DW", "LOG_BTOP",
                      "VOL_ABOVE_BTOP", "VOL_ABOVE_UTOP",
                      "VOL_BELOW_BTOP", "VOL_BELOW_UTOP",
                      "LOG_UTOP",
                      "LOG_L_10", "LOG_D_10",
                      "MEASUREMENT_ANOMALY_CODE") := NULL]
    saveRDS(tree_ms7_temp, file.path(compilationPaths$compilation_db,
                                     "compiled_vi_c.rds"))
    rm(tree_ms7_temp)
  }


  rm(siteAgeTable, tree_ms6)

  #######
  ### 6. start to calculate tree volume components for H-enhanced and non-enhanced trees in auxi plots
  ### this is specific to nonPSP compilation
  if(compilationType == "nonPSP"){
    cat(paste(Sys.time(), ": Compile NON-/H-enhanced trees for nonPSP using regression and ratio approach.\n", sep = ""))
    # derive ratio and regression routine
    if(needNewCoffs){
      cat(paste0("    Start to derive coefficients and ratios for year ", compilationYear, "\n"))
      alltreelist <- mergeAllVolTrees(treeMS = data.table::copy(tree_ms7),
                                      treeAX = data.table::copy(tree_ax1))
      samples_beccls <- unique(samples[,.(CLSTR_ID, BEC_ZONE)], by = "CLSTR_ID")
      alltreelist <- merge(alltreelist, samples_beccls, by = "CLSTR_ID", all.x = TRUE)
      allbecsplvd <- unique(alltreelist[,.(BEC_ZONE, SP0, LV_D)])

      ## if the regratiodata can not be found in coeff folder
      ## generate regratiodata and derive coeff and ratio using mixed effect models
      regRatioData <- regRatioDataSelect(samples, tree_ms7, usage = "ismc")
      saveRDS(regRatioData,
              file.path(compilationPaths$compilation_coeff,
                        paste0("regRatioData", compilationYear, ".rds")))
      cat(paste0("        Selected data and saved to regRatioData", compilationYear, "\n"))
      coefs <- regBA_WSV(regRatioData, needCombs = allbecsplvd)
      if(compilationYear > 2021){ ## comparison starts from 2022 to select the better model to predict BA-WSV relationship
        fixedcoeff_prev <- readRDS(file.path(compilationPaths$compilation_coeff,
                                             paste0("fixedCoefs", compilationYear-1, ".rds")))
        fixedcoeff_prev[, uni_strata := paste0(BEC_ZONE, SP0, LV_D)]

        randomcoeff_prev <- readRDS(file.path(compilationPaths$compilation_coeff,
                                              paste0("randomCoefs", compilationYear-1, ".rds")))
        randomcoeff_prev[, uni_strata := paste0(BEC_ZONE, SP0, LV_D)]

        allfix <- merge(coefs$fixedcoeff[,.(uni_strata,
                                            R2_Marginal_crt = R2_Marginal)],
                        fixedcoeff_prev[,.(uni_strata,
                                           R2_Marginal_prev = R2_Marginal,
                                           YEAR_FIT_prev = YEAR_FIT)],
                        by = c("uni_strata"),
                        all = TRUE)
        allfix[R2_Marginal_crt+0.05 >= R2_Marginal_prev,
               YEAR_FIT := compilationYear] ## 0.01 was chosen as an indicator
        ## of a significant improvement
        allfix[!is.na(R2_Marginal_crt) & is.na(R2_Marginal_prev),
               YEAR_FIT := compilationYear] ## the new strata
        allfix[is.na(YEAR_FIT),
               YEAR_FIT := YEAR_FIT_prev]

        fixedcoeff_crt <- coefs$fixedcoeff
        fixedcoeff_crt[, uni_strata := paste0(BEC_ZONE, SP0, LV_D)]
        randomcoeff_crt <- coefs$randomcoeff
        randomcoeff_crt[, uni_strata := paste0(BEC_ZONE, SP0, LV_D)]

        fixedcoeff_final <- fixedcoeff_crt[uni_strata %in% allfix[YEAR_FIT == compilationYear,]$uni_strata,]
        fixedcoeff_final <- rbindlist(list(fixedcoeff_final,
                                           fixedcoeff_prev[uni_strata %in% allfix[YEAR_FIT != compilationYear,]$uni_strata,]),
                                      fill = TRUE)
        fixedcoeff_final[,':='(uni_strata = NULL)]
        fixedcoeff_final[is.na(YEAR_FIT),':='(YEAR_FIT = compilationYear)]

        randomcoeff_final <- randomcoeff_crt[uni_strata %in% allfix[YEAR_FIT == compilationYear,]$uni_strata,]
        randomcoeff_final <- rbindlist(list(randomcoeff_final,
                                            randomcoeff_prev[uni_strata %in% allfix[YEAR_FIT != compilationYear,]$uni_strata,]),
                                       fill = TRUE)
        randomcoeff_final[,':='(uni_strata = NULL)]
        randomcoeff_final[is.na(YEAR_FIT),':='(YEAR_FIT = compilationYear)]
      } else {
        fixedcoeff_final <- coefs$fixedcoeff
        fixedcoeff_final[,':='(YEAR_FIT = compilationYear)]

        randomcoeff_final <- coefs$randomcoeff
        randomcoeff_final[,':='(YEAR_FIT = compilationYear)]
      }
      saveRDS(fixedcoeff_final,
              file.path(compilationPaths$compilation_coeff,
                        paste0("fixedCoefs", compilationYear, ".rds")))
      saveRDS(randomcoeff_final,
              file.path(compilationPaths$compilation_coeff,
                        paste0("randomCoefs", compilationYear, ".rds")))
      write.table(fixedcoeff_final,
                  file.path(compilationPaths$compilation_coeff,
                            paste0("fixedCoefs", compilationYear, ".txt")),
                  row.names = FALSE, sep = ",")
      write.table(randomcoeff_final,
                  file.path(compilationPaths$compilation_coeff,
                            paste0("randomCoefs", compilationYear, ".txt")),
                  row.names = FALSE, sep = ",")
      cat(paste0("        Derived and saved coefficients to fixedCoefs", compilationYear, "\n"))
      cat(paste0("                                     and randomCoefs", compilationYear, "\n"))
      ratios <- toWSVRatio(inputData = regRatioData, needCombs = allbecsplvd)
      saveRDS(ratios,
              file.path(compilationPaths$compilation_coeff,
                        paste0("ratios", compilationYear, ".rds")))
      write.table(ratios,
                  file.path(compilationPaths$compilation_coeff,
                            paste0("ratios", compilationYear, ".txt")),
                  row.names = FALSE, sep = ",")
      cat(paste0("        Calculated and saved ratios to ratios", compilationYear, "\n"))
      rm(coefs, ratios, regRatioData)
    } else {
      cat(paste0("    Use the existing coefficients and ratios of year ", compilationYear, "\n"))
    }
    fixedcoeffs <- readRDS(file.path(compilationPaths$compilation_coeff,
                                     paste0("fixedCoefs", compilationYear, ".rds")))

    randomcoeffs <- readRDS(file.path(compilationPaths$compilation_coeff,
                                      paste0("randomCoefs", compilationYear, ".rds")))
    randomcoeffs[, SAMP_POINT := as.numeric(SAMP_POINT)]
    ratios <- readRDS(file.path(compilationPaths$compilation_coeff,
                                paste0("ratios", compilationYear, ".rds")))
    ## tree with H-enhanced and non-enhanced
    auxTrees_compiled <- treeVolEst_RegRatio(tree_ms7[MEAS_INTENSE %in% c("H-ENHANCED", "NON-ENHANCED"),],
                                             fixedcoeffs,
                                             randomcoeffs,
                                             ratios)
    useRatioCurve <- TRUE
    if(useRatioCurve){
      merRatioCoef <- readRDS(file.path(compilationPaths$compilation_coeff,
                                        "mer_ratio_curve.rds"))
      ntwbRatioCoef <- readRDS(file.path(compilationPaths$compilation_coeff,
                                         "ntwb_ratio_curve.rds"))
      auxTrees_compiled <- merge(auxTrees_compiled,
                                 merRatioCoef[,.(BEC_ZONE, SP0, LV_D, a, b, c)],
                                 by = c("BEC_ZONE", "SP0", "LV_D"),
                                 all.x = TRUE)
      auxTrees_compiled[, MER_RATIO := a * (1 - exp(-b * (DBH-10)))^c]
      auxTrees_compiled[MEAS_INTENSE == "NON-ENHANCED", VOL_MER := MER_RATIO * VOL_WSV]
      auxTrees_compiled[, c("a", "b", "c", "MER_RATIO") := NULL]
      auxTrees_compiled <- merge(auxTrees_compiled,
                                 ntwbRatioCoef[,.(BEC_ZONE, SP0, LV_D, a, b, c)],
                                 by = c("BEC_ZONE", "SP0", "LV_D"),
                                 all.x = TRUE)

      auxTrees_compiled[, NTWB_RATIO := a * (1 - exp(-b * (DBH-10)))^c]
      auxTrees_compiled[!is.na(a), VOL_NTWB := NTWB_RATIO * VOL_WSV]
      auxTrees_compiled[, c("a", "b", "c", "NTWB_RATIO") := NULL]
      prep_smy <- rbindlist(list(tree_ms7[MEAS_INTENSE %in% c("FULL", "ENHANCED"),],
                                 auxTrees_compiled),
                            fill = TRUE)
    } else {
      prep_smy <- rbindlist(list(tree_ms7[MEAS_INTENSE %in% c("FULL", "ENHANCED"),],
                                 auxTrees_compiled),
                            fill = TRUE)
    }
    prep_smy[MEAS_INTENSE %in% c("FULL", "ENHANCED", "H-ENHANCED"),
             WSV_VOL_SRCE := "Calculated"]
    prep_smy[!is.na(VOL_WSV) &
               MEAS_INTENSE == "NON-ENHANCED",
             WSV_VOL_SRCE := "Regression"]
    prep_smy[is.na(VOL_WSV),
             WSV_VOL_SRCE := "Not applicable"]
    rm(auxTrees_compiled)
  }

  prep_smy <- merge(prep_smy, unique(lookup_species()[,.(SPECIES, SP_TYPE)], by = "SPECIES"),
                    by = "SPECIES", all.x = TRUE)
  volVariables <- c(paste("VOL_",c("NET", "MER", "NETM", "NTW2",
                                   "NTWB", "D", "DW", "DWB"),
                          sep = ""), "VAL_MER")
  prep_smy[DBH < 10, c(volVariables) := 0]
  prep_smy[MEAS_INTENSE %in% c( "H-ENHANCED") &
             VOL_NETM %>>% VOL_MER,
           VOL_NETM := VOL_MER]
  prep_smy[MEAS_INTENSE %in% c( "H-ENHANCED") &
             VOL_NTW2 %>>% VOL_MER,
           VOL_NTW2 := VOL_MER]
  prep_smy[MEAS_INTENSE %in% c( "H-ENHANCED") &
             VOL_NTWB %>>% VOL_MER,
           VOL_NTWB := VOL_MER]

  ## some trees might be cut to dead, hence, the vol_mer, vol_ntwb and vol_dwb should
  ## be zero, see Rene's email on 2022-05-24
  ## those trees were not marked, and need some criteria to identify
  ## specifically, those trees have extreme low height/DBH ratio, i.e., 0.25
  ## with height less than 2.5 and dead.
  ## the following process is used to deal with this issue
  prep_smy[HEIGHT/DBH <= 0.25 &
             HEIGHT <= 2.5 &
             LV_D == "D",
           ':='(VOL_MER = 0,
                VOL_NTWB = 0,
                VOL_DWB = 0)]
  prep_smy[is.na(S_F), S_F := "S"]
  ## end of process

  if(compilationType == "nonPSP"){

    prep_smy_temp <- prep_smy[,.(CLSTR_ID, PLOT, TREE_NO, SPECIES, MEAS_INTENSE, LV_D,
                                 S_F, HEIGHT, HT_TOTAL, HT_TOTAL_SOURCE, BTOP, H_MERCH, SP0, BA_TREE,
                                 PHF_TREE, DBH, TREE_WT, VOL_WSV, VOL_MER, VOL_NTWB,
                                 VOL_DWB, SPECIES_ORG, NET_FCT_METHOD, WSV_VOL_SRCE, SP_TYPE)]
  } else {
    prep_smy_temp <- prep_smy[,.(CLSTR_ID, PLOT, TREE_NO, SPECIES, MEAS_INTENSE, LV_D,
                                 S_F, HEIGHT, HT_TOTAL, HT_TOTAL_SOURCE, BTOP, H_MERCH, SP0, BA_TREE,
                                 PHF_TREE, DBH, TREE_WT, VOL_WSV, VOL_MER, VOL_NTWB,
                                 VOL_DWB, SPECIES_ORG, NET_FCT_METHOD, WSV_VOL_SRCE, SP_TYPE,
                                 CR_CL, DIB_BH, DIB_STUMP, DIB_UTOP, HT_BH, HT_BRCH,
                                 HT_STUMP, PCT_BRK, PCT_DCY, PCT_WST, RESIDUAL, SECTOR,
                                 VOL_STUMP)]
  }

  saveRDS(prep_smy_temp[order(CLSTR_ID, PLOT, TREE_NO),],
          file.path(compilationPaths$compilation_db, "treelist.rds"))
  rm(prep_smy_temp)


  ## 7. sammarize and save compiled tree-level data at cluster and cluster/species level
  cat(paste(Sys.time(), ": Summarize volume at sample level.\n", sep = ""))
  nvafratio <- read.xlsx(file.path(compilationPaths$compilation_coeff, "nvafall.xlsx")) %>%
    data.table
  vrisummaries <- VRISummaries(allVolumeTrees = data.table::copy(prep_smy),
                               clusterPlotHeader = samples,
                               utilLevel = utilLevel,
                               weirdUtil = weirdUtil,
                               equation = "KBEC",
                               nvafRatio = nvafratio)
  vrisummaries$vol_bycs[,':='(VHA_DWB_LF = NULL,
                              VHA_DWB_DS = NULL,
                              VHA_DWB_DF = NULL)]
  vrisummaries$vol_byc[,':='(VHA_DWB_LF = NULL,
                             VHA_DWB_DS = NULL,
                             VHA_DWB_DF = NULL)]
  saveRDS(vrisummaries$vol_bycs, file.path(compilationPaths$compilation_db, "Smries_volume_byCLSP.rds"))
  saveRDS(vrisummaries$vol_byc, file.path(compilationPaths$compilation_db, "Smries_volume_byCL.rds"))
  saveRDS(vrisummaries$heightsmry_byc, file.path(compilationPaths$compilation_db, "Smries_height_byCL.rds"))
  saveRDS(vrisummaries$compositionsmry_byc, file.path(compilationPaths$compilation_db, "Smries_speciesComposition_byCL.rds"))


  ## 8. small tree and stump compilation
  ## stump data
  cat(paste(Sys.time(), ": Small tree and stump compilation.\n", sep = ""))
  vi_e <- readRDS(file.path(compilationPaths$compilation_sa, "vi_e.rds")) %>% data.table
  vi_e <- vi_e[CLSTR_ID %in% unique(samples$CLSTR_ID),]

  ## small tree data
  vi_f <- readRDS(file.path(compilationPaths$compilation_sa, "vi_f.rds")) %>% data.table
  vi_f[, obslength := length(TOTAL2), by = c("CLSTR_ID", "PLOT", "SPECIES")]
  vi_f <- unique(vi_f, by = c("CLSTR_ID", "PLOT", "SPECIES"))
  vi_f[, clusterplot := paste(CLSTR_ID, "_", PLOT, sep = "")]
  vi_e[, clusterplot := paste(CLSTR_ID, "_", PLOT, sep = "")]
  vi_f <- vi_f[clusterplot %in% unique(vi_e[PL_ORIG == "SML_TR",]$clusterplot),]
  smalltreecompile <- smallTreeSmry(smallTreeData = vi_f,
                                    smallTreePlotHeader = vi_e[PL_ORIG == "SML_TR",])
  saveRDS(smalltreecompile$clusterSummaries,
          file.path(compilationPaths$compilation_db, "Smries_smallTree_byCL.rds"))
  saveRDS(smalltreecompile$clusterSpeciesSummaries,
          file.path(compilationPaths$compilation_db, "Smries_smallTree_byCLSP.rds"))
  rm(smalltreecompile)
  vi_g <- readRDS(file.path(compilationPaths$compilation_sa, "vi_g.rds")) %>% data.table
  vi_g[, clusterplot := paste(CLSTR_ID, "_", PLOT, sep = "")]
  vi_g <- vi_g[clusterplot %in% unique(vi_e[PL_ORIG == "SML_TR",]$clusterplot),]

  ## plot header for stump and small trees
  stumpCompile <- stumpVolSmry(stumpData = vi_g,
                               stumpPlotHeader = vi_e[PL_ORIG == "STUMP",])
  saveRDS(stumpCompile$stmp_c,
          file.path(compilationPaths$compilation_db, "Smries_stump_byCL.rds"))
  saveRDS(stumpCompile$stmp_cs,
          file.path(compilationPaths$compilation_db, "Smries_stump_byCLSP.rds"))
  #############################

  cat(paste(Sys.time(), ": Convert RDS to xlsx.\n", sep = ""))

  if(recompile == TRUE){
    allfolders <- c(compilationPaths$compilation_db,
                    compilationPaths$compilation_sa)
  } else {
    allfolders <- c(compilationPaths$compilation_db,
                    compilationPaths$compilation_sa,
                    compilationPaths$raw_from_oracle)
  }

  for (indifolder in allfolders){
    allfiles_indifolder <- dir(pattern = ".rds", indifolder)
    allfiles_indifolder <- gsub(".rds", "", allfiles_indifolder)

    for (indifile in allfiles_indifolder) {
      thedata <- readRDS(file.path(indifolder, paste0(indifile, ".rds")))
      if(compilationType == "PSP" & indifile == "treelist"){
        # write.csv(thedata,
        #           file.path(indifolder, paste0(indifile, ".csv")),
        #           row.names = FALSE)
      } else {
        write.xlsx(thedata,
                   file.path(indifolder, paste0(indifile, ".xlsx")))
      }
    }
  }
  if(recompile == FALSE){
    cat(paste(Sys.time(), ": Generate reports in report folder.\n", sep = ""))
    vegcompversion <- readRDS(file.path(compilationPaths$compilation_coeff,
                                        paste0("SA_VEGCOMP_", compilationType, ".rds")))
    vegcompversion <- substr(vegcompversion$vegCompVersion, 1, 10)
    lastCompilationDate <- gsub(paste0(compilationPath, "/Archive_", compilationType, "_"), "",
                                compilationPaths$compilation_last)
    rmarkdown::render(input = file.path(compilationPaths$compilation_report,
                                        "general_report.Rmd"),
                      params = list(projectType = compilationType,
                                    crtPath = compilationPaths$compilation_db,
                                    lastPath = compilationPaths$compilation_last,
                                    mapPath = compilationPaths$compilation_map,
                                    coeffPath = compilationPaths$compilation_coeff,
                                    compilationDate = compilationDate,
                                    lastCompilationDate = lastCompilationDate,
                                    compilationYear = compilationYear,
                                    sindexVersion = as.numeric(SIndexR::SIndexR_VersionNumber())/100,
                                    vegcompVersion = vegcompversion),
                      quiet = TRUE)
    cat(paste(Sys.time(), ": Archive compilation to Archive_", gsub("-", "", Sys.Date()), ".\n", sep = ""))
    if(dir.exists(compilationPaths$compilation_archive)){
      unlink(compilationPaths$compilation_archive, recursive = TRUE)
    }
    fs::dir_create(compilationPaths$compilation_archive)
    fs::dir_copy(compilationPaths$compilation_sa,
                 compilationPaths$compilation_archive)
    fs::dir_copy(compilationPaths$compilation_db,
                 compilationPaths$compilation_archive)
    fs::dir_copy(compilationPaths$raw_from_oracle,
                 compilationPaths$compilation_archive)
    fs::dir_copy(compilationPaths$compilation_coeff,
                 compilationPaths$compilation_archive)
    fs::dir_copy(compilationPaths$compilation_map,
                 compilationPaths$compilation_archive)
    fs::dir_copy(compilationPaths$compilation_report,
                 compilationPaths$compilation_archive)
  } else {
    cat(paste(Sys.time(), ": Generate reports in report folder.\n", sep = ""))
    cat(paste(Sys.time(), ": All recompiled outputs saved into Archive_", archiveDate, "_recomp", gsub("-", "", Sys.Date()), ".\n", sep = ""))
    vegcompversion <- readRDS(file.path(compilationPaths$compilation_coeff,
                                        paste0("SA_VEGCOMP_", compilationType, ".rds")))
    vegcompversion <- substr(vegcompversion$vegCompVersion, 1, 10)
    rmarkdown::render(input = file.path(compilationPaths$compilation_report,
                                        "general_report.Rmd"),
                      params = list(projectType = compilationType,
                                    crtPath = compilationPaths$compilation_db,
                                    lastPath = compilationPaths$compilation_last,
                                    mapPath = compilationPaths$compilation_map,
                                    coeffPath = compilationPaths$compilation_coeff,
                                    compilationDate = compilationDate,
                                    lastCompilationDate = compilationDate,
                                    compilationYear = compilationYear,
                                    sindexVersion = as.numeric(SIndexR::SIndexR_VersionNumber())/100,
                                    vegcompVersion = vegcompversion),
                      quiet = TRUE)
  }
}
