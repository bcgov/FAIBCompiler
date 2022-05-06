#' ISMC compiler - Adapted from VRI compiler to compile data converted from VGIS database
#'
#'
#' @description This compiler is adapted version of original VRI compiler. It loads
#'              data from ISMC, pipes data into compilation processes and outputs
#'              compilated results at both tree and stand levels.
#'
#' @param oracleUserName character, User name to access to ISMC database.
#' @param oraclePassword character, Password to access to ISMC database.
#' @param oracleEnv character, Specify which environment of ISMC database the data download from.
#'                             Currently, it supports 1) \code{INT} for intergration environment;
#'                             2) \code{TST} for test environment; 3) \code{PROD} for final production
#'                             environment.
#' @param compilationPath character, Specifies the path that stores all the data/processes. By specifying this,
#'                         four folders will be created to record all the data/processes. Specifically,
#'                         raw_from_oracle stores the data just after oracle and ascii without editing;
#'                         compilation_sa stores key data (not all) that after editing and before volume and age compilation;
#'                         compilation_db stores compiled results for volume and age compilation at both tree level
#'                         and cluater level;
#'                         Archive_YYYYMMDD achives all the data mentioned above for the future use or reference.
#'                         By default, this path is set as \code{//albers/gis_tib/VRI/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/RCompilation},
#'                         which is consistent with our rdw system.
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
#'
#' @return This function compiles data and save outputs in \code{compilationPaths$compilation_db} and no file is returned.
#'
#' @importFrom data.table ':='
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
#'

ISMCCompiler <- function(oracleUserName,
                         oraclePassword,
                         oracleEnv = "INT",
                         compilationPath = "//albers/gis_tib/VRI/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/FromRCompiler",
                         equation = "KBEC",
                         walkThru = TRUE,
                         logMinLength = 0.1,
                         stumpHeight = 0.3,
                         breastHeight = 1.3,
                         UTOPDIB = 10,
                         utilLevel = 4,
                         weirdUtil = "4"){

  # rm(list = ls())
  # compilationPath <- "D:/ISMC project/ISMC compiler/ismc compiler development"
  cat(paste(Sys.time(), ": Prepare folders in compilation path.\n", sep = ""))
  compilationDate <- gsub("-", "", Sys.Date())
  compilationPaths <- compilerPathSetup(compilationPath,
                                        compilationDate)
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

  sampletypes <- c("M", "Y", "L", "Q", "N", "Z", "D", "T",
                   "O", "F", "E", "C", "B")

  if(!(toupper(oracleEnv) %in% c("INT", "TST", "PROD"))){
    stop("oracleEnv must be correctly specified from INT, TST and PROD.")
  }

  ### 2. load oracle data
  cat(paste(Sys.time(), ": Load data from ISMC database.\n", sep = ""))
  FAIBOracle::loadISMC_bySampleType(userName = oracleUserName,
                                    passWord = oraclePassword,
                                    env = toupper(oracleEnv),
                                    sampleType = sampletypes,
                                    savePath = compilationPaths$raw_from_oracle,
                                    saveFormat = "rds",
                                    overWrite = TRUE)
  cat(paste(Sys.time(), ": Translate ISMC data to compiler.\n", sep = ""))
  ISMC_VGISTranslator(inputPath = compilationPaths$raw_from_oracle,
                      outputPath = compilationPaths$compilation_sa,
                      coeffPath = compilationPaths$compilation_coeff)

  vi_a <- readRDS(file.path(compilationPaths$compilation_sa, "vi_a.rds"))
  cat(paste(Sys.time(), ": Update spatial attributes.\n", sep = ""))
  spatialLookups <- updateSpatial(samplesites = vi_a,
                                  mapPath = compilationPaths$compilation_map)
  saveRDS(spatialLookups,
          file.path(compilationPaths$compilation_sa,
                    "vi_a.rds"))
  spatialLookups_simp <- unique(spatialLookups[,.(SITE_IDENTIFIER, SAMP_POINT = SITE_IDENTIFIER,
                                                  IP_UTM, IP_NRTH, IP_EAST, BC_ALBERS_X, BC_ALBERS_Y,
                                                  Longitude, Latitude, BEC, BEC_SBZ, BEC_VAR,
                                                  TSA, TSA_DESC, FIZ, TFL, OWNER, SCHEDULE,
                                                  PROJ_ID, SAMP_NO)],
                                by = "SAMP_POINT")
  saveRDS(spatialLookups_simp,
          file.path(compilationPaths$compilation_db,
                    "spatiallookup.rds"))
  cat("    Saved spatial attribute table as spatiallookup \n")


  ### 2.1 load cluster/plot header
  clusterplotheader_VRI <- VRIInit_clusterplot(dataSourcePath = compilationPaths$compilation_sa)
  samples <- data.table::copy(clusterplotheader_VRI)
  samples[,':='(SITE_IDENTIFIER = substr(CLSTR_ID, 1, 7),
                SAMPLE_SITE_PURPOSE_TYPE_CODE = substr(CLSTR_ID, 9, 9),
                VISIT_NUMBER = substr(CLSTR_ID, 10, 10))]
  standage_vegcomp <- read.xlsx(file.path(compilationPaths$compilation_coeff,
                                          paste0("stand_age_from_vegcomp_dan_", compilationYear, ".xlsx")),
                                detectDates = TRUE) %>%
    data.table
  samples <- merge(samples,
                   standage_vegcomp[,.(SITE_IDENTIFIER = as.character(SITE_IDENTIFIER), PROJ_AGE_1,
                                       PROJECTED_Year = as.numeric(substr(PROJECTED_DATE, 1, 4)))],
                   by = "SITE_IDENTIFIER",
                   all.x = TRUE)
  samples[, measYear := as.numeric(substr(MEAS_DT, 1, 4))]

  samples[, SA_VEGCOMP := measYear - PROJECTED_Year + PROJ_AGE_1]
  samples[, ':='(PROJ_AGE_1 = NULL,
                 PROJECTED_Year = NULL,
                 measYear = NULL)]
  samples_tmp <- data.table::copy(samples)
  samples_tmp[, ':='(PRJ_GRP = NULL,
                     SA_VEGCOMP = NULL)]
  saveRDS(samples_tmp,
          file.path(compilationPaths$compilation_db, "samples.rds"))
  # write.csv(samples, file.path(compilationPaths$compilation_db, "samples.csv"), row.names = FALSE)

  rm(clusterplotheader_VRI)
  ### 2.2 load vi_c data
  ## vi_c contains the trees of: 1) fully measured trees in IPC (trees have dbh, height and call grading)
  ##                             2) enhanced trees in auxi plots (trees have dbh, height and call grading)
  ##                             3) H-enhanced trees in auxi plots (trees have dbh, height)
  ##                             4) B-sample trees in fixed area lidar projects (trees have dbh, height)

  # equation  <- "KBEC"
  # walkThru <-  TRUE
  # logMinLength <-  0.1
  # stumpHeight <-  0.3
  # breastHeight <-  1.3
  # UTOPDIB <-  10
  # utilLevel <-  4
  # weirdUtil = "No"

  # samples <- readRDS("D:/ISMC project/ISMC compiler/ismc compiler prod env/compilation_db/samples.rds")
  # compilationPaths <- list()
  # compilationPaths$compilation_sa <- "D:/ISMC project/ISMC compiler/ismc compiler prod env/compilation_sa"


  tree_ms1 <- VRIInit_measuredTree(data.table::copy(samples),
                                   compilationPaths$compilation_sa,
                                   walkThru)
  ### 2.3 load vi_d data
  ## vi_d contains call grading data for fully measured trees and enhanced trees
  vi_d <- VRIInit_lossFactor(fullMeasuredTrees = tree_ms1[,.(CLSTR_ID, PLOT, TREE_NO, SPECIES, SPECIES_ORG, SP0)],
                             dataSourcePath = compilationPaths$compilation_sa)

  vi_d_temp <- readRDS(file.path(compilationPaths$compilation_sa, "vi_d.rds"))
  vi_d_temp[,':='(SPECIES = NULL,
                  SP0 = NULL)]
  vi_d_temp <- merge(vi_d_temp,
                     vi_d[,.(CLSTR_ID, PLOT, TREE_NO, SPECIES)],
                     by = c("CLSTR_ID", "PLOT", "TREE_NO"))
  vi_d_temp[, c(paste0("T_SIGN", 1:10),
                paste0("F_SIGN", 1:10),
                paste0("OLD_AGN", LETTERS[1:8])) := NULL]
  setnames(vi_d_temp, "STEM", "STEM_MAPPED_IND")
  saveRDS(vi_d_temp,
          file.path(compilationPaths$compilation_db, "compiled_vi_d.rds"))
  rm(vi_d_temp)
  ### 2.4 load vi_i data
  ## vi_i has trees in auxi plots without height information (mostly), however, some of these trees are also in vi_c
  tree_ax1 <- VRIInit_auxTree(data.table::copy(samples),
                              compilationPaths$compilation_sa)

  tree_ax1 <- merge(tree_ax1, unique(lookup_species()[,.(SPECIES, SP0)], by = "SPECIES"),
                    by = "SPECIES")
  ### 2.5 load vi_h data
  ## vi_h data is the site age trees
  tree_ah1 <- VRIInit_siteTree(data.table::copy(samples),
                               compilationPaths$compilation_sa)


  ### 3. vi_c compilation
  cat(paste(Sys.time(), ": Compile full/enhanced and h-enhanced volume trees.\n", sep = ""))
  tree_ms1[LOG_G_1 == "*",
           MEAS_INTENSE := "H-ENHANCED"]
  ## B sample trees are H-enhnced trees
  tree_ms1[substr(CLSTR_ID, 9, 9) == "B",
           MEAS_INTENSE := "H-ENHANCED"]
  tree_ms1[is.na(MEAS_INTENSE) & PLOT == "I",
           MEAS_INTENSE := "FULL"]
  tree_ms1[is.na(MEAS_INTENSE),
           MEAS_INTENSE := "ENHANCED"]
  ## for the full/enhanced trees, if the length of first log is missing, assign
  ## them with tree height
  tree_ms1[MEAS_INTENSE %in% c("FULL", "ENHANCED") & LOG_L_1 %in% c(NA, 0),
           LOG_L_1 := HEIGHT]
  ## for the zero tree height trees, force them as non-enhanced trees, which means
  ## they only have DBH information
  tree_ms1[HEIGHT %in% c(NA, 0), MEAS_INTENSE := "NON-ENHANCED"]
  nonenhancedtreedata <- tree_ms1[MEAS_INTENSE == "NON-ENHANCED",]
  voltrees <- data.table::copy(tree_ms1)[MEAS_INTENSE %in% c("FULL", "ENHANCED", "H-ENHANCED"),]
  voltrees <- merge(voltrees, unique(samples[,.(CLSTR_ID, FIZ, BEC_ZONE, BEC_SBZ, BEC_VAR)],
                                     by = "CLSTR_ID"),
                    by = "CLSTR_ID",
                    all.x = TRUE)

  voltrees[is.na(SP0), ':='(SPECIES = "X", SP0 = "F")]
  best_height_models <- read.csv(file.path(compilationPaths$compilation_coeff,
                                           "best_height_models.csv"),
                                 stringsAsFactors = FALSE) %>%
    data.table
  tree_ms6 <- VRIVolTree(treeData = data.table::copy(voltrees),
                         equation = equation,
                         logMinLength = logMinLength,
                         stumpHeight = stumpHeight,
                         breastHeight = breastHeight,
                         UTOPDIB = UTOPDIB,
                         bestHeightModels = best_height_models,
                         HTBTOPModel = "height")
  tree_ms6 <- rbindlist(list(tree_ms6, nonenhancedtreedata), fill = TRUE)
  rm(tree_ms1, voltrees, nonenhancedtreedata)

  ######################
  ###################### start the site age compilation
  ### 4. vi_h site age compilation
  cat(paste(Sys.time(), ": Compile age trees.\n", sep = ""))
  tree_ah1 <- FAIBBase::merge_dupUpdate(tree_ah1,
                                        unique(samples[,.(CLSTR_ID, PLOT,
                                                   FIZ = as.character(FIZ),
                                                   BEC_ZONE)],
                                               by = c("CLSTR_ID", "PLOT")),
                                        by = c("CLSTR_ID", "PLOT"),
                                        all.x = TRUE)
  tree_ah2 <- siteAgeCompiler(siteAgeData = data.table::copy(tree_ah1))
  tree_ah2[, CR_CL := NULL]
  tree_ah2_temp <- data.table::copy(tree_ah2)
  tree_ah2_temp[,c("FIZ", "BEC_ZONE", "SP0", "AGE_CORR",
                   "TOTAL_AG", "PHYS_AGE", "TREE_LEN",
                   "SI_SP", "BARK_PCT") := NULL]
  saveRDS(tree_ah2_temp, file.path(compilationPaths$compilation_db, "compiled_vi_h.rds"))
  rm(tree_ah2_temp)
  # write.csv(tree_ah2, file.path(compilationPaths$compilation_db, "compiled_vi_h.csv"), row.names = FALSE)

  siteAgeSummaries <- siteAgeSummary(tree_ah2)
  cl_ah <- siteAgeSummaries$cl_ah
  saveRDS(cl_ah, file.path(compilationPaths$compilation_db,
                           "Smries_siteAge_byCL.rds"))
  # write.csv(cl_ah, file.path(compilationPaths$compilation_db,
  #                          "Smries_siteAge_byCL.csv"), row.names = FALSE)
  saveRDS(siteAgeSummaries$spc_ah,
          file.path(compilationPaths$compilation_db, "Smries_siteAge_byCLSP.rds"))
  # write.csv(siteAgeSummaries$spc_ah,
  #         file.path(compilationPaths$compilation_db, "Smries_siteAge_byCLSP.csv"), row.names = FALSE)
  rm(siteAgeSummaries, tree_ah1, tree_ah2)

  ######################
  ######################
  ### 5. start the decay, waste and breakage calculation for full/enhanced trees in vi_c
  cat(paste(Sys.time(), ": Compile DWB.\n", sep = ""))

  siteAgeTable <- FAIBBase::merge_dupUpdate(cl_ah[,.(CLSTR_ID, AT_M_TLS, AT_M_TXO)],
                                            unique(samples[,.(CLSTR_ID, PROJ_ID, SAMP_NO, TYPE_CD)],
                                                   by = "CLSTR_ID"),
                                            by = "CLSTR_ID",
                                            all.x = TRUE)
  tree_ms6 <- FAIBBase::merge_dupUpdate(tree_ms6,
                                        unique(samples[,.(CLSTR_ID, PROJ_ID, BEC_ZONE, BEC_SBZ, BEC_VAR,
                                                          TSA, TYPE_CD)],
                                               by = "CLSTR_ID"),
                                        by = "CLSTR_ID",
                                        all.x = TRUE)
  tree_ms7 <- DWBCompiler(treeMS = tree_ms6[MEAS_INTENSE %in% c("FULL", "ENHANCED"),],
                          siteAge = unique(siteAgeTable, by = "CLSTR_ID"),
                          treeLossFactors = vi_d, equation = "KBEC")

  tree_ms7 <- rbindlist(list(tree_ms7,
                             tree_ms6[MEAS_INTENSE %in% c("H-ENHANCED", "NON-ENHANCED"),]),
                        fill = TRUE)
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
tree_ms7_temp <- data.table::copy(tree_ms7)
tree_ms7_temp[, c("SPECIES_ORG", "SP0", "HT_PROJ",
                  "DIAM_BTP", "FIZ", "VOL_MULT",
                  "BEC", "BEC_ZONE", "BEC_SBZ", "BEC_VAR",
                  "BTOP", "BTOP_ESTIMATE_TYPE",
                  "HT_BTOP", "LOGADJUST", "LOG_L_0",
                  "HT_UTOP", "VOL_PSP_MERCH", "VOL_TOP",
                  "VOL_BKT", "VOL_NET", "VOL_NETM",
                  "VAL_NET", "VAL_MER",
                  paste0("LOG_C_", 1:9),
                  "PROJ_ID", "AGE_DWB", "AGE_FLG",
                  "PATH_IND", "RISK_GRP", "ADJ_ID",
                  "VOL_W2", "VOL_NTW2", "VOL_B", "VOL_D",
                  "VOL_DW") := NULL]
  saveRDS(tree_ms7_temp, file.path(compilationPaths$compilation_db,
                              "compiled_vi_c.rds"))
  rm(tree_ms7_temp)
  # write.csv(tree_ms7, file.path(compilationPaths$compilation_db,
  #                             "compiled_vi_c.csv"), row.names = FALSE)
  rm(vi_d, siteAgeTable, tree_ms6)

  #######
  ### 6. start to calculate tree volume components for H-enhanced and non-enhanced trees in auxi plots
  cat(paste(Sys.time(), ": Compile NON- and H-enhanced trees.\n", sep = ""))
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
  ratios <- readRDS(file.path(compilationPaths$compilation_coeff,
                              paste0("ratios", compilationYear, ".rds")))
  auxtreecompilation <- auxiTreeCompiler(fullMeasuredTrees = data.table::copy(tree_ms7),
                                         auxiTrees = data.table::copy(tree_ax1),
                                         clusterPlotHeader = samples,
                                         fixedCoeff = fixedcoeffs,
                                         randomCoeff = randomcoeffs,
                                         ratios = ratios)
  useRatioCurve <- TRUE
  if(useRatioCurve){
    merRatioCoef <- readRDS(file.path(compilationPaths$compilation_coeff,
                                      "mer_ratio_curve.rds"))
    ntwbRatioCoef <- readRDS(file.path(compilationPaths$compilation_coeff,
                                       "ntwb_ratio_curve.rds"))
    HnonenhancedTrees <- merge(auxtreecompilation$HnonenhancedTrees,
                               merRatioCoef[,.(BEC_ZONE, SP0, LV_D, a, b, c)],
                               by = c("BEC_ZONE", "SP0", "LV_D"),
                               all.x = TRUE)
    HnonenhancedTrees[, MER_RATIO := a * (1 - exp(-b * (DBH-10)))^c]
    HnonenhancedTrees[MEAS_INTENSE == "NON-ENHANCED", VOL_MER := MER_RATIO * VOL_WSV]
    HnonenhancedTrees[, c("a", "b", "c", "MER_RATIO") := NULL]

    HnonenhancedTrees <- merge(HnonenhancedTrees,
                               ntwbRatioCoef[,.(BEC_ZONE, SP0, LV_D, a, b, c)],
                               by = c("BEC_ZONE", "SP0", "LV_D"),
                               all.x = TRUE)

    HnonenhancedTrees[, NTWB_RATIO := a * (1 - exp(-b * (DBH-10)))^c]
    HnonenhancedTrees[!is.na(a), VOL_NTWB := NTWB_RATIO * VOL_WSV]
    HnonenhancedTrees[, c("a", "b", "c", "NTWB_RATIO") := NULL]
    prep_smy <- rbindlist(list(auxtreecompilation$fullenhancedtrees,
                               HnonenhancedTrees),
                          fill = TRUE)
  } else {
    prep_smy <- rbindlist(list(auxtreecompilation$fullenhancedtrees,
                               auxtreecompilation$HnonenhancedTrees),
                          fill = TRUE)
  }

  prep_smy[MEAS_INTENSE %in% c("FULL", "ENHANCED", "H-ENHANCED"),
           WSV_VOL_SRCE := "Calculated"]
  prep_smy[!is.na(VOL_WSV) &
             MEAS_INTENSE == "NON-ENHANCED",
           WSV_VOL_SRCE := "Regression"]
  prep_smy[is.na(VOL_WSV),
           WSV_VOL_SRCE := "Not applicable"]

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
  prep_smy_temp <- data.table::copy(prep_smy)
  prep_smy_temp[, c("LOG_G_1", paste0("VOL_", c("NET", "NETM", "NTW2", "D", "DW")),
                    "VAL_MER", "BEC_ZONE", "SAMP_POINT") := NULL]
  saveRDS(prep_smy_temp[order(CLSTR_ID, PLOT, TREE_NO),],
          file.path(compilationPaths$compilation_db, "treelist.rds"))
  rm(prep_smy_temp)
  # write.csv(prep_smy[order(CLSTR_ID, PLOT, TREE_NO),],
  #         file.path(compilationPaths$compilation_db, "treelist.csv"), row.names = FALSE)
  rm(auxtreecompilation)

  ## 7. sammarize and save compiled tree-level data at cluster and cluster/species level
  cat(paste(Sys.time(), ": Summarize volume and age.\n", sep = ""))
  nvafratio <- read.xlsx(file.path(compilationPaths$compilation_coeff, "nvafall.xlsx")) %>%
    data.table
  vrisummaries <- VRISummaries(allVolumeTrees = data.table::copy(prep_smy),
                               clusterPlotHeader = samples,
                               utilLevel = utilLevel,
                               weirdUtil = weirdUtil,
                               equation = equation,
                               nvafRatio = nvafratio)
  saveRDS(vrisummaries$vol_bycs, file.path(compilationPaths$compilation_db, "Smries_volume_byCLSP.rds"))
  saveRDS(vrisummaries$vol_byc, file.path(compilationPaths$compilation_db, "Smries_volume_byCL.rds"))
  saveRDS(vrisummaries$heightsmry_byc, file.path(compilationPaths$compilation_db, "Smries_height_byCL.rds"))
  saveRDS(vrisummaries$compositionsmry_byc, file.path(compilationPaths$compilation_db, "Smries_speciesComposition_byCL.rds"))

  # write.csv(vrisummaries$vol_bycs, file.path(compilationPaths$compilation_db, "Smries_volume_byCLSP.csv"), row.names = FALSE)
  # write.csv(vrisummaries$vol_byc, file.path(compilationPaths$compilation_db, "Smries_volume_byCL.csv"), row.names = FALSE)
  # write.csv(vrisummaries$vol_byc, file.path(compilationPaths$compilation_db, "Smries_height_byCL.csv"), row.names = FALSE)
  # write.csv(vrisummaries$compositionsmry_byc, file.path(compilationPaths$compilation_db, "Smries_speciesComposition_byCL.csv"), row.names = FALSE)


  ## 8. small tree and stump compilation
  ## stump data
  cat(paste(Sys.time(), ": Small tree and stump compilation.\n", sep = ""))
  vi_e <- readRDS(file.path(compilationPaths$compilation_sa, "vi_e.rds")) %>% data.table
  names(vi_e) <- toupper(names(vi_e))
  vi_e <- vi_e[CLSTR_ID %in% unique(samples$CLSTR_ID),]

  ## small tree data
  vi_f <- readRDS(file.path(compilationPaths$compilation_sa, "vi_f.rds")) %>% data.table
  names(vi_f) <- toupper(names(vi_f))
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
  # write.csv(smalltreecompile$clusterSummaries,
  #         file.path(compilationPaths$compilation_db, "Smries_smallTree_byCL.csv"), row.names = FALSE)
  # write.csv(smalltreecompile$clusterSpeciesSummaries,
  # file.path(compilationPaths$compilation_db, "Smries_smallTree_byCLSP.csv"), row.names = FALSE)
  rm(smalltreecompile)
  vi_g <- readRDS(file.path(compilationPaths$compilation_sa, "vi_g.rds")) %>% data.table
  names(vi_g) <- toupper(names(vi_g))

  vi_g[, clusterplot := paste(CLSTR_ID, "_", PLOT, sep = "")]
  vi_g <- vi_g[clusterplot %in% unique(vi_e[PL_ORIG == "SML_TR",]$clusterplot),]

  ## plot header for stump and small trees
  stumpCompile <- stumpVolSmry(stumpData = vi_g,
                               stumpPlotHeader = vi_e[PL_ORIG == "STUMP",])
  saveRDS(stumpCompile$stmp_c,
          file.path(compilationPaths$compilation_db, "Smries_stump_byCL.rds"))
  saveRDS(stumpCompile$stmp_cs,
          file.path(compilationPaths$compilation_db, "Smries_stump_byCLSP.rds"))
  # write.csv(stumpCompile$stmp_c,
  #         file.path(compilationPaths$compilation_db, "Smries_stump_byCL.csv"), row.names = FALSE)
  # write.csv(stumpCompile$stmp_cs,
  #         file.path(compilationPaths$compilation_db, "Smries_stump_byCLSP.csv"), row.names = FALSE)
  #############################

  for (indifolder in c(compilationPaths$compilation_db,
                       compilationPaths$compilation_sa,
                       compilationPaths$raw_from_oracle)){
    allfiles_indifolder <- dir(pattern = ".rds", indifolder)
    allfiles_indifolder <- gsub(".rds", "", allfiles_indifolder)

    for (indifile in allfiles_indifolder) {
      thedata <- readRDS(file.path(indifolder, paste0(indifile, ".rds")))
      write.xlsx(thedata,
                 file.path(indifolder, paste0(indifile, ".xlsx")))
    }
  }

  ## generate reports
  cat(paste(Sys.time(), ": Generate reports and save them to report folder.\n", sep = ""))

  # compilationPath <- "D:/ISMC project/ISMC compiler/ismc compiler prod env"
  # compilationPaths <- list()
  # compilationPaths$compilation_db <- "D:/ISMC project/ISMC compiler/ismc compiler prod env/compilation_db"
  # compilationPaths$compilation_report <- "D:/ISMC project/ISMC compiler/ismc compiler prod env/compilation_report"
  # compilationPaths$compilation_last <- "D:/ISMC project/ISMC compiler/ismc compiler prod env/Archive_20210415"
  # compilationDate <- "20210421"

  lastCompilationDate <- gsub(paste0(compilationPath, "/Archive_"), "",
                              compilationPaths$compilation_last)
  rmarkdown::render(input = file.path(compilationPaths$compilation_report,
                                      "general_report.Rmd"),
                    params = list(crtPath = compilationPaths$compilation_db,
                                  lastPath = compilationPaths$compilation_last,
                                  mapPath = compilationPaths$compilation_map,
                                  coeffPath = compilationPaths$compilation_coeff,
                                  compilationDate = compilationDate,
                                  lastCompilationDate = lastCompilationDate,
                                  compilationYear = compilationYear,
                                  sindexVersion = as.numeric(SIndexR::SIndexR_VersionNumber())/100),
                    quiet = TRUE)

  cat(paste(Sys.time(), ": Archive compilation to Archive_", gsub("-", "", Sys.Date()), ".\n", sep = ""))

  file.copy(from = compilationPaths$compilation_sa,
            to = compilationPaths$compilation_archive,
            recursive = TRUE)
  file.copy(from = compilationPaths$compilation_db,
            to = compilationPaths$compilation_archive,
            recursive = TRUE)
  file.copy(from = compilationPaths$raw_from_oracle,
            to = compilationPaths$compilation_archive,
            recursive = TRUE)
  file.copy(from = compilationPaths$compilation_coeff,
            to = compilationPaths$compilation_archive,
            recursive = TRUE)
  file.copy(from = compilationPaths$compilation_map,
            to = compilationPaths$compilation_archive,
            recursive = TRUE)
  file.copy(from = compilationPaths$compilation_report,
            to = compilationPaths$compilation_archive,
            recursive = TRUE)
}
