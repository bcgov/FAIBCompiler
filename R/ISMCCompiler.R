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
#' @param mapSourcePath character, Specifies the path to maps of FIZ, TFL and OWNERSHIP.
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
#'
#' @author Yong Luo
#'

ISMCCompiler <- function(oracleUserName,
                         oraclePassword,
                         oracleEnv = "INT",
                         compilationPath = "//albers/gis_tib/VRI/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/FromRCompiler",
                         mapSourcePath = "//spatialfiles2.bcgov/work/for/vic/hts/dam/workarea/data/infrastructure",
                         equation = "KBEC",
                         walkThru = TRUE,
                         logMinLength = 0.1,
                         stumpHeight = 0.3,
                         breastHeight = 1.3,
                         UTOPDIB = 10,
                         utilLevel = 4,
                         weirdUtil = "No"){
  # rm(list = ls())
  # compilationPath <- "D:/ISMC project/ISMC compiler/ismc compiler development"
  cat(paste(Sys.time(), ": Prepare folders in compilation path.\n", sep = ""))
  compilationPaths <- compilerPathSetup(compilationPath)

  cat(paste(Sys.time(), ": Check requirements for compilation:\n", sep = ""))
  # rm(list = ls())
  # mapSourcePath <- "//spatialfiles2.bcgov/work/for/vic/hts/dam/workarea/data/infrastructure"
  # mapPath <- "D:/ISMC project/ISMC compiler/ismc compiler development/maps"

  maptimes <- checkMaps(mapSourcePath,
                        mapPath = compilationPaths$compilation_map)
  todayDate <- as.Date(Sys.time())
  todayYear <- substr(todayDate, 1, 4)
  if(todayDate >= as.Date(paste0(todayYear, "-01-01")) &
     todayDate < as.Date(paste0(todayYear, "-01-31"))){
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
                      outputPath = compilationPaths$compilation_sa)

  vi_a <- readRDS(file.path(compilationPaths$compilation_sa, "vi_a.rds"))
  cat(paste(Sys.time(), ": Update spatial attributes.\n", sep = ""))
  spatialLookups <- updateSpatial(samplesites = vi_a,
                                  mapPath = compilationPaths$compilation_map,
                                  mapTimes = maptimes)
  saveRDS(spatialLookups,
          file.path(compilationPaths$compilation_sa,
                    "vi_a.rds"))
  todaydate <- gsub("-", "", Sys.Date())

  spatialLookups_simp <- unique(spatialLookups[,.(SITE_IDENTIFIER, SAMP_POINT = SITE_IDENTIFIER,
                                                  IP_UTM, IP_NRTH, IP_EAST, BC_ALBERS_X, BC_ALBERS_Y,
                                                  Longitude, Latitude, BEC, BEC_SBZ, BEC_VAR,
                                                  TSA, TSA_DESC, FIZ, TFL, OWNER, SCHEDULE,
                                                  PROJ_ID, SAMP_NO)],
                                by = "SAMP_POINT")

  spatialLookups_prev <- read.table(file.path(compilationPaths$compilation_coeff,
                                              "spatiallookup.txt"),
                                    sep = ",", header = TRUE) %>% data.table

  write.table(spatialLookups_simp,
              file.path(compilationPaths$compilation_coeff,
                        "spatiallookup.txt"),
              sep = ",",
              row.names = FALSE)
  write.xlsx(spatialLookups_simp,
              file.path(compilationPaths$compilation_coeff,
                        "spatiallookup.xlsx"))
  cat("    Saved spatial attribute table as spatiallookup \n")


  ### 2.1 load cluster/plot header
  clusterplotheader_VRI <- VRIInit_clusterplot(dataSourcePath = compilationPaths$compilation_sa)
  samples <- data.table::copy(clusterplotheader_VRI)

  saveRDS(samples, file.path(compilationPaths$compilation_db, "samples.rds"))
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


  tree_ms1 <- VRIInit_measuredTree(data.table::copy(samples),
                                   compilationPaths$compilation_sa,
                                   walkThru)
  ### 2.3 load vi_d data
  ## vi_d contains call grading data for fully measured trees and enhanced trees
  vi_d <- VRIInit_lossFactor(fullMeasuredTrees = tree_ms1[,.(CLSTR_ID, PLOT, TREE_NO)],
                             dataSourcePath = compilationPaths$compilation_sa)

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
  voltrees <- merge(voltrees, unique(samples[,.(CLSTR_ID, FIZ, BGC_ZONE, BGC_SBZN, BGC_VAR)],
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
                                        samples[,.(CLSTR_ID, PLOT,
                                                   FIZ = as.character(FIZ),
                                                   BGC_ZONE)],
                                        by = c("CLSTR_ID", "PLOT"),
                                        all.x = TRUE)
  tree_ah2 <- siteAgeCompiler(siteAgeData = data.table::copy(tree_ah1))
  saveRDS(tree_ah2, file.path(compilationPaths$compilation_db, "compiled_vi_h.rds"))
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
                                        unique(samples[,.(CLSTR_ID, PROJ_ID, BGC_ZONE, BGC_SBZN, BGC_VAR,
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
  saveRDS(tree_ms7, file.path(compilationPaths$compilation_db,
                              "compiled_vi_c.rds"))
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
    samples_beccls <- unique(samples[,.(CLSTR_ID, BGC_ZONE)], by = "CLSTR_ID")
    alltreelist <- merge(alltreelist, samples_beccls, by = "CLSTR_ID", all.x = TRUE)
    allbecsplvd <- unique(alltreelist[,.(BGC_ZONE, SP0, LV_D)])

    ## if the regratiodata can not be found in coeff folder
    ## generate regratiodata and derive coeff and ratio using mixed effect models
    regRatioData <- regRatioDataSelect(samples, tree_ms7, usage = "ismc")
    saveRDS(regRatioData,
            file.path(compilationPaths$compilation_coeff,
                      paste0("regRatioData", compilationYear, ".rds")))
    cat(paste0("        Selected data and saved to regRatioData", compilationYear, "\n"))
    coefs <- regBA_WSV(regRatioData, needCombs = allbecsplvd)
    saveRDS(coefs$fixedcoeff,
            file.path(compilationPaths$compilation_coeff,
                      paste0("fixedCoefs", compilationYear, ".rds")))
    saveRDS(coefs$randomcoeff,
            file.path(compilationPaths$compilation_coeff,
                      paste0("randomCoefs", compilationYear, ".rds")))
    write.table(coefs$fixedcoeff,
                file.path(compilationPaths$compilation_coeff,
                          paste0("fixedCoefs", compilationYear, ".txt")),
                row.names = FALSE, sep = ",")
    write.table(coefs$randomcoeff,
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
                               merRatioCoef[,.(BGC_ZONE, SP0, LV_D, a, b, c)],
                               by = c("BGC_ZONE", "SP0", "LV_D"),
                               all.x = TRUE)
    HnonenhancedTrees[, MER_RATIO := a * (1 - exp(-b * (DBH-10)))^c]
    HnonenhancedTrees[MEAS_INTENSE == "NON-ENHANCED", VOL_MER := MER_RATIO * VOL_WSV]
    HnonenhancedTrees[, c("a", "b", "c", "MER_RATIO") := NULL]

    HnonenhancedTrees <- merge(HnonenhancedTrees,
                               ntwbRatioCoef[,.(BGC_ZONE, SP0, LV_D, a, b, c)],
                               by = c("BGC_ZONE", "SP0", "LV_D"),
                               all.x = TRUE)
    HnonenhancedTrees[, NTWB_RATIO := a * (1 - exp(-b * (DBH-10)))^c]
    HnonenhancedTrees[, VOL_NTWB := NTWB_RATIO * VOL_WSV]
    HnonenhancedTrees[, c("a", "b", "c", "NTWB_RATIO") := NULL]
    prep_smy <- rbindlist(list(auxtreecompilation$fullenhancedtrees,
                               HnonenhancedTrees),
                          fill = TRUE)
  } else {
    prep_smy <- rbindlist(list(auxtreecompilation$fullenhancedtrees,
                               auxtreecompilation$HnonenhancedTrees),
                          fill = TRUE)
  }

  prep_smy[MEAS_INTENSE %in% c("FULL", "ENHANCED", "H-ENHANCED"), VOL_SRCE := "Calc"]
  prep_smy[is.na(VOL_SRCE), VOL_SRCE := "Unk"]
  prep_smy <- merge(prep_smy, unique(lookup_species()[,.(SPECIES, SP_TYPE)], by = "SPECIES"),
                    by = "SPECIES", all.x = TRUE)
  volVariables <- c(paste("VOL_",c("NET", "MER", "NETM", "NTW2",
                                   "NTWB", "D", "DW", "DWB"),
                          sep = ""), "VAL_MER")
  prep_smy[DBH < 10, c(volVariables) := 0]
  saveRDS(prep_smy[order(CLSTR_ID, PLOT, TREE_NO),],
          file.path(compilationPaths$compilation_db, "treelist.rds"))
  # write.csv(prep_smy[order(CLSTR_ID, PLOT, TREE_NO),],
  #         file.path(compilationPaths$compilation_db, "treelist.csv"), row.names = FALSE)
  rm(auxtreecompilation)

  ## 7. sammarize and save compiled tree-level data at cluster and cluster/species level
  cat(paste(Sys.time(), ": Summarize volume and age.\n", sep = ""))
  vrisummaries <- VRISummaries(allVolumeTrees = data.table::copy(prep_smy),
                               clusterPlotHeader = samples,
                               utilLevel = utilLevel,
                               weirdUtil = weirdUtil,
                               equation = equation)
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
  vi_f[, obslength := length(TOTAL1), by = c("CLSTR_ID", "PLOT", "SPECIES")]
  vi_f <- unique(vi_f, by = c("CLSTR_ID", "PLOT", "SPECIES"))
  vi_f[, clusterplot := paste(CLSTR_ID, "_", PLOT, sep = "")]
  vi_e[, clusterplot := paste(CLSTR_ID, "_", PLOT, sep = "")]
  vi_f <- vi_f[clusterplot %in% unique(vi_e[PL_ORIG == "SML_TR",]$clusterplot),]
  smalltreecompile <- smallTreeVolSmry(smallTreeData = vi_f,
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
