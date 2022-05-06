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
#' @param HTEstimateMethod character, Specifies the method to estimate height based on DBH.
#'                                    Currently it supports \code{bestMEM}, which is best mixed
#'                                    effect model.
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
#' @rdname ISMCCompiler_PSP
#' @importFrom FAIBOracle loadISMC_bySampleType
#' @importFrom FAIBBase merge_dupUpdate
#' @importFrom openxlsx write.xlsx
#' @importFrom SIndexR SIndexR_VersionNumber
#'
#' @author Yong Luo
ISMCCompiler_PSP <- function(oracleUserName,
                             oraclePassword,
                             oracleEnv = "PROD",
                             compilationPath = "//albers/gis_tib/VRI/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/FromRCompiler",
                             equation = "KBEC",
                             HTEstimateMethod = "bestMEM",
                             walkThru = TRUE,
                             logMinLength = 0.1,
                             stumpHeight = 0.3,
                             breastHeight = 1.3,
                             UTOPDIB = 10,
                             utilLevel = 3,
                             weirdUtil = c("2", "4")){

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
  # cat("    Check VOL~BA coefficients and ratios:.\n")
  # if(file.exists(file.path(compilationPaths$compilation_coeff,
  #                          paste0("fixedCoefs", compilationYear, ".rds"))) &
  #    file.exists(file.path(compilationPaths$compilation_coeff,
  #                          paste0("randomCoefs", compilationYear, ".rds"))) &
  #    file.exists(file.path(compilationPaths$compilation_coeff,
  #                          paste0("ratios", compilationYear, ".rds")))){
  #   cat(paste0("        All coefficients and ratios for year ", compilationYear, " are checked.\n"))
  #   needNewCoffs <- FALSE
  # } else {
  #   cat(paste0("        Coefficients and ratios for year ", compilationYear, " are not available\n"))
  #   cat("        and will be calculated by this compiler.\n")
  #   needNewCoffs <- TRUE
  # }
  cat("    Check stand age table from vegcomp:.\n")
  if(!file.exists(file.path(compilationPaths$compilation_coeff,
                            paste0("stand_age_from_vegcomp_dan_", compilationYear, ".xlsx")))){
    stop(paste0("Ask Dan Turner to derive stand age table from vegcomp layer for ", compilationYear, ".\nAnd save it in coeff fold."))
  }
  sampletypes <- c("PSP")

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

  cat(paste(Sys.time(), ": Initiate PSP data for compilation.\n", sep = ""))
  PSPCompilation_Init(inputPath = compilationPaths$raw_from_oracle,
                      outputPath = compilationPaths$compilation_sa)
  vi_a <- readRDS(file.path(compilationPaths$compilation_sa, "vi_a.rds"))
  cat(paste(Sys.time(), ": Update spatial attributes.\n", sep = ""))
  spatialLookups <- updateSpatial(samplesites = vi_a,
                                  mapPath = compilationPaths$compilation_map)
  spatialLookups[, ':='(BEC_source = "fromMap",
                        TSA_source = "fromMap",
                        FIZ_source = "fromMap",
                        TFL_source = "fromMap",
                        OWNER_source = "fromMap")]
  ## for PSP, some samples do not have good spatial coordinates, hence, causing
  ## missing spatial attributes
  spUpdateMethod <- "fromRegionCompartMap"
  if(spUpdateMethod == "fromOldSAS"){
    ## for those samples, the compiler takes those variables from SAS outputs
    spatialLookups <- updateMissingSpAttribute(spatialtable = spatialLookups,
                                               coeffPath = compilationPaths$compilation_coeff,
                                               updateMethod = spUpdateMethod)
  } else if (spUpdateMethod == "fromRegionCompartMap"){
    spatialLookups <- updateMissingSpAttribute(spatialtable = spatialLookups,
                                               mapPath = compilationPaths$compilation_map,
                                               updateMethod = spUpdateMethod)
  }
  saveRDS(spatialLookups,
          file.path(compilationPaths$compilation_sa,
                    "vi_a.rds"))
  spatialLookups_simp <- unique(spatialLookups[,.(SITE_IDENTIFIER, SAMP_POINT = SITE_IDENTIFIER,
                                                  IP_UTM, IP_NRTH, IP_EAST, BC_ALBERS_X, BC_ALBERS_Y,
                                                  Longitude, Latitude, BEC, BEC_SBZ, BEC_VAR,
                                                  TSA, TSA_DESC, FIZ, TFL, OWNER, SCHEDULE,
                                                  PROJ_ID, SAMP_NO, BEC_source, TSA_source, FIZ_source,
                                                  TFL_source, OWNER_source)],
                                by = "SAMP_POINT")
  saveRDS(spatialLookups_simp,
          file.path(compilationPaths$compilation_db,
                    "spatiallookup.rds"))
  cat("    Saved spatial attribute table as spatiallookup \n")


  ### 2.1 load cluster/plot header
  clusterplotheader_PSP <- PSPInit_clusterplot(dataSourcePath = compilationPaths$compilation_sa)
  samples <- data.table::copy(clusterplotheader_PSP)
  samples[,':='(SITE_IDENTIFIER = substr(CLSTR_ID, 1, 7),
                SAMPLE_SITE_PURPOSE_TYPE_CODE = substr(CLSTR_ID, 9, 11))]
  samples[nchar(CLSTR_ID) == 12,
          VISIT_NUMBER := as.numeric(substr(CLSTR_ID, 12, 12))]
  samples[nchar(CLSTR_ID) == 13,
          VISIT_NUMBER := as.numeric(substr(CLSTR_ID, 12, 13))]
  samples[nchar(CLSTR_ID) == 14,
          VISIT_NUMBER := as.numeric(substr(CLSTR_ID, 12, 14))]
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
  saveRDS(samples,
          file.path(compilationPaths$compilation_db, "samples.rds"))
  # write.csv(samples, file.path(compilationPaths$compilation_db, "samples.csv"), row.names = FALSE)

  rm(clusterplotheader_PSP)
  ### 2.2 load vi_c data
  ## vi_c contains the trees of: 1) fully measured trees in IPC (trees have dbh, height and call grading)
  ##                             2) enhanced trees in auxi plots (trees have dbh, height and call grading)
  ##                             3) H-enhanced trees in auxi plots (trees have dbh, height)
  ##                             4) B-sample trees in fixed area lidar projects (trees have dbh, height)

  tree_ms1 <- PSPInit_measuredTree(data.table::copy(samples),
                                   compilationPaths$compilation_sa)

  ### 2.3 load vi_d data
  ## vi_d contains call grading data for fully measured trees and enhanced trees
  vi_d <- PSPInit_lossFactor(fullMeasuredTrees = tree_ms1[,.(CLSTR_ID, PLOT, TREE_NO)],
                             dataSourcePath = compilationPaths$compilation_sa)
  vi_d_temp <- readRDS(file.path(compilationPaths$compilation_sa, "vi_d.rds"))
  saveRDS(vi_d_temp,
          file.path(compilationPaths$compilation_db, "compiled_vi_d.rds"))
  ### 2.4 load vi_i data
  ## vi_i has trees in auxi plots without height information (mostly), however, some of these trees are also in vi_c
  tree_nonHT <- PSPInit_nonHTTree(data.table::copy(samples),
                                  compilationPaths$compilation_sa)

  tree_nonHT <- merge(tree_nonHT, unique(lookup_species()[,.(SPECIES, SP0)], by = "SPECIES"),
                      by = "SPECIES")
  ### 2.5 load vi_h data
  ## vi_h data is the site age trees
  tree_ah1 <- PSPInit_siteTree(data.table::copy(samples),
                               compilationPaths$compilation_sa)

  tree_ms1[BROKEN_TOP_IND != "Y",
           MEAS_INTENSE := "H-ENHANCED"] # for the non-broken top trees, tree height is tree length
  tree_ms1[BROKEN_TOP_IND == "Y",
           MEAS_INTENSE := "NON-ENHANCED"] # for the broken top trees,
  # tree height needs to be estimated using either projected height
  # or DBH-height equations

  tree_nonHT[, MEAS_INTENSE := "NON-ENHANCED"]
  tree_all <- rbindlist(list(tree_ms1, tree_nonHT),
                        fill = TRUE)
  tree_all <- merge(tree_all,
                    unique(samples[,.(CLSTR_ID, SITE_IDENTIFIER,
                                      VISIT_NUMBER,
                                      MEAS_DT,
                                      BEC_ZONE,
                                      BEC_SBZ)],
                           by = "CLSTR_ID"),
                    by = "CLSTR_ID",
                    all.x = TRUE)
  tree_all <- merge(tree_all, vi_d[,.(CLSTR_ID, TREE_NO, full = TRUE)],
                    by = c("CLSTR_ID", "TREE_NO"),
                    all.x = TRUE)
  tree_all[MEAS_INTENSE == "NON-ENHANCED" & full == TRUE]
  tree_all[MEAS_INTENSE == "H-ENHANCED" & full == TRUE,
           MEAS_INTENSE := "FULL"]

  if(HTEstimateMethod == "bestMEM"){
    height_dbh_coeff <- read.csv(file.path(compilationPaths$compilation_coeff,
                                           "best_height_models.csv")) %>%
      data.table
  }

  ## fit height for nonHT trees
  nonHtrees <- pspHT(treeData = tree_all[MEAS_INTENSE == "NON-ENHANCED",],
                     method = HTEstimateMethod,
                     coeffs = height_dbh_coeff)

  alltrees <- rbindlist(list(nonHtrees, tree_all[MEAS_INTENSE != "NON-ENHANCED",]),
                        fill = TRUE)
  alltrees[is.na(HEIGHT_SOURCE),
           HEIGHT_SOURCE := "Measured"]
  ### 3. vi_c compilation
  tree_ms6 <- PSPVolTree(treeData = data.table::copy(alltrees),
                         equation = equation,
                         logMinLength = logMinLength,
                         stumpHeight = stumpHeight,
                         breastHeight = breastHeight,
                         UTOPDIB = UTOPDIB,
                         HTEstimateMethod = HTEstimateMethod,
                         htDBHCoeff = height_dbh_coeff)

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
                                        unique(samples[,.(CLSTR_ID, PROJ_ID, BEC_ZONE, BEC_SBZ, BEC_VAR,
                                                          TSA, TYPE_CD)],
                                               by = "CLSTR_ID"),
                                        by = "CLSTR_ID",
                                        all.x = TRUE)



  tree_ms7 <- DWBCompiler_PSP(treeMS = tree_ms6,
                              siteAge = unique(siteAgeTable, by = "CLSTR_ID"),
                              treeLossFactors = vi_d,
                              equation = "KBEC")


  vi_c_sa <- readRDS(file.path(compilationPaths$compilation_sa,
                               "vi_c.rds"))
  tree_ms7[, ADJ_ID := NULL]
  prep_smy <- merge(tree_ms7,
                    vi_c_sa[,.(CLSTR_ID, PLOT, TREE_NO,
                               CR_CL, WALKTHRU_STATUS,
                               SECTOR, RESIDUAL,
                               HT_BRCH)],
                    by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                    all.x = TRUE)
  saveRDS(prep_smy, file.path(compilationPaths$compilation_db,
                              "compiled_vi_c.rds"))
  # write.csv(tree_ms7, file.path(compilationPaths$compilation_db,
  #                             "compiled_vi_c.csv"), row.names = FALSE)
  rm(vi_d, siteAgeTable, tree_ms6)


  prep_smy <- merge(prep_smy, unique(lookup_species()[,.(SPECIES, SP_TYPE)], by = "SPECIES"),
                    by = "SPECIES", all.x = TRUE)
  volVariables <- c(paste("VOL_",c("MER", "NETM", "NTW2",
                                   "NTWB", "D", "DW", "DWB"),
                          sep = ""))
  prep_smy[DBH < 10, c(volVariables) := 0]
  saveRDS(prep_smy[order(CLSTR_ID, PLOT, TREE_NO),],
          file.path(compilationPaths$compilation_db, "treelist.rds"))
  # write.csv(prep_smy[order(CLSTR_ID, PLOT, TREE_NO),],
  #         file.path(compilationPaths$compilation_db, "treelist.csv"), row.names = FALSE)
  ## 7. sammarize and save compiled tree-level data at cluster and cluster/species level
  cat(paste(Sys.time(), ": Summarize volume and age.\n", sep = ""))
  prep_smy[, ':='(VOL_NET = 0,
                  VAL_MER = 0)]
  prep_smy[is.na(S_F), S_F := "S"]
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
  smalltreecompile <- smallTreeSmry(smallTreeData = vi_f,
                                    smallTreePlotHeader = vi_e[PL_ORIG == "SML_TR",])
  saveRDS(smalltreecompile$clusterSummaries,
          file.path(compilationPaths$compilation_db, "Smries_smallTree_byCL.rds"))
  saveRDS(smalltreecompile$clusterSpeciesSummaries,
          file.path(compilationPaths$compilation_db, "Smries_smallTree_byCLSP.rds"))
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
  #############################

  for (indifolder in c(compilationPaths$compilation_db,
                       compilationPaths$compilation_sa,
                       compilationPaths$raw_from_oracle)){
    allfiles_indifolder <- dir(pattern = ".rds", indifolder)
    allfiles_indifolder <- gsub(".rds", "", allfiles_indifolder)

    for (indifile in allfiles_indifolder) {
      thedata <- readRDS(file.path(indifolder, paste0(indifile, ".rds")))
      write.csv(thedata,
                file.path(indifolder, paste0(indifile, ".csv")),
                na = "",
                row.names = FALSE)
    }
  }

  ## generate reports
  # cat(paste(Sys.time(), ": Generate reports and save them to report folder.\n", sep = ""))
  #
  # lastCompilationDate <- gsub(paste0(compilationPath, "/Archive_"), "",
  #                             compilationPaths$compilation_last)
  # rmarkdown::render(input = file.path(compilationPaths$compilation_report,
  #                                     "general_report.Rmd"),
  #                   params = list(crtPath = compilationPaths$compilation_db,
  #                                 lastPath = compilationPaths$compilation_last,
  #                                 mapPath = compilationPaths$compilation_map,
  #                                 coeffPath = compilationPaths$compilation_coeff,
  #                                 compilationDate = compilationDate,
  #                                 lastCompilationDate = lastCompilationDate,
  #                                 compilationYear = compilationYear,
  #                                 sindexVersion = as.numeric(SIndexR::SIndexR_VersionNumber())/100),
  #                   quiet = TRUE)

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
