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
#' @param syncTo character, Specifies the path, i.e., network drive, that user wants to share the compilation outputs
#'                          with coworkers.
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
#' @param download logical, Specifies if the data from ISMC need to be downloaded. Default is \code{TRUE}, which m
#'                          means need download.
#' @param saveCSV logical, Specifies if the outputs need to be saved into CSV. Default is save.
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
#' @importFrom data.table ':=' fwrite
#' @importFrom dplyr left_join
#' @importFrom fpCompare %>>% %<<% %==% %!=% %<=% %>=%
#' @importFrom FAIBBase heightEstimateForBTOP_D heightEstimateForBTOP_H treeVolCalculator
#' @importFrom FAIBBase heightEstimateForBTOP_D heightEstimateForBTOP_H treeVolCalculator
#' @importFrom parallel detectCores makeCluster clusterExport parLapply stopCluster
#' @author Yong Luo
ISMCCompiler <- function(compilationType,
                         ismcUserName,
                         ismcPassword,
                         oracleEnv = "PROD",
                         bcgwUserName,
                         bcgwPassword,
                         compilationPath,
                         syncTo = as.character(NA),
                         equation = "KBEC",
                         walkThru = TRUE,
                         logMinLength = 0.1,
                         stumpHeight = 0.3,
                         breastHeight = 1.3,
                         UTOPDIB = 10,
                         utilLevel = 4,
                         weirdUtil = "4",
                         download = TRUE,
                         saveCSV = TRUE,
                         recompile = FALSE,
                         archiveDate = as.character(NA)){
  if(!(compilationType %in% c("PSP", "nonPSP"))){
    stop("The compilationType must be either PSP or nonPSP")
  }
  cat(paste(substr(Sys.time(), 1, 16), ": Prepare folders in compilation path.\n", sep = ""))
  compilationDate <- gsub("-", "", Sys.Date())
  compilationPaths <- compilerPathSetup_new(compilationPath,
                                            compilationDate,
                                            compilationType,
                                            recompile = recompile,
                                            download = download,
                                            archiveDate = archiveDate)

  cat(paste(substr(Sys.time(), 1, 16), ": Check requirements for compilation:\n", sep = ""))
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
                           paste0("bestmodels_BA_WSV", compilationYear, ".rds"))) &
     file.exists(file.path(compilationPaths$compilation_coeff,
                           paste0("ratios", compilationYear, ".rds")))){
    cat(paste0("        All coefficients and ratios for year ", compilationYear, " are checked.\n"))
    needNewCoffs <- FALSE
  } else {
    cat(paste0("        Coefficients and ratios for year ", compilationYear, " are not available\n"))
    cat("        and will be calculated by this compiler.\n")
    needNewCoffs <- TRUE
  }
  cat("    Check DBH-Height models:.\n")
  if(file.exists(file.path(compilationPaths$compilation_coeff,
                           paste0("bestmodels_ht_dbh_bysp", compilationYear, ".rds")))){
    cat(paste0("        DBH-Height models for year ", compilationYear, " are checked.\n"))
    newDBH_HTfit <- FALSE
  } else {
    cat(paste0("        DBH-Height models for year ", compilationYear, " are not available\n"))
    cat("        and will be developed by this compiler.\n")
    newDBH_HTfit <- TRUE
  }


  if(recompile == FALSE){
    if(download == TRUE){
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
      cat(paste(substr(Sys.time(), 1, 16), ": Load data from ISMC database.\n", sep = ""))
      FAIBOracle::loadISMC_bySampleType(userName = ismcUserName,
                                        passWord = ismcPassword,
                                        env = toupper(oracleEnv),
                                        sampleType = sampletypes,
                                        savePath = compilationPaths$raw_from_oracle,
                                        saveFormat = "rds",
                                        overWrite = TRUE)
    } else {
      cat(paste(substr(Sys.time(), 1, 16), ": Loading data from ISMC database is turned off.\n", sep = ""))
    }
  } else {
    downloaddate <- dir(compilationPaths$raw_from_oracle, pattern = "AccessNotes.rds")
    downloaddate <- gsub("_AccessNotes.rds", "", downloaddate)
    cat(paste(substr(Sys.time(), 1, 16), paste0(": The compiler recompiles existing raw data: ", downloaddate, "\n"), sep = ""))
  }
  cat(paste(substr(Sys.time(), 1, 16), ": Prepare sample data for compilation.\n", sep = ""))
  dataPrepSample(compilationType = compilationType,
                 inputPath = compilationPaths$raw_from_oracle,
                 outputPath = compilationPaths$compilation_sa,
                 coeffPath = compilationPaths$compilation_coeff)
  cat(paste(substr(Sys.time(), 1, 16), ": Compile sample and plot information.\n", sep = ""))
  samplePlotResults <- samplePlotCompilation(compilationType = compilationType,
                                             dataSourcePath = compilationPaths$compilation_sa,
                                             mapPath = compilationPaths$compilation_map,
                                             coeffPath = compilationPaths$compilation_coeff)
  saveRDS(samplePlotResults$sampleplots,
          file.path(compilationPaths$compilation_db, "sample_plot_header.rds"))
  cat("    Saved compiled sample plot information. \n")
  cat(paste(substr(Sys.time(), 1, 16), ": Update stand age from vegcomp layer.\n", sep = ""))
  sample_site_msmt <- updateSA_vegcomp(compilationType = compilationType,
                                       coeffPath = compilationPaths$compilation_coeff,
                                       bcgwUserName = bcgwUserName,
                                       bcgwPassword = bcgwPassword,
                                       sampleSites = samplePlotResults$samplesites,
                                       sampleMsmts = samplePlotResults$samplevisits)
  saveRDS(sample_site_msmt$sampleSites,
          file.path(compilationPaths$compilation_db,
                    "sample_site_header.rds"))
  cat("    Saved compiled site information. \n")

  cat(paste(substr(Sys.time(), 1, 16), ": Prepare tree data for compilation.\n", sep = ""))
  sampleMsmts <- dataPrepTree(compilationType = compilationType,
                              inputPath = compilationPaths$raw_from_oracle,
                              outputPath = compilationPaths$compilation_sa,
                              coeffPath = compilationPaths$compilation_coeff,
                              sampleMsmts = sample_site_msmt$sampleMsmts)
  samples <- data.table::copy(sampleMsmts)
  samples_tmp <- unique(samples[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, VISIT_TYPE,
                                   FIRST_MSMT, LAST_MSMT, MEAS_DT,
                                   SAMPLE_SITE_PURPOSE_TYPE_CODE = TYPE_CD, SAMPLE_SITE_PURPOSE_TYPE_DESCRIPTION,
                                   MEAS_YR, PERIOD,
                                   SAMP_TYP, NO_PLOTS, PROJ_ID, SAMP_NO, SAMPLE_BREAK_POINT,
                                   SAMPLE_BREAK_POINT_TYPE, DBH_LIMIT_COUNT = DBHLIMIT_COUNT,
                                   DBH_LIMIT_TAG, SA_VEGCOMP, SA_VEGCOMP_SOURCE,
                                   NO_LIVE_TREE_MAPPED, NO_TOTAL_LIVE_TREE,
                                   STEM_MAPPED_SAMPLE)],
                        by = "CLSTR_ID")
  saveRDS(samples_tmp,
          file.path(compilationPaths$compilation_db,
                    "sample_msmt_header.rds"))
  rm(samples_tmp, sampleMsmts)
  cat("    Saved compiled sample visit information. \n")


  cat(paste(substr(Sys.time(), 1, 16), ": Assign component change at tree level.\n", sep = ""))
  ## the species correct depends on BEC and BECsubzone, hence should be
  ## done after the bec zone information updated
  treemsmt <- readRDS(file.path(compilationPaths$compilation_sa,
                                "treemeasurements.rds"))

  treemsmt_rep <- merge(treemsmt,
                        unique(samples[,.(CLSTR_ID, VISIT_TYPE)],
                               by = "CLSTR_ID"),
                        by = "CLSTR_ID",
                        all.x = TRUE)
  treemsmt_rep <- treemsmt_rep[VISIT_TYPE == "REP",]
  treelist_compchange <- assignChangeComponent(treelist = treemsmt_rep,
                                               samples = samples[VISIT_TYPE == "REP",])
  saveRDS(treelist_compchange,
          file.path(compilationPaths$compilation_db,
                    "component_change_treelevel.rds"))
  rm(treelist_compchange, treemsmt_rep)
  gc()
  treelist_db <- treemsmt[,.(CLSTR_ID,
                             SITE_IDENTIFIER, VISIT_NUMBER,
                             PLOT, TREE_NO = TREE_NUMBER,
                             OUT_OF_PLOT_IND,
                             SPECIES = TREE_SPECIES_CODE, SP0,
                             DBH = DIAMETER,
                             HEIGHT = LENGTH,
                             HT_PROJ = PROJECTED_HEIGHT,
                             LV_D = TREE_EXTANT_CODE,
                             S_F = TREE_STANCE_CODE,
                             STEM_MAP_BEARING, STEM_MAP_DISTANCE,
                             TAGGING_SECTOR_NO = TAGGING_SECTOR_NUMBER,
                             SITE_SECTOR_NO = SITE_SECTOR_NUMBER,
                             BROKEN_TOP_IND,
                             SUIT_SI = SUITABLE_FOR_SITE_INDEX_IND,
                             SUIT_HT = SUITABLE_FOR_HEIGHT_IND,
                             SUIT_AGE = SUITABLE_FOR_AGE_IND,
                             CROWN_CLASS_CODE,
                             RESIDUAL_IND,
                             TREE_CLASS_CODE,
                             STOP,
                             TREE_MEASUREMENT_COMMENT,
                             MEASUREMENT_ANOMALY_CODE,
                             AGE_MEASURE_CODE,
                             LVD_EDIT, DIAMETER_EDIT, HEIGHT_EDIT,
                             MSMT_MISSING_EDIT,
                             BTOP_EDIT, CRCL_EDIT,
                             RESIDUAL_EDIT,
                             SP_EDIT, DIAM_MSMT_HT_EDIT,
                             SUIT_HT_EDIT, SUIT_SI_EDIT,
                             LAB_AGE_EDIT)]
  saveRDS(treelist_db,
          file.path(compilationPaths$compilation_db,
                    "treelist.rds"))
  rm(treelist_db, treemsmt)
  gc()


  cat(paste(substr(Sys.time(), 1, 16), ": Compile tree-level WSV_VOL and MER_VOL for volume trees.\n", sep = ""))
  ## The volume trees are in two files: vi_c and vi_i
  ##
  ## the vi_c contains the trees of: 1) fully measured trees in IPC (trees have dbh, height and call grading)
  ##                             2) enhanced trees in auxi plots (trees have dbh, height and call grading)
  ##                             3) H-enhanced trees in auxi plots (trees have dbh, height)
  ##                             4) B-sample trees in fixed area lidar projects (trees have dbh, height)
  ## the vi_i contains trees in auxi plots without height information,
  ##
  samples <- merge(samplePlotResults$sampleplots,
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
  saveRDS(tree_callGrading,
          file.path(compilationPaths$compilation_sa, "vi_d_tmp.rds"))

  rm(tree_ms1, tree_callGrading, tree_nonHT)
  # treat broken top trees
  if(newDBH_HTfit){
    cat(paste0("    Start to develop DBH-Height models for year ", compilationYear, ".\n"))
    DBH_Height_MEM(compilationPath = compilationPath,
                   coeffSavePath = compilationPaths$compilation_coeff,
                   fityear = compilationYear)
  }
  HTEstimateMethod <-  "bestMEM"
  if(HTEstimateMethod == "bestMEM"){
    best_height_models <- readRDS(file.path(compilationPaths$compilation_coeff,
                                            paste0("bestmodels_ht_dbh_bysp", compilationYear, ".rds")))
    ## for all the trees calculate tree heights based on Mixed effect model
    treelist_db <- readRDS(file.path(compilationPaths$compilation_db,
                                     "treelist.rds"))
    treelist_db[, unitreeid := paste0(SITE_IDENTIFIER, "-", PLOT, "-", TREE_NO)]
    treelist_db[,':='(HT_TOTAL_EST = round(heightEstimate_mixedEffect_nlme(siteID = SITE_IDENTIFIER,
                                                                           unitreeid = unitreeid,
                                                                           species = SPECIES, DBH = DBH,
                                                                           fixedEffects = best_height_models$fixed_best,
                                                                           randomEffects_site = best_height_models$random_best_site,
                                                                           randomEffects_tree = best_height_models$random_best_tree),
                                           1))]
    rm(best_height_models)
    treelist_db[, unitreeid := NULL]
    if(compilationType == "PSP"){
      ## as suggested by Rene, as the quality of projected height is bad for PSPs,
      ## even though we have projected height for broken top trees,
      ## we should not use it
      treelist_db[, HT_PROJ := NA] ## force projected height as NA for PSP
    }
    # for btop trees, if field projected height is available,
    ## use this as HT_TOTAL
    treelist_db[BROKEN_TOP_IND == "Y" &
                  !is.na(HT_PROJ),
                ':='(HT_TOTAL = round(FAIBBase::heightEstimateForBTOP_H(HT_PROJ), 1),
                     HT_TOTAL_SOURCE = "Field projected")]
    treelist_db[BROKEN_TOP_IND == "Y" &
                  is.na(HT_PROJ),
                ':='(HT_TOTAL = HT_TOTAL_EST,
                     HT_TOTAL_SOURCE = "Estimated based on DBH")]
    treelist_db[!is.na(HEIGHT) &
                  BROKEN_TOP_IND == "N",
                ':='(HT_TOTAL = HEIGHT,
                     HT_TOTAL_SOURCE = "Field measured")]
    treelist_db[!is.na(HT_TOTAL_EST) & is.na(HT_TOTAL),
                ':='(HT_TOTAL = HT_TOTAL_EST,
                     HT_TOTAL_SOURCE = "Estimated based on DBH")]
    saveRDS(treelist_db,
            file.path(compilationPaths$compilation_db, "treelist.rds"))
    # treat nonHT trees
    ## only PSP needs to calculate height for the volume calculation
    ## while nonPSP is used regression method for whole stem volumen estimate based on BA
    if(compilationType == "PSP"){
      voltrees <- rbindlist(list(alltrees$fullDimTrees, alltrees$nonHTTrees),
                            fill = TRUE)
    } else {
      voltrees <- alltrees$fullDimTrees
      nonHTTrees <- alltrees$nonHTTrees
    }

    # this is the best height-dbh model in the mixed effect model forms
    voltrees <- merge(voltrees,
                      treelist_db[,.(CLSTR_ID, PLOT, TREE_NO,
                                     HT_TOTAL, HT_TOTAL_SOURCE)],
                      by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                      all.x = TRUE)
    rm(alltrees, treelist_db)
    gc()
    voltrees[BROKEN_TOP_IND == "Y" & HT_BTOP %==% 1.3, HT_BTOP := 1.4]
    voltrees[BROKEN_TOP_IND == "Y" & (HT_TOTAL < HEIGHT | is.na(HT_TOTAL)),
             HT_TOTAL := HEIGHT]
  } else {
    # to allow using SAS routine to calculate Height based on DBH
  }

  # enable parallel computation
  # calculate whole stem volume and merchantable volume
  voltrees[,':='(full = NULL,
                 HT_TOTAL_SOURCE = NULL,
                 RESIDUAL = NULL,
                 SITE_SECTOR_NO = NULL,
                 TAGGING_SECTOR_NO = NULL,
                 TREE_PLANTED_IND = NULL,
                 HT_PROJ = NULL)]
  gc()
  allclusters <- unique(voltrees$CLSTR_ID)

  numCore <- as.integer(0.5*detectCores())
  clusterInFunction <- makeCluster(numCore)
  numofrow <- as.integer(length(allclusters)/numCore)
  voltrees_list <- list()
  for (indicore in 1:numCore) {
    if(indicore != numCore){
      indiclusters <- allclusters[((indicore-1)*numofrow+1):(indicore*numofrow)]
    } else {
      indiclusters <- allclusters[((indicore-1)*numofrow+1):(length(allclusters))]
    }
    indivoltrees <- voltrees[CLSTR_ID %in% indiclusters,]
    voltrees_list[[indicore]] <- list("voltrees" = indivoltrees,
                                      "compilationType" = compilationType,
                                      "logMinLength" = logMinLength,
                                      "stumpHeight" = stumpHeight,
                                      "breastHeight" = breastHeight,
                                      "UTOPDIB" = 10)
    rm(indivoltrees, indiclusters)
  }
  rm(voltrees)
  gc()
  clusterExport(clusterInFunction,
                varlist = c("grossVolCal_kozak",
                            "data.table", "setnames", ":=",
                            "left_join", "%>>%", "%<<%", "%==%",
                            "%!=%", "%<=%", "%>=%",
                            "heightEstimateForBTOP_D", "heightEstimateForBTOP_H",
                            "treeVolCalculator"),
                envir = environment())
  allresults <- parLapply(cl = clusterInFunction,
                          voltrees_list,
                          function(x){grossVolCal_kozak(compilationType = x$compilationType,
                                                        fullDimTreeData = data.table::copy(x$voltrees),
                                                        logMinLength = x$logMinLength,
                                                        stumpHeight = x$stumpHeight,
                                                        breastHeight = x$breastHeight,
                                                        UTOPDIB = x$UTOPDIB)})
  rm(voltrees_list)
  stopCluster(clusterInFunction)
  for (i in 1:length(allresults)) {
    if(i == 1){
      tree_ms6 <- allresults[[i]]
    } else {
      tree_ms6 <- rbind(tree_ms6, allresults[[i]])
    }
  }
  rm(allresults,
     numCore, clusterInFunction, indicore, i,
     numofrow)
  gc()

  tree_ms6[, WSV_VOL_SRCE := "Calculated"]
  if(compilationType == "nonPSP"){
    tree_ms6 <- rbindlist(list(tree_ms6, nonHTTrees), fill = TRUE)
  }
  cat(paste(substr(Sys.time(), 1, 16), ": Compile age trees.\n", sep = ""))
  ## vi_h data is the site age trees
  tree_ah1 <- readRDS(file.path(compilationPaths$compilation_sa, "vi_h.rds"))
  ## if a tree was bored multiple times, the age from the last measurements will be used
  ## to adjust the previous measurements
  tree_ah1 <- vihPrep(msmtInterval = unique(samples[,.(CLSTR_ID, MEAS_YR)],
                                            by = "CLSTR_ID"),
                      siteAgeTrees = data.table::copy(tree_ah1))
  treelist_db <- readRDS(file.path(compilationPaths$compilation_db,
                                   "treelist.rds"))
  if(compilationType == "PSP"){
    tree_ah1 <- merge(tree_ah1,
                      treelist_db[,.(CLSTR_ID, PLOT, TREE_NO, HT_TOTAL, HT_TOTAL_SOURCE)],
                      by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                      all.x = TRUE)
    tree_ah1[is.na(TREE_LEN), TREE_LEN := HT_TOTAL]
    tree_ah1[,':='(HT_TOTAL = NULL)]
  } else {
    tree_ah1[, HT_TOTAL_SOURCE := "Field measured"]
  }
  tree_ah1 <- merge(tree_ah1,
                    unique(samples[,.(CLSTR_ID,
                                      FIZ = as.character(FIZ),
                                      BEC_ZONE)],
                           by = c("CLSTR_ID")),
                    by = c("CLSTR_ID"),
                    all.x = TRUE)
  # if &bec_in in ('CWH','CDF','MH') then bec_i_c = 'C';
  # else bec_i_c = 'I';
  tree_ah1[is.na(FIZ),
           FIZ := ifelse(BEC_ZONE %in% c("CWH", "CDF", "MH"), "C", "I")]
  tree_ah2 <- siteAgeCompiler(siteAgeData = data.table::copy(tree_ah1),
                              compilationType = compilationType)
  tree_ah2_temp <- data.table::copy(tree_ah2)
  tree_ah2_temp[,c("FIZ", "BEC_ZONE", "REGION_IC", "SI_SP",
                   "RESIDUAL") := NULL]
  treelist_db <- merge(treelist_db,
                       tree_ah2_temp[,.(CLSTR_ID, PLOT, TREE_NO,
                                        TH_TREE, TP_TREE, RA_TREE,
                                        BORED_HT, BORED_AGE_FINAL,
                                        BORED_AGE_SOURCE, SI_TREE,
                                        CURVE_SOURCE, CURVE_NAME,
                                        AGE_BH, AGE_TO_BH, AGE_TOT,
                                        BORED_AGE_FLAG,
                                        SITE_TREE = "Y")],
                       by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                       all.x = TRUE)
  saveRDS(treelist_db, file.path(compilationPaths$compilation_db,
                                 "treelist.rds"))
  saveRDS(tree_ah2_temp, file.path(compilationPaths$compilation_db,
                                   "compiled_vi_h.rds"))
  # site age and site index summaries are different between nonPSP and PSP data
  if(compilationType == "nonPSP"){
    rm(tree_ah2_temp, treelist_db)
    siteAgeSummaries <- siteAgeSummary(tree_ah2)
    cl_ah <- siteAgeSummaries$cl_ah
    saveRDS(cl_ah, file.path(compilationPaths$compilation_db,
                             "Smries_siteAge_byCL.rds"))
    saveRDS(siteAgeSummaries$spc_ah,
            file.path(compilationPaths$compilation_db, "Smries_siteAge_byCLSP.rds"))
    rm(siteAgeSummaries, tree_ah1, tree_ah2)
  } else {
    rm(tree_ah2_temp)
    # to summarize site age and site index for PSP
    treelist_db <- merge(treelist_db,
                         samplePlotResults$sampleplots[,.(CLSTR_ID, PLOT, PLOT_AREA_MAIN )],
                         by = c("CLSTR_ID", "PLOT"),
                         all.x = TRUE)
    treelist_db <- merge(treelist_db,
                         unique(samplePlotResults$samplevisits[,.(CLSTR_ID, MEAS_YR)]),
                         by = "CLSTR_ID",
                         all.x = TRUE)
    voltrees_mark <- tree_ms6[,.(CLSTR_ID, PLOT, TREE_NO,
                                 volumeTree = TRUE)]
    treelist_db <- merge(treelist_db,
                         voltrees_mark,
                         by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                         all.x = TRUE)
    treelist_db <- treelist_db[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER,
                                  PLOT, TREE_NO,
                                  SPECIES,
                                  LV_D,
                                  TAGGING_SECTOR_NO,
                                  SITE_SECTOR_NO,
                                  DBH,
                                  BA_TREE = pi*((DBH/200)^2),
                                  SUIT_SI, SUIT_HT, SUIT_AGE,
                                  HT_TOTAL,
                                  HT_TOTAL_SOURCE,
                                  RESIDUAL = RESIDUAL_IND,
                                  BTOP = BROKEN_TOP_IND,
                                  CROWN_CLASS_CODE,
                                  # TOP_HEIGHT_TREE_IND,
                                  TREE_CLASS_CODE,
                                  PLOT_AREA_MAIN,
                                  MEAS_YR,
                                  volumeTree)]
    # for debug
    saveRDS(tree_ah2,
            file.path(compilationPaths$compilation_sa, "tree_ah2.rds"))
    siteAgeSummaries <- siteAgeSummary_PSP(cpldSiteAgeData = tree_ah2,
                                           treemsmt = treelist_db)
    cl_ah <- siteAgeSummaries$smry_by_cl
    saveRDS(siteAgeSummaries$smry_by_cl,
            file.path(compilationPaths$compilation_db,
                      "Smries_siteAge_byCL.rds"))
    saveRDS(siteAgeSummaries$smry_by_clsp,
            file.path(compilationPaths$compilation_db,
                      "Smries_siteAge_byCLSP.rds"))
    saveRDS(siteAgeSummaries$tree_order,
            file.path(compilationPaths$compilation_sa,
                      "treelist_for_siteage_smry.rds"))
    rm(treelist_db, siteAgeSummaries, tree_ah1, tree_ah2,
       voltrees_mark)
  }


  ######################
  ######################
  ### 5. start the decay, waste and breakage calculation for full/enhanced trees in vi_c
  cat(paste(substr(Sys.time(), 1, 16), ": Compile DWB.\n", sep = ""))
  tree_ms6 <- FAIBBase::merge_dupUpdate(tree_ms6,
                                        unique(samples[,.(CLSTR_ID, PROJ_ID,
                                                          TSA)],
                                               by = "CLSTR_ID"),
                                        by = "CLSTR_ID",
                                        all.x = TRUE)
  tree_callGrading <- readRDS(file.path(compilationPaths$compilation_sa,
                                        "vi_d_tmp.rds"))
  file.remove(file.path(compilationPaths$compilation_sa,
                        "vi_d_tmp.rds"))
  if(compilationType == "nonPSP"){
    siteAgeTable <- FAIBBase::merge_dupUpdate(cl_ah[,.(CLSTR_ID, AT_M_TLS = AT_M_TLSO, AT_M_TXO)],
                                              unique(samples[,.(CLSTR_ID, PROJ_ID, SAMP_NO, TYPE_CD)],
                                                     by = "CLSTR_ID"),
                                              by = "CLSTR_ID",
                                              all.x = TRUE)
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
    # adjust meanage for dwb
    # if tot_stand_age < 0 then
    # agem_grp = 50 + (meas_yr - meas_yr_first);
    # else
    #   agem_grp = tot_stand_age + 50
    siteAgeTable <- merge(unique(samples[,.(CLSTR_ID, SITE_IDENTIFIER, PROJ_ID,
                                            SAMP_NO, TYPE_CD,
                                            MEAS_YR)],
                                 by = "CLSTR_ID"),
                          cl_ah[,.(CLSTR_ID,
                                   TOT_STAND_AGE,
                                   AT_M_TXO = as.numeric(NA))],
                          by = "CLSTR_ID",
                          all.x = TRUE)
    siteAgeTable[, MEAS_YR_FIRST := min(MEAS_YR),
                 by = "SITE_IDENTIFIER"]
    siteAgeTable[is.na(TOT_STAND_AGE),
                 AT_M_TLS := 50 + (MEAS_YR - MEAS_YR_FIRST)]
    siteAgeTable[!is.na(TOT_STAND_AGE),
                 AT_M_TLS := 50 + TOT_STAND_AGE]
    siteAgeTable[, ':='(SITE_IDENTIFIER = NULL,
                        MEAS_YR = NULL,
                        MEAS_YR_FIRST = NULL,
                        TOT_STAND_AGE = NULL)]
    tree_ms7 <- DWBCompiler(compilationType = compilationType,
                            treeMS = tree_ms6,
                            siteAge = unique(siteAgeTable, by = "CLSTR_ID"),
                            treeLossFactors = tree_callGrading)
    tree_ms7[,':='(TSA = NULL,
                   TYPE_CD = NULL,
                   NET_FCT_METHOD = "JF_reg")]
  }
  rm(tree_callGrading)
  vi_c_sa <- readRDS(file.path(compilationPaths$compilation_sa,
                               "vi_c.rds"))
  tree_ms7[, ADJ_ID := NULL]
  tree_ms7 <- merge(tree_ms7,
                    vi_c_sa[,.(CLSTR_ID, PLOT, TREE_NO,
                               CR_CL, WALKTHRU_STATUS, HT_BRCH)],
                    by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                    all.x = TRUE)
  tree_ms7[DIB_STUMP < DIB_BH,
           DIB_STUMP := DIB_BH] # see rene's comment on this as per communication on April 19, 2022
  tree_ms7[(HEIGHT - HT_BRCH) < 0,
           HT_BRCH := NA]
  if(compilationType == "PSP"){
    prep_smy <- data.table::copy(tree_ms7)
    rm(tree_ms7, siteAgeTable, tree_ms6)
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
    rm(tree_ms7_temp, siteAgeTable, tree_ms6)
  }
  gc()

  #######
  ### 6. start to calculate tree volume components for H-enhanced and non-enhanced trees in auxi plots
  ### this is specific to nonPSP compilation
  if(compilationType == "nonPSP"){
    cat(paste(substr(Sys.time(), 1, 16), ": Compile NON-/H-enhanced trees for nonPSP using regression and ratio approach.\n", sep = ""))
    # derive ratio and regression routine
    if(needNewCoffs){
      cat(paste0("    Start to derive coefficients and ratios for year ", compilationYear, "\n"))
      allbecsplvd <- unique(tree_ms7[,.(BEC_ZONE, SP0, LV_D)])
      ## if the regratiodata can not be found in coeff folder
      ## generate regratiodata and derive coeff and ratio using mixed effect models
      ## use tree_ms7 and trees in PSP dataset
      psptrees <- readRDS(file.path(compilationPath, "compilation_PSP_db", "treelist.rds"))
      psptrees <- psptrees[MEAS_INTENSE %in% c("FULL", "H-ENHANCED"),]
      pspsamples <- readRDS(file.path(compilationPath, "compilation_PSP_db",
                                      "sample_msmt_header.rds"))
      pspsites <- readRDS(file.path(compilationPath, "compilation_PSP_db",
                                    "sample_site_header.rds"))
      psptrees <- merge(psptrees, pspsites[,.(SITE_IDENTIFIER, BEC_ZONE)],
                        by = "SITE_IDENTIFIER",
                        all.x = TRUE)
      alltrees <- rbind(tree_ms7[,.(CLSTR_ID, PLOT, TREE_NO, SPECIES, SP0,
                                    LV_D, S_F, DBH, BA_TREE, MEAS_INTENSE,
                                    BROKEN_TOP_IND, BEC_ZONE,
                                    VOL_WSV, VOL_MER, VOL_NTWB, VOL_DWB,
                                    TYPE_CD)],
                        psptrees[,.(CLSTR_ID, PLOT, TREE_NO, SPECIES, SP0,
                                    LV_D, S_F, DBH, BA_TREE, MEAS_INTENSE,
                                    BROKEN_TOP_IND, BEC_ZONE,
                                    VOL_WSV, VOL_MER, VOL_NTWB, VOL_DWB,
                                    TYPE_CD = "PSP")])
      pspsampleplots <- readRDS(file.path(compilationPath, "compilation_PSP_db",
                                          "sample_plot_header.rds"))
      pspsamples <- merge(pspsampleplots, pspsamples,
                          by = "CLSTR_ID",
                          all.x = TRUE)
      allsamples <- rbind(samples,
                          pspsamples,
                          fill = TRUE)
      regRatioData <- regRatioDataSelect(sampledata = allsamples,
                                         alltreedata = alltrees,
                                         usage = "ismc")
      rm(alltrees, allsamples, psptrees, pspsampleplots, pspsamples, pspsites)
      saveRDS(regRatioData,
              file.path(compilationPaths$compilation_coeff,
                        paste0("regRatioData", compilationYear, ".rds")))
      cat(paste0("        Selected data and saved to regRatioData", compilationYear, "\n"))
      coefs <- regBA_WSV(regRatioData, needCombs = allbecsplvd)

      if(compilationYear > 2024){ ## comparison starts from 2024 to select the better model to predict BA-WSV relationship
        BA_WSV_prev <- readRDS(file.path(compilationPaths$compilation_coeff,
                                         paste0("bestmodels_BA_WSV", as.numeric(compilationYear)-1, ".rds")))
        fixedcoeff_prev <- BA_WSV_prev$fixedcoeff
        fixedcoeff_prev[, uni_strata := paste0(BEC_ZONE, SP0, LV_D)]

        randomcoeff_prev <- BA_WSV_prev$randomcoeff
        randomcoeff_prev[, uni_strata := paste0(BEC_ZONE, SP0, LV_D)]

        allfix <- merge(coefs$fixedcoeff[,.(uni_strata = paste0(BEC_ZONE, SP0, LV_D),
                                            R2_Marginal_crt = R2_Marginal)],
                        fixedcoeff_prev[,.(uni_strata,
                                           R2_Marginal_prev = R2_Marginal,
                                           YEAR_FIT_prev = YEAR_FIT)],
                        by = c("uni_strata"),
                        all = TRUE)
        allfix[R2_Marginal_crt+0.01 >= R2_Marginal_prev,
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
        rm(coefs)
      }
      bestmodels_BA_WSV <- list(fixedcoeff = fixedcoeff_final,
                                randomcoeff = randomcoeff_final)
      saveRDS(bestmodels_BA_WSV,
              file.path(compilationPaths$compilation_coeff,
                        paste0("bestmodels_BA_WSV", compilationYear, ".rds")))
      cat(paste0("        Derived and saved coefficients to bestmodels_BA_WSV",
                 compilationYear, "\n"))

      ratios <- toWSVRatio_curve(inputData = regRatioData,
                                 needCombs = allbecsplvd)
      saveRDS(ratios,
              file.path(compilationPaths$compilation_coeff,
                        paste0("ratios", compilationYear, ".rds")))
      cat(paste0("        Calculated and saved ratios to ratios", compilationYear, "\n"))
      rm(coefs, ratios, regRatioData)
    } else {
      cat(paste0("    Use the existing coefficients and ratios of year ", compilationYear, "\n"))
    }
    bestmodels_BA_WSV <- readRDS(file.path(compilationPaths$compilation_coeff,
                                           paste0("bestmodels_BA_WSV", compilationYear, ".rds")))
    ratios <- readRDS(file.path(compilationPaths$compilation_coeff,
                                paste0("ratios", compilationYear, ".rds")))
    ## tree with H-enhanced and non-enhanced
    auxTrees_compiled <- treeVolEst_RegRatio_new(tree_ms7[MEAS_INTENSE %in% c("H-ENHANCED", "NON-ENHANCED"),],
                                                 bestmodels_BA_WSV,
                                                 ratios)

    prep_smy <- rbindlist(list(tree_ms7[MEAS_INTENSE %in% c("FULL", "ENHANCED"),],
                               auxTrees_compiled),
                          fill = TRUE)
    prep_smy[MEAS_INTENSE %in% c("FULL", "ENHANCED", "H-ENHANCED"),
             WSV_VOL_SRCE := "Calculated"]
    prep_smy[!is.na(VOL_WSV) &
               MEAS_INTENSE == "NON-ENHANCED",
             WSV_VOL_SRCE := "Regression"]
    prep_smy[is.na(VOL_WSV),
             WSV_VOL_SRCE := "Not applicable"]
    rm(auxTrees_compiled)
    rm(tree_ms7)
  }
  prep_smy <- merge(prep_smy,
                    unique(lookup_species()[,.(SPECIES, SP_TYPE)],
                           by = "SPECIES"),
                    by = "SPECIES",
                    all.x = TRUE)
  volVariables <- paste0("VOL_", c("MER", "NTWB", "DWB"))
  prep_smy[DBH %<<% 10, c(volVariables) := 0]

  ## for the ratio-based netted volumes, they cannot be bigger than vol_mer
  ## in case of vol_mer is directly derived

  prep_smy[MEAS_INTENSE %in% c( "H-ENHANCED") &
             VOL_NTWB %>>% VOL_MER,
           VOL_NTWB := VOL_MER]
  prep_smy[MEAS_INTENSE %in% c( "H-ENHANCED") &
             VOL_DWB %>>% VOL_MER,
           VOL_DWB := VOL_MER]

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

  ## for the A samples with -SizeMOD
  ## i need to bring the volumes back to 0
  ## bring the sizes to the original sizes
  ## DBH is kept for summaries
  prep_smy[substr(CLSTR_ID, 9, 9) == "A" & grepl("-SizeMOD", MEASUREMENT_ANOMALY_CODE),
           ':='(HEIGHT = HEIGHT - 7,
                HT_TOTAL = HT_TOTAL -7,
                BA_TREE = 0,
                VOL_WSV = 0,
                VOL_MER = 0,
                VOL_NTWB = 0,
                VOL_DWB = 0,
                VOL_STUMP = 0,
                WSV_VOL_SRCE = as.character(NA))]

  ba_lv <- prep_smy[,.(BA_ALL = sum(BA_TREE)),
                    by = "CLSTR_ID"]
  ba_lv_cut <- prep_smy[MEASUREMENT_ANOMALY_CODE == "H",
                        .(BA_cut = sum(BA_TREE)),
                        by = "CLSTR_ID"]
  ba_lv_cut_pct <- merge(ba_lv, ba_lv_cut,
                         by = "CLSTR_ID")
  sites_treated <- unique(substr(ba_lv_cut_pct[BA_cut/BA_ALL >= 0.25]$CLSTR_ID, 1, 7))
  samplesites <- readRDS(file.path(compilationPaths$compilation_db,
                                   "sample_site_header.rds"))
  samplesites[is.na(TREATMENT) & SITE_IDENTIFIER %in% sites_treated,
              TREATMENT := "THINNED"]
  saveRDS(samplesites,
          file.path(compilationPaths$compilation_db,
                    "sample_site_header.rds"))
  ## end of process
  if(compilationType == "nonPSP"){
    treelist_db <- readRDS(file.path(compilationPaths$compilation_db,
                                     "treelist.rds"))
    treelist_db[substr(CLSTR_ID, 9, 9) == "A" & grepl("-SizeMOD", MEASUREMENT_ANOMALY_CODE),
                ':='(HEIGHT = HEIGHT - 7,
                     HT_TOTAL = HT_TOTAL -7,
                     DBH = NA,
                     HT_TOTAL_EST = NA,
                     MEASUREMENT_ANOMALY_CODE = gsub("-SizeMOD", "", MEASUREMENT_ANOMALY_CODE))]
    treelist_db[substr(CLSTR_ID, 9, 9) == "A" & MEASUREMENT_ANOMALY_CODE == "NA",
                MEASUREMENT_ANOMALY_CODE := NA]
    prep_smy_temp <- prep_smy[,.(CLSTR_ID, PLOT, TREE_NO, MEAS_INTENSE,
                                 H_MERCH, BA_TREE,
                                 PHF_TREE, TREE_WT, VOL_WSV, VOL_STUMP, VOL_MER, VOL_NTWB,
                                 VOL_DWB, NET_FCT_METHOD, WSV_VOL_SRCE,
                                 VOLUME_TREE = "Y")]
    prep_smy_temp[is.na(TREE_WT),
                  TREE_WT := 1]
    treelist_db <- merge(treelist_db,
                         prep_smy_temp,
                         by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                         all.x = TRUE)
    treelist_db[is.na(HEIGHT) & BROKEN_TOP_IND == "N" & VOLUME_TREE == "Y",
                BROKEN_TOP_IND := as.character(NA)]
    prep_smy <- merge(prep_smy,
                      treelist_db[,.(CLSTR_ID, PLOT, TREE_NO, RESIDUAL_IND)],
                      by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                      all.x = TRUE)
    prep_smy[, RESIDUAL := RESIDUAL_IND]
  } else {
    treelist_db <- readRDS(file.path(compilationPaths$compilation_db,
                                     "treelist.rds"))
    prep_smy_temp <- prep_smy[,.(CLSTR_ID, PLOT, TREE_NO, MEAS_INTENSE,
                                 H_MERCH, BA_TREE,
                                 PHF_TREE, TREE_WT, VOL_WSV, WSV_VOL_SRCE,
                                 VOL_STUMP,
                                 VOL_MER, VOL_NTWB,
                                 VOL_DWB, NET_FCT_METHOD,
                                 VOLUME_TREE = "Y")]
    prep_smy_temp[is.na(TREE_WT),
                  TREE_WT := 1]
    treelist_db <- merge(treelist_db,
                         prep_smy_temp,
                         by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                         all.x = TRUE)
    prep_smy <- merge(prep_smy,
                      treelist_db[,.(CLSTR_ID, PLOT, TREE_NO, RESIDUAL_IND)],
                      by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                      all.x = TRUE)
    ingrowthtrees <- readRDS(file.path(compilationPaths$compilation_db,
                                       "component_change_treelevel.rds"))
    ingrowthtrees <- ingrowthtrees[COMPONENT_CHANGE == "I",
                                   .(CLSTR_ID  = paste0(SITE_IDENTIFIER, "-PSP", VISIT_NUMBER),
                                     PLOT,
                                     TREE_NO,
                                     INGROWTH = "Y")]
    prep_smy <- merge(prep_smy,
                      ingrowthtrees,
                      by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                      all.x = TRUE)
    prep_smy[is.na(INGROWTH), INGROWTH := "N"]
    rm(ingrowthtrees)
  }
  saveRDS(treelist_db[order(CLSTR_ID, PLOT, TREE_NO),],
          file.path(compilationPaths$compilation_db, "treelist.rds"))
  rm(prep_smy_temp, treelist_db)
  gc()

  ## 7. sammarize and save compiled tree-level data at cluster and cluster/species level
  cat(paste(substr(Sys.time(), 1, 16), ": Summarize volume at sample level.\n", sep = ""))
  if(compilationType == "nonPSP"){
    nvafratio <- read.xlsx(file.path(compilationPaths$compilation_coeff, "nvafall.xlsx")) %>%
      data.table
    vrisummaries <- VRISummaries(allVolumeTrees = data.table::copy(prep_smy),
                                 clusterPlotHeader = samples,
                                 utilLevel = utilLevel,
                                 weirdUtil = weirdUtil,
                                 equation = "KBEC",
                                 nvafRatio = nvafratio)
    # for eysm samples, the QMD will be forced to NA, as the DBH infor is not available for them
    vrisummaries$vol_bycs <- merge(vrisummaries$vol_bycs,
                                   unique(samples[,.(CLSTR_ID, SAMPLE_ESTABLISHMENT_TYPE)],
                                          by = "CLSTR_ID"),
                                   by = "CLSTR_ID",
                                   all.x = TRUE)
    vrisummaries$vol_bycs[SAMPLE_ESTABLISHMENT_TYPE == "EYSM",
                          ':='(QMD_LS = NA,
                               QMD_DS = NA)]
    vrisummaries$vol_byc <- merge(vrisummaries$vol_byc,
                                  unique(samples[,.(CLSTR_ID, SAMPLE_ESTABLISHMENT_TYPE)],
                                         by = "CLSTR_ID"),
                                  by = "CLSTR_ID",
                                  all.x = TRUE)
    vrisummaries$vol_byc[SAMPLE_ESTABLISHMENT_TYPE == "EYSM",
                         ':='(QMD_LS = NA,
                              QMD_DS = NA)]
    vrisummaries$vol_bycs[,':='(SAMPLE_ESTABLISHMENT_TYPE = NULL)]
    vrisummaries$vol_byc[,':='(SAMPLE_ESTABLISHMENT_TYPE = NULL)]
  } else {
    samples_tmp <- data.table::copy(samples)
    samples_tmp[, NO_PLOTS := 1]
    vrisummaries <- VolumeSummaries_PSP(allVolumeTrees = data.table::copy(prep_smy),
                                        clusterPlotHeader = samples_tmp,
                                        utilLevel = 3,
                                        weirdUtil = c("2", "4"),
                                        equation = "KBEC")
    rm(samples_tmp)
    dbh_tagged_range <- prep_smy[MEASUREMENT_ANOMALY_CODE != "PSP-TALLY" |
                                   is.na(MEASUREMENT_ANOMALY_CODE),
                                 .(DBH_OBS_MIN = min(DBH, na.rm = TRUE),
                                   DBH_OBS_MAX = max(DBH, na.rm = TRUE)),
                                 by = "CLSTR_ID"]
    rm(prep_smy)
    vrisummaries$vol_bycs <- merge(vrisummaries$vol_bycs,
                                   unique(samples[,.(CLSTR_ID, DBH_LIMIT_TAG, DBHLIMIT_COUNT)],
                                          by = "CLSTR_ID"),
                                   by = "CLSTR_ID",
                                   all.x = TRUE)
    vrisummaries$vol_bycs[!is.na(DBHLIMIT_COUNT), DBH_LIMIT_TAG := DBHLIMIT_COUNT]
    vrisummaries$vol_bycs[!(UTIL < DBH_LIMIT_TAG),
                          SUMRY_RANGE := "FULL"]
    vrisummaries$vol_bycs[is.na(SUMRY_RANGE),
                          SUMRY_RANGE := "PARTIAL"]
    vrisummaries$vol_bycs[,':='(DBH_LIMIT_TAG = NULL,
                                DBHLIMIT_COUNT = NULL)]
    vrisummaries$vol_byc <- merge(vrisummaries$vol_byc,
                                  unique(samples[,.(CLSTR_ID, DBH_LIMIT_TAG, DBHLIMIT_COUNT)],
                                         by = "CLSTR_ID"),
                                  by = "CLSTR_ID",
                                  all.x = TRUE)
    vrisummaries$vol_byc[!is.na(DBHLIMIT_COUNT), DBH_LIMIT_TAG := DBHLIMIT_COUNT]
    vrisummaries$vol_byc[!(UTIL < DBH_LIMIT_TAG),
                         SUMRY_RANGE := "FULL"]
    vrisummaries$vol_byc[is.na(SUMRY_RANGE),
                         SUMRY_RANGE := "PARTIAL"]
    vrisummaries$vol_byc[,':='(DBH_LIMIT_TAG = NULL,
                               DBHLIMIT_COUNT = NULL)]
    samp_msmt <- readRDS(file.path(compilationPaths$compilation_db,
                                   "sample_msmt_header.rds"))
    samp_msmt[!is.na(DBH_LIMIT_COUNT),
              PSP_PLOT_DESIGN := "IMPERIAL"]
    samp_msmt <- merge(samp_msmt,
                       dbh_tagged_range,
                       by = "CLSTR_ID",
                       all.x = TRUE)
    saveRDS(samp_msmt,
            file.path(compilationPaths$compilation_db,
                      "sample_msmt_header.rds"))
  }
  saveRDS(vrisummaries$vol_bycs, file.path(compilationPaths$compilation_db, "Smries_volume_byCLSP.rds"))
  saveRDS(vrisummaries$vol_byc, file.path(compilationPaths$compilation_db, "Smries_volume_byCL.rds"))
  saveRDS(vrisummaries$heightsmry_byc, file.path(compilationPaths$compilation_db, "Smries_height_byCL.rds"))
  saveRDS(vrisummaries$compositionsmry_byc, file.path(compilationPaths$compilation_db, "Smries_speciesComposition_byCL.rds"))

  rm(vrisummaries)
  gc()

  ## 8. small tree and stump compilation
  ## stump data
  cat(paste(substr(Sys.time(), 1, 16), ": Small tree and stump compilation.\n", sep = ""))
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
  if(compilationType == "nonPSP"){
    smalltreesamplemsmt <- readRDS(dir(compilationPaths$raw_from_oracle,
                                       pattern = "SampleMeasurements.rds",
                                       full.names = TRUE))
    smalltreesamplemsmt <- smalltreesamplemsmt[PLOT_CATEGORY_CODE == "IPC SM" &
                                                 MEASUREMENT_STATUS_CODE == "NOC",
                                               .(CLSTR_ID = paste0(SITE_IDENTIFIER, "-",
                                                                   SAMPLE_SITE_PURPOSE_TYPE_CODE,
                                                                   VISIT_NUMBER))]

    smalltreecompile$clusterSummaries[CLSTR_ID %in% smalltreesamplemsmt$CLSTR_ID,
                                      ':='(SMTR_TO2 = as.numeric(NA),
                                           SMTR_TO3 = as.numeric(NA),
                                           SMTR_TO4 = as.numeric(NA),
                                           SMTR2_HA = as.numeric(NA),
                                           SMTR3_HA = as.numeric(NA),
                                           SMTR4_HA = as.numeric(NA),
                                           SMTR_HA = as.numeric(NA),
                                           SMTR_TOT = as.numeric(NA))]
    smalltreecompile$clusterSpeciesSummaries[CLSTR_ID %in% smalltreesamplemsmt$CLSTR_ID,
                                             ':='(SMTR_CT2 = as.numeric(NA),
                                                  SMTR_CT3 = as.numeric(NA),
                                                  SMTR_CT4 = as.numeric(NA),
                                                  SMTR2_HA = as.numeric(NA),
                                                  SMTR3_HA = as.numeric(NA),
                                                  SMTR4_HA = as.numeric(NA),
                                                  SMTR_HA = as.numeric(NA),
                                                  SMTR_TOT = as.numeric(NA))]
  }
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
  cat(paste(substr(Sys.time(), 1, 16), ": Generate data dictionary.\n", sep = ""))
  allfiles_db <- dir(compilationPaths$compilation_db, pattern = ".rds")
  allfiles_db <- gsub(".rds", "", allfiles_db)
  dictionary_file_db <- list()
  col_missing <- NULL
  datadictionary_master <- read.xlsx(file.path(compilationPaths$compilation_coeff,
                                               "data_dictionary_master.xlsx")) %>%
    data.table
  datadictionary_master <- datadictionary_master[!is.na(Description)]
  for (indifile_db in allfiles_db) {
    indifile_db_file <- readRDS(file.path(compilationPaths$compilation_db,
                                          paste0(indifile_db, ".rds")))
    indifile_db_name <- data.table(ColumnName = names(indifile_db_file))
    rm(indifile_db_file)
    indifile_db_name <- merge(indifile_db_name,
                              datadictionary_master,
                              by = "ColumnName",
                              all.x = TRUE)
    indifile_db_name_missing <- indifile_db_name[is.na(Description),]
    dictionary_file_db[[indifile_db]] <- indifile_db_name
    col_missing <- rbind(col_missing, indifile_db_name_missing)
  }
  write.xlsx(dictionary_file_db,
             file.path(compilationPaths$compilation_db,
                       "Data_dictionary.xlsx"))


  if(nrow(col_missing) > 0){
    col_missing <- unique(col_missing)
    warning("There are some attributes do not have description. Please add them in the master table.")
    datadictionary_master <- rbind(datadictionary_master,
                                   col_missing)
    write.xlsx(datadictionary_master,
               file.path(compilationPaths$compilation_coeff,
                         "data_dictionary_master.xlsx"))
  }
  if(saveCSV == TRUE){
    cat(paste(substr(Sys.time(), 1, 16), ": Convert RDS to csv.\n", sep = ""))
    if(recompile == TRUE){
      fileTable <- rbind(data.table(folderName = compilationPaths$compilation_db,
                                    fileName = gsub(".rds", "",
                                                    dir(pattern = ".rds",
                                                        compilationPaths$compilation_db))))
    } else {
      fileTable <- rbind(data.table(folderName = compilationPaths$compilation_db,
                                    fileName = gsub(".rds", "",
                                                    dir(pattern = ".rds",
                                                        compilationPaths$compilation_db))),
                         data.table(folderName = compilationPaths$raw_from_oracle,
                                    fileName = gsub(".rds", "",
                                                    dir(pattern = ".rds",
                                                        compilationPaths$raw_from_oracle))))
    }
    numCore <- as.integer(0.5*detectCores())
    clusterInFunction <- makeCluster(numCore)
    numofrow <- as.integer(nrow(fileTable)/numCore)

    inputdata_list <- list()
    for (indicore in 1:numCore) {
      if(indicore == 1){
        indi_table <- fileTable[((indicore-1)*numofrow+1):(indicore*numofrow),]
      } else {
        indi_table <- fileTable[((indicore-1)*numofrow+1):(nrow(fileTable)),]
      }
      inputdata_list[[indicore]] <- list("fileTable" = indi_table,
                                         "compilationType" = compilationType,
                                         "compilationPaths" = compilationPaths)
    }
    clusterExport(clusterInFunction,
                  varlist = c("writeOutputs",
                              "fwrite"),
                  envir = environment())
    allresults <- parLapply(cl = clusterInFunction,
                            inputdata_list,
                            function(x){writeOutputs(fileTable = x$fileTable,
                                                     compilationType = x$compilationType,
                                                     compilationPaths = x$compilationPaths)})
    stopCluster(clusterInFunction)
    rm(inputdata_list)
    gc()
  }



  if(recompile == FALSE){
    cat(paste(substr(Sys.time(), 1, 16), ": Generate reports in report folder.\n", sep = ""))
    vegcompversion <- readRDS(file.path(compilationPaths$compilation_coeff,
                                        paste0("SA_VEGCOMP_", compilationType, ".rds")))
    vegcompversion <- substr(vegcompversion$vegCompVersion, 1, 10)
    lastCompilationDate <- gsub(paste0(compilationPath, "/Archive_", compilationType, "_"), "",
                                compilationPaths$compilation_last)
    rmarkdown::render(input = file.path(compilationPaths$compilation_coeff,
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
                      output_file = "compilation_report",
                      output_dir = compilationPaths$compilation_report,
                      quiet = TRUE)
    cat(paste(substr(Sys.time(), 1, 16), ": Archive compilation to Archive_", gsub("-", "", Sys.Date()), ".\n", sep = ""))
    if(dir.exists(compilationPaths$compilation_archive)){
      unlink(compilationPaths$compilation_archive, recursive = TRUE)
    }
    fs::dir_create(compilationPaths$compilation_archive)

    clusterInFunction <- makeCluster(6)
    inputdata_list <- list()
    for (indicore in 1:6) {
      if(indicore == 1){
        indi_dir_path <- compilationPaths$compilation_sa
      } else if (indicore == 2){
        indi_dir_path <- compilationPaths$compilation_db
      } else if (indicore == 3){
        indi_dir_path <- compilationPaths$raw_from_oracle
      } else if (indicore == 4){
        indi_dir_path <- compilationPaths$compilation_coeff
      } else if (indicore == 5){
        indi_dir_path <- compilationPaths$compilation_map
      } else {
        indi_dir_path <- compilationPaths$compilation_report
      }
      inputdata_list[[indicore]] <- list("dir_path" = indi_dir_path,
                                         "new_path" = compilationPaths$compilation_archive)
    }

    clusterExport(clusterInFunction,
                  varlist = c("dir_copy"),
                  envir = environment())
    allresults <- parLapply(cl = clusterInFunction,
                            inputdata_list,
                            function(x){dir_copy(path = x$dir_path,
                                                 new_path = x$new_path)})
    stopCluster(clusterInFunction)
  } else {
    cat(paste(substr(Sys.time(), 1, 16), ": Generate reports in report folder.\n", sep = ""))
    cat(paste(substr(Sys.time(), 1, 16), ": All recompiled outputs saved into Archive_", archiveDate, "_recomp", gsub("-", "", Sys.Date()), ".\n", sep = ""))
    vegcompversion <- readRDS(file.path(compilationPaths$compilation_coeff,
                                        paste0("SA_VEGCOMP_", compilationType, ".rds")))
    vegcompversion <- substr(vegcompversion$vegCompVersion, 1, 10)
    rmarkdown::render(input = file.path(compilationPaths$compilation_coeff,
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
                      output_file = "compilation_report",
                      output_dir = compilationPaths$compilation_report,
                      quiet = TRUE)
  }
  if(!is.na(syncTo)){ ## let's sync the compilation to the target folder
    cat(paste(substr(Sys.time(), 1, 16), ": Sync compilation to network drive.\n", sep = ""))
    if(!dir.exists(syncTo)){
      stop("Network folder is not available.")
    }
    ## for archivement
    fs::dir_copy(compilationPaths$compilation_archive,
                 syncTo)
  }
  cat(paste(substr(Sys.time(), 1, 16), ": Compilation is done. \n", sep = ""))
}
