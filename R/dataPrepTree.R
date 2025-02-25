#' prepare raw data from ismc for ISMCCompiler
#'
#'
#' @description This function is to prepare ismc data for compilation and save them
#'              in \code{outputPath} folder
#'
#' @param compilationType character, either \code{PSP} or \code{nonPSP}. If it is \code{PSP}, it
#'                               is consistent with original PSP compiler, otherwise, it
#'                               is consistent with VRI compiler.
#' @param inputPath character, Specifies the path that stores data from oracle data base.
#' @param outputPath character, Specifies the path to save your outputs. If missing, the current working
#'                   directory will be choose.
#' @param coeffPath character, Specifies the path where the crosswalk
#'                   table is stored.
#'
#' @return no item returned
#'
#' @importFrom data.table ':=' data.table melt shift
#' @importFrom dplyr '%>%'
#' @importFrom parallel detectCores makeCluster clusterExport parLapply stopCluster
#' @export
#' @docType methods
#' @rdname dataPrepTree
#' @author Yong Luo
dataPrepTree <- function(compilationType,
                         inputPath,
                         outputPath,
                         coeffPath,
                         sampleMsmts){
  treemeasurements <- readRDS(dir(inputPath, "TreeMeasurements.rds",
                                  full.names = TRUE)) %>%
    data.table
  treemeasurements <- treemeasurements[SITE_IDENTIFIER %in% unique(sampleMsmts$SITE_IDENTIFIER),]
  #to write
  if(compilationType == "PSP"){
    treemeasurements[, PLOT := PLOT_NUMBER]
  } else {
    treemeasurements[, PLOT := PLOT_CATEGORY_CODE]
    treemeasurements[PLOT == "IPC TD", PLOT := "I"]
    treemeasurements[PLOT != "I", PLOT := substr(PLOT, 5, 5)]
  }

  treemeasurements[,':='(PLOT_CATEGORY_CODE = NULL,
                         PLOT_NUMBER = NULL,
                         COUNT_TREE_IND = NULL,
                         FELLED_IND = NULL,
                         NVAF_TREE_IND = NULL,
                         NEAR_TREE_NUMBER = NULL,
                         STEM_MAP_TARGET_CODE = NULL,
                         TAG_MISSING_IND = NULL,
                         HEIGHT_SOURCE_CODE = NULL,
                         HEIGHT_CROWN_SOURCE_CODE = NULL,
                         HT_DIAMTR_CURVE_USE_CODE = NULL,
                         HEIGHT_TO_LEANING_TIP = NULL,
                         SITE_INDEX = NULL,
                         BENCHMARK_AGE_IND = NULL,
                         FALLING_HAZARD_IND = NULL,
                         SPIRAL_GRAIN_IND = NULL,
                         VERT_DISTANCE_MEAS_REQ_IND = NULL,
                         LEANING_TREE_BEARING = NULL,
                         SPECIES_CHANGE_IND = NULL)]
  # browser()
  # treemeasurements <- treemsmtEditing(compilationType = "PSP",
  #                 treemsmts = treemeasurements,
  #                 sitevisits = sampleMsmts)
  gc()
  allsites <- unique(sampleMsmts$SITE_IDENTIFIER)
  numCore <- as.integer(0.5*detectCores())
  clusterInFunction <- makeCluster(numCore)
  numofrow <- as.integer(length(allsites)/numCore)
  inputdata_list <- list()
  for (indicore in 1:numCore) {
    if(indicore != numCore){
      indisites <- allsites[((indicore-1)*numofrow+1):(indicore*numofrow)]
    } else {
      indisites <- allsites[((indicore-1)*numofrow+1):(length(allsites))]
    }
    indi_treemsmt <- treemeasurements[SITE_IDENTIFIER %in% indisites,]
    indi_sampleMsmts <- unique(sampleMsmts[SITE_IDENTIFIER %in% indisites,
                                           .(SITE_IDENTIFIER, VISIT_NUMBER, VISIT_TYPE)])
    inputdata_list[[indicore]] <- list("treemeasurements" = indi_treemsmt,
                                       "sitevisits" = indi_sampleMsmts)
    rm(indi_treemsmt, indisites, indi_sampleMsmts)
  }
  rm(treemeasurements)
  gc()
  clusterExport(clusterInFunction,
                varlist = c("treemsmtEditing",
                            "dbhManualCorrection",
                            "data.table", ":=",
                            "compilationType",
                            "shift"),
                envir = environment())
  allresults <- parLapply(cl = clusterInFunction,
                          inputdata_list,
                          function(x){treemsmtEditing(compilationType = compilationType,
                                                      treemsmts = x$treemeasurements,
                                                      sitevisits = x$sitevisits)})
  stopCluster(clusterInFunction)
  rm(inputdata_list)
  gc()
  for (i in 1:length(allresults)) {
    if(i == 1){
      treemeasurements <- allresults[[i]]
    } else {
      treemeasurements <- rbind(treemeasurements, allresults[[i]],
                                fill = TRUE)
    }
  }
  rm(allsites, numCore, clusterInFunction, numofrow,
     allresults)
  gc()

  ## based on discussion between Dan and sampling team on April 25, 2023
  ## for the ages that were measured as rotten (ROT) and cannot reach center (CRC)
  ## using prorate method to estimate boring age
  ## the prolen was measured in the field, however, with missing prorated ages
  ## the missing prorated ages were firstly using lab age, if lab age is missing using field boring age
  treemeasurements[AGE_MEASURE_CODE %in% c("ROT", "CRC") &
                     !is.na(PRORATE_LENGTH) & is.na(PRORATE_RING_COUNT),
                   PRORATE_RING_COUNT := ifelse(!is.na(MICROSCOPE_AGE), MICROSCOPE_AGE,
                                                BORING_AGE)]
  treemeasurements <- merge(treemeasurements,
                            unique(sampleMsmts[,.(SITE_IDENTIFIER, VISIT_NUMBER,
                                                  CLSTR_ID)],
                                   by = c("SITE_IDENTIFIER", "VISIT_NUMBER")),
                            by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                            all.x = TRUE)
  treemeasurements <- treemeasurements[!is.na(CLSTR_ID),]
  SmallLiveTreeTallies <- readRDS(dir(inputPath,
                                      pattern =  "SmallLiveTreeTallies.rds",
                                      full.names = TRUE))
  SmallLiveTreeTallies <- merge(SmallLiveTreeTallies,
                                unique(sampleMsmts[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER)],
                                       by = c("SITE_IDENTIFIER", "VISIT_NUMBER")),
                                by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                                all.x = TRUE)
  SmallLiveTreeTallies <- SmallLiveTreeTallies[!is.na(CLSTR_ID),]
  SmallLiveTreeTallies[, low_bnd := as.numeric(SMALL_TREE_TALLY_CLASS_CODE)]
  if(compilationType == "PSP"){
    treeno_max <- treemeasurements[PLOT == 1,
                                   .(TREE_NO_max = max(TREE_NUMBER)),
                                   by = c("SITE_IDENTIFIER", "CLSTR_ID")]
    treeno_max[, TREE_NO_max := max(TREE_NO_max),
               by = "SITE_IDENTIFIER"]
    treeno_max[, SITE_IDENTIFIER := NULL]
    smalltrees_forvol <- SmallLiveTreeTallies[SMALL_TREE_TALLY_CLASS_CODE %in% 5:7,]
    smalltrees_forvol <- merge(smalltrees_forvol,
                               sampleMsmts[,.(CLSTR_ID, DBH_LIMIT_TAG)],
                               by = "CLSTR_ID")
    # remove tally trees that do not make sense
    # 1. for DBH_LIMIT_TAG = 2
    smalltrees_forvol <- smalltrees_forvol[DBH_LIMIT_TAG != 2]
    # 2. for DBH_LIMIT_TAG = 4, and tally_class = 6 and 7
    smalltrees_forvol <- smalltrees_forvol[!(DBH_LIMIT_TAG == 4 & SMALL_TREE_TALLY_CLASS_CODE %in% c(6, 7)),]
    # 3. for DBH_LIMIT_TAG = 6.5 and tally_class = 7
    smalltrees_forvol <- smalltrees_forvol[!(DBH_LIMIT_TAG == 6.5 & SMALL_TREE_TALLY_CLASS_CODE %in% c(7)),]
    # 4. for DBH_LIMIT_TAG = 7.5 and tally_class = 7
    # prorate number_of_tree by multiplying by 0.4, which is derived using (7.5-6.5)/(9-6.5)
    # assign a dummy class code 8 for them
    smalltrees_forvol[DBH_LIMIT_TAG == 7.5 &
                        SMALL_TREE_TALLY_CLASS_CODE == 7,
                      ':='(SMALL_TREE_TALLY_CLASS_CODE = 8,
                           NUMBER_OF_TREES = round(NUMBER_OF_TREES * 0.4))]
    smalltrees_forvol <- smalltrees_forvol[NUMBER_OF_TREES > 0,]
    small_tree_full <- NULL
    NUMBER_OF_TREES_uni <- unique(smalltrees_forvol$NUMBER_OF_TREES)
    for(indinumboftrees in NUMBER_OF_TREES_uni){
      indismalltrees <- smalltrees_forvol[NUMBER_OF_TREES == indinumboftrees,]
      small_tree_full_indi <- data.table(expand.grid(CLSTR_ID = unique(indismalltrees$CLSTR_ID),
                                                     SMALL_TREE_TALLY_CLASS_CODE = as.character(5:7),
                                                     TREE_SPECIES_CODE = unique(indismalltrees$TREE_SPECIES_CODE),
                                                     TREE_NO_temp = 1:indinumboftrees))
      small_tree_full_indi <- merge(small_tree_full_indi,
                                    indismalltrees[,.(CLSTR_ID, SMALL_TREE_TALLY_CLASS_CODE,
                                                      TREE_SPECIES_CODE)],
                                    by = c("CLSTR_ID", "SMALL_TREE_TALLY_CLASS_CODE",
                                           "TREE_SPECIES_CODE"))
      small_tree_full <- rbind(small_tree_full,
                               small_tree_full_indi)
      rm(indismalltrees, small_tree_full_indi)
    }
    small_tree_full[SMALL_TREE_TALLY_CLASS_CODE == 5,
                    DBH := (1.5+3.9)/2] # trees with dbh between 1.5 and 3.9
    small_tree_full[SMALL_TREE_TALLY_CLASS_CODE == 6,
                    DBH := (4.0+6.4)/2] # trees with dbh between 4 and 6.4
    small_tree_full[SMALL_TREE_TALLY_CLASS_CODE == 7,
                    DBH := (6.5+9)/2] # trees with dbh between 6.5 and 9
    small_tree_full[SMALL_TREE_TALLY_CLASS_CODE == 8,
                    DBH := (6.5+7.4)/2] # trees with dbh between 6.5 and 7.4

    dbhlimit_count <- small_tree_full[,.(tally_class_min = min(SMALL_TREE_TALLY_CLASS_CODE)),
                                      by = "CLSTR_ID"]
    dbhlimit_count[tally_class_min == 5,
                   DBHLIMIT_COUNT := 1.5]
    dbhlimit_count[tally_class_min == 6,
                   DBHLIMIT_COUNT := 4]
    dbhlimit_count[is.na(DBHLIMIT_COUNT),
                   DBHLIMIT_COUNT := 6.5]
    sampleMsmts <- merge(sampleMsmts,
                         dbhlimit_count[,.(CLSTR_ID, DBHLIMIT_COUNT)],
                         by = "CLSTR_ID",
                         all.x = TRUE)
    small_tree_full <- merge(small_tree_full,
                             treeno_max,
                             by = "CLSTR_ID",
                             all.x = TRUE)
    small_tree_full[is.na(TREE_NO_max),
                    TREE_NO_max := 0]
    small_tree_full <- small_tree_full[order(CLSTR_ID, SMALL_TREE_TALLY_CLASS_CODE,
                                             TREE_SPECIES_CODE),]
    small_tree_full[, temp_treeno := 1:length(TREE_NO_temp),
                    by = "CLSTR_ID"]
    small_tree_full[, TREE_NO := TREE_NO_max + temp_treeno]
    small_tree_full <- small_tree_full[,.(CLSTR_ID, PLOT = 1,
                                          TREE_NUMBER = TREE_NO,
                                          TREE_SPECIES_CODE,
                                          DIAMETER = DBH,
                                          TREE_EXTANT_CODE = "L",
                                          TREE_STANCE_CODE = "S",
                                          DIAMETER_MEASMT_HEIGHT = 1.3,
                                          BROKEN_TOP_IND = "N",
                                          MEASUREMENT_ANOMALY_CODE = "PSP-TALLY",
                                          TREE_CLASS_CODE = 1,
                                          OUT_OF_PLOT_IND = "N")]
    small_tree_full <- merge(small_tree_full,
                             sampleMsmts[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER)],
                             by = "CLSTR_ID",
                             all.x = TRUE)

    treemeasurements <- rbind(treemeasurements, small_tree_full, fill = TRUE)
    rm(small_tree_full, dbhlimit_count, indinumboftrees,
       NUMBER_OF_TREES_uni, smalltrees_forvol,
       treeno_max)
    gc()
  } else {
    sampleMsmts[, DBHLIMIT_COUNT := NA]
  }
  if(compilationType == "nonPSP"){
    vi_f <- SmallLiveTreeTallies[SMALL_TREE_TALLY_CLASS_CODE < 5,
                                 .(CLSTR_ID, PLOT = "I", TREE_SPECIES_CODE,
                                   low_bnd, TOTAL = NUMBER_OF_TREES)]
  } else {
    vi_f <- SmallLiveTreeTallies[SMALL_TREE_TALLY_CLASS_CODE < 5,
                                 .(CLSTR_ID, PLOT = PLOT_NUMBER, TREE_SPECIES_CODE,
                                   low_bnd, TOTAL = NUMBER_OF_TREES)]
  }

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
  vi_f <- merge(vi_f,
                unique(sampleMsmts[,.(CLSTR_ID,
                                      BEC_ZONE, BEC_SBZ)],
                       by = c("CLSTR_ID")),
                by = c("CLSTR_ID"),
                all.x = TRUE)

  vi_f[, SPECIES := speciesCorrection(TREE_SPECIES_CODE,
                                      BEC_ZONE,
                                      BEC_SBZ)]
  vi_f[, TREE_SPECIES_CODE := NULL]
  saveRDS(vi_f, file.path(outputPath, "vi_f.rds"))
  rm(vi_f, indiname,
     SmallLiveTreeTallies,
     totalnames)
  # JD = common juniper
  # My guess is we consider Juniper spp as shrubs, not trees.
  # And as a shrub, we would ignore in our compilation.
  # Rene's email on 2022-09-09
  treemeasurements <- treemeasurements[TREE_SPECIES_CODE != "JD",]
  treemeasurements <- merge(treemeasurements,
                            unique(sampleMsmts[,.(TYPE_CD,
                                                  CLSTR_ID, MEAS_DT,
                                                  DBH_LIMIT_TAG,
                                                  SAMPLE_BREAK_POINT,
                                                  BEC_ZONE, BEC_SBZ, BEC_VAR)],
                                   by = c("CLSTR_ID")),
                            by = c("CLSTR_ID"),
                            all.x = TRUE)

  treemeasurements[, SPECIES_ORG := TREE_SPECIES_CODE]
  treemeasurements[, SPECIES := speciesCorrection(TREE_SPECIES_CODE,
                                                  BEC_ZONE,
                                                  BEC_SBZ)]
  treemeasurements[SPECIES_ORG != SPECIES & is.na(SP_EDIT),
                   SP_EDIT := "Corrected based on BEC"]
  treemeasurements[SPECIES_ORG != SPECIES & SP_EDIT == "Species changed based on last msmt",
                   SP_EDIT := paste0(SP_EDIT, ". Corrected based on BEC")]
  treemeasurements[TREE_SPECIES_CODE != SPECIES,
                   TREE_SPECIES_CODE := SPECIES]
  treemeasurements[, ':='(SPECIES = NULL)]


  ## adjust dbh if a tree is from main plot to subplot if the sample break point does not change
  ## based on rene's suggestions on March 14
  treemeasurements_simp <- treemeasurements[!is.na(DIAMETER),.(SITE_IDENTIFIER, PLOT,
                                                               TREE_NUMBER, VISIT_NUMBER,
                                                               DIAMETER,
                                                               DBH_LIMIT_TAG,
                                                               SAMPLE_BREAK_POINT)]






  treemeasurements_simp <- treemeasurements_simp[order(SITE_IDENTIFIER, PLOT, TREE_NUMBER,
                                                       VISIT_NUMBER),]
  needadjust <- NULL
  for(i in 1:(max(treemeasurements_simp$VISIT_NUMBER)-1)){
    treemeasurements_simp[DIAMETER %<<% DBH_LIMIT_TAG,
                          plot_crt := "L"] # a tree should not be tagged
    treemeasurements_simp[DIAMETER %>=% SAMPLE_BREAK_POINT,
                          plot_crt := "M"]
    treemeasurements_simp[is.na(plot_crt),
                          plot_crt := "S"]
    treemeasurements_simp[, plot_first := shift(plot_crt, type = "lag"),
                          by = c("SITE_IDENTIFIER", "PLOT", "TREE_NUMBER")]
    treemeasurements_simp[, breakpoint_first := shift(SAMPLE_BREAK_POINT, type = "lag"),
                          by = c("SITE_IDENTIFIER", "PLOT", "TREE_NUMBER")]
    treemeasurements_simp[, taggingdbh_first := shift(DBH_LIMIT_TAG, type = "lag"),
                          by = c("SITE_IDENTIFIER", "PLOT", "TREE_NUMBER")]

    treemeasurements_simp[!is.na(plot_first), ':='(change = paste0(plot_first, "-", plot_crt))]

    treemeasurements_simp[change == "M-S" &
                            SAMPLE_BREAK_POINT %==% breakpoint_first &
                            VISIT_NUMBER == (i+1),
                          ':='(dbh_new = SAMPLE_BREAK_POINT,
                               DIAMETER = SAMPLE_BREAK_POINT)]
    treemeasurements_simp[change == "S-L" &
                            DBH_LIMIT_TAG %==% taggingdbh_first &
                            VISIT_NUMBER == (i+1),
                          ':='(dbh_new = DBH_LIMIT_TAG,
                               DIAMETER = DBH_LIMIT_TAG)]
    needadjust <- rbind(needadjust,
                        treemeasurements_simp[!is.na(dbh_new),
                                              .(SITE_IDENTIFIER, PLOT,
                                                TREE_NUMBER, VISIT_NUMBER, dbh_new)])
    treemeasurements_simp <- treemeasurements_simp[VISIT_NUMBER != i,.(SITE_IDENTIFIER, PLOT,
                                                                       TREE_NUMBER, VISIT_NUMBER,
                                                                       DIAMETER,
                                                                       DBH_LIMIT_TAG,
                                                                       SAMPLE_BREAK_POINT)]
  }
  treemeasurements <- merge(treemeasurements,
                            needadjust,
                            by = c("SITE_IDENTIFIER", "PLOT",
                                   "TREE_NUMBER", "VISIT_NUMBER"),
                            all.x = TRUE)
  treemeasurements[!is.na(dbh_new),
                   DIAMETER := dbh_new]
  treemeasurements[,':='(DBH_LIMIT_TAG = NULL,
                         SAMPLE_BREAK_POINT = NULL,
                         dbh_new = NULL)]
  rm(treemeasurements_simp, needadjust, i)
  gc()
  # for nonPSP
  if(compilationType == "nonPSP"){
    # remove A samples from vol and age compilation
    ## output A sample treelist
    ## after discussion with Dan and Chris
    treemeasurements_A_samples <- treemeasurements[TYPE_CD == "A",]
    saveRDS(treemeasurements_A_samples,
            file.path(outputPath, "treelist_A_samples.rds"))
    # for some A samples the trees are very small so that the DBH can not be taken
    # I populated diameter_msmt_ht and diameter and flagged this modification with
    # -SizeMOD in measurement_anomaly_code
    treemeasurements[TYPE_CD == "A" &
                       is.na(DIAMETER_MEASMT_HEIGHT) &
                       is.na(LENGTH),
                     LENGTH := 0]
    treemeasurements[TYPE_CD == "A" &
                       is.na(DIAMETER_MEASMT_HEIGHT) &
                       TREE_PLANTED_IND == "Y", # this is for eysm
                     ':='(DIAMETER_MEASMT_HEIGHT = 1.3,
                          DIAMETER = 7, # so that this can be summarized at 4 but not at 7.5cm util
                          LENGTH = 7 + LENGTH,
                          MEASUREMENT_ANOMALY_CODE = paste0(MEASUREMENT_ANOMALY_CODE,
                                                            "-SizeMOD"))]
    # merge suit_si_from_notes to all tree measurements
    suit_si_from_notes <- readRDS(file.path(outputPath,
                                            "suit_si_from_notes.rds"))
    treemeasurements <- merge(treemeasurements, suit_si_from_notes,
                              by = c("SITE_IDENTIFIER", "VISIT_NUMBER", "TREE_NUMBER"),
                              all.x = TRUE)
    treemeasurements[!is.na(SUIT_SI_temp),
                     SUITABLE_FOR_SITE_INDEX_IND := SUIT_SI_temp]
    treemeasurements[, SUIT_SI_temp := NULL]
    treemeasurements[, ':='(SUIT_HT_EDIT = as.character(NA),
                            SUIT_SI_EDIT = as.character(NA),
                            LAB_AGE_EDIT = as.character(NA))]

    # for the l samples, the live fallen trees should be removed from the compilation
    treemeasurements[, uniid := 1:length(DIAMETER_MEASMT_HEIGHT)]
    uniid_removed <- treemeasurements[substr(CLSTR_ID, 9, 9) == "L" &
                                        TREE_EXTANT_CODE == "L" &
                                        TREE_STANCE_CODE == "F"]$uniid
    treemeasurements <- treemeasurements[!(uniid %in% uniid_removed),]
  } else {
    # for PSP
    treemeasurements[, TREE_DETAIL_COMMENT := NULL]
    treemeasurements[,':='(MICROSCOPE_AGE_org = MICROSCOPE_AGE)]
    ## this is a temporary fix for new age in ISMC
    treemeasurements[SITE_IDENTIFIER == 4018864 &
                       PLOT == 1 &
                       TREE_NUMBER == 70 &
                       VISIT_NUMBER == 4,
                     NEW_AGE := BORING_AGE] # new_age is wrong for this tree
    # new_age is revised lab age with higher accuracy
    treemeasurements[NEW_AGE > 0 &
                       (MICROSCOPE_AGE != NEW_AGE |
                          is.na(MICROSCOPE_AGE)),
                     ':='(MICROSCOPE_AGE = NEW_AGE,
                          LAB_AGE_EDIT = "Replaced with new_age")]
  }
  specieslookup <- lookup_species()
  treemeasurements <- merge(treemeasurements,
                            unique(specieslookup[,.(TREE_SPECIES_CODE = SPECIES, SP0)],
                                   by = "TREE_SPECIES_CODE"),
                            by = "TREE_SPECIES_CODE",
                            all.x = TRUE)
  # summarize stem mapping information by clster
  treemsmt_live_inplot <- treemeasurements[TREE_EXTANT_CODE == "L" &
                                             OUT_OF_PLOT_IND == "N" &
                                             !(MEASUREMENT_ANOMALY_CODE %in% c("Z", "D",
                                                                               "PSP-TALLY")),
                                           .(CLSTR_ID, TREE_NUMBER,
                                             STEM_MAP_BEARING, STEM_MAP_DISTANCE,
                                             MEASUREMENT_ANOMALY_CODE)]
  treemsmt_live_inplot[!is.na(STEM_MAP_BEARING) & !is.na(STEM_MAP_DISTANCE),
                       stem_mapped := TRUE]
  treemsmt_live_inplot[is.na(stem_mapped),
                       stem_mapped := FALSE]

  stem_mapped_smry_live <- treemsmt_live_inplot[,
                                                .(NO_LIVE_TREE_MAPPED = sum(stem_mapped),
                                                  NO_TOTAL_LIVE_TREE = length(stem_mapped)),
                                                by = c("CLSTR_ID")] # the summary is conducted for live trees
  stem_mapped_smry_live[NO_LIVE_TREE_MAPPED/NO_TOTAL_LIVE_TREE > 0.75,
                        STEM_MAPPED_SAMPLE := TRUE]
  stem_mapped_smry_live[is.na(STEM_MAPPED_SAMPLE),
                        STEM_MAPPED_SAMPLE := FALSE]
  sampleMsmts <- merge(sampleMsmts,
                       stem_mapped_smry_live,
                       by = c("CLSTR_ID"),
                       all.x = TRUE)
  rm(treemsmt_live_inplot, stem_mapped_smry_live)
  saveRDS(treemeasurements, file.path(outputPath, "treemeasurements.rds"))

  # specieslookup <- lookup_species()
  # specieslookup <- unique(specieslookup[,.(SPECIES, SP0)],
  #                         by = "SPECIES")
  # setnames(specieslookup, "SPECIES", "TREE_SPECIES_CODE")
  # treemeasurements <- merge(treemeasurements, specieslookup,
  #                           by = "TREE_SPECIES_CODE",
  #                           all.x = TRUE)
  vi_c <- treemeasurements[DIAMETER_MEASMT_HEIGHT == 1.3 &
                             !is.na(DIAMETER) &
                             !is.na(LENGTH) &
                             OUT_OF_PLOT_IND == "N" &
                             (MEASUREMENT_ANOMALY_CODE %in% c(NA, "M", "D", "F", "H", "N") |
                                grepl("-SizeMOD", MEASUREMENT_ANOMALY_CODE)), ## non tally tree, can not used for volume, see scott's comments
                           .(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, TYPE_CD, PLOT,
                             TREE_NO = TREE_NUMBER, SPECIES = TREE_SPECIES_CODE, SP0,
                             DBH = DIAMETER, BROKEN_TOP_IND, DIAM_BTP = BROKEN_TOP_DIAMETER,
                             HEIGHT_TO_BREAK, TREE_PLANTED_IND,
                             CR_CL = CROWN_CLASS_CODE, TREE_LEN = LENGTH,
                             HT_PROJ = PROJECTED_HEIGHT,
                             HT_BRCH = HEIGHT_TO_LIVE_CROWN,
                             S_F = TREE_STANCE_CODE,
                             LV_D = TREE_EXTANT_CODE, WALKTHRU_STATUS = CMI_WALKTHROUGH_CODE,
                             BARK_PER = REMAINING_BARK_PERCENT,
                             TAGGING_SECTOR_NO = TAGGING_SECTOR_NUMBER,
                             SITE_SECTOR_NO = SITE_SECTOR_NUMBER,
                             RESIDUAL = RESIDUAL_IND, WL_USE = WILDLIFE_USE_CODE,
                             WL_APPEA = TREE_APPEARANCE_CODE,
                             WL_CROWN = CROWN_CONDITION_CODE,
                             WL_BARK = BARK_RETENTION_CODE,
                             WL_WOOD = WOOD_CONDITION_CODE,
                             WL_LICHE = LICHEN_LOADING_RATING_CODE,
                             MEASUREMENT_ANOMALY_CODE,
                             TREE_CLASS_CODE)]
  # only nonPSP has log information
  if(compilationType == "nonPSP"){
    treelog <- readRDS(dir(inputPath, "TreeLogAssessments.rds",
                           full.names = TRUE)) %>%
      data.table
    treelog <- merge(treelog,
                     unique(sampleMsmts[,.(SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID)],
                            by = c("SITE_IDENTIFIER", "VISIT_NUMBER")),
                     by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                     all.x = TRUE)
    treelog[PLOT_CATEGORY_CODE == "IPC TD", PLOT_CATEGORY_CODE := "I"]
    treelog[PLOT_CATEGORY_CODE != "IPC TD",
            PLOT_CATEGORY_CODE := gsub("AUX ", "", PLOT_CATEGORY_CODE)]

    treelog <- treelog[,.(CLSTR_ID, PLOT = PLOT_CATEGORY_CODE, TREE_NO = TREE_NUMBER,
                          LOG_NUMBER, LOG_LEN = LOG_LENGTH,
                          LOG_GRD = LOG_GRADE_CODE,
                          LOG_SND = PERCENT_SOUND)]
    treelog[, NO_LOGS := length(LOG_NUMBER),
            by = c("CLSTR_ID", "PLOT", "TREE_NO")]
    treelog[, LOG_GRD := substr(LOG_GRD, 1, 1)]

    maxlength <- max(treelog$LOG_NUMBER)

    treelog <- reshape(treelog,
                       v.names = c("LOG_LEN", "LOG_SND", "LOG_GRD"),
                       timevar = "LOG_NUMBER",
                       idvar = c("CLSTR_ID", "PLOT", "TREE_NO", "NO_LOGS"),
                       direction = "wide")
    setnames(treelog,
             c(paste("LOG_LEN.", 1:maxlength, sep = ""),
               paste("LOG_GRD.", 1:maxlength, sep = ""),
               paste("LOG_SND.", 1:maxlength, sep = "")),
             c(paste("LOG_L_", 1:maxlength, sep = ""),
               paste("LOG_G_", 1:maxlength, sep = ""),
               paste("LOG_S_", 1:maxlength, sep = "")))

    treelog <- treelog[, c("CLSTR_ID", "PLOT", "TREE_NO", "NO_LOGS",
                           paste("LOG_L_", 1:maxlength, sep = ""),
                           paste("LOG_G_", 1:maxlength, sep = ""),
                           paste("LOG_S_", 1:maxlength, sep = "")),
                       with = FALSE]
    rm(maxlength)
    vi_c <- merge(vi_c, treelog,
                  by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                  all.x = TRUE)
    vi_c[BROKEN_TOP_IND == "Y" & !is.na(NO_LOGS),
         NO_LOGS := NO_LOGS+1L]
    brknologs <- sort(unique(vi_c[BROKEN_TOP_IND == "Y" & !is.na(NO_LOGS),]$NO_LOGS))
    for(i in brknologs){
      vi_c[BROKEN_TOP_IND == "Y" & NO_LOGS == i,
           paste("LOG", i, "_GRD", sep = "") := "N"]
      vi_c[BROKEN_TOP_IND == "Y" & NO_LOGS == i,
           paste("LOG", i, "_LEN", sep = "") := NA]
      vi_c[BROKEN_TOP_IND == "Y" & NO_LOGS == i,
           paste("LOG", i, "_SND", sep = "") := 0]
    }
    rm(brknologs, i, treelog)

    vi_c[is.na(NO_LOGS), needchangerows := TRUE]
    vi_c[needchangerows == TRUE, ':='(NO_LOGS = 1,
                                      LOG_G_1 = "*", # flag for h enhanced trees
                                      LOG_L_1 = TREE_LEN,
                                      LOG_S_1 = 100)]
    vi_c[needchangerows == TRUE & BROKEN_TOP_IND == "Y",
         ':='(LOG_G_2 = "N",
              LOG_L_2 = 0,
              LOG_S_2 = 0,
              NO_LOGS = 2)]
    vi_c[, ':='(needchangerows = NULL)]
  }
  saveRDS(vi_c, file.path(outputPath, "vi_c.rds"))

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
  treemeasurements[, unitreeid := paste0(SITE_IDENTIFIER, "-", PLOT, "-", TREE_NUMBER)]
  ## all site trees
  allsitetrees <- treemeasurements[!is.na(AGE_MEASMT_HEIGHT) | !is.na(AGE_MEASURE_CODE),
                                   .(age_msmt_last = max(VISIT_NUMBER)),
                                   by = "unitreeid"]
  vi_h <- merge(treemeasurements,
                allsitetrees,
                by = "unitreeid")
  if(compilationType == "PSP"){
    vi_h <- vi_h[,
                 .(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, TYPE_CD, PLOT,
                   TREE_NO = TREE_NUMBER,  SPECIES = TREE_SPECIES_CODE,  SP0,
                   CR_CL = CROWN_CLASS_CODE, PRO_RING = PRORATE_RING_COUNT,
                   BORE_AGE_FLD = BORING_AGE + AGE_CORE_MISSED_YEARS_FIELD,
                   BORE_AGE_LAB = MICROSCOPE_AGE + AGE_CORE_MISSED_YEARS_LAB,
                   AGE_CORR = AGE_CORRECTION,  TOTAL_AG = TOTAL_AGE,
                   PHYS_AGE = as.numeric(NA), GROW_20YR = RADIAL_INCREMENT_LAST_20_YR,
                   GROW_10YR = RADIAL_INCREMENT_LAST_10_YR,
                   GROW_5YR = RADIAL_INCREMENT_LAST_5_YR,
                   PRO_LEN = PRORATE_LENGTH, TREE_LEN = LENGTH,
                   BNG_DIAM = DIAMETER_AT_BORING_HEIGHT, BARK_THK = BARK_THICKNESS,
                   SUIT_TR = SUITABLE_FOR_AGE_IND, BORED_HT = AGE_MEASMT_HEIGHT,
                   SUIT_HT = SUITABLE_FOR_HEIGHT_IND,
                   SUIT_SI = SUITABLE_FOR_SITE_INDEX_IND,
                   PLOT_TYP = NA, # not sure
                   BARK_THKX = NA, # not sure
                   AGE_MEASURE_CODE,
                   RANDOM_TREE_IND, RESIDUAL_IND,
                   AGE_CORE_TAKEN_IND,
                   LAB_AGE_EDIT)]
  } else {
    vi_h <- vi_h[,
                 .(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, TYPE_CD, PLOT,
                   TREE_NO = TREE_NUMBER,  SPECIES = TREE_SPECIES_CODE,  SP0,
                   CR_CL = CROWN_CLASS_CODE, PRO_RING = PRORATE_RING_COUNT,
                   BORE_AGE_FLD = BORING_AGE + AGE_CORE_MISSED_YEARS_FIELD,
                   BORE_AGE_LAB = MICROSCOPE_AGE + AGE_CORE_MISSED_YEARS_LAB,
                   AGE_CORR = AGE_CORRECTION,  TOTAL_AG = TOTAL_AGE,
                   PHYS_AGE = as.numeric(NA), GROW_20YR = RADIAL_INCREMENT_LAST_20_YR,
                   GROW_10YR = RADIAL_INCREMENT_LAST_10_YR,
                   GROW_5YR = RADIAL_INCREMENT_LAST_5_YR,
                   PRO_LEN = PRORATE_LENGTH, TREE_LEN = LENGTH,
                   BNG_DIAM = DIAMETER_AT_BORING_HEIGHT, BARK_THK = BARK_THICKNESS,
                   SUIT_TR = SUITABLE_FOR_AGE_IND, BORED_HT = AGE_MEASMT_HEIGHT,
                   SUIT_HT = SUITABLE_FOR_HEIGHT_IND,
                   SUIT_SI = SUITABLE_FOR_SITE_INDEX_IND,
                   PLOT_TYP = NA, # not sure
                   BARK_THKX = NA, # not sure
                   AGE_MEASURE_CODE,
                   RANDOM_TREE_IND, RESIDUAL_IND,
                   AGE_CORE_TAKEN_IND,
                   LAB_AGE_EDIT = as.character(NA))]
  }
  vi_h_th <- treemeasurements[!is.na(AGE_MEASMT_HEIGHT) | !is.na(AGE_MEASURE_CODE),
                              .(CLSTR_ID, PLOT,
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
  vi_h[, RESIDUAL := RESIDUAL_IND]
  vi_h[, RESIDUAL_IND := NULL]
  saveRDS(vi_h, file.path(outputPath, "vi_h.rds"))
  rm(vi_h, vi_h_th)
  gc()
  if(compilationType == "nonPSP"){
    vi_i <- treemeasurements[DIAMETER_MEASMT_HEIGHT == 1.3 & is.na(LENGTH) &
                               OUT_OF_PLOT_IND == "N",
                             .(CLSTR_ID, PLOT, TREE_NO = TREE_NUMBER,
                               SPECIES = TREE_SPECIES_CODE,  SP0,
                               DBH = DIAMETER,
                               LV_D = TREE_EXTANT_CODE,
                               S_F = TREE_STANCE_CODE,
                               MEASUREMENT_ANOMALY_CODE,
                               TREE_CLASS_CODE,
                               TAGGING_SECTOR_NO = TAGGING_SECTOR_NUMBER,
                               SITE_SECTOR_NO = SITE_SECTOR_NUMBER,
                               RESIDUAL = RESIDUAL_IND)]
  } else {
    vi_i <- treemeasurements[!is.na(DIAMETER) & is.na(LENGTH) &
                               OUT_OF_PLOT_IND == "N",
                             .(CLSTR_ID, PLOT,
                               TREE_NO = TREE_NUMBER,
                               SPECIES = TREE_SPECIES_CODE,
                               SP0,
                               DBH = DIAMETER,
                               LV_D = TREE_EXTANT_CODE,
                               S_F = TREE_STANCE_CODE,
                               BROKEN_TOP_IND,
                               DIAM_BTP = BROKEN_TOP_DIAMETER,
                               HEIGHT_TO_BREAK,
                               HT_PROJ = PROJECTED_HEIGHT,
                               MEASUREMENT_ANOMALY_CODE,
                               TREE_CLASS_CODE,
                               TAGGING_SECTOR_NO = TAGGING_SECTOR_NUMBER,
                               SITE_SECTOR_NO = SITE_SECTOR_NUMBER,
                               RESIDUAL = RESIDUAL_IND)]
  }
  saveRDS(vi_i, file.path(outputPath, "vi_i.rds"))
  rm(vi_i)
  treeloss <- readRDS(dir(inputPath, "TreeLossIndicators.rds",
                          full.names = TRUE)) %>%
    data.table
  treeloss <- treeloss[SITE_IDENTIFIER %in% sampleMsmts$SITE_IDENTIFIER,]
  if(compilationType == "nonPSP"){
    treeloss[PLOT_CATEGORY_CODE == "IPC TD", PLOT := "I"]
    treeloss[PLOT_CATEGORY_CODE != "IPC TD",
             PLOT := gsub("AUX ", "", PLOT_CATEGORY_CODE)]
  } else {
    treeloss[, PLOT := PLOT_NUMBER]
  }
  treeloss <- treeloss[order(SITE_IDENTIFIER, VISIT_NUMBER,
                             PLOT, TREE_NUMBER, LOCATION_FROM),
                       .(SITE_IDENTIFIER, VISIT_NUMBER, PLOT, TREE_NUMBER,
                         LOSS_IN = TREE_LOSS_INDICATOR_CODE, # not sure
                         LOC_FROM = LOCATION_FROM,
                         LOC_TO = LOCATION_TO,
                         FREQ = FREQUENCY,
                         T_SIGN = NA,
                         F_SIGN = NA)]
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
  if(compilationType == "nonPSP"){
    treedamage[PLOT_CATEGORY_CODE == "IPC TD", PLOT := "I"]
    treedamage[PLOT_CATEGORY_CODE != "IPC TD",
               PLOT := gsub("AUX ", "", PLOT_CATEGORY_CODE)]
  } else {
    treedamage[, PLOT := PLOT_NUMBER]
  }
  treedamage <- treedamage[order(SITE_IDENTIFIER, VISIT_NUMBER,
                                 PLOT, TREE_NUMBER),
                           .(SITE_IDENTIFIER, VISIT_NUMBER, PLOT, TREE_NUMBER,
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
                treemeasurements[,.(CLSTR_ID, SITE_IDENTIFIER, VISIT_NUMBER, TYPE_CD, PLOT,
                                    TREE_NUMBER, SPECIES = TREE_SPECIES_CODE,  SP0,
                                    DISTANCE = STEM_MAP_DISTANCE, AZIMUTH = STEM_MAP_BEARING,
                                    OUT_OF_PLOT_IND, MEASUREMENT_ANOMALY_CODE)],
                by = c("SITE_IDENTIFIER", "VISIT_NUMBER", "PLOT", "TREE_NUMBER"),
                all = TRUE)
  vi_d <- vi_d[!is.na(CLSTR_ID),]
  vi_d <- vi_d[OUT_OF_PLOT_IND == "N",]
  vi_d <- vi_d[MEASUREMENT_ANOMALY_CODE %in% c(NA, "M", "D", "F", "H", "N"),]
  vi_d[, ':='(OUT_OF_PLOT_IND = NULL,
              MEASUREMENT_ANOMALY_CODE = NULL)]
  vi_d[!is.na(DISTANCE) & !is.na(AZIMUTH), STEM := TRUE]
  vi_d <- vi_d[ind == TRUE | STEM == TRUE,]
  vi_d[, ':='(ind = NULL)]
  setnames(vi_d, "TREE_NUMBER", "TREE_NO")
  saveRDS(vi_d, file.path(outputPath, "vi_d.rds"))
  rm(vi_d)
  gc()

  stumptallies <- readRDS(dir(inputPath, pattern = "StumpTallies.rds",
                              full.names = TRUE))
  stumptallies <- merge(stumptallies,
                        unique(sampleMsmts[,.(CLSTR_ID, SITE_IDENTIFIER,
                                              VISIT_NUMBER, BEC_ZONE, BEC_SBZ)],
                               by = c("SITE_IDENTIFIER", "VISIT_NUMBER")),
                        by = c("SITE_IDENTIFIER", "VISIT_NUMBER"),
                        all.x = TRUE)
  stumptallies <- stumptallies[!is.na(CLSTR_ID),]

  stumptallies[, SPECIES := speciesCorrection(TREE_SPECIES_CODE,
                                              BEC_ZONE,
                                              BEC_SBZ)]
  stumptallies[, TREE_SPECIES_CODE := NULL]
  vi_g <- stumptallies[,.(CLSTR_ID, PLOT = "I", SPECIES,
                          FREQ = STUMP_OCCURRENCE_FREQUENCY, DIB = DIAMETER_INSIDE_BARK,
                          HEIGHT = STUMP_HEIGHT, PCT_SND = SOUND_WOOD_PERCENT,
                          WL_USE = WILDLIFE_USE_CODE, WL_WOOD = WOOD_CONDITION_CODE,
                          BARK_RET = BARK_RETENTION_CODE, ROOT_ROT = DAMAGE_AGENT_CODE)]
  saveRDS(vi_g, file.path(outputPath, "vi_g.rds"))

  return(sampleMsmts)
}
