#' Check psp trees for the abnormal observations
#' @description This function is to check psp trees in terms of dramatic
#'              change of size, live dead code, species and missing observation.
#' @param treesData data.table, Contains both height trees and nonHT trees in PSPs.
#'
#'
#' @return Full list of trees with flags.
#'
#' @importFrom data.table data.table ':=' set rbindlist setnames setkey copy
#' @importFrom dplyr '%>%'
#' @export
#' @docType methods
#' @rdname pspTreeCheck
#'
#' @author Yong Luo
pspTreeCheck<- function(treeData){
  ### need repeatedly-measured sample check
  ### for 1) species
  ###     2) dramatically increased and decreased tree size
  ###     3) dead and live code
  ###     4) check missing in between
  rm(list = ls())
  treeData <- readRDS("treeall.rds")
  treeData_org <- data.table::copy(treeData)


  #   In you old PSP compiler, do you have some rules to check and correct/adjust the abnormal PSP tree data? Specifically, I am looking at:
  #     1)	DBH drop tolerance to accommodate measurement error,
  #
  # Rene: I ignored subtle DBH decreases in live trees. I only included a correction if the live dbh drop likely missed a decimal placement in one measurement (eg., going from 62cm to 6.7cm). My flag was if the previous/current dbh ratio exceeded 9, then I assumed a missed decimal occurred, and chose to believe the last measure, and therefore changed the previous measure. I’m not sure if this was the right call, since other criteria could help improve  this (eg., what was the height if taken, how many previous measurements were taken).  So maybe a better rule (or one-off fixes may be required).  I also realized that I did not do an opposite check for missed decimals (eg,. going from 6.2cm to 67cm); you identify these in you next issue #2.
  #
  # 2)	an abnormal increase/decrease of DBH per year to consider as bad data, e.g.,
  # tree_id Visit_number      DBH     pass                            reason memo
  # 1: 4000432-1-266                      1       9.1      NA                        <NA>   NA
  # 2: 4000432-1-266                      2     99.3     FALSE abnormal change rate 90.2
  #
  # Rene: I have computed an annual DBH increment (based on the measurement interval), to flag trees that exceeded 3cm / yr of DBH growth.  While I have flagged these trees as potential issues, I didn’t change them yet in the compilation.  This check should be corrected as a missed decimal check, but it may be hard to define a general rule if the problem was the last or previous measurement.
  #
  # 3)	species correction if species code changes among different measurement, e.g.,
  # tree_id VISIT_NUMBER SPECIES  DBH LV_D
  # 1: 4001544-1-120                          2        HW     2.8    L
  # 2: 4001544-1-120                          3          FD     4.4    L
  #
  #
  # 4)	live and dead abnormal, i.e., live after dead,
  # tree_id VISIT_NUMBER  DBH LV_D
  # 1: 4000400-1-4561                          1 12.2    L
  # 2: 4000400-1-4561                          2 12.2    D
  # 3: 4000400-1-4561                          3 12.2    D
  # 4: 4000400-1-4561                          4 12.3    L
  #
  #
  # 5)	missing measurement, i.e., a tree record is missing between two measurements (missing middle), or the last live_dead is live and there is no sequential measurement (missing tail), e.g.,
  # tree_id VISIT_NUMBER  DBH LV_D
  # 1: 4000057-1-9032                           1 19.9    L
  #
  # Rene:  Hi Yong, for cases where a tree is tagged at initial measurement,
  # and missing at the next measurement,
  # and the ‘missed_dropped_fallen’ code is not populated,
  # then I assumed the tree is ‘dead fallen’.
  # There may be many other reasons, but they will
  # only be known once (if ever) the sample is revisited.



  treeData[, lastGrowthYear := as.numeric(substr(MEAS_DT, 1, 4))]
  treeData[, currentYear_cut := as.Date(paste0(lastGrowthYear, "-06-01"))]
  treeData[MEAS_DT < currentYear_cut,
           lastGrowthYear := lastGrowthYear - 1]
  treeData[, currentYear_cut := NULL]
  treeData[, uniTreeID := paste0(SITE_IDENTIFIER, "-",
                                 PLOT, "-", TREE_NO)]
  # remove potential duplicates
  treeData <- treeData[!(SITE_IDENTIFIER == "4000870" &
                           VISIT_NUMBER == 2),]
  treeData <- treeData[!(SITE_IDENTIFIER == "4000952" &
                           VISIT_NUMBER %in% c(2)),]
  treeData <- treeData[!(SITE_IDENTIFIER == "4042217" &
                           VISIT_NUMBER %in% c(4)),]

  nrow(treeData) == nrow(unique(treeData,
                                by = c("uniTreeID", "lastGrowthYear")))

  ## check species
  # Rene: I also checked for changing species,
  # and chose to believe the species call at the last measurement
  # (ie., based on the assumption that the crew had the previous
  #   records and subsequently made the correction.
  #   But again I don’t know the right answer here).
  treeData[, sp_n := length(unique(SPECIES)),
           by = "uniTreeID"]


  tree_sp_change <- treeData[sp_n > 1,
                             .(uniTreeID, lastGrowthYear,
                               SPECIES)]
  tree_sp_change[, lastYear := max(lastGrowthYear),
                 by = "uniTreeID"]
  tree_sp_change <- tree_sp_change[lastYear == lastGrowthYear,
                                   .(uniTreeID, SPECIES_new = SPECIES)]
  treeData <- merge(treeData,
                    tree_sp_change,
                    by = "uniTreeID",
                    all.x = TRUE)
  treeData[!is.na(SPECIES_new),
           ':='(SPECIES = SPECIES_new,
                species_change_flag = "species changed, last used")]
  treeData[, ':='(SPECIES_new = NULL,
                  sp_n = NULL)]

  # lv check
  # Rene: I also checked for changes from dead to live,
  # and chose to believe the status at last measurement.
  # In your example, there may be more evidence that the tree is really
  # dead based on what was record from the previous two measures,
  # so there may be a better approach than what I’m assuming as a general rule.

  lvcheck <- checkLD_remeas(subjectID = treeData$uniTreeID,
                            measNo = as.numeric(treeData$lastGrowthYear),
                            LDStatus = treeData$LV_D,
                            liveCode = "L",
                            deadCode = "D")

  treeData_lv_false <- treeData[uniTreeID %in%
                                  lvcheck[pass == FALSE,]$subjectID,
                                .(uniTreeID, lastGrowthYear, DBH, LV_D)]
  # last measurement status
  treeData_lv_false[, meas_las := max(lastGrowthYear),
                    by = "uniTreeID"]
  treeData_lv_false_last <- treeData_lv_false[lastGrowthYear == meas_las]
  treeData_lv_false_last[LV_D == "D"] # 0 rows therefore, all live for last meas

  # multiple dead trees
  treeData_lv_false_dead <- treeData_lv_false[LV_D == "D"]
  treeData_lv_false_dead[, nobs := length(meas_las),
                         by = "uniTreeID"]
  treeData_lv_false_dead[, measYear_firstD := min(lastGrowthYear),
                         by = "uniTreeID"]
  singledead <- unique(treeData_lv_false_dead[nobs == 1,]$uniTreeID) # 37 trees

  multipledead <- unique(treeData_lv_false_dead[nobs > 1,
                                                .(uniTreeID, measYear_firstD)]) # 37 trees
  ## if there is one dead between two live status,
  ## force dead to live
  treeData[uniTreeID %in% singledead,
           ':='(LV_D = "L",
                lv_d_change_flag = "single dead, last see live, force to live")]
  ## if there is multiple dead between live
  ## force live to dead after first dead
  treeData <- merge(treeData,
                    multipledead,
                    by = "uniTreeID",
                    all.x = TRUE)
  treeData[lastGrowthYear >= measYear_firstD,
           ':='(LV_D = "D",
                lv_d_change_flag = "multiple dead, force to dead after first dead")]
  treeData[, measYear_firstD := NULL]

  treeData_dead <- treeData[LV_D == "D",
                            .(firstdeadyear = min(lastGrowthYear)),
                            by = "uniTreeID"]

  treeData <- merge(treeData,
                    treeData_dead,
                    by = "uniTreeID",
                    all.x = TRUE)

  treeData <- treeData[!(lastGrowthYear > firstdeadyear) |
                         is.na(firstdeadyear),]
  treeData[, firstdeadyear := NULL]



  missingcheck <- NULL
  allsites <- unique(treeData$SITE_IDENTIFIER)
  for(indisite in allsites){
    indisitedata <- treeData[SITE_IDENTIFIER == indisite,]
    intendM_indisite <- sort(as.numeric(unique(indisitedata$VISIT_NUMBER)))
    alltrees <- unique(indisitedata$uniTreeID)
    for(inditree in alltrees){
      inditreedata <- indisitedata[uniTreeID == inditree,]
      missingcheck_indi <- checkMissing_remeas(subjectID = inditreedata$uniTreeID,
                                               measNo = as.numeric(inditreedata$VISIT_NUMBER),
                                               intendedMeasNo = intendM_indisite,
                                               deadCode = "D",
                                               LDStatus = inditreedata$LV_D)
      missingcheck <- rbind(missingcheck, missingcheck_indi)
    }
  }

  missingcheck <- readRDS("missingtrees.rds")


  # for the missing tail
  # adding tail with same size as the previous measurement
  # assume the missing is caused by dead, so force to dead

  missing_tail <- missingcheck[pass == FALSE & missingReason == "missing tail",
                               .(uniTreeID = subjectID,
                                 VISIT_NUMBER = missingMeasNo - 1)]
  missing_tail_trees <- merge(missing_tail,
                              treeData,
                              by = c("uniTreeID", "VISIT_NUMBER"),
                              all.x = TRUE)
  missing_tail_trees[, ':='(missing_flag = "missing tail, added",
                            VISIT_NUMBER = VISIT_NUMBER + 1,
                            LV_D = "D")]

  missing_tail_trees <- merge(missing_tail_trees,
                              unique(treeData[,.(SITE_IDENTIFIER, VISIT_NUMBER,
                                                 MEAS_DT_new = MEAS_DT,
                                                 lastGrowthYear_new = lastGrowthYear)]),
                              by = c("SITE_IDENTIFIER",
                                     "VISIT_NUMBER"),
                              all.x = TRUE)
  missing_tail_trees[,':='(MEAS_DT = MEAS_DT_new,
                           lastGrowthYear = lastGrowthYear_new)]
  missing_tail_trees[, ':='(MEAS_DT_new = NULL,
                            lastGrowthYear_new = NULL)]
  missing_tail_trees <- missing_tail_trees[!is.na(lastGrowthYear),]
  treeData <- rbindlist(list(treeData,
                             missing_tail_trees),
                        fill = TRUE)


  # for the missing middle
  # adding middle
  # if in the following measure, this tree is dead, then a dead tree will be added,
  # with the same size as the previous measurement.
  # otherwise, a live tree is added, with size proportional to total size difference
  # (size_follow - size_prev)*(time_mid - time_prev)/(time_follow - time_prev)
  missing_mid <- missingcheck[pass == FALSE &
                                missingReason == "missing middle"]
  missing_mid_follow <- missing_mid[,.(uniTreeID = subjectID,
                                       VISIT_NUMBER = missingMeasNo + 1)]
  missing_mid_follow <- merge(missing_mid_follow,
                              treeData[,.(uniTreeID,
                                          VISIT_NUMBER,
                                          LV_D)],
                              by = c("uniTreeID",
                                     "VISIT_NUMBER"),
                              all.x = TRUE)

  missing_mid_follow_dead <- missing_mid_follow[LV_D == "D",
                                                .(uniTreeID,
                                                  VISIT_NUMBER)]

  missing_mid_follow_dead_trees <- merge(missing_mid_follow_dead,
                                         treeData,
                                         by = c("uniTreeID",
                                                "VISIT_NUMBER"),
                                         all.x = TRUE)
  missing_mid_follow_dead_trees[, ':='(missing_flag = "missing middle, dead tree added",
                                       VISIT_NUMBER = VISIT_NUMBER - 1,
                                       LV_D = "D",
                                       MEAS_DT = NA,
                                       lastGrowthYear = NA,
                                       DBH = NA,
                                       BA_TREE = NA,
                                       HEIGHT = NA)]


  missing_mid_follow_live <- missing_mid_follow[LV_D == "L",
                                                .(uniTreeID,
                                                  VISIT_NUMBER)]
  missing_mid_follow_live_trees <- merge(missing_mid_follow_live,
                                         treeData,
                                         by = c("uniTreeID",
                                                "VISIT_NUMBER"),
                                         all.x = TRUE)
  missing_mid_follow_live_trees[, ':='(missing_flag = "missing middle, live tree added",
                                       VISIT_NUMBER = VISIT_NUMBER - 1,
                                       LV_D = "L",
                                       MEAS_DT = NA,
                                       lastGrowthYear = NA,
                                       DBH = NA,
                                       BA_TREE = NA,
                                       HEIGHT = NA)]

  missing_mid_added_trees <- rbindlist(list(missing_mid_follow_dead_trees,
                                            missing_mid_follow_live_trees))

  missing_mid_non_added_trees <- merge(missing_mid[,.(uniTreeID = subjectID,
                                                      VISIT_NUMBER = missingMeasNo)],
                                       missing_mid_added_trees[,.(uniTreeID, VISIT_NUMBER,
                                                                  found = TRUE)],
                                       by = c("uniTreeID", "VISIT_NUMBER"),
                                       all.x = TRUE)
  missing_mid_non_added_trees <- missing_mid_non_added_trees[is.na(found),]

  while (nrow(missing_mid_non_added_trees) > 0) {
    missing_mid_follow_more <- missing_mid_non_added_trees[,.(uniTreeID,
                                         VISIT_NUMBER = VISIT_NUMBER + 1)]
    missing_mid_follow_more <- merge(missing_mid_follow_more,
                                missing_mid_added_trees,
                                by = c("uniTreeID",
                                       "VISIT_NUMBER"),
                                all.x = TRUE)
    missing_mid_follow_more[, VISIT_NUMBER := VISIT_NUMBER - 1]
    missing_mid_added_trees <- rbindlist(list(missing_mid_added_trees,
                                              missing_mid_follow_more),
                                         use.names = TRUE)

    missing_mid_non_added_trees <- merge(missing_mid[,.(uniTreeID = subjectID,
                                                        VISIT_NUMBER = missingMeasNo)],
                                         missing_mid_added_trees[,.(uniTreeID, VISIT_NUMBER,
                                                                    found = TRUE)],
                                         by = c("uniTreeID", "VISIT_NUMBER"),
                                         all.x = TRUE)
    missing_mid_non_added_trees <- missing_mid_non_added_trees[is.na(found),]
  }

  missing_mid_added_trees <- merge(missing_mid_added_trees,
                                         unique(treeData[,.(SITE_IDENTIFIER, VISIT_NUMBER,
                                                            MEAS_DT_new = MEAS_DT,
                                                            lastGrowthYear_new = lastGrowthYear)]),
                                         by = c("SITE_IDENTIFIER",
                                                "VISIT_NUMBER"),
                                         all.x = TRUE)
  missing_mid_added_trees[,':='(MEAS_DT = MEAS_DT_new,
                                      lastGrowthYear = lastGrowthYear_new)]
  missing_mid_added_trees[, ':='(MEAS_DT_new = NULL,
                                       lastGrowthYear_new = NULL)]


  treeData <- rbindlist(list(treeData, missing_mid_added_trees),
                        fill = TRUE)
  treeData <- treeData[order(uniTreeID, VISIT_NUMBER),]



  samplesite <- readRDS(dir("D:/ISMC project/ISMC compiler/ismc compiler for psp/raw_from_oracle",
                            pattern = "SampleSites.rds",
                            full.names = TRUE))

  # for cotton species
  treedata_cott <- treeData[SPECIES %in% c("AC", "AT")]
  treedata_non_cott <- treeData[!(SPECIES %in% c("AC", "AT"))]


  # from Rene maximum annual growth rate is less than 3cm/year
  sizecheck_cott <- checkSize_remeas(subjectID = treedata_cott$uniTreeID,
                                measTime = as.numeric(treedata_cott$lastGrowthYear),
                                size = treedata_cott$DBH,
                                change = "increase",
                                maxChangeRate = 9,
                                toleranceAbs = 3,
                                toleranceRel = 0.2)
  sizecheck_non_cott <- checkSize_remeas(subjectID = treedata_non_cott$uniTreeID,
                                measTime = as.numeric(treedata_non_cott$lastGrowthYear),
                                size = treedata_non_cott$DBH,
                                change = "increase",
                                maxChangeRate = 4,
                                toleranceAbs = 3,
                                toleranceRel = 0.2)

  sizecheck_false <- rbind(sizecheck_cott[pass == FALSE,
                               .(uniTreeID = subjectID,
                                 lastGrowthYear = measTime,
                                 pass, reason, memo)],
                      sizecheck_non_cott[pass == FALSE,
                                     .(uniTreeID = subjectID,
                                       lastGrowthYear = measTime,
                                       pass, reason, memo)])

  sizecheck_false <- merge(sizecheck_false,
                           treeData[,.(uniTreeID, VISIT_NUMBER,
                                       lastGrowthYear, LV_D, MEASUREMENT_ANOMALY_CODE)],
                           by = c("uniTreeID", "lastGrowthYear"),
                           all.x = TRUE)


  sizecheck_false_missingdbh <- sizecheck_false[reason == "missing size",]

  treeData_missingcheck <- treeData[,.(SITE_IDENTIFIER, uniTreeID, VISIT_NUMBER, DBH, LV_D,
                                       MEASUREMENT_ANOMALY_CODE)]
  treeData_missingcheck[, totalDBH_bytree := sum(DBH, na.rm = TRUE),
                        by = "uniTreeID"]

  treeData_missingcheck[, totalDBH_bysite := sum(DBH, na.rm = TRUE),
                        by = "SITE_IDENTIFIER"]

  treeData_missingcheck[, totalDBH_bysitevisit := sum(DBH, na.rm = TRUE),
                        by = c("SITE_IDENTIFIER", "VISIT_NUMBER")]

  treeData_missing_all_DBH_bytree <- treeData_missingcheck[totalDBH_bytree == 0,]
  treeData_missing_all_DBH_bysite <- treeData_missingcheck[totalDBH_bysite == 0,]
  treeData_missing_all_DBH_bysitevisit <- treeData_missingcheck[totalDBH_bysitevisit == 0,]

  treeData_missing_all_DBH_bytree <- merge(treeData_missing_all_DBH_bytree,
                                           samplesite[,.(SITE_IDENTIFIER = as.character(SITE_IDENTIFIER),
                                                         SAMPLE_SITE_NAME)],
                                           by = "SITE_IDENTIFIER",
                                           all.x = TRUE)


  treeData_missing_all_DBH_bytree[, namelength := nchar(SAMPLE_SITE_NAME)]
  treeData_missing_all_DBH_bytree[, PSP_Sample_Type := substr(SAMPLE_SITE_NAME, namelength, namelength)]

  treeData_missing_bytree <- list("treedetails" = treeData_missing_all_DBH_bytree[,.(uniTreeID, SITE_IDENTIFIER, SAMPLE_SITE_NAME,
                                                                                   VISIT_NUMBER, LV_D, MEASUREMENT_ANOMALY_CODE,
                                                                                   PSP_Sample_Type)],
                                  "summary" = treeData_missing_all_DBH_bytree[,.(No_of_trees = length(unique(uniTreeID))),
                                                                            by = "PSP_Sample_Type"])

  write.xlsx(treeData_missing_bytree, "missingDBH_at_tree.xlsx")
  sizecheck_false_missingdbh <- sizecheck_false_missingdbh[!(uniTreeID %in% treeData_missing_all_DBH_bytree$uniTreeID),]


  ## for the live trees,
  sizecheck_false_missingdbh_live <- sizecheck_false_missingdbh[LV_D == "L"]
  trees_missingdbh_live <- treeData[uniTreeID %in% unique(sizecheck_false_missingdbh_live$uniTreeID),
                                    .(uniTreeID, VISIT_NUMBER, DBH, LV_D, lv_d_change_flag, MEASUREMENT_ANOMALY_CODE)]

  ## a. if the missing size found at the beginning of the measurements
  ## and a relative small dbh (less than 10cm) measured at later visits,
  ## these trees are highly possible to be ingrowth trees, i.e., the size of those trees did not
  ## pass the size threshold for the first couple of visits. an example is:
  #        uniTreeID DBH VISIT_NUMBER LV_D
  # 1: 4000317-4-127  NA            1    L
  # 2: 4000317-4-127  NA            2    L
  # 3: 4000317-4-127 2.8            3    L
  ## for those trees, these record will be removed
  trees_missingdbh_live[is.na(DBH), ':='(lastmissingVisit = max(VISIT_NUMBER),
                                         missingdbhcount = length(DBH)),
                        by = "uniTreeID"]
  trees_missingdbh_live[VISIT_NUMBER <= lastmissingVisit, ':='(
                               tomissingdbhcount = length(DBH)),
                        by = "uniTreeID"]

  trees_missingdbh_live[!is.na(DBH),
                        firstdbhvisit := min(VISIT_NUMBER, na.rm = TRUE),
                        by = "uniTreeID"]

  trees_missingdbh_live[VISIT_NUMBER == firstdbhvisit,
                        firstDBH := DBH]
  trees_missingdbh_live[, ':='(lastmissingVisit = mean(lastmissingVisit, na.rm = TRUE),
                               missingdbhcount = mean(missingdbhcount, na.rm = TRUE),
                               tomissingdbhcount = mean(tomissingdbhcount, na.rm = TRUE),
                               firstdbhvisit = mean(firstdbhvisit, na.rm = TRUE),
                               firstDBH = mean(firstDBH, na.rm = TRUE)),
                        by = "uniTreeID"]

  trees_missingdbh_live_removal <- unique(trees_missingdbh_live[lastmissingVisit < firstdbhvisit &
                          firstDBH <= 10 &
                          missingdbhcount == tomissingdbhcount,
                        .(uniTreeID, firstdbhvisit)])

  treeData <- merge(treeData,
                    trees_missingdbh_live_removal,
                    by = "uniTreeID",
                    all.x = TRUE)
  treeData <- treeData[!(VISIT_NUMBER < firstdbhvisit) | is.na(firstdbhvisit),]

  ## b. if the missing size found at the beginning of the measurements
  ## and a relative big dbh (bigger than 10cm) measured at later visits,
  ## these trees are highly possible to have missing dbh for the first couple of visits.
  ## an example is:
  #        uniTreeID VISIT_NUMBER  DBH LV_D
  # 1: 4010868-1-900            1   NA    L
  # 2: 4010868-1-900            2   NA    L
  # 3: 4010868-1-900            3   NA    L
  # 4: 4010868-1-900            4 42.9    L
  ## for those trees, the previous records should be calculated to minimize the information loss
  trees_missingdbh_live_needDBH1 <- unique(trees_missingdbh_live[lastmissingVisit < firstdbhvisit &
                                                                  firstDBH > 10 &
                                                                  missingdbhcount == tomissingdbhcount,
                                                                .(uniTreeID, firstdbhvisit)])

  ## c. if the missing size found at the end of the measurements
  ## an example is:
  #        uniTreeID VISIT_NUMBER  DBH LV_D
  # 1: 4010868-1-900            1   NA    L
  # 2: 4010868-1-900            2   NA    L
  # 3: 4010868-1-900            3   NA    L
  # 4: 4010868-1-900            4 42.9    L
  ## for those trees, the previous records should be calculated to minimize the information loss










  ## the dbh will be assigned using stand or BEC based species
  ## specific growth rate


  View(treeData[uniTreeID %in% sizecheck_false_missingdbh_live$uniTreeID,
                .(uniTreeID, VISIT_NUMBER, DBH, LV_D)])


  ## for the dead trees, the dbh will be assigned using last live dbh
  ## if
  sizecheck_false_missingdbh[LV_D == "D"]

  treeData[uniTreeID == "4001307-1-1313",.(uniTreeID, DBH, VISIT_NUMBER,
                                           LV_D)]


  sizecheck_false <- sizecheck_false[!(LV_D == "D" &
                      reason == "break both tolerance")]

  sizecheck_false <- sizecheck_false[MEASUREMENT_ANOMALY_CODE != "H" |
                                       is.na(MEASUREMENT_ANOMALY_CODE), ]


  treeData_sizefalse <- treeData[uniTreeID %in% sizecheck_false$uniTreeID,]

  sizefalse_smry <- data.frame(table(substr(sizecheck_false[reason != "missing size"]$uniTreeID, 1, 7))) %>%
    data.table
  names(sizefalse_smry) <- c("SITE_IDENTIFIER", "NofIssueTrees")
  sizefalse_smry <- sizefalse_smry[order(-NofIssueTrees),]

  issuetrees <- treeData[uniTreeID %in% sizecheck_false[reason != "missing size"]$uniTreeID,
                         .(SITE_IDENTIFIER, uniTreeID, SPECIES = SPECIES_ORG, lastGrowthYear, DBH, LV_D)]
  issuetrees[,':='(DBH_next = shift(DBH, type = "lead"),
                   measYear_next = shift(lastGrowthYear, type = "lead"),
                   LV_D_next = shift(LV_D, type = "lead")),
             by = "uniTreeID"]
  issuetree_mark <- sizecheck_false[,.(uniTreeID, measYear_next = lastGrowthYear,
                                       reason, memo)]

  issuetrees <- merge(issuetrees,
                      issuetree_mark,
                      by = c("uniTreeID", "measYear_next"),
                      all.x = TRUE)



  issuetrees <- merge(issuetrees,
                      unique(siteid[,.(SITE_IDENTIFIER = as.character(SITE_IDENTIFIER),
                                       SAMPLE_SITE_NAME)]),
                      by = "SITE_IDENTIFIER",
                      all.x = TRUE)





  issuetrees <- issuetrees[!is.na(measYear_next),
                           .(SITE_IDENTIFIER, SAMPLE_SITE_NAME, uniTreeID, SPECIES,
                             lastGrowthYear, DBH, LV_D,
                             measYear_next, DBH_next, LV_D_next,
                             reason, memo)]


  issuetrees_stand_check <- issuetrees[SITE_IDENTIFIER %in% sizefalse_smry[NofIssueTrees >= 5]$SITE_IDENTIFIER,]
  issuetrees_tree_check <- issuetrees[SITE_IDENTIFIER %in% sizefalse_smry[NofIssueTrees < 5]$SITE_IDENTIFIER,]

  totaltreessmry <- treeData[,.(NofTotalTrees = length(unique(uniTreeID))),
                             by = "SITE_IDENTIFIER"]

  sizefalse_smry <- merge(sizefalse_smry,
                          totaltreessmry,
                          by = "SITE_IDENTIFIER",
                          all.x = TRUE)
  sizefalse_smry <- sizefalse_smry[order(-NofIssueTrees)]
  sizefalse_smry[, percentage := round(100*NofIssueTrees/NofTotalTrees, 2)]
  sizefalse_smry <- merge(sizefalse_smry,
                          unique(siteid[,.(SITE_IDENTIFIER = as.character(SITE_IDENTIFIER),
                                           SAMPLE_SITE_NAME)]),
                          by = "SITE_IDENTIFIER",
                          all.x = TRUE)

  issuetrees_save <- list("issueTreeSmryBySiteID" = sizefalse_smry,
                          "issueTreeDetails" = issuetrees)
  write.xlsx(issuetrees_save,
             "issuetrees.xlsx")


  ## for the abnormal change rate, i.e., increase rate is bigger than 3cm/year
  treeData_abrate <- treeData[uniTreeID %in%
                                sizecheck_false[reason == "abnormal change rate",]$uniTreeID,]
  treeData_abrate <- merge(treeData_abrate,
                           sizecheck_false,
                           by = c("uniTreeID", "lastGrowthYear"),
                           all.x = TRUE)

  treeData_abrate_simp <- treeData_abrate[,.(uniTreeID, DBH, lastGrowthYear, reason, memo, LV_D)]

  treeData_abrate_simp[, ':='(DBH_next = shift(DBH, type = "lead"),
                              measYear_next = shift(lastGrowthYear, type = "lead")),
                       by = "uniTreeID"]

  treeData_abrate_simp[, ':='(ratio_to_next = DBH/DBH_next,
                              ratio_to_prev = DBH_next/DBH,
                              dif_abs = abs(DBH - DBH_next))]

  trees8X1 <- treeData_abrate_simp[ratio_to_next > 8 | ratio_to_prev > 8 |
                                     dif_abs >= 8,
                       .(uniTreeID, DBH, lastGrowthYear, DBH_next, measYear_next,
                         ratio_to_next, ratio_to_prev, dif_abs)]
  treeData_abrate_simp[uniTreeID %in% trees8X1,]


  ### manual fix 8X trees based on full measurements and size distribution
  ### in the stand
  treeData[uniTreeID == "4000432-1-266" &
             lastGrowthYear == 2001,
           ':='(DBH = 9.3,
                DBH_correction_flag = "8X trees, manual corrected")]

  treeData[uniTreeID == "4000884-1-593" &
             lastGrowthYear == 2002,
           ':='(DBH = 10,
                DBH_correction_flag = "8X trees, manual corrected")]

  treeData[uniTreeID == "4000891-1-526" &
             lastGrowthYear == 2002,
           ':='(DBH = 5.4,
                DBH_correction_flag = "8X trees, manual corrected")]

  treeData[uniTreeID == "4010285-1-415" &
             lastGrowthYear == 2014,
           ':='(DBH = 5.5,
                DBH_correction_flag = "8X trees, manual corrected")]

  treeData[uniTreeID == "4016157-1-264" &
             lastGrowthYear == 2007,
           ':='(DBH = 12.4,
                DBH_correction_flag = "8X trees, manual corrected")]

  treeData[uniTreeID == "4017327-1-1128" &
             lastGrowthYear == 2014,
           ':='(DBH = 4.5,
                DBH_correction_flag = "8X trees, manual corrected")]

  treeData[uniTreeID == "4032543-1-80" &
             lastGrowthYear == 2001,
           ':='(DBH = 26,
                DBH_correction_flag = "8X trees, manual corrected")]

  treeData[uniTreeID == "4041848-1-107" &
             lastGrowthYear == 2006,
           ':='(DBH = 12.2,
                DBH_correction_flag = "8X trees, manual corrected")]

  treeData[uniTreeID == "4042204-1-161" &
             lastGrowthYear == 2004,
           ':='(DBH = 4.9,
                DBH_correction_flag = "8X trees, manual corrected")]

  treeData[uniTreeID == "4042260-1-76" &
             lastGrowthYear == 2005,
           ':='(DBH = 11.4,
                DBH_correction_flag = "8X trees, manual corrected")]

  write.xlsx(treeData_abrate_simp,
             "treedata_abnormal_increase_rate.xlsx")
  ## for the measurement that break the tolerance
  treeData_breaktol <- sizecheck_false[reason == "break tolerance",]
  treeData_breaktol <- merge(treeData_breaktol,
                             treeData[,.(uniTreeID, lastGrowthYear, LV_D)],
                             by = c("uniTreeID", "lastGrowthYear"),
                             all.x = TRUE)

  ## remove shrink due to dead
  treeData_breaktol <- treeData_breaktol[LV_D == "L"]
  treeData_breaktol <- treeData[uniTreeID %in%
                                  treeData_breaktol[reason == "break tolerance",]$uniTreeID,]

  treeData_breaktol <- merge(treeData_breaktol,
                             sizecheck_false,
                             by = c("uniTreeID", "lastGrowthYear"),
                             all.x = TRUE)

  treeData_breaktol_simp <- treeData_breaktol[,.(uniTreeID, DBH, lastGrowthYear, reason, memo, LV_D)]
  treeData_breaktol_simp[, ':='(DBH_next = shift(DBH, type = "lead"),
                              measYear_next = shift(lastGrowthYear, type = "lead")),
                       by = "uniTreeID"]

  treeData_breaktol_simp[, ':='(ratio_to_next = DBH/DBH_next,
                              ratio_to_prev = DBH_next/DBH,
                              dif_abs = DBH - DBH_next)]

  trees8X2 <- treeData_breaktol_simp[ratio_to_next > 8 | ratio_to_prev > 8 | dif_abs >= 8,
                         .(uniTreeID, DBH, lastGrowthYear, DBH_next, measYear_next,
                           ratio_to_next, ratio_to_prev, dif_abs)]$uniTreeID

  treeData[uniTreeID == "4009397-1-70" &
             lastGrowthYear == 2020,
           ':='(DBH = 50,
                DBH_correction_flag = "8X trees, manual corrected")]

  treeData[uniTreeID == "4024192-1-923" &
             lastGrowthYear == 2006,
           ':='(DBH = 36.8,
                LV_D = "D", ## assume this tree is dead
                DBH_correction_flag = "8X trees, manual corrected")]

  treeData[uniTreeID == "4024537-1-615" &
             lastGrowthYear == 1999,
           ':='(DBH = 2.2,
                DBH_correction_flag = "8X trees, manual corrected")]

  treeData[uniTreeID == "4044252-1-322" &
             lastGrowthYear == 1992,
           ':='(DBH = 6.1,
                DBH_correction_flag = "8X trees, manual corrected")]

  treeData[uniTreeID == "4045167-1-110" &
             lastGrowthYear == 1992,
           ':='(DBH = 9.8,
                DBH_correction_flag = "8X trees, manual corrected")]

  treeData[uniTreeID == "4045167-1-120" &
             lastGrowthYear == 1992,
           ':='(DBH = 4.4,
                DBH_correction_flag = "8X trees, manual corrected")]

  treeData[uniTreeID == "4045167-1-164" &
             lastGrowthYear == 1992,
           ':='(DBH = 6.6,
                DBH_correction_flag = "8X trees, manual corrected")]

  treeData[uniTreeID == "4053322-1-185" &
             lastGrowthYear == 1992,
           ':='(DBH = 5.1,
                DBH_correction_flag = "8X trees, manual corrected")]



  write.xlsx(treeData_breaktol_simp,
             "treedata_break_tolerance.xlsx")


  ## for the trees that have missing DBH
  treeData_missingDBH <- treeData[uniTreeID %in%
                                    sizecheck_false[reason == "missing size",]$uniTreeID,]

  treeData_missingDBH <- merge(treeData_missingDBH,
                               sizecheck_false,
                               by = c("uniTreeID", "lastGrowthYear"),
                               all.x = TRUE)

  treeData_missingDBH_simp <- treeData_missingDBH[,.(uniTreeID, DBH, lastGrowthYear, reason, memo, LV_D, missing_flag, lv_d_change_flag)]
  write.xlsx(treeData_missingDBH_simp,
             "treedata_missing_dbh.xlsx")





  sizecheckdata_dead <- sizecheckdata[pass == FALSE & LV_D == "D"]
  sizecheckdata_live <- sizecheckdata[pass == FALSE & LV_D == "L"]

  treeData_size_failure1 <- treeData[uniTreeID %in% sizecheckdata_live[reason == "break tolerance"]$uniTreeID,
                                     .(uniTreeID, lastGrowthYear, DBH, LV_D)]

  treeData_size_failure2 <- treeData[uniTreeID %in% sizecheckdata_live[reason == "missing size"]$uniTreeID,
                                     .(uniTreeID, lastGrowthYear, DBH, LV_D)]

  treeData_size_failure3 <- treeData[uniTreeID %in% sizecheckdata_live[reason == "abnormal change rate"]$uniTreeID,
                                     .(uniTreeID, lastGrowthYear, DBH, LV_D)]



  View(treeData[uniTreeID %in% sizecheck[pass == FALSE & memo < -5,]$subjectID,
                .(uniTreeID, DBH, lastGrowthYear, LV_D)])




}
