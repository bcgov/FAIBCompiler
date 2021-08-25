#' Check psp trees for the abnormal observations
#' @description This function is to check psp trees in terms of dramatic
#'              change of size, live dead code, species and missing observation.
#' @param treesData data.table, Contains both height trees and nonHT trees in PSPs.
#'
#'
#' @return Full list of trees with flags.
#'
#' @importFrom data.table data.table ':=' set rbindlist setnames setkey
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
  treeData <- readRDS("treeall.rds")


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



  treeData[, measYear := as.numeric(substr(MEAS_DT, 1, 4))]
  treeData[, uniTreeID := paste0(SITE_IDENTIFIER, "-",
                                 PLOT, "-", TREE_NO)]
  # remove potential duplicates
  treeData <- treeData[!(SITE_IDENTIFIER == "4000870" &
                                         VISIT_NUMBER == 2),]

  treeData <- treeData[!(SITE_IDENTIFIER == "4000952" &
                                         VISIT_NUMBER %in% c(2)),]
  treeData <- treeData[!(SITE_IDENTIFIER == "4016893" &
                                         VISIT_NUMBER %in% c(3)),]
  treeData <- treeData[!(SITE_IDENTIFIER == "4042217" &
                                         VISIT_NUMBER %in% c(4)),]

  nrow(treeData) == nrow(unique(treeData,
                                by = c("uniTreeID", "measYear")))

  ## check species
  # Rene: I also checked for changing species,
  # and chose to believe the species call at the last measurement
  # (ie., based on the assumption that the crew had the previous
  #   records and subsequently made the correction.
  #   But again I don’t know the right answer here).
  treeData[, sp_n := length(unique(SPECIES)),
           by = "uniTreeID"]


  tree_sp_change <- treeData[sp_n > 1,
                             .(uniTreeID, measYear,
                               SPECIES)]
  tree_sp_change[, lastYear := max(measYear),
                 by = "uniTreeID"]
  tree_sp_change <- tree_sp_change[lastYear == measYear,
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
                            measNo = as.numeric(treeData$measYear),
                            LDStatus = treeData$LV_D,
                            liveCode = "L",
                            deadCode = "D")

  treeData_lv_false <- treeData[uniTreeID %in%
                                  lvcheck[pass == FALSE,]$subjectID,
                                .(uniTreeID, measYear, DBH, LV_D)]
  # last measurement status
  treeData_lv_false[, meas_las := max(measYear),
                    by = "uniTreeID"]
  treeData_lv_false_last <- treeData_lv_false[measYear == meas_las]
  treeData_lv_false_last[LV_D == "D"] # 0 rows therefore, all live for last meas

  # multiple dead trees
  treeData_lv_false_dead <- treeData_lv_false[LV_D == "D"]
  treeData_lv_false_dead[, nobs := length(meas_las),
                         by = "uniTreeID"]
  treeData_lv_false_dead[, measYear_firstD := min(measYear),
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
  treeData[measYear >= measYear_firstD,
           ':='(LV_D = "D",
                lv_d_change_flag = "multiple dead, force to dead after first dead")]
  treeData[, measYear_firstD := NULL]

  treeData_dead <- treeData[LV_D == "D",
                            .(firstdeadyear = min(measYear)),
                            by = "uniTreeID"]

  treeData <- merge(treeData,
                    treeData_dead,
                    by = "uniTreeID",
                    all.x = TRUE)

  treeData <- treeData[!(measYear > firstdeadyear) |
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


  ## check size
  # from Rene maximum annual growth rate is less than 3cm/year
  sizecheck <- checkSize_remeas(subjectID = treeData$uniTreeID,
                                measTime = as.numeric(treeData$measYear),
                                size = treeData$DBH,
                                change = "increase",
                                maxChangeRate = 3,
                                tolerance = 5)

  sizecheckdata <- sizecheck[pass == FALSE,
            .(uniTreeID = subjectID,
              measYear = measTime,
              pass, reason, memo)]

  sizecheckdata <- merge(sizecheckdata,
                         treeData[,.(uniTreeID, measYear, LV_D)],
                    by = c("uniTreeID", "measYear"),
                    all.x = TRUE)

  sizecheckdata_dead <- sizecheckdata[pass == FALSE & LV_D == "D"]
  sizecheckdata_live <- sizecheckdata[pass == FALSE & LV_D == "L"]

  treeData_size_failure1 <- treeData[uniTreeID %in% sizecheckdata_live[reason == "break tolerance"]$uniTreeID,
                                     .(uniTreeID, measYear, DBH, LV_D)]

  treeData_size_failure2 <- treeData[uniTreeID %in% sizecheckdata_live[reason == "missing size"]$uniTreeID,
                                     .(uniTreeID, measYear, DBH, LV_D)]

  treeData_size_failure3 <- treeData[uniTreeID %in% sizecheckdata_live[reason == "abnormal change rate"]$uniTreeID,
                                     .(uniTreeID, measYear, DBH, LV_D)]



  View(treeData[uniTreeID %in% sizecheck[pass == FALSE & memo < -5,]$subjectID,
           .(uniTreeID, DBH, measYear, LV_D)])




}
