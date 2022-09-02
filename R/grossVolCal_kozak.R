#' Calcualte gross volume using kozak equations
#'
#' @description This function use kozak BEC and species-specific taper equation to
#'              calculate tree volume.
#'              This functions also assigns the volume multiplier adjustment.
#' @param compilationType character, either \code{PSP} or \code{nonPSP}. If it is \code{PSP}, it
#'                               is consistent with original PSP compiler, otherwise, it
#'                               is consistent with VRI compiler.
#' @param fullDimTreeData data.table, This is full dimension tree data, i.e., it has both DBH and height.
#' @param logMinLength numeric, Specifies a minimum length for a log.
#' @param stumpHeight numeric, Specifies stump height. If missing, 0.3 m will be used.
#' @param breastHeight numeric, Specifies breast height. 1.3 m will be used when this arguement is missing.
#' @param UTOPDIB numeric, Specifies minimum merchantable inside bark diameter. 10 cm is used as a default.
#' @return A data table
#'
#' @importFrom data.table ':='
#' @importFrom dplyr left_join
#' @importFrom fpCompare %>>% %<<% %==% %!=% %<=% %>=%
#' @importFrom FAIBBase heightEstimateForBTOP_D heightEstimateForBTOP_H treeVolCalculator
#'
#'
#' @export
#' @docType methods
#' @rdname grossVolCal_kozak
#'
#' @author Yong Luo
grossVolCal_kozak<- function(compilationType,
                             fullDimTreeData, logMinLength,
                             stumpHeight, breastHeight, UTOPDIB){
  ## 1. check species, FIZ and BEC zone
  fullDimTreeData[, BEC := BEC_ZONE]
  fullDimTreeData[BEC_ZONE %in% c("BAFA", "CMA", "IMA"), BEC := "AT"]
  fullDimTreeData[BEC_ZONE %in% c("BG"), BEC := "PP"]
  fullDimTreeData[BROKEN_TOP_IND == "Y",
                  BTOP := "Y"]
  if(compilationType == "PSP"){
    fullDimTreeData[, c(paste0("LOG_L_", 1:9),
                        paste0("LOG_G_", 1:9),
                        paste0("LOG_S_", 1:9)) := as.numeric(NA)]
    fullDimTreeData[BROKEN_TOP_IND == "Y",
                    LOG_L_1 := HEIGHT]
    fullDimTreeData[is.na(LOG_L_1),
                    LOG_L_1 := HT_TOTAL]
    fullDimTreeData[, NO_LOGS := 1]
  }
  # 3. adjust log for fully-measured trees and enhanced trees
  fullDimTreeData <- logAdjustment(treeData = data.table::copy(fullDimTreeData),
                                   stumpHeight = stumpHeight)
  treeVolumes <- FAIBBase::treeVolCalculator(FIZorBEC = fullDimTreeData$BEC,
                                             species = fullDimTreeData$SP0,
                                             height = fullDimTreeData$HT_TOTAL,
                                             DBH = fullDimTreeData$DBH,
                                             taperEquationForm = "KBEC",
                                             volMultiplier = 1,
                                             stumpHeight = stumpHeight,
                                             breastHeight = breastHeight,
                                             UTOPDIB = UTOPDIB,
                                             BTOPEstimateType = 1,
                                             BTOPHeight = fullDimTreeData$HT_BTOP,
                                             BTOPDIB = fullDimTreeData$DIAM_BTP,
                                             logLengthMatrix = fullDimTreeData[, paste("LOG_L_", 0:9, sep = ""),
                                                                               with = FALSE],
                                             logMinLength = logMinLength)
  fullDimTreeData <- cbind(fullDimTreeData, treeVolumes)
  if(compilationType == "PSP"){
    set(fullDimTreeData, , c(paste0("LOG_L_", 0:9),
                             paste0("LOG_V_", 0:9),
                             paste0("LOG_VM_", 0:9),
                             paste0("LOG_D_", 0:9),
                             paste0("LOG_G_", 1:9)),
        NULL)
  }
  volumenames <- names(treeVolumes)
  rm(treeVolumes)
  if(compilationType == "nonPSP"){
    NAvolumesForHenhancedtrees <- volumenames[!(volumenames %in% c("HT_STUMP", "DIB_STUMP", "VOL_STUMP", "HT_BH",
                                                                   "DIB_BH", "HT_UTOP", "DIB_UTOP", "VOL_WSV",
                                                                   "VOL_BELOW_UTOP", "VOL_ABOVE_UTOP", "VOL_BELOW_BTOP",
                                                                   "VOL_ABOVE_BTOP"))]
    fullDimTreeData[MEAS_INTENSE %in% c("H-ENHANCED"),
                    c(NAvolumesForHenhancedtrees) := NA]
  }
  fullDimTreeData[,':='(VOL_TOP = VOL_ABOVE_UTOP,
                        VOL_MER = VOL_BELOW_UTOP,
                        VOL_BKT = 0,
                        H_MERCH = HT_UTOP)]
  ## the break is above the utop
  fullDimTreeData[!is.na(BTOP) &
                    VOL_BELOW_UTOP %<<% VOL_BELOW_BTOP,
                  ':='(VOL_BKT = VOL_ABOVE_BTOP,
                       VOL_TOP = VOL_ABOVE_UTOP-VOL_ABOVE_BTOP)]
  ## the break is below the utop
  fullDimTreeData[!is.na(BTOP) &
                    VOL_BELOW_UTOP %>=% VOL_BELOW_BTOP,
                  ':='(VOL_MER = VOL_BELOW_BTOP,
                       VOL_BKT = VOL_ABOVE_BTOP,
                       VOL_TOP = 0,
                       H_MERCH = HT_BTOP)]
  ## for whole stem volume adjustment for btop trees
  fullDimTreeData[!is.na(BTOP),
                  VOL_WSV := VOL_BELOW_BTOP + VOL_STUMP]

  if(compilationType == "nonPSP"){
    fullDimTreeData[, uniobs := 1:nrow(fullDimTreeData)]
    ## adjust log volume for btop trees
    needadjustdata <- fullDimTreeData[!is.na(BTOP) & MEAS_INTENSE %in% c("FULL", "ENHANCED"), ]
    fullDimTreeData <- fullDimTreeData[!(uniobs %in% needadjustdata$uniobs), ]
    for(indilogbtop in unique(needadjustdata$LOG_BTOP)){
      needadjustdata[LOG_BTOP == indilogbtop, paste("LOG_V_", (indilogbtop+1):9, sep = "") := as.numeric(NA)]
      if(indilogbtop == 1){
        needadjustdata[LOG_BTOP == indilogbtop, lowerlogs := 0]
      } else {
        needadjustdata[LOG_BTOP == indilogbtop, lowerlogs := rowSums(needadjustdata[LOG_BTOP == indilogbtop, paste("LOG_V_", 1:(indilogbtop-1), sep = ""),
                                                                                    with = FALSE], na.rm = TRUE)]
      }
      needadjustdata[LOG_BTOP == indilogbtop, paste("LOG_V_", indilogbtop, sep = "") := VOL_BELOW_BTOP - lowerlogs]
      needadjustdata[, lowerlogs := NULL]
    }
    fullDimTreeData <- rbindlist(list(fullDimTreeData, needadjustdata))
    rm(needadjustdata)
    ## adjust the merchantable volume for the log for btop trees
    ## for btop higher than utop, nonthing need to do
    ## for btop lower than utop
    needadjustdata <- fullDimTreeData[!is.na(BTOP) & VOL_BELOW_UTOP %>>% VOL_BELOW_BTOP &
                                        MEAS_INTENSE %in% c("FULL", "ENHANCED"), ]
    fullDimTreeData <- fullDimTreeData[!(uniobs %in% needadjustdata$uniobs), ]
    for(indilogbtop in unique(needadjustdata$LOG_BTOP)){
      needadjustdata[LOG_BTOP == indilogbtop, paste("LOG_VM_", (indilogbtop+1):9, sep = "") := as.numeric(NA)]
      if(indilogbtop == 1){
        needadjustdata[LOG_BTOP == indilogbtop, lowerlogs := 0]
      } else {
        needadjustdata[LOG_BTOP == indilogbtop, lowerlogs := rowSums(needadjustdata[LOG_BTOP == indilogbtop, paste("LOG_VM_", 0:(indilogbtop-1), sep = ""),
                                                                                    with = FALSE], na.rm = TRUE)]
      }
      needadjustdata[LOG_BTOP == indilogbtop, paste("LOG_VM_", indilogbtop, sep = "") := VOL_MER - lowerlogs]
      needadjustdata[, lowerlogs := NULL]
    }
    fullDimTreeData <- rbindlist(list(fullDimTreeData, needadjustdata))
    rm(needadjustdata)
    netFacteredTree <- fullDimTreeData[MEAS_INTENSE %in% c("FULL", "ENHANCED"),]
    nonnetFacteredTree <- fullDimTreeData[!(uniobs %in% netFacteredTree$uniobs),]
    netvols <- netVolumeCalculator(grossVolMatrix = netFacteredTree[, paste("LOG_V_", 0:9, sep = ""), with = FALSE],
                                   grossMerchVolMatrix = netFacteredTree[, paste("LOG_VM_", 1:9, sep = ""), with = FALSE],
                                   netFactorMatrix = netFacteredTree[, paste("LOG_S_", 1:9, sep = ""), with = FALSE])
    netFacteredTree <- cbind(netFacteredTree, netvols)
    fullDimTreeData <- rbindlist(list(netFacteredTree, nonnetFacteredTree), fill = TRUE)
    rm(netFacteredTree, nonnetFacteredTree, netvols)
    gradedTree <- fullDimTreeData[MEAS_INTENSE %in% c("FULL", "ENHANCED") & !(LOG_G_1 %in% c("*", NA)),]
    nonGradedTree <- fullDimTreeData[!(uniobs %in% gradedTree$uniobs),]
    treevalue <- valueCalculator(species = gradedTree$SPECIES,
                                 grossVolMatrix = gradedTree[, paste("LOG_V_", 0:9, sep = ""), with = FALSE],
                                 grossMerchVolMatrix = gradedTree[, paste("LOG_VM_", 1:9, sep = ""), with = FALSE],
                                 callGradeMatrix = gradedTree[, paste("LOG_G_", 1:9, sep = ""), with = FALSE])
    gradedTree <- cbind(gradedTree, treevalue)
    fullDimTreeData <- rbindlist(list(gradedTree, nonGradedTree), fill = TRUE)
    fullDimTreeData[, uniobs := NULL]
  }
  return(fullDimTreeData)
}
