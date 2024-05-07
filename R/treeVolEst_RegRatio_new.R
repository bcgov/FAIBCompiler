#' Estimate volume for H-enhanced and non-enhanced trees-VRI specific
#'
#' @description This function estimates the volumes for JH-enhanced and non-enhanced trees using \code{BA-WSV} equation and
#'              \code{toWSV} ratio methods. For H-enhanced trees, the whole stem volume and gross merchantable volume are already calculated directly using
#'              taper equations; and rest of volume components will be calculated using ratio method in this function.
#'              For non-enhanced trees, the whole stem volume is derived using regression equation between basal area
#'              and whole stem volume and the rest of volume components will be computed using ratio method in this function.
#'
#' @param nonVolTrees data.table, H-enhanced trees and non-enhanced trees.
#' @param BA_WSVCoeff list, Contains fixed and random coefficients the WSV-BA equations by project group \code{PRJ_GRP}, live and dead status \code{LV_D},
#'                                   stand and falling status \code{SF_COMPILE} and species code \code{SP0}. The table can be
#'                                   generated using \code{\link{WSV_BARegression}}.
#' @param ratioTable list, Specifies \code{toWSV} ratio by project group \code{PRJ_GRP}, live and dead status \code{LV_D},
#'                                   stand and falling status \code{SF_COMPILE} and species code \code{SP0}.
#'                                   The table can be generated using \code{\link{toWSVRatio}}.
#'
#' @return A data table that has compiled non volume trees.
#'
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#' @importFrom FAIBBase merge_dupUpdate
#'
#'
#' @export
#' @docType methods
#' @rdname treeVolEst_RegRatio_new
#'
#' @author Yong Luo
#'
treeVolEst_RegRatio_new <- function(nonVolTrees,
                                    BA_WSVCoeff,
                                    ratioTable){
  nonVolTrees[, SAMP_POINT := substr(CLSTR_ID, 1, 7)]
  fixedCoeffTable <- BA_WSVCoeff$fixedcoeff[,.(BEC_ZONE, SP0, LV_D, INTERCEPT, SLOPE)]
  randomCoeffTable <- unique(BA_WSVCoeff$randomcoeff[,.(BEC_ZONE, SP0, LV_D, SAMP_POINT, INTERCEPT_RDM, SLOPE_RDM)],
                             by = c("BEC_ZONE", "SP0", "LV_D", "SAMP_POINT"))
  nonVolTrees <- merge(nonVolTrees, fixedCoeffTable,
                       by = c("BEC_ZONE", "SP0", "LV_D"),
                       all.x = TRUE)
  nonVolTrees <- merge(nonVolTrees, randomCoeffTable,
                       by = c("BEC_ZONE", "SP0", "LV_D", "SAMP_POINT"),
                       all.x = TRUE)
  nonVolTrees[is.na(INTERCEPT_RDM), INTERCEPT_RDM := 0]
  nonVolTrees[is.na(SLOPE_RDM), SLOPE_RDM := 0]
  nonVolTrees[, VOL_WSV_new := exp(INTERCEPT + (SLOPE + SLOPE_RDM)*log(BA_TREE) + INTERCEPT_RDM)]
  nonVolTrees[MEAS_INTENSE == "NON-ENHANCED", VOL_WSV := VOL_WSV_new]
  nonVolTrees[,':='(INTERCEPT = NULL,
                    SLOPE = NULL,
                    INTERCEPT_RDM = NULL,
                    SLOPE_RDM = NULL,
                    VOL_WSV_new = NULL)]

  # for vol_mer
  nonVolTrees <- FAIBBase::merge_dupUpdate(nonVolTrees,
                                           ratioTable$mer_ratio[,.(BEC_ZONE, SP0, LV_D, a, b, c, d, j)],
                                           by = c("BEC_ZONE", "SP0", "LV_D"),
                                           all.x = TRUE)
  ## for DBH >= 10
  nonVolTrees[DBH %>=% 10,
              MER_RATIO := a * (1 - exp(-b * (DBH-10)))^c + (d*(DBH-10))/(DBH-10 + j)]
  nonVolTrees[MEAS_INTENSE == "NON-ENHANCED" &
                DBH %>=% 10, VOL_MER := MER_RATIO * VOL_WSV]
  nonVolTrees[, c("a", "b", "c", "d", "j", "MER_RATIO") := NULL]
  ## for DBH <= 10, the vol_mer is 0
  nonVolTrees[MEAS_INTENSE == "NON-ENHANCED" & DBH %<<% 10,
              VOL_MER := 0]

  #########################################
  # for vol_ntwb
  nonVolTrees <- FAIBBase::merge_dupUpdate(nonVolTrees,
                                           ratioTable$ntwb_ratio[,.(BEC_ZONE, SP0, LV_D, a, b, c)],
                                           by = c("BEC_ZONE", "SP0", "LV_D"),
                                           all.x = TRUE)
  # for DBH > 10
  nonVolTrees[DBH %>=% 10,
              NTWB_RATIO := a * (1 - exp(-b * (DBH-10)))^c]
  nonVolTrees[MEAS_INTENSE %in% c("NON-ENHANCED", "H-ENHANCED") &
                DBH %>=% 10, VOL_NTWB := NTWB_RATIO * VOL_WSV]
  nonVolTrees[, c("a", "b", "c", "NTWB_RATIO") := NULL]
  # for DBH <= 10
  nonVolTrees[MEAS_INTENSE %in% c("NON-ENHANCED", "H-ENHANCED") & DBH %<<% 10,
              VOL_NTWB := 0]
  #########################################
  # for vol_dwb
  nonVolTrees <- FAIBBase::merge_dupUpdate(nonVolTrees,
                                           ratioTable$dwb_ratio[,.(BEC_ZONE, SP0, LV_D, a, b, c)],
                                           by = c("BEC_ZONE", "SP0", "LV_D"),
                                           all.x = TRUE)
  # for DBH > 10
  nonVolTrees[DBH %>=% 10,
              DWB_RATIO := a * (1 - exp(-b * (DBH-10)))^c]
  nonVolTrees[MEAS_INTENSE %in% c("NON-ENHANCED", "H-ENHANCED") &
                DBH %>=% 10, VOL_DWB := DWB_RATIO * VOL_WSV]
  nonVolTrees[, c("a", "b", "c", "DWB_RATIO") := NULL]
  # for DBH <= 10
  nonVolTrees[MEAS_INTENSE %in% c("NON-ENHANCED", "H-ENHANCED") & DBH %<<% 10,
              VOL_DWB := 0]

  nonVolTrees[MEAS_INTENSE %in% c("NON-ENHANCED", "H-ENHANCED") &
                DBH %>=% 10, NET_FCT_METHOD := "Ratio"]
  nonVolTrees[DBH %<<% 10 | is.na(DBH),
              NET_FCT_METHOD := "Not applicable"] # add is.na to make the whole dataset
  nonVolTrees[is.na(VOL_WSV), VOL_WSV := 0]

  nonVolTrees[VOL_WSV %<<% VOL_NTWB,
              VOL_NTWB := VOL_WSV]
  return(nonVolTrees)
}
