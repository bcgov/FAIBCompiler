#' Estimate volume for H-enhanced and non-enhanced trees-VRI specific
#'
#' @description This function estimates the volumes for JH-enhanced and non-enhanced trees using \code{BA-WSV} equation and
#'              \code{toWSV} ratio methods. For H-enhanced trees, the whole stem volume and gross merchantable volume are already calculated directly using
#'              taper equations; and rest of volume components will be calculated using ratio method in this function.
#'              For non-enhanced trees, the whole stem volume is derived using regression equation between basal area
#'              and whole stem volume and the rest of volume components will be computed using ratio method in this function.
#'
#' @param nonVolTrees data.table, H-enhanced trees and non-enhanced trees.
#' @param fixedCoeffTable data.table, Specifies the WSV-BA equations by project group \code{PRJ_GRP}, live and dead status \code{LV_D},
#'                                   stand and falling status \code{SF_COMPILE} and species code \code{SP0}. The table can be
#'                                   generated using \code{\link{WSV_BARegression}}.
#' @param randomCoeffTable data.table, Specifies the WSV-BA equations by project group \code{PRJ_GRP}, live and dead status \code{LV_D},
#'                                   stand and falling status \code{SF_COMPILE} and species code \code{SP0}. The table can be
#'                                   generated using \code{\link{WSV_BARegression}}.
#' @param ratioTable data.table, Specifies \code{toWSV} ratio by project group \code{PRJ_GRP}, live and dead status \code{LV_D},
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
#' @rdname treeVolEst_RegRatio
#'
#' @author Yong Luo
#'
treeVolEst_RegRatio <- function(nonVolTrees, fixedCoeffTable, randomCoeffTable, ratioTable){
  nonVolTrees[, SAMP_POINT := as.numeric(substr(CLSTR_ID, 1, 7))]
  fixedCoeffTable <- fixedCoeffTable[,.(BGC_ZONE, SP0, LV_D, INTERCEPT, SLOPE)]
  randomCoeffTable <- unique(randomCoeffTable[,.(BGC_ZONE, SP0, LV_D, SAMP_POINT, INTERCEPT_RDM, SLOPE_RDM)],
                             by = c("BGC_ZONE", "SP0", "LV_D", "SAMP_POINT"))

  volVariables <- c(paste("VOL_",c("WSV", "NET", "MER", "NETM", "NTW2",
                                   "NTWB", "D", "DW", "DWB"),
                          sep = ""), "VAL_MER")
  ## why the last one is included?? val_mer is value not volume

  ratioVariables <- paste("RATIO_",c("WSV", "NET", "MER", "NETM", "NTW2",
                                     "NTWB", "D", "DW", "DWB", "VAL"),
                          sep = "")
  ratioTable <- ratioTable[, c("BGC_ZONE", "SP0", "LV_D", ratioVariables), with = FALSE]
  nonVolTrees <- merge(nonVolTrees, fixedCoeffTable,
                                 by = c("BGC_ZONE", "SP0", "LV_D"),
                                 all.x = TRUE)
  nonVolTrees <- merge(nonVolTrees, randomCoeffTable,
                                 by = c("BGC_ZONE", "SP0", "LV_D", "SAMP_POINT"),
                                 all.x = TRUE)
  nonVolTrees[is.na(INTERCEPT_RDM), INTERCEPT_RDM := 0]
  nonVolTrees[is.na(SLOPE_RDM), SLOPE_RDM := 0]
  nonVolTrees[, VOL_WSV_new := exp(INTERCEPT + (SLOPE + SLOPE_RDM)*log(BA_TREE) + INTERCEPT_RDM)]
  nonVolTrees[MEAS_INTENSE == "NON-ENHANCED", VOL_WSV := VOL_WSV_new]
  if(nrow(nonVolTrees[MEAS_INTENSE == "NON-ENHANCED" & is.na(INTERCEPT)]) > 0){
    a <- nonVolTrees[MEAS_INTENSE == "NON-ENHANCED" & is.na(INTERCEPT),.(No_of_Trees = paste0(length(DBH), " trees")),
                     by = c("BGC_ZONE", "SP0", "LV_D")]
    a[, text := paste0(BGC_ZONE, " + ", SP0, " + ", LV_D, ": ", No_of_Trees, "\n")]
    warning("Whole stem volume for below trees can not be derived using regression method: \n", a$text)
  }
  nonVolTrees[,':='(INTERCEPT = NULL,
                    SLOPE = NULL,
                    INTERCEPT_RDM = NULL,
                    SLOPE_RDM = NULL,
                    VOL_WSV_new = NULL)]

  nonVolTrees <- FAIBBase::merge_dupUpdate(nonVolTrees, ratioTable,
                                 by = c("BGC_ZONE", "SP0", "LV_D"),
                                 all.x = TRUE)
  # nonVolTrees[, NET_FCT_METHOD := "Ratio"]
  nonVolTrees[is.na(VOL_WSV), VOL_WSV := 0]
  ### estimate volumn components for tree with dbh >= 10cm
  output <- nonVolTrees[DBH %<<% 10 | is.na(DBH),] # add is.na to make the whole dataset
  ## is covered
  output[, NET_FCT_METHOD := "Not applicable"]
  nonVolTrees <- nonVolTrees[DBH %>=% 10,]
  nonVolTrees[, NET_FCT_METHOD := "Ratio"]
  ### for trees with height information in the recent inventoried data
  ### vol_mer can be estimated directly using kozak equations, and do not need to apply ratio method.
  ### for the trees without height (I.E., NON-ENHANCED), assign vol_net using ratio method
  nonVolTrees[MEAS_INTENSE == "NON-ENHANCED",
              volVariables[3] := unlist(nonVolTrees[MEAS_INTENSE == "NON-ENHANCED",
                                                    ratioVariables[3], with = FALSE]) * VOL_WSV]
  ### for all trees, i.e., with and without height information, estimate the rest of volume components
  ### using ratio method
  for(i in c(2, 4:length(volVariables))){
    nonVolTrees[, volVariables[i] := unlist(nonVolTrees[, ratioVariables[i],
                                                        with = FALSE]) * VOL_WSV]
  }
  output <- rbindlist(list(nonVolTrees, output), fill = TRUE)

  set(output, ,c(ratioVariables), NULL)
  rm(nonVolTrees)
  output[VOL_WSV %<<% VOL_NTWB, VOL_NTWB := VOL_WSV]
  return(output)
}
