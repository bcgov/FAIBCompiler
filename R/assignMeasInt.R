#' assign measurement intensity based on field measured attributes
#'
#' @description This function it to assign measurement intensity based on field measured attributes.
#'
#' @param compilationType character, either \code{PSP} or \code{nonPSP}. If it is \code{PSP}, it
#'                               is consistent with original PSP compiler, otherwise, it
#'                               is consistent with VRI compiler.
#' @param vic data.table, vic.
#' @param vii data.table, vii.
#' @param vid data.table, vid.
#'
#' @return Two data tables: fullDimTrees contains all the trees that have both DBH and HT,
#'                          nonHTTrees contains the trees that have only DBH infor.
#'
#'
#' @importFrom data.table data.table ':=' set rbindlist setnames setkey
#' @importFrom dplyr '%>%'
#' @export
#' @docType methods
#' @rdname assignMeasInt
#'
#' @author Yong Luo
assignMeasInt <- function(compilationType,
                          vic,
                          vii,
                          vid){
  if(compilationType == "nonPSP"){
    vic[LOG_G_1 == "*",
        MEAS_INTENSE := "H-ENHANCED"]
    ## B sample trees are H-enhnced trees
    vic[TYPE_CD == "B",
        MEAS_INTENSE := "H-ENHANCED"]
    vic[is.na(MEAS_INTENSE) & PLOT == "I",
        MEAS_INTENSE := "FULL"]
    vic[is.na(MEAS_INTENSE),
        MEAS_INTENSE := "ENHANCED"]
    ## for the full/enhanced trees, if the length of first log is missing, assign
    ## them with tree height
    vic[MEAS_INTENSE %in% c("FULL", "ENHANCED") & LOG_L_1 %in% c(NA, 0),
        LOG_L_1 := HEIGHT]
    ## for the zero tree height trees, force them as non-enhanced trees, which means
    ## they only have DBH information
    vic[HEIGHT %in% c(NA, 0), MEAS_INTENSE := "NON-ENHANCED"]
    vii[, MEAS_INTENSE := "NON-ENHANCED"]
    tree_all <- rbindlist(list(vic, vii), fill = TRUE)
  } else {
    vic[, MEAS_INTENSE := "H-ENHANCED"] # for the non-broken top trees, tree height is tree length
    vii[, MEAS_INTENSE := "NON-ENHANCED"]
    tree_all <- rbindlist(list(vic, vii), fill = TRUE)
    tree_all <- merge(tree_all, vid[,.(CLSTR_ID, PLOT, TREE_NO, full = TRUE)],
                      by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                      all.x = TRUE)
    tree_all[MEAS_INTENSE == "H-ENHANCED" & full == TRUE,
             MEAS_INTENSE := "FULL"]
  }
  return(list(fullDimTrees = tree_all[MEAS_INTENSE %in% c("FULL", "ENHANCED", "H-ENHANCED"),],
              nonHTTrees = tree_all[MEAS_INTENSE == "NON-ENHANCED",
                                          .(CLSTR_ID, BEC_ZONE, BEC_SBZ, BEC_VAR, PLOT, TREE_NO,
                                            SPECIES_ORG, SPECIES, SP0,  DBH,  BA_TREE,
                                            PHF_TREE, LV_D, MEAS_INTENSE,
                                            MEASUREMENT_ANOMALY_CODE, TREE_CLASS_CODE,
                                            TAGGING_SECTOR_NO, SITE_SECTOR_NO,
                                            RESIDUAL)]))
}
