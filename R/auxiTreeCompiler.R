#' Derive volume components for H-enhanced and non-enhanced trees using external coeff and ratio-VRI specific
#'
#'
#' @description Estimates volume components for H-enhanced and non-enhanced trees using regression and ratio methods.
#'              For H-enhanced trees, the whole stem volume and gross merchantable volume are already calculated directly using
#'              taper equations; and rest of volume components will be calculated using ratio method in this function.
#'              For non-enhanced trees, the whole stem volume is derived using regression equation between basal area
#'              and whole stem volume and the rest of volume components will be computed using ratio method in this function.
#'
#' @param fullMeasuredTrees Compiled tree-level data in vi_c, which contains full measured trees, enhanced trees
#'                          and H-enhanced trees. This data is output of \code{\link{DWBCompiler}}
#'
#' @param auxiTrees data.table, Non-enhanced trees in anxilirary plots, however, it may have enhanced trees and H-enhanced trees.
#'                              An output from \code{\link{VRIInit_auxTree}}.
#'
#' @param clusterPlotHeader data.table, Cluster and plot-level information. An output of \code{\link{VRIInit_clusterplot}}.
#'
#' @param fixedCoeff data.table, Specifies the WSV-BA equations by project group \code{PRJ_GRP}, live and dead status \code{LV_D},
#'                                   stand and falling status \code{SF_COMPILE} and species code \code{SP0}. The table can be
#'                                   generated using \code{\link{WSV_BARegression}}.
#' @param randomCoeff data.table, Specifies the WSV-BA equations by project group \code{PRJ_GRP}, live and dead status \code{LV_D},
#'                                   stand and falling status \code{SF_COMPILE} and species code \code{SP0}. The table can be
#'                                   generated using \code{\link{WSV_BARegression}}.
#'
#' @param ratios data.table, Specifies \code{toWSV} ratio by project group \code{PRJ_GRP}, live and dead status \code{LV_D},
#'                                   stand and falling status \code{SF_COMPILE} and species code \code{SP0}.
#'                                   The table can be generated using \code{\link{toWSVRatio}}.
#'
#' @return A list of four tables: 1. fullenhancedtrees: full and enhanced trees;
#'         2. HnonenhancedTrees: Height enhanced and non-enhanced trees;
#'
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#' @note The data selection procedure for regression has been standardized as following:
#' \enumerate{
#' \item Start from vi_c, which has all trees have minimum information of DBH and Height;
#' \item Select all the full, enhanced and H-enhanced trees;
#' \item Remove observations in Audit plots and have zero whole stem volume;
#' \item Select the latest observation for each tree by live_dead status. A tree's identity is considered same when it is from same proj_id, samp_no and plot.
#' }
#' The data selection for ratio has been standardized as following:
#' \enumerate{
#' \item Start from the that used for regression;
#' \item Select all the full and enhanced trees;
#' \item Select the trees with DBH >= 10cm
#' }
#'
#' @export
#' @docType methods
#' @rdname auxiTreeCompiler
#'
#' @author Yong Luo
auxiTreeCompiler <- function(fullMeasuredTrees, auxiTrees, clusterPlotHeader,
                                                   fixedCoeff, randomCoeff, ratios){
  randomCoeff[, SAMP_POINT := as.numeric(SAMP_POINT)]
  samples <- unique(clusterPlotHeader[,.(CLSTR_ID, BGC_ZONE)], by = "CLSTR_ID")
  tree_vb <- mergeAllVolTrees(treeMS = data.table::copy(fullMeasuredTrees),
                              treeAX = data.table::copy(auxiTrees))
  rm(fullMeasuredTrees, auxiTrees)
  tree_vb <- merge(tree_vb, samples, by = "CLSTR_ID", all.x = TRUE)
  # derive whole stem volume for non-enhanced trees
  nonvoltrees <- tree_vb[MEAS_INTENSE %in% c("H-ENHANCED", "NON-ENHANCED"),] ## tree with H-enhanced and non-enhanced
  nonvoltrees_compiled <- treeVolEst_RegRatio(nonvoltrees,
                                                  fixedCoeff, randomCoeff, ratios)
  fullenhancedtrees <- tree_vb[MEAS_INTENSE %in% c("FULL", "ENHANCED")]
  return(list(fullenhancedtrees = fullenhancedtrees,
              HnonenhancedTrees = nonvoltrees_compiled))
}
