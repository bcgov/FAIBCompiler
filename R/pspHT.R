#' Fit height for PSP nonHT data using different method
#' @description This function is to fit height for the nonHT trees in PSPs.
#'
#' @param treesData data.table, Contains both height trees and nonHT trees in PSPs.
#'
#' @param method character, Method to derive height, currently it supports
#'                          \code{bestHeightModel}.
#'
#' @param coeffs data.table, Coefficient table.
#'
#' @return Full list of trees with derived height.
#'
#' @importFrom data.table data.table ':=' set rbindlist setnames setkey
#' @importFrom dplyr '%>%'
#' @export
#' @docType methods
#' @rdname pspHT
#'
#' @author Yong Luo
pspHT<- function(treeData,
                 method = "bestMEM",
                 coeffs){

  if(nrow(treeData[is.na(DBH)]) > 0){
    warning("There are some observations do not have DBH. Hence, the height will not be produced.")
  }
  if(method == "bestMEM"){
    browser()
    treeData[!is.na(DBH) & is.na(HEIGHT),
             Height_est := round(heightEstimate_byHeightModel(beczone = BEC_ZONE,
                                                              subzone = BEC_SBZ,
                                                              species = SPECIES,
                                                              DBH = DBH,
                                                              heightModels = coeffs))]
    treeData[!is.na(DBH) & is.na(HEIGHT),
             ':='(HEIGHT = Height_est,
                  HEIGHT_SOURCE = "Estimated")]
    treeData[is.na(HEIGHT_SOURCE),
             ':='(HEIGHT_SOURCE = "Measured")]
    treeData[, ht_both := length(unique(HEIGHT_SOURCE)),
             by = c("SITE_IDENTIFIER", "PLOT", "TREE_NO")]

    treeData_mixedest <- treeData[ht_both > 1,]
    treeData[, ':='(ht_both = NULL,
                    Height_est = NULL)]
    treeData_mixedest <- treeData_mixedest[order(SITE_IDENTIFIER, PLOT, TREE_NO, VISIT_NUMBER),
                                           .(SITE_IDENTIFIER, PLOT, TREE_NO, VISIT_NUMBER,
                                             DBH, HEIGHT, HEIGHT_SOURCE)]
    treeData_mixedest[, ':='(height_next = shift(HEIGHT, type = "lead"),
                             est_next = shift(HEIGHT_SOURCE, type = "lead")),
                      by = c("SITE_IDENTIFIER", "PLOT", "TREE_NO")]
    treeData_mixedest[, height_dif := height_next - HEIGHT]
    treeData_mixedest[HEIGHT_SOURCE != est_next & height_dif < -3]

  }
  return(treeData)
}
