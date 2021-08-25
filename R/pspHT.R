#' Fit height for PSP nonHT data using different method
#' @description This function is to fit height for the nonHT trees in PSPs.
#'
#' @param treesData data.table, Contains both height trees and nonHT trees in PSPs.
#'
#' @param method character, Method to derive height, currently it supports
#'                          \code{bestHeightModel}.
#'
#' @param coeffPath character, Path that contains coefficients.
#'
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
                 method = "bestHeightModel",
                 coeffPath){
  coeffPath <- compilationPaths$compilation_coeff
  if(method == "bestHeightModel"){
    best_height_models <- read.csv(file.path(coeffPath,
                                             "best_height_models.csv"),
                                   stringsAsFactors = FALSE) %>%
      data.table

  }
}
