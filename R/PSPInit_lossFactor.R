#' Load and select trees that have loss factor information-VRI specific
#'
#' @description This function loads and selects trees that have loss factor information (\code{vi_d}, cardd) based on
#'              selected trees from vi_c.
#'
#' @param fullMeasuredTrees data.table, Selected trees in vi_c, which includes full, enhanced and H-enhanced trees.
#'                                      An output of \code{\link{VRIInit_measuredTree}}.
#'
#' @param dataSourcePath character, Specifies the path that directs to the VRI original data soruce, i.e.,
#'                                  \code{//Mayhem/GIS_TIB/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/vri_sa}.
#'
#'
#' @return A data table that contains loss factor data. A log file documents the detailed process
#'
#'
#' @importFrom data.table data.table ':=' set rbindlist setnames setkey
#' @importFrom dplyr '%>%'
#' @export
#' @docType methods
#' @rdname PSPInit_lossFactor
#'
#' @author Yong Luo
#'
PSPInit_lossFactor<- function(fullMeasuredTrees,
                              dataSourcePath){
  lossfactors <- readRDS(file.path(dataSourcePath, "vi_d.rds")) %>% data.table
  names(lossfactors) <- toupper(names(lossfactors))
  targetnames <- names(lossfactors)
  targetnames <- targetnames[substr(targetnames, 1, 4) == "LOSS"]
  targetnames <- gsub("LOSS", "", targetnames)
  targetnames <- gsub("_IN", "", targetnames)
  targetnames_max <- max(as.numeric(targetnames))
  lossfactors <- lossfactors[, c("CLSTR_ID", "PLOT", "TREE_NO",
                                 paste("LOSS", 1:targetnames_max, "_IN", sep = ""),
                                 paste("LOC", 1:targetnames_max, "_FRO", sep = "")), with = FALSE]
  rm(targetnames, targetnames_max)
  lossfactors <- unique(lossfactors, by = c("CLSTR_ID", "PLOT", "TREE_NO"))
  # fullMeasuredTrees[, clusterplottree := paste(CLSTR_ID, "_", PLOT, "_", TREE_NO, sep = "")]
  # lossfactors[, clusterplottree := paste(CLSTR_ID, "_", PLOT, "_", TREE_NO, sep = "")]
  # lossfactors <- lossfactors[clusterplottree %in% fullMeasuredTrees$clusterplottree,]
  # lossfactors[, ':='(clusterplottree = NULL)]
  return(lossfactors)
}
