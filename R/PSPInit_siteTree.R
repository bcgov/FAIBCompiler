#' Load and select site trees-VRI specific
#'
#' @description This function connects site tree data (vi_h, cardh) to selected cluster/plot-level data.
#'              Site tree data is located in  \code{//Mayhem/GIS_TIB/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/vri_sa}
#'
#' @param clusterplotHeader data.table, contains cluster/plot-level attributes, an output from \code{\link{VRIInit_clusterplot}}.
#'
#' @param dataSourcePath character, Specifies the path that directs to the VRI original data soruce, i.e.,
#'                                  \code{//Mayhem/GIS_TIB/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/vri_sa}.
#'
#'
#' @return A data table that contains site tree data information. A log file documents the detailed process
#'
#' @note VRI specific
#'
#' @importFrom data.table data.table ':=' set rbindlist setkey
#' @importFrom dplyr '%>%'
#' @export
#' @docType methods
#' @rdname PSPInit_siteTree
#'
#' @author Yong Luo
PSPInit_siteTree <- function(clusterplotHeader,
                            dataSourcePath){
  displayColumn <- c("CLSTR_ID", "PLOT", "TREE_NO")
  vi_h <- readRDS(file.path(dataSourcePath, "vi_h.rds")) %>% data.table
  names(vi_h) <- toupper(names(vi_h))
  clusterplotHeader[, clusterplot := paste(CLSTR_ID, PLOT, sep = "_")]
  vi_h[, clusterplot := paste(CLSTR_ID, PLOT, sep = "_")]

  vi_h <- vi_h[clusterplot %in% clusterplotHeader$clusterplot,]

  vi_h <- FAIBBase::merge_dupUpdate(vi_h, clusterplotHeader[, .(clusterplot,
                                                                BEC_ZONE,
                                                                BEC_SBZ)],
                                    by = "clusterplot", all.x = TRUE)
  vi_h[, SPECIES_ORG := SPECIES]
  vi_h[, SPECIES := speciesCorrection(SPECIES,
                                      BEC_ZONE,
                                      BEC_SBZ)]

  vi_h <- unique(vi_h, by = displayColumn)

  # range(vi_h$BNG_DIAM, na.rm = TRUE) # from 0.1 to 999.9, is 999.9 correct?
  vi_h[(!is.na(BNG_DIAM) | BNG_DIAM != 0) & (!is.na(BARK_THK) | BARK_THK != 0),
       DBH := BNG_DIAM + 2*BARK_THK/10]
  vi_h[, ':='(clusterplot = NULL)]
  ##### sas codes
  # IF UPCASE(WALKTHRU_STATUS) = 'W' THEN TREE_WT = 2;
  # ELSE IF UPCASE(WALKTHRU_STATUS) = 'O' THEN TREE_WT = 0;
  # ELSE WALKTHRU_STATUS = 1;
  vi_h <- vi_h[order(CLSTR_ID, PLOT, TREE_NO),]
  return(vi_h)
}
