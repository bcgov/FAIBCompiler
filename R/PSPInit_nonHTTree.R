#' Load nonHT trees for PSP compilation
#'
#' @description This function loads nonHT tree based on cluster/plot header.
#'
#'
#' @param clusterplotHeader data.table, Cluster and plot level attributes, an output from \code{\link{VRIInit_clusterplot}}.
#'
#' @param dataSourcePath character, Specifies the path that directs to the VRI original data soruce, i.e.,
#'                                  \code{//Mayhem/GIS_TIB/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/vri_sa}.
#'
#'
#' @return A data table that contains auxiliary plot tree data.
#'
#'
#' @importFrom data.table data.table ':=' set rbindlist setnames setkey
#' @importFrom dplyr '%>%'
#' @importFrom FAIBBase merge_dupUpdate PHFCalculator
#'
#' @export
#' @docType methods
#' @rdname PSPInit_nonHTTree
#'
#' @author Yong Luo
PSPInit_nonHTTree<- function(clusterplotHeader,
                           dataSourcePath){
  vi_i <- readRDS(file.path(dataSourcePath, "vi_i.rds")) %>% data.table
  names(vi_i) <- toupper(names(vi_i))

  clusterplotHeader[, clusterplot := paste(CLSTR_ID, PLOT, sep = "_")]
  vi_i[, clusterplot := paste(CLSTR_ID, PLOT, sep = "_")]
  vi_i <- vi_i[clusterplot %in% clusterplotHeader$clusterplot,]
  if(nrow(vi_i) > 0){
    vi_i <- unique(vi_i, by = c("CLSTR_ID", "PLOT", "TREE_NO"))
    vi_i[is.na(LV_D), LV_D := "L"]
    vi_i[, TREE_WT := 1]
    vi_i[DBH != 0, BA_TREE := pi * ((DBH/200)^2)]
    vi_i <- FAIBBase::merge_dupUpdate(vi_i, clusterplotHeader[, .(clusterplot, SAMP_TYP,
                                                                  BLOWUP, PLOT_WT,
                                                                  BEC_ZONE,
                                                                  BEC_SBZ)],
                                      by = "clusterplot", all.x = TRUE)
    vi_i[, SPECIES_ORG := SPECIES]
    vi_i[, SPECIES := speciesCorrection(SPECIES,
                                        BEC_ZONE,
                                        BEC_SBZ)]

    vi_i[, PHF_TREE := FAIBBase::PHFCalculator(sampleType = SAMP_TYP, blowUp = BLOWUP, treeWeight = TREE_WT,
                                               plotWeight = 1, treeBasalArea = BA_TREE)]
    return(vi_i[,.(CLSTR_ID, PLOT, TREE_NO, SPECIES_ORG, SPECIES,
                   DBH, BA_TREE,
                   PHF_TREE, LV_D,
                   BROKEN_TOP_IND, DIAM_BTP,
                   HT_PROJ,
                   HT_BTOP = HEIGHT_TO_BREAK)])
  } else {
    return(vi_i[,.(CLSTR_ID, PLOT, SPECIES, TREE_NO)])
  }
  ## please note that for auxiliary plots, they are part of VRI design, therefore, there no small tree plot (DBH<9),
  ## no need for adjusting tree per ha factor

}
