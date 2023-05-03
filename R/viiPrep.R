#' Load and select auxiliary plot trees-VRI specific
#'
#' @description This function loads and selects auxiliary data (\code{vi_i}, cardi) based on cluster/plot header.
#'
#' @param compilationType character, either \code{PSP} or \code{nonPSP}. If it is \code{PSP}, it
#'                               is consistent with original PSP compiler, otherwise, it
#'                               is consistent with VRI compiler.
#' @param clusterplotHeader data.table, Cluster and plot level attributes, an output from \code{\link{VRIInit_clusterplot}}.
#'
#' @param dataSourcePath character, Specifies the path that directs to the VRI original data soruce.
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
#' @rdname viiPrep
#'
#' @author Yong Luo
viiPrep<- function(compilationType,
                   clusterplotHeader,
                   dataSourcePath){
  vi_i <- readRDS(file.path(dataSourcePath, "vi_i.rds")) %>% data.table
  clusterplotHeader[, clusterplot := paste(CLSTR_ID, PLOT, sep = "_")]
  vi_i[, clusterplot := paste(CLSTR_ID, PLOT, sep = "_")]
  vi_i <- vi_i[clusterplot %in% clusterplotHeader$clusterplot,]
  if(nrow(vi_i) > 0){
    vi_i <- unique(vi_i, by = c("CLSTR_ID", "PLOT", "TREE_NO"))
    vi_i[is.na(LV_D), LV_D := "L"]
    vi_i[, TREE_WT := 1]
    vi_i[DBH != 0, BA_TREE := pi * ((DBH/200)^2)]
    vi_i <- FAIBBase::merge_dupUpdate(vi_i, clusterplotHeader[, .(clusterplot, SAMP_TYP,
                                                                  BLOWUP_MAIN, BLOWUP_SUBPLOT,
                                                                  SAMPLE_BREAK_POINT,
                                                                  DBH_LIMIT_TAG,
                                                                  PLOT_WT)],
                                      by = "clusterplot", all.x = TRUE)
    if(compilationType == "nonPSP"){
      vi_i[, PHF_TREE := FAIBBase::PHFCalculator(sampleType = SAMP_TYP, blowUp = BLOWUP_MAIN, treeWeight = TREE_WT,
                                                 plotWeight = PLOT_WT, treeBasalArea = BA_TREE)]
    } else {
      vi_i[DBH >= SAMPLE_BREAK_POINT |
             MEASUREMENT_ANOMALY_CODE == "PSP-TALLY",
           PHF_TREE := FAIBBase::PHFCalculator(sampleType = SAMP_TYP, blowUp = BLOWUP_MAIN,
                                               treeWeight = TREE_WT, plotWeight = 1,
                                               treeBasalArea = BA_TREE)]
      vi_i[is.na(PHF_TREE) &
             !is.na(BLOWUP_SUBPLOT),
           PHF_TREE := FAIBBase::PHFCalculator(sampleType = SAMP_TYP, blowUp = BLOWUP_SUBPLOT,
                                               treeWeight = TREE_WT, plotWeight = 1,
                                               treeBasalArea = BA_TREE)]
    }
    return(vi_i[,.(CLSTR_ID, BEC_ZONE, BEC_SBZ, BEC_VAR, PLOT, TREE_NO, SPECIES_ORG, SPECIES, SP0,
                   DBH, BA_TREE,
                   PHF_TREE, LV_D,
                   MEASUREMENT_ANOMALY_CODE,
                   TREE_CLASS_CODE)])
  } else {
    return(vi_i[,.(CLSTR_ID, BEC_ZONE, BEC_SBZ, BEC_VAR, PLOT, SPECIES, TREE_NO)])
  }
  ## please note that for auxiliary plots, they are part of VRI design, therefore, there no small tree plot (DBH<9),
  ## no need for adjusting tree per ha factor
}
