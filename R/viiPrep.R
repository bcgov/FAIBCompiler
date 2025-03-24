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
    vi_i[, ':='(TREE_WT = 1,
                TREE_WT_WK = 1)]
    vi_i[DBH != 0, BA_TREE := pi * ((DBH/200)^2)]
    vi_i <- FAIBBase::merge_dupUpdate(vi_i, clusterplotHeader[, .(clusterplot, SAMP_TYP,
                                                                  BEC_ZONE, BEC_SBZ, BEC_VAR,
                                                                  BLOWUP_MAIN, BLOWUP_SUBPLOT,
                                                                  SAMPLE_BREAK_POINT,
                                                                  DBH_LIMIT_TAG,
                                                                  PLOT_WT, TYPE_CD,
                                                                  SAMPLE_ESTABLISHMENT_TYPE)],
                                      by = "clusterplot", all.x = TRUE)
    if(compilationType == "nonPSP"){
      vi_i <- vi_i[DBH >= 4, ] # for nonPSP, only dbh bigger than or equal to 4 cm is valid
      vi_i[, PHF_TREE := FAIBBase::PHFCalculator(sampleType = SAMP_TYP, blowUp = BLOWUP_MAIN, treeWeight = TREE_WT,
                                                 plotWeight = PLOT_WT, treeBasalArea = BA_TREE)]
      vi_i[, PHF_TREE_WK := FAIBBase::PHFCalculator(sampleType = SAMP_TYP, blowUp = BLOWUP_MAIN,
                                                    treeWeight = TREE_WT_WK,
                                                 plotWeight = PLOT_WT, treeBasalArea = BA_TREE)]
      # for NFI (F), CMI and YSMI, the plots use a 100 m2 subplot for
      # trees with a dbh < 9, therefore should be extrapolate to 400 m2 (size of large tree plot)
      vi_i[TYPE_CD %in% c("F", "M", "Y", "L") & DBH < 9,
           ':='(PHF_TREE = PHF_TREE*4,
                PHF_TREE_WK = PHF_TREE_WK*4)]
      #   2)	FHYSM Only - Can we change the PHF_TREE from 25 to 100 for the 4-9cm DBH deciduous trees in the 5.64m subplot?
      #   •	The reason why is because there are non-standard diameter limits used in this project. All conifers >1m height in the 11.28m radius main plot are tagged (all conifers get a PHF_TREE = 25) while deciduous are tagged using standard YSM/CMI protocol.  ie., trees 4-9cm dbh in the 5.64m radius subplot (PHF_TREE = 100), and trees >9cm dbh in the 11.28m radius main plot (PHF_TREE = 25).
      #   •	It looks like it only impacts one tree so far but these FHYSM samples will be remeasured and we may get more.
      deci_sp <- lookup_species()
      deci_sp <- unique(deci_sp[SP_TYPE == "D"]$SPECIES)
      vi_i[SAMPLE_ESTABLISHMENT_TYPE == "FHYSM" & DBH < 9 & SPECIES %in% deci_sp,
           PHF_TREE := PHF_TREE*4]
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
      vi_i[,':='(TREE_WT_WK = as.numeric(NA),
                 PHF_TREE_WK = as.numeric(NA))]
    }
    return(vi_i[,.(CLSTR_ID, BEC_ZONE, BEC_SBZ, BEC_VAR, PLOT, TREE_NO, SPECIES, SP0,
                   DBH, BA_TREE,
                   TREE_WT, TREE_WT_WK,
                   PHF_TREE, PHF_TREE_WK, LV_D,
                   MEASUREMENT_ANOMALY_CODE,
                   TREE_CLASS_CODE,
                   TAGGING_SECTOR_NO,
                   SITE_SECTOR_NO,
                   RESIDUAL)])
  } else {
    return(vi_i[,.(CLSTR_ID, BEC_ZONE, BEC_SBZ, BEC_VAR, PLOT, SPECIES, TREE_NO)])
  }
  ## please note that for auxiliary plots, they are part of VRI design, therefore, there no small tree plot (DBH<9),
  ## no need for adjusting tree per ha factor
}
