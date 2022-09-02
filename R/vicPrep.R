#' prepare vi_c for compilation
#'
#' @description This function selects the tree-level data from vi_c (cardc) based on selected cluster/plot headers.
#'              Additonally, the function calculates basal area and tree per ha factor.
#'
#' @param compilationType character, either \code{PSP} or \code{nonPSP}. If it is \code{PSP}, it
#'                               is consistent with original PSP compiler, otherwise, it
#'                               is consistent with VRI compiler.
#' @param clusterplotHeader data.table, Cluster and plot-level attributes.
#' @param dataSourcePath character, Specifies the path that directs to the compilation_sa.
#'
#' @param walkThru logical, Indicates whether walkthrough sampling protocal is used,
#'                          Tree weight is determined by walkthrough method. In walkthrough
#'                          method, a tree is identified as \code{NA} (no walkthrough applied),
#'                           \code{O} for out tree (not counted), and \code{W} for double counted tree.
#'
#'
#'
#'
#' @return A data table that contains tree-level information. A log file that describes the detailed process.
#'
#'
#' @importFrom data.table data.table ':=' set rbindlist setnames setkey
#' @importFrom dplyr '%>%'
#' @importFrom FAIBBase merge_dupUpdate PHFCalculator
#' @export
#' @docType methods
#' @rdname vicPrep
#'
#' @author Yong Luo
#'
#'
vicPrep<- function(compilationType,
                   clusterplotHeader,
                   dataSourcePath,
                   walkThru = TRUE){
  vi_c <- readRDS(file.path(dataSourcePath, "vi_c.rds")) %>% data.table
  vi_c[, clusterPlot := paste(CLSTR_ID, PLOT, sep = "_")]
  clusterplotHeader[, clusterPlot := paste(CLSTR_ID, PLOT, sep = "_")]
  vi_c <- vi_c[clusterPlot %in% unique(clusterplotHeader$clusterPlot), ]
  vi_c[DBH != 0, BA_TREE := pi * ((DBH/200)^2)]
  # remove get_vars function as SP0 remains same as that in vi_pc table
  vi_c[DBH == 0, ':='(SP0 = NA, BA_TREE = NA)]
  vi_c[, TREE_WT := 1]
  if(compilationType == "nonPSP"){
    if(walkThru){
      vi_c[toupper(WALKTHRU_STATUS) == "O", TREE_WT := 0] # tree is out and is not used
      vi_c[toupper(WALKTHRU_STATUS) == "W", TREE_WT := 2] # tree is
    }
  }
  vi_c <- FAIBBase::merge_dupUpdate(vi_c,
                                    unique(clusterplotHeader[,.(clusterPlot, SAMP_TYP,
                                                                PLOT_WT, BLOWUP,
                                                                FIZ,
                                                                TYPE_CD)],
                                           by = "clusterPlot"),
                                    by = "clusterPlot", all.x = TRUE)

  if(compilationType == "nonPSP"){

    vi_c[, PHF_TREE := FAIBBase::PHFCalculator(sampleType = SAMP_TYP, blowUp = BLOWUP,
                                               treeWeight = TREE_WT, plotWeight = PLOT_WT,
                                               treeBasalArea = BA_TREE)]
  } else {
    vi_c[, PHF_TREE := FAIBBase::PHFCalculator(sampleType = SAMP_TYP, blowUp = BLOWUP,
                                               treeWeight = TREE_WT, plotWeight = 1,
                                               treeBasalArea = BA_TREE)]
  }

  # for NFI (F), CMI and YSMI, the plots use a 100 m2 subplot for
  # trees with a dbh < 9, therefore should be extrapolate to 400 m2 (size of large tree plot)
  vi_c[TYPE_CD %in% c("F", "M", "Y", "L") & DBH < 9,
       PHF_TREE := PHF_TREE*4]
  vi_c[BROKEN_TOP_IND == "Y" &
         !is.na(TREE_LEN),
       HT_BTOP := TREE_LEN] ## as long as TREE_LEN is available, the break height is TREE_LEN
  vi_c[BROKEN_TOP_IND == "Y" &
         is.na(TREE_LEN) &
         !is.na(HEIGHT_TO_BREAK),
       HT_BTOP := HEIGHT_TO_BREAK] ## otherwise, the height_to_break is used as break height
  if(compilationType == "PSP"){
    vi_c <- vi_c[order(CLSTR_ID, PLOT, TREE_NO),.(CLSTR_ID, PLOT,
                                                  BEC_ZONE, BEC_SBZ, BEC_VAR, FIZ,
                                                  TYPE_CD, TREE_NO,
                                                  SPECIES, SPECIES_ORG,
                                                  LV_D, S_F, NO_LOGS = 1,
                                                  TREE_WT, DBH, SP0, BA_TREE, PHF_TREE,
                                                  HEIGHT = TREE_LEN, BARK_PER,
                                                  HT_PROJ, DIAM_BTP, BROKEN_TOP_IND,
                                                  HT_BTOP,
                                                  MEASUREMENT_ANOMALY_CODE)]
    return(vi_c)
  } else {
    vi_c <- vi_c[order(CLSTR_ID, PLOT, TREE_NO),.(CLSTR_ID, PLOT,
                                                  BEC_ZONE, BEC_SBZ, BEC_VAR, FIZ,
                                                  TYPE_CD, TREE_NO,
                                                  SPECIES, SPECIES_ORG,
                                                  LV_D, S_F, NO_LOGS,
                                                  TREE_WT, DBH, SP0, BA_TREE, PHF_TREE,
                                                  HEIGHT = TREE_LEN, BARK_PER,
                                                  HT_PROJ, DIAM_BTP, BROKEN_TOP_IND,
                                                  HT_BTOP,
                                                  MEASUREMENT_ANOMALY_CODE,
                                                  LOG_G_1,  LOG_G_2,  LOG_G_3,  LOG_G_4,
                                                  LOG_G_5,  LOG_G_6,  LOG_G_7, LOG_G_8,
                                                  LOG_G_9 = as.numeric(NA),
                                                  LOG_L_1,  LOG_L_2,  LOG_L_3, LOG_L_4,
                                                  LOG_L_5, LOG_L_6,  LOG_L_7,  LOG_L_8,
                                                  LOG_L_9 = as.numeric(NA),
                                                  LOG_S_1, LOG_S_2,  LOG_S_3,  LOG_S_4,
                                                  LOG_S_5, LOG_S_6,  LOG_S_7,  LOG_S_8,
                                                  LOG_S_9 = as.numeric(NA))]

    return(vi_c)
  }
}
