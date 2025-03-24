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
  vi_c[DBH == 0, ':='(BA_TREE = 0)]
  vi_c[, ':='(TREE_WT = 1,
              TREE_WT_WK = 1)]
  if(compilationType == "nonPSP"){
    if(walkThru){
      vi_c[toupper(WALKTHRU_STATUS) == "O", TREE_WT_WK := 0] # tree is out and is not used
      vi_c[toupper(WALKTHRU_STATUS) == "W", TREE_WT_WK := 2] # tree is
    }
  }
  vi_c <- FAIBBase::merge_dupUpdate(vi_c,
                                    unique(clusterplotHeader[,.(clusterPlot, SAMP_TYP,
                                                                BEC_ZONE, BEC_SBZ, BEC_VAR,
                                                                PLOT_WT, BLOWUP_MAIN, BLOWUP_SUBPLOT,
                                                                FIZ, TYPE_CD,
                                                                SAMPLE_ESTABLISHMENT_TYPE,
                                                                DBH_LIMIT_TAG, SAMPLE_BREAK_POINT)],
                                           by = "clusterPlot"),
                                    by = "clusterPlot", all.x = TRUE)

  if(compilationType == "nonPSP"){
    vi_c <- vi_c[DBH >= 4, ] # for nonPSP, only dbh bigger than or equal to 4 cm is valid
    vi_c[, PHF_TREE := FAIBBase::PHFCalculator(sampleType = SAMP_TYP, blowUp = BLOWUP_MAIN,
                                               treeWeight = TREE_WT, plotWeight = PLOT_WT,
                                               treeBasalArea = BA_TREE)]
    vi_c[, PHF_TREE_WK := FAIBBase::PHFCalculator(sampleType = SAMP_TYP, blowUp = BLOWUP_MAIN,
                                               treeWeight = TREE_WT_WK, plotWeight = PLOT_WT,
                                               treeBasalArea = BA_TREE)]
  # for NFI (F), CMI and YSMI, the plots use a 100 m2 subplot for
  # trees with a dbh < 9, therefore should be extrapolate to 400 m2 (size of large tree plot)
  vi_c[TYPE_CD %in% c("F", "M", "Y", "L") & DBH < 9,
       ':='(PHF_TREE = PHF_TREE*4,
            PHF_TREE_WK = PHF_TREE_WK*4)]
  #   2)	FHYSM Only - Can we change the PHF_TREE from 25 to 100 for the 4-9cm DBH deciduous trees in the 5.64m subplot?
  #   •	The reason why is because there are non-standard diameter limits used in this project. All conifers >1m height in the 11.28m radius main plot are tagged (all conifers get a PHF_TREE = 25) while deciduous are tagged using standard YSM/CMI protocol.  ie., trees 4-9cm dbh in the 5.64m radius subplot (PHF_TREE = 100), and trees >9cm dbh in the 11.28m radius main plot (PHF_TREE = 25).
  #   •	It looks like it only impacts one tree so far but these FHYSM samples will be remeasured and we may get more.
  deci_sp <- lookup_species()
  deci_sp <- unique(deci_sp[SP_TYPE == "D"]$SPECIES)
  vi_c[SAMPLE_ESTABLISHMENT_TYPE == "FHYSM" & DBH < 9 & SPECIES %in% deci_sp,
       PHF_TREE := PHF_TREE*4]
  } else {
    vi_c[DBH >= SAMPLE_BREAK_POINT,
         PHF_TREE := FAIBBase::PHFCalculator(sampleType = SAMP_TYP, blowUp = BLOWUP_MAIN,
                                               treeWeight = TREE_WT, plotWeight = 1,
                                               treeBasalArea = BA_TREE)]
    vi_c[DBH < SAMPLE_BREAK_POINT &
           is.na(PHF_TREE) &
           !is.na(BLOWUP_SUBPLOT),
         PHF_TREE := FAIBBase::PHFCalculator(sampleType = SAMP_TYP, blowUp = BLOWUP_SUBPLOT,
                                               treeWeight = TREE_WT, plotWeight = 1,
                                               treeBasalArea = BA_TREE)]
  }

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
                                                  SPECIES,
                                                  LV_D, S_F, NO_LOGS = 1,
                                                  TREE_WT, TREE_WT_WK = as.numeric(NA),
                                                  DBH, SP0, BA_TREE,
                                                  PHF_TREE, PHF_TREE_WK = as.numeric(NA),
                                                  HEIGHT = TREE_LEN, BARK_PER,
                                                  HT_PROJ, DIAM_BTP, BROKEN_TOP_IND,
                                                  HT_BTOP,
                                                  MEASUREMENT_ANOMALY_CODE,
                                                  TREE_PLANTED_IND,
                                                  TREE_CLASS_CODE,
                                                  TAGGING_SECTOR_NO,
                                                  SITE_SECTOR_NO,
                                                  RESIDUAL)]
    return(vi_c)
  } else {
    vi_c <- vi_c[order(CLSTR_ID, PLOT, TREE_NO),.(CLSTR_ID, PLOT,
                                                  BEC_ZONE, BEC_SBZ, BEC_VAR, FIZ,
                                                  TYPE_CD, TREE_NO,
                                                  SPECIES,
                                                  LV_D, S_F, NO_LOGS,
                                                  TREE_WT, TREE_WT_WK, DBH, SP0, BA_TREE,
                                                  PHF_TREE, PHF_TREE_WK,
                                                  HEIGHT = TREE_LEN, BARK_PER,
                                                  HT_PROJ, DIAM_BTP, BROKEN_TOP_IND,
                                                  HT_BTOP,
                                                  MEASUREMENT_ANOMALY_CODE,
                                                  TREE_PLANTED_IND,
                                                  TREE_CLASS_CODE,
                                                  TAGGING_SECTOR_NO,
                                                  SITE_SECTOR_NO,
                                                  RESIDUAL,
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
