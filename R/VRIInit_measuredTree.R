#' Load and select fully measured tree data-VRI specific
#'
#' @description This function selects the tree-level data from vi_c (cardc) based on selected cluster/plot headers.
#'              Additonally, the function calculates basal area and tree per ha factor.
#'
#'
#' @param clusterplotHeader data.table, Cluster and plot-level attributes, an output from \code{\link{VRIInit_clusterplot}}.
#'
#'
#' @param dataSourcePath character, Specifies the path that directs to the VRI original data soruce, i.e.,
#'                                  \code{//Mayhem/GIS_TIB/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/vri_sa}.
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
#' @rdname VRIInit_measuredTree
#'
#' @author Yong Luo
#'
#'
VRIInit_measuredTree<- function(clusterplotHeader,
                        dataSourcePath,
                        walkThru = TRUE){
    vi_c <- readRDS(file.path(dataSourcePath, "vi_c.rds")) %>% data.table
    names(vi_c) <- toupper(names(vi_c))

    vi_c[, clusterPlot := paste(CLSTR_ID, PLOT, sep = "_")]
    clusterplotHeader[, clusterPlot := paste(CLSTR_ID, PLOT, sep = "_")]

    vi_c <- vi_c[clusterPlot %in% unique(clusterplotHeader$clusterPlot), ]

    vi_c[DBH != 0, BA_TREE := pi * ((DBH/200)^2)]

    # remove get_vars function as SP0 remains same as that in vi_pc table
    vi_c[DBH == 0, ':='(SP0 = NA, BA_TREE = NA)]
    vi_c[, TREE_WT := 1]
    if(walkThru){
      vi_c[toupper(WALKTHRU_STATUS) == "O", TREE_WT := 0] # tree is out and is not used
      vi_c[toupper(WALKTHRU_STATUS) == "W", TREE_WT := 2] # tree is
    }
    vi_c <- FAIBBase::merge_dupUpdate(vi_c,
                                      unique(clusterplotHeader[,.(clusterPlot, SAMP_TYP,
                                                           PLOT_WT, BLOWUP,
                                                           BEC_ZONE, BEC_SBZ, BEC_VAR,
                                                           TYPE_CD)],
                                             by = "clusterPlot"),
                            by = "clusterPlot", all.x = TRUE)
    # correction of species
    vi_c[, SPECIES_ORG := SPECIES]
    vi_c[, SPECIES := speciesCorrection(SPECIES,
                                        BEC_ZONE,
                                        BEC_SBZ)]

    vi_c[, PHF_TREE := FAIBBase::PHFCalculator(sampleType = SAMP_TYP, blowUp = BLOWUP,
                                     treeWeight = TREE_WT, plotWeight = PLOT_WT,
                                     treeBasalArea = BA_TREE)]


    # for NFI (F), CMI and YSMI, the plots use a 100 m2 subplot for
    # trees with a dbh < 9, therefore should be extrapolate to 400 m2 (size of large tree plot)
    vi_c[TYPE_CD %in% c("F", "M", "Y", "L") & DBH < 9,
         PHF_TREE := PHF_TREE*4]
    vi_c <- vi_c[order(CLSTR_ID, PLOT, TREE_NO),.(CLSTR_ID, PLOT,
                                                  BEC_ZONE, BEC_SBZ, BEC_VAR,
                                                  TYPE_CD, TREE_NO,
                                                  SPECIES, SPECIES_ORG,
                                                  LV_D, S_F, NO_LOGS,
                                                  TREE_WT, DBH, SP0, BA_TREE, PHF_TREE,
                                                  HEIGHT = TREE_LEN, BARK_PER,
                                                  HT_PROJ, DIAM_BTP, BROKEN_TOP_IND,
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
