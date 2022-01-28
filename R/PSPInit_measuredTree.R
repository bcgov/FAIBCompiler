#' Load and select fully measured tree data for PSP compilation
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
#' @rdname PSPInit_measuredTree
#'
#' @author Yong Luo
PSPInit_measuredTree<- function(clusterplotHeader,
                                dataSourcePath){
  vi_c <- readRDS(file.path(dataSourcePath, "vi_c.rds")) %>% data.table
  names(vi_c) <- toupper(names(vi_c))

  vi_c[, clusterPlot := paste(CLSTR_ID, PLOT, sep = "_")]
  clusterplotHeader[, clusterPlot := paste(CLSTR_ID, PLOT, sep = "_")]

  vi_c <- vi_c[clusterPlot %in% unique(clusterplotHeader$clusterPlot), ]

  vi_c[DBH != 0, BA_TREE := pi * ((DBH/200)^2)]

  # remove get_vars function as SP0 remains same as that in vi_pc table
  vi_c[DBH == 0, ':='(SP0 = NA, BA_TREE = NA)]
  vi_c[, TREE_WT := 1]
  vi_c <- FAIBBase::merge_dupUpdate(vi_c,
                                    clusterplotHeader[,.(clusterPlot, SAMP_TYP,
                                                         PLOT_WT, BLOWUP,
                                                         BGC_ZONE,	BGC_SBZN)],
                                    by = "clusterPlot", all.x = TRUE)
  # correction of species
  vi_c[, SPECIES_ORG := SPECIES]
  vi_c[, SPECIES := speciesCorrection(SPECIES,
                                      BGC_ZONE,
                                      BGC_SBZN)]

  vi_c[, PHF_TREE := FAIBBase::PHFCalculator(sampleType = SAMP_TYP, blowUp = BLOWUP,
                                             treeWeight = TREE_WT, plotWeight = 1,
                                             treeBasalArea = BA_TREE)]


  # for NFI (F), CMI and YSMI, the plots use a 100 m2 subplot for
  # trees with a dbh < 9, therefore should be extrapolate to 400 m2 (size of large tree plot)
  # need attention here for PSP
  # vi_c[substr(CLSTR_ID, 9, 9) %in% c("F", "M", "Y", "L") & DBH < 9,
  #      PHF_TREE := PHF_TREE*4]
  vi_c[BROKEN_TOP_IND == "Y" &
         !is.na(TREE_LEN),
       HT_BTOP := TREE_LEN] ## as long as TREE_LEN is available, the break height is TREE_LEN

  vi_c[BROKEN_TOP_IND == "Y" &
         is.na(TREE_LEN) &
         !is.na(HEIGHT_TO_BREAK),
       HT_BTOP := HEIGHT_TO_BREAK] ## otherwise, the height_to_break is used as break height

  vi_c <- vi_c[order(CLSTR_ID, PLOT, TREE_NO),
               .(CLSTR_ID, PLOT, TREE_NO,
                 SPECIES, SPECIES_ORG,
                 LV_D, S_F,
                 TREE_WT, DBH, SP0, BA_TREE, PHF_TREE,
                 HEIGHT = TREE_LEN, BARK_PER,
                 HT_PROJ, DIAM_BTP, BROKEN_TOP_IND,
                 HT_BTOP,
                 MEASUREMENT_ANOMALY_CODE)]
  return(vi_c)
}
