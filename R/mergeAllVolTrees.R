#' Merge all volume trees-VRI specific
#'
#'
#' @description Merge all the volume trees, including full trees (fully-measure trees in IPC),
#'              enhanced trees (fully-measured trees in auxi plots), H-enhanced trees (Height measured in
#'              auxi plots) and non-enhanced trees (only DBH measured in auxi plots).
#'              The function is part of \code{vol_ha_2017.sas} and modified dramatically in R compiler.
#'
#' @param treeMS data.table, Compiled full, enhanced and H-enhanced trees. This data should be listed in vi_c table.
#'                           This data is an output of \code{\link{DWBCompiler}}
#' @param treeAX data.table, Non-enhanced trees in anxilirary plots (vi_i). Supposedly, the table only contains
#'                           non-enhanced tree list. However, some enhanced and H-enhanced trees also been
#'                           stored in this dataset.
#'                           An output from \code{\link{VRIInit_auxTree}}.
#'
#'
#' @return A data table that contains all volume trees without duplicates. Equivalent to \code{tree_vb} table.
#'
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#'
#'
#' @export
#' @docType methods
#' @rdname mergeAllVolTrees
#'
#' @author Yong Luo
#'
setGeneric("mergeAllVolTrees",
           function(treeMS, treeAX) {
             standardGeneric("mergeAllVolTrees")
           })

#' @rdname mergeAllVolTrees
setMethod(
  "mergeAllVolTrees",
  signature = c(treeMS = "data.table",
                treeAX = "data.table"),
  definition = function(treeMS, treeAX){
    ## the below codes are modified from original sas codes to avoid reduntant tables and simplify
    ## the process
    ## standardize the names that used in this function, please note that some of outputs names
    ## are different from the sas compiler
    ## 1. the columns that will be summarized
    summaryCols <- c(paste("VOL_",
                           c("WSV", "NET", "MER", "NETM", "NTW2", "NTWB", "D", "DW", "DWB"),
                           sep = ""), "VAL_MER")

    ## select count trees that have cluster id in the treeMS
    treeMS <- treeMS[,c("CLSTR_ID", "PLOT", "TREE_NO", "MEAS_INTENSE", "SPECIES", "LV_D", "S_F",
                        "LOG_G_1", "HEIGHT", "HT_TOTAL", "BTOP", "H_MERCH", "SP0", "BA_TREE", "PHF_TREE",
                        "DBH", "TREE_WT", summaryCols), with = FALSE]
    ############on hold for this and confirm with Rene and Bob about the B samples
    # ## full measured tree data treeMS have three scenarios in terms of calculating volumes
    # ## 1. have both height and call grading net factoring information, for those observations,
    # ##    the volume compilation is done
    # ## 2. have height information and no net factoring information in non-YSM plots (mostly in VRI design),
    # ##    those obervations could be in auxi plots or in IPC plots.
    # ##    for these observations, total volume is calculated,
    # ##    however, the net volumes after call grading have not been compiled and need to be calculated using
    # ##    ratio method.
    # ##    those observations are labeled as * for LOG_G_1, and not in NFI, CMI, LIDAR and YSM plots
    # ## 3. have height information and call grading information is not available in YSM plots.
    # ##    do not need to process?? seems like this in SAS compiler
    # treeMS[LOG_G_1 == "*" &
    #          !(substr(TYPE_CD, 1, 1) %in% c("F", "M", "L", "Y")),
    #        ':='(VOL_CALC = "YES", CALL_GRADING = "NO")] ## scenario 2
    # treeMS[is.na(VOL_CALC), ':='(VOL_CALC = "YES", CALL_GRADING = "YES")] ## scenario 1 and 3
    treeMS[, unitreeid := paste(CLSTR_ID, "_", PLOT, "_", TREE_NO, sep = "")]
    treeAX[, unitreeid := paste(CLSTR_ID, "_", PLOT, "_", TREE_NO, sep = "")]
    ## remove the trees that are in treeMS from treeAX
    treeAX <- treeAX[!(unitreeid %in% treeMS$unitreeid),]
    treeAX[, MEAS_INTENSE := "NON-ENHANCED"]
    tree_vb <- rbindlist(list(treeMS[, unitreeid := NULL],
                              treeAX[, unitreeid := NULL]),
                         fill = TRUE)
    rm(treeAX, treeMS)
    tree_vb[is.na(S_F), S_F := "S"]
    return(tree_vb)
  })
