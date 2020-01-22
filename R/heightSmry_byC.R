#' Summarize mean and lorry's height by cluster-VRI specific
#'
#'
#' @description Summarizes mean and lorry's height by cluster for standing trees, standing + live trees, and standing
#'              + live + non-broken top trees.
#'              The function is improved version to calculate mean height in \code{vol_ha_2017.sas} by outputing lorey's
#'              height.
#'              For both fixed and variable area plots, the function computes mean height by using plot weight (\code{PLOT_WT})
#'              weighted height. For lorey's height computation, the function treats variable and fixed area plots differently.
#'              Specifically, the function uses the mean height as lorey's height for variable plots,
#'              while uses height that weighted both by plot weight (\code{PLOT_WT}) and basal area (\code{BA_TREE})
#'              for fixed area plots.
#'
#' @param treeMC data.table, Compiled tree-level data that contains both measured trees and counted trees.
#'
#'
#' @return A table contains computed mean height \code{MN or MEAN} and lorey's height \code{LRY} for all standing trees \code{ALL},
#'         standing and live trees \code{1} and standing and non-broken top trees \code{2}. The output is equevalent
#'         to \code{height} table in original compiler.
#'
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#' @importFrom FAIBBase merge_dupUpdate
#'
#'
#' @export
#' @docType methods
#' @rdname heightSmry_byC
#'
#' @author Yong Luo
#'
setGeneric("heightSmry_byC",
           function(treeMC) {
             standardGeneric("heightSmry_byC")
           })

#' @rdname heightSmry_byC
setMethod(
  "heightSmry_byC",
  signature = c(treeMC = "data.table"),
  definition = function(treeMC){
    worktable <- treeMC[,.(CLSTR_ID, HEIGHT, BA_TREE, PLOT_WT, S_F, LV_D, BTOP, SAMP_TYP)]
    rm(treeMC)
    if(nrow(worktable[!(SAMP_TYP %in% c("F", "V"))]) > 0){
      stop("SAMP_TYP must be either V or F in your dataset.")
    } else {

      if(length(unique(worktable[SAMP_TYP == "F"]$CLSTR_ID)) + length(unique(worktable[SAMP_TYP == "V"]$CLSTR_ID)) !=
         length(unique(worktable$CLSTR_ID))) {
        stop("In your dataset, some clusters have multiple sample type.")
      }
    }
    ## mean height calculation, for mean height, both fixed and variable plots follow the same
    ## calculation routine
    worktable[, heightweighted := HEIGHT * PLOT_WT]
    ## FOR ALL STANDING TREES
    stand <- worktable[HEIGHT %>>% 0 & S_F == "S",
                       .(WTHA = sum(PLOT_WT, na.rm = TRUE),
                         HT_MNALL = sum(heightweighted, na.rm = TRUE),
                         SAMP_TYP = unique(SAMP_TYP)),
                       by = "CLSTR_ID"]
    stand[, HT_MNALL := HT_MNALL/WTHA]
    stand[, WTHA := NULL]

    ## for standing and live trees
    standlive <- worktable[HEIGHT %>>% 0 & S_F == "S" & LV_D == "L",
                           .(WTH1 = sum(PLOT_WT),
                             HT_MEAN1 = sum(heightweighted)),
                           by = "CLSTR_ID"]
    standlive[, HT_MEAN1 := HT_MEAN1/WTH1]
    standlive[, WTH1 := NULL]

    standliveNONBTOP <- worktable[HEIGHT %>>% 0 & S_F == "S" & LV_D == "L" & is.na(BTOP),
                                  .(WTH2 = sum(PLOT_WT),
                                    HT_MEAN2 = sum(heightweighted)),
                                  by = "CLSTR_ID"]
    standliveNONBTOP[, HT_MEAN2 := HT_MEAN2/WTH2]
    standliveNONBTOP[, WTH2 := NULL]

    meanHeightOutput <- merge(stand, standlive, by = "CLSTR_ID", all.x = TRUE)
    meanHeightOutput <- merge(meanHeightOutput, standliveNONBTOP, by = "CLSTR_ID", all.x = TRUE)
    rm(stand, standlive, standliveNONBTOP)


    ## treat fixed area plot and variable plot differently to calculate lorey's height
    vplotoutput <- meanHeightOutput[SAMP_TYP == "V",]
    vplotoutput[, ':='(HT_LRYALL = HT_MNALL,
                       HT_LRY1 = HT_MEAN1,
                       HT_LRY2 = HT_MEAN2)]
    fplotoutput <- meanHeightOutput[SAMP_TYP == "F",]
    rm(meanHeightOutput)



    worktable <- worktable[SAMP_TYP == "F"]
    worktable[, ':='(plotbaweight = PLOT_WT*BA_TREE,
                     heightweighted = HEIGHT*PLOT_WT*BA_TREE)]

    stand <- worktable[HEIGHT %>>% 0 & S_F == "S",
                       .(WTHA = sum(plotbaweight, na.rm = TRUE),
                         HT_LRYALL = sum(heightweighted, na.rm = TRUE)),
                       by = "CLSTR_ID"]
    stand[, HT_LRYALL := HT_LRYALL/WTHA]
    stand[, WTHA := NULL]

    ## for standing and live trees
    standlive <- worktable[HEIGHT %>>% 0 & S_F == "S" & LV_D == "L",
                           .(WTH1 = sum(plotbaweight, na.rm = TRUE),
                             HT_LRY1 = sum(heightweighted, na.rm = TRUE)),
                           by = "CLSTR_ID"]
    standlive[, HT_LRY1 := HT_LRY1/WTH1]
    standlive[, WTH1 := NULL]

    standliveNONBTOP <- worktable[HEIGHT %>>% 0 & S_F == "S" & LV_D == "L" & is.na(BTOP),
                                  .(WTH2 = sum(plotbaweight, na.rm = TRUE),
                                    HT_LRY2 = sum(heightweighted, na.rm = TRUE)),
                                  by = "CLSTR_ID"]
    standliveNONBTOP[, HT_LRY2 := HT_LRY2/WTH2]
    standliveNONBTOP[, WTH2 := NULL]

    fplotoutput <- FAIBBase::merge_dupUpdate(fplotoutput, stand, by = "CLSTR_ID", all.x = TRUE)
    fplotoutput <- FAIBBase::merge_dupUpdate(fplotoutput, standlive, by = "CLSTR_ID", all.x = TRUE)
    fplotoutput <- FAIBBase::merge_dupUpdate(fplotoutput, standliveNONBTOP, by = "CLSTR_ID", all.x = TRUE)
    rm(stand, standlive, standliveNONBTOP)
    output <- rbindlist(list(vplotoutput, fplotoutput))
    decimal2cols <- c("HT_LRYALL",  "HT_MNALL",
                      "HT_LRY1",  "HT_MEAN1",
                      "HT_LRY2", "HT_MEAN2")
    output[, c(decimal2cols) := lapply(.SD, function(s) round(s, 2)), .SDcols = decimal2cols]
    return(output)
  })

