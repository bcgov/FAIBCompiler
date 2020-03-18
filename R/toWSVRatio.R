#' To toWSV ratio for each of strata
#'
#' @description This function takes the selected data for derive ratio for each of strata by
#'              BEC+SP0+LV_D
#'
#' @param inputData data.table, The data for deriving ratios, that must contain full and enhanced
#'                  trees.
#' @param needCombs data.table, The combinations of BEC+SP0+LV_D are needed to derive coefficients.
#'
#' @param minDBH numeric, The minimum DBH for selecting trees to derive ratios. If missing,
#'               10 cm will be used.
#'
#' @param minObs numeric, The minimum samples size for a stratum. If missing,
#'                        30 observations will be used.
#'
#' @return ratio table
#'
#'
#' @importFrom data.table ':=' rbindlist
#'
#'
#' @export
#' @docType methods
#' @rdname toWSVRatio
#'
#' @author Yong Luo
toWSVRatio <- function(inputData, needCombs, minDBH = 10, minObs = 30){
  volVariables <- c(paste("VOL_",c("WSV", "NET", "MER", "NETM", "NTW2",
                                   "NTWB", "D", "DW", "DWB"),
                          sep = ""), "VAL_MER")

  ratioVariables <- paste("RATIO_",c("WSV", "NET", "MER", "NETM", "NTW2",
                                     "NTWB", "D", "DW", "DWB", "VAL"),
                          sep = "")

  all_trees_ratio <- inputData[MEAS_INTENSE %in% c("FULL", "ENHANCED") &
                                 DBH >= minDBH & VOL_NTWB > 0,]

  all_trees_ratio[, SAMP_POINT := substr(CLSTR_ID, 1, 9)]

  specieslookup <- lookup_species()
  specieslookup <- unique(specieslookup[,.(SP0, SP_TYPE)], by = "SP0")
  all_trees_ratio <- merge(all_trees_ratio, specieslookup, by = "SP0", all.x = TRUE)

  all_trees_ratio <- all_trees_ratio[, c("SAMP_POINT", "CLSTR_ID", "BGC_ZONE", "SP0",
                                         "SP_TYPE", "LV_D", volVariables), with = FALSE]

  all_trees_ratio[VOL_NET < 0, VOL_NET := 0]

  needCombs <- merge(needCombs[,.(BGC_ZONE, SP0, LV_D)], specieslookup, by = "SP0", all.x = TRUE)
  allstrata <- needCombs[SP0 != "X",]
  strata_missspecies <- needCombs[SP0 == "X",]


  ## the first attempt by
  ratiotable <- all_trees_ratio[,c(.N, lapply(.SD, mean, na.rm = TRUE)),
                                .SDcols = volVariables,
                                by = c("BGC_ZONE",  "LV_D",  "SP0")]

  ratiotable[, c(ratioVariables) := lapply(.SD, function(s){s/VOL_WSV}),
             .SDcols = volVariables]

  ratiotable <- ratiotable[N >= minObs,c("BGC_ZONE",  "LV_D",  "SP0", "N",
                                         ratioVariables), with = FALSE]

  allstrata <- merge(allstrata, ratiotable,
                     by = c("BGC_ZONE",  "LV_D",  "SP0"),
                     all.x = TRUE)
  allsuccessed <- allstrata[!is.na(N)]
  allsuccessed[, DATA_SRCE := "BEC+SP0+LV_D"]

  strata_failed <- allstrata[is.na(N),.(BGC_ZONE, SP0, LV_D, SP_TYPE)]
  rm(ratiotable, allstrata)

  if(nrow(strata_failed) > 0){ # initiate second attempt
    # The second attempt is to group bgc zone into coastal and interior
    strata_failed[BGC_ZONE %in% c("CMA", "MH", "CDF", "CWH"), BGC_REG := "COAST"]
    strata_failed[is.na(BGC_REG), BGC_REG := "INTERIOR"]

    all_trees_ratio[BGC_ZONE %in% c("CMA", "MH", "CDF", "CWH"), BGC_REG := "COAST"]
    all_trees_ratio[is.na(BGC_REG), BGC_REG := "INTERIOR"]

    ratiotable <- all_trees_ratio[,c(.N, lapply(.SD, mean, na.rm = TRUE)),
                                  .SDcols = volVariables,
                                  by = c("BGC_REG", "SP0", "LV_D")]

    ratiotable[, c(ratioVariables) := lapply(.SD, function(s){s/VOL_WSV}),
               .SDcols = volVariables]

    ratiotable <- ratiotable[N >= minObs,c("BGC_REG",  "LV_D",  "SP0", "N",
                                           ratioVariables), with = FALSE]

    strata_failed <- merge(strata_failed, ratiotable,
                           by = c("BGC_REG",  "LV_D",  "SP0"),
                           all.x = TRUE)
    successed_add <- strata_failed[!is.na(N)]
    successed_add[, DATA_SRCE := "BEC_GRP+SP0+LV_D"]
    allsuccessed <- rbindlist(list(allsuccessed, successed_add),
                              fill = TRUE)
    strata_failed <- strata_failed[is.na(N),.(BGC_ZONE, SP0, LV_D, SP_TYPE)]
    rm(ratiotable, successed_add)

    if(nrow(strata_failed) > 0){ ## initiate third attempt
      # The third attempt is to combine all bec zones regardless of coastal and interior
      ratiotable <- all_trees_ratio[,c(.N, lapply(.SD, mean, na.rm = TRUE)),
                                    .SDcols = volVariables,
                                    by = c("SP0", "LV_D")]

      ratiotable[, c(ratioVariables) := lapply(.SD, function(s){s/VOL_WSV}),
                 .SDcols = volVariables]

      ratiotable <- ratiotable[N >= minObs,c("SP0", "LV_D",  "N",
                                             ratioVariables), with = FALSE]

      strata_failed <- merge(strata_failed, ratiotable,
                             by = c("LV_D",  "SP0"),
                             all.x = TRUE)
      successed_add <- strata_failed[!is.na(N)]
      successed_add[, DATA_SRCE := "SP0+LV_D"]
      allsuccessed <- rbindlist(list(allsuccessed, successed_add),
                                fill = TRUE)
      strata_failed <- strata_failed[is.na(N),.(BGC_ZONE, SP0, LV_D, SP_TYPE)]
      rm(ratiotable, successed_add)

      if(nrow(strata_failed) > 0){ # initiate the last attempt
        # The forth attempt is to regardless bec and lv_d
        ratiotable <- all_trees_ratio[,c(.N, lapply(.SD, mean, na.rm = TRUE)),
                                      .SDcols = volVariables,
                                      by = c("SP_TYPE", "LV_D")]

        ratiotable[, c(ratioVariables) := lapply(.SD, function(s){s/VOL_WSV}),
                   .SDcols = volVariables]

        ratiotable <- ratiotable[N >= minObs,c("SP_TYPE", "LV_D", "N",
                                               ratioVariables), with = FALSE]

        strata_failed <- merge(strata_failed, ratiotable,
                               by = c("SP_TYPE", "LV_D"),
                               all.x = TRUE)
        successed_add <- strata_failed[!is.na(N)]
        successed_add[, DATA_SRCE := "SP_TYPE+LV_D"]
        allsuccessed <- rbindlist(list(allsuccessed, successed_add),
                                  fill = TRUE)
        allstrata_failed <- strata_failed[is.na(N),.(BGC_ZONE, SP0, LV_D, SP_TYPE)]
        rm(ratiotable, successed_add)
      } # end of forth attempt
    } # end of third attempt
  } # end of second attempt

  if(nrow(strata_missspecies) > 0){
    ratiotable <- all_trees_ratio[,c(.N, lapply(.SD, mean, na.rm = TRUE)),
                                  .SDcols = volVariables,
                                  by = c("BGC_ZONE", "LV_D")]

    ratiotable[, c(ratioVariables) := lapply(.SD, function(s){s/VOL_WSV}),
               .SDcols = volVariables]

    ratiotable <- ratiotable[N >= minObs,c("BGC_ZONE", "LV_D", "N",
                                           ratioVariables), with = FALSE]

    strata_missspecies <- merge(strata_missspecies, ratiotable,
                                by = c("BGC_ZONE", "LV_D"),
                                all.x = TRUE)
    successed_add <- strata_missspecies[!is.na(N)]
    successed_add[, DATA_SRCE := "BEC+LV_D"]
    allsuccessed <- rbindlist(list(allsuccessed, successed_add),
                              fill = TRUE)
    strata_failed_last2 <- strata_missspecies[is.na(N),.(BGC_ZONE, SP0, LV_D, SP_TYPE)]
    if(exists("allstrata_failed")){
      allstrata_failed <- rbind(allstrata_failed, strata_failed_last2)
    } else {
      allstrata_failed <- strata_failed_last2
    }
    rm(ratiotable, successed_add)
  }

  if(exists("allstrata_failed")){
    if(nrow(allstrata_failed) > 0){
      allstrata_failed[, texts := paste0(BGC_ZONE, " + ", SP0, " + ", LV_D, "\n")]
      warning("The toWSV ratios can not be derived for the strata below: \n", allstrata_failed$texts)
    }
  }

  return(allsuccessed)
}
