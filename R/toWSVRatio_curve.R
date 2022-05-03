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
#' @importFrom data.table ':=' rbindlist
#'
#'
#' @export
#' @docType methods
#' @rdname toWSVRatio_curve
#'
#' @author Yong Luo
toWSVRatio_curve <- function(inputData, needCombs, minDBH = 10, minObs = 30){
  volVariables <- c(paste("VOL_",c("WSV", "NET", "MER", "NETM", "NTW2",
                                   "NTWB", "D", "DW", "DWB"),
                          sep = ""), "VAL_MER")

  ratioVariables <- paste("RATIO_",c("WSV", "NET", "MER", "NETM", "NTW2",
                                     "NTWB", "D", "DW", "DWB", "VAL"),
                          sep = "")

  all_trees_ratio <- inputData[MEAS_INTENSE %in% c("FULL", "ENHANCED", "H-ENHANCED") &
                                 DBH >= minDBH & VOL_NTWB > 0,]


  specieslookup <- lookup_species()
  specieslookup <- unique(specieslookup[,.(SP0, SP_TYPE)], by = "SP0")
  all_trees_ratio <- merge(all_trees_ratio, specieslookup, by = "SP0", all.x = TRUE)

  all_trees_ratio <- all_trees_ratio[, c("CLSTR_ID", "BEC_ZONE", "SP0", "DBH",
                                         "SP_TYPE", "LV_D", volVariables), with = FALSE]

  all_trees_ratio[VOL_NET < 0, VOL_NET := 0]

  needCombs <- merge(needCombs[,.(BEC_ZONE, SP0, LV_D)], specieslookup, by = "SP0", all.x = TRUE)
  allstrata <- needCombs[SP0 != "X",]
  strata_missspecies <- needCombs[SP0 == "X",]

  ## the first attempt by

  strata_failed_mer <- needCombs[0,]
  strata_failed_ntwb <- needCombs[0,]


  all_trees_ratio[, c(ratioVariables) := lapply(.SD, function(s){s/VOL_WSV}),
                  .SDcols = volVariables]
  all_trees_ratio[, DBH_MOD := DBH-10]
  allcoeffs_mer <- data.table(BEC_ZONE = character(),
                              SP0 = character(),
                              LV_D = character(),
                              a = numeric(),
                              b = numeric(),
                              c = numeric(),
                              N_OBS = numeric(),
                              DATA_SOURCE = character())
  allcoeffs_ntwb <- data.table(BEC_ZONE = character(),
                               SP0 = character(),
                               LV_D = character(),
                               a = numeric(),
                               b = numeric(),
                               c = numeric(),
                               N_OBS = numeric(),
                               DATA_SOURCE = character())
  for(i in 1:nrow(needCombs)){
    indistrata_data <- all_trees_ratio[BEC_ZONE == needCombs$BEC_ZONE[i] &
                                         SP0 == needCombs$SP0[i] &
                                         LV_D == needCombs$LV_D[i],]
    if(nrow(indistrata_data) > minObs){
      fit_mer <- try(nlsout_mer <- nls(RATIO_MER ~ a * (1 - exp(-b * DBH_MOD))^c,
                                       data = indistrata_data[RATIO_MER != 0, ],
                                       start = list(a = 0.9, b = 0.5, c = 1.5),
                                       trace = FALSE),
                     TRUE)



      fit_ntwb <- try(nlsout_ntwb <- nls(RATIO_NTWB ~ a * (1 - exp(-b * DBH_MOD))^c,
                                         data = indistrata_data[RATIO_NTWB != 0, ],
                                         start = list(a = 0.9, b = 0.5, c = 1.5),
                                         trace = FALSE),
                      TRUE)


      ## for mer
      if(class(fit_mer) == "try-error"){
        strata_failed_mer <- rbind(strata_failed_mer, needCombs[i])
      } else {
        indistrata_mer_coef <- data.table(rbind(coef(nlsout_mer)))
        indistrata_mer_coef[, ':='(BEC_ZONE = needCombs$BEC_ZONE[i],
                                   SP0 = needCombs$SP0[i],
                                   LV_D = needCombs$LV_D[i],
                                   N_OBS = nrow(indistrata_data))]
        indistrata_mer_coef[, DATA_SOURCE := paste0(BEC_ZONE, " + ", SP0, " + ", LV_D)]
        allcoeffs_mer <- rbind(allcoeffs_mer, indistrata_mer_coef)
        rm(nlsout_mer, indistrata_mer_coef)
      }

      if(class(fit_ntwb) == "try-error"){
        strata_failed_ntwb <- rbind(strata_failed_ntwb, needCombs[i])
      } else {
        indistrata_ntwb_coef <- data.table(rbind(coef(nlsout_ntwb)))
        indistrata_ntwb_coef[, ':='(BEC_ZONE = needCombs$BEC_ZONE[i],
                                    SP0 = needCombs$SP0[i],
                                    LV_D = needCombs$LV_D[i],
                                    N_OBS = nrow(indistrata_data))]
        indistrata_ntwb_coef[, DATA_SOURCE := paste0(BEC_ZONE, " + ", SP0, " + ", LV_D)]
        allcoeffs_ntwb <- rbind(allcoeffs_ntwb, indistrata_ntwb_coef)
        rm(nlsout_ntwb, indistrata_ntwb_coef)
      }
      rm(fit_mer, fit_ntwb)
    } else {
      strata_failed_mer <- rbind(strata_failed_mer, needCombs[i])
      strata_failed_ntwb <- rbind(strata_failed_ntwb, needCombs[i])
    }
    rm(indistrata_data)
  }

  strata_failed <- merge(strata_failed_mer[, MER_FAILED := TRUE],
                         strata_failed_ntwb[, NTWB_FAILED := TRUE],
                         by = c("SP0", "BEC_ZONE", "LV_D", "SP_TYPE"),
                         all = TRUE)
  allcoeffs_mer_suc <- allcoeffs_mer
  allcoeffs_ntwb_suc <- allcoeffs_ntwb
  rm(i, strata_failed_mer, strata_failed_ntwb, allcoeffs_mer, allcoeffs_ntwb)


  if(nrow(strata_failed) > 0){ # initiate second attempt
    # The second attempt is to group bgc zone into coastal and interior
    strata_failed[BEC_ZONE %in% c("CMA", "MH", "CDF", "CWH"), BEC_REG := "COAST"]
    strata_failed[is.na(BEC_REG), BEC_REG := "INTERIOR"]

    all_trees_ratio[BEC_ZONE %in% c("CMA", "MH", "CDF", "CWH"), BEC_REG := "COAST"]
    all_trees_ratio[is.na(BEC_REG), BEC_REG := "INTERIOR"]

    needCombs <- unique(strata_failed[,.(BEC_REG, SP0, LV_D)])

    strata_failed_mer <- needCombs[0,]
    strata_failed_ntwb <- needCombs[0,]

    allcoeffs_mer <- data.table(BEC_REG = character(),
                                SP0 = character(),
                                LV_D = character(),
                                a = numeric(),
                                b = numeric(),
                                c = numeric(),
                                N_OBS = numeric(),
                                DATA_SOURCE = character())
    allcoeffs_ntwb <- data.table(BEC_REG = character(),
                                 SP0 = character(),
                                 LV_D = character(),
                                 a = numeric(),
                                 b = numeric(),
                                 c = numeric(),
                                 N_OBS = numeric(),
                                 DATA_SOURCE = character())
    for(i in 1:nrow(needCombs)){
      indistrata_data <- all_trees_ratio[BEC_REG == needCombs$BEC_REG[i] &
                                           SP0 == needCombs$SP0[i] &
                                           LV_D == needCombs$LV_D[i],]
      if(nrow(indistrata_data) > minObs){
        fit_mer <- try(nlsout_mer <- nls(RATIO_MER ~ a * (1 - exp(-b * DBH_MOD))^c,
                                         data = indistrata_data[RATIO_MER != 0, ],
                                         start = list(a = 0.9, b = 0.5, c = 1.5),
                                         trace = FALSE),
                       TRUE)



        fit_ntwb <- try(nlsout_ntwb <- nls(RATIO_NTWB ~ a * (1 - exp(-b * DBH_MOD))^c,
                                           data = indistrata_data[RATIO_NTWB != 0, ],
                                           start = list(a = 0.9, b = 0.5, c = 1.5),
                                           trace = FALSE),
                        TRUE)


        ## for mer
        if(class(fit_mer) == "try-error"){
          strata_failed_mer <- rbind(strata_failed_mer, needCombs[i])
        } else {
          indistrata_mer_coef <- data.table(rbind(coef(nlsout_mer)))
          indistrata_mer_coef[, ':='(BEC_REG = needCombs$BEC_REG[i],
                                     SP0 = needCombs$SP0[i],
                                     LV_D = needCombs$LV_D[i],
                                     N_OBS = nrow(indistrata_data))]
          indistrata_mer_coef[, DATA_SOURCE := paste0(BEC_REG, " + ", SP0, " + ", LV_D)]
          allcoeffs_mer <- rbind(allcoeffs_mer, indistrata_mer_coef)

          rm(nlsout_mer, indistrata_mer_coef)
        }

        if(class(fit_ntwb) == "try-error"){
          strata_failed_ntwb <- rbind(strata_failed_ntwb, needCombs[i])
        } else {
          indistrata_ntwb_coef <- data.table(rbind(coef(nlsout_ntwb)))
          indistrata_ntwb_coef[, ':='(BEC_REG = needCombs$BEC_REG[i],
                                      SP0 = needCombs$SP0[i],
                                      LV_D = needCombs$LV_D[i],
                                      N_OBS = nrow(indistrata_data))]
          indistrata_ntwb_coef[, DATA_SOURCE := paste0(BEC_REG, " + ", SP0, " + ", LV_D)]
          allcoeffs_ntwb <- rbind(allcoeffs_ntwb, indistrata_ntwb_coef)
          rm(nlsout_ntwb, indistrata_ntwb_coef)
        }
        rm(fit_mer, fit_ntwb)
      } else {
        strata_failed_mer <- rbind(strata_failed_mer, needCombs[i])
        strata_failed_ntwb <- rbind(strata_failed_ntwb, needCombs[i])
      }
      rm(indistrata_data)
    }

    strata_mer <- merge(strata_failed[MER_FAILED == TRUE, .(BEC_REG, BEC_ZONE, SP0, LV_D, SP_TYPE)],
                        allcoeffs_mer,
                        by = c("BEC_REG", "SP0", "LV_D"),
                        all.x = TRUE)
    strata_ntwb <- merge(strata_failed[NTWB_FAILED == TRUE, .(BEC_REG, BEC_ZONE, SP0, LV_D, SP_TYPE)],
                         allcoeffs_ntwb,
                         by = c("BEC_REG", "SP0", "LV_D"),
                         all.x = TRUE)
    strata_mer_suc <- strata_mer[!is.na(N_OBS),]
    strata_failed_mer <- strata_mer[is.na(N_OBS),.(BEC_ZONE, SP0, LV_D, SP_TYPE, MER_FAILED = TRUE)]
    strata_ntwb_suc <- strata_ntwb[!is.na(N_OBS)]
    strata_failed_ntwb <- strata_ntwb[is.na(N_OBS),
                                      .(BEC_ZONE, SP0, LV_D, SP_TYPE, NTWB_FAILED = TRUE)]
    allcoeffs_mer_suc <- rbindlist(list(allcoeffs_mer_suc, strata_mer_suc), fill = TRUE)
    allcoeffs_ntwb_suc <- rbindlist(list(allcoeffs_ntwb_suc, strata_ntwb_suc), fill = TRUE)
    strata_failed <- merge(strata_failed_mer,
                           strata_failed_ntwb,
                           by = c("BEC_ZONE", "SP0", "LV_D", "SP_TYPE"),
                           all = TRUE)
    rm(i, strata_failed_mer, strata_failed_ntwb, allcoeffs_mer, allcoeffs_ntwb,
       strata_mer_suc, strata_ntwb_suc, needCombs, strata_mer, strata_ntwb)
  } # end of second attempt



  if(nrow(strata_failed) > 0){ ## initiate third attempt
    # The third attempt is to combine all bec zones regardless of coastal and interior
    needCombs <- unique(strata_failed[,.(SP0, LV_D)])

    strata_failed_mer <- needCombs[0,]
    strata_failed_ntwb <- needCombs[0,]

    allcoeffs_mer <- data.table(SP0 = character(),
                                LV_D = character(),
                                a = numeric(),
                                b = numeric(),
                                c = numeric(),
                                N_OBS = numeric(),
                                DATA_SOURCE = character())
    allcoeffs_ntwb <- data.table(SP0 = character(),
                                 LV_D = character(),
                                 a = numeric(),
                                 b = numeric(),
                                 c = numeric(),
                                 N_OBS = numeric(),
                                 DATA_SOURCE = character())
    for(i in 1:nrow(needCombs)){
      indistrata_data <- all_trees_ratio[SP0 == needCombs$SP0[i] &
                                           LV_D == needCombs$LV_D[i],]
      if(nrow(indistrata_data) > minObs){
        fit_mer <- try(nlsout_mer <- nls(RATIO_MER ~ a * (1 - exp(-b * DBH_MOD))^c,
                                         data = indistrata_data[RATIO_MER != 0, ],
                                         start = list(a = 0.9, b = 0.5, c = 1.5),
                                         trace = FALSE),
                       TRUE)



        fit_ntwb <- try(nlsout_ntwb <- nls(RATIO_NTWB ~ a * (1 - exp(-b * DBH_MOD))^c,
                                           data = indistrata_data[RATIO_NTWB != 0, ],
                                           start = list(a = 0.9, b = 0.5, c = 1.5),
                                           trace = FALSE),
                        TRUE)


        ## for mer
        if(class(fit_mer) == "try-error"){
          strata_failed_mer <- rbind(strata_failed_mer, needCombs[i])
        } else {
          indistrata_mer_coef <- data.table(rbind(coef(nlsout_mer)))
          indistrata_mer_coef[, ':='(SP0 = needCombs$SP0[i],
                                     LV_D = needCombs$LV_D[i],
                                     N_OBS = nrow(indistrata_data))]
          indistrata_mer_coef[, DATA_SOURCE := paste0(SP0, " + ", LV_D)]
          allcoeffs_mer <- rbind(allcoeffs_mer, indistrata_mer_coef)
          rm(nlsout_mer, indistrata_mer_coef)
        }

        if(class(fit_ntwb) == "try-error"){
          strata_failed_ntwb <- rbind(strata_failed_ntwb, needCombs[i])
        } else {
          indistrata_ntwb_coef <- data.table(rbind(coef(nlsout_ntwb)))
          indistrata_ntwb_coef[, ':='(SP0 = needCombs$SP0[i],
                                      LV_D = needCombs$LV_D[i],
                                      N_OBS = nrow(indistrata_data))]
          indistrata_ntwb_coef[, DATA_SOURCE := paste0(SP0, " + ", LV_D)]
          allcoeffs_ntwb <- rbind(allcoeffs_ntwb, indistrata_ntwb_coef)
          rm(nlsout_ntwb, indistrata_ntwb_coef)
        }
        rm(fit_mer, fit_ntwb)
      } else {
        strata_failed_mer <- rbind(strata_failed_mer, needCombs[i])
        strata_failed_ntwb <- rbind(strata_failed_ntwb, needCombs[i])
      }
      rm(indistrata_data)
    }

    strata_mer <- merge(strata_failed[MER_FAILED == TRUE, .(BEC_ZONE, SP0, LV_D, SP_TYPE)],
                        allcoeffs_mer,
                        by = c("SP0", "LV_D"),
                        all.x = TRUE)
    strata_ntwb <- merge(strata_failed[NTWB_FAILED == TRUE, .(BEC_ZONE, SP0, LV_D, SP_TYPE)],
                         allcoeffs_ntwb,
                         by = c("SP0", "LV_D"),
                         all.x = TRUE)
    strata_mer_suc <- strata_mer[!is.na(N_OBS),]
    strata_failed_mer <- strata_mer[is.na(N_OBS),.(BEC_ZONE, SP0, LV_D, SP_TYPE, MER_FAILED = TRUE)]
    strata_ntwb_suc <- strata_ntwb[!is.na(N_OBS)]
    strata_failed_ntwb <- strata_ntwb[is.na(N_OBS),
                                      .(BEC_ZONE, SP0, LV_D, SP_TYPE, NTWB_FAILED = TRUE)]
    allcoeffs_mer_suc <- rbindlist(list(allcoeffs_mer_suc, strata_mer_suc), fill = TRUE)
    allcoeffs_ntwb_suc <- rbindlist(list(allcoeffs_ntwb_suc, strata_ntwb_suc), fill = TRUE)
    strata_failed <- merge(strata_failed_mer,
                           strata_failed_ntwb,
                           by = c("BEC_ZONE", "SP0", "LV_D", "SP_TYPE"),
                           all = TRUE)
    rm(i, strata_failed_mer, strata_failed_ntwb, allcoeffs_mer, allcoeffs_ntwb,
       strata_mer_suc, strata_ntwb_suc, needCombs, strata_mer, strata_ntwb)
  } # end of third attempt

  if(nrow(strata_failed) > 0){ # initiate the last attempt
    # The forth attempt is to use species type and lv_d regardless of bec
    needCombs <- unique(strata_failed[,.(BEC_ZONE, SP_TYPE, LV_D)])

    strata_failed_mer <- needCombs[0,]
    strata_failed_ntwb <- needCombs[0,]

    allcoeffs_mer <- data.table(BEC_ZONE = character(),
                                SP_TYPE = character(),
                                LV_D = character(),
                                a = numeric(),
                                b = numeric(),
                                c = numeric(),
                                N_OBS = numeric(),
                                DATA_SOURCE = character())
    allcoeffs_ntwb <- data.table(BEC_ZONE = character(),
                                 SP_TYPE = character(),
                                 LV_D = character(),
                                 a = numeric(),
                                 b = numeric(),
                                 c = numeric(),
                                 N_OBS = numeric(),
                                 DATA_SOURCE = character())
    for(i in 1:nrow(needCombs)){
      indistrata_data <- all_trees_ratio[BEC_ZONE == needCombs$BEC_ZONE[i] &
                                         SP_TYPE == needCombs$SP_TYPE[i] &
                                           LV_D == needCombs$LV_D[i],]
      if(nrow(indistrata_data) > minObs){
        fit_mer <- try(nlsout_mer <- nls(RATIO_MER ~ a * (1 - exp(-b * DBH_MOD))^c,
                                         data = indistrata_data[RATIO_MER != 0, ],
                                         start = list(a = 0.9, b = 0.5, c = 1.5),
                                         trace = FALSE),
                       TRUE)

        fit_ntwb <- try(nlsout_ntwb <- nls(RATIO_NTWB ~ a * (1 - exp(-b * DBH_MOD))^c,
                                           data = indistrata_data[RATIO_NTWB != 0, ],
                                           start = list(a = 0.9, b = 0.5, c = 1.5),
                                           trace = FALSE),
                        TRUE)


        ## for mer
        if(class(fit_mer) == "try-error"){
          strata_failed_mer <- rbind(strata_failed_mer, needCombs[i])
        } else {
          indistrata_mer_coef <- data.table(rbind(coef(nlsout_mer)))
          indistrata_mer_coef[, ':='(BEC_ZONE = needCombs$BEC_ZONE[i],
                                     SP_TYPE = needCombs$SP_TYPE[i],
                                     LV_D = needCombs$LV_D[i],
                                     N_OBS = nrow(indistrata_data))]
          indistrata_mer_coef[, DATA_SOURCE := paste0(BEC_ZONE, " + ", SP_TYPE, " + ", LV_D)]
          allcoeffs_mer <- rbind(allcoeffs_mer, indistrata_mer_coef)
          rm(nlsout_mer, indistrata_mer_coef)
        }

        if(class(fit_ntwb) == "try-error"){
          strata_failed_ntwb <- rbind(strata_failed_ntwb, needCombs[i])
        } else {
          indistrata_ntwb_coef <- data.table(rbind(coef(nlsout_ntwb)))
          indistrata_ntwb_coef[, ':='(BEC_ZONE = needCombs$BEC_ZONE[i],
                                      SP_TYPE = needCombs$SP_TYPE[i],
                                      LV_D = needCombs$LV_D[i],
                                      N_OBS = nrow(indistrata_data))]
          indistrata_ntwb_coef[, DATA_SOURCE := paste0(BEC_ZONE, " + ", SP_TYPE, " + ", LV_D)]
          allcoeffs_ntwb <- rbind(allcoeffs_ntwb, indistrata_ntwb_coef)
          rm(nlsout_ntwb, indistrata_ntwb_coef)
        }
        rm(fit_mer, fit_ntwb)
      } else {
        strata_failed_mer <- rbind(strata_failed_mer, needCombs[i])
        strata_failed_ntwb <- rbind(strata_failed_ntwb, needCombs[i])
      }
      rm(indistrata_data)
    }

    strata_mer <- merge(strata_failed[MER_FAILED == TRUE, .(BEC_ZONE, SP0, LV_D, SP_TYPE)],
                        allcoeffs_mer,
                        by = c("BEC_ZONE", "SP_TYPE", "LV_D"),
                        all.x = TRUE)
    strata_ntwb <- merge(strata_failed[NTWB_FAILED == TRUE, .(BEC_ZONE, SP0, LV_D, SP_TYPE)],
                         allcoeffs_ntwb,
                         by = c("BEC_ZONE", "SP_TYPE", "LV_D"),
                         all.x = TRUE)
    strata_mer_suc <- strata_mer[!is.na(N_OBS),]
    strata_failed_mer <- strata_mer[is.na(N_OBS),.(BEC_ZONE, SP0, LV_D, SP_TYPE, MER_FAILED = TRUE)]
    strata_ntwb_suc <- strata_ntwb[!is.na(N_OBS)]
    strata_failed_ntwb <- strata_ntwb[is.na(N_OBS),
                                      .(BEC_ZONE, SP0, LV_D, SP_TYPE, NTWB_FAILED = TRUE)]
    allcoeffs_mer_suc <- rbindlist(list(allcoeffs_mer_suc, strata_mer_suc), fill = TRUE)
    allcoeffs_ntwb_suc <- rbindlist(list(allcoeffs_ntwb_suc, strata_ntwb_suc), fill = TRUE)
    strata_failed <- merge(strata_failed_mer,
                           strata_failed_ntwb,
                           by = c("BEC_ZONE", "SP0", "LV_D", "SP_TYPE"),
                           all = TRUE)
    rm(i, strata_failed_mer, strata_failed_ntwb, allcoeffs_mer, allcoeffs_ntwb,
       strata_mer_suc, strata_ntwb_suc, needCombs, strata_mer, strata_ntwb)
  } # end of forth attempt


  if(nrow(strata_failed) > 0){ # initiate LAST attempt
    # The second attempt is to group bgc zone into coastal and interior
    strata_failed[BEC_ZONE %in% c("CMA", "MH", "CDF", "CWH"), BEC_REG := "COAST"]
    strata_failed[is.na(BEC_REG), BEC_REG := "INTERIOR"]

    all_trees_ratio[BEC_ZONE %in% c("CMA", "MH", "CDF", "CWH"), BEC_REG := "COAST"]
    all_trees_ratio[is.na(BEC_REG), BEC_REG := "INTERIOR"]

    needCombs <- unique(strata_failed[,.(BEC_REG, SP_TYPE, LV_D)])

    strata_failed_mer <- needCombs[0,]
    strata_failed_ntwb <- needCombs[0,]

    allcoeffs_mer <- data.table(BEC_REG = character(),
                                SP_TYPE = character(),
                                LV_D = character(),
                                a = numeric(),
                                b = numeric(),
                                c = numeric(),
                                N_OBS = numeric(),
                                DATA_SOURCE = character())
    allcoeffs_ntwb <- data.table(BEC_REG = character(),
                                 SP_TYPE = character(),
                                 LV_D = character(),
                                 a = numeric(),
                                 b = numeric(),
                                 c = numeric(),
                                 N_OBS = numeric(),
                                 DATA_SOURCE = character())
    for(i in 1:nrow(needCombs)){
      indistrata_data <- all_trees_ratio[BEC_REG == needCombs$BEC_REG[i] &
                                           SP_TYPE == needCombs$SP_TYPE[i] &
                                           LV_D == needCombs$LV_D[i],]
      if(nrow(indistrata_data) > minObs){
        fit_mer <- try(nlsout_mer <- nls(RATIO_MER ~ a * (1 - exp(-b * DBH_MOD))^c,
                                         data = indistrata_data[RATIO_MER != 0, ],
                                         start = list(a = 0.9, b = 0.5, c = 1.5),
                                         trace = FALSE),
                       TRUE)



        fit_ntwb <- try(nlsout_ntwb <- nls(RATIO_NTWB ~ a * (1 - exp(-b * DBH_MOD))^c,
                                           data = indistrata_data[RATIO_NTWB != 0, ],
                                           start = list(a = 0.9, b = 0.5, c = 1.5),
                                           trace = FALSE),
                        TRUE)


        ## for mer
        if(class(fit_mer) == "try-error"){
          strata_failed_mer <- rbind(strata_failed_mer, needCombs[i])
        } else {
          indistrata_mer_coef <- data.table(rbind(coef(nlsout_mer)))
          indistrata_mer_coef[, ':='(BEC_REG = needCombs$BEC_REG[i],
                                     SP_TYPE = needCombs$SP_TYPE[i],
                                     LV_D = needCombs$LV_D[i],
                                     N_OBS = nrow(indistrata_data))]
          indistrata_mer_coef[, DATA_SOURCE := paste0(BEC_REG, " + ", SP_TYPE, " + ", LV_D)]
          allcoeffs_mer <- rbind(allcoeffs_mer, indistrata_mer_coef)
          rm(nlsout_mer, indistrata_mer_coef)
        }

        if(class(fit_ntwb) == "try-error"){
          strata_failed_ntwb <- rbind(strata_failed_ntwb, needCombs[i])
        } else {
          indistrata_ntwb_coef <- data.table(rbind(coef(nlsout_ntwb)))
          indistrata_ntwb_coef[, ':='(BEC_REG = needCombs$BEC_REG[i],
                                      SP_TYPE = needCombs$SP_TYPE[i],
                                      LV_D = needCombs$LV_D[i],
                                      N_OBS = nrow(indistrata_data))]
          indistrata_ntwb_coef[, DATA_SOURCE := paste0(BEC_REG, " + ", SP_TYPE, " + ", LV_D)]
          allcoeffs_ntwb <- rbind(allcoeffs_ntwb, indistrata_ntwb_coef)
          rm(nlsout_ntwb, indistrata_ntwb_coef)
        }
        rm(fit_mer, fit_ntwb)
      } else {
        strata_failed_mer <- rbind(strata_failed_mer, needCombs[i])
        strata_failed_ntwb <- rbind(strata_failed_ntwb, needCombs[i])
      }
      rm(indistrata_data)
    }

    strata_mer <- merge(strata_failed[MER_FAILED == TRUE, .(BEC_REG, BEC_ZONE, SP0, LV_D, SP_TYPE)],
                        allcoeffs_mer,
                        by = c("BEC_REG", "SP_TYPE", "LV_D"),
                        all.x = TRUE)
    strata_ntwb <- merge(strata_failed[NTWB_FAILED == TRUE, .(BEC_REG, BEC_ZONE, SP0, LV_D, SP_TYPE)],
                         allcoeffs_ntwb,
                         by = c("BEC_REG", "SP_TYPE", "LV_D"),
                         all.x = TRUE)
    strata_mer_suc <- strata_mer[!is.na(N_OBS),]
    strata_failed_mer <- strata_mer[is.na(N_OBS),.(BEC_ZONE, SP0, LV_D, SP_TYPE, MER_FAILED = TRUE)]
    strata_ntwb_suc <- strata_ntwb[!is.na(N_OBS)]
    strata_failed_ntwb <- strata_ntwb[is.na(N_OBS),
                                      .(BEC_ZONE, SP0, LV_D, SP_TYPE, NTWB_FAILED = TRUE)]
    allcoeffs_mer_suc <- rbindlist(list(allcoeffs_mer_suc, strata_mer_suc), fill = TRUE)
    allcoeffs_ntwb_suc <- rbindlist(list(allcoeffs_ntwb_suc, strata_ntwb_suc), fill = TRUE)
    strata_failed <- merge(strata_failed_mer,
                           strata_failed_ntwb,
                           by = c("BEC_ZONE", "SP0", "LV_D", "SP_TYPE"),
                           all = TRUE)
    rm(i, strata_failed_mer, strata_failed_ntwb, allcoeffs_mer, allcoeffs_ntwb,
       strata_mer_suc, strata_ntwb_suc, needCombs, strata_mer, strata_ntwb)
  } # end of second attempt
  if(exists("strata_failed")){
    if(nrow(strata_failed) > 0){
      strata_failed[, texts := paste0(BEC_ZONE, " + ", SP0, " + ", LV_D, "\n")]
      warning("The toWSV ratio curve can not be derived for the strata below: \n", strata_failed$texts)
    }
  }
  return(list(mer_ratio_coef = allcoeffs_mer_suc,
              ntwb_ratio_coef = allcoeffs_ntwb_suc))
}



