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
toWSVRatio_curve <- function(inputData, needCombs,
                             minDBH = 10, minObs = 100){
  inputData[, ":="(vol_mer_ratio = VOL_MER/VOL_WSV,
                   vol_ntwb_ratio = VOL_NTWB/VOL_WSV,
                   vol_dwb_ratio = VOL_DWB/VOL_WSV)]
  all_trees_ratio <- inputData[MEAS_INTENSE %in% c("FULL", "ENHANCED", "H-ENHANCED") &
                                 DBH >= minDBH &
                                 !is.na(vol_mer_ratio) &
                                 VOL_MER %!=% 0 &
                                 !is.na(DBH),]
  all_trees_ratio[, DBH_MOD := DBH - 10]
  # first fit the vol_mer_ratio using all the full, enhanced and h-enhanced trees
  all_trees_ratio[, BEC_REG := "I"]
  all_trees_ratio[BEC_ZONE %in% c("CMA", "MH", "CDF", "CWH"), BEC_REG := "C"]
  sptype <- lookup_species()
  all_trees_ratio <- merge(all_trees_ratio,
                           unique(sptype[,.(SP0, SP_TYPE)]),
                           by = "SP0",
                           all.x = TRUE)
  # needCombs <- unique(all_trees_ratio[,.(BEC_ZONE, SP0, LV_D, BEC_REG, SP_TYPE)])
  needCombs[, uniid := 1: nrow(needCombs)]
  needCombs[, BEC_REG := "I"]
  needCombs[BEC_ZONE %in% c("CMA", "MH", "CDF", "CWH"), BEC_REG := "C"]
  needCombs <- merge(needCombs,
                     unique(sptype[,.(SP0, SP_TYPE)]),
                     by = "SP0",
                     all.x = TRUE)
  rm(sptype, inputData)

  all_vol_attributes <- c("mer", "ntwb", "dwb")
  all_ratio <- list()
  for(indivol in all_vol_attributes){
    if(indivol == "mer"){
      fitdata_all <- data.table::copy(all_trees_ratio)
    } else if(indivol == "ntwb"){
      ## for ntwb, using nonpsp full and enhanced data
      fitdata_all <- all_trees_ratio[MEAS_INTENSE %in% c("FULL", "ENHANCED") &
                                       VOL_NTWB > 0 &
                                       TYPE_CD != "PSP",]
    } else {
      ## for dwb, using all full and enhanced data
      fitdata_all <- all_trees_ratio[MEAS_INTENSE %in% c("FULL", "ENHANCED") &
                                       VOL_DWB > 0,]
    }
    needCombs_done <- NULL
    ## start the first attempt using bec+sp0+lvd
    for (i in needCombs$uniid) {
      fitdata_i <- fitdata_all[BEC_ZONE == needCombs[uniid == i,]$BEC_ZONE &
                                 SP0 == needCombs[uniid == i,]$SP0 &
                                 LV_D == needCombs[uniid == i,]$LV_D,]
      if(nrow(fitdata_i) >= minObs){
        best_model <- best_ratio_model(attribute = paste0(indivol, "_ratio"),
                                       fitdata = fitdata_i)
        row_indi <- needCombs[uniid == i,]
        row_indi[, ':='(DATA_SOURCE = "BEC+SP0+LVD",
                        NOBS = nrow(fitdata_i))]
        needCombs_done <- rbind(needCombs_done,
                                cbind(row_indi, best_model))
        rm(row_indi, best_model)
      }
      rm(fitdata_i)
    }
    rm(i)

    needCombs_done <- needCombs_done[!is.na(a),]
    needCombs_rest <- needCombs[!(uniid %in% needCombs_done$uniid),]
    ## start the second attempt using becreg+sp0+lvd
    for (i in needCombs_rest$uniid) {
      fitdata_i <- fitdata_all[BEC_REG == needCombs_rest[uniid == i,]$BEC_REG &
                                 SP0 == needCombs_rest[uniid == i,]$SP0 &
                                 LV_D == needCombs_rest[uniid == i,]$LV_D,]
      if(nrow(fitdata_i) >= minObs){
        best_model <- best_ratio_model(attribute = paste0(indivol, "_ratio"),
                                       fitdata = fitdata_i)
        row_indi <- needCombs_rest[uniid == i,]
        row_indi[, ':='(DATA_SOURCE = "BECREG+SP0+LVD",
                        NOBS = nrow(fitdata_i))]
        needCombs_done <- rbind(needCombs_done,
                                cbind(row_indi, best_model))
        rm(row_indi, best_model)
      }
      rm(fitdata_i)
    }
    rm(needCombs_rest, i)

    needCombs_done <- needCombs_done[!is.na(a),]
    needCombs_rest <- needCombs[!(uniid %in% needCombs_done$uniid),]
    ## start the third attempt using sp0+lvd
    for (i in needCombs_rest$uniid) {
      fitdata_i <- fitdata_all[SP0 == needCombs_rest[uniid == i,]$SP0 &
                                 LV_D == needCombs_rest[uniid == i,]$LV_D,]
      if(nrow(fitdata_i) >= minObs){
        best_model <- best_ratio_model(attribute = paste0(indivol, "_ratio"),
                                       fitdata = fitdata_i)
        row_indi <- needCombs_rest[uniid == i,]
        row_indi[, ':='(DATA_SOURCE = "SP0+LVD",
                        NOBS = nrow(fitdata_i))]
        needCombs_done <- rbind(needCombs_done,
                                cbind(row_indi, best_model))
        rm(row_indi, best_model)
      }
      rm(fitdata_i)
    }
    rm(needCombs_rest, i)
    needCombs_done <- needCombs_done[!is.na(a),]
    needCombs_rest <- needCombs[!(uniid %in% needCombs_done$uniid),]
    ## start the forth attempt using sptype+lvd
    for (i in needCombs_rest$uniid) {
      fitdata_i <- fitdata_all[SP_TYPE == needCombs_rest[uniid == i,]$SP_TYPE &
                                 LV_D == needCombs_rest[uniid == i,]$LV_D,]
      if(nrow(fitdata_i) >= minObs){
        best_model <- best_ratio_model(attribute = paste0(indivol, "_ratio"),
                                       fitdata = fitdata_i)
        row_indi <- needCombs_rest[uniid == i,]
        row_indi[, ':='(DATA_SOURCE = "SPTYPE+LVD",
                        NOBS = nrow(fitdata_i))]
        needCombs_done <- rbind(needCombs_done,
                                cbind(row_indi, best_model))
        rm(row_indi, best_model)
      }
      rm(fitdata_i)
    }
    needCombs_done[, uniid := NULL]
    rm(i, needCombs_rest)
    all_ratio[[paste0(indivol, "_ratio")]] <- needCombs_done
  }
  return(all_ratio)
}


best_ratio_model <- function(attribute,
                             fitdata){
  if(attribute == "mer_ratio"){
    j_mod <- NA
    mer_model_comp <- NULL
    mer_models <- list()
    for (j in seq(1, 10, by = 0.5)) {
      fitdata[, DBH_MOD2 := j + DBH_MOD]
      fit_mer <- try(nlsout_mer <- nls(vol_mer_ratio ~ a * (1 - exp(-b * DBH_MOD))^c + (d*DBH_MOD)/DBH_MOD2,
                                       data = fitdata,
                                       start = list(a = 0.9, b = 0.5, c = 1.5, d = 0.1),
                                       trace = FALSE,
                                       control = nls.control(maxiter = 500)),
                     TRUE)
      fitdata[, DBH_MOD2 := NULL]

      if(class(fit_mer) != "try-error"){
        nlsout_mer_smry <- summary(nlsout_mer)
        fitdata$residuals <- nlsout_mer_smry$residuals
        mer_model_comp <- rbind(mer_model_comp,
                                data.table(j = j,
                                           SS_res_all = (sum(fitdata$residuals^2))^(1/2),
                                           SS_res_big30 = (sum(fitdata[DBH >= 30,]$residuals^2))^(1/2)))
        mer_models[[paste0("model", j)]] <- nlsout_mer
      }
    }

    if(!is.null(mer_model_comp)){
      mer_model_best <- mer_model_comp[which.min(SS_res_big30),]
      j_mod <- mer_model_best$j
      coeff_best <- data.table(rbind(coef(mer_models[[paste0("model", j_mod)]])))
      coeff_best[, j := j_mod]
    } else {
      fit_mer <- try(nlsout_mer_best <- nls(vol_mer_ratio ~ a * (1 - exp(-b * DBH_MOD))^c,
                                            data = fitdata,
                                            start = list(a = 0.9, b = 0.5, c = 1.5),
                                            trace = FALSE,
                                            control = nls.control(maxiter = 500)),
                     TRUE)
      if(class(fit_mer) != "try-error"){
        coeff_best <- data.table(rbind(coef(nlsout_mer_best)))
        coeff_best[, ':='(d = 0,
                          j = 0)]



      } else {
        coeff_best <- data.table(a = as.numeric(NA),
                                 b = as.numeric(NA),
                                 c = as.numeric(NA),
                                 d = as.numeric(NA),
                                 j = as.numeric(NA))
      }
    }
  } else if (attribute == "ntwb_ratio"){
    fit_ntwb <- try(nlsout_best <- nls(vol_ntwb_ratio ~ a * (1 - exp(-b * DBH_MOD))^c,
                                       data = fitdata,
                                       start = list(a = 0.9, b = 0.5, c = 1.5),
                                       trace = FALSE,
                                       control = nls.control(maxiter = 500)),
                    TRUE)
    if(class(fit_ntwb) != "try-error"){
      coeff_best <- data.table(rbind(coef(nlsout_best)))
    } else {
      coeff_best <- data.table(a = as.numeric(NA),
                               b = as.numeric(NA),
                               c = as.numeric(NA))
    }
  } else {
    fit_dwb <- try(nlsout_best <- nls(vol_dwb_ratio ~ a * (1 - exp(-b * DBH_MOD))^c,
                                      data = fitdata,
                                      start = list(a = 0.9, b = 0.5, c = 1.5),
                                      trace = FALSE,
                                      control = nls.control(maxiter = 500)),
                   TRUE)
    if(class(fit_dwb) != "try-error"){
      coeff_best <- data.table(rbind(coef(nlsout_best)))
    } else {
      coeff_best <- data.table(a = as.numeric(NA),
                               b = as.numeric(NA),
                               c = as.numeric(NA))
    }
  }
  return(coeff_best)
}

