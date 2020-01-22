WSV_BARegression <- function(masterTable, regressionData, minObs = 3, minR2 = 0.3){
  if(missing(masterTable)){
    masterTable <- unique(regressionData[,.(PRJ_GRP, LV_D, SP0)],
                          by = c("PRJ_GRP", "LV_D", "SP0"))
  }

  outputHeader <- data.table::copy(masterTable)
  outputHeader[, combination := paste(PRJ_GRP, LV_D, SP0, sep = " & ")]
  ## first attempt at prj_grp lv_d and sp0 level
  allmodels <- FAIBBase::lm_group(formula = "L10WSV~L10BA",
                        data = regressionData,
                        groupBy = c("PRJ_GRP", "LV_D", "SP0"))
  reg_sp0 <- wsv_baRegSummary(allmodels)
  # select models that fullfills the conditions
  reg_sp0 <- reg_sp0[COUNT %>>% minObs & EDF > 0 & RSQ %>=% minR2, ][, SOURCE := "Sp0"]
  rm(allmodels)
  outputHeader <- FAIBBase::merge_dupUpdate(outputHeader, reg_sp0, by = "combination", all.x = TRUE)
  output <- outputHeader[!is.na(RSQ), ]
  outputHeader <- outputHeader[is.na(RSQ),.(PRJ_GRP, LV_D, SP0, TYPE)]


  if(nrow(outputHeader) > 0){
    ## second attempt at prj_grp lv_d and type level
    outputHeader[, combination := paste(PRJ_GRP, LV_D, TYPE, sep = " & ")]
    allmodels <- FAIBBase::lm_group(formula = "L10WSV~L10BA",
                          data = regressionData,
                          groupBy = c("PRJ_GRP", "LV_D", "TYPE"))
    reg_type <- wsv_baRegSummary(allmodels)
    ## select models
    rep_type <- reg_type[COUNT %>>% minObs & EDF > 0 & RSQ %>=% minR2, ][, SOURCE := "Type"]
    outputHeader <- FAIBBase::merge_dupUpdate(outputHeader, rep_type, by = "combination",
                                    all.x = TRUE)
    tempoutput <- outputHeader[!is.na(RSQ),]
    output <- rbindlist(list(output,
                             tempoutput[,names(output), with = FALSE]))
    outputHeader <- outputHeader[is.na(RSQ),]
    rm(allmodels, tempoutput)
    if(nrow(outputHeader) > 0){
      ## one more attempt to derive regression based on prj_grp and lv_d
      outputHeader[, combination := paste(PRJ_GRP, LV_D, sep = " & ")]
      allmodels <- FAIBBase::lm_group(formula = "L10WSV~L10BA",
                            data = regressionData,
                            groupBy = c("PRJ_GRP", "LV_D"))
      reg_lv_d <- wsv_baRegSummary(allmodels)
      ## select models
      reg_lv_d <- reg_lv_d[COUNT %>>% minObs & EDF > 0 & RSQ %>=% minR2, ][, SOURCE := "Lv_D"]
      outputHeader <- FAIBBase::merge_dupUpdate(outputHeader, reg_lv_d, by = "combination",
                                      all.x = TRUE)
      tempoutput <- outputHeader[!is.na(RSQ),]
      output <- rbindlist(list(output,
                               tempoutput[,names(output), with = FALSE]))
      outputHeader <- outputHeader[is.na(RSQ),]
      rm(allmodels, tempoutput)
    }
  }
  output[, ':='(combination = NULL)]
  output[LV_D %in% c("L", "D"), SF_COMPILE := "S"]
  return(output[order(PRJ_GRP, LV_D, SP0)])
}


wsv_baRegSummary <- function(modellist){
  modeloutputlist <- lapply(modellist, function(s) data.table(COUNT = length(s$residuals),
                                                              MODEL = "WSV",
                                                              INTERCEPT = s$coefficients[1],
                                                              L10BA = s$coefficients[2],
                                                              EDF = s$df.residual,
                                                              RSQ = summary(s)$r.squared,
                                                              MSE = mean(s$residuals^2)))
  for(i in names(modeloutputlist)){
    if(i == names(modeloutputlist)[1]){
      output <- cbind(data.table(combination = i),
                      modeloutputlist[[i]])
    } else {
      output <- rbind(output,
                      cbind(data.table(combination = i),
                            modeloutputlist[[i]]))
    }
  }
  return(output)
}

