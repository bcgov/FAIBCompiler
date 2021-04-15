#' To fit basal area and whole stem volume regression using mixed effect model
#'
#' @description This function takes the selected data for fitting regression model between
#'              basal area and whole stem volume using mixed effect model. The models are
#'              fitted based on strata of BEC+SP0+LV_D
#'
#' @param inputData data.table, The data for fitting regression.
#' @param needCombs data.table, The combinations of BEC+SP0+LV_D are needed to derive coefficients.
#'
#' @return coefficient table
#'
#'
#' @importFrom data.table ':=' rbindlist
#' @importFrom nlme lme
#' @importFrom MuMIn r.squaredGLMM
#'
#'
#' @export
#' @docType methods
#' @rdname regBA_WSV
#'
#' @author Yong Luo
#'
regBA_WSV <- function(inputData, needCombs){
  all_trees_reg <- inputData[,.(SAMP_POINT = as.numeric(substr(CLSTR_ID, 1, 7)),
                                CLSTR_ID, BGC_ZONE, SP0, LV_D, BA_TREE, VOL_WSV,
                                logba = log(BA_TREE), logwsv = log(VOL_WSV))]
  specieslookuptable <- lookup_species()
  specieslookuptable <- unique(specieslookuptable[,.(SP0, SP_TYPE)], by = "SP0")
  all_trees_reg <- merge(all_trees_reg, specieslookuptable, by = "SP0", all.x = TRUE)
  needCombs <- merge(needCombs, specieslookuptable, by = "SP0", all.x = TRUE)
  allStrata <- needCombs[SP0 != "X",] ## not missing species
  strata_missing <- needCombs[SP0 == "X"]

  # first attempt by bec+sp0+lv_d
  outputs_coeff <- NULL
  outputs_random <- NULL
  strata_failed <- allStrata[0,]

  for(i in 1:nrow(allStrata)){
    indidataset <- all_trees_reg[BGC_ZONE == allStrata$BGC_ZONE[i] &
                                   SP0 == allStrata$SP0[i] &
                                   LV_D == allStrata$LV_D[i] &
                                   logwsv != -Inf]
    indidataset[, noobs := length(SP0), by = "SAMP_POINT"]
    indidataset <- indidataset[noobs >= 3,]
    a <- try(mixmodel <- nlme::lme(logwsv~logba, random = ~logba|SAMP_POINT, data = indidataset),
             TRUE)
    if(class(a) == "try-error" | length(unique(indidataset$SAMP_POINT)) < 5){
      strata_failed <- rbind(strata_failed, allStrata[i,])
    } else {
      r2s <- round(data.table(r.squaredGLMM(mixmodel)), 4)
      names(r2s) <- c("R2_Marginal", "R2_Condition")
      randomcoeff <- data.frame(mixmodel$coefficients$random)
      names(randomcoeff) <- c("INTERCEPT_RDM", "SLOPE_RDM")
      randomcoeff$SAMP_POINT <- row.names(randomcoeff)
      randomcoeff <- data.table(randomcoeff)
      outputs_coeff <- rbind(outputs_coeff,
                             cbind(data.table(BGC_ZONE = allStrata$BGC_ZONE[i],
                                        SP0 = allStrata$SP0[i],
                                        LV_D = allStrata$LV_D[i],
                                        NOOBS = nrow(indidataset),
                                        INTERCEPT = as.numeric(mixmodel$coefficients$fixed)[1],
                                        SLOPE = as.numeric(mixmodel$coefficients$fixed)[2],
                                        DATA_SRCE = "BEC+SP0+LV_D"),
                                   r2s))
      outputs_random <- rbind(outputs_random,
                              randomcoeff[,.(BGC_ZONE = allStrata$BGC_ZONE[i],
                                             SP0 = allStrata$SP0[i],
                                             LV_D = allStrata$LV_D[i],
                                             SAMP_POINT, INTERCEPT_RDM, SLOPE_RDM,
                                             DATA_SRCE = "BEC+SP0+LV_D")])
      rm(mixmodel, randomcoeff)
    }
    rm(a)
  }
  rm(i, allStrata)

  coeff_succ <- outputs_coeff
  random_succ <- outputs_random
  rm(outputs_coeff, outputs_random)

  if(nrow(strata_failed) > 0){ # initiate second attempt
    # The second attempt is to group bgc zone into coastal and interior
    strata_failed[BGC_ZONE %in% c("CMA", "MH", "CDF", "CWH"), BGC_REG := "COAST"]
    strata_failed[is.na(BGC_REG), BGC_REG := "INTERIOR"]
    all_trees_reg[BGC_ZONE %in% c("CMA", "MH", "CDF", "CWH"), BGC_REG := "COAST"]
    all_trees_reg[is.na(BGC_REG), BGC_REG := "INTERIOR"]
    workstrata <- unique(strata_failed[,.(BGC_REG, SP0, LV_D)],
                         by = c("BGC_REG", "SP0", "LV_D"))
    workstrata_failed <- workstrata[0,]
    outputs_coeff <- NULL
    outputs_random <- NULL
    for(i in 1:nrow(workstrata)){
      indidataset <- all_trees_reg[BGC_REG == workstrata$BGC_REG[i] &
                                     SP0 == workstrata$SP0[i] &
                                     LV_D == workstrata$LV_D[i] &
                                     logwsv != -Inf]
      indidataset[, noobs := length(SP0), by = "SAMP_POINT"]
      indidataset <- indidataset[noobs >= 3,]
      a <- try(mixmodel <- nlme::lme(logwsv~logba, random = ~logba|SAMP_POINT, data = indidataset),
               TRUE)
      if(class(a) == "try-error" | length(unique(indidataset$SAMP_POINT)) < 5){
        workstrata_failed <- rbind(workstrata_failed, workstrata[i,])
      } else {
        r2s <- round(data.table(r.squaredGLMM(mixmodel)), 4)
        names(r2s) <- c("R2_Marginal", "R2_Condition")
        randomcoeff <- data.frame(mixmodel$coefficients$random)
        names(randomcoeff) <- c("INTERCEPT_RDM", "SLOPE_RDM")
        randomcoeff$SAMP_POINT <- row.names(randomcoeff)
        randomcoeff <- data.table(randomcoeff)
        outputs_coeff <- rbind(outputs_coeff,
                               cbind(data.table(BGC_REG = workstrata$BGC_REG[i],
                                          SP0 = workstrata$SP0[i],
                                          LV_D = workstrata$LV_D[i],
                                          NOOBS = nrow(indidataset),
                                          INTERCEPT = as.numeric(mixmodel$coefficients$fixed)[1],
                                          SLOPE = as.numeric(mixmodel$coefficients$fixed)[2],
                                          DATA_SRCE = "BEC_GRP+SP0+LV_D"),
                                     r2s))
        outputs_random <- rbind(outputs_random,
                                randomcoeff[,.(BGC_REG = workstrata$BGC_REG[i],
                                               SP0 = workstrata$SP0[i],
                                               LV_D = workstrata$LV_D[i],
                                               SAMP_POINT, INTERCEPT_RDM, SLOPE_RDM,
                                               DATA_SRCE = "BEC_GRP+SP0+LV_D")])
        rm(mixmodel, randomcoeff)
      }
      rm(a)
    }
    rm(i, workstrata)


    coeff_succ2 <- merge(strata_failed, outputs_coeff,
                         by = c("BGC_REG", "SP0", "LV_D"))
    random_succ2 <- merge(strata_failed, outputs_random,
                          by = c("BGC_REG", "SP0", "LV_D"),
                          allow.cartesian = TRUE)
    coeff_succ <- rbindlist(list(coeff_succ, coeff_succ2), fill = TRUE)
    random_succ <- rbindlist(list(random_succ, random_succ2), fill = TRUE)
    rm(coeff_succ2, random_succ2, outputs_coeff, outputs_random)
    strata_failed <- merge(workstrata_failed, strata_failed,
                           by = c("BGC_REG", "SP0", "LV_D"))

    if(nrow(strata_failed) > 0){ ## initiate third attempt
      ## third attempt by sp0 and lv_d, regardless of bec
      workstrata <- unique(strata_failed[,.(SP0, LV_D)], by = c("SP0", "LV_D"))
      workstrata_failed <- workstrata[0,]
      outputs_coeff <- NULL
      outputs_random <- NULL
      for(i in 1:nrow(workstrata)){
        indidataset <- all_trees_reg[SP0 == workstrata$SP0[i] &
                                       LV_D == workstrata$LV_D[i] &
                                       logwsv != -Inf]
        indidataset[, noobs := length(SP0), by = "SAMP_POINT"]
        indidataset <- indidataset[noobs >= 3,]
        a <- try(mixmodel <- nlme::lme(logwsv~logba, random = ~logba|SAMP_POINT, data = indidataset),
                 TRUE)
        if(class(a) == "try-error" |length(unique(indidataset$SAMP_POINT)) < 5){
          workstrata_failed <- rbind(workstrata_failed, workstrata[i,])
        } else {
          r2s <- round(data.table(r.squaredGLMM(mixmodel)), 4)
          names(r2s) <- c("R2_Marginal", "R2_Condition")
          randomcoeff <- data.frame(mixmodel$coefficients$random)
          names(randomcoeff) <- c("INTERCEPT_RDM", "SLOPE_RDM")
          randomcoeff$SAMP_POINT <- row.names(randomcoeff)
          randomcoeff <- data.table(randomcoeff)
          outputs_coeff <- rbind(outputs_coeff,
                                 cbind(data.table(SP0 = workstrata$SP0[i],
                                            LV_D = workstrata$LV_D[i],
                                            NOOBS = nrow(indidataset),
                                            INTERCEPT = as.numeric(mixmodel$coefficients$fixed)[1],
                                            SLOPE = as.numeric(mixmodel$coefficients$fixed)[2],
                                            DATA_SRCE = "SP0+LV_D"),
                                       r2s))
          outputs_random <- rbind(outputs_random,
                                  randomcoeff[,.(SP0 = workstrata$SP0[i],
                                                 LV_D = workstrata$LV_D[i],
                                                 SAMP_POINT, INTERCEPT_RDM, SLOPE_RDM,
                                                 DATA_SRCE = "SP0+LV_D")])
          rm(mixmodel, randomcoeff)
        }
        rm(a)
      }
      rm(i, workstrata)
      coeff_succ3 <- merge(strata_failed, outputs_coeff,
                           by = c("SP0", "LV_D"))
      random_succ3 <- merge(strata_failed, outputs_random,
                            by = c("SP0", "LV_D"),
                            allow.cartesian = TRUE)
      coeff_succ <- rbindlist(list(coeff_succ, coeff_succ3), fill = TRUE)
      random_succ <- rbindlist(list(random_succ, random_succ3), fill = TRUE)
      rm(coeff_succ3, random_succ3, outputs_coeff, outputs_random)
      strata_failed <- merge(workstrata_failed, strata_failed,
                             by = c("SP0", "LV_D"))

      if(nrow(strata_failed) > 0){ # initiate last attempt
        ## last attempts using species type * bec * lv_d
        workstrata <- unique(strata_failed[,.(SP_TYPE, LV_D)],
                             by = c("SP_TYPE", "LV_D"))
        workstrata_failed <- workstrata[0,]

        outputs_coeff <- NULL
        outputs_random <- NULL
        for(i in 1:nrow(workstrata)){
          indidataset <- all_trees_reg[SP_TYPE == workstrata$SP_TYPE[i] &
                                         LV_D == workstrata$LV_D[i] &
                                         logwsv != -Inf]
          indidataset[, noobs := length(SP0), by = "SAMP_POINT"]
          indidataset <- indidataset[noobs >= 3,]
          a <- try(mixmodel <- nlme::lme(logwsv~logba, random = ~logba|SAMP_POINT, data = indidataset),
                   TRUE)
          if(class(a) == "try-error" |length(unique(indidataset$SAMP_POINT)) < 5){
            workstrata_failed <- rbind(workstrata_failed, workstrata[i,])
          } else {
            r2s <- round(data.table(r.squaredGLMM(mixmodel)), 4)
            names(r2s) <- c("R2_Marginal", "R2_Condition")
            randomcoeff <- data.frame(mixmodel$coefficients$random)
            names(randomcoeff) <- c("INTERCEPT_RDM", "SLOPE_RDM")
            randomcoeff$SAMP_POINT <- row.names(randomcoeff)
            randomcoeff <- data.table(randomcoeff)
            outputs_coeff <- rbind(outputs_coeff,
                                   cbind(data.table(SP_TYPE = workstrata$SP_TYPE[i],
                                              LV_D = workstrata$LV_D[i],
                                              NOOBS = nrow(indidataset),
                                              INTERCEPT = as.numeric(mixmodel$coefficients$fixed)[1],
                                              SLOPE = as.numeric(mixmodel$coefficients$fixed)[2],
                                              DATA_SRCE = "SP_TYPE+LV_D"),
                                         r2s))
            outputs_random <- rbind(outputs_random,
                                    randomcoeff[,.(SP_TYPE = workstrata$SP_TYPE[i],
                                                   LV_D = workstrata$LV_D[i],
                                                   SAMP_POINT, INTERCEPT_RDM, SLOPE_RDM,
                                                   DATA_SRCE = "SP_TYPE+LV_D")])
            rm(mixmodel, randomcoeff)
          }
          rm(a)
        }
        rm(i, workstrata)
        coeff_succ4 <- merge(strata_failed, outputs_coeff,
                             by = c("SP_TYPE", "LV_D"))
        random_succ4 <- merge(strata_failed, outputs_random,
                              by = c("SP_TYPE", "LV_D"),
                              allow.cartesian = TRUE)
        coeff_succ <- rbindlist(list(coeff_succ, coeff_succ4), fill = TRUE)
        random_succ <- rbindlist(list(random_succ, random_succ4), fill = TRUE)
        rm(coeff_succ4, random_succ4, outputs_coeff, outputs_random)
        allstrata_failed <- merge(workstrata_failed, strata_failed,
                                  by = c("SP_TYPE", "LV_D"))
      } # end of forth attempt
    } # end of third
  } # end of second

  if(nrow(strata_missing) > 0){ # for missing species, using all the species for a given lv_d status

    workstrata <- unique(strata_missing, by = c("BGC_ZONE", "LV_D"))
    workstrata_failed <- workstrata[0,]

    outputs_coeff <- NULL
    outputs_random <- NULL
    for(i in 1:nrow(workstrata)){
      indidataset <- all_trees_reg[BGC_ZONE == workstrata$BGC_ZONE[i] &
                                     LV_D == workstrata$LV_D[i] &
                                     logwsv != -Inf]
      indidataset[, noobs := length(SP0), by = "SAMP_POINT"]
      indidataset <- indidataset[noobs >= 3,]
      a <- try(mixmodel <- nlme::lme(logwsv~logba, random = ~logba|SAMP_POINT, data = indidataset),
               TRUE)
      if(class(a) == "try-error" |length(unique(indidataset$SAMP_POINT)) < 5){
        workstrata_failed <- rbind(workstrata_failed, workstrata[i,])
      } else {
        r2s <- round(data.table(r.squaredGLMM(mixmodel)), 4)
        names(r2s) <- c("R2_Marginal", "R2_Condition")
        randomcoeff <- data.frame(mixmodel$coefficients$random)
        names(randomcoeff) <- c("INTERCEPT_RDM", "SLOPE_RDM")
        randomcoeff$SAMP_POINT <- row.names(randomcoeff)
        randomcoeff <- data.table(randomcoeff)
        outputs_coeff <- rbind(outputs_coeff,
                               cbind(data.table(BGC_ZONE = workstrata$BGC_ZONE[i],
                                          LV_D = workstrata$LV_D[i],
                                          NOOBS = nrow(indidataset),
                                          INTERCEPT = as.numeric(mixmodel$coefficients$fixed)[1],
                                          SLOPE = as.numeric(mixmodel$coefficients$fixed)[2],
                                          DATA_SRCE = "BEC+LV_D"),
                                     r2s))
        outputs_random <- rbind(outputs_random,
                                randomcoeff[,.(BGC_ZONE = workstrata$BGC_ZONE[i],
                                               LV_D = workstrata$LV_D[i],
                                               SAMP_POINT, INTERCEPT_RDM, SLOPE_RDM,
                                               DATA_SRCE = "BEC+LV_D")])
        rm(mixmodel, randomcoeff)
      }
      rm(a)
    }
    rm(i, workstrata)
    coeff_succ5 <- merge(strata_missing, outputs_coeff,
                         by = c("BGC_ZONE", "LV_D"))
    random_succ5 <- merge(strata_missing, outputs_random,
                          by = c("BGC_ZONE", "LV_D"),
                          allow.cartesian = TRUE)
    coeff_succ <- rbindlist(list(coeff_succ, coeff_succ5), fill = TRUE)
    random_succ <- rbindlist(list(random_succ, random_succ5), fill = TRUE)
    rm(coeff_succ5, random_succ5, outputs_coeff, outputs_random)
    strata_failed_last2 <- merge(strata_missing, strata_failed,
                                 by = c("SP_TYPE", "LV_D"))
    if(exists("allstrata_failed")){
      allstrata_failed <- rbind(allstrata_failed, strata_failed_last2)
    } else {
      allstrata_failed <- strata_failed_last2
    }
  }

  if(exists("allstrata_failed")){
    if(nrow(allstrata_failed) > 0){
      allstrata_failed[, texts := paste0(BGC_ZONE, " + ", SP0, " + ", LV_D, "\n")]
      warning("The coefficients can not be successfully derived for below strata: \n", allstrata_failed$texts)
    }
  }

  return(list(fixedcoeff = coeff_succ,
              randomcoeff = random_succ))
}
