#' Develop the mixed effect models between DBH and height by strata.
#'
#'
#' @description Develop the mixed effect model, and select the best models per strata.
#' @param compilationPath character, Specifies the path that stores all the data/processes.
#' @param coeffSavePath character, Specifies the path to save your outputs.
#' @param fityear numeric, Specifies the year of the fit.
#'
#' @return no item returned
#'
#' @importFrom data.table ':=' data.table melt
#' @importFrom dplyr '%>%'
#' @importFrom lmfor fithd startHDlogistic startHDkorf startHDweibull startHDrichards startHDcurtis startHDnaslund
#' @importFrom nlme nlme nlmeControl
#'
#' @rdname DBH_Height_MEM
#' @author Yong Luo
DBH_Height_MEM <- function(compilationPath, coeffSavePath, fityear){
  treelist_psp <- readRDS(file.path(compilationPath, "compilation_PSP_sa", "treemeasurements.rds"))
  treelist_psp <- treelist_psp[!is.na(DIAMETER) & DIAMETER_MEASMT_HEIGHT == 1.3 &
                                 LENGTH > 1.3 & # all height and dbh are present
                                 TREE_EXTANT_CODE == "L" & # all alive trees
                                 is.na(HEIGHT_EDIT) &  # no modification for height
                                 is.na(DIAMETER_EDIT) & # no diameter modification
                                 BROKEN_TOP_IND == "N", # no broken top trees
                               .(CLSTR_ID, PLOT, TREE_NO = TREE_NUMBER,
                                 DBH = DIAMETER, HEIGHT = LENGTH,
                                 SPECIES, BEC_ZONE)]
  splookup <- lookup_species()
  treelist_psp <- merge(treelist_psp,
                        unique(splookup[,.(SPECIES, SP0, SP_TYPE)]),
                        by = "SPECIES",
                        all.x = TRUE)
  # treelist_all <- rbind(treelist_psp, treelist_nonpsp)
  treelist_all <- data.table::copy(treelist_psp)
  treelist_all[, SITE_IDENTIFIER := as.numeric(substr(CLSTR_ID, 1, 7))]

  allcombo <- unique(treelist_all[,.(SPECIES, SP0, SP_TYPE)])

  fitdata <- data.table::copy(treelist_all)

  fitdata_smry <- fitdata[,.(Nobs = length(DBH)),
                          by = c("SPECIES", "SP0", "SP_TYPE")]
  allcombo <- merge(allcombo,
                    fitdata_smry,
                    by = c("SPECIES", "SP0", "SP_TYPE"))
  allcombo[Nobs >= 1000,
           datagroup := "species"]

  fitdata_smry2 <- fitdata[,.(Nobs2 = length(DBH)),
                           by = "SP0"]
  allcombo <- merge(allcombo,
                    fitdata_smry2,
                    by = "SP0",
                    all.x = TRUE)
  allcombo[is.na(datagroup) & Nobs2 >= 1000,
           ':='(datagroup = "sp0",
                Nobs = Nobs2)]


  fitdata_smry4 <- fitdata[,.(Nobs4 = length(DBH)),
                           by = c("SP_TYPE")]
  allcombo <- merge(allcombo,
                    fitdata_smry4,
                    by = c("SP_TYPE"),
                    all.x = TRUE)
  allcombo[is.na(datagroup),
           ':='(datagroup = "sp_type",
                Nobs = Nobs4)]

  allcombo[,':='(Nobs2 = NULL,
                 Nobs4 = NULL)]
  fitdata[, unitreeid := paste0(SITE_IDENTIFIER, "-", PLOT, "-", TREE_NO)]
  # saveRDS(fitdata, file.path(coeffSavePath, paste0("fitdata_ht_dbh", fityear, ".rds")))
  # allcombo_org <- data.table::copy(allcombo)
  # allcombo <- data.table::copy(allcombo_org)[1:2,]
  for(indidatagroup in c("species", "sp0", "sp_type")){
    indifitdata <- allcombo[datagroup == indidatagroup]
    if(nrow(indifitdata) > 0){
      # modelforms = c("logistic", "korf", "weibull", "richards")
      indiresults <- fithdmodel(groupType = indidatagroup,
                                masterRow = indifitdata,
                                fitData = fitdata,
                                modelforms = c("naslund", "curtis"))
      if(nrow(indiresults$strata_failed) > 0){
        if(indidatagroup == "species"){
          allcombo[SPECIES %in% indiresults$strata_failed$SPECIES,
                   datagroup := "sp0"]
          indiresults$strata_failed <- NULL
        } else if(indidatagroup == "sp0"){
          allcombo[SPECIES %in% indiresults$strata_failed$SPECIES,
                   datagroup := "sp_type"]
          indiresults$strata_failed <- NULL
        }
      }
      if(indidatagroup == "species"){
        allresults <- indiresults
      } else {
        allresults$fixed_best <- rbind(allresults$fixed_best,
                                       indiresults$fixed_best)
        allresults$random_best_site <- rbind(allresults$random_best_site,
                                             indiresults$random_best_site)
        allresults$random_best_tree <- rbind(allresults$random_best_tree,
                                             indiresults$random_best_tree)

        allresults$strata_failed <- rbind(allresults$strata_failed,
                                          indiresults$strata_failed)
      }
    }
  }
  rm(indidatagroup, indifitdata)
  saveRDS(allresults, file.path(coeffSavePath,
                                paste0("bestmodels_ht_dbh_bysp", fityear, ".rds")))
}



fithdmodel <- function(groupType, masterRow,
                       fitData, modelforms){
  if(groupType == "sp0"){
    uniqueRow <- unique(masterRow[,.(SP0)])
  } else if(groupType == "species"){
    uniqueRow <- unique(masterRow[,.(SPECIES)])
  } else if (groupType == "bec+sp0"){
    uniqueRow <- unique(masterRow[,.(BEC, SP0)])
  } else if (groupType == "bec+sp_type"){
    uniqueRow <- unique(masterRow[,.(BEC, SP_TYPE)])
  } else if (groupType == "sp_type"){
    uniqueRow <- unique(masterRow[,.(SP_TYPE)])
  }
  masterRow_len <- nrow(masterRow)
  masterRow <- do.call("rbind", replicate(length(modelforms),
                                          masterRow, simplify = FALSE))
  masterRow[, model := sort(rep(modelforms, masterRow_len))]
  allfixcoef <- NULL
  allrandomcoef_site <- NULL
  allrandomcoef_tree <- NULL
  for (i in 1:nrow(uniqueRow)) {
    if(groupType == "sp0"){
      fitdata_ind <- fitData[SP0 == uniqueRow$SP0[i],]
    } else if (groupType == "species"){
      fitdata_ind <- fitData[SPECIES == uniqueRow$SPECIES[i],]
    } else if (groupType == "bec+sp0"){
      fitdata_ind <- fitData[BEC == uniqueRow$BEC[i] &
                               SP0 == uniqueRow$SP0[i],]
    } else if (groupType == "bec+sp_type"){
      fitdata_ind <- fitData[BEC == uniqueRow$BEC[i] &
                               SP_TYPE == uniqueRow$SP_TYPE[i],]
    } else if (groupType == "sp_type"){
      fitdata_ind <- fitData[SP_TYPE == uniqueRow$SP_TYPE[i],]
    }
    R2_bottom <- sum((fitdata_ind$HEIGHT-mean(fitdata_ind$HEIGHT))^2)
    for (indmodelform in modelforms) {
      if(indmodelform == "logistic"){
        start_metrix <- startHDlogistic(fitdata_ind$DBH, fitdata_ind$HEIGHT)
        try_model <- try(outputs_m <- nlme::nlme(HEIGHT ~ 1.3 + a/(1 + b*exp(-c*DBH)),
                                                 data = fitdata_ind,
                                                 fixed = a + b + c~1,
                                                 start = start_metrix,
                                                 random =  a ~1|SITE_IDENTIFIER/unitreeid,
                                                 control = nlmeControl(minScale=1e-100, maxIter=1000)),
                         silent = TRUE)
      } else if (indmodelform == "korf"){
        start_metrix <- startHDkorf(fitdata_ind$DBH, fitdata_ind$HEIGHT)
        try_model <- try(outputs_m <- nlme::nlme(HEIGHT ~ 1.3 + a*exp(-b*(DBH^-c)),
                                                 data = fitdata_ind,
                                                 fixed = a + b + c~1,
                                                 start = start_metrix,
                                                 random =  a ~1|SITE_IDENTIFIER/unitreeid,
                                                 control = nlmeControl(minScale=1e-100, maxIter=1000)),
                         silent = TRUE)
      } else if (indmodelform == "richards"){
        start_metrix <- startHDrichards(fitdata_ind$DBH, fitdata_ind$HEIGHT)
        try_model <- try(outputs_m <- nlme::nlme(HEIGHT ~ 1.3 + a*(((1-exp(-b*DBH)))^c),
                                                 data = fitdata_ind,
                                                 fixed = a + b + c~1,
                                                 start = start_metrix,
                                                 random =  a ~1|SITE_IDENTIFIER/unitreeid,
                                                 control = nlmeControl(minScale=1e-100, maxIter=1000)),
                         silent = TRUE)
      } else if(indmodelform == "weibull"){
        start_metrix <- startHDweibull(fitdata_ind$DBH, fitdata_ind$HEIGHT)
        try_model <- try(outputs_m <- nlme::nlme(HEIGHT ~ 1.3 + a*(1-exp(-b*(DBH^c))),
                                                 data = fitdata_ind,
                                                 fixed = a + b + c~1,
                                                 start = start_metrix,
                                                 random =  a ~1|SITE_IDENTIFIER/unitreeid,
                                                 control = nlmeControl(minScale=1e-100, maxIter=1000)),
                         silent = TRUE)
      } else if(indmodelform == "naslund"){
        start_metrix <- startHDnaslund(fitdata_ind$DBH, fitdata_ind$HEIGHT)
        try_model <- try(outputs_m <- nlme::nlme(HEIGHT ~ 1.3 + (DBH^2)/((a + b*DBH)^2),
                                                 data = fitdata_ind,
                                                 fixed = a + b ~ 1,
                                                 start = start_metrix,
                                                 random =  a ~ 1|SITE_IDENTIFIER/unitreeid,
                                                 control = nlmeControl(minScale=1e-100, maxIter=1000)),
                         silent = TRUE)
      } else if(indmodelform == "curtis"){
        start_metrix <- startHDcurtis(fitdata_ind$DBH, fitdata_ind$HEIGHT)
        try_model <- try(outputs_m <- nlme::nlme(HEIGHT ~ 1.3 + a*((DBH/(1 + DBH))^b),
                                                 data = fitdata_ind,
                                                 fixed = a + b ~ 1,
                                                 start = start_metrix,
                                                 random =  a ~ 1|SITE_IDENTIFIER/unitreeid,
                                                 control = nlmeControl(minScale=1e-100, maxIter=1000)),
                         silent = TRUE)
      }
      # try_model <- try(outputs_m <- lmfor::fithd(d = fitdata_ind$DBH,
      #                                            h = fitdata_ind$HEIGHT,
      #                                            plot = fitdata_ind$SITE_IDENTIFIER,
      #                                            modelName = indmodelform,
      #                                            start = start_metrix,
      #                                            nranp = 2),
      #                  silent = TRUE)
      if(class(try_model)[1] == "try-error"){
        if(groupType == "sp0"){
          m_coef_fixed <- data.table(SP0 = uniqueRow$SP0[i],
                                     model = indmodelform,
                                     r2_marg = 0,
                                     r2_cond = 0,
                                     a = 0,
                                     b = 0,
                                     c = 0)
        } else if (groupType == "species"){
          m_coef_fixed <- data.table(SPECIES = uniqueRow$SPECIES[i],
                                     model = indmodelform,
                                     r2_marg = 0,
                                     r2_cond = 0,
                                     a = 0,
                                     b = 0,
                                     c = 0)
        } else if (groupType == "sp_type"){
          m_coef_fixed <- data.table(SP_TYPE = uniqueRow$SP_TYPE[i],
                                     model = indmodelform,
                                     r2_marg = 0,
                                     r2_cond = 0,
                                     a = 0,
                                     b = 0,
                                     c = 0)
        }
        m_coef_random_siteid <- NULL
        m_coef_random_treeid <- NULL
      } else {
        r2_table <- data.table(outputs_m$residuals)
        r2_table <- r2_table[,.(r2_marg = 1-sum(fixed^2)/R2_bottom,
                                r2_cond = 1-sum(unitreeid^2)/R2_bottom)]

        m_coef_random_siteid <- data.frame(outputs_m$coefficients$random$SITE_IDENTIFIER)
        m_coef_random_siteid$SITE_IDENTIFIER <- row.names(m_coef_random_siteid)
        m_coef_random_siteid <- data.table(m_coef_random_siteid)

        m_coef_random_treeid <- data.frame(outputs_m$coefficients$random$unitreeid)
        m_coef_random_treeid$unitreeid <- row.names(m_coef_random_treeid)
        m_coef_random_treeid <- data.table(m_coef_random_treeid)
        m_coef_random_treeid[, totellen := nchar(unitreeid)]
        m_coef_random_treeid[, unitreeid := substr(unitreeid, 9, totellen)]
        m_coef_random_treeid[, totellen := NULL]


        if(groupType == "sp0"){
          if(indmodelform %in% c("naslund", "curtis")){
            m_coef_fixed <- data.table(SP0 = uniqueRow$SP0[i],
                                       model = indmodelform,
                                       r2_marg = r2_table$r2_marg,
                                       r2_cond = r2_table$r2_cond,
                                       a = outputs_m$coefficients$fixed[1],
                                       b = outputs_m$coefficients$fixed[2],
                                       c = NA)
          } else {
            m_coef_fixed <- data.table(SP0 = uniqueRow$SP0[i],
                                       model = indmodelform,
                                       r2_marg = r2_table$r2_marg,
                                       r2_cond = r2_table$r2_cond,
                                       a = outputs_m$coefficients$fixed[1],
                                       b = outputs_m$coefficients$fixed[2],
                                       c = outputs_m$coefficients$fixed[3])
          }
          m_coef_random_siteid[, ':='(model = indmodelform,
                                      SP0 = uniqueRow$SP0[i])]
          m_coef_random_treeid[, ':='(model = indmodelform,
                                      SP0 = uniqueRow$SP0[i])]
        } else if (groupType == "species"){
          if(indmodelform %in% c("naslund", "curtis")){
            m_coef_fixed <- data.table(SPECIES = uniqueRow$SPECIES[i],
                                       model = indmodelform,
                                       r2_marg = r2_table$r2_marg,
                                       r2_cond = r2_table$r2_cond,
                                       a = outputs_m$coefficients$fixed[1],
                                       b = outputs_m$coefficients$fixed[2],
                                       c = NA)
          } else {
            m_coef_fixed <- data.table(SPECIES = uniqueRow$SPECIES[i],
                                       model = indmodelform,
                                       r2_marg = r2_table$r2_marg,
                                       r2_cond = r2_table$r2_cond,
                                       a = outputs_m$coefficients$fixed[1],
                                       b = outputs_m$coefficients$fixed[2],
                                       c = outputs_m$coefficients$fixed[3])
          }
          m_coef_random_siteid[, ':='(model = indmodelform,
                                      SPECIES = uniqueRow$SPECIES[i])]
          m_coef_random_treeid[, ':='(model = indmodelform,
                                      SPECIES = uniqueRow$SPECIES[i])]
        } else if (groupType == "bec+sp0"){
          if(indmodelform %in% c("naslund", "curtis")){
            m_coef_fixed <- data.table(BEC = uniqueRow$BEC[i],
                                       SP0 = uniqueRow$SP0[i],
                                       model = indmodelform,
                                       r2_marg = r2_table$r2_marg,
                                       r2_cond = r2_table$r2_cond,
                                       a = outputs_m$coefficients$fixed[1],
                                       b = outputs_m$coefficients$fixed[2],
                                       c = NA)
          } else {
            m_coef_fixed <- data.table(BEC = uniqueRow$BEC[i],
                                       SP0 = uniqueRow$SP0[i],
                                       model = indmodelform,
                                       r2_marg = r2_table$r2_marg,
                                       r2_cond = r2_table$r2_cond,
                                       a = outputs_m$coefficients$fixed[1],
                                       b = outputs_m$coefficients$fixed[2],
                                       c = outputs_m$coefficients$fixed[3])
          }
          m_coef_random_siteid[, ':='(model = indmodelform,
                                      BEC = uniqueRow$BEC[i],
                                      SP0 = uniqueRow$SP0[i])]
          m_coef_random_treeid[, ':='(model = indmodelform,
                                      BEC = uniqueRow$BEC[i],
                                      SP0 = uniqueRow$SP0[i])]

        } else if (groupType == "bec+sp_type"){
          if(indmodelform %in% c("naslund", "curtis")){
            m_coef_fixed <- data.table(BEC = uniqueRow$BEC[i],
                                       SP_TYPE = uniqueRow$SP_TYPE[i],
                                       model = indmodelform,
                                       r2_marg = r2_table$r2_marg,
                                       r2_cond = r2_table$r2_cond,
                                       a = outputs_m$coefficients$fixed[1],
                                       b = outputs_m$coefficients$fixed[2],
                                       c = NA)
          } else {
            m_coef_fixed <- data.table(BEC = uniqueRow$BEC[i],
                                       SP_TYPE = uniqueRow$SP_TYPE[i],
                                       model = indmodelform,
                                       r2_marg = r2_table$r2_marg,
                                       r2_cond = r2_table$r2_cond,
                                       a = outputs_m$coefficients$fixed[1],
                                       b = outputs_m$coefficients$fixed[2],
                                       c = outputs_m$coefficients$fixed[3])
          }
          m_coef_random_siteid[, ':='(model = indmodelform,
                                      BEC = uniqueRow$BEC[i],
                                      SP_TYPE = uniqueRow$SP_TYPE[i])]
          m_coef_random_treeid[, ':='(model = indmodelform,
                                      BEC = uniqueRow$BEC[i],
                                      SP_TYPE = uniqueRow$SP_TYPE[i])]

        } else if (groupType == "sp_type"){
          if(indmodelform %in% c("naslund", "curtis")){
            m_coef_fixed <- data.table(SP_TYPE = uniqueRow$SP_TYPE[i],
                                       model = indmodelform,
                                       r2_marg = r2_table$r2_marg,
                                       r2_cond = r2_table$r2_cond,
                                       a = outputs_m$coefficients$fixed[1],
                                       b = outputs_m$coefficients$fixed[2],
                                       c = NA)
          } else {
            m_coef_fixed <- data.table(SP_TYPE = uniqueRow$SP_TYPE[i],
                                       model = indmodelform,
                                       r2_marg = r2_table$r2_marg,
                                       r2_cond = r2_table$r2_cond,
                                       a = outputs_m$coefficients$fixed[1],
                                       b = outputs_m$coefficients$fixed[2],
                                       c = outputs_m$coefficients$fixed[3])
          }
          m_coef_random_siteid[, ':='(model = indmodelform,
                                      SP_TYPE = uniqueRow$SP_TYPE[i])]
          m_coef_random_treeid[, ':='(model = indmodelform,
                                      SP_TYPE = uniqueRow$SP_TYPE[i])]
        }
        rm(outputs_m)
      }
      allfixcoef <- rbind(allfixcoef, m_coef_fixed)
      allrandomcoef_site <- rbind(allrandomcoef_site, m_coef_random_siteid)
      allrandomcoef_tree <- rbind(allrandomcoef_tree, m_coef_random_treeid)
      rm(m_coef_random_siteid, m_coef_random_treeid, m_coef_fixed)
    }
  }

  if(groupType == "sp0"){
    masterRow <- merge(masterRow,
                       allfixcoef,
                       by = c("SP0", "model"),
                       all.x = TRUE)
    masterRow[, ':='(totalR2 = sum(r2_marg),
                     maxR2 = max(r2_cond)),
              by = c("SPECIES")]
    fixed_best <- masterRow[totalR2 > 0 &
                              maxR2 == r2_cond,]
    random_best_site <- merge(fixed_best[,.(SPECIES, SP0, SP_TYPE, model)],
                              allrandomcoef_site,
                              by = c("SP0", "model"),
                              allow.cartesian=TRUE)
    random_best_tree <- merge(fixed_best[,.(SPECIES, SP0, SP_TYPE, model)],
                              allrandomcoef_tree,
                              by = c("SP0", "model"),
                              allow.cartesian=TRUE)
    masterRow_fail <- unique(masterRow[totalR2 == 0,
                                       .(SPECIES, SP0, SP_TYPE, Nobs, datagroup)])
  } else if (groupType == "species"){
    masterRow <- merge(masterRow,
                       allfixcoef,
                       by = c("SPECIES", "model"),
                       all.x = TRUE)
    masterRow[, ':='(totalR2 = sum(r2_marg),
                     maxR2 = max(r2_cond)),
              by = c("SPECIES")]
    fixed_best <- masterRow[totalR2 > 0 &
                              maxR2 == r2_cond,]
    random_best_site <- merge(fixed_best[,.(SPECIES, SP0, SP_TYPE, model)],
                              allrandomcoef_site,
                              by = c("SPECIES", "model"),
                              allow.cartesian=TRUE)
    random_best_tree <- merge(fixed_best[,.(SPECIES, SP0, SP_TYPE, model)],
                              allrandomcoef_tree,
                              by = c("SPECIES", "model"),
                              allow.cartesian=TRUE)
    masterRow_fail <- unique(masterRow[totalR2 == 0,
                                       .(SPECIES, SP0, SP_TYPE, Nobs, datagroup)])
  } else if (groupType == "sp_type"){
    masterRow <- merge(masterRow,
                       allfixcoef,
                       by = c("SP_TYPE", "model"),
                       all.x = TRUE)
    masterRow[, ':='(totalR2 = sum(r2_marg),
                     maxR2 = max(r2_cond)),
              by = c("SPECIES")]
    fixed_best <- masterRow[totalR2 > 0 &
                              maxR2 == r2_cond,]
    random_best_site <- merge(fixed_best[,.(SPECIES, SP0, SP_TYPE, model)],
                              allrandomcoef_site,
                              by = c("SP_TYPE", "model"),
                              allow.cartesian=TRUE)
    random_best_tree <- merge(fixed_best[,.(SPECIES, SP0, SP_TYPE, model)],
                              allrandomcoef_tree,
                              by = c("SP_TYPE", "model"),
                              allow.cartesian=TRUE)
    masterRow_fail <- unique(masterRow[totalR2 == 0,
                                       .(SPECIES, SP0, SP_TYPE, Nobs, datagroup)])
  }
  fixed_best[,':='(totalR2 = NULL,
                   maxR2 = NULL)]
  return(list(fixed_best = fixed_best,
              random_best_site = random_best_site,
              random_best_tree = random_best_tree,
              strata_failed = masterRow_fail))
}
