#' Develop the mixed effect models between DBH and height by strata.
#'
#'
#' @description Develop the mixed effect model, and select the best models per strata.
#' @param compilationPath character, Specifies the path that stores all the data/processes.
#' @param coeffSavePath character, Specifies the path to save your outputs.
#'
#' @return no item returned
#'
#' @importFrom data.table ':=' data.table melt
#' @importFrom dplyr '%>%'
#' @importFrom lmfor fithd startHDlogistic startHDkorf startHDweibull startHDrichards startHDcurtis startHDnaslund
#' @importFrom nlme nlme
#'
#' @rdname DBH_Height_MEM
#' @author Yong Luo
DBH_Height_MEM <- function(compilationPath, coeffSavePath){
  compilationPath_psp <- "D:/ISMC project/ISMC compiler/ISMC compiler G version/Archive_PSP_20230530/"

    coeffSavePath <- "D:/ISMC project/ISMC compiler/ISMC compiler G version/compilation_coeff/"
  treelist_psp <- readRDS(file.path(compilationPath_psp, "compilation_PSP_db", "treelist.rds"))
  treelist_psp <- treelist_psp[!is.na(HEIGHT) & is.na(BTOP) &
                                 HT_TOTAL_SOURCE == "Field measured",
                               .(CLSTR_ID, PLOT, TREE_NO, SPECIES,
                                 LV_D, DBH, HEIGHT, SP0, SP_TYPE)]
  compilationPath <- "D:/ISMC project/ISMC compiler/ISMC compiler G version/"
  treelist_nonpsp <- readRDS(file.path(compilationPath, "compilation_nonPSP_db", "treelist.rds"))
  treelist_nonpsp <- treelist_nonpsp[!is.na(HEIGHT) & is.na(BTOP) &
                                       HT_TOTAL_SOURCE == "Field measured",
                                     .(CLSTR_ID, PLOT, TREE_NO, SPECIES,
                                       LV_D, DBH, HEIGHT, SP0, SP_TYPE)]
  # treelist_all <- rbind(treelist_psp, treelist_nonpsp)
  treelist_all <- data.table::copy(treelist_psp)
  treelist_all[, SITE_IDENTIFIER := as.numeric(substr(CLSTR_ID, 1, 7))]

  samp_site_psp <- readRDS(file.path(compilationPath, "compilation_PSP_db",
                                     "sample_site_header.rds"))
  samp_site_nonpsp <- readRDS(file.path(compilationPath, "compilation_nonPSP_db",
                                        "sample_site_header.rds"))
  samp_site_all <- rbind(samp_site_nonpsp, samp_site_psp)
  treelist_all <- merge(treelist_all,
                        samp_site_all[,.(SITE_IDENTIFIER, BEC = BEC_ZONE, NO_MEAS)],
                        by = "SITE_IDENTIFIER",
                        all.x = TRUE)
  allcombo <- unique(treelist_all[,.(SPECIES, SP0, SP_TYPE)])

  ## select trees that had been measured multiple times
  treelist_all <- treelist_all[LV_D == "L",]
  treelist_all[, meas_time_tree := length(DBH),
               by = c("SITE_IDENTIFIER", "PLOT", "TREE_NO")]
  fitdata <- data.table::copy(treelist_all[meas_time_tree > 1,])
  fitdata <- data.table::copy(treelist_all)


  # treelist_all[, cls_len := nchar(CLSTR_ID)]
  # treelist_all[, VISIT_NO := ifelse(cls_len == 10, as.numeric(substr(CLSTR_ID, 10, 10)),
  #                                   ifelse(cls_len == 12, as.numeric(substr(CLSTR_ID, 12, 12)),
  #                                          as.numeric(substr(CLSTR_ID, 12, 13))))]
  # fitdata <- treelist_all[VISIT_NO == NO_MEAS]

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
  # fitdata_smry3 <- fitdata[,.(Nobs3 = length(DBH)),
  #                          by = c("BEC", "SP_TYPE")]
  # allcombo <- merge(allcombo,
  #                   fitdata_smry3,
  #                   by = c("BEC", "SP_TYPE"),
  #                   all.x = TRUE)
  # allcombo[is.na(datagroup) & Nobs3 >= 500,
  #          ':='(datagroup = "bec+sp_type",
  #               Nobs = Nobs3)]

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
  saveRDS(fitdata, file.path(coeffSavePath, "fitdata_ht_dbh.rds"))

  # allcombo_org <- data.table::copy(allcombo)
  # allcombo <- data.table::copy(allcombo_org)[1:2,]
  for(indidatagroup in c("species", "sp0", "sp_type")){
    indifitdata <- allcombo[datagroup == indidatagroup]
    # indifitdata <- indifitdata[1:2,]
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
  saveRDS(allresults, file.path(coeffSavePath, "besthdmodels_nlme_by_species.rds"))
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
    cat("fit model for", i, "of", nrow(uniqueRow),
        "using", indmodelform, "\n")
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
