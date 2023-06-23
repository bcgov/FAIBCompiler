#' Estimate height using height-DBH curves
#'
#' @description Estimate height using height-DBH curves by bec subzone and species
#'
#' @param beczone character, BEC zone.
#'
#' @param subzone character, BEC subzone.
#'
#' @param species character, Species.
#'
#' @param DBH numeric, Diameter at breast height.
#'
#' @param heightModels data.table, Specifies the best model and coefficients by each
#'                                 BEC subzone and species.
#'
#' @return projected total height
#'
#' @importFrom data.table ':='
#' @importFrom dplyr left_join
#' @importFrom fpCompare %>>% %<<% %==% %!=% %<=% %>=%
#' @importFrom FAIBBase heightEstimateForBTOP_D heightEstimateForBTOP_H treeVolCalculator
#'
#'
#' @export
#' @docType methods
#' @rdname heightEstimate_byHeightModel
#'
#' @author Yong Luo
heightEstimate_byHeightModel<- function(beczone, subzone, species,
                                        DBH, heightModels){
  worktable <- data.table(tempID = 1:length(beczone),
                          BEC_ZONE = beczone,
                          BEC_SBZ = subzone,
                          SPECIES = species,
                          DBH = DBH)

  specieslookup <- lookup_species()

  specieslookup <- unique(specieslookup[,.(SPECIES, SP0, SP_TYPE)])
  worktable <- merge(worktable, specieslookup,
                     by = "SPECIES", all.x = TRUE)

  worktable <- merge(worktable,
                     heightModels,
                     by = c("BEC_ZONE", "BEC_SBZ", "SPECIES",
                            "SP0", "SP_TYPE"),
                     all.x = TRUE)

  worktable_pass <- worktable[!is.na(bestModel),]
  worktable_fail <- worktable[is.na(bestModel),
                              .(tempID, SPECIES, SP0, SP_TYPE, DBH)]
  ## find in the same bec zone and with same species
  heightModels_species <- unique(heightModels[GroupMethod == "species",],
                                 by = c("SPECIES"))

  worktable_fail <- merge(worktable_fail,
                          heightModels_species,
                          by = c("SPECIES", "SP0", "SP_TYPE"),
                          all.x = TRUE)
  worktable_pass <- rbindlist(list(worktable_pass,
                                   worktable_fail[!is.na(GroupMethod),]),
                              fill = TRUE)
  worktable_fail <- worktable_fail[is.na(GroupMethod),
                                   .(tempID, SP0, SP_TYPE, DBH)]
  ## find in same sp0
  heightModels_sp0 <- unique(heightModels[GroupMethod == "sp0",],
                             by = c("SP0"))
  worktable_fail <- merge(worktable_fail,
                          heightModels_sp0,
                          by = c("SP0", "SP_TYPE"),
                          all.x = TRUE)
  worktable_pass <- rbindlist(list(worktable_pass,
                                   worktable_fail[!is.na(GroupMethod),]),
                              fill = TRUE)

  ## by species type
  worktable_fail <- worktable_fail[is.na(GroupMethod),
                                   .(tempID, SP_TYPE, DBH)]
  heightModels_sp_type <- unique(heightModels[GroupMethod == "species type",],
                                 by = "SP_TYPE")
  worktable_fail <- merge(worktable_fail,
                          heightModels_sp_type,
                          by = "SP_TYPE",
                          all.x = TRUE)
  worktable_pass <- rbindlist(list(worktable_pass,
                                   worktable_fail),
                              fill = TRUE)

  worktable_pass[bestModel == "Chapman-Richards",
                 ':='(height_projected = b1 * (1 - exp(-b2*DBH))^b3)]
  worktable_pass[bestModel == "Korf",
                 ':='(height_projected = 1.3 + b1 * exp((-b2)*(DBH^(-b3))))]
  worktable_pass[bestModel == "Logistic",
                 ':='(height_projected = 1.3 + b1 /(1 + b2*exp(-b3*DBH)))]
  worktable_pass[bestModel == "Weibull",
                 ':='(height_projected = 1.3 + b1 * (1 - exp(-b2*(DBH^b3))))]

  worktable_pass <- worktable_pass[order(tempID),]
  return(worktable_pass$height_projected)
}




#' Estimate height using mixed effect height-DBH curves
#'
#' @description Estimate height using height-DBH curves by bec and species
#'
#' @param BEC character, BEC zone.
#'
#' @param siteID character, site identifier
#'
#' @param species character, Species.
#'
#' @param DBH numeric, Diameter at breast height (cm).
#'
#' @param fixedEffects data.table, Fixed effect of the best model by each
#'                                 BEC and sp0.
#' @param randomEffects data.table, Random effects of the best model by each
#'                                 BEC, sp0 and site identifier.
#'
#' @return projected total height
#'
#' @importFrom data.table ':='
#' @importFrom dplyr left_join
#' @importFrom fpCompare %>>% %<<% %==% %!=% %<=% %>=%
#'
#' @export
#' @docType methods
#' @rdname heightEstimate_mixedEffect
#'
#' @author Yong Luo
heightEstimate_mixedEffect <- function(BEC, siteID, sp0,
                                        DBH, fixedEffects,
                                       randomEffects){
  worktable <- data.table(uniID = 1:length(DBH),
                          SITE_IDENTIFIER = siteID,
                          BEC = BEC,
                          SP0 = sp0,
                          DBH = DBH)
  worktable <- merge(worktable,
                     fixedEffects[,.(SP0, BEC, model,
                                     a_fixed = a,
                                     b_fixed = b,
                                     c_fixed = c)],
                     by = c("BEC", "SP0"),
                     all.x = TRUE)
  worktable <- merge(worktable,
                     randomEffects[,.(SITE_IDENTIFIER = as.numeric(SITE_IDENTIFIER),
                                      SP0, BEC,
                                      model_random = model,
                                      a_random = a,
                                      b_random = b)],
                     by = c("BEC", "SP0", "SITE_IDENTIFIER"),
                     all.x = TRUE)
  if(nrow(worktable[model != model_random,]) > 0){
    stop("Models for fixed effect and random effect do not match.")
  }
  worktable[!is.na(model) & is.na(a_random),
           ':='(a_random = 0,
                b_random = 0)]
  worktable[, ':='(a = a_fixed+a_random,
                  b = b_fixed+b_random,
                  c = c_fixed)]
  worktable[model == "logistic",
           HT_predicted := 1.3 + a/(1 + b*exp(-c*DBH))]
  worktable[model == "korf",
           HT_predicted := 1.3 + a*exp(-b*(DBH^-c))]
  worktable[model == "richards",
           HT_predicted := 1.3 + a*(((1-exp(-b*DBH)))^c)]
  worktable[model == "weibull",
           HT_predicted := 1.3 + a*(1-exp(-b*(DBH^c)))]
  worktable[model == "weibull",
           HT_predicted := 1.3 + a*(1-exp(-b*(DBH^c)))]
  worktable[model == "curtis",
           HT_predicted := 1.3 + a*((DBH/(1 + DBH))^b)]
  worktable[model == "naslund",
           HT_predicted := 1.3 + (DBH^2)/((a + b*DBH)^2)]
  worktable <- worktable[order(uniID),]
  return(worktable$HT_predicted)
}



#' Estimate height using mixed effect height-DBH curves using nlme
#'
#' @description Estimate height using height-DBH curves by species
#'
#' @param siteID character, site identifier
#'
#' @param species character, Species.
#'
#' @param DBH numeric, Diameter at breast height (cm).
#'
#' @param fixedEffects data.table, Fixed effect of the best model by each
#'                                 sp0.
#' @param randomEffects_site data.table, Random effects of the best model at site level.
#' @param randomEffects_tree data.table, Random effects of the best model at tree level.
#'
#' @return projected total height
#'
#' @importFrom data.table ':='
#' @importFrom dplyr left_join
#' @importFrom fpCompare %>>% %<<% %==% %!=% %<=% %>=%
#'
#' @export
#' @docType methods
#' @rdname heightEstimate_mixedEffect_nlme
#'
#' @author Yong Luo
heightEstimate_mixedEffect_nlme <- function(siteID, unitreeid, sp0,
                                        DBH, fixedEffects,
                                       randomEffects_site,
                                       randomEffects_tree){
  worktable <- data.table(uniID = 1:length(DBH),
                          SITE_IDENTIFIER = siteID,
                          unitreeid,
                          SP0 = sp0,
                          DBH = DBH)
  worktable <- merge(worktable,
                     fixedEffects[,.(SP0, model,
                                     a_fixed = a,
                                     b_fixed = b,
                                     c_fixed = c)],
                     by = c("SP0"),
                     all.x = TRUE)
  worktable <- merge(worktable,
                     randomEffects_site[,.(SITE_IDENTIFIER = as.numeric(SITE_IDENTIFIER),
                                      SP0,
                                      model_random_site = model,
                                      a_random_site = a)],
                     by = c("SP0", "SITE_IDENTIFIER"),
                     all.x = TRUE)

  worktable <- merge(worktable,
                     randomEffects_tree[,.(unitreeid,
                                      SP0,
                                      model_random_tree = model,
                                      a_random_tree = a)],
                     by = c("SP0", "unitreeid"),
                     all.x = TRUE)

  if(nrow(worktable[model != model_random_site,]) > 0){
    stop("Models for fixed effect and random effect do not match.")
  }
  worktable[!is.na(model) & is.na(a_random_site),
           ':='(a_random_site = 0)]
  worktable[!is.na(model) & is.na(a_random_tree),
           ':='(a_random_tree = 0)]
  worktable[, ':='(a = a_fixed + a_random_site + a_random_tree,
                  b = b_fixed,
                  c = c_fixed)]
  worktable[model == "logistic",
           HT_predicted := 1.3 + a/(1 + b*exp(-c*DBH))]
  worktable[model == "korf",
           HT_predicted := 1.3 + a*exp(-b*(DBH^-c))]
  worktable[model == "richards",
           HT_predicted := 1.3 + a*(((1-exp(-b*DBH)))^c)]
  worktable[model == "weibull",
           HT_predicted := 1.3 + a*(1-exp(-b*(DBH^c)))]
  worktable[model == "weibull",
           HT_predicted := 1.3 + a*(1-exp(-b*(DBH^c)))]
  worktable[model == "curtis",
           HT_predicted := 1.3 + a*((DBH/(1 + DBH))^b)]
  worktable[model == "naslund",
           HT_predicted := 1.3 + (DBH^2)/((a + b*DBH)^2)]
  worktable <- worktable[order(uniID),]
  return(worktable$HT_predicted)
}

