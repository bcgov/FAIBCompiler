#' Calculate site index using site tools
#'
#' @description This function calculates site index based on bored age (\code{boredAge}), tree height (\code{height}),
#'              species (\code{species}) and region (\code{ICRegion}) using site tools program. This function
#'              is equivalent to sindex_httoage.sas.
#'
#' @param boredAge numeric, Age at bored height.
#' @param height numeric, Total tree height.
#' @param species character, Species code, must be consistent with the species code in site tools, which can be converted
#'                           from the original species code by using \code{\link{siteToolsSpeciesConvertor}}.
#' @param ICRegion character, Must be either \code{I} (interior) and \code{C} (coastal).
#'                            IC regions can be derived using \code{\link{BEC2IC}}.
#' @param ageType numeric, Must be either \code{0} or \code{1}. \code{0} stands for total age, for which site index is
#'                         calculated for 50 years of total tree age. While \code{1} stands for breast height age, for which
#'                         site index is calculated for 50 year old at breast height.
#' @param estType numeric, Defines how the site tools estimate site index. Valued as \code{0} and \code{1},
#'                                \code{0} is interative and while \code{1} is directive. Default is \code{1}, which is directive.
#'
#'
#' @return Site index
#'
#' @importFrom data.table ':=' data.table
#' @importFrom SIndexR SIndexR_HtAgeToSI SIndexR_DefCurve SIndexR_DefGICurve SIndexR_SpecRemap
#'
#'
#' @export
#' @docType methods
#' @rdname SiteTools_HTBoredAge2SI
#'
#' @author Yong Luo
#'
setGeneric("SiteTools_HTBoredAge2SI",
           function(boredAge, height, species, ICRegion,
                    ageType, estType) {
             standardGeneric("SiteTools_HTBoredAge2SI")
           })

#' @rdname SiteTools_HTBoredAge2SI
setMethod(
  "SiteTools_HTBoredAge2SI",
  signature = c(boredAge = "numeric",
                height = "numeric",
                species = "character",
                ICRegion = "character",
                ageType = "numeric",
                estType = "numeric"),
  definition = function(boredAge, height, species, ICRegion,
                        ageType, estType){
    worktable <- data.table(uniObs = 1:max(length(boredAge), length(height)),
                            age = boredAge, height, ageType,
                            speciesFRED = species, BEC_I_C = ICRegion)
    worktable[, SI_SP := SIndexR::SIndexR_SpecRemap(speciesFRED, BEC_I_C)]
    worktable[SI_SP >= 0, ':='(SITE_CURVE = SIndexR::SIndexR_DefCurve(SI_SP),
                               GRTH_CURVE = SIndexR::SIndexR_DefGICurve(SI_SP))]
    worktable[SI_SP >= 0, ':='(SI_ERR = SIndexR::SIndexR_HtAgeToSI(curve = SITE_CURVE,
                                                                   age = age,
                                                                   ageType = ageType,
                                                                   height = height,
                                                                   estType = estType)$error,
                               SI_TREE = SIndexR::SIndexR_HtAgeToSI(curve = SITE_CURVE,
                                                                    age = age,
                                                                    ageType = ageType,
                                                                    height = height,
                                                                    estType = estType)$output,
                               SI_SOURCE = "SI")]
    worktable[SI_SP >= 0 & SI_ERR < 0,
              ':='(SI_TREE = as.numeric(NA),
                   SI_SOURCE = NA)]
    worktable[SI_SP >= 0 & age <= 50, ':='(GI_ERR = SIndexR::SIndexR_HtAgeToSI(curve = GRTH_CURVE,
                                                                               age = age,
                                                                               ageType = ageType,
                                                                               height = height,
                                                                               estType = estType)$error,
                                           SI_GI = SIndexR::SIndexR_HtAgeToSI(curve = GRTH_CURVE,
                                                                              age = age,
                                                                              ageType = ageType,
                                                                              height = height,
                                                                              estType = estType)$output)]
    worktable[SI_SP >= 0 & age <= 50 & GI_ERR >= 0,
              ':='(SI_TREE = SI_GI,
                   SI_SOURCE = "GI")]
    curvenametable <- unique(worktable[,.(SI_SOURCE, SITE_CURVE, GRTH_CURVE)])
    curvenametable[, CURVE_NAME := as.character(NA)]
    for(indirow in 1:nrow(curvenametable)){
      if(!is.na(curvenametable$SI_SOURCE[indirow])){

        if(curvenametable$SI_SOURCE[indirow] == "SI"){
          curvenametable$CURVE_NAME[indirow] <- SIndexR::Sindex_CurveName(curvenametable$SITE_CURVE[indirow])
        } else if (curvenametable$SI_SOURCE[indirow] == "GI"){
          curvenametable$CURVE_NAME[indirow] <- SIndexR::Sindex_CurveName(curvenametable$GRTH_CURVE[indirow])
        }
      }
    }
    worktable <- merge(worktable,
                       curvenametable,
                       by = c("SI_SOURCE", "SITE_CURVE", "GRTH_CURVE"),
                       all.x = TRUE)
    worktable <- worktable[order(uniObs),]
    return(list(SI_TREE = worktable$SI_TREE,
                SI_SOURCE = worktable$SI_SOURCE,
                CURVE_NAME = worktable$CURVE_NAME))
  })
