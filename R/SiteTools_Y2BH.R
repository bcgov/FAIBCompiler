#' Derive years to breast height using site tools
#'
#' @description Derive years to breast height based on species (\code{species}), region (\code{ICRegion}) and site index (\code{siteIndex})
#'              using site tools. This function is equivalent to \code{sindex_httoage.sas}.
#'
#' @param species character, Species code, must be consistent with the species code in site tools, which can be converted
#'                           from the original species code by using \code{\link{siteToolsSpeciesConvertor}}.
#' @param ICRegion character, Must be either \code{I} (interior) and \code{C} (coastal).
#'                            IC regions can be derived using \code{\link{BEC2IC}} function.
#' @param siteIndex numeric, Site index. Defined as tree height at 50 years old.
#'
#' @return Years to breast height
#'
#' @importFrom data.table ':=' data.table
#' @importFrom SIndexR SIndexR_SpecRemap SIndexR_DefCurve SIndexR_Y2BH
#'
#' @export
#' @docType methods
#' @rdname SiteTools_Y2BH
#'
#' @author Yong Luo
#'
setGeneric("SiteTools_Y2BH",
           function(species, ICRegion, siteIndex) {
             standardGeneric("SiteTools_Y2BH")
           })

#' @rdname SiteTools_Y2BH
setMethod(
  "SiteTools_Y2BH",
  signature = c(species = "character",
                ICRegion = "character",
                siteIndex = "numeric"),
  definition = function(species, ICRegion, siteIndex){
    worktable <- data.table(uniObs = 1:length(siteIndex), speciesFRED = species,
                            BEC_I_C = ICRegion, SI_TREE = siteIndex)
    worktable[, SI_SP := SIndexR::SIndexR_SpecRemap(speciesFRED, BEC_I_C)]
    worktable[SI_SP >= 0, SITE_CURVE := SIndexR::SIndexR_DefCurve(SI_SP)]
    worktable[SI_SP >= 0, ':='(crtedAge = SIndexR::SIndexR_Y2BH(SITE_CURVE, SI_TREE)$output,
                               YBHErr = SIndexR::SIndexR_Y2BH(SITE_CURVE, SI_TREE)$error)]
    worktable[SI_SP >= 0 & YBHErr < 0, crtedAge := 0]
    return(worktable[order(uniObs)]$crtedAge)
  })
