#' Convert VRI species code to site tools species code
#'
#' @description This function converts BC VRI species code to site tools species code.
#'              The convertion is based on a hardcoded lookup table \code{spv_frd}.
#'
#' @param species character, Species code in VRI data sets.
#'
#'
#' @return Converted species codes that can be recognized by site tools program. NA is the
#'         species that failed to be converted.
#'
#' @importFrom data.table ':='
#'
#'
#' @export
#' @docType methods
#' @rdname siteToolsSpeciesConvertor
#'
#' @author Yong Luo
#'
setGeneric("siteToolsSpeciesConvertor",
           function(species) {
             standardGeneric("siteToolsSpeciesConvertor")
           })

#' @rdname siteToolsSpeciesConvertor
setMethod(
  "siteToolsSpeciesConvertor",
  signature = c(species = "character"),
  definition = function(species){
    worktable <- data.table(uniObs = 1:length(species),
                            species)
    lookuptable <- unique(lookup_species()[,.(SPECIES, SP_SINDEX)], by = "SPECIES")
    names(lookuptable) <- c("species", "speciesCvt")
    worktable <- merge(worktable, lookuptable, by = "species", all.x = TRUE)
    if(nrow(worktable[is.na(speciesCvt),]) > 0){
      warning("Species can not be converted to site tools species code. NA is generted.")
    }
    return(worktable[order(uniObs),]$speciesCvt)
  })
