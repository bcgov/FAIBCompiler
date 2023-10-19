#' Calculates species composition based on cluster/species summary
#'
#'
#' @description Calculates species composition at cluster level based on cluster/species summary.
#'              The cluster/species-level summaries is an output of \code{\link{volSmry_byCS}}
#'              function. This function is equivalent to \code{sp_comp.sas} in original compiler.
#'
#' @param CSSmryTable data.table, Summarized volume components for both measured and counted trees at cluster and species level.
#'                                See \code{\link{volSmry_byCS}} for details.
#' @param basedOn character, Specifies which component is used for species composition summary.
#' @param speciesMaxNO numeric, Maximum number of species entries to calculate.
#' @param smallTreeCompile logical, Defines whether the function is used for calculate species composition
#'                                  for small trees. If missing, \code{FALSE} is used.
#'
#'
#' @return A data table that contains species composition at cluster level.
#'
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#'
#'
#' @export
#' @docType methods
#' @rdname speciesComp_byC
#'
#' @author Yong Luo
#'
setGeneric("speciesComp_byC",
           function(CSSmryTable, basedOn, speciesMaxNO, smallTreeCompile){
             standardGeneric("speciesComp_byC")
           })

#' @rdname speciesComp_byC
setMethod(
  "speciesComp_byC",
  signature = c(CSSmryTable = "data.table",
                basedOn = "character",
                speciesMaxNO = "numeric",
                smallTreeCompile = "logical"),
  definition = function(CSSmryTable, basedOn, speciesMaxNO, smallTreeCompile){
    if(smallTreeCompile){
      bygroup <- c("CLSTR_ID")
    } else {
      bygroup <- c("CLSTR_ID", "UTIL")
    }
    if (speciesMaxNO %>>% 99 | speciesMaxNO %<<% 3){
      stop("speciesMaxNO must defined between 3 and 99.")
    }
    CSSmryTable[, SPECIES := gsub(" ", "", SPECIES)]
    CSSmryTable <- CSSmryTable[,c(bygroup, "SPECIES", basedOn), with = FALSE]
    setnames(CSSmryTable, basedOn, "BASEDON")
    CSSmryTable[, TOTALBYCL := sum(BASEDON), by = bygroup]
    CSSmryTable[, ':='(PCT = 100*BASEDON/TOTALBYCL,
                       PCTINTER = as.integer(100*BASEDON/TOTALBYCL))]
    CSSmryTable[, PCTDECI := PCT-PCTINTER]
    if(smallTreeCompile){
      CSSmryTable <- CSSmryTable[order(CLSTR_ID, -PCTDECI),]
    } else {
      CSSmryTable <- CSSmryTable[order(CLSTR_ID, UTIL, -PCTDECI),]
    }
    CSSmryTable[, TOTALPCTINTERDIFF := 100 - sum(PCTINTER), by = bygroup]
    CSSmryTable[, pctorder := 1:length(TOTALBYCL), by = bygroup]
    CSSmryTable[, PCTINTER_ADD := 1]
    CSSmryTable[pctorder %>>% TOTALPCTINTERDIFF, PCTINTER_ADD := 0]
    CSSmryTable[, PCTINTER := PCTINTER + PCTINTER_ADD]
    CSSmryTable <- CSSmryTable[PCTINTER %!=% 0,]
    CSSmryTable[, TOTALPCTINTER := sum(PCTINTER), by = bygroup]

    if(nrow(CSSmryTable[TOTALPCTINTER != 100]) > 0){
      stop("Something wrong with total species composition percentage.")
    }
    if(smallTreeCompile){
      CSSmryTable <- CSSmryTable[order(CLSTR_ID, -PCTINTER),]
    } else {
      CSSmryTable <- CSSmryTable[order(CLSTR_ID, UTIL, -PCTINTER),]
    }
    CSSmryTable[, ':='(MAXPCTINTER = max(PCTINTER),
                       SPNO = 1:length(SPECIES)), by = bygroup]
    CSSmryTable <- CSSmryTable[SPNO <= speciesMaxNO, ]

    CSSmryTable[, ':='(PCTINTER = as.character(PCTINTER),
                       MAXPCTINTER = as.character(MAXPCTINTER))]
    CSSmryTable[, SPPCT := PCTINTER]
    CSSmryTable[nchar(PCTINTER) == 2,
                SPPCT := paste("0", SPPCT, sep = "")]
    CSSmryTable[nchar(PCTINTER) == 1,
                SPPCT := paste("00", SPPCT, sep = "")]
    CSSmryTable[nchar(SPECIES) == 1,
                SPECIES := paste(SPECIES, " ", sep = "")]
    CSSmryTable[, SPPCT := paste(SPECIES, SPPCT, sep = "")]
    CSSmryTable <- CSSmryTable[, .(SPB_CPCT = paste(SPPCT, collapse = ""),
                                   TOTALBYCL = unique(TOTALBYCL)), by = bygroup]
    CSSmryTable[TOTALBYCL %==% 0, SPB_CPCT := ""]
    CSSmryTable[, TOTALBYCL := NULL]
    return(CSSmryTable)
  })

#' @export
#' @rdname speciesComp_byC
setMethod(
  "speciesComp_byC",
  signature = c(CSSmryTable = "data.table",
                basedOn = "character",
                speciesMaxNO = "numeric",
                smallTreeCompile = "missing"),
  definition = function(CSSmryTable, basedOn, speciesMaxNO){
    return(speciesComp_byC(CSSmryTable, basedOn, speciesMaxNO, smallTreeCompile = FALSE))
  })



