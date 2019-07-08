#' Derive age at breast height
#'
#' @description This function uses site tools to derive age at breast height for the bored age that is not taken at
#'              breast height, i.e., \code{1.3 m}. The process was documented in BC VRI Sample Data Compilation Process.
#'
#'
#' @param boredAge numeric, Age at bored height
#' @param boredHeight numeric, Height at the bore core.
#' @param treeHeight numeric, Total tree height
#' @param species character, Species code, must be consistent with the species code in site tools.
#'                           Can be derived using \code{\link{siteToolsSpeciesConvertor}}.
#' @param FIZ character, Forest inventory zone.
#'
#' @return Tree age at breast height (1.3 m). For the estimated age <= 0.1, the bored age will be used as breast age and
#'              a warning message will be given.
#'
#' @importFrom data.table ':=' data.table
#'
#'
#' @export
#' @docType methods
#' @rdname boredAgeCalculator_Crted
#'
#' @author Yong Luo
#'
setGeneric("boredAgeCalculator_Crted",
           function(boredAge, boredHeight, treeHeight,
                    species, FIZ) {
             standardGeneric("boredAgeCalculator_Crted")
           })

#' @rdname boredAgeCalculator_Crted
setMethod(
  "boredAgeCalculator_Crted",
  signature = c(boredAge = "numeric",
                boredHeight = "numeric",
                treeHeight = "numeric",
                species = "character",
                FIZ = "character"),
  definition = function(boredAge, boredHeight, treeHeight,
                        species, FIZ){
    worktable <- data.table(uniObs = 1:(max(length(boredHeight), length(boredAge))),
                            speciesFRED = species, AGE_BORE = boredAge,
                            HT_BORE = boredHeight, HT_TREE = treeHeight,
                            FIZ = FIZ)
    ## the following codes are not adopted from orginal sas code, as there is a
    ## huge bug in that file
    ## the below codes are following the VRI documentation
    worktable[, ':='(AGE_TP = 1)]
    worktable[HT_BORE < 1.3, AGE_TP := 0]
    ## for the bored core below 1.3, total age is used
    worktable[, ':='(firstBHA = AGE_BORE, # first breast height age
                     secondBHA = AGE_BORE, # second breast height age
                     I = 0)]
    newdata <- worktable[0,]
    while(nrow(worktable) > 0){
      worktable[, I := I+1]
      worktable[, firstBHA := secondBHA] # first BHA is the first bored age
      ## what should we use for ageType
      worktable[firstBHA > 0, SI_N := SiteTools_HTBoredAge2SI(boredAge = firstBHA, height = HT_TREE,
                                                              species = speciesFRED, ICRegion = FIZ,
                                                              ageType = 1, estType = 1)]
      ## call SI_Y2BH an equivalent function to sindex_y2bh.sas
      worktable[firstBHA > 0, CORR_N := SiteTools_Y2BH(species = speciesFRED, ICRegion = FIZ,
                                                       siteIndex = SI_N)]
      ## update bored age
      worktable[firstBHA > 0, secondBHA := AGE_BORE - CORR_N*(1.3 - HT_BORE)/1.3]
      tempnewdata <- worktable[(I >= 2 & round(firstBHA) == round(secondBHA)) |
                                 I == 10,] ## set 10 as the last iteration
      set(tempnewdata, , c("SI_N", "CORR_N"), NULL)
      newdata <- rbindlist(list(newdata, tempnewdata))
      worktable <- worktable[!(uniObs %in% tempnewdata$uniObs),]
      rm(tempnewdata)
    }
    ## give user a warning message when the corrected age at breast height is less than
    ## or equation to 0.1
    if(nrow(newdata[secondBHA <= 0.1]) > 0){
      warning(paste("Derived age @breast height <= 0.1, the bored age is used: ", nrow(newdata[secondBHA <= 0.1]),
                    " observations.", sep = ""))
      newdata[secondBHA <= 0.1, secondBHA := AGE_BORE]
    }
    return(newdata[order(uniObs),]$secondBHA)
  })
