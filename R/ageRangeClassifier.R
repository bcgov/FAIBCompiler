#' Derive age range code
#'
#'
#' @description This function derives age range code based on \code{age}, \code{species} and \code{FIZ}.
#'              The returned age range code includes: \code{1}-young(immature), \code{2}-older(immature),
#'              \code{3}-mature and \code{4}-overmature. This function is equivalent to age_rng.sas macro.
#'
#' @param age numeric, Usually layer mean age. what does this mean?
#' @param species character, Tree basic species code, which is SP0 in VRI original data.
#' @param FIZ character, BC forest inventory zone.
#'
#'
#' @return DWB age range code
#'
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#'
#' @export
#' @docType methods
#' @rdname ageRangeClassifier
#'
#' @author Yong Luo
ageRangeClassifier<- function(age, species, FIZ){
  ####### TABLE FOR DWB USAGE (4-VALUE CODE)-----------------------------------
  ##
  ##  SP0 (IN)      YOUNG/IMMATURE    OLDER/IMMATURE    MATURE      OVERMATURE
  ##  AGE_RNG (OUT)      1                2               3           4
  ##
  ##  CONIFEROUS         1 - 80          81 - 120         121 +         N/A
  ##
  ##  PL                 1 - 60          61 - 120         121 +         N/A
  ##
  ##  DECIDUOUS          1 - 20          21 - 40           41 +         N/A
  ##
  ##  AC, AT: FIZ K,L    1 - 40          41 - 80           81 - 140     141 +
  ##
  ##------------------------------------------------------------------#/
  worktable <- data.table(uniObs = 1:max(length(age), length(species)),
                          AGE = as.integer(age), SP0 = species, FIZ)

  worktable[SP0 == "PL" & 1 %<=% AGE & AGE %<=% 60, AGE_RNG := "1"]
  worktable[SP0 == "PL" & 61 %<=% AGE & AGE %<=% 120, AGE_RNG := "2"]
  worktable[SP0 == "PL" & 121 %<=% AGE, AGE_RNG := "3"]

  worktable[is.na(AGE_RNG) & SP0 %in% c("AC", "AT") & FIZ %in% c("K", "L") &
              1 %<=% AGE & AGE %<=% 40, AGE_RNG := "1"]
  worktable[is.na(AGE_RNG) & SP0 %in% c("AC", "AT") & FIZ %in% c("K", "L") &
              41 %<=% AGE & AGE %<=% 80, AGE_RNG := "2"]
  worktable[is.na(AGE_RNG) & SP0 %in% c("AC", "AT") & FIZ %in% c("K", "L") &
              81 %<=% AGE & AGE %<=% 140, AGE_RNG := "3"]
  worktable[is.na(AGE_RNG) & SP0 %in% c("AC", "AT") & FIZ %in% c("K", "L") &
              141 %<=% AGE, AGE_RNG := "1"]

  worktable[is.na(AGE_RNG) & speciesCode2speciesType(SP0) == "C" &
              1 %<=% AGE & AGE %<=% 80, AGE_RNG := "1"]
  worktable[is.na(AGE_RNG) & speciesCode2speciesType(SP0) == "C" &
              81 %<=% AGE & AGE %<=% 120, AGE_RNG := "2"]
  worktable[is.na(AGE_RNG) & speciesCode2speciesType(SP0) == "C" &
              121 %<=% AGE, AGE_RNG := "3"]

  worktable[is.na(AGE_RNG) & 1 %<=% AGE & AGE %<=% 20, AGE_RNG := "1"]
  worktable[is.na(AGE_RNG) & 21 %<=% AGE & AGE %<=% 40, AGE_RNG := "2"]
  worktable[is.na(AGE_RNG) & 41 %<=% AGE, AGE_RNG := "1"]
  return(worktable[order(uniObs)]$AGE_RNG)
}
