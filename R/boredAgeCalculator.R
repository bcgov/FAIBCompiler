#' Derive bored age using office and field bored age
#'
#' @description This function is to derive bore age based on either office bored age (\code{officeBoredAge})
#'              or field bored age (\code{fieldBoredAge}). When both bore age information are available, the function takes
#'              \code{officeBoredAge} as priority. The function is one of the four functions that derive bored age using
#'              different method. The rests are \code{\link{boredAgeCalculator_Total}}, \code{\link{boredAgeCalculator_Phys}} and
#'              \code{\link{boredAgeCalculator_Prorated}}.
#'
#' @param officeBoredAge numeric, Office bored age, which is measured in lab by professionals.
#' @param fieldBoredAge numeric, Field bored age, estimated in field by field crew.
#'
#'
#' @return bored age
#'
#' @importFrom data.table ':=' data.table
#' @seealso \code{\link{boredAgeCalculator_Total}}
#'          \code{\link{boredAgeCalculator_Phys}}
#'          \code{\link{boredAgeCalculator_Prorated}}
#'
#'
#' @export
#' @docType methods
#' @rdname boredAgeCalculator_Bore
#'
#' @author Yong Luo
#'
setGeneric("boredAgeCalculator_Bore",
           function(officeBoredAge, fieldBoredAge) {
             standardGeneric("boredAgeCalculator_Bore")
           })

#' @rdname boredAgeCalculator_Bore
setMethod(
  "boredAgeCalculator_Bore",
  signature = c(officeBoredAge = "numeric",
                fieldBoredAge = "numeric"),
  definition = function(officeBoredAge, fieldBoredAge){
    worktable <- data.table(uniObs = 1:(max(length(officeBoredAge), length(fieldBoredAge))),
                            officeBoredAge, fieldBoredAge)
    if(nrow(worktable[(officeBoredAge <= 0 & !is.na(officeBoredAge)) |
                      (fieldBoredAge <= 0 & !is.na(fieldBoredAge)), ]) > 0){
      stop("Both office bored age and field bored age must be positive value.")
    }
    if(nrow(worktable[is.na(officeBoredAge) & is.na(fieldBoredAge),]) > 0){
      warning(paste(nrow(worktable[is.na(officeBoredAge) & is.na(fieldBoredAge),]),
                    " observation(s) do not have neither office bored age nor field bored age. NA is returned.",
                    sep = ""))
    }
    worktable[fieldBoredAge > 0, boredAge := fieldBoredAge]
    worktable[officeBoredAge > 0, boredAge := officeBoredAge]
    return(worktable[order(uniObs),]$boredAge)
  })

#' @export
#' @rdname boredAgeCalculator_Bore
setMethod(
  "boredAgeCalculator_Bore",
  signature = c(officeBoredAge = "numeric",
                fieldBoredAge = "missing"),
  definition = function(officeBoredAge){
    return(boredAgeCalculator_Bore(officeBoredAge, fieldBoredAge = as.numeric(NA)))
  })

#' @export
#' @rdname boredAgeCalculator_Bore
setMethod(
  "boredAgeCalculator_Bore",
  signature = c(officeBoredAge = "missing",
                fieldBoredAge = "numeric"),
  definition = function(fieldBoredAge){
    return(boredAgeCalculator_Bore(officeBoredAge = as.numeric(NA), fieldBoredAge))
  })




#' Derive bored age using total age
#'
#'
#' @description This function is to derive bore age based on total age (\code{totalAge}).The function is one of the four functions that derive bored age using
#'              different method. The rests are \code{\link{boredAgeCalculator_Bore}}, \code{\link{boredAgeCalculator_Phys}} and
#'              \code{\link{boredAgeCalculator_Prorated}}.
#'
#' @param totalAge numeric, Total tree age, ie., age at height of 0.
#'
#'
#' @return bored age
#'
#' @importFrom data.table ':=' data.table
#'
#' @seealso \code{\link{boredAgeCalculator_Total}}
#'          \code{\link{boredAgeCalculator_Phys}}
#'          \code{\link{boredAgeCalculator_Prorated}}
#'
#' @export
#' @docType methods
#' @rdname boredAgeCalculator_Total
#'
#' @author Yong Luo
#'
setGeneric("boredAgeCalculator_Total",
           function(totalAge) {
             standardGeneric("boredAgeCalculator_Total")
           })

#' @rdname boredAgeCalculator_Total
setMethod(
  "boredAgeCalculator_Total",
  signature = c(totalAge = "numeric"),
  definition = function(totalAge){
    if(length(totalAge[totalAge <= 0 & !is.na(totalAge)]) > 0){
      stop("Total age must be positive value.")
    }
    if(length(totalAge[is.na(totalAge)]) > 0){
      warning(paste(length(totalAge[is.na(totalAge)]),
                    " observation(s) do not have total age. NA is returned.", sep = ""))
    }
    return(totalAge)
  })




#' Derive bored age using physiological age
#'
#'
#' @description This function is to derive bore age based on physiological age (\code{physAge}). The function is one of the four functions that derive bored age using
#'              different method. The rests are \code{\link{boredAgeCalculator_Bore}}, \code{\link{boredAgeCalculator_Total}} and
#'              \code{\link{boredAgeCalculator_Prorated}}.
#'
#' @param physAge numeric, Pysiological age.
#'
#'
#' @return bored age
#'
#'
#' @seealso \code{\link{boredAgeCalculator_Total}}
#'          \code{\link{boredAgeCalculator_Phys}}
#'          \code{\link{boredAgeCalculator_Prorated}}
#'
#' @export
#' @docType methods
#' @rdname boredAgeCalculator_Phys
#'
#' @author Yong Luo
#'
setGeneric("boredAgeCalculator_Phys",
           function(physAge) {
             standardGeneric("boredAgeCalculator_Phys")
           })

#' @rdname boredAgeCalculator_Phys
setMethod(
  "boredAgeCalculator_Phys",
  signature = c(physAge = "numeric"),
  definition = function(physAge){
    if(length(physAge[physAge <= 0 & !is.na(physAge)]) > 0){
      stop("Physiological age must be positive value.")
    }
    if(length(physAge[is.na(physAge)]) > 0){
      warning(paste(length(physAge[is.na(physAge)]),
                    " observation(s) do not have physiological age. NA is returned.", sep = ""))
    }
    return(physAge)
  })




#' Derive bored age using pro-rated age
#'
#'
#'
#' @description This function is to derive bore age based on diameter at bore (\code{boreDiameter}),
#'              bark thickness (\code{barkThickness}), pro-rated ring length (\code{ringLength_prorated}) and
#'              pro-rated ring count (\code{ringCount_prorated}).  The function is one of the four functions that derive bored age using
#'              different method. The rests are \code{\link{boredAgeCalculator_Bore}}, \code{\link{boredAgeCalculator_Total}} and
#'              \code{\link{boredAgeCalculator_Phys}}.
#'
#' @param ringLength_prorated numeric, Pro-rated ring length in cm
#' @param ringCount_prorated numeric, Pro-rated ring count
#' @param boreDiameter numeric, Diameter at bore in cm
#' @param barkThickness numeric, Bark thickness in mm. If missing, 0.05 is used in the function.
#'
#'
#' @return bored age
#'
#' @importFrom data.table data.table ':='
#' @seealso \code{\link{boredAgeCalculator_Total}}
#'          \code{\link{boredAgeCalculator_Phys}}
#'          \code{\link{boredAgeCalculator_Prorated}}
#'
#' @export
#' @docType methods
#' @rdname boredAgeCalculator_Prorated
#'
#' @author Yong Luo
#'
setGeneric("boredAgeCalculator_Prorated",
           function(ringLength_prorated, ringCount_prorated, boreDiameter, barkThickness) {
             standardGeneric("boredAgeCalculator_Prorated")
           })

#' @rdname boredAgeCalculator_Prorated
setMethod(
  "boredAgeCalculator_Prorated",
  signature = c(ringLength_prorated = "numeric",
                ringCount_prorated = "numeric",
                boreDiameter = "numeric",
                barkThickness = "numeric"),
  definition = function(ringLength_prorated, ringCount_prorated,
                        boreDiameter, barkThickness){
    worktable <- data.table(uniObs = 1:max(length(ringLength_prorated), length(ringCount_prorated)),
                            boreDiameter, barkThickness, ringLength_prorated, ringCount_prorated)
    worktable[barkThickness <= 0 | is.na(barkThickness), barkThickness := 0.05]
    if(nrow(worktable[boreDiameter <= 0]) > 0 |
       nrow(worktable[ringLength_prorated <= 0]) > 0 |
       nrow(worktable[ringCount_prorated <= 0]) > 0){
      stop("All the inputs must be positive or NA value.")
    }
    if(nrow(worktable[is.na(boreDiameter) | is.na(ringLength_prorated) | is.na(ringCount_prorated)]) > 0){
      warning(paste(nrow(worktable[is.na(boreDiameter) | is.na(ringLength_prorated) | is.na(ringCount_prorated)]),
                    " observation(s) do not have either bore diameter, ring length or ring count information. NA is returned"))
    }
    worktable[, li := (boreDiameter/2)-(barkThickness/10) - ringLength_prorated]
    worktable[li < 0, li := 0]
    worktable[, gy := pi*((li+ringLength_prorated)^2 - li^2)/ringCount_prorated]
    worktable[, age_rot := (pi*li^2)/gy]
    worktable[, age_pro := ringCount_prorated + age_rot]
    return(round(worktable[order(uniObs)]$age_pro, 5)) ## follow sas code, but not sure it is a good idea
  })


#' @export
#' @rdname boredAgeCalculator_Prorated
setMethod(
  "boredAgeCalculator_Prorated",
  signature = c(ringLength_prorated = "numeric",
                ringCount_prorated = "numeric",
                boreDiameter = "numeric",
                barkThickness = "missing"),
  definition = function(ringLength_prorated, ringCount_prorated,
                        boreDiameter){
    return(boredAgeCalculator_Prorated(ringLength_prorated, ringCount_prorated,
                                       boreDiameter, barkThickness = 0.05))
  })

