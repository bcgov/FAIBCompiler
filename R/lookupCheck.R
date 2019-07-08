#' Test whether the lookup table is updated
#'
#' @description Reports whether a lookup table is updated. This function is highly recommended before
#'              proceed the VRI compiler.
#'
#' @param lookupName character, Specifies the name of lookup table.
#' @param lookupPath character, Path that directs to lookup tables.
#'
#'
#'
#' @return Not value returned. A warning message is given if the lookup table is changed.
#'
#' @importFrom data.table ':=' fread
#' @importFrom dplyr left_join
#' @importFrom haven read_sas
#'
#' @export
#' @docType methods
#' @rdname lookupCheck
#'
#' @author Yong Luo
#'
setGeneric("lookupCheck",
           function(lookupName, lookupPath) {
             standardGeneric("lookupCheck")
           })

#' @rdname lookupCheck
setMethod(
  "lookupCheck",
  signature = c(lookupName = "character",
                lookupPath = "character"),
  definition = function(lookupName, lookupPath){
    moditime <- file.info(file.path(lookupPath, paste(lookupName, ".sas7bdat", sep = "")))$mtime
    moditime <- substr(moditime, 1, 16)
    if(lookupName %in% c("vri_grp", "vri_bec")){
      refertime <- "2017-06-08 16:05"
    } else if (lookupName %in% c("spv_spc", "sp_cost", "spv_frd")){
      refertime <- "2002-06-21 11:03"
    } else if (lookupName %in% c("dcy_v3", "brk_99", "wst_v3")){
      refertime <- "2002-02-08 13:44"
    } else if (lookupName %in% c("dcy_v3x")){
      refertime <- "2002-04-24 11:55"
    } else if (lookupName %in% c("sp_type")){
      refertime <- "2003-10-15 14:39"
    }
    if(moditime == refertime){
      text <- appendedCat(paste("lookup table (", lookupName, ") is not changed. You can use it in the compiler.", sep = ""))
    } else {
      temdp <- tempdir()
      lookuptable_org <- read_sas(file.path(lookupPath, paste(lookupName, ".sas7bdat", sep = ""))) %>%
        data.table
      write.csv(lookuptable_org, file.path(temdp, "lookuptable.csv"), row.names = FALSE)
      lookuptable_org <- fread(file.path(temdp, "lookuptable.csv"))
      lookupfunction <- get(paste("lookup_", lookupName, sep = ""))
      lookuptable_hardcoded <- lookupfunction()
      names(lookuptable_org) <- names(lookuptable_hardcoded)
      if(all.equal(lookuptable_org, lookuptable_hardcoded)){
        text <- appendedCat(paste("lookup table (", lookupName, ") is not changed. You can use it in the compiler.", sep = ""))
      } else {
        stop(paste("lookup table (", lookupName, ") is changed. Please update it before compiling.", sep = ""))
      }
    }
    return(text)
  })


#' @export
#' @rdname lookupCheck
setMethod(
  "lookupCheck",
  signature = c(lookupName = "character",
                lookupPath = "missing"),
  definition = function(lookupName){
    lookupName <- tolower(lookupName)
    if(lookupName %in% c("vri_grp", "vri_bec", "spv_spc", "sp_cost", "spv_frd", "sp_type")){
      lookupPath <- "\\\\Mayhem.dmz\\GIS_TIB\\VRI\\RDW\\RDW_Data1\\System\\Lookups"
    } else if (lookupName %in% c("dcy_v3", "dcy_v3x", "brk_99", "wst_v3")){
      lookupPath <- "\\\\Mayhem.dmz\\GIS_TIB\\VRI\\RDW\\RDW_Data1\\Production\\DWB_RF"
    }
    return(lookupCheck(lookupName, lookupPath = lookupPath))
  })
