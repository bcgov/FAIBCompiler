#' Load and select trees that have loss factor information-VRI specific
#'
#' @description This function loads and selects trees that have loss factor information (\code{vi_d}, cardd) based on
#'              selected trees from vi_c.
#'
#' @param dataSourcePath character, Specifies the path that directs to the VRI original data soruce, i.e.,
#'                                  \code{//Mayhem/GIS_TIB/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/vri_sa}.
#'
#'
#' @return A data table that contains loss factor data. A log file documents the detailed process
#'
#'
#' @importFrom data.table data.table ':=' set rbindlist setnames setkey
#' @importFrom dplyr '%>%'
#' @export
#' @docType methods
#' @rdname vidPrep
#'
#' @author Yong Luo
vidPrep<- function(dataSourcePath){
  lossfactors <- readRDS(file.path(dataSourcePath, "vi_d.rds")) %>% data.table
  lossfactors <- unique(lossfactors, by = c("CLSTR_ID", "PLOT", "TREE_NO"))
  targetnames <- names(lossfactors)
  targetnames <- targetnames[substr(targetnames, 1, 4) == "LOSS"]
  targetnames <- gsub("LOSS", "", targetnames)
  targetnames <- gsub("_IN", "", targetnames)
  targetnames_max <- max(as.numeric(targetnames))
  lossfactors_simp <- lossfactors[, c("CLSTR_ID", "PLOT", "TREE_NO",
                                 paste("LOSS", 1:targetnames_max, "_IN", sep = ""),
                                 paste("LOC", 1:targetnames_max, "_FRO", sep = "")), with = FALSE]
  rm(targetnames, targetnames_max)
  lossfactors[, c(paste0("T_SIGN", 1:10),
                paste0("F_SIGN", 1:10),
                paste0("OLD_AGN", LETTERS[1:8]),
                "maxseq",
                "SITE_IDENTIFIER", "VISIT_NUMBER",
                "TYPE_CD", "BEC_ZONE", "BEC_SBZ",
                "BEC_VAR", "SPECIES_ORG", "SP0") := NULL]
  setnames(lossfactors, "STEM", "STEM_MAPPED_IND")
  return(list(lossfactors_simp = lossfactors_simp,
              lossfactors_full = lossfactors))
}
