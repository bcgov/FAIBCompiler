#' tree level species correct for ISMC database, based on bec and bec subzone
#'
#' @description This function is to correct species.
#'
#' @param BECInfor data.table, Cluster and plot level attributes, an output from \code{\link{VRIInit_clusterplot}}.
#'
#' @param dataSourcePath character, Specifies the path that directs to compilation_sa.
#'
#'
#' @return A data table that contains auxiliary plot tree data.
#'
#'
#' @importFrom data.table data.table ':=' set rbindlist setnames setkey
#' @importFrom dplyr '%>%'
#' @importFrom FAIBBase merge_dupUpdate PHFCalculator
#'
#' @export
#' @docType methods
#' @rdname spCorr
#'
#' @author Yong Luo
spCorr<- function(BECInfor,
                  dataSourcePath){
  file_ext <- c("c", "d", "f", "g", "h", "i")
  specieslookup <- unique(lookup_species()[,.(SPECIES, SP0)], by = "SPECIES")
  for(indiext in file_ext){
    thefile <- readRDS(file.path(dataSourcePath,
                                 paste0("vi_", indiext, ".rds"))) %>% data.table
    thefile <- merge(thefile, BECInfor[, .(CLSTR_ID, BEC_ZONE, BEC_SBZ, BEC_VAR)],
                     by = "CLSTR_ID", all.x = TRUE)
    thefile[, SPECIES_ORG := SPECIES]
    thefile[, SPECIES := speciesCorrection(SPECIES,
                                           BEC_ZONE,
                                           BEC_SBZ)]
    thefile <- merge(thefile, specieslookup,
                     by = "SPECIES")
    thefile[is.na(SP0), ':='(SPECIES = "X", SP0 = "F")]
    saveRDS(thefile, file.path(dataSourcePath,
                               paste0("vi_", indiext, ".rds")))
  }
}
