#' get tsa, bec, fiz and ownership attributes based on region and compartment map
#'
#'
#' @description get tsa, bec, fiz and ownership attributes based on region and compartment map
#'
#' @param regionCompartMap sf, regioncompartment map.
#' @param spAttributeMap sf, spatial attribute map.
#' @param mapName character, spatial attribute map name, must be one of \code{BEC}, \code{TSA},
#'                \code{FIZ}, \code{TFL}, and \code{Ownership}.
#'
#' @return table of spatial attributes corresponding to region and compartment.
#' @note The spatial attributes must be one-on-one match to a unique combination of region
#'       and compartment
#'
#' @importFrom data.table ':=' data.table melt
#' @importFrom dplyr '%>%'
#' @importFrom sf st_transform st_intersection st_crs
#' @export
#' @docType methods
#' @rdname spAttriFromRegCompt
#' @author Yong Luo
spAttriFromRegCompt <- function(regionCompartMap,
                                spAttributeMap,
                                mapName){
  if(as.character(st_crs(regionCompartMap))[1] != as.character(st_crs(spAttributeMap))[1]){
    regionCompartMap <- st_transform(regionCompartMap, crs = st_crs(spAttributeMap))
  }
  intersection_table <- suppressWarnings(st_intersection(regionCompartMap,
                                                         spAttributeMap)) %>%
    data.table
  if(mapName == "BEC"){
    intersection_table <- unique(intersection_table[,.(SAMPLING_REGION_NUMBER = REGION,
                                                       COMPARTMENT_NUMBER = CMPRTMNT,
                                                       BEC_rcp = ZONE,
                                                       BEC_sbz_rcp = SUBZONE)])
    ## the accuracy for the bec map is subzone
    intersection_table[,NUMBER := length(BEC_rcp),
                                             by = c("SAMPLING_REGION_NUMBER",
                                                    "COMPARTMENT_NUMBER")]
  } else if(mapName == "TSA"){
    intersection_table <- unique(intersection_table[,.(SAMPLING_REGION_NUMBER = REGION,
                                                       COMPARTMENT_NUMBER = CMPRTMNT,
                                                       TSA_rcp = TSA_NUMBER,
                                                       TSA_dsc_rcp = TSA_NUMBER_DESCRIPTION)])
    intersection_table[,NUMBER := length(TSA_rcp),
                       by = c("SAMPLING_REGION_NUMBER",
                              "COMPARTMENT_NUMBER")]
  } else if(mapName == "FIZ"){
    intersection_table <- unique(intersection_table[,.(SAMPLING_REGION_NUMBER = REGION,
                                                       COMPARTMENT_NUMBER = CMPRTMNT,
                                                       FIZ_rcp = FOREST_INVENTORY_ZONE)])
    intersection_table[,NUMBER := length(FIZ_rcp),
      by = c("SAMPLING_REGION_NUMBER",
             "COMPARTMENT_NUMBER")]
  } else if(mapName == "TFL"){
    intersection_table <- unique(intersection_table[,.(SAMPLING_REGION_NUMBER = REGION,
                                                       COMPARTMENT_NUMBER = CMPRTMNT,
                                                       TFL_rcp = FOREST_FILE_ID)])
    intersection_table[,NUMBER := length(TFL_rcp),
      by = c("SAMPLING_REGION_NUMBER",
             "COMPARTMENT_NUMBER")]
  } else if(mapName == "Ownership"){
    intersection_table <- unique(intersection_table[,.(SAMPLING_REGION_NUMBER = REGION,
                                                       COMPARTMENT_NUMBER = CMPRTMNT,
                                                       OWNER_rcp = OWN,
                                                       SCHEDULE_rcp = SCHEDULE)])
    intersection_table[,NUMBER := length(OWNER_rcp),
                       by = c("SAMPLING_REGION_NUMBER",
                              "COMPARTMENT_NUMBER")]
  } else {
    stop("mapName must be correctly specified.")
  }
    intersection_table <- intersection_table[NUMBER == 1,]
    intersection_table[, NUMBER := NULL]
  return(intersection_table)
}
