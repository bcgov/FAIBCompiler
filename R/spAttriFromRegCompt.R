#' get tsa, bec, fiz and ownership attributes based on region and compartment map
#'
#'
#' @description get tsa, bec, fiz and ownership attributes based on region and compartment map
#'
#' @param region numeric, region number.
#' @param compartment numeric, compartment number.
#'
#' @param regionCompartMap sf, regioncompartment map.
#' @param spAttributeMap sf, spatial attribute map.
#' @param mapName character, spatial attribute map name, must be one of \code{BEC}, \code{TSA},
#'                \code{FIZ}, \code{TFL}, and \code{Ownership}.
#'
#' @return table of spatial attributes corresponding to region and compartment.
#'
#' @importFrom data.table ':=' data.table melt
#' @importFrom dplyr '%>%'
#'
#' @rdname spAttriFromRegCompt
#' @author Yong Luo
spAttriFromRegCompt <- function(region,
                                compartment,
                                regionCompartMap,
                                spAttributeMap,
                                mapName){
  regionCompartTable <- unique(data.table(SAMPLING_REGION_NUMBER = region,
                                          COMPARTMENT_NUMBER = compartment))
  if(as.character(crs(regionCompartMap)) != as.character(crs(spAttributeMap))){
    regionCompartMap <- st_transform(regionCompartMap, crs = spAttributeMap)
  }
  intersection_table <- suppressWarnings(st_intersection(regionCompartMap,
                                                         spAttributeMap)) %>%
    data.table
  if(mapName == "BEC"){
    intersection_table <- unique(intersection_table[,.(SAMPLING_REGION_NUMBER = REGION,
                                                       COMPARTMENT_NUMBER = CMPRTMNT,
                                                       BEC_rcp = ZONE,
                                                       BEC_sbz_rcp = SUBZONE,
                                                       BEC_var_rcp = VARIANT)])
    intersection_table <- intersection_table[order(SAMPLING_REGION_NUMBER,
                                                   COMPARTMENT_NUMBER,
                                                   BEC_rcp,
                                                   BEC_sbz_rcp,
                                                   BEC_var_rcp),]
  } else if(mapName == "TSA"){
    intersection_table <- unique(intersection_table[,.(SAMPLING_REGION_NUMBER = REGION,
                                                       COMPARTMENT_NUMBER = CMPRTMNT,
                                                       TSA_rcp = TSA_NUMBER,
                                                       TSA_dsc_rcp = TSA_NUMBER_DESCRIPTION)])
    intersection_table <- intersection_table[order(SAMPLING_REGION_NUMBER,
                                                   COMPARTMENT_NUMBER,
                                                   TSA_rcp,
                                                   TSA_dsc_rcp),]
  } else if(mapName == "FIZ"){
    intersection_table <- unique(intersection_table[,.(SAMPLING_REGION_NUMBER = REGION,
                                                       COMPARTMENT_NUMBER = CMPRTMNT,
                                                       FIZ_rcp = FOREST_INVENTORY_ZONE)])
    intersection_table <- intersection_table[order(SAMPLING_REGION_NUMBER,
                                                   COMPARTMENT_NUMBER,
                                                   FIZ_rcp),]
  } else if(mapName == "TFL"){
    intersection_table <- unique(intersection_table[,.(SAMPLING_REGION_NUMBER = REGION,
                                                       COMPARTMENT_NUMBER = CMPRTMNT,
                                                       TFL_rcp = FOREST_FILE_ID)])
    intersection_table <- intersection_table[order(SAMPLING_REGION_NUMBER,
                                                   COMPARTMENT_NUMBER,
                                                   TFL_rcp),]
  } else if(mapName == "Ownership"){
    intersection_table <- unique(intersection_table[,.(SAMPLING_REGION_NUMBER = REGION,
                                                       COMPARTMENT_NUMBER = CMPRTMNT,
                                                       OWNER_rcp = OWN,
                                                       SCHEDULE_rcp = SCHEDULE)])
    intersection_table <- intersection_table[order(SAMPLING_REGION_NUMBER,
                                                   COMPARTMENT_NUMBER,
                                                   OWNER_rcp,
                                                   SCHEDULE_rcp),]
  } else {
    stop("mapName must be correctly specified.")
  }
    intersection_table <- unique(intersection_table,
                                 by = c("SAMPLING_REGION_NUMBER",
                                        "COMPARTMENT_NUMBER"))
    regionCompartTable <- merge(regionCompartTable,
                                intersection_table,
                                by = c("SAMPLING_REGION_NUMBER",
                                       "COMPARTMENT_NUMBER"),
                                all.x = TRUE)
  return(regionCompartTable)
}
