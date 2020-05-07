#' Used for updating spatial attributes based on locations
#'
#'
#' @description Used for updating spatial attributes based on locations.
#'
#' @param samplesites data.table A table that contains spatial loctions. Currently it is
#'                    designed for accepting UTM coordinates. Therefore, zone, northing
#'                    and easting must be provided.
#' @param mapPath character The path to fiz map.
#'
#' @return a table that contains spatial attributes of bc albers, longitude/latitude,
#'         TSA, BEC, FIZ, TFL and OWNERSHIP.
#'
#' @importFrom FAIBBase UTM_Convertor getSpatial
#' @export
#' @author Yong Luo
updateSpatial <- function(samplesites, mapPath, mapTimes){
  samplesites_Loc <- unique(samplesites[!is.na(IP_UTM) & !is.na(IP_NRTH) & !is.na(IP_EAST),
                                        .(SITE_IDENTIFIER,
                                          IP_UTM, IP_NRTH, IP_EAST)],
                            by = "SITE_IDENTIFIER")
  samplesites_Loc_bcalbers <- FAIBBase::UTM_Convertor(point_ID = samplesites_Loc$SITE_IDENTIFIER,
                                            zone = samplesites_Loc$IP_UTM,
                                            northing = samplesites_Loc$IP_NRTH,
                                            easting = samplesites_Loc$IP_EAST,
                                            class = "table")
  names(samplesites_Loc_bcalbers) <- c("SITE_IDENTIFIER", "BC_ALBERS_X", "BC_ALBERS_Y")
  samplesites_Loc_longlat <- FAIBBase::UTM_Convertor(point_ID = samplesites_Loc$SITE_IDENTIFIER,
                                           zone = samplesites_Loc$IP_UTM,
                                           northing = samplesites_Loc$IP_NRTH,
                                           easting = samplesites_Loc$IP_EAST,
                                           CRS_To = "+proj=longlat",
                                           class = "table")
  names(samplesites_Loc_longlat) <- c("SITE_IDENTIFIER", "Longitude", "Latitude")

  becmap <- readRDS(file.path(mapPath, paste0("BEC_", mapTimes$currenttime, ".rds")))
  samplesites_loc_bec <- FAIBBase::getSpatial(pointID = samplesites_Loc$SITE_IDENTIFIER,
                                    zone = samplesites_Loc$IP_UTM,
                                    northing = samplesites_Loc$IP_NRTH,
                                    easting = samplesites_Loc$IP_EAST,
                                    spatialMap = becmap,
                                    spatialAttribute = "bec")
  names(samplesites_loc_bec) <- c("SITE_IDENTIFIER", "BEC", "BEC_SBZ", "BEC_VAR")

  tsamap <- readRDS(file.path(mapPath, paste0("TSA_", mapTimes$currenttime, ".rds")))
  samplesites_loc_tsa <- FAIBBase::getSpatial(pointID = samplesites_Loc$SITE_IDENTIFIER,
                                    zone = samplesites_Loc$IP_UTM,
                                    northing = samplesites_Loc$IP_NRTH,
                                    easting = samplesites_Loc$IP_EAST,
                                    spatialMap = tsamap,
                                    spatialAttribute = "tsa")
  names(samplesites_loc_tsa) <- c("SITE_IDENTIFIER", "TSA", "TSA_DESC")

  fizmap <- readRDS(file.path(mapPath, paste0("FIZ_", mapTimes$fiz_time, ".rds")))
  samplesites_loc_fiz <- FAIBBase::getSpatial(pointID = samplesites_Loc$SITE_IDENTIFIER,
                                    zone = samplesites_Loc$IP_UTM,
                                    northing = samplesites_Loc$IP_NRTH,
                                    easting = samplesites_Loc$IP_EAST,
                                    spatialMap = fizmap,
                                    spatialAttribute = "fiz")
  names(samplesites_loc_fiz) <- c("SITE_IDENTIFIER", "FIZ")

  tflmap <- readRDS(file.path(mapPath, paste0("TFL_", mapTimes$tfl_time, ".rds")))
  samplesites_loc_tfl <- FAIBBase::getSpatial(pointID = samplesites_Loc$SITE_IDENTIFIER,
                                    zone = samplesites_Loc$IP_UTM,
                                    northing = samplesites_Loc$IP_NRTH,
                                    easting = samplesites_Loc$IP_EAST,
                                    spatialMap = tflmap,
                                    spatialAttribute = "tfl")
  names(samplesites_loc_tfl) <- c("SITE_IDENTIFIER", "TFL")
  ownermap <- readRDS(file.path(mapPath, paste0("OWNER_", mapTimes$ownership_time, ".rds")))
  samplesites_loc_ownership <- FAIBBase::getSpatial(pointID = samplesites_Loc$SITE_IDENTIFIER,
                                          zone = samplesites_Loc$IP_UTM,
                                          northing = samplesites_Loc$IP_NRTH,
                                          easting = samplesites_Loc$IP_EAST,
                                          spatialMap = ownermap,
                                          spatialAttribute = "OWNERSHIP")
  names(samplesites_loc_ownership)[1] <- c("SITE_IDENTIFIER")
  samplesites <- merge(samplesites, samplesites_Loc_bcalbers,
                       by = "SITE_IDENTIFIER", all.x = TRUE)
  samplesites <- merge(samplesites, samplesites_Loc_longlat,
                       by = "SITE_IDENTIFIER", all.x = TRUE)
  samplesites <- merge(samplesites, samplesites_loc_bec,
                       by = "SITE_IDENTIFIER", all.x = TRUE)
  samplesites <- merge(samplesites, samplesites_loc_tsa,
                       by = "SITE_IDENTIFIER", all.x = TRUE)
  samplesites <- merge(samplesites, samplesites_loc_fiz,
                       by = "SITE_IDENTIFIER", all.x = TRUE)
  samplesites <- merge(samplesites, samplesites_loc_tfl,
                       by = "SITE_IDENTIFIER", all.x = TRUE)
  samplesites <- merge(samplesites, samplesites_loc_ownership,
                       by = "SITE_IDENTIFIER", all.x = TRUE)
  return(samplesites)
}
