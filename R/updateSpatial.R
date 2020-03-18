#' Used for updating spatial attributes based on locations
#'
#'
#' @description Used for updating spatial attributes based on locations.
#'
#' @param samplesites data.table A table that contains spatial loctions. Currently it is
#'                    designed for accepting UTM coordinates. Therefore, zone, northing
#'                    and easting must be provided.
#' @param fizmapPath character The path to fiz map.
#' @param fizmapName character The name of fiz map.
#' @param fizmapFormat character The format of fiz map.
#'
#' @param tflmapPath character The path to tfl map.
#' @param tflmapName character The name of tfl map.
#' @param tflmapFormat character The format of tfl map.
#'
#' @param ownershipmapPath character The path to ownership map.
#' @param ownershipmapName character The name of ownership map.
#' @param ownershipmapFormat character The format of ownership map.
#'
#' @return a table that contains spatial attributes of bc albers, longitude/latitude,
#'         TSA, BEC, FIZ, TFL and OWNERSHIP.
#'
#' @importFrom FAIBBase UTM_Convertor getSpatial
#' @export
#' @author Yong Luo
updateSpatial <- function(samplesites,
                          fizmapPath, fizmapName, fizmapFormat,
                          tflmapPath, tflmapName, tflmapFormat,
                          ownershipmapPath, ownershipmapName, ownershipmapFormat){
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
  samplesites_loc_bec <- FAIBBase::getSpatial(pointID = samplesites_Loc$SITE_IDENTIFIER,
                                    zone = samplesites_Loc$IP_UTM,
                                    northing = samplesites_Loc$IP_NRTH,
                                    easting = samplesites_Loc$IP_EAST,
                                    spatialAttribute = "bec")
  names(samplesites_loc_bec) <- c("SITE_IDENTIFIER", "BEC", "BEC_SBZ", "BEC_VAR")
  samplesites_loc_tsa <- FAIBBase::getSpatial(pointID = samplesites_Loc$SITE_IDENTIFIER,
                                    zone = samplesites_Loc$IP_UTM,
                                    northing = samplesites_Loc$IP_NRTH,
                                    easting = samplesites_Loc$IP_EAST,
                                    spatialAttribute = "tsa")
  names(samplesites_loc_tsa) <- c("SITE_IDENTIFIER", "TSA", "TSA_DESC")
  samplesites_loc_fiz <- FAIBBase::getSpatial(pointID = samplesites_Loc$SITE_IDENTIFIER,
                                    zone = samplesites_Loc$IP_UTM,
                                    northing = samplesites_Loc$IP_NRTH,
                                    easting = samplesites_Loc$IP_EAST,
                                    spatialAttribute = "fiz",
                                    mapPath = fizmapPath,
                                    mapName = fizmapName,
                                    mapFormat = fizmapFormat)
  names(samplesites_loc_fiz) <- c("SITE_IDENTIFIER", "FIZ")
  samplesites_loc_tfl <- FAIBBase::getSpatial(pointID = samplesites_Loc$SITE_IDENTIFIER,
                                    zone = samplesites_Loc$IP_UTM,
                                    northing = samplesites_Loc$IP_NRTH,
                                    easting = samplesites_Loc$IP_EAST,
                                    spatialAttribute = "tfl",
                                    mapPath = tflmapPath,
                                    mapName = tflmapName,
                                    mapFormat = tflmapFormat)
  names(samplesites_loc_tfl) <- c("SITE_IDENTIFIER", "TFL")
  samplesites_loc_ownership <- FAIBBase::getSpatial(pointID = samplesites_Loc$SITE_IDENTIFIER,
                                          zone = samplesites_Loc$IP_UTM,
                                          northing = samplesites_Loc$IP_NRTH,
                                          easting = samplesites_Loc$IP_EAST,
                                          spatialAttribute = "OWNERSHIP",
                                          mapPath = ownershipmapPath,
                                          mapName = ownershipmapName,
                                          mapFormat = ownershipmapFormat)
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
