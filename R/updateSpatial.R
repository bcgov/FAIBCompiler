#' Used for updating spatial attributes based on locations
#'
#'
#' @description Used for updating spatial attributes based on locations.
#' @param compilationType character, either \code{PSP} or \code{nonPSP}. If it is \code{PSP}, it
#'                               is consistent with original PSP compiler, otherwise, it
#'                               is consistent with VRI compiler.
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
updateSpatial <- function(compilationType, samplesites, mapPath){
  previousSamples <- readRDS(file.path(mapPath, paste0("spatiallookup_", compilationType, ".rds")))
  previousMapVer <- previousSamples$mapsource
  previousSamples <- previousSamples$spatiallookup
  names(previousSamples) <- paste0(names(previousSamples), "_prev")
  setnames(previousSamples, "SITE_IDENTIFIER_prev", "SITE_IDENTIFIER")
  samplesites_Loc <- unique(samplesites[,.(SITE_IDENTIFIER,
                                          IP_UTM, IP_NRTH, IP_EAST)],
                            by = "SITE_IDENTIFIER")
  samples_proc_all <- unique(samplesites[!is.na(IP_UTM) & !is.na(IP_NRTH) & !is.na(IP_EAST),
                                         .(SITE_IDENTIFIER,
                                           IP_UTM, IP_NRTH, IP_EAST)],
                             by = "SITE_IDENTIFIER")

  allsamples <- merge(previousSamples[, inprev := TRUE],
                      samplesites_Loc[, incurt := TRUE],
                      by = "SITE_IDENTIFIER",
                      all = TRUE)
  allsamples[, unid := 1:nrow(allsamples)]

  samples_skip <- allsamples[(inprev == TRUE & incurt == TRUE) &
                               (IP_UTM_prev == IP_UTM |
                                  (is.na(IP_UTM_prev) & is.na(IP_UTM))) &
                               (IP_EAST_prev == IP_EAST |
                                  (is.na(IP_EAST_prev) & is.na(IP_EAST))) &
                               (IP_NRTH_prev == IP_NRTH |
                                  (is.na(IP_NRTH_prev) & is.na(IP_NRTH)))]
  samples_proc <- allsamples[!(unid %in% samples_skip$unid) &
                               incurt == TRUE &
                               (!is.na(IP_UTM) & !is.na(IP_NRTH) & !is.na(IP_EAST)),
                             .(SITE_IDENTIFIER,
                               IP_UTM, IP_NRTH, IP_EAST)]
  ## for albers and latlong
  samples_skip_latlong <- samples_skip[,.(SITE_IDENTIFIER,
                                          BC_ALBERS_X = BC_ALBERS_X_prev,
                                          BC_ALBERS_Y = BC_ALBERS_Y_prev,
                                          Longitude = Longitude_prev,
                                          Latitude = Latitude_prev)]
  if(nrow(samples_proc) > 0){
    samplesites_Loc_bcalbers <- FAIBBase::UTM_Convertor(point_ID = samples_proc$SITE_IDENTIFIER,
                                                        zone = samples_proc$IP_UTM,
                                                        northing = samples_proc$IP_NRTH,
                                                        easting = samples_proc$IP_EAST,
                                                        class = "table")
    names(samplesites_Loc_bcalbers) <- c("SITE_IDENTIFIER", "BC_ALBERS_X", "BC_ALBERS_Y")
    samples_proc_latlong <- merge(samples_proc[,.(SITE_IDENTIFIER)],
                                  samplesites_Loc_bcalbers,
                                  by = "SITE_IDENTIFIER",
                                  all.x = TRUE)
    samplesites_Loc_longlat <- FAIBBase::UTM_Convertor(point_ID = samples_proc$SITE_IDENTIFIER,
                                                       zone = samples_proc$IP_UTM,
                                                       northing = samples_proc$IP_NRTH,
                                                       easting = samples_proc$IP_EAST,
                                                       CRS_To = "+proj=longlat",
                                                       class = "table")
    names(samplesites_Loc_longlat) <- c("SITE_IDENTIFIER", "Longitude", "Latitude")
    samples_proc_latlong <- merge(samples_proc_latlong,
                                  samplesites_Loc_longlat,
                                  by = "SITE_IDENTIFIER",
                                  all.x = TRUE)
    samples_skip_latlong <- rbind(samples_skip_latlong,
                                  samples_proc_latlong)
  }
  samplesites <- merge(samplesites, samples_skip_latlong,
                       by = "SITE_IDENTIFIER", all.x = TRUE)
  if(dir(mapPath, pattern = "BEC_map") %in% previousMapVer$mapFile){ # bec map does not change
    samples_skip_bec <- samples_skip[,.(SITE_IDENTIFIER,
                                        BEC = BEC_ZONE_prev,
                                        BEC_SBZ = BEC_SBZ_prev,
                                        BEC_VAR = BEC_VAR_prev)]
    if(nrow(samples_proc) > 0){
      becmap <- readRDS(dir(mapPath, pattern = "BEC_map", full.names = TRUE))
      samplesites_loc_bec <- FAIBBase::getSpatial(pointID = samples_proc$SITE_IDENTIFIER,
                                                  zone = samples_proc$IP_UTM,
                                                  northing = samples_proc$IP_NRTH,
                                                  easting = samples_proc$IP_EAST,
                                                  spatialMap = becmap$map,
                                                  spatialAttribute = "bec")
      names(samplesites_loc_bec) <- c("SITE_IDENTIFIER", "BEC", "BEC_SBZ", "BEC_VAR")
      samplesites_loc_bec[, BEC_SOURCE := 1] # 1 means from map
      samples_skip_bec <- rbind(samples_skip_bec, samplesites_loc_bec, fill = TRUE)
    }
    samplesites <- merge(samplesites, samples_skip_bec,
                         by = "SITE_IDENTIFIER", all.x = TRUE)
  } else { # bec map changed
    becmap <- readRDS(dir(mapPath, pattern = "BEC_map", full.names = TRUE))
    samplesites_loc_bec <- FAIBBase::getSpatial(pointID = samples_proc_all$SITE_IDENTIFIER,
                                                zone = samples_proc_all$IP_UTM,
                                                northing = samples_proc_all$IP_NRTH,
                                                easting = samples_proc_all$IP_EAST,
                                                spatialMap = becmap$map,
                                                spatialAttribute = "bec")

    names(samplesites_loc_bec) <- c("SITE_IDENTIFIER", "BEC", "BEC_SBZ", "BEC_VAR")
    samplesites_loc_bec[, BEC_SOURCE := 1] # 1 means from map
    samplesites <- merge(samplesites, samplesites_loc_bec,
                         by = "SITE_IDENTIFIER", all.x = TRUE)
  }

  if(dir(mapPath, pattern = "TSA_map") %in% previousMapVer$mapFile){ # TSA map does not change
    samples_skip_tsa <- samples_skip[,.(SITE_IDENTIFIER,
                                        TSA = TSA_prev,
                                        TSA_DESC = TSA_DESC_prev)]
    if(nrow(samples_proc) > 0){
      tsamap <- readRDS(dir(mapPath, "TSA_", full.names = TRUE))
      samplesites_loc_tsa <- FAIBBase::getSpatial(pointID = samples_proc$SITE_IDENTIFIER,
                                                  zone = samples_proc$IP_UTM,
                                                  northing = samples_proc$IP_NRTH,
                                                  easting = samples_proc$IP_EAST,
                                                  spatialMap = tsamap$map,
                                                  spatialAttribute = "tsa")
      names(samplesites_loc_tsa) <- c("SITE_IDENTIFIER", "TSA", "TSA_DESC")
      samples_skip_tsa <- rbind(samples_skip_tsa, samplesites_loc_tsa)
    }
    samplesites <- merge(samplesites, samples_skip_tsa,
                         by = "SITE_IDENTIFIER", all.x = TRUE)
  } else { # tsa map changed
    tsamap <- readRDS(dir(mapPath, "TSA_", full.names = TRUE))
    samplesites_loc_tsa <- FAIBBase::getSpatial(pointID = samples_proc_all$SITE_IDENTIFIER,
                                                zone = samples_proc_all$IP_UTM,
                                                northing = samples_proc_all$IP_NRTH,
                                                easting = samples_proc_all$IP_EAST,
                                                spatialMap = tsamap$map,
                                                spatialAttribute = "tsa")
    names(samplesites_loc_tsa) <- c("SITE_IDENTIFIER", "TSA", "TSA_DESC")
    samplesites <- merge(samplesites, samplesites_loc_tsa,
                         by = "SITE_IDENTIFIER", all.x = TRUE)
  }


  if(dir(mapPath, pattern = "FIZ_") %in% previousMapVer$mapFile){ # FIZ map does not change
    samples_skip_fiz <- samples_skip[,.(SITE_IDENTIFIER,
                                        FIZ = FIZ_prev)]
    if(nrow(samples_proc) > 0){
      fizmap <- readRDS(dir(mapPath, "FIZ_", full.names = TRUE))
      samplesites_loc_fiz <- FAIBBase::getSpatial(pointID = samples_proc$SITE_IDENTIFIER,
                                                  zone = samples_proc$IP_UTM,
                                                  northing = samples_proc$IP_NRTH,
                                                  easting = samples_proc$IP_EAST,
                                                  spatialMap = fizmap$map,
                                                  spatialAttribute = "fiz")
      names(samplesites_loc_fiz) <- c("SITE_IDENTIFIER", "FIZ")
      samples_skip_fiz <- rbind(samples_skip_fiz, samplesites_loc_fiz)
    }
    samplesites <- merge(samplesites, samples_skip_fiz,
                         by = "SITE_IDENTIFIER", all.x = TRUE)
  } else { # fiz map changed
    fizmap <- readRDS(dir(mapPath, "FIZ_", full.names = TRUE))
    samplesites_loc_fiz <- FAIBBase::getSpatial(pointID = samples_proc_all$SITE_IDENTIFIER,
                                                zone = samples_proc_all$IP_UTM,
                                                northing = samples_proc_all$IP_NRTH,
                                                easting = samples_proc_all$IP_EAST,
                                                spatialMap = fizmap$map,
                                                spatialAttribute = "fiz")
    names(samplesites_loc_fiz) <- c("SITE_IDENTIFIER", "FIZ")
    samplesites <- merge(samplesites, samplesites_loc_fiz,
                         by = "SITE_IDENTIFIER", all.x = TRUE)
  }

  if(dir(mapPath, pattern = "TFL_") %in% previousMapVer$mapFile){ # tfl map does not change
    samples_skip_tfl <- samples_skip[,.(SITE_IDENTIFIER,
                                        TFL = TFL_prev,
                                        TFL_LICENCEE = TFL_LICENCEE_prev)]
    if(nrow(samples_proc) > 0){
      tflmap <- readRDS(dir(mapPath, "TFL_", full.names = TRUE))
      samplesites_loc_tfl <- FAIBBase::getSpatial(pointID = samples_proc$SITE_IDENTIFIER,
                                                  zone = samples_proc$IP_UTM,
                                                  northing = samples_proc$IP_NRTH,
                                                  easting = samples_proc$IP_EAST,
                                                  spatialMap = tflmap$map,
                                                  spatialAttribute = "tfl")
      names(samplesites_loc_tfl) <- c("SITE_IDENTIFIER", "TFL", "TFL_LICENCEE")
      samples_skip_tfl <- rbind(samples_skip_tfl, samplesites_loc_tfl)
    }
    samplesites <- merge(samplesites, samples_skip_tfl,
                         by = "SITE_IDENTIFIER", all.x = TRUE)
  } else { # tfl map changed
    tflmap <- readRDS(dir(mapPath, "TFL_", full.names = TRUE))
    samplesites_loc_tfl <- FAIBBase::getSpatial(pointID = samples_proc_all$SITE_IDENTIFIER,
                                                zone = samples_proc_all$IP_UTM,
                                                northing = samples_proc_all$IP_NRTH,
                                                easting = samples_proc_all$IP_EAST,
                                                spatialMap = tflmap$map,
                                                spatialAttribute = "tfl")
    names(samplesites_loc_tfl) <- c("SITE_IDENTIFIER", "TFL", "TFL_LICENCEE")
    samplesites <- merge(samplesites, samplesites_loc_tfl,
                         by = "SITE_IDENTIFIER", all.x = TRUE)
  }

  if(dir(mapPath, pattern = "Ownership_") %in% previousMapVer$mapFile){ # owner map does not change
    samples_skip_own <- samples_skip[,.(SITE_IDENTIFIER,
                                        OWNER = OWNER_prev,
                                        SCHEDULE = SCHEDULE_prev,
                                        OWNERSHIP_DESCRIPTION = OWNERSHIP_DESCRIPTION_prev)]
    if(nrow(samples_proc) > 0){
      ownermap <- readRDS(dir(mapPath, "Ownership_", full.names = TRUE))
      samplesites_loc_own <- FAIBBase::getSpatial(pointID = samples_proc$SITE_IDENTIFIER,
                                                  zone = samples_proc$IP_UTM,
                                                  northing = samples_proc$IP_NRTH,
                                                  easting = samples_proc$IP_EAST,
                                                  spatialMap = ownermap$map,
                                                  spatialAttribute = "OWNERSHIP")
      names(samplesites_loc_own)[1] <- c("SITE_IDENTIFIER")
      samples_skip_own <- rbind(samples_skip_own, samplesites_loc_own)
    }
    samplesites <- merge(samplesites, samples_skip_own,
                         by = "SITE_IDENTIFIER", all.x = TRUE)
  } else { # owner map changed
    ownermap <- readRDS(dir(mapPath, "Ownership_", full.names = TRUE))
    samplesites_loc_own <- FAIBBase::getSpatial(pointID = samples_proc_all$SITE_IDENTIFIER,
                                                zone = samples_proc_all$IP_UTM,
                                                northing = samples_proc_all$IP_NRTH,
                                                easting = samples_proc_all$IP_EAST,
                                                spatialMap = ownermap$map,
                                                spatialAttribute = "OWNERSHIP")
    names(samplesites_loc_own)[1] <- c("SITE_IDENTIFIER")
    samplesites <- merge(samplesites, samplesites_loc_own,
                         by = "SITE_IDENTIFIER", all.x = TRUE)
  }
  return(samplesites)
}
