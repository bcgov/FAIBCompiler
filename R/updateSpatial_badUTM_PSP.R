#' Used for updating spatial attributes for the sites with bad utm
#'
#'
#' @description Used for updating spatial attributes for the sites with bad utm.
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
#' @note This routine is based on discussion with Dan and Anya on 2023-10-05
#'
#' @importFrom FAIBBase UTM_Convertor getSpatial
#' @export
#' @author Yong Luo
updateSpatial_badUTM_PSP <- function(mapPath,
                                     samplesites){
  # to update sp attributes lookup table based on region and compartment
  regionCompartMap <- sf::st_read(file.path(mapPath, "FADM_REGION_COMPARTMENT",
                                            "FADM_RGN_C_polygon.shp"))
  if(!file.exists(file.path(mapPath, "spatiallookup_regioncompartment.rds"))){
    spAttr_regionCompartment <- list()
    mapversion <- NULL
    for (indisp in c("BEC", "TSA", "TFL", "Ownership", "FIZ")) {
      ## for bec update
      mapname <- dir(mapPath, pattern = paste0(indisp, "_map"))
      mapname <- data.table(mapversion = gsub(".rds", "", mapname))
      mapversion <- rbind(mapversion, mapname)
      spmap <- readRDS(dir(mapPath, pattern = paste0(indisp, "_map"),
                           full.names = TRUE))
      spattri_lookup <- spAttriFromRegCompt(regionCompartMap = regionCompartMap,
                                            spAttributeMap = spmap$map,
                                            mapName = indisp)
      spAttr_regionCompartment[[indisp]] <- spattri_lookup
      rm(mapname, spattri_lookup, spmap)
    }
    spAttr_regionCompartment[["mapversion"]] <- mapversion
    saveRDS(spAttr_regionCompartment,
            file.path(mapPath, "spatiallookup_regioncompartment.rds"))
    rm(mapversion)
  } else {
    spAttr_regionCompartment <- readRDS(file.path(mapPath,
                                                  "spatiallookup_regioncompartment.rds"))
    for (indisp in c("BEC", "TSA", "TFL", "Ownership", "FIZ")) {
      ## check map version from lookup table
      mapname <- dir(mapPath, pattern = paste0(indisp, "_map"))
      mapname <- gsub(".rds", "", mapname)
      if(!(mapname %in% spAttr_regionCompartment$mapversion$mapversion)){ # map version does not match
        spmap <- readRDS(dir(mapPath, pattern = paste0(indisp, "_map"),
                             full.names = TRUE))
        spattri_lookup <- spAttriFromRegCompt(regionCompartMap = regionCompartMap,
                                              spAttributeMap = spmap$map,
                                              mapName = indisp)
        spAttr_regionCompartment[[indisp]] <- spattri_lookup # replace the old with new
        spAttr_regionCompartment$mapversion[grepl(indisp, mapversion),
                                            mapversion := mapname] # update version
        rm(spmap, spattri_lookup)
      }
    }
    saveRDS(spAttr_regionCompartment,
            file.path(mapPath, "spatiallookup_regioncompartment.rds"))
  }
  rm(regionCompartMap, indisp)
  samplesites[SITE_IDENTIFIER == 4038129,
                  ':='(BEC_SUBZONE_ISMC = "wk",
                       BEC_VAR_ISMC = 1)]
  samplesites[SITE_IDENTIFIER == 4011360,
                  ':='(BEC_SUBZONE_ISMC = "",
                       BEC_VAR_ISMC = "")]

  becmap <- readRDS(dir(mapPath, pattern = "BEC_map", full.names = TRUE))
  becmaptable <- data.frame(becmap$map) %>% data.table

  samplesites[is.na(BEC_ZONE_ISMC), BEC_ZONE_ISMC := ""]
  samplesites[is.na(BEC_SUBZONE_ISMC), BEC_SUBZONE_ISMC := ""]
  samplesites[is.na(BEC_VAR_ISMC), BEC_VAR_ISMC := ""]
  samplesites[, ':='(BECLABEL_ISMC_var = paste0(BEC_ZONE_ISMC, BEC_SUBZONE_ISMC, BEC_VAR_ISMC),
                     BECLABEL_ISMC_subzone = paste0(BEC_ZONE_ISMC, BEC_SUBZONE_ISMC))]

  becmaptable[is.na(VARIANT), VARIANT := ""]
  becmaptable[, ':='(BECLABEL_var = paste0(ZONE, SUBZONE, VARIANT),
                     BECLABEL_subzone = paste0(ZONE, SUBZONE))]

  # 1 attempt using valid ismc bec labels at var level
  samplesites[BECLABEL_ISMC_var %in% becmaptable$BECLABEL_var &
                is.na(BEC_SOURCE),
                      ':='(BEC = BEC_ZONE_ISMC,
                           BEC_SBZ = BEC_SUBZONE_ISMC,
                           BEC_VAR = BEC_VAR_ISMC,
                           BEC_SOURCE = 2)]

  # 2 attempt using valid ismc bec labels at subzone level
  samplesites[BECLABEL_ISMC_subzone %in% becmaptable$BECLABEL_subzone &
                is.na(BEC_SOURCE),
              ':='(BEC = BEC_ZONE_ISMC,
                   BEC_SBZ = BEC_SUBZONE_ISMC,
                   BEC_SOURCE = 2)]
  # 3 attempt using region and compartment number to find only
  # one bec and becsub zone
  # note: the region compartment and bec lookup table is
  # derived by overlay region compartment map and bec map
  # conservatively, only region/compartment that has one bec+becsubzone is used
  # in other words, the area of one region/compartment is in a bec/becsubzone area
  samplesites <- merge(samplesites,
                       spAttr_regionCompartment$BEC,
                       by = c("SAMPLING_REGION_NUMBER", "COMPARTMENT_NUMBER"),
                       all.x = TRUE)
  samplesites[is.na(BEC_SOURCE) & !is.na(BEC_rcp),
              ':='(BEC = BEC_rcp,
                   BEC_SBZ = BEC_sbz_rcp,
                   BEC_SOURCE = 3)] # marked this method as 3
  samplesites[,':='(BEC_rcp = NULL,
                    BEC_sbz_rcp = NULL)]

  # 4 attempt using ismc bec zone but no subzone information
  # populate subzone by using guessed subzone based the largest subzone
  # for that bec zone
  beclabelsmry <- becmaptable[,.(ZONE = unique(ZONE),
                                 SUBZONE = unique(SUBZONE),
                                 area_total = sum(FEATURE_AREA)),
                              by = "BECLABEL_subzone"]

  beclabelsmry <- beclabelsmry[order(ZONE, -area_total),]

  biggest_subzone_byzone <- unique(beclabelsmry, by = "ZONE")
  samplesites <- merge(samplesites,
                               biggest_subzone_byzone[,.(BEC_ZONE_ISMC = ZONE,
                                                         SUBZONE)],
                               by = "BEC_ZONE_ISMC",
                               all.x = TRUE)
  samplesites[is.na(BEC_SOURCE) & !is.na(SUBZONE),
                      ':='(BEC = BEC_ZONE_ISMC,
                           BEC_SBZ = SUBZONE,
                           BEC_SOURCE = 4)] # use ismc bec zone, subzone is the largest
                                            # subzone in that bec zone

  samplesites[,':='(BEC_ZONE_ISMC = NULL,
                    BEC_SUBZONE_ISMC = NULL,
                    BEC_VAR_ISMC = NULL,
                    BECLABEL_ISMC_subzone = NULL,
                    BECLABEL_ISMC_var = NULL,
                    SUBZONE = NULL)]


  # there still some sites do not have bec information
  # to populate that, using the bec+subzone that have most sites for a
  # given region+compartment
  regioncompartsmry <- unique(samplesites[!is.na(BEC_SOURCE),
                                           .(SITE_IDENTIFIER,
                                             SAMPLING_REGION_NUMBER,
                                             COMPARTMENT_NUMBER,
                                             BEC, BEC_SBZ)],
                              by = "SITE_IDENTIFIER")

  regioncompartsmry <- regioncompartsmry[,.(Nosites = length(SITE_IDENTIFIER)),
                                           by = c("SAMPLING_REGION_NUMBER",
                                                  "COMPARTMENT_NUMBER",
                                                  "BEC", "BEC_SBZ")]
  regioncompartsmry <- regioncompartsmry[order(SAMPLING_REGION_NUMBER,
                                               COMPARTMENT_NUMBER,
                                               -Nosites),]
  regioncompartsmry <- unique(regioncompartsmry,
                              by = c("SAMPLING_REGION_NUMBER",
                                     "COMPARTMENT_NUMBER"))
  samplesites <- merge(samplesites,
                               regioncompartsmry[,.(SAMPLING_REGION_NUMBER,
                                                    COMPARTMENT_NUMBER,
                                                    BEC_new = BEC,
                                                    BEC_SBZ_new = BEC_SBZ)],
                               by = c("SAMPLING_REGION_NUMBER",
                                      "COMPARTMENT_NUMBER"),
                               all.x = TRUE)
  samplesites[is.na(BEC_SOURCE) & !is.na(BEC_new),
                      ':='(BEC = BEC_new,
                           BEC_SBZ = BEC_SBZ_new,
                          BEC_SOURCE = 5)]
  samplesites[,':='(BEC_new = NULL,
                            BEC_SBZ_new = NULL)]
  rm(regioncompartsmry)


  # for TSA
  tsamap <- readRDS(dir(mapPath, pattern = "TSA_map", full.names = TRUE))
  samplesites[is.na(TSA) & !is.na(TSA_ISMC),
              ':='(TSA = TSA_ISMC)] # update from ISMC data
  # update from region compartment
  # from region compartment lookup table
  # one on one match
  samplesites <- merge(samplesites,
                       spAttr_regionCompartment$TSA,
                       by = c("SAMPLING_REGION_NUMBER", "COMPARTMENT_NUMBER"),
                       all.x = TRUE)
  samplesites[is.na(TSA) & !is.na(TSA_rcp),
              ':='(TSA = TSA_rcp,
                   TSA_DESC = TSA_dsc_rcp)]
  samplesites[, ':='(TSA_rcp = NULL,
                     TSA_dsc_rcp = NULL)]

  # from region compartment which contains the most sites
  regioncompartsmry <- unique(samplesites[!is.na(TSA),
                                          .(SITE_IDENTIFIER,
                                            SAMPLING_REGION_NUMBER,
                                            COMPARTMENT_NUMBER,
                                            TSA)],
                              by = "SITE_IDENTIFIER")

  regioncompartsmry <- regioncompartsmry[,.(Nosites = length(SITE_IDENTIFIER)),
                                         by = c("SAMPLING_REGION_NUMBER",
                                                "COMPARTMENT_NUMBER",
                                                "TSA")]
  regioncompartsmry <- regioncompartsmry[order(SAMPLING_REGION_NUMBER,
                                               COMPARTMENT_NUMBER,
                                               -Nosites),]
  regioncompartsmry <- unique(regioncompartsmry,
                              by = c("SAMPLING_REGION_NUMBER",
                                     "COMPARTMENT_NUMBER"))
  samplesites <- merge(samplesites,
                       regioncompartsmry[,.(SAMPLING_REGION_NUMBER,
                                            COMPARTMENT_NUMBER,
                                            TSA_new = TSA)],
                       by = c("SAMPLING_REGION_NUMBER",
                              "COMPARTMENT_NUMBER"),
                       all.x = TRUE)
  samplesites[is.na(TSA) & !is.na(TSA_new),
              ':='(TSA = TSA_new)]
  samplesites[,':='(TSA_new = NULL)]

  rm(regioncompartsmry)
  tsamap_table <- data.frame(tsamap$map) %>% data.table
  tsamap_table <- unique(tsamap_table[,.(TSA = TSA_NUMBER,
                                  TSA_NUMBER_DESCRIPTION)])
  samplesites <- merge(samplesites,
                       tsamap_table,
                       by = "TSA",
                       all.x = TRUE)
  samplesites[is.na(TSA_DESC) & !is.na(TSA_NUMBER_DESCRIPTION),
              TSA_DESC := TSA_NUMBER_DESCRIPTION]
  samplesites[, TSA_NUMBER_DESCRIPTION := NULL]

  ## for tfl
  samplesites[is.na(TFL) & !is.na(TFL_ISMC),
                      ':='(TFL = TFL_ISMC)]


  ## for fiz
  samplesites[is.na(FIZ) & !is.na(FIZ_ISMC),
                      FIZ := FIZ_ISMC]

  return(samplesites)
}


