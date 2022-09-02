## to update missing spatial attributes using different method
## this function is specific to PSP compiler, in which some samples do not have
## good spatial coordinates, hence, spatial attributes can not be derived from spatial maps
updateMissingSpAttribute <- function(spatialtable,
                                     coeffPath,
                                     mapPath,
                                     updateMethod){
  if(updateMethod == "fromOldSAS"){
    spatialAttributes_fromSAS <- read.csv(file.path(coeffPath,
                                                    "spatialAttributes_fromSAS.csv")) %>%
      data.table
    spatialtable <- merge(spatialtable,
                          spatialAttributes_fromSAS,
                          by = "SITE_IDENTIFIER",
                          all.x = TRUE)
    spatialtable[is.na(BEC), ':='(BEC_ZONE = BGC_ZONE_sas,
                                  BEC_SBZ = BGC_SUBZONE_sas,
                                  BEC_VAR = BGC_VAR_sas,
                                  BEC_source = updateMethod)]
    spatialtable[, TSA_sas := as.character(TSA_sas)]
    spatialtable[nchar(TSA_sas) == 1,
                 TSA_sas := paste0("0", TSA_sas)]
    spatialtable[is.na(TSA), ':='(TSA = TSA_sas,
                                  TSA_DESC = MGMT_UNIT_sas,
                                  TSA_source = updateMethod)]
    spatialtable[is.na(FIZ), ':='(FIZ = FIZ_sas,
                                  FIZ_source = updateMethod)]
    spatialtable[,':='(BEC_ZONE_sas = NULL,
                       BGC_SUBZONE_sas = NULL,
                       BGC_VAR_sas = NULL,
                       OWNER_sas = NULL,
                       TSA_sas = NULL,
                       MGMT_UNIT_sas = NULL,
                       FIZ_sas = NULL)]
  } else if (updateMethod == "fromRegionCompartMap"){
    regionCompartMap <- sf::st_read(file.path(mapPath, "FADM_REGION_COMPARTMENT",
                                          "FADM_RGN_C_polygon.shp"))
    ## for bec update
    becmap <- readRDS(dir(mapPath, pattern = "BEC_map", full.names = TRUE))
    spatialAttributes_fromRC <- spAttriFromRegCompt(region = spatialtable$SAMPLING_REGION_NUMBER,
                                                    compartment = spatialtable$COMPARTMENT_NUMBER,
                                                    regionCompartMap = regionCompartMap,
                                                    spAttributeMap = becmap$map,
                                                    mapName = "BEC")
    spatialtable <- merge(spatialtable,
                          spatialAttributes_fromRC,
                          by = c("SAMPLING_REGION_NUMBER", "COMPARTMENT_NUMBER"),
                          all.x = TRUE)
    spatialtable[is.na(BEC) & !is.na(BEC_rcp),
                 ':='(BEC = BEC_rcp,
                      BEC_SBZ = BEC_sbz_rcp,
                      BEC_VAR = BEC_var_rcp,
                      BEC_source = updateMethod)]
    spatialtable_failed <- unique(spatialtable[is.na(BEC)]$SAMPLING_REGION_NUMBER)
    spatialtable_passed <- unique(spatialtable[SAMPLING_REGION_NUMBER %in% spatialtable_failed &
                                          !is.na(BEC_rcp),
                                        .(SAMPLING_REGION_NUMBER, BEC_rcp, BEC_sbz_rcp, BEC_var_rcp)])
    spatialtable_passed <- spatialtable_passed[, .(becrcp_length = length(BEC_sbz_rcp)),
                                               by = c("SAMPLING_REGION_NUMBER", "BEC_rcp")]
    spatialtable_passed <- spatialtable_passed[order(becrcp_length, decreasing = TRUE)]
    spatialtable_passed <- unique(spatialtable_passed[,.(SAMPLING_REGION_NUMBER, BEC_rcp)],
                                  by = "SAMPLING_REGION_NUMBER")
    spatialtable[,':='(BEC_rcp = NULL,
                       BEC_sbz_rcp = NULL,
                       BEC_var_rcp = NULL)]
    spatialtable <- merge(spatialtable,
                          spatialtable_passed,
                          by = "SAMPLING_REGION_NUMBER",
                          all.x = TRUE)
    spatialtable[is.na(BEC) & !is.na(BEC_rcp),
                 ':='(BEC = BEC_rcp,
                      BEC_SBZ = NA,
                      BEC_VAR = NA,
                      BEC_source = updateMethod)]
    spatialtable[,':='(BEC_rcp = NULL)]
    ## for tsa update
    tsamap <- readRDS(dir(mapPath, pattern = "TSA_map", full.names = TRUE))
    spatialAttributes_fromRC <- spAttriFromRegCompt(region = spatialtable$SAMPLING_REGION_NUMBER,
                                                    compartment = spatialtable$COMPARTMENT_NUMBER,
                                                    regionCompartMap = regionCompartMap,
                                                    spAttributeMap = tsamap$map,
                                                    mapName = "TSA")
    spatialtable <- merge(spatialtable,
                          spatialAttributes_fromRC,
                          by = c("SAMPLING_REGION_NUMBER", "COMPARTMENT_NUMBER"),
                          all.x = TRUE)
    spatialtable[is.na(TSA) & !is.na(TSA_rcp),
                 ':='(TSA = TSA_rcp,
                      TSA_DESC = TSA_dsc_rcp,
                      BEC_source = updateMethod)]
    spatialtable[,':='(TSA_rcp = NULL,
                       TSA_dsc_rcp = NULL)]
    ## for fiz update
    fizmap <- readRDS(dir(mapPath, pattern = "FIZ_map", full.names = TRUE))
    spatialAttributes_fromRC <- spAttriFromRegCompt(region = spatialtable$SAMPLING_REGION_NUMBER,
                                                    compartment = spatialtable$COMPARTMENT_NUMBER,
                                                    regionCompartMap = regionCompartMap,
                                                    spAttributeMap = fizmap$map,
                                                    mapName = "FIZ")
    spatialtable <- merge(spatialtable,
                          spatialAttributes_fromRC,
                          by = c("SAMPLING_REGION_NUMBER", "COMPARTMENT_NUMBER"),
                          all.x = TRUE)
    spatialtable[is.na(FIZ) & !is.na(FIZ_rcp),
                 ':='(FIZ = FIZ_rcp,
                      FIZ_source = updateMethod)]
    spatialtable[,':='(FIZ_rcp = NULL)]
    ## for fiz update
    tflmap <- readRDS(dir(mapPath, pattern = "TFL_map", full.names = TRUE))
    spatialAttributes_fromRC <- spAttriFromRegCompt(region = spatialtable$SAMPLING_REGION_NUMBER,
                                                    compartment = spatialtable$COMPARTMENT_NUMBER,
                                                    regionCompartMap = regionCompartMap,
                                                    spAttributeMap = tflmap$map,
                                                    mapName = "TFL")
    spatialtable <- merge(spatialtable,
                          spatialAttributes_fromRC,
                          by = c("SAMPLING_REGION_NUMBER", "COMPARTMENT_NUMBER"),
                          all.x = TRUE)
    spatialtable[is.na(TFL) & !is.na(TFL_rcp),
                 ':='(TFL = TFL_rcp,
                      TFL_source = updateMethod)]
    spatialtable[,':='(TFL_rcp = NULL)]
    ## for ownership update
    ownermap <- readRDS(dir(mapPath, pattern = "Ownership_map", full.names = TRUE))
    spatialAttributes_fromRC <- spAttriFromRegCompt(region = spatialtable$SAMPLING_REGION_NUMBER,
                                                    compartment = spatialtable$COMPARTMENT_NUMBER,
                                                    regionCompartMap = regionCompartMap,
                                                    spAttributeMap = ownermap$map,
                                                    mapName = "Ownership")
    spatialtable <- merge(spatialtable,
                          spatialAttributes_fromRC,
                          by = c("SAMPLING_REGION_NUMBER", "COMPARTMENT_NUMBER"),
                          all.x = TRUE)
    spatialtable[is.na(OWNER) & !is.na(OWNER_rcp),
                 ':='(OWNER = OWNER_rcp,
                      SCHEDULE = SCHEDULE_rcp,
                      OWNER_source = updateMethod)]
    spatialtable[,':='(OWNER_rcp = NULL,
                       SCHEDULE_rcp = NULL)]
  }
  return(spatialtable)
}

