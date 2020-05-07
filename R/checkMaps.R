#' Load maps from map source
#'
#' @description This function is to load maps from
#'              \code{mapSourcePath} and save them to \code{mapPath}.
#'              For TSA and BEC, the maps are direct from bcmaps package.
#'
#' @param mapSourcePath character, Path to map source. The compilation will take the
#'        actively maintained and updated map folder from
#'        Edward Fong.
#' @param mapPath character, The path to save all the maps.
#'                Note that all the saved maps have time
#'                stemps, which suggest when the files have been
#'                modified in \code{mapSourcePath}.
#'
#'
#' @return A list of time, which will be a unique time mark
#'         when compiler does spatial algorithms.
#'
#' @note TSA and BEC maps are not loaded from source path.
#'       bcmaps package is used to load those maps. Therefore,
#'       the time mark for those maps are current time.
#'       When multiple spatial files (e.g., gdb) are found for
#'       a map, the most recent modified one will be used.
#'
#' @export
#' @docType methods
#' @rdname checkMaps
#' @author Yong Luo
checkMaps <- function(mapSourcePath, mapPath){
  currenttime <- Sys.time()
  currenttime <- gsub("-", "", currenttime)
  currenttime <- gsub(" ", "", currenttime)
  currenttime <- gsub(":", "", currenttime)
  cat("    Check maps in mapSourcePath:\n")
  cat("        Map TSA skiped, will use the one from bcmaps.\n")
  ## remove existing tsa map
  existingTSAmap <- dir(mapPath, pattern = "TSA_", full.names = TRUE)
  aa <- suppressWarnings(file.remove(existingTSAmap))
  rm(aa, existingTSAmap)
  tsamap <- bcmaps::tsa(class = "sp")
  saveRDS(tsamap,
          file.path(mapPath, paste0("TSA_", currenttime, ".rds")))
  cat(paste0("          Archived as TSA_", currenttime,
             ".rds.\n"))
  cat("        Map BEC skiped, will use the one from bcmaps.\n")
  ## remove existing bec map
  existingBECmap <- dir(mapPath, pattern = "BEC_", full.names = TRUE)
  aa <- suppressWarnings(file.remove(existingBECmap))
  rm(aa, existingBECmap)
  becmap <- bcmaps::bec(class = "sp")
  saveRDS(becmap,
          file.path(mapPath, paste0("BEC_", currenttime, ".rds")))
  cat(paste0("          Archived as BEC_", currenttime,
             ".rds.\n"))

  ## for tfl
  cat("        Check TFL map:\n")
  alltflfiles <- dir(mapSourcePath, pattern = "TFL__Overview",
                     full.names = TRUE)
  ## confirmed with Edward, select folders
  alltflfiles <- alltflfiles[file_test("-d", alltflfiles)]
  moditime <- file.info(alltflfiles)$mtime
  mostRecentTFL <- alltflfiles[which.max(moditime)]
  if(length(mostRecentTFL) > 1){
    stop("There are multiple TFL maps at the most recent time.
       Please check with Edward Fong.")
  } else {
    mostRecentTime_TFL <- max(moditime)
    mostRecentTime_TFL <- gsub("-", "", mostRecentTime_TFL)
    mostRecentTime_TFL <- gsub(" ", "", mostRecentTime_TFL)
    mostRecentTime_TFL <- gsub(":", "", mostRecentTime_TFL)

    tfl_mapPath <- dir(mapPath, pattern = "TFL_")
    tfl_dates <- gsub("TFL_", "", tfl_mapPath)
    tfl_dates <- gsub(".rds", "", tfl_dates)
    if(length(tfl_dates) > 0){
      tfl_dates_last <- max(tfl_dates)
    } else {
      tfl_dates_last <- 0 # that means there is no existing tfl maps in mappath
    }

    if(mostRecentTime_TFL <= tfl_dates_last){
      tfl_time_final <- tfl_dates_last
    } else {
      tfl_map_final <- raster::shapefile(file.path(mostRecentTFL, "TFL_overview.shp"))
      saveRDS(tfl_map_final,
              file.path(mapPath, paste0("TFL_", mostRecentTime_TFL, ".rds")))
      tfl_time_final <- mostRecentTime_TFL
    }

    existingTFLmap <- dir(mapPath, pattern = "TFL_")
    existingTFLmap_rm <- existingTFLmap[existingTFLmap != paste0("TFL_", mostRecentTime_TFL, ".rds")]
    if(length(existingTFLmap_rm) > 0){
    aa <- suppressWarnings(file.remove(file.path(mapPath, existingTFLmap_rm)))
      rm(aa)
    }
    rm(existingTFLmap, existingTFLmap_rm)

    cat(paste0("          TFL map @ ",
               tfl_time_final,
               " is used for compilation.\n"))
  }
  rm(moditime)


  ## for FIZ
  cat("        Check FIZ map:\n")
  allfizfiles <- dir(mapSourcePath, pattern = "FIZ",
                     full.names = TRUE)
  ## confirmed with Edward, select folders
  allfizfiles <- allfizfiles[file_test("-d", allfizfiles)]
  moditime_tmp <- lapply(allfizfiles,
                         function(s){max(file.info(dir(s, full.names = TRUE))$mtime)})
  moditime <- moditime_tmp[[1]]
  if(length(moditime_tmp) > 1){
    for (i in 2:length(moditime_tmp)) {
      moditime <- c(moditime, moditime_tmp[[i]])
    }

  }

  mostRecentFIZ <- allfizfiles[which.max(moditime)]
  if(length(mostRecentFIZ) > 1){
    stop("There are multiple FIZ maps at the most recent time.
       Please check with Edward Fong.")
  } else {
    mostRecentTime_FIZ <- max(moditime)
    mostRecentTime_FIZ <- gsub("-", "", mostRecentTime_FIZ)
    mostRecentTime_FIZ <- gsub(" ", "", mostRecentTime_FIZ)
    mostRecentTime_FIZ <- gsub(":", "", mostRecentTime_FIZ)

    fiz_mapPath <- dir(mapPath, pattern = "FIZ_")
    fiz_dates <- gsub("FIZ_", "", fiz_mapPath)
    fiz_dates <- gsub(".rds", "", fiz_dates)
    if(length(fiz_dates) > 0){
      fiz_dates_last <- max(fiz_dates)
    } else {
      fiz_dates_last <- 0
    }

    if(mostRecentTime_FIZ <= fiz_dates_last){
      fiz_time_final <- fiz_dates_last
    } else {
      fiz_map_final <- rgdal::readOGR(dsn = mostRecentFIZ,
                                      layer = "forest_inventory_zone",
                                      verbose = FALSE)
      saveRDS(fiz_map_final,
              file.path(mapPath, paste0("FIZ_", mostRecentTime_FIZ, ".rds")))
      fiz_time_final <- mostRecentTime_FIZ
    }

    existingFIZmap <- dir(mapPath, pattern = "FIZ_")
    existingFIZmap_rm <- existingFIZmap[existingFIZmap != paste0("FIZ_", mostRecentTime_FIZ, ".rds")]
    if(length(existingFIZmap_rm) > 0){
      aa <- suppressWarnings(file.remove(file.path(mapPath, existingFIZmap_rm)))
      rm(aa)
    }
    rm(existingFIZmap, existingFIZmap_rm)

    cat(paste0("          FIZ map @ ",
               fiz_time_final,
               " is used for compilation.\n"))
  }






  ## for OWNERSHIP
  cat("        Check OWNERSHIP map:\n")
  allownerfiles <- dir(mapSourcePath,
                       pattern = "Ownership",
                       full.names = TRUE)
  allownerfiles <- allownerfiles[substr(allownerfiles,
                                        nchar(allownerfiles)-2,
                                        nchar(allownerfiles)) == "gdb"]

  ## confirmed with Edward, select folders
  moditime_tmp <- lapply(allownerfiles,
                         function(s){max(file.info(dir(s, full.names = TRUE))$mtime)})

  moditime <- moditime_tmp[[1]]
  if(length(moditime_tmp) > 1){
    for (i in 2:length(moditime_tmp)) {
      moditime <- c(moditime, moditime_tmp[[i]])
    }
  }

  mostRecentOWNER <- allownerfiles[which.max(moditime)]
  if(length(mostRecentOWNER) > 1){
    stop("There are multiple OWNER maps at the most recent time.
       Please check with Edward Fong.")
  } else {
    mostRecentTime_OWNER <- max(moditime)
    mostRecentTime_OWNER <- gsub("-", "", mostRecentTime_OWNER)
    mostRecentTime_OWNER <- gsub(" ", "", mostRecentTime_OWNER)
    mostRecentTime_OWNER <- gsub(":", "", mostRecentTime_OWNER)
    owner_mapPath <- dir(mapPath, pattern = "OWNER_")
    owner_dates <- gsub("OWNER_", "", owner_mapPath)
    owner_dates <- gsub(".rds", "", owner_dates)
    if(length(owner_dates) > 0){
      owner_dates_last <- max(owner_dates)
    } else {
      owner_dates_last <- 0
    }

    if(mostRecentTime_OWNER <= owner_dates_last){
      owner_time_final <- owner_dates_last
    } else {
      owner_map_final <- rgdal::readOGR(dsn = mostRecentOWNER,
                                        layer = "F_OWN",
                                        verbose = FALSE)
      saveRDS(owner_map_final,
              file.path(mapPath, paste0("OWNER_", mostRecentTime_OWNER, ".rds")))
      owner_time_final <- mostRecentTime_OWNER
    }
    cat(paste0("          OWNERSHIP map @ ",
               owner_time_final,
               " is used for compilation.\n"))

    existingOWNERmap <- dir(mapPath, pattern = "OWNER_")
    existingOWNERmap_rm <- existingOWNERmap[existingOWNERmap != paste0("OWNER_", mostRecentTime_OWNER, ".rds")]
    if(length(existingOWNERmap_rm) > 0){
      aa <- suppressWarnings(file.remove(file.path(mapPath, existingOWNERmap_rm)))
      rm(aa)
    }
    rm(existingOWNERmap, existingOWNERmap_rm)
  }
  return(list(currenttime = currenttime,
              fiz_time = fiz_time_final,
              tfl_time = tfl_time_final,
              ownership_time = owner_time_final))
}
