#' Load maps from map source
#'
#' @description This function is to load maps from
#'              \code{mapSourcePath} and save them to \code{mapPath}.
#'              For TSA and BEC, the maps are direct from bcmaps package.
#'
#' @param mapPath character, The path to save all the maps.
#'                Note that all the saved maps have time
#'                stemps, which suggest when the files have been
#'                modified in \code{mapSourcePath}.
#'
#'
#' @return no value is returned from this function.
#'
#' @note all the maps relies on the bcdata package.
#' @importFrom bcdata bcdc_get_record bcdc_get_data
#' @export
#' @docType methods
#' @rdname checkMaps
#' @author Yong Luo
checkMaps <- function(mapPath){
  ## from April 2021, all the maps will be checked and downloaded from BC Data catalogue
  ## using bcdata package
  cat("    Check maps from BC Data Catalogue:\n")

  TSA_id <- "8daa29da-d7f4-401c-83ae-d962e3a28980"
  BEC_id <- "f358a53b-ffde-4830-a325-a5a03ff672c3"
  TFL_id <- "454f2153-efbd-4a6e-8966-a6d9755da9a6"
  FIZ_id <- "67e95c68-c1ef-4363-b351-0dfead151122"
  Ownership_id <- "5fc4e8ce-dd1d-44fd-af17-e0789cf65e4e"

  for (indiMapName in c("TSA", "BEC", "TFL", "FIZ", "Ownership")) {
    existingmap <- dir(mapPath, pattern = indiMapName, full.names = FALSE)
    bcdata_id <- get(paste0(indiMapName, "_id"))
    map_metadata <- bcdata::bcdc_get_record(bcdata_id)
    last_date <- gsub("-", "", map_metadata$record_last_modified)
    if(length(existingmap) == 0){ # their is no existing map in the mapPath
      cat(paste0("        There is no existing ", indiMapName, " map. The one modified on ", last_date, " will be downloaded and used in this compilation.\n"))
      themap <- bcdata::bcdc_get_data(bcdata_id)
      saveRDS(list("map" = themap, "metadata" = map_metadata),
              file.path(mapPath, paste0(indiMapName, "_map", last_date, ".rds")))
      cat(paste0("            The ", indiMapName, " map has been downloaded and saved as ", indiMapName, "_", last_date, ".rds\n"))

    } else { # this is tsa map in the path
      existingmap_date <- gsub(paste0(indiMapName, "_map"), "", existingmap)
      existingmap_date <- max(as.numeric(gsub(".rds", "", existingmap_date)))
      if(existingmap_date == last_date){ ## the map is the most recent one from bc data
        cat(paste0("        The existing ", indiMapName, " map is the most recent one, and will be used.\n"))
      } else if (existingmap_date < last_date) {## the existing map is not most recent one from bc data
        wantupdate <- readline(paste0("The ", indiMapName, " map has been updated on ", last_date, ", use it? (Yes/No)"))
        if(toupper(wantupdate) %in% c("Y", "YES")){
          themap <- bcdata::bcdc_get_data(bcdata_id)
          saveRDS(list("map" = themap, "metadata" = map_metadata),
                  file.path(mapPath, paste0(indiMapName, "_", last_date, ".rds")))
          cat(paste0("            The ", indiMapName, " map has been downloaded and saved as ", indiMapName, "_", last_date, ".rds\n"))
          for (indifile in existingmap) { ## remove the previous maps
            unlink(file.path(mapPath, indifile), recursive = TRUE)
          }
        } else if (toupper(wantupdate) %in% c("N", "NO")){
          cat(paste0("        The existing ", indiMapName, " map modified on ", existingmap_date, " will be used.\n"))
        } else {
          stop("Incorrect input.")
        }
      } else {
        stop("The modified date for the current update may be wrong, cannot be earlier than the last update date.")
      }
    }
  }
}
