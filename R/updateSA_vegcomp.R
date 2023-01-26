#' Update projected stand age from veg comp rank 1 layer
#'
#'
#' @description This function is to update projected stand age from vegcomp rank 1 layer.
#'
#' @param compilationType character, either \code{PSP} or \code{nonPSP}. If it is \code{PSP}, it
#'                               is consistent with original PSP compiler, otherwise, it
#'                               is consistent with VRI compiler.
#' @param coeffPath character, Specifies the path dependent coeffs are stored.
#' @param bcgwUserName character, User name to access to bcgw database.
#' @param bcgwPassword character, Password to access to bcgw database.
#' @param sampleSites data.table, A data table must contain site_identifier, bc albers coordinates.
#' @return A data table with feature_id, proj_age_1 and projected_date.
#'
#' @importFrom data.table ':='
#' @importFrom dplyr '%>%'
#' @importFrom FAIBBase merge_dupUpdate
#' @importFrom FAIBOracle projDate_rank1Layer extractVegComp_byLoc
#'
#' @export
#' @docType methods
#' @rdname updateSA_vegcomp
#'
#' @author Yong Luo
updateSA_vegcomp <- function(compilationType,
                             coeffPath,
                             bcgwUserName,
                             bcgwPassword,
                             sampleSites){
  need_extract <- FALSE
  additional <- FALSE
  vegcompversion <- projDate_rank1Layer(bcgwUserName = bcgwUserName,
                                        bcgwPassword = bcgwPassword)
  if(!file.exists(file.path(coeffPath,
                            paste0("SA_VEGCOMP_", compilationType, ".rds")))){
    need_extract <- TRUE
    sampleSites_valid <- sampleSites[!is.na(BC_ALBERS_X) &
                                                     !is.na(BC_ALBERS_Y),]
  } else {
    sample_site_valid_all_prev <- readRDS(file.path(coeffPath,
                                                    paste0("SA_VEGCOMP_", compilationType, ".rds")))
    if(sample_site_valid_all_prev$vegCompVersion != vegcompversion){
      need_extract <- TRUE
      sampleSites_valid <- sampleSites[!(is.na(BC_ALBERS_X) &
                                                         is.na(BC_ALBERS_Y)),]
    } else {
      sampleSites_valid_prev <- sample_site_valid_all_prev$SAFromVegComp
      sampleSites <- merge(sampleSites,
                                  sampleSites_valid_prev,
                                  by = "SITE_IDENTIFIER",
                                  all.x = TRUE)
      sampleSites[is.na(x_BCAlbers), x_BCAlbers := -1] # force NAs to -1
      sampleSites[is.na(y_BCAlbers), y_BCAlbers := -1] # force NAs to -1
      sampleSites_valid <- sampleSites[!(is.na(BC_ALBERS_X) &
                                                         is.na(BC_ALBERS_Y)) &
                                                       !(BC_ALBERS_X == x_BCAlbers &
                                                           BC_ALBERS_Y == y_BCAlbers),]
      sampleSites[,':='(x_BCAlbers = NULL,
                               y_BCAlbers = NULL)]
      if(nrow(sampleSites_valid) > 0){
        need_extract <- TRUE
        additional <- TRUE
        sampleSites_valid_prev_keep <- sampleSites_valid_prev[!(SITE_IDENTIFIER %in% sampleSites_valid$SITE_IDENTIFIER),]
        sampleSites[,':='(FEATURE_ID = NULL,
                                 PROJ_AGE_1 = NULL,
                                 PROJECTED_DATE = NULL)]
      }
    }
  }
  if(need_extract){
    sampleSites_valid <- extractVegComp_byLoc(bcgwUserName = bcgwUserName,
                                                     bcgwPassword = bcgwPassword,
                                                     locationID = sampleSites_valid$SITE_IDENTIFIER,
                                                     x_BCAlbers = sampleSites_valid$BC_ALBERS_X,
                                                     y_BCAlbers = sampleSites_valid$BC_ALBERS_Y,
                                                     extractAttributes = c("Feature_ID", "PROJ_AGE_1", "PROJECTED_DATE"))
    sampleSites_valid <- sampleSites_valid[,.(SITE_IDENTIFIER = locationID,
                                                            x_BCAlbers, y_BCAlbers,
                                                            FEATURE_ID, PROJ_AGE_1, PROJECTED_DATE)]
    if(additional){
      sampleSites_valid <- rbind(sampleSites_valid,
                                        sampleSites_valid_prev_keep)
    }
    sampleSites <- merge(sampleSites,
                                sampleSites_valid[,.(SITE_IDENTIFIER, FEATURE_ID,
                                                            PROJ_AGE_1, PROJECTED_DATE)],
                                by = "SITE_IDENTIFIER",
                                all.x = TRUE)
    saveRDS(list(vegCompVersion = vegcompversion,
                 SAFromVegComp = sampleSites_valid),
            file.path(coeffPath,
                      paste0("SA_VEGCOMP_", compilationType, ".rds")))
    rm(sampleSites_valid, vegcompversion)
  }
return(sampleSites)
}


