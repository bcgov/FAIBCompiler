#' Estimate height using height-DBH curves
#'
#' @description Estimate height using height-DBH curves by bec subzone and species
#'
#' @param beczone character, BEC zone.
#'
#' @param subzone character, BEC subzone.
#'
#' @param species character, Species.
#'
#' @param DBH numeric, Diameter at breast height.
#'
#' @param heightModels data.table, Specifies the best model and coefficients by each
#'                                 BEC subzone and species.
#'
#' @return projected total height
#'
#' @importFrom data.table ':='
#' @importFrom dplyr left_join
#' @importFrom fpCompare %>>% %<<% %==% %!=% %<=% %>=%
#' @importFrom FAIBBase heightEstimateForBTOP_D heightEstimateForBTOP_H treeVolCalculator
#'
#'
#' @export
#' @docType methods
#' @rdname heightEstimate_byHeightModel
#'
#' @author Yong Luo
heightEstimate_byHeightModel<- function(beczone, subzone, species,
                                        DBH, heightModels){
  worktable <- data.table(tempID = 1:length(beczone),
                          BGC_ZONE = beczone,
                          BGC_SBZN = subzone,
                          SPECIES = species,
                          DBH = DBH)

  specieslookup <- lookup_species()

  specieslookup <- unique(specieslookup[,.(SPECIES, SP0, SP_TYPE)])
  worktable <- merge(worktable, specieslookup,
                     by = "SPECIES", all.x = TRUE)

  worktable <- merge(worktable,
                     heightModels,
                     by = c("BGC_ZONE", "BGC_SBZN", "SPECIES",
                            "SP0", "SP_TYPE"),
                     all.x = TRUE)

  worktable_pass <- worktable[!is.na(bestModel),]
  worktable_fail <- worktable[is.na(bestModel),
                              .(tempID, SPECIES, SP0, SP_TYPE, DBH)]
  ## find in the same bec zone and with same species
  heightModels_species <- unique(heightModels[GroupMethod == "species",],
                                 by = c("SPECIES"))

  worktable_fail <- merge(worktable_fail,
                          heightModels_species,
                          by = c("SPECIES", "SP0", "SP_TYPE"),
                          all.x = TRUE)
  worktable_pass <- rbindlist(list(worktable_pass,
                                   worktable_fail[!is.na(GroupMethod),]),
                              fill = TRUE)
  worktable_fail <- worktable_fail[is.na(GroupMethod),
                                   .(tempID, SP0, SP_TYPE, DBH)]
  ## find in same sp0
  heightModels_sp0 <- unique(heightModels[GroupMethod == "sp0",],
                             by = c("SP0"))
  worktable_fail <- merge(worktable_fail,
                          heightModels_sp0,
                          by = c("SP0", "SP_TYPE"),
                          all.x = TRUE)
  worktable_pass <- rbindlist(list(worktable_pass,
                                   worktable_fail[!is.na(GroupMethod),]),
                              fill = TRUE)

  ## by species type
  worktable_fail <- worktable_fail[is.na(GroupMethod),
                                   .(tempID, SP_TYPE, DBH)]
  heightModels_sp_type <- unique(heightModels[GroupMethod == "species type",],
                                 by = "SP_TYPE")
  worktable_fail <- merge(worktable_fail,
                          heightModels_sp_type,
                          by = "SP_TYPE",
                          all.x = TRUE)
  worktable_pass <- rbindlist(list(worktable_pass,
                                   worktable_fail),
                              fill = TRUE)

  worktable_pass[bestModel == "Chapman-Richards",
                 ':='(height_projected = b1 * (1 - exp(-b2*DBH))^b3)]
  worktable_pass[bestModel == "Korf",
                 ':='(height_projected = 1.3 + b1 * exp((-b2)*(DBH^(-b3))))]
  worktable_pass[bestModel == "Logistic",
                 ':='(height_projected = 1.3 + b1 /(1 + b2*exp(-b3*DBH)))]
  worktable_pass[bestModel == "Weibull",
                 ':='(height_projected = 1.3 + b1 * (1 - exp(-b2*(DBH^b3))))]

  worktable_pass <- worktable_pass[order(tempID),]
  return(worktable_pass$height_projected)
}
