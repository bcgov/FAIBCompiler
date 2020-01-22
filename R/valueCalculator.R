#' Calcualte tree value-VRI specific
#'
#' @description This function calculates tree value for each tree
#'              based on ground called grade. From the second column to the last column,
#'              the \code{grossVolMatrix} table should have same dimensions (i.e.,
#'              number of rows and columns) of \code{callGradeMatrix} and \code{grossMerchVolMatrix} if
#'              they are provided. Furthermore, be aware of the correspondingness among the matrix.
#'              In the function, two lookup table are hardcoded (i.e., \code{spv_spc} and \code{sp_cost}).
#'              This function is part of the \code{log_valu_2017.sas}.
#'
#' @param species character, Species codes in BC inventory system.
#'
#' @param grossVolMatrix data.table, Calculated gross volume for each log. The first column of this table is
#'                                   the volume for the stump. If missing, the function calculates the total
#'                                   net merchantable volume.
#'
#' @param grossMerchVolMatrix data.table, Calculated gross merchantable volume for each log. If missing, all the merchantable
#'                                        volume is assigned as 0.
#'
#' @param callGradeMatrix data.table, Ground call grading table.
#'
#' @return Data table that contains net value (\code{VAL_NET}) and net merchantable value (\code{VAL_MER}).
#'
#' @importFrom data.table ':=' data.table
#' @importFrom dplyr '%>%'
#' @importFrom FAIBBase merge_dupUpdate
#' @note
#'
#' @export
#' @docType methods
#' @rdname valueCalculator
#'
#' @author Yong Luo
#'
setGeneric("valueCalculator",
           function(species, grossVolMatrix, grossMerchVolMatrix, callGradeMatrix) {
             standardGeneric("valueCalculator")
           })

#' @rdname valueCalculator
setMethod(
  "valueCalculator",
  signature = c(species = "character",
                grossVolMatrix = "data.table",
                grossMerchVolMatrix = "data.table",
                callGradeMatrix = "data.table"),
  definition = function(species, grossVolMatrix, grossMerchVolMatrix, callGradeMatrix){
    if(nrow(grossMerchVolMatrix) == 0 & nrow(grossVolMatrix) == 0){
      stop("None of log volume and log merchantable volume is available.")
    }
    ## if just merchantable volume matrix is available
    if(nrow(grossVolMatrix) == 0){
      grossVolMatrix <- data.frame(matrix(data = rep(0, (nrow(grossMerchVolMatrix))*(1+ncol(grossMerchVolMatrix))),
                                          nrow = nrow(grossMerchVolMatrix))) %>% data.table
    }
    ## when just log volume matrix is available
    if(nrow(grossMerchVolMatrix) == 0){
      grossMerchVolMatrix <- data.frame(matrix(data = rep(0, (nrow(grossVolMatrix))*(ncol(grossVolMatrix) - 1)),
                                               nrow = nrow(grossVolMatrix))) %>% data.table
    }

    ## check matrix demensions
    if(nrow(grossVolMatrix) != nrow(grossMerchVolMatrix) |
       nrow(grossVolMatrix) != nrow(callGradeMatrix) |
       (ncol(grossVolMatrix)-1) != ncol(grossMerchVolMatrix) |
       (ncol(grossVolMatrix)-1) != ncol(callGradeMatrix)){
      stop("Demensions of log volume, log merchantable volume and sound factoring are not match.")
    }
    names(grossVolMatrix) <- paste("LOG_V_", 0:(ncol(grossMerchVolMatrix)), sep = "")
    names(grossMerchVolMatrix) <- paste("LOG_VM_", 1:(ncol(grossMerchVolMatrix)), sep = "")
    names(callGradeMatrix) <- paste("LOG_G_", 1:(ncol(grossMerchVolMatrix)), sep = "")
    nologs <- ncol(grossMerchVolMatrix)
    processData <- cbind(data.table(uniObsID = 1:nrow(grossVolMatrix),
                                    SPECIES = species),
                      grossVolMatrix, grossMerchVolMatrix, callGradeMatrix)

    rm(species, grossVolMatrix, grossMerchVolMatrix, callGradeMatrix)

    spvspcTable <- unique(lookup_species()[,.(SPECIES, SP_COST)], by = "SPECIES")
    processData <- FAIBBase::merge_dupUpdate(processData, spvspcTable, by = "SPECIES", all.x = TRUE)
    spcostTable <- lookup_sp_cost()
    spcostTable[, COST := as.numeric(COST)]
    processData[, ':='(VAL_MER = 0,
                       VAL_NET = 0)]
    processData[, paste("LOG_C_", 1:nologs, sep = "") := as.numeric(NA)]
    for(indilog in 1:nologs){
      processData[, ':='(tempG = as.character(unlist(processData[, paste("LOG_G_", indilog, sep = ""), with = FALSE])),
                       tempV = as.numeric(unlist(processData[, paste("LOG_V_", indilog, sep = ""), with = FALSE])),
                       tempVM = as.numeric(unlist(processData[, paste("LOG_VM_", indilog, sep = ""), with = FALSE])))]
      processData[is.na(tempV), tempV := 0]
      processData[is.na(tempVM), tempVM := 0]
      processData[, GRD_SPC := paste(tempG, SP_COST, sep = "")]
      processData <- FAIBBase::merge_dupUpdate(processData, spcostTable, by = "GRD_SPC", all.x = TRUE)
      processData[tempV %>>% 0 & is.na(COST), COST := 0]

      processData[tempV %>>% 0, paste("LOG_C_", indilog, sep = "") := COST*tempV]
      processData[tempV %>>% 0, ':='(VAL_MER = VAL_MER + COST*tempVM,
                                   VAL_NET = VAL_NET + COST*tempV)]
      processData[, c("tempG", "tempV", "tempVM", "COST") := NULL]
    }
    processData <- processData[order(uniObsID),]
    processData <- processData[,c("VAL_NET", "VAL_MER", paste("LOG_C_", 1:nologs, sep = "")), with = FALSE]
    return(processData)
  })
