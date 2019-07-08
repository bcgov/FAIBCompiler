#' Calcualtes total net volume and merchantable volume-VRI specific
#' 
#' @description This function calculates total net volume and net merchantable volume for each tree
#'              based on ground called sound percentage. From the second column to the last column,
#'              the \code{grossVolMatrix} table should have same dimensions (i.e.,
#'              number of rows and columns) of \code{netFactorMatrix} and \code{grossMerchVolMatrix} if
#'              they are provided. Furthermore, be aware of the correspondingness among the matrix.
#'              This function is part of \code{log_valu_2017.sas}.
#'
#' @param grossVolMatrix data.table, Calculated gross volume for each log. The first column of this table is 
#'                                   the volume for the stump. If missing, the function calculates the total 
#'                                   net merchantable volume.
#'                                   
#' @param grossMerchVolMatrix data.table, Calculated gross merchantable volume for each log. If missing, all the merchantable
#'                                        volume is assigned as 0.
#'                                        
#' @param netFactorMatrix data.table, Ground call for sound percentage. If missing, the net factoring will be assigned as 100.
#' 
#' @return Data table that contains total net volume (\code{VOL_NET}) and total net merchantable volume (\code{VOL_NETM}) for each tree.
#'              
#' 
#' @importFrom data.table ':=' data.table
#' @importFrom dplyr '%>%'
#'
#' 
#' @export
#' @docType methods
#' @rdname netVolumeCalculator
#'
#' @author Yong Luo
#'
setGeneric("netVolumeCalculator",
           function(grossVolMatrix, grossMerchVolMatrix, netFactorMatrix) {
             standardGeneric("netVolumeCalculator")
           })

#' @rdname netVolumeCalculator
setMethod(
  "netVolumeCalculator",
  signature = c(grossVolMatrix = "data.table",
                grossMerchVolMatrix = "data.table",
                netFactorMatrix = "data.table"),
  definition = function(grossVolMatrix, grossMerchVolMatrix, netFactorMatrix){
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
    ## if net factoring is not available, assuming all net factoring is 100
    if(nrow(netFactorMatrix) == 0){
      netFactorMatrix <- data.frame(matrix(data = rep(100, (nrow(grossMerchVolMatrix))*(ncol(grossMerchVolMatrix))),
                                       nrow = nrow(grossMerchVolMatrix))) %>% data.table
    }
    
    ## check matrix demensions
    if(nrow(grossVolMatrix) != nrow(grossMerchVolMatrix) |
       nrow(grossVolMatrix) != nrow(netFactorMatrix) |
       (ncol(grossVolMatrix)-1) != ncol(grossMerchVolMatrix) |
       (ncol(grossVolMatrix)-1) != ncol(netFactorMatrix)){
      stop("Demensions of log volume, log merchantable volume and sound factoring are not match.")
    }
    names(grossVolMatrix) <- paste("LOG_V_", 0:(ncol(grossMerchVolMatrix)), sep = "")
    names(grossMerchVolMatrix) <- paste("LOG_VM_", 1:(ncol(grossMerchVolMatrix)), sep = "")
    names(netFactorMatrix) <- paste("LOG_S_", 1:(ncol(grossMerchVolMatrix)), sep = "")
    nologs <- ncol(grossMerchVolMatrix)
    processData <- cbind(data.table(uniObsID = 1:nrow(grossVolMatrix)),
                         grossVolMatrix, grossMerchVolMatrix, netFactorMatrix)
    rm(grossVolMatrix, grossMerchVolMatrix, netFactorMatrix)
    processData[, ':='(VOL_NET = LOG_V_0*LOG_S_1/100,
                    VOL_NETM = 0)]
    for(indilog in 1:nologs){
      processData[, ':='(tempS = as.numeric(unlist(processData[, paste("LOG_S_", indilog, sep = ""), with = FALSE])),
                         tempV = as.numeric(unlist(processData[, paste("LOG_V_", indilog, sep = ""), with = FALSE])),
                         tempVM = as.numeric(unlist(processData[, paste("LOG_VM_", indilog, sep = ""), with = FALSE])))]
      processData[is.na(tempS), tempS := 0]
      processData[is.na(tempV), tempV := 0]
      processData[is.na(tempVM), tempVM := 0]
      processData[, ':='(VOL_NET = VOL_NET + tempV*tempS/100,
                         VOL_NETM = VOL_NETM + tempVM*tempS/100)]
      processData[, c("tempS", "tempV", "tempVM") := NULL]
    }
    processData <- processData[order(uniObsID),]
    processData <- processData[,.(VOL_NET, VOL_NETM)]
    return(processData)
  })

#' @export
#' @rdname netVolumeCalculator
setMethod(
  "netVolumeCalculator",
  signature = c(grossVolMatrix = "missing",
                grossMerchVolMatrix = "data.table",
                netFactorMatrix = "data.table"),
  definition = function(grossMerchVolMatrix, netFactorMatrix){
    return(netVolumeCalculator(grossVolMatrix = data.table(a = numeric()),
                               grossMerchVolMatrix, netFactorMatrix))
  })


#' @export
#' @rdname netVolumeCalculator
setMethod(
  "netVolumeCalculator",
  signature = c(grossVolMatrix = "data.table",
                grossMerchVolMatrix = "missing",
                netFactorMatrix = "data.table"),
  definition = function(grossVolMatrix, netFactorMatrix){
    return(netVolumeCalculator(grossVolMatrix,
                               grossMerchVolMatrix = data.table(a = numeric()),
                               netFactorMatrix))
  })

#' @export
#' @rdname netVolumeCalculator
setMethod(
  "netVolumeCalculator",
  signature = c(grossVolMatrix = "data.table",
                grossMerchVolMatrix = "data.table",
                netFactorMatrix = "missing"),
  definition = function(grossVolMatrix, grossMerchVolMatrix){
    return(netVolumeCalculator(grossVolMatrix,
                               grossMerchVolMatrix, 
                               netFactorMatrix = data.table(a = numeric())))
  })



