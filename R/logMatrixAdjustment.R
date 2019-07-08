#' Adjust log length matrix
#' 
#' @description This function is to adjust log length matrix based on tree height, minimum log length and default log length.
#'              This function is equivalent to vol_tree_log_validation macro in original sas compiler.
#'
#' @param logLengthMatrix data.table, A matrix of log length for each tree, NA is accepted in the matrix.      
#'                                    The order of log from bottom to top must be presented from left to right in the table
#' @param height numeric, Tree height
#' @param stumpHeight, numeric, Stump height. If missing, 0.3 m is used.
#' @param logMinLength numeric, Minimum log length. If missing, 3 m is used.
#' @param logDefaultLength numeric, Default log length. If missing, 5 m is used.
#'    
#'
#' @return A data.table that contains the matrix of adjusted log length
#' 
#' 
#' @importFrom data.table ':=' 
#'
#' @export
#' @docType methods
#' @rdname logMatrixAdjustment
#'
#' @author Yong Luo
#'
setGeneric("logMatrixAdjustment",
           function(logLengthMatrix, height, stumpHeight,
                    logMinLength, logDefaultLength) {
             standardGeneric("logMatrixAdjustment")
           })

#' @rdname logMatrixAdjustment
setMethod(
  "logMatrixAdjustment",
  
  signature = c(logLengthMatrix = "data.table",
                height = "numeric",
                stumpHeight = "numeric",
                logMinLength = "numeric",
                logDefaultLength = "numeric"),
  
  definition = function(logLengthMatrix, height, stumpHeight,
                        logMinLength, logDefaultLength){
    logsMax <- ncol(logLengthMatrix)
    names(logLengthMatrix) <- paste("LOG_L_", 1:logsMax, sep = "")
    processedData <- cbind(uniObsID = 1:length(height),
                           LOG_L_0 = stumpHeight,
                           logLengthMatrix, 
                           height = round(height, 1))
    if(nrow(processedData[LOG_L_1 %in% c(NA, 0) | height %in% c(NA, 0),]) > 0){
      warning("Input is missing for either length of first log or height. NA is returned.")
      
    }
    
    invalidData <- processedData[LOG_L_1 %in% c(NA, 0) | height %in% c(NA, 0),]
    processedData <- processedData[!(uniObsID %in% invalidData$uniObsID),]
    processedData[, paste("new_LOG_L_", 1:logsMax, sep = "") := as.numeric(NA)]
    processedData[, ':='(endfirstlog = 0,
                         endsecondlog = stumpHeight,
                         VOL_TREE_MAX_LOGS = 0,
                         LOGS_DONE = 0,
                         log_i = as.numeric(NA))]
    for(i in 1:logsMax){
      processedData[LOGS_DONE %==% 0, VOL_TREE_MAX_LOGS := i]
      processedData[LOGS_DONE %==% 0, log_i := .SD, .SDcols = paste("LOG_L_", i, sep = "")]
      processedData[log_i %<<% logMinLength & LOGS_DONE %==% 0, log_i := logDefaultLength]
      processedData[LOGS_DONE %==% 0 & is.na(log_i), log_i := 0]
      processedData[LOGS_DONE %==% 0, endfirstlog := endsecondlog]
      processedData[LOGS_DONE %==% 0, endsecondlog := endsecondlog + log_i]
      ## first case: when the endsecondlog >= height, adjust last log length and stop process
      processedData[LOGS_DONE %==% 0 & endsecondlog %>=% height, ':='(log_i = log_i - (endsecondlog - height),
                                                                      LOGS_DONE = 1)]
      ## when the the endfirstlog == endsecondlog & height > endsecondlog, choose to add a log
      ## or extend the last log
      processedData[LOGS_DONE %==% 0 & endfirstlog == endsecondlog & height %>>% endfirstlog,  
                    ':='(log_i = height - endfirstlog,
                         LOGS_DONE = 1)]
      processedData[, paste("new_LOG_L_", i, sep = "") := log_i]
      if(i %!=% logsMax){
        processedData[, log_i := as.numeric(NA)]
      }
    }
    rm(i)
    ## extend length for last log
    processedData[LOGS_DONE == 0, paste("new_LOG_L_", logsMax, sep = "") := log_i + (height - endsecondlog)]
    processedData <- processedData[,c("uniObsID", "VOL_TREE_MAX_LOGS", "LOG_L_0", paste("new_LOG_L_", 1:logsMax, sep = "")), with = FALSE]
    setnames(processedData, paste("new_LOG_L_", 1:logsMax, sep = ""), paste("LOG_L_", 1:logsMax, sep = ""))
    all <- rbindlist(list(processedData,
                          invalidData[,.(uniObsID)]), fill = TRUE)
    all <- all[order(uniObsID),]
    return(all[, uniObsID := NULL])
  })


#' @export
#' @rdname logMatrixAdjustment
setMethod(
  "logMatrixAdjustment",
  signature = c(logLengthMatrix = "data.table",
                height = "numeric",
                stumpHeight = "missing",
                logMinLength = "numeric",
                logDefaultLength = "numeric"),
  definition = function(logLengthMatrix, height, 
                        logMinLength, logDefaultLength){
    return(logMatrixAdjustment(logLengthMatrix, height, stumpHeight = 0.3,
                               logMinLength, logDefaultLength))
  })


#' @export
#' @rdname logMatrixAdjustment
setMethod(
  "logMatrixAdjustment",
  signature = c(logLengthMatrix = "data.table",
                height = "numeric",
                stumpHeight = "numeric",
                logMinLength = "missing",
                logDefaultLength = "numeric"),
  definition = function(logLengthMatrix, height, stumpHeight,
                        logDefaultLength){
    return(logMatrixAdjustment(logLengthMatrix, height, stumpHeight,
                               logMinLength = 3, logDefaultLength))
  })


#' @export
#' @rdname logMatrixAdjustment
setMethod(
  "logMatrixAdjustment",
  signature = c(logLengthMatrix = "data.table",
                height = "numeric",
                stumpHeight = "numeric",
                logMinLength = "numeric",
                logDefaultLength = "missing"),
  definition = function(logLengthMatrix, height, stumpHeight,
                        logMinLength){
    return(logMatrixAdjustment(logLengthMatrix, height, stumpHeight,
                               logMinLength, logDefaultLength = 5))
  })



#' @export
#' @rdname logMatrixAdjustment
setMethod(
  "logMatrixAdjustment",
  signature = c(logLengthMatrix = "data.table",
                height = "numeric",
                stumpHeight = "missing",
                logMinLength = "missing",
                logDefaultLength = "numeric"),
  definition = function(logLengthMatrix, height, 
                        logDefaultLength){
    return(logMatrixAdjustment(logLengthMatrix, height, stumpHeight = 0.3,
                               logMinLength = 3, logDefaultLength))
  })


#' @export
#' @rdname logMatrixAdjustment
setMethod(
  "logMatrixAdjustment",
  signature = c(logLengthMatrix = "data.table",
                height = "numeric",
                stumpHeight = "missing",
                logMinLength = "numeric",
                logDefaultLength = "missing"),
  definition = function(logLengthMatrix, height, 
                        logMinLength){
    return(logMatrixAdjustment(logLengthMatrix, height, stumpHeight = 0.3,
                               logMinLength, logDefaultLength = 5))
  })



#' @export
#' @rdname logMatrixAdjustment
setMethod(
  "logMatrixAdjustment",
  signature = c(logLengthMatrix = "data.table",
                height = "numeric",
                stumpHeight = "missing",
                logMinLength = "missing",
                logDefaultLength = "missing"),
  definition = function(logLengthMatrix, height){
    return(logMatrixAdjustment(logLengthMatrix, height, stumpHeight = 0.3,
                               logMinLength = 3, logDefaultLength = 5))
  })