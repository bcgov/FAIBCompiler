#' To display a table to log file
#' 
#' @description Simple tool to display a table to log file.
#' @param reason character, Reason to trigger an action.
#' @param action character, Specifies action from one of \code{removed}, \code{no} and \code{changed}.
#' @param displayTable data.table, A table of interest
#' @param displayColumn character, Specifies which column(s) will be displayed in the log file.
#' @param changedVariable, charcater, Specifies the variable that has been modified, must be present if action
#'                                    is \code{changed}.
#' @param fromTo character, This is two vectors character. Specifies columns that before and after alteration.
#'                          Must be present when action is set as \code{changed}. 
#'                            
#' @return A tring of text
#' 
#' 
#' @export
#' @docType methods
#' @rdname logFileProducer
#'  
#' @author Yong Luo
#'   
#' 
#' 
setGeneric("logFileProducer",
           function(reason,
                    action,
                    displayTable, 
                    displayColumn,
                    changedVariable,
                    fromTo){
             standardGeneric("logFileProducer")   
           })       

#' @rdname logFileProducer
setMethod(
  "logFileProducer",
  signature = c(reason = "character",
                action = "character",
                displayTable = "data.table",
                displayColumn = "character",
                changedVariable = "character",
                fromTo = "character"),
  definition = function(reason, action, displayTable, 
                        displayColumn, changedVariable, fromTo){
    if(action %in% c("removed", "no")){
      if(action == "removed"){
        logfileTitle <- paste(reason, ", ", action, ": \n", sep = "")
      } else {
        logfileTitle <- paste(reason, ": \n", sep = "")
      }
      columnLength <- length(displayColumn)
      if(nrow(displayTable) == 0){
        if(columnLength == 1){
          logfile <- paste("  ", displayColumn, ": N/A \n\n", sep = "")
        } else {
          logfile <- paste(paste("  ", displayColumn[1:(columnLength - 1)], ": N/A,", sep = ""),
                           collapse = "")
          logfile <- paste(logfile, 
                           paste("  ", displayColumn[columnLength], ": N/A \n\n", sep = ""), 
                           collapse = "")
        }
      } else {
        displayTable <- displayTable[, displayColumn, with = FALSE]
        displayTable <- unique(displayTable, by = displayColumn)
        if(columnLength == 1){
          logfile <- paste("  ", displayColumn, ": ",
                           unlist(displayTable[, displayColumn, with = FALSE]), "\n", sep = "")
        } else {
          for(i in 1:columnLength){
            if(i == 1){
              logfile <- paste("  ", displayColumn[i], ": ",
                               unlist(displayTable[, displayColumn[i], with = FALSE]), ",", sep = "")  
            } else if(i > 1 & i != columnLength){
              logfile <- paste(logfile, 
                               paste("  ", displayColumn[i], ": ",
                                     unlist(displayTable[, displayColumn[i], with = FALSE]), ",", sep = ""))
            } else {
              logfile <- paste(logfile, 
                               paste("    ", displayColumn[columnLength], ": ",
                                     unlist(displayTable[, displayColumn[columnLength], with = FALSE]), sep = ""),
                               "\n", sep = "")
            }
          }
        }
        logfile <- paste("  ", 1:nrow(displayTable), ". ", logfile, sep = "")
        logfile[nrow(displayTable)] <- paste(logfile[nrow(displayTable)], "\n", sep = "")
        if(nrow(displayTable) > 50){
        logfile <- c(logfile[1:10], "   ...\n", logfile[(nrow(displayTable)-10):nrow(displayTable)])
        }
      }
      logfile <- paste(logfileTitle, paste(logfile, collapse = ""), sep = "")
    } else if(action == "changed"){
      logfileTitle <- paste(reason, ", ",changedVariable, " was ", action, ": \n", sep = "")
      columnLength <- length(displayColumn)
      if(nrow(displayTable) == 0){
        displayTable <- displayTable[, displayColumn, with = FALSE]
        displayTable <- unique(displayTable, by = displayColumn)
        if(columnLength == 1){
          logfile <- paste("  ", displayColumn, ": N/A \n\n", sep = "")
        } else {
          logfile <- paste(paste("  ", displayColumn[1:(columnLength - 1)], ": N/A,", sep = ""),
                           collapse = "")
          logfile <- paste(logfile, 
                           paste("  ", displayColumn[columnLength], ": N/A \n\n", sep = ""), 
                           collapse = "")
        }
      } else {
        if(columnLength == 1){
          logfile <- paste("  ", displayColumn, ": ",
                           unlist(displayTable[, displayColumn, with = FALSE]), "\n", sep = "")
        } else {
          for(i in 1:columnLength){
            if(i == 1){
              logfile <- paste("  ", displayColumn[i], ": ",
                               unlist(displayTable[, displayColumn[i], with = FALSE]), ",", sep = "")  
            } else {
              logfile <- paste(logfile, 
                               paste("  ", displayColumn[i], ": ",
                                     unlist(displayTable[, displayColumn[i], with = FALSE]), ",", sep = ""))
            } 
          }
          logfile <- paste(logfile, " From: ", unlist(displayTable[, fromTo[1], with = FALSE]),
                           ", To: ", unlist(displayTable[, fromTo[2], with = FALSE]), "\n", sep = "")
        }
        logfile <- paste("  ", 1:nrow(displayTable), ". ", logfile, sep = "")
        logfile[nrow(displayTable)] <- paste(logfile[nrow(displayTable)], "\n", sep = "")
        if(nrow(displayTable) > 50){
          logfile <- c(logfile[1:10], "   ...\n", logfile[(nrow(displayTable)-10):nrow(displayTable)])
        }
      }
      logfile <- paste(logfileTitle, paste(logfile, collapse = ""), sep = "")
    } 
    return(logfile)
  })



#' @export
#' @rdname logFileProducer
setMethod(
  "logFileProducer",
  signature = c(reason = "character",
                action = "character",
                displayTable = "data.table",
                displayColumn = "missing",
                changedVariable = "character",
                fromTo = "character"),
  definition = function(reason, action, displayTable, 
                        changedVariable, fromTo){
    return(logFileProducer(reason, action, displayTable, 
                      displayColumn = c("CLSTR_ID", "PLOT"),
                      changedVariable, fromTo))
  })


#' @export
#' @rdname logFileProducer
setMethod(
  "logFileProducer",
  signature = c(reason = "character",
                action = "character",
                displayTable = "data.table",
                displayColumn = "character",
                changedVariable = "missing",
                fromTo = "missing"),
  definition = function(reason, action, displayTable, 
                        displayColumn){
    return(logFileProducer(reason, action, displayTable, 
                      displayColumn,
                      changedVariable = "NA", fromTo = "NA"))
  })


#' @export
#' @rdname logFileProducer
setMethod(
  "logFileProducer",
  signature = c(reason = "character",
                action = "character",
                displayTable = "data.table",
                displayColumn = "missing",
                changedVariable = "missing",
                fromTo = "missing"),
  definition = function(reason, action, displayTable){
    return(logFileProducer(reason, action, displayTable, 
                      displayColumn = c("CLSTR_ID", "PLOT"),
                      changedVariable = "NA", fromTo = "NA"))
  })
