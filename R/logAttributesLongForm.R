#' Transpose wide form table to long form table-VRI specific
#'
#' @description This function transposes wide form outputs to long form outputs. This function is not
#'              included in the VRI compiler anymore.
#'
#' @param treeData data.table, an output from \code{logValueCalculator} function, i.e., tree_ms6. This table currently
#'                 has top diameter (LOG_D_X), length (LOG_L_X), volume (LOG_V_X), merchantable volume (LOG_VM_X),
#'                 grade (LOG_G_X), sound percentage (LOG_S_X) and value (LOG_c_x). X is log number from 1 to maximum log number.
#' @param maximumLogNO numeric, determine the maximum number of logs. In VRI compiler, it is 9. Therefore, 9 is default.
#'
#' @return A data table and a log file
#'
#' @importFrom data.table ':='
#' @importFrom dplyr left_join
#'
#'
#' @export
#' @docType methods
#' @rdname logAttributesLongForm
#'
#' @author Yong Luo
#'
setGeneric("logAttributesLongForm",
           function(treeData, maximumLogNO) {
             standardGeneric("logAttributesLongForm")
           })

#' @rdname logAttributesLongForm
setMethod(
  "logAttributesLongForm",
  signature = c(treeData = "data.table",
                maximumLogNO = "numeric"),
  definition = function(treeData, maximumLogNO){
    for(i in c("G", "S", "L", "D", "V", "C", "VM")){
      if(i == "G"){
        keepCol <- c("CLSTR_ID", "PLOT", "TREE_NO", "SPECIES")
      } else {
        keepCol <- c("CLSTR_ID", "PLOT", "TREE_NO")
      }
      targettable <- treeData[,c(keepCol, paste("LOG_", i, "_", 1:maximumLogNO, sep = "")),
                              with = FALSE]
      targettable_tranformed <- suppressWarnings(melt(targettable, id.vars = keepCol,
                                                      measure.vars = paste("LOG_", i, "_", 1:maximumLogNO, sep = ""),
                                                      variable.name = "LOG_NO",
                                                      value.name = i))
      targettable_tranformed[, LOG_NO := as.numeric(gsub(pattern = paste("LOG_", i, "_", sep = ""),
                                                         replacement = "",
                                                         x = LOG_NO))]
      if(i == "G"){
        alltranformed <- targettable_tranformed
      } else {
        alltranformed <- merge(alltranformed, targettable_tranformed, by = c("CLSTR_ID", "PLOT", "TREE_NO", "LOG_NO"),
                               all.x = TRUE)
      }
    }
    setnames(alltranformed, c("G", "S", "L", "D", "V", "C", "VM"),
             c("GRADE", "PCT_SND", "LENGTH",
               "D_TOP", "VOL_WSV", "VAL_WSV", "VOL_MER"))
    return(alltranformed)
  })



#' @export
#' @rdname logAttributesLongForm
setMethod(
  "logAttributesLongForm",
  signature = c(treeData = "data.table",
                maximumLogNO = "missing"),
  definition = function(treeData, maximumLogNO){
    return(logAttributesLongForm(treeData, maximumLogNO = 9))
  })
