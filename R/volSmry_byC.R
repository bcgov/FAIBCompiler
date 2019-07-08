#' Summarize volume components at cluster level-VRI specific
#' 
#' 
#' @description This function summarizes the cluster-level volume components using cluster/species-level
#'              summaries. The cluster/species-level summaries is an output of \code{\link{volSmry_byCS}} 
#'              function. 
#'
#' @param volSmryByCS data.table, Summarized volume components for both measured and counted trees.
#'                                See \code{\link{volSmry_byCS}} for details.
#'                                                                     
#' @return A data table
#' 
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#'
#' 
#' @export
#' @docType methods
#' @rdname volSmry_byC
#'
#' @author Yong Luo
#'
setGeneric("volSmry_byC",
           function(volSmryByCS){
             standardGeneric("volSmry_byC")
           })

#' @rdname volSmry_byC
setMethod(
  "volSmry_byC",
  signature = c(volSmryByCS = "data.table"),
  definition = function(volSmryByCS){
    summarycolsLS <- c(paste("VHA_",c("WSV", "NET", "MER", "NETM", "NTW2",
                                      "NTWB", "D", "DW", "DWB"),
                             sep = ""), "DHA_MER", "DBH2", "BA_HA", "STEMS_HA")
    summarycolsLF <- paste(summarycolsLS, "LF", sep = "")
    summarycolsDS <- paste(summarycolsLS, "DS", sep = "")
    summarycolsDF <- paste(summarycolsLS, "DF", sep = "")
    output <- unique(volSmryByCS[,.(CLSTR_ID, UTIL, PRJ_GRP, NO_PLOTS, PLOT_DED,
                                    PROJ_ID)],
                     by = c("CLSTR_ID", "UTIL"))
    smrytable <- volSmryByCS[,lapply(.SD, sum), .SDcols = c(summarycolsLS, summarycolsLF,
                                                            summarycolsDS, summarycolsDF, "NO_TREES"),
                             by = c("CLSTR_ID", "UTIL")] 
    output <- merge_dupUpdate(output, smrytable, by = c("CLSTR_ID", "UTIL"))
    output[STEMS_HA %>>% 0, QMD := sqrt(DBH2/STEMS_HA)]
    output[STEMS_HALF %>>% 0, QMDLF := sqrt(DBH2LF/STEMS_HALF)]
    output[STEMS_HADS %>>% 0, QMDDS := sqrt(DBH2DS/STEMS_HADS)]
    output[STEMS_HADF %>>% 0, QMDDF := sqrt(DBH2DF/STEMS_HADF)]
    decimal3cols <- c(summarycolsLS[1:12], summarycolsLF[1:12], summarycolsDS[1:12], summarycolsDF[1:12],
                      "QMD", "QMDLF", "QMDDS", "QMDDF")
    output[, c(decimal3cols) := lapply(.SD, function(s) round(s, 3)), .SDcols = decimal3cols]
    roundcols <- c(paste("STEMS_HA", c("", "LF", "DS", "DF"), sep = ""), "NO_TREES")
    output[, c(roundcols) := lapply(.SD, round), .SDcols = roundcols]
    return(output)
  })