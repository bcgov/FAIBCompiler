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
#' @importFrom FAIBBase merge_dupUpdate
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
    summarycols <- c(paste("VHA_",c("WSV", "MER", "NTWB", "DWB"), sep = ""),
                     "DBH2", "BA_HA", "STEMS_HA")

    summarycolsLS <- paste(summarycols, "_LS", sep = "")
    summarycolsLF <- paste(summarycols, "_LF", sep = "")
    summarycolsDS <- paste(summarycols, "_DS", sep = "")
    summarycolsDF <- paste(summarycols, "_DF", sep = "")
    summarycolsGVAFNVAF <- c("VHA_WSV_GVAF_LS",
                             "VHA_WSV_GVAF_DS",
                             "VHA_MER_GVAF_LS",
                             "VHA_MER_GVAF_DS",
                             "VHA_NTWB_NVAF_LS",
                             "VHA_NTWB_NVAF_DS")
    output <- unique(volSmryByCS[,.(CLSTR_ID, UTIL)],
                     by = c("CLSTR_ID", "UTIL"))
    smrytable <- volSmryByCS[,lapply(.SD, sum), .SDcols = c(summarycolsLS, summarycolsLF,
                                                            summarycolsDS, summarycolsDF,
                                                            summarycolsGVAFNVAF,
                                                            "NO_TREES"),
                             by = c("CLSTR_ID", "UTIL")]
    output <- FAIBBase::merge_dupUpdate(output, smrytable, by = c("CLSTR_ID", "UTIL"))
    output[STEMS_HA_LS %>>% 0, QMD_LS := sqrt(DBH2_LS/STEMS_HA_LS)]
    output[STEMS_HA_LF %>>% 0, QMD_LF := sqrt(DBH2_LF/STEMS_HA_LF)]
    output[STEMS_HA_DS %>>% 0, QMD_DS := sqrt(DBH2_DS/STEMS_HA_DS)]
    output[STEMS_HA_DF %>>% 0, QMD_DF := sqrt(DBH2_DF/STEMS_HA_DF)]
    decimal3cols <- c(summarycolsLS[1:6], summarycolsLF[1:6],
                      summarycolsDS[1:6], summarycolsDF[1:6],
                      summarycolsGVAFNVAF,
                      "QMD_LS", "QMD_LF", "QMD_DS", "QMD_DF")
    output[, c(decimal3cols) := lapply(.SD, function(s) round(s, 3)), .SDcols = decimal3cols]
    roundcols <- c(paste("STEMS_HA", c("_LS", "_LF", "_DS", "_DF"), sep = ""), "NO_TREES")
    output[, c(roundcols) := lapply(.SD, round), .SDcols = roundcols]
    output[is.na(QMD_LS), QMD_LS := 0]
    output[is.na(QMD_LF), QMD_LF := 0]
    output[is.na(QMD_DS), QMD_DS := 0]
    output[is.na(QMD_DF), QMD_DF := 0]
    return(output)
  })
