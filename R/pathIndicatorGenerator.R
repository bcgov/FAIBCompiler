#' Generate path indicator string for risk group
#'
#' @description This function to generate a length of eight character path indicator string.
#'              The path indicator string is an input for rish group for Jim's decay, waste and breakage function.
#'              The function varies with PSP compiler and VRI compiler.
#'
#' @param lossIndicatorMatrix data.table, Contains eight columns of loss indicator, i.e., LOSS1...8_IN.
#' @param lossIndicatorLocMatrix    data.table, Contains the location for each corresponding loss indicator
#'                                  in \code{lossIndicatorMatrix}. This matrix will be used for VRI compiler.
#' @param merchantableHeight    numeric, Specifies the maximum height for merchantable volume.
#'                              This arguement will be used for VRI compiler.
#' @param compiler    character, Specifies in which compiler the path indicator will be generated.
#'                               It can be either \code{PSP} or \code{VRI}.
#'
#' @return A length of eight character that contains 0 and 1, e.g., 10010000.
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#'
#' @note For PSP compiler, the path indicator is only based on loss indicator. However, for
#'       VRI compiler, the path indicator also based on loss indicator location and merchantable height.
#' @export
#' @docType methods
#' @rdname pathIndicatorGenerator
#'
#' @author Yong Luo
pathIndicatorGenerator <- function(lossIndicatorMatix,
                                   lossIndicatorLocMatrix,
                                   merchantableHeight,
                                   compiler){
  if(compiler == "PSP"){
    loss_fct <- lossIndicatorMatix
    loss_fct[, PATH_IND := "00000000"]
    ### based on Rene's email on 20211005, in PSP, the path_ind is only take
    ### loss indicator

    for(i in 1:8){
      loss_fct[, ':='(tempLOSS_IN = unlist(loss_fct[, paste("LOSS", i, "_IN", sep = ""),
                                                    with = FALSE]))]
      loss_fct[substr(tempLOSS_IN, 1, 2) %in% c("DD", "DR", "CN", "DI", "D"),
               PATH_IND := paste("1", substr(PATH_IND, 2, 8), sep = "")]
      loss_fct[tempLOSS_IN %in% c("BNK"),
               PATH_IND := paste(substr(PATH_IND, 1, 1), "1", substr(PATH_IND, 3, 8), sep = "")]
      loss_fct[tempLOSS_IN %in% c("SCA"),
               PATH_IND := paste(substr(PATH_IND, 1, 2), "1", substr(PATH_IND, 4, 8), sep = "")]
      loss_fct[tempLOSS_IN %in% c("FRK", "CRO", "CRK"),
               PATH_IND := paste(substr(PATH_IND, 1, 3), "1", substr(PATH_IND, 5, 8), sep = "")]
      loss_fct[tempLOSS_IN %in% c("AFC", "NGC", "FRS"),
               PATH_IND := paste(substr(PATH_IND, 1, 4), "1", substr(PATH_IND, 6, 8), sep = "")]
      loss_fct[tempLOSS_IN %in% c("MIS", "DM"),
               PATH_IND := paste(substr(PATH_IND, 1, 5), "1", substr(PATH_IND, 7, 8), sep = "")]
      loss_fct[tempLOSS_IN %in% c("LRB"),
               PATH_IND := paste(substr(PATH_IND, 1, 6), "1", substr(PATH_IND, 8, 8), sep = "")]
      loss_fct[tempLOSS_IN %in% c("DTP", "BTP", "CD"),
               PATH_IND := paste(substr(PATH_IND, 1, 7), "1", sep = "")]
    }
  } else if (compiler == "VRI"){
    loss_fct <- cbind(lossIndicatorMatix,
                      lossIndicatorLocMatrix,
                      data.table(H_MERCH = merchantableHeight))
    loss_fct[, PATH_IND := "00000000"]
    for(i in 1:8){
      loss_fct[, ':='(tempLOC_FRO = as.numeric(unlist(loss_fct[, paste("LOC", i, "_FRO", sep = ""), with = FALSE])),
                      tempLOSS_IN = unlist(loss_fct[, paste("LOSS", i, "_IN", sep = ""), with = FALSE]))]
      loss_fct[tempLOSS_IN != "" & is.na(tempLOC_FRO), tempLOC_FRO := 0]
      loss_fct[(tempLOC_FRO %<<% H_MERCH | is.na(H_MERCH)) &
                 substr(tempLOSS_IN, 1, 2) %in% c("DD", "DR", "CN", "DI", "D"),
               PATH_IND := paste("1", substr(PATH_IND, 2, 8), sep = "")]
      loss_fct[(tempLOC_FRO %<<% H_MERCH | is.na(H_MERCH)) &
                 tempLOSS_IN %in% c("BNK"),
               PATH_IND := paste(substr(PATH_IND, 1, 1), "1", substr(PATH_IND, 3, 8), sep = "")]
      loss_fct[(tempLOC_FRO %<<% H_MERCH | is.na(H_MERCH)) &
                 tempLOSS_IN %in% c("SCA"),
               PATH_IND := paste(substr(PATH_IND, 1, 2), "1", substr(PATH_IND, 4, 8), sep = "")]
      loss_fct[(tempLOC_FRO %<<% H_MERCH | is.na(H_MERCH)) &
                 tempLOSS_IN %in% c("FRK", "CRO", "CRK"),
               PATH_IND := paste(substr(PATH_IND, 1, 3), "1", substr(PATH_IND, 5, 8), sep = "")]
      loss_fct[(tempLOC_FRO %<<% H_MERCH | is.na(H_MERCH)) &
                 tempLOSS_IN %in% c("AFC", "NGC", "FRS"),
               PATH_IND := paste(substr(PATH_IND, 1, 4), "1", substr(PATH_IND, 6, 8), sep = "")]
      loss_fct[(tempLOC_FRO %<<% H_MERCH | is.na(H_MERCH)) &
                 tempLOSS_IN %in% c("MIS", "DM"),
               PATH_IND := paste(substr(PATH_IND, 1, 5), "1", substr(PATH_IND, 7, 8), sep = "")]
      loss_fct[(tempLOC_FRO %<<% H_MERCH | is.na(H_MERCH)) &
                 tempLOSS_IN %in% c("LRB"),
               PATH_IND := paste(substr(PATH_IND, 1, 6), "1", substr(PATH_IND, 8, 8), sep = "")]
      loss_fct[(tempLOC_FRO %<<% H_MERCH | is.na(H_MERCH)) &
                 tempLOSS_IN %in% c("DTP", "BTP", "CD"),
               PATH_IND := paste(substr(PATH_IND, 1, 7), "1", sep = "")]
    }
  } else {
    stop("compiler must be correctly specified using PSP or VRI.")
  }
  return(loss_fct$PATH_IND)
}







