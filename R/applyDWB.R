#' Apply decay, waste and breakage percentage to gross merchantable volume-VRI specific
#'
#'
#' @description This function calculates merchantable volume after removing decay, waste and breakage in VRI compiler.
#'              The function is part of \code{cp_vegi_2017.sas} to derive \code{tree_ms7}.
#'
#' @param treeMS data.table, Compiled full and enhanced trees with percentage of decay, waste and breakage.
#'
#'
#' @return A data table that contains \code{VOL_NTWB} (net volume that waste 2 wood and breakage), \code{VOL_D}
#'         (merchantable volume after removing decay), \code{VOL_DW} (merchantable volume after removing decay
#'         and waste) and \code{ VOL_DWB} (merchantable volume after removing decay, waste and breakage).
#'
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#'
#'
#' @export
#' @docType methods
#' @rdname applyDWB
#'
#' @author Yong Luo
applyDWB<- function(treeMS){
    treeMS[, uniObs := 1:nrow(treeMS)]
    sndArray <- paste("LOG_S_", 1:9, sep = "")
    merArray <- paste("LOG_VM_", 1:9, sep = "")

    treeMS[, tempcol := (VOL_MER-VOL_NETM)/VOL_MER]
    # Waste2 - is defined at the tree level as -
    #   if the decayed wood ie
    # (volume less top and stump) - (cruiser called net merch volume vol_netm ie sound wood)
    # is greater than 50% then the entire tree is considered to be "waste2"
    #
    # if any log has more than 50 % decay, ie if the sound wood is less than 50% then it is also waste2 wood */
    treeMS[VOL_MER %>>% 0 & tempcol %>>% 0.5,
             VOL_W2 := VOL_NETM]
    treeMS[, tempcol := NULL]
    processdata <- treeMS[VOL_MER %>>% 0 & is.na(VOL_W2),]
    unprocessdata <- treeMS[!(uniObs %in% processdata$uniObs),]
    rm(treeMS)
    processdata[, VOL_W2 := 0]
    for(i in 1:9){
      processdata[, ':='(tempSND = unlist(processdata[, sndArray[i], with = FALSE]),
                         tempMER = as.numeric(unlist(processdata[, merArray[i], with = FALSE])))]
      processdata[is.na(tempMER), tempMER := 0]
      processdata[tempSND %<<% 50, VOL_W2 := VOL_W2 + tempMER*tempSND/100]
      processdata[, ':='(tempSND = NULL, tempMER = NULL)]
    }
    treeMS <- rbindlist(list(processdata, unprocessdata))
    rm(processdata, unprocessdata)
    treeMS[is.na(VOL_W2), VOL_W2 := 0]
    treeMS[, VOL_NTW2 := VOL_NETM - VOL_W2]
    treeMS[VOL_NTW2 %<<% 0, VOL_NTW2 := 0]
    treeMS[, VOL_B := VOL_MER*PCT_BRK/100]
    treeMS[is.na(VOL_B), VOL_B := 0]
    treeMS[, VOL_NTWB := VOL_NTW2 - VOL_B]
    treeMS[VOL_NTWB %<<% 0, VOL_NTWB := 0]
    treeMS[, VOL_D := VOL_MER-(VOL_MER*(PCT_DCY/100))]
    treeMS[, VOL_DW := VOL_MER-(VOL_MER*(PCT_DCY + PCT_WST)/100)]
    treeMS[, VOL_DWB := VOL_MER-(VOL_MER*(PCT_DCY + PCT_WST + PCT_BRK)/100)]
    treeMS[is.na(VOL_D), VOL_D := 0]
    treeMS[is.na(VOL_DW), VOL_DW := 0]
    treeMS[is.na(VOL_DWB), VOL_DWB := 0]
    treeMS[, uniObs := NULL]
    return(treeMS[order(CLSTR_ID, PLOT, TREE_NO),])
  }
