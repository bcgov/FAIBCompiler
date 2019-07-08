getClusterID <- function(projID, sampleNO,
                         sampleType, intent, visit){
  thedata <- data.table::data.table(uniobs = 1:max(length(projID), length(sampleNO)),
                                    PROJ_ID = projID,
                                    SAMP_NO = sampleNO,
                                    TYPE_ = sampleType,
                                    INTENT = intent,
                                    VISIT = visit)
  thedata[,':='(PROJ_ID = gsub(" ", "", PROJ_ID),
                SAMP_NO = as.character(gsub(" ", "", SAMP_NO)),
                TYPE_ = gsub(" ", "", TYPE_),
                INTENT = gsub(" ", "", INTENT),
                VISIT = gsub(" ", "", VISIT))]
  if(nrow(thedata[nchar(PROJ_ID) != 4,]) > 0){
    stop("projID must be 4 length character.")
  }
  if(nrow(thedata[nchar(SAMP_NO) > 4 | nchar(SAMP_NO) == 0,]) > 0){
    stop("sampleNO must be 4 length character.")
  }
  thedata[nchar(SAMP_NO) == 3, SAMP_NO := paste("0", SAMP_NO, sep = "")]
  thedata[nchar(SAMP_NO) == 2, SAMP_NO := paste("00", SAMP_NO, sep = "")]
  thedata[nchar(SAMP_NO) == 1, SAMP_NO := paste("000", SAMP_NO, sep = "")]
  if(nrow(thedata[nchar(TYPE_) != 1]) > 0){
    stop("sampleType code must be a one length character.")
  }
  if(nrow(thedata[nchar(INTENT) > 1]) > 0){
    stop("intent code must be a one or zero length character.")
  }
  thedata[is.na(INTENT), INTENT := " "]
  if(nrow(thedata[nchar(VISIT) != 1]) > 0){
    stop("visit code must be a one length character.")
  }
  thedata[, ':='(CLSTR_ID = toupper(paste(PROJ_ID, "-", SAMP_NO, "-",
                                          TYPE_, INTENT, VISIT, sep = "")))]
  return(thedata[order(uniobs),]$CLSTR_ID)

}


getTypeCode <- function(sampleType, intent, visit){
  thedata <- data.table::data.table(uniobs = 1:max(length(sampleType), length(intent)),
                                    TYPE_ = sampleType,
                                    INTENT = intent,
                                    VISIT = visit)
  thedata[,':='(TYPE_ = gsub(" ", "", TYPE_),
                INTENT = gsub(" ", "", INTENT),
                VISIT = gsub(" ", "", VISIT))]
  if(nrow(thedata[nchar(TYPE_) != 1]) > 0){
    stop("sampleType code must be a one length character.")
  }
  if(nrow(thedata[nchar(INTENT) > 1]) > 0){
    stop("intent code must be a one or zero length character.")
  }
  thedata[is.na(INTENT), INTENT := " "]
  if(nrow(thedata[nchar(VISIT) != 1]) > 0){
    stop("visit code must be a one length character.")
  }
  thedata[, ':='(TYPE_CD = toupper(paste(TYPE_, INTENT, VISIT, sep = "")))]
  return(thedata[order(uniobs),]$TYPE_CD)

}


getPlotCode <- function(plotID){
  plotCode <- rep("I", length(plotID))
  plotCode[plotID == 2] <- "N"
  plotCode[plotID == 3] <- "E"
  plotCode[plotID == 4] <- "S"
  plotCode[plotID == 5] <- "W"
  plotCode[plotID > 6] <- as.character(NA)
  return(plotCode)
}


getTreeID <- function(treeNumber){
  treeNumbertable <- data.table::data.table(TREE_NUM = treeNumber)
  treeNumbertable[, TREE_NUM := gsub(" ", "", TREE_NUM)]
  treeNumbertable[nchar(TREE_NUM) == 1, TREE_NUM := paste("00", TREE_NUM, sep = "")]
  treeNumbertable[nchar(TREE_NUM) == 2, TREE_NUM := paste("0", TREE_NUM, sep = "")]
  return(treeNumbertable$TREE_NUM)
}
