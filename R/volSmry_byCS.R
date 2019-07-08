#' Summarize volume components per hectare by cluster and species-VRI specific
#' 
#' 
#' @description Summarizes volume components per hectare by cluster and species.
#'              The function is last part of \code{vol_ha_2017.sas}.
#'
#' @param treeMC data.table, Tree-level compiled data for all volume trees. 
#' @param utilLevel numeric, Utilization levels. Default is 4.
#' @param weirdUtil character, Weird util. Default is \code{No}. Otherwise need to be specified as a number.
#' @param equation character, Specifies whether the compiler is based on \code{KBEC} or \code{KFIZ}.
#'                            Default is \code{KBEC}.       
#'                                                                     
#' @return A data table summarizes volume components by cluster and species. Equevalent to \code{smy_cs}.
#' 
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#'
#' 
#' @export
#' @docType methods
#' @rdname volSmry_byCS
#'
#' @author Yong Luo
#'
setGeneric("volSmry_byCS",
           function(treeMC, utilLevel, weirdUtil, equation) {
             standardGeneric("volSmry_byCS")
           })

#' @rdname volSmry_byCS
setMethod(
  "volSmry_byCS",
  signature = c(treeMC = "data.table",
                utilLevel = "numeric",
                weirdUtil = "character",
                equation = "character"),
  definition = function(treeMC, utilLevel, weirdUtil, equation){
    vha_ls <- c(paste("VHA_",c("WSV", "NET", "MER", "NETM", "NTW2",
                               "NTWB", "D", "DW", "DWB"),
                      sep = ""), "DHA_MER")
    vha_lf <- paste(vha_ls, "LF", sep = "")
    vha_ds <- paste(vha_ls, "DS", sep = "")
    vha_df <- paste(vha_ls, "DF", sep = "")
    
    treeMC[, ':='(DBH2 = DBH^2,
                  STEMS_HA = 1,
                  PRJ_GRP = prj_ID2Grp(PROJ_ID))]
    treeMC[LV_D == "L" & SF_COMPILE == "S", TREE_FACTOR := PHF_TREE/NO_PLOTS]
    ## NO matter a tree is live or dead, as long as it is falling
    treeMC[SF_COMPILE == "F", TREE_FACTOR := PHF_TREE]
    treeMC[LV_D == "D" & SF_COMPILE == "S", TREE_FACTOR := PHF_TREE/PLOT_DED]
    
    if(toupper(weirdUtil) == "NO"){
      dbhcatlist <- c(0, (1:utilLevel)*5+2.5)
    } else {
      dbhcatlist <- sort(unique(c(0, (1:utilLevel)*5+2.5, as.numeric(weirdUtil))))
    }
    all_volList <- lapply(dbhcatlist, function(s) dbhcatsummary(summarizeddata = treeMC,
                                                                dbhcat = s))
    for(i in 1:length(all_volList)) {
      if(i == 1){
        smy_cs_all <- all_volList[[i]]
      } else {
        smy_cs_all <- rbindlist(list(smy_cs_all,
                                  all_volList[[i]]))
      }
    }
    rm(all_volList, i)

    if(toupper(equation) == "KBEC"){
      smy_cs_all[is.na(BGC_ZONE), c(vha_ls, vha_lf, vha_ds, vha_df) := as.numeric(NA)]
    }
    otherarray_ls <- c("DBH2", "QMD", "BA_HA", "STEMS_HA")
    otherarray_lf <- paste(otherarray_ls, "LF", sep = "")
    otherarray_ds <- paste(otherarray_ls, "DS", sep = "")
    otherarray_df <- paste(otherarray_ls, "DF", sep = "")
    decimal3cols <- c(vha_ls, vha_lf, vha_ds, vha_df,
                      otherarray_ls[1:3], otherarray_lf[1:3],
                      otherarray_ds[1:3], otherarray_df[1:3])
    smy_cs_all[, c(decimal3cols) := lapply(.SD, function(s) round(s, 3)), .SDcols = decimal3cols]
    roundcols <- c(paste("STEMS_HA", c("", "LF", "DS", "DF"), sep = ""), "NO_TREES")
    smy_cs_all[, c(roundcols) := lapply(.SD, round), .SDcols = roundcols]
    return(smy_cs_all)
  })


#' @export
#' @rdname volSmry_byCS
setMethod(
  "volSmry_byCS",
  signature = c(treeMC = "data.table",
                utilLevel = "missing",
                weirdUtil = "character",
                equation = "character"),
  definition = function(treeMC, weirdUtil, equation){
    return(volSmry_byCS(treeMC, utilLevel = 4, weirdUtil, equation))
  })

#' @export
#' @rdname volSmry_byCS
setMethod(
  "volSmry_byCS",
  signature = c(treeMC = "data.table",
                utilLevel = "numeric",
                weirdUtil = "missing",
                equation = "character"),
  definition = function(treeMC, utilLevel, equation){
    return(volSmry_byCS(treeMC, utilLevel, weirdUtil = "No", equation))
  })

#' @export
#' @rdname volSmry_byCS
setMethod(
  "volSmry_byCS",
  signature = c(treeMC = "data.table",
                utilLevel = "numeric",
                weirdUtil = "numeric",
                equation = "character"),
  definition = function(treeMC, utilLevel, weirdUtil, equation){
    return(volSmry_byCS(treeMC, utilLevel, weirdUtil = as.character(weirdUtil), equation))
  })

#' @export
#' @rdname volSmry_byCS
setMethod(
  "volSmry_byCS",
  signature = c(treeMC = "data.table",
                utilLevel = "numeric",
                weirdUtil = "character",
                equation = "missing"),
  definition = function(treeMC, utilLevel, weirdUtil){
    return(volSmry_byCS(treeMC, utilLevel, weirdUtil, equation = "KBEC"))
  })

#' @export
#' @rdname volSmry_byCS
setMethod(
  "volSmry_byCS",
  signature = c(treeMC = "data.table",
                utilLevel = "missing",
                weirdUtil = "missing",
                equation = "missing"),
  definition = function(treeMC){
    return(volSmry_byCS(treeMC, utilLevel = 4,
                              weirdUtil = "No", 
                              equation = "KBEC"))
  })


## leave the function here for now, will consider moving to other place
dbhcatsummary <- function(summarizeddata, dbhcat){
  summarizeddata <- summarizeddata[DBH %>=% dbhcat, ]
  summaryColumns <- c(paste("VOL_",c("WSV", "NET", "MER", "NETM", "NTW2",
                                     "NTWB", "D", "DW", "DWB"),
                            sep = ""), "VAL_MER", "DBH2", "BA_TREE", "STEMS_HA")
  for(i in summaryColumns){
    summarizeddata[, c(i) := TREE_FACTOR * unlist(summarizeddata[, i, with = FALSE])]
  }
  rm(i)
  lssummarycolumns <- c(paste("VHA_",c("WSV", "NET", "MER", "NETM", "NTW2",
                                       "NTWB", "D", "DW", "DWB"),
                              sep = ""), "DHA_MER", "DBH2", "BA_HA", "STEMS_HA")
  
  
  summarizedoutput <- unique(summarizeddata[,.(CLSTR_ID, SPECIES, PRJ_GRP, NO_PLOTS, PLOT_DED, 
                                               PROJ_ID, BGC_ZONE)],
                             by = c("CLSTR_ID", "SPECIES"))
  summarizedoutput1 <- summarizeddata[,c(.N, lapply(.SD, function(s) sum(s, na.rm = TRUE))),
                                      .SDcols = summaryColumns,
                                      by = c("CLSTR_ID", "SPECIES", "LV_D", "SF_COMPILE")]
  rm(summarizeddata)
  for(lv in c("L", "D")){
    for(sf in c("S", "F")){
      subtable <- summarizedoutput1[LV_D == lv & SF_COMPILE == sf, ]
      set(subtable, , c("LV_D", "SF_COMPILE"), NULL)
      if(lv == "L" & sf == "S"){
        setnames(subtable, c("N", summaryColumns), c("LIVE", lssummarycolumns))
      } else if (lv == "L" & sf == "F") {
        lfsummarycolumns <- paste(lssummarycolumns, "LF", sep = "")
        setnames(subtable, c("N", summaryColumns), 
                 c("LIVE_FALL", lfsummarycolumns))
      } else if (lv == "D" & sf == "S") {
        dssummarycolumns <- paste(lssummarycolumns, "DS", sep = "")
        setnames(subtable, c("N", summaryColumns), 
                 c("DEAD_STAND", dssummarycolumns))
      } else if (lv == "D" & sf == "F") {
        dfsummarycolumns <- paste(lssummarycolumns, "DF", sep = "")
        setnames(subtable, c("N", summaryColumns), 
                 c("DEAD_FALL", dfsummarycolumns))
      }
      summarizedoutput <- merge(summarizedoutput, subtable,
                                by = c("CLSTR_ID", "SPECIES"),
                                all.x = TRUE)
    }
  }
  rm(lv, sf, summarizedoutput1, subtable)
  
  summarizedoutput[, UTIL := dbhcat]
  summarizedoutput[UTIL == 0, UTIL := 4]
  ## can have dead and live for the same species, can have live only or dead only,
  ## therefore not D, or not L is the best test.
  summarizedoutput[is.na(DEAD_STAND),  c(dssummarycolumns) := 0]
  summarizedoutput[is.na(DEAD_STAND),  DEAD_STAND := 0]
  
  summarizedoutput[is.na(DEAD_FALL),  c(dfsummarycolumns) := 0]
  summarizedoutput[is.na(DEAD_FALL),  DEAD_FALL := 0]
  
  summarizedoutput[is.na(LIVE_FALL),  c(lfsummarycolumns) := 0]
  summarizedoutput[is.na(LIVE_FALL),  LIVE_FALL := 0]
  
  summarizedoutput[is.na(LIVE),  c(lssummarycolumns) := 0]
  summarizedoutput[is.na(LIVE),  LIVE := 0]
  
  summarizedoutput[STEMS_HA %>>% 0, QMD := sqrt(DBH2/STEMS_HA)]
  summarizedoutput[is.na(QMD), QMD := 0]
  
  summarizedoutput[STEMS_HALF %>>% 0, QMDLF := sqrt(DBH2LF/STEMS_HALF)]
  summarizedoutput[is.na(QMDLF), QMDLF := 0]
  
  summarizedoutput[STEMS_HADS %>>% 0, QMDDS := sqrt(DBH2DS/STEMS_HADS)]
  summarizedoutput[is.na(QMDDS), QMDDS := 0]
  
  summarizedoutput[STEMS_HADF %>>% 0, QMDDF := sqrt(DBH2DF/STEMS_HADF)]
  summarizedoutput[is.na(QMDDF), QMDDF := 0]
  
  summarizedoutput[, NO_TREES := LIVE + LIVE_FALL + DEAD_STAND + DEAD_FALL]
  summarizedoutput[, c("LIVE", "LIVE_FALL", "DEAD_STAND", "DEAD_FALL") := NULL]  
  return(summarizedoutput)
}

