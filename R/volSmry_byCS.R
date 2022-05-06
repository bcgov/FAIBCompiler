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
    vha <- c(paste("VHA_",c("WSV", "MER", "NTWB", "DWB"),
                      sep = ""), "DHA_MER")
    vha_ls <- paste(vha, "_LS", sep = "")
    vha_lf <- paste(vha, "_LF", sep = "")
    vha_ds <- paste(vha, "_DS", sep = "")
    vha_df <- paste(vha, "_DF", sep = "")
    treeMC[, ':='(DBH2 = DBH^2,
                  STEMS_HA = 1)]
    treeMC[LV_D == "L" & S_F == "S", TREE_FACTOR := PHF_TREE/NO_PLOTS]
    ## NO matter a tree is live or dead, as long as it is falling
    treeMC[S_F == "F", TREE_FACTOR := PHF_TREE]
    treeMC[LV_D == "D" & S_F == "S", TREE_FACTOR := PHF_TREE/PLOT_DED]
    if(length(weirdUtil) == 1){
      if(toupper(weirdUtil) == "NO"){
        dbhcatlist <- c(0, (1:utilLevel)*5+2.5)
      } else {
        dbhcatlist <- sort(unique(c((1:utilLevel)*5+2.5, as.numeric(weirdUtil))))
      }
    } else {
      dbhcatlist <- sort(unique(c((1:utilLevel)*5+2.5, as.numeric(weirdUtil))))
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
      smy_cs_all[is.na(BEC_ZONE), c(vha_ls, vha_lf, vha_ds, vha_df) := as.numeric(NA)]
    }
    otherarray <- c("DBH2", "QMD", "BA_HA", "STEMS_HA")
    otherarray_ls <- paste(otherarray, "_LS", sep = "")
    otherarray_lf <- paste(otherarray, "_LF", sep = "")
    otherarray_ds <- paste(otherarray, "_DS", sep = "")
    otherarray_df <- paste(otherarray, "_DF", sep = "")
    decimal3cols <- c(vha_ls, vha_lf, vha_ds, vha_df,
                      otherarray_ls[1:3], otherarray_lf[1:3],
                      otherarray_ds[1:3], otherarray_df[1:3])
    smy_cs_all[, c(decimal3cols) := lapply(.SD, function(s) round(s, 3)), .SDcols = decimal3cols]
    roundcols <- c(paste("STEMS_HA", c("_LS", "_LF", "_DS", "_DF"), sep = ""), "NO_TREES")
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
  summaryColumns <- c(paste("VOL_",c("WSV", "MER", "NTWB", "DWB"),
                            sep = ""), "DBH2", "BA_TREE", "STEMS_HA")
  for(i in summaryColumns){
    summarizeddata[, c(i) := TREE_FACTOR * unlist(summarizeddata[, i, with = FALSE])]
  }
  rm(i)
  vhacolumns <- c(paste("VHA_",c("WSV", "MER", "NTWB", "DWB"), sep = ""),
                      "DBH2", "BA_HA", "STEMS_HA")

  lssummarycolumns <- paste0(vhacolumns, "_LS")
  summarizedoutput <- unique(summarizeddata[,.(CLSTR_ID, SPECIES, NO_PLOTS, PLOT_DED,
                                               BEC_ZONE)],
                             by = c("CLSTR_ID", "SPECIES"))
  summarizedoutput1 <- summarizeddata[,c(.N, lapply(.SD, function(s) sum(s, na.rm = TRUE))),
                                      .SDcols = summaryColumns,
                                      by = c("CLSTR_ID", "SPECIES", "LV_D", "S_F")]
  rm(summarizeddata)
  for(lv in c("L", "D")){
    for(sf in c("S", "F")){
      subtable <- summarizedoutput1[LV_D == lv & S_F == sf, ]
      set(subtable, , c("LV_D", "S_F"), NULL)
      if(lv == "L" & sf == "S"){
        setnames(subtable, c("N", summaryColumns), c("LIVE_STAND", lssummarycolumns))
      } else if (lv == "L" & sf == "F") {
        lfsummarycolumns <- paste(vhacolumns, "_LF", sep = "")
        setnames(subtable, c("N", summaryColumns),
                 c("LIVE_FALL", lfsummarycolumns))
      } else if (lv == "D" & sf == "S") {
        dssummarycolumns <- paste(vhacolumns, "_DS", sep = "")
        setnames(subtable, c("N", summaryColumns),
                 c("DEAD_STAND", dssummarycolumns))
      } else if (lv == "D" & sf == "F") {
        dfsummarycolumns <- paste(vhacolumns, "_DF", sep = "")
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
  ## can have dead and live for the same species, can have live only or dead only,
  ## therefore not D, or not L is the best test.
  summarizedoutput[is.na(DEAD_STAND),  c(dssummarycolumns) := 0]
  summarizedoutput[is.na(DEAD_STAND),  DEAD_STAND := 0]

  summarizedoutput[is.na(DEAD_FALL),  c(dfsummarycolumns) := 0]
  summarizedoutput[is.na(DEAD_FALL),  DEAD_FALL := 0]

  summarizedoutput[is.na(LIVE_FALL),  c(lfsummarycolumns) := 0]
  summarizedoutput[is.na(LIVE_FALL),  LIVE_FALL := 0]

  summarizedoutput[is.na(LIVE_STAND),  c(lssummarycolumns) := 0]
  summarizedoutput[is.na(LIVE_STAND),  LIVE_STAND := 0]

  summarizedoutput[STEMS_HA_LS %>>% 0, QMD_LS := sqrt(DBH2_LS/STEMS_HA_LS)]
  summarizedoutput[is.na(QMD_LS), QMD_LS := 0]

  summarizedoutput[STEMS_HA_LF %>>% 0, QMD_LF := sqrt(DBH2_LF/STEMS_HA_LF)]
  summarizedoutput[is.na(QMD_LF), QMD_LF := 0]

  summarizedoutput[STEMS_HA_DS %>>% 0, QMD_DS := sqrt(DBH2_DS/STEMS_HA_DS)]
  summarizedoutput[is.na(QMD_DS), QMD_DS := 0]

  summarizedoutput[STEMS_HA_DF %>>% 0, QMD_DF := sqrt(DBH2_DF/STEMS_HA_DF)]
  summarizedoutput[is.na(QMD_DF), QMD_DF := 0]

  summarizedoutput[, NO_TREES := LIVE_STAND + LIVE_FALL + DEAD_STAND + DEAD_FALL]
  summarizedoutput[, c("LIVE_STAND", "LIVE_FALL", "DEAD_STAND", "DEAD_FALL") := NULL]
  return(summarizedoutput)
}

