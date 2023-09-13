#' Summarize the PSP tree-level data at cluster or cluster/species level
#'
#'
#' @description Summarizes the compiled tree data (including both enhanced tree data and non-enhanced tree data) at
#'              cluster level. This function is equevalent to the summary part in sas compiler in \code{cp_vegi_2017.sas}.
#'              Different from the original compiler, this function outputs the summaries by summarized components,
#'              rather than putting all together.
#'
#'
#'
#' @param allVolumeTrees data.table, Compiled tree-level volumes data.
#' @param clusterPlotHeader data.table, Cluster and plot-level information. An output of \code{\link{VRIInit_clusterplot}}.
#' @param utilLevel numeric, Utilization levels.
#' @param weirdUtil character, Weird util. Default is No. Otherwise need to be specified as a number.
#' @param equation character, Specifies whether the compiler is based on \code{KBEC} or \code{KFIZ}.
#'
#' @return Cluster and species-level volume summaries; cluster-level volume summaries; cluster-level height summaries;
#'         cluster-level species composition summaries and log file.
#'
#'
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#' @importFrom FAIBBase merge_dupUpdate
#'
#'
#' @export
#' @docType methods
#' @rdname VolumeSummaries_PSP
#'
#' @author Yong Luo
setGeneric("VolumeSummaries_PSP",
           function(allVolumeTrees, clusterPlotHeader, utilLevel,
                    weirdUtil, equation) {
             standardGeneric("VolumeSummaries_PSP")
           })

#' @rdname VolumeSummaries_PSP
setMethod(
  "VolumeSummaries_PSP",
  signature = c(allVolumeTrees = "data.table",
                clusterPlotHeader = "data.table",
                utilLevel = "numeric",
                weirdUtil = "character",
                equation = "character"),
  definition = function(allVolumeTrees, clusterPlotHeader, utilLevel,
                        weirdUtil, equation){
    allVolumeTrees[is.na(LV_D), LV_D := "L"]
    allVolumeTrees[is.na(RESIDUAL_IND), RESIDUAL_IND := "N"]
    allVolumeTrees[RESIDUAL_IND == "Y" & INGROWTH == "Y",
                   INGROWTH := "N"]
    allVolumeTrees[, LIV := LV_D]
    allVolumeTrees[INGROWTH == "Y" & LV_D == "L", LIV := "I"]
    allVolumeTrees[RESIDUAL_IND == "Y" & LV_D == "L", LIV := "V"]
    # for PSP summary, there is no need to summarize ls and lf
    # it is used liv, which include all live trees of 1) live, 2) ingrowth, 3) vetern
    allVolumeTrees[, S_F := "S"] # force to all standing
    allVolumeTrees <- FAIBBase::merge_dupUpdate(allVolumeTrees,
                                                unique(clusterPlotHeader[,.(CLSTR_ID, PLOT, NO_PLOTS, PLOT_DED,
                                                                            BEC_ZONE, PLOT_WT, SAMP_TYP, SA_VEGCOMP)],
                                                       by = c("CLSTR_ID", "PLOT")),
                                                by = c("CLSTR_ID", "PLOT"))
    if(length(weirdUtil) == 1){
      if(toupper(weirdUtil) == "NO"){
        dbhcatlist <- c(4, (1:utilLevel)*5+2.5)
      } else {
        dbhcatlist <- sort(unique(c((1:utilLevel)*5+2.5, as.numeric(weirdUtil))))
      }
    } else {
      dbhcatlist <- sort(unique(c((1:utilLevel)*5+2.5, as.numeric(weirdUtil))))
    }
    volsmy_cs <- volSmry_byCS(treeMC = data.table::copy(allVolumeTrees),
                              utilLevel = utilLevel,
                              weirdUtil = weirdUtil,
                              equation = equation)
    nvafadjattributes <- c("VHA_WSV_GVAF_LS",
                           "VHA_WSV_GVAF_DS",
                           "VHA_MER_GVAF_LS",
                           "VHA_MER_GVAF_DS",
                           "VHA_NTWB_NVAF_LS",
                           "VHA_NTWB_NVAF_DS")
    volsmy_cs[, c(nvafadjattributes) := 0]

    volsmy_c <- volSmry_byC(volSmryByCS = volsmy_cs)

    volsmy_cs[, c(nvafadjattributes) := NULL]
    fallingattributes <- c(names(volsmy_cs)[grepl("_LF", names(volsmy_cs))],
                           names(volsmy_cs)[grepl("_DF", names(volsmy_cs))])
    volsmy_cs[, c(fallingattributes) := NULL]
    volsmy_cs[, c("NO_PLOTS", "PLOT_DED", "BEC_ZONE",
                  "DBH2_LS", "DBH2_DS") := NULL]
    volsmy_cs[, c("VHA_NTWB_LS", "VHA_NTWB_DS") := NULL]
    names(volsmy_cs) <- gsub("_LS", "_LIV", names(volsmy_cs))
    names(volsmy_cs) <- gsub("_DS", "_D", names(volsmy_cs))
    setnames(volsmy_cs, "NO_TREES", "NO_TREES_ALL")

    volsmy_c[, c(nvafadjattributes) := NULL]
    volsmy_c[, c(fallingattributes) := NULL]
    volsmy_c[, c("DBH2_LS", "DBH2_DS") := NULL]
    volsmy_c[, c("VHA_NTWB_LS", "VHA_NTWB_DS") := NULL]
    names(volsmy_c) <- gsub("_LS", "_LIV", names(volsmy_c))
    names(volsmy_c) <- gsub("_DS", "_D", names(volsmy_c))
    setnames(volsmy_c, "NO_TREES", "NO_TREES_ALL")

    # a seperate process to summarize live non-ingrowth or non residual trees
    l_trees <- allVolumeTrees[LIV == "L"]
    volsmy_cs_l <- volSmry_byCS(treeMC = data.table::copy(l_trees),
                                utilLevel = utilLevel,
                                weirdUtil = weirdUtil,
                                equation = equation)
    volsmy_cs_l[, c(nvafadjattributes) := 0]

    volsmy_c_l <- volSmry_byC(volSmryByCS = volsmy_cs_l)

    volsmy_cs_l[, c(nvafadjattributes) := NULL]
    volsmy_cs_l[, c(fallingattributes) := NULL]
    volsmy_cs_l[, c("NO_PLOTS", "PLOT_DED", "BEC_ZONE",
                    "DBH2_LS", "DBH2_DS") := NULL]
    volsmy_cs_l[, c("VHA_NTWB_LS", "VHA_NTWB_DS") := NULL]
    deadstandingattributes <- names(volsmy_cs_l)[grepl("_DS", names(volsmy_cs_l))]
    volsmy_cs_l[, c(deadstandingattributes) := NULL]
    names(volsmy_cs_l) <- gsub("_LS", "_L", names(volsmy_cs_l))
    setnames(volsmy_cs_l, "NO_TREES", "NO_TREES_L")

    volsmy_c_l[, c(nvafadjattributes) := NULL]
    volsmy_c_l[, c(fallingattributes) := NULL]
    volsmy_c_l[, c(deadstandingattributes) := NULL]
    volsmy_c_l[, c("DBH2_LS", "DBH2_DS") := NULL]
    volsmy_c_l[, c("VHA_NTWB_LS", "VHA_NTWB_DS") := NULL]
    names(volsmy_c_l) <- gsub("_LS", "_L", names(volsmy_c_l))
    setnames(volsmy_c_l, "NO_TREES", "NO_TREES_L")

    volsmy_cs <- merge(volsmy_cs,
                       volsmy_cs_l,
                       by = c("CLSTR_ID", "UTIL", "SPECIES"),
                       all.x = TRUE)
    volsmy_c <- merge(volsmy_c,
                      volsmy_c_l,
                      by = c("CLSTR_ID", "UTIL"),
                      all.x = TRUE)
    rm(l_trees, volsmy_cs_l, volsmy_c_l)

    # a seperate process to summarize Vetern trees
    v_trees <- allVolumeTrees[LIV == "V"]
    volsmy_cs_v <- volSmry_byCS(treeMC = data.table::copy(v_trees),
                                utilLevel = utilLevel,
                                weirdUtil = weirdUtil,
                                equation = equation)
    volsmy_cs_v[, c(nvafadjattributes) := 0]

    volsmy_c_v <- volSmry_byC(volSmryByCS = volsmy_cs_v)

    volsmy_cs_v[, c(nvafadjattributes) := NULL]
    volsmy_cs_v[, c(fallingattributes) := NULL]
    volsmy_cs_v[, c("NO_PLOTS", "PLOT_DED", "BEC_ZONE",
                    "DBH2_LS", "DBH2_DS") := NULL]
    volsmy_cs_v[, c("VHA_NTWB_LS", "VHA_NTWB_DS") := NULL]
    volsmy_cs_v[, c(deadstandingattributes) := NULL]
    names(volsmy_cs_v) <- gsub("_LS", "_V", names(volsmy_cs_v))
    setnames(volsmy_cs_v, "NO_TREES", "NO_TREES_V")
    volsmy_c_v[, c(nvafadjattributes) := NULL]
    volsmy_c_v[, c(fallingattributes) := NULL]
    volsmy_c_v[, c(deadstandingattributes) := NULL]
    volsmy_c_v[, c("DBH2_LS", "DBH2_DS") := NULL]
    volsmy_c_v[, c("VHA_NTWB_LS", "VHA_NTWB_DS") := NULL]
    names(volsmy_c_v) <- gsub("_LS", "_V", names(volsmy_c_v))
    setnames(volsmy_c_v, "NO_TREES", "NO_TREES_V")

    volsmy_cs <- merge(volsmy_cs,
                       volsmy_cs_v,
                       by = c("CLSTR_ID", "UTIL", "SPECIES"),
                       all.x = TRUE)
    volsmy_c <- merge(volsmy_c,
                      volsmy_c_v,
                      by = c("CLSTR_ID", "UTIL"),
                      all.x = TRUE)
    rm(v_trees, volsmy_cs_v, volsmy_c_v)

    # a seperate process to summarize ingrowth trees
    i_trees <- allVolumeTrees[LIV == "I"]
    volsmy_cs_i <- volSmry_byCS(treeMC = data.table::copy(i_trees),
                                utilLevel = utilLevel,
                                weirdUtil = weirdUtil,
                                equation = equation)
    volsmy_cs_i[, c(nvafadjattributes) := 0]

    volsmy_c_i <- volSmry_byC(volSmryByCS = volsmy_cs_i)

    volsmy_cs_i[, c(nvafadjattributes) := NULL]
    volsmy_cs_i[, c(fallingattributes) := NULL]
    volsmy_cs_i[, c("NO_PLOTS", "PLOT_DED", "BEC_ZONE",
                    "DBH2_LS", "DBH2_DS") := NULL]
    volsmy_cs_i[, c(deadstandingattributes) := NULL]
    volsmy_cs_i[, c("VHA_NTWB_LS", "VHA_NTWB_DS") := NULL]
    names(volsmy_cs_i) <- gsub("_LS", "_I", names(volsmy_cs_i))
    setnames(volsmy_cs_i, "NO_TREES", "NO_TREES_I")

    volsmy_c_i[, c(nvafadjattributes) := NULL]
    volsmy_c_i[, c(fallingattributes) := NULL]
    volsmy_c_i[, c(deadstandingattributes) := NULL]
    volsmy_c_i[, c("DBH2_LS", "DBH2_DS") := NULL]
    volsmy_c_i[, c("VHA_NTWB_LS", "VHA_NTWB_DS") := NULL]
    names(volsmy_c_i) <- gsub("_LS", "_I", names(volsmy_c_i))
    setnames(volsmy_c_i, "NO_TREES", "NO_TREES_I")
    volsmy_cs <- merge(volsmy_cs,
                       volsmy_cs_i,
                       by = c("CLSTR_ID", "UTIL", "SPECIES"),
                       all.x = TRUE)
    volsmy_c <- merge(volsmy_c,
                      volsmy_c_i,
                      by = c("CLSTR_ID", "UTIL"),
                      all.x = TRUE)
    rm(i_trees, volsmy_cs_i, volsmy_c_i)

    # for tally trees
    tally_trees <- allVolumeTrees[MEASUREMENT_ANOMALY_CODE == "PSP-TALLY"]
    volsmy_cs_tally <- volSmry_byCS(treeMC = data.table::copy(tally_trees),
                                    utilLevel = utilLevel,
                                    weirdUtil = weirdUtil,
                                    equation = equation)
    volsmy_cs_tally[, c(nvafadjattributes) := 0]

    volsmy_c_tally <- volSmry_byC(volSmryByCS = volsmy_cs_tally)

    volsmy_cs <- merge(volsmy_cs,
                       volsmy_cs_tally[,.(CLSTR_ID, UTIL, SPECIES,
                                          VHA_WSV_TALLY = VHA_WSV_LS,
                                          VHA_MER_TALLY = VHA_MER_LS,
                                          VHA_DWB_TALLY = VHA_DWB_LS,
                                          STEMS_HA_TALLY = STEMS_HA_LS,
                                          BA_HA_TALLY = BA_HA_LS,
                                          NO_TREES_TALLY = NO_TREES)],
                       by = c("CLSTR_ID", "UTIL", "SPECIES"),
                       all.x = TRUE)
    volsmy_c <- merge(volsmy_c,
                      volsmy_c_tally[,.(CLSTR_ID, UTIL,
                                        VHA_WSV_TALLY = VHA_WSV_LS,
                                        VHA_MER_TALLY = VHA_MER_LS,
                                        VHA_DWB_TALLY = VHA_DWB_LS,
                                        STEMS_HA_TALLY = STEMS_HA_LS,
                                        BA_HA_TALLY = BA_HA_LS,
                                        NO_TREES_TALLY = NO_TREES)],
                      by = c("CLSTR_ID", "UTIL"),
                      all.x = TRUE)
    rm(tally_trees, volsmy_cs_tally, volsmy_c_tally)


    nato0attributes <- c("VHA_WSV_LIV", "VHA_MER_LIV",
                         "VHA_DWB_LIV", "BA_HA_LIV", "STEMS_HA_LIV", "VHA_WSV_D",
                         "VHA_MER_D", "VHA_DWB_D", "BA_HA_D",
                         "STEMS_HA_D", "NO_TREES_ALL", "QMD_LIV", "QMD_D", "VHA_WSV_L",
                         "VHA_MER_L", "VHA_DWB_L", "BA_HA_L",
                         "STEMS_HA_L", "NO_TREES_L", "QMD_L", "VHA_WSV_V", "VHA_MER_V",
                         "VHA_DWB_V", "BA_HA_V", "STEMS_HA_V",
                         "NO_TREES_V", "QMD_V", "VHA_WSV_I", "VHA_MER_I",
                         "VHA_DWB_I", "BA_HA_I", "STEMS_HA_I", "NO_TREES_I",
                         "QMD_I", "NO_TREES_TALLY", "VHA_WSV_TALLY", "VHA_MER_TALLY",
                         "VHA_DWB_TALLY", "STEMS_HA_TALLY", "BA_HA_TALLY")

    for (indiattri in nato0attributes) {
      setnames(volsmy_c, indiattri, "tempattri")
      setnames(volsmy_cs, indiattri, "tempattri")
      volsmy_c[is.na(tempattri),
               tempattri := 0]
      volsmy_cs[is.na(tempattri),
                tempattri := 0]
      setnames(volsmy_c, "tempattri", indiattri)
      setnames(volsmy_cs, "tempattri", indiattri)
    }
    rm(nato0attributes)

    volsmy_c[, NO_TREES_LIV := NO_TREES_L + NO_TREES_V + NO_TREES_I]
    volsmy_cs[, NO_TREES_LIV := NO_TREES_L + NO_TREES_V + NO_TREES_I]

    # for psp species composition summary
    # it uses only live and ingrowth
    # summaries are based on ba and stem
    volsmy_cs[, ':='(STEMS_HA_LI = STEMS_HA_L + STEMS_HA_I,
                     BA_HA_LI = BA_HA_L + BA_HA_I)]

    volsmy_cs[, ':='(stem_li_tot = sum(STEMS_HA_LI, na.rm = TRUE),
                     ba_li_tot = sum(BA_HA_LI, na.rm = TRUE),
                     ba_dead = sum(BA_HA_D, na.rm = TRUE)),
              by = c("CLSTR_ID", "UTIL")]

    volsmy_cs[stem_li_tot %>>% 0, SP_PCT_STEM_LI := 100*STEMS_HA_LI/stem_li_tot]
    volsmy_cs[is.na(SP_PCT_STEM_LI), SP_PCT_STEM_LI := 0]

    volsmy_cs[ba_li_tot %>>% 0, SP_PCT_BA_LI := 100*BA_HA_LI/ba_li_tot]
    volsmy_cs[is.na(SP_PCT_BA_LI), SP_PCT_BA_LI := 0]

    volsmy_cs[ba_dead %>>% 0, SP_PCT_BA_D := 100*BA_HA_D/ba_dead]
    volsmy_cs[is.na(SP_PCT_BA_D), SP_PCT_BA_D := 0]

    volsmy_cs[,c("ba_li_tot", "stem_li_tot",
                 "ba_dead") := NULL]
    formatcols <- c("SP_PCT_STEM_LI", "SP_PCT_BA_LI",
                    "SP_PCT_BA_D")
    volsmy_cs[, c(formatcols) := lapply(.SD, function(s) round(s, 1)),
              .SDcols = formatcols]
    ## derive leading and sedondary species by li stem
    species_order_stem <- volsmy_cs[order(CLSTR_ID, UTIL, -SP_PCT_STEM_LI),
                                    .(CLSTR_ID, UTIL, SPECIES, SP_PCT_STEM_LI)]
    species_order_stem[, species_order := 1:length(SPECIES),
                       by = c("CLSTR_ID", "UTIL")]
    leadingsp_stem <- species_order_stem[species_order == 1,
                                         .(CLSTR_ID, UTIL,
                                           LEAD_SP_LI_STEM = SPECIES,
                                           LEAD_SP_PCT_LI_STEM = SP_PCT_STEM_LI)]
    secondarysp_stem <- species_order_stem[species_order == 2,
                                           .(CLSTR_ID, UTIL,
                                             SECD_SP_LI_STEM = SPECIES,
                                             SECD_SP_PCT_LI_STEM = SP_PCT_STEM_LI)]
    volsmy_c <- merge(volsmy_c,
                      leadingsp_stem,
                      by = c("CLSTR_ID", "UTIL"),
                      all.x = TRUE)
    volsmy_c <- merge(volsmy_c,
                      secondarysp_stem,
                      by = c("CLSTR_ID", "UTIL"),
                      all.x = TRUE)
    rm(species_order_stem, leadingsp_stem, secondarysp_stem)
    ## derive leading and sedondary species by li ba
    species_order_ba <- volsmy_cs[order(CLSTR_ID, UTIL, -SP_PCT_BA_LI),
                                  .(CLSTR_ID, UTIL, SPECIES, SP_PCT_BA_LI)]
    species_order_ba[, species_order := 1:length(SPECIES),
                     by = c("CLSTR_ID", "UTIL")]
    leadingsp_ba <- species_order_ba[species_order == 1,
                                     .(CLSTR_ID, UTIL,
                                       LEAD_SP_LI_BA = SPECIES,
                                       LEAD_SP_PCT_LI_BA = SP_PCT_BA_LI)]
    secondarysp_ba <- species_order_ba[species_order == 2,
                                       .(CLSTR_ID, UTIL,
                                         SECD_SP_LI_BA = SPECIES,
                                         SECD_SP_PCT_LI_BA = SP_PCT_BA_LI)]
    volsmy_c <- merge(volsmy_c,
                      leadingsp_ba,
                      by = c("CLSTR_ID", "UTIL"),
                      all.x = TRUE)
    volsmy_c <- merge(volsmy_c,
                      secondarysp_ba,
                      by = c("CLSTR_ID", "UTIL"),
                      all.x = TRUE)
    rm(species_order_ba, leadingsp_ba, secondarysp_ba)

    ## height summary by cluster
    # 1)
    heightsmry_c <- allVolumeTrees[LIV %in% c("L", "I", "V"),
                                   .(HT_MEAN = round(mean(HT_TOTAL, na.rm = TRUE), 2)),
                                   by = c("CLSTR_ID", "LIV")]
    heightsmry_c <- reshape(heightsmry_c,
                            direction = "wide",
                            idvar = "CLSTR_ID",
                            timevar = "LIV",
                            v.names = "HT_MEAN",
                            sep = "_")
    heightsmry_li <- allVolumeTrees[LIV %in% c("L", "I"),
                                    .(HT_MEAN_LI = round(mean(HT_TOTAL, na.rm = TRUE), 2)),
                                    by = "CLSTR_ID"]
    heightsmry_c <- merge(heightsmry_c, heightsmry_li,
                          by = "CLSTR_ID",
                          all = TRUE)

    heightsmry_lorey <- heightSmry_byC(treeMC = allVolumeTrees[LIV %in% c("L", "I")])
    heightsmry_c <- merge(heightsmry_c,
                          heightsmry_lorey[,.(CLSTR_ID,
                                              HT_LRY_LI = HT_LRYALL)],
                          by = "CLSTR_ID",
                          all.x = TRUE)
    rm(heightsmry_li, heightsmry_lorey)
    cl_spc_li <- speciesComp_byC(CSSmryTable = volsmy_cs,
                                 basedOn = "BA_HA_LI",
                                 speciesMaxNO = 12)
    setnames(cl_spc_li, "SPB_CPCT", "SPB_CPCT_LI")
    cl_spc_liv <- speciesComp_byC(CSSmryTable = volsmy_cs,
                                  basedOn = "BA_HA_LIV",
                                  speciesMaxNO = 12)
    setnames(cl_spc_liv, "SPB_CPCT", "SPB_CPCT_LIV")
    cl_spc <- merge(cl_spc_liv,
                    cl_spc_li,
                    by = c("CLSTR_ID", "UTIL"),
                    all.x = TRUE)
    rm(cl_spc_liv, cl_spc_li)
    cl_spc_d <- speciesComp_byC(CSSmryTable = volsmy_cs,
                                basedOn = "BA_HA_D",
                                speciesMaxNO = 12)
    setnames(cl_spc_d, "SPB_CPCT", "SPB_CPCT_D")


    allclustersByUtil <- data.table(expand.grid(CLSTR_ID = unique(clusterPlotHeader$CLSTR_ID),
                                                UTIL = dbhcatlist))
    summarycols <- c(paste("VHA_",c("WSV", "MER", "DWB"), sep = ""),
                     "BA_HA", "STEMS_HA", "QMD", "NO_TREES")
    summarycolsLiv <- paste(summarycols, "_LIV", sep = "")
    summarycolsL <- paste(summarycols, "_L", sep = "")
    summarycolsI <- paste(summarycols, "_I", sep = "")
    summarycolsV <- paste(summarycols, "_V", sep = "")
    summarycolsD <- paste(c(paste("VHA_",c("WSV", "MER", "DWB"), sep = ""),
                            "BA_HA", "STEMS_HA", "QMD"),
                          "_D", sep = "")

    volsmy_cs <- merge(allclustersByUtil, volsmy_cs, by = c("CLSTR_ID", "UTIL"),
                       all = TRUE)
    volsmy_cs[is.na(NO_TREES_LIV),
              c(summarycolsLiv) := 0]
    volsmy_cs[is.na(NO_TREES_L),
              c(summarycolsL) := 0]
    volsmy_cs[is.na(NO_TREES_I),
              c(summarycolsI) := 0]
    volsmy_cs[is.na(NO_TREES_V),
              c(summarycolsV) := 0]
    volsmy_cs[is.na(STEMS_HA_D),
              c(summarycolsD) := 0]
    volsmy_cs[is.na(NO_TREES_ALL),
              NO_TREES_ALL := 0]
    volsmy_cs[is.na(SP_PCT_BA_LI),
              SP_PCT_BA_LI := 0]
    volsmy_cs[is.na(SP_PCT_STEM_LI),
              SP_PCT_STEM_LI := 0]
    volsmy_cs[is.na(SP_PCT_BA_D),
              SP_PCT_BA_D := 0]
    volsmy_cs[is.na(NO_TREES_TALLY),
              NO_TREES_TALLY := 0]

    volsmy_c <- merge(allclustersByUtil, volsmy_c, by = c("CLSTR_ID", "UTIL"),
                      all = TRUE)
    volsmy_c[is.na(NO_TREES_LIV),
             c(summarycolsLiv) := 0]
    volsmy_c[is.na(NO_TREES_L),
             c(summarycolsL) := 0]
    volsmy_c[is.na(NO_TREES_I),
             c(summarycolsI) := 0]
    volsmy_c[is.na(NO_TREES_V),
             c(summarycolsV) := 0]
    volsmy_c[is.na(STEMS_HA_D),
             c(summarycolsD) := 0]
    volsmy_c[is.na(NO_TREES_ALL),
             NO_TREES_ALL := 0]
    volsmy_c[is.na(NO_TREES_TALLY),
             NO_TREES_TALLY := 0]
    cl_spc <- merge(allclustersByUtil, cl_spc,
                    by = c("CLSTR_ID", "UTIL"),
                    all = TRUE)
    cl_spc <- merge(cl_spc, cl_spc_d,
                    by = c("CLSTR_ID", "UTIL"),
                    all = TRUE)
    return(list(vol_bycs = volsmy_cs,
                vol_byc = volsmy_c,
                heightsmry_byc = heightsmry_c,
                compositionsmry_byc = cl_spc))
  })
