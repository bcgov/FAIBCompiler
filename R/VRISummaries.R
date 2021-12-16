#' Summarize the tree-level data at cluster or cluster/species level-VRI specific
#'
#'
#' @description Summarizes the compiled tree data (including both enhanced tree data and non-enhanced tree data) at
#'              cluster level. This function is equevalent to the summary part in sas compiler in \code{cp_vegi_2017.sas}.
#'              Different from the original compiler, this function outputs the summaries by summarized components,
#'              rather than putting all together.
#'
#'
#'
#' @param allVolumeTrees data.table, All tree data from vi_c and vi_i that have been compiled with
#'                                   tree volume.
#' @param clusterPlotHeader data.table, Cluster and plot-level information. An output of \code{\link{VRIInit_clusterplot}}.
#' @param utilLevel numeric, Utilization levels.
#' @param weirdUtil character, Weird util. Default is No. Otherwise need to be specified as a number.
#' @param equation character, Specifies whether the compiler is based on \code{KBEC} or \code{KFIZ}.
#' @param nvafRatio data.table, NVAF adjustment table based on \code{BEC}, \code{Species}, and \code{LV_D}.
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
#' @rdname VRISummaries
#'
#' @author Yong Luo
#'
setGeneric("VRISummaries",
           function(allVolumeTrees, clusterPlotHeader, utilLevel,
                    weirdUtil, equation, nvafRatio) {
             standardGeneric("VRISummaries")
           })

#' @rdname VRISummaries
setMethod(
  "VRISummaries",
  signature = c(allVolumeTrees = "data.table",
                clusterPlotHeader = "data.table",
                utilLevel = "numeric",
                weirdUtil = "character",
                equation = "character",
                nvafRatio = "data.table"),
  definition = function(allVolumeTrees, clusterPlotHeader, utilLevel,
                        weirdUtil, equation, nvafRatio){
    allVolumeTrees <- FAIBBase::merge_dupUpdate(allVolumeTrees,
                                                unique(clusterPlotHeader[,.(CLSTR_ID, PLOT, PROJ_ID, NO_PLOTS, PLOT_DED,
                                                                            BGC_ZONE, PLOT_WT, SAMP_TYP, SA_VEGCOMP)],
                                                       by = c("CLSTR_ID", "PLOT")),
                                                by = c("CLSTR_ID", "PLOT"))
    if(toupper(weirdUtil) == "NO"){
      dbhcatlist <- c(4, (1:utilLevel)*5+2.5)
    } else {
      dbhcatlist <- sort(unique(c(4, (1:utilLevel)*5+2.5, as.numeric(weirdUtil))))
    }

    volsmy_cs <- volSmry_byCS(treeMC = data.table::copy(allVolumeTrees),
                              utilLevel = utilLevel,
                              weirdUtil = weirdUtil,
                              equation = equation)
    # write.csv(volsmy_cs, file.path(r_path, "smy_cs.csv"), row.names = F)
    specieslookup <- unique(lookup_species()[,.(SPECIES, SP0)], by = "SPECIES")
    volsmy_cs <- merge(volsmy_cs,
                       specieslookup,
                       by = "SPECIES",
                       all.x = TRUE)
    volsmy_cs <- merge(volsmy_cs,
                       unique(clusterPlotHeader[,.(CLSTR_ID,
                                                   BGC_SBZN, SA_VEGCOMP)],
                              by = "CLSTR_ID"),
                       by = "CLSTR_ID",
                       all.x = TRUE)
    volsmy_cs[, ':='(MATURITY1 = "All",
                     MATURITY2 = "Imm")]
    volsmy_cs[SA_VEGCOMP > 120,
              MATURITY2 := "Mat"]
    # rene's comment: •	For assigning age class, we use the intersected VEGCOMP
    # rank1 layer, where immature is (1-120yrs) and mature is (121+yrs)
    volsmy_cs <- merge(volsmy_cs,
                       nvafRatio[LV_D == "Live",
                                 .(BGC_ZONE, BGC_SBZN, SP0,
                                   MATURITY1  = MATURITY,
                                   GVAF_L1 = GVAF, NVAF_L1 = NVAF)],
                       by = c("BGC_ZONE", "BGC_SBZN", "SP0",
                              "MATURITY1"),
                       all.x = TRUE) # this is the second priority
    volsmy_cs <- merge(volsmy_cs,
                       nvafRatio[LV_D == "Live",
                                 .(BGC_ZONE, BGC_SBZN, SP0,
                                   MATURITY2  = MATURITY,
                                   GVAF_L2 = GVAF, NVAF_L2 = NVAF)],
                       by = c("BGC_ZONE", "BGC_SBZN", "SP0",
                              "MATURITY2"),
                       all.x = TRUE) # this is the priority
    volsmy_cs <- merge(volsmy_cs,
                       nvafRatio[LV_D == "Live" & is.na(BGC_SBZN),
                                 .(BGC_ZONE, SP0,
                                   MATURITY2  = MATURITY,
                                   GVAF_L3 = GVAF, NVAF_L3 = NVAF)],
                       by = c("BGC_ZONE", "SP0",
                              "MATURITY2"),
                       all.x = TRUE)
    volsmy_cs[, ':='(GVAF_L = GVAF_L2,
                     NVAF_L = NVAF_L2)] ## using sub bec zone * imm/mat combination
    volsmy_cs[is.na(GVAF_L),
              ':='(GVAF_L = GVAF_L1,
                   NVAF_L = NVAF_L1)] ## using sub bec zone * all combination
    volsmy_cs[is.na(GVAF_L),
              ':='(GVAF_L = GVAF_L3,
                   NVAF_L = NVAF_L3)] ## using bec zone * imm/mat combination
    volsmy_cs[is.na(GVAF_L),
              ':='(GVAF_L = 1,
                   NVAF_L = 1)] # if gvaf and nvaf can not be found, using 1 for no adjustment

    volsmy_cs <- merge(volsmy_cs,
                       nvafRatio[LV_D == "Dead",
                                 .(BGC_ZONE, BGC_SBZN, SP0,
                                   MATURITY1  = MATURITY,
                                   GVAF_D1 = GVAF, NVAF_D1 = NVAF)],
                       by = c("BGC_ZONE", "BGC_SBZN", "SP0",
                              "MATURITY1"),
                       all.x = TRUE) # this is the second priority
    volsmy_cs <- merge(volsmy_cs,
                       nvafRatio[LV_D == "Dead",
                                 .(BGC_ZONE, BGC_SBZN, SP0,
                                   MATURITY2  = MATURITY,
                                   GVAF_D2 = GVAF, NVAF_D2 = NVAF)],
                       by = c("BGC_ZONE", "BGC_SBZN", "SP0",
                              "MATURITY2"),
                       all.x = TRUE) # this is the priority
    volsmy_cs <- merge(volsmy_cs,
                       nvafRatio[LV_D == "Dead" & is.na(BGC_SBZN),
                                 .(BGC_ZONE, SP0,
                                   MATURITY2  = MATURITY,
                                   GVAF_D3 = GVAF, NVAF_D3 = NVAF)],
                       by = c("BGC_ZONE", "SP0",
                              "MATURITY2"),
                       all.x = TRUE)
    volsmy_cs[, ':='(GVAF_D = GVAF_D2,
                     NVAF_D = NVAF_D2)] ## using sub bec zone * imm/mat combination
    volsmy_cs[is.na(GVAF_D),
              ':='(GVAF_D = GVAF_D1,
                   NVAF_D = NVAF_D1)] ## using sub bec zone * all combination
    volsmy_cs[is.na(GVAF_D),
              ':='(GVAF_D = GVAF_D3,
                   NVAF_D = NVAF_D3)] ## using bec zone * imm/mat combination
    volsmy_cs[is.na(GVAF_D),
              ':='(GVAF_D = 1,
                   NVAF_D = 1)] # if gvaf and nvaf can not be found, using 1 for no adjustment

    set(volsmy_cs, , c(paste0("GVAF_L", 1:3), paste0("NVAF_L", 1:3),
                       paste0("GVAF_D", 1:3), paste0("NVAF_D", 1:3)), NULL)
    volsmy_cs[, ':='(VHA_WSV_GVAF_LS = VHA_WSV * GVAF_L,
                     VHA_WSV_GVAF_LF = VHA_WSVLF * GVAF_L,
                     VHA_WSV_GVAF_DS = VHA_WSVDS * GVAF_D,
                     VHA_WSV_GVAF_DF = VHA_WSVDF * GVAF_D,
                     VHA_MER_GVAF_LS = VHA_MER * GVAF_L,
                     VHA_MER_GVAF_LF = VHA_MERLF * GVAF_L,
                     VHA_MER_GVAF_DS = VHA_MERDS * GVAF_D,
                     VHA_MER_GVAF_DF = VHA_MERDF * GVAF_D,
                     VHA_NTWB_NVAF_LS = VHA_NTWB * NVAF_L,
                     VHA_NTWB_NVAF_LF = VHA_NTWBLF * NVAF_L,
                     VHA_NTWB_NVAF_DS = VHA_NTWBDS * NVAF_D,
                     VHA_NTWB_NVAF_DF = VHA_NTWBDF * NVAF_D)]


    ## volume summary by cluster
    volsmy_c <- volSmry_byC(volSmryByCS = volsmy_cs)
    volsmy_cs[, ':='(VPT_WSV = sum(VHA_WSV, na.rm = TRUE),
                     VPT_NETM = sum(VHA_NETM, na.rm = TRUE),
                     VPT_MER = sum(VHA_MER, na.rm = TRUE),
                     BA_PT = sum(BA_HA, na.rm = TRUE),
                     VPT_WSVDS = sum(VHA_WSVDS, na.rm = TRUE)),
              by = c("CLSTR_ID", "UTIL")]
    volsmy_cs[VPT_WSV %>>% 0, VPC_WSV := 100*VHA_WSV/VPT_WSV]
    volsmy_cs[VPT_NETM %>>% 0, VPC_NETM := 100*VHA_NETM/VPT_NETM]
    volsmy_cs[VPT_MER %>>% 0, VPC_MER := 100*VHA_MER/VPT_MER]
    volsmy_cs[VPT_WSVDS %>>% 0, VPC_WSVDS := 100*VHA_WSVDS/VPT_WSVDS]
    volsmy_cs[BA_PT %>>% 0, BA_PC := 100*BA_HA/BA_PT]
    volsmy_cs[,c("VPT_WSV", "VPT_NETM", "VPT_MER", "VPT_WSVDS", "BA_PT") := NULL]
    formatcols <- c("VPC_WSV", "VPC_NETM", "VPC_MER", "BA_PC", "VPC_WSVDS")
    volsmy_cs[, c(formatcols) := lapply(.SD, function(s) round(s, 1)),
              .SDcols = formatcols]

    ## height summary by cluster
    heightsmry_c <- heightSmry_byC(treeMC = allVolumeTrees)
    cl_spc <- speciesComp_byC(CSSmryTable = volsmy_cs, basedOn = "BA_HA", speciesMaxNO = 12)

    allclustersByUtil <- data.table(expand.grid(CLSTR_ID = unique(clusterPlotHeader$CLSTR_ID),
                                                UTIL = dbhcatlist))

    allclustersByUtil <- merge(allclustersByUtil,
                               unique(clusterPlotHeader[,.(CLSTR_ID, PRJ_GRP, NO_PLOTS, PLOT_DED, PROJ_ID)],
                                      by = "CLSTR_ID"),
                               by = "CLSTR_ID",
                               all.x = TRUE)

    summarycolsLS <- c(paste("VHA_",c("WSV", "NET", "MER", "NETM", "NTW2",
                                      "NTWB", "D", "DW", "DWB"),
                             sep = ""), "DHA_MER", "DBH2", "BA_HA", "STEMS_HA")
    summarycolsLF <- paste(summarycolsLS, "LF", sep = "")
    summarycolsDS <- paste(summarycolsLS, "DS", sep = "")
    summarycolsDF <- paste(summarycolsLS, "DF", sep = "")

    volsmy_cs[, c("PRJ_GRP", "NO_PLOTS", "PLOT_DED", "PROJ_ID") := NULL]
    volsmy_cs <- merge(allclustersByUtil, volsmy_cs, by = c("CLSTR_ID", "UTIL"),
                       all = TRUE)
    volsmy_cs[is.na(VHA_WSV), c(summarycolsLS) := 0]
    volsmy_cs[is.na(VHA_WSVLF), c(summarycolsLF) := 0]
    volsmy_cs[is.na(VHA_WSVDS), c(summarycolsDS) := 0]
    volsmy_cs[is.na(VHA_WSVDF), c(summarycolsDF) := 0]

    volsmy_c[, c("PRJ_GRP", "NO_PLOTS", "PLOT_DED", "PROJ_ID") := NULL]
    volsmy_c <- merge(allclustersByUtil, volsmy_c, by = c("CLSTR_ID", "UTIL"),
                      all = TRUE)
    volsmy_c[is.na(VHA_WSV),
             c(summarycolsLS, "NO_TREES", "QMD", "QMDLF", "QMDDS", "QMDDF") := 0]
    volsmy_c[is.na(VHA_WSVLF), c(summarycolsLF) := 0]
    volsmy_c[is.na(VHA_WSVDS), c(summarycolsDS) := 0]
    volsmy_c[is.na(VHA_WSVDF), c(summarycolsDF) := 0]
    volsmy_c[is.na(VHA_WSV_GVAF_LS), VHA_WSV_GVAF_LS := 0]
    volsmy_c[is.na(VHA_WSV_GVAF_LF), VHA_WSV_GVAF_LF := 0]
    volsmy_c[is.na(VHA_WSV_GVAF_DS), VHA_WSV_GVAF_DS := 0]
    volsmy_c[is.na(VHA_WSV_GVAF_DF), VHA_WSV_GVAF_DF := 0]
    volsmy_c[is.na(VHA_MER_GVAF_LS), VHA_MER_GVAF_LS := 0]
    volsmy_c[is.na(VHA_MER_GVAF_LF), VHA_MER_GVAF_LF := 0]
    volsmy_c[is.na(VHA_MER_GVAF_DS), VHA_MER_GVAF_DS := 0]
    volsmy_c[is.na(VHA_MER_GVAF_DF), VHA_MER_GVAF_DF := 0]
    volsmy_c[is.na(VHA_NTWB_NVAF_LS), VHA_NTWB_NVAF_LS := 0]
    volsmy_c[is.na(VHA_NTWB_NVAF_LF), VHA_NTWB_NVAF_LF := 0]
    volsmy_c[is.na(VHA_NTWB_NVAF_DS), VHA_NTWB_NVAF_DS := 0]
    volsmy_c[is.na(VHA_NTWB_NVAF_DF), VHA_NTWB_NVAF_DF := 0]
    cl_spc <- merge(allclustersByUtil, cl_spc,
                    by = c("CLSTR_ID", "UTIL"),
                    all = TRUE)

    return(list(vol_bycs = volsmy_cs, vol_byc = volsmy_c,
                heightsmry_byc = heightsmry_c,
                compositionsmry_byc = cl_spc))
  })



