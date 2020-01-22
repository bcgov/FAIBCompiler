#' Summarizes the volume for small trees - VRI specific
#'
#'
#' @description Calculates and summarizes volume of small trees at both cluster/species level and cluster.
#'              This function is equivalent to \code{sml_tree.sas} in original compiler.
#'
#' @param smallTreeData data.table, Small tree data. This data is from card f, i.e., \code{vi_f}.
#' @param smallTreePlotHeader data.table, Plot header data for stump and small tree data. The data is from card e, i.e.,
#'                                    \code{vi_e}.
#'
#'
#' @return Two tables: stmp_c is summarized volume at cluster level; stmp_cs is summarized volume at cluster/species level.
#'
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#' @importFrom FAIBBase merge_dupUpdate
#'
#'
#' @export
#' @docType methods
#' @rdname smallTreeVolSmry
#'
#' @author Yong Luo
#'
setGeneric("smallTreeVolSmry",
           function(smallTreeData, smallTreePlotHeader){
             standardGeneric("smallTreeVolSmry")
           })

#' @rdname smallTreeVolSmry
setMethod(
  "smallTreeVolSmry",
  signature = c(smallTreeData = "data.table",
                smallTreePlotHeader = "data.table"),
  definition = function(smallTreeData, smallTreePlotHeader){
    clustersummaries <- unique(smallTreePlotHeader[,.(CLSTR_ID)], by = "CLSTR_ID")
    smallTreePlotHeader <- smallTreePlotHeader[,.(CLSTR_ID, F_FULL, F_HALF, F_QRTR, F_RAD)]
    smallTreePlotHeader <- smallTreePlotHeader[!duplicated(smallTreePlotHeader),]
    sm_tr <- merge(smallTreeData, smallTreePlotHeader,
                   by = "CLSTR_ID", all.x = TRUE)
    rm(smallTreeData, smallTreePlotHeader)
    sm_tr[F_FULL == "X", PLOT_WT := 1]
    sm_tr[F_HALF == "X", PLOT_WT := 2]
    sm_tr[F_QRTR == "X", PLOT_WT := 4]

    sm_tr[,':='(SMTR1_HA = round(TOTAL1 * 10000/(pi * PLOT_WT * (F_RAD^2)), 1),
                SMTR2_HA = round(TOTAL2 * 10000/(pi * PLOT_WT * (F_RAD^2)), 1),
                SMTR3_HA = round(TOTAL3 * 10000/(pi * PLOT_WT * (F_RAD^2)), 1))]
    sm_tr[,':='(SMTR_HA = SMTR1_HA + SMTR2_HA + SMTR3_HA,
                SMTR_TOT = TOTAL1 + TOTAL2 + TOTAL3)]
    setnames(sm_tr, paste("TOTAL", 1:3, sep = ""), paste("SMTR_CT", 1:3, sep = ""))
    smtr_cs <- sm_tr[,.(SMTR_CT1 = sum(SMTR_CT1),
                        SMTR_CT2 = sum(SMTR_CT2),
                        SMTR_CT3 = sum(SMTR_CT3),
                        SMTR_TOT = sum(SMTR_TOT),
                        SMTR1_HA = sum(SMTR1_HA),
                        SMTR2_HA = sum(SMTR2_HA),
                        SMTR3_HA = sum(SMTR3_HA),
                        SMTR_HA = sum(SMTR_HA)),
                     by = c("CLSTR_ID", "SPECIES")]

    smtr_c <- sm_tr[,.(SMTR_TO1 = sum(SMTR_CT1),
                       SMTR_TO2 = sum(SMTR_CT2),
                       SMTR_TO3 = sum(SMTR_CT3),
                       SMTR_TT = sum(SMTR_TOT),
                       SMTR1_HT = sum(SMTR1_HA),
                       SMTR2_HT = sum(SMTR2_HA),
                       SMTR3_HT = sum(SMTR3_HA),
                       SMTR_HT = sum(SMTR_HA)),
                    by = c("CLSTR_ID")]
    clustersummaries <- FAIBBase::merge_dupUpdate(clustersummaries, smtr_c, by = "CLSTR_ID",
                                        all.x = TRUE)
    rm(smtr_c)
    sp_cmp1 <- speciesComp_byC(CSSmryTable = smtr_cs, basedOn = "SMTR_CT1", speciesMaxNO = 12,
                               smallTreeCompile = TRUE)
    setnames(sp_cmp1, "SPB_CPCT", "ST1_CMP")
    clustersummaries <- FAIBBase::merge_dupUpdate(clustersummaries, sp_cmp1, by = "CLSTR_ID", all.x = TRUE)
    rm(sp_cmp1)

    sp_cmp2 <- speciesComp_byC(CSSmryTable = smtr_cs, basedOn = "SMTR_CT2", speciesMaxNO = 12,
                               smallTreeCompile = TRUE)
    setnames(sp_cmp2, "SPB_CPCT", "ST2_CMP")
    clustersummaries <- FAIBBase::merge_dupUpdate(clustersummaries, sp_cmp2, by = "CLSTR_ID", all.x = TRUE)
    rm(sp_cmp2)

    sp_cmp3 <- speciesComp_byC(CSSmryTable = smtr_cs, basedOn = "SMTR_CT3", speciesMaxNO = 12,
                               smallTreeCompile = TRUE)
    setnames(sp_cmp3, "SPB_CPCT", "ST3_CMP")
    clustersummaries <- FAIBBase::merge_dupUpdate(clustersummaries, sp_cmp3, by = "CLSTR_ID", all.x = TRUE)
    rm(sp_cmp3)
    sp_cmp4 <- speciesComp_byC(CSSmryTable = smtr_cs, basedOn = "SMTR_TOT", speciesMaxNO = 12,
                               smallTreeCompile = TRUE)
    setnames(sp_cmp4, "SPB_CPCT", "STT_CMP")
    clustersummaries <- FAIBBase::merge_dupUpdate(clustersummaries, sp_cmp4, by = "CLSTR_ID", all.x = TRUE)
    rm(sp_cmp4)
    return(list(clusterSummaries = clustersummaries, clusterSpeciesSummaries = smtr_cs))
  })
