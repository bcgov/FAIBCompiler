#' Summarizes the tally for small trees - VRI specific
#'
#'
#' @description Calculates and summarizes tally of small trees at both cluster/species level and cluster.
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
#' @rdname smallTreeSmry
#'
#' @author Yong Luo
#'
setGeneric("smallTreeSmry",
           function(smallTreeData, smallTreePlotHeader){
             standardGeneric("smallTreeSmry")
           })

#' @rdname smallTreeSmry
setMethod(
  "smallTreeSmry",
  signature = c(smallTreeData = "data.table",
                smallTreePlotHeader = "data.table"),
  definition = function(smallTreeData, smallTreePlotHeader){
    #    SMALL_TREE_TALLY_CLASS_CODE                                                                            DESCRIPTION
    # 1:                           1 Live trees with height greater than 1.3 meters with diameter less than 2 cm (PSP only)
    # 2:                           2                                            Live trees with height between 10 and 29 cm
    # 3:                           3                                    Live trees with height between 30 cm and 1.3 meters
    # 4:                           4            Live trees with height greater than 1.3 meters with diameter less than 4 cm
    # 5:                           5                               Live trees with height between 1.5 and 3.9 cm (PSP only)
    # 6:                           6                               Live trees with height between 4.0 and 6.4 cm (PSP only)
    # 7:                           7                               Live trees with height between 6.5 and 9.0 cm (PSP only)

    clustersummaries <- unique(smallTreePlotHeader[,.(CLSTR_ID)], by = "CLSTR_ID")
    smallTreePlotHeader <- smallTreePlotHeader[,.(CLSTR_ID, F_FULL, F_HALF, F_QRTR, F_RAD)]
    smallTreePlotHeader <- smallTreePlotHeader[!duplicated(smallTreePlotHeader),]
    sm_tr <- merge(smallTreeData, smallTreePlotHeader,
                   by = "CLSTR_ID", all.x = TRUE)
    rm(smallTreeData, smallTreePlotHeader)
    sm_tr[F_FULL %in% c("X", TRUE), PLOT_WT := 1]
    sm_tr[F_HALF %in% c("X", TRUE), PLOT_WT := 2]
    sm_tr[F_QRTR %in% c("X", TRUE), PLOT_WT := 4]
    totalnames <- names(sm_tr)
    totalnames <- totalnames[substr(totalnames, 1, 5) == "TOTAL"]
    totalnames <- sort(gsub("TOTAL", "", totalnames))
    sm_tr[, ':='(SMTR_HA = 0,
                 SMTR_TOT = 0)]
    for (indiname in totalnames) {
      setnames(sm_tr, paste0("TOTAL", indiname), "tempcol")
      sm_tr[,':='(SMTR_HA_temp = round(tempcol * 10000/(pi * PLOT_WT * (F_RAD^2)), 1))]
      sm_tr[,':='(SMTR_HA = SMTR_HA_temp + SMTR_HA,
                  SMTR_TOT = tempcol + SMTR_TOT)]
      setnames(sm_tr, c("tempcol", "SMTR_HA_temp"), c(paste0("SMTR_CT", indiname),
                                                      paste0("SMTR", indiname, "_HA")))
    }
    totalnames <- names(sm_tr)
    totalnames <- totalnames[substr(totalnames, 1, 4) %in% c("SMTR")]
    totalnames <- sort(totalnames)
    rm(indiname)
    smtr_cs <- unique(sm_tr[,.(CLSTR_ID, SPECIES)])
    sm_tr_org <- data.table::copy(sm_tr)
    sm_tr <- data.table::copy(sm_tr_org)

    for (indiname in totalnames) {
      setnames(sm_tr, indiname, "tempcol")
      indismry_cs <- sm_tr[,.(tempcol = sum(tempcol)),
                           by = c("CLSTR_ID", "SPECIES")]
      smtr_cs <- merge(smtr_cs,
                       indismry_cs,
                       by = c("CLSTR_ID", "SPECIES"),
                       all.x = TRUE)
      smtr_cs[is.na(tempcol), tempcol := 0]
      setnames(smtr_cs, "tempcol", indiname)

      indismry_c <- sm_tr[,.(tempcol = sum(tempcol)),
                          by = c("CLSTR_ID")]
      clustersummaries <- merge(clustersummaries,
                                indismry_c,
                                by = c("CLSTR_ID"),
                                all.x = TRUE)
      clustersummaries[is.na(tempcol), tempcol := 0]
      setnames(clustersummaries, "tempcol", indiname)
      sm_tr[, tempcol := NULL]

      if(substr(indiname, 1, 5) == "SMTR_"){
        sp_cmp_indi <- speciesComp_byC(CSSmryTable = indismry_cs,
                                       basedOn = "tempcol", speciesMaxNO = 12,
                                       smallTreeCompile = TRUE)
        if(substr(indiname, 1, 7) == "SMTR_CT"){

          setnames(sp_cmp_indi, "SPB_CPCT", paste0("ST", gsub("SMTR_CT", "", indiname), "_CMP"))
        } else {
          setnames(sp_cmp_indi, "SPB_CPCT", "STT_CMP")

        }
        clustersummaries <- FAIBBase::merge_dupUpdate(clustersummaries,
                                                      sp_cmp_indi,
                                                      by = "CLSTR_ID",
                                                      all.x = TRUE)
        rm(sp_cmp_indi)
      }
      rm(indismry_c, indismry_cs)
    }
    rm(indiname)
    totalnames <- names(clustersummaries)
    totalnames <- gsub("SMTR_CT", "SMTR_TO", totalnames)
    totalnames <- gsub("_HA", "_HT", totalnames)
    names(clustersummaries) <- totalnames
    return(list(clusterSummaries = clustersummaries, clusterSpeciesSummaries = smtr_cs))
  })
