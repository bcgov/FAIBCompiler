#' Compile decay, waste and breakage for standard tables
#'
#'
#' @description This function compiles decay, waste and breakage for standard tables in VRI compiler. The function
#'              is equivalent to \code{dwb_vri_2017.sas}.
#' @param compilationType character, either \code{PSP} or \code{nonPSP}. If it is \code{PSP}, it
#'                               is consistent with original PSP compiler, otherwise, it
#'                               is consistent with VRI compiler.
#' @param treeMS data.table, Tree-level data that has been compiled whole stem volume and gross merchantable volume for full and enhanced trees.
#'
#' @param siteAge data.table, Cluster-level summaries of age and height. This table is an output from
#'                            \code{\link{siteAgeSummary}}
#' @param treeLossFactors data.table, The tree loss factor data, an output of \code{\link{VRIInit_lossFactor}}.
#'                        In this funtion, this table provides loss indicator.
#' @param equation character, Specifies whether the compiler is based on \code{KFIZ} or \code{KBEC}.
#'                                       Default is set as \code{KBEC}.
#'
#' @return A compiled volume after removing decay, waste and breakage; a log file
#'
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#' @importFrom FAIBBase merge_dupUpdate DBHClassifier
#'
#' @export
#' @docType methods
#' @rdname DWBCompiler
#'
#' @author Yong Luo
#'
setGeneric("DWBCompiler",
           function(compilationType, treeMS, siteAge, treeLossFactors, equation) {
             standardGeneric("DWBCompiler")
           })

#' @rdname DWBCompiler
setMethod(
  "DWBCompiler",
  signature = c(compilationType = "character",
                treeMS = "data.table",
                siteAge = "data.table",
                treeLossFactors = "data.table",
                equation = "character"),
  definition = function(compilationType, treeMS, siteAge, treeLossFactors, equation){
    loss_fct <- merge(treeMS[, uniobs := 1:nrow(treeMS)],
                      treeLossFactors,
                      by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                      all.x = TRUE)
    rm(treeLossFactors)

    if(compilationType == "PSP"){
      set(loss_fct, , c(paste0("LOC", 6:8, "_FRO"),
                        paste0("LOSS", 6:8, "_IN")), NA)
      loss_fct$PATH_IND <- pathIndicatorGenerator(lossIndicatorMatix = loss_fct[,paste0("LOSS", 1:8, "_IN"), with = FALSE],
                                                  compiler = "PSP")
    } else {
      loss_fct$PATH_IND <- pathIndicatorGenerator(lossIndicatorMatix = loss_fct[,paste0("LOSS", 1:8, "_IN"), with = FALSE],
                                                  lossIndicatorLocMatrix = loss_fct[,paste0("LOC", 1:8, "_FRO"), with = FALSE],
                                                  merchantableHeight = loss_fct$H_MERCH,
                                                  compiler = "VRI")
    }
    siteAge[, ':='(AGE_DWB = AT_M_TLS,
                   AGE_FLG = "NONE")]
    siteAge[!is.na(AT_M_TLS), AGE_FLG := "TLS"]
    siteAge[is.na(AGE_DWB), ':='(AGE_DWB = ageByForester(projectID = PROJ_ID,
                                                         sampleNumber = SAMP_NO,
                                                         sampleTypeCode = TYPE_CD),
                                 AGE_FLG = "ASGN")]
    siteAge[is.na(AGE_DWB), AGE_FLG := "NONE"]
    siteAge[is.na(AGE_DWB) & !is.na(AT_M_TXO), ':='(AGE_DWB = AT_M_TXO,
                                                    AGE_FLG = "TXO")]
    tree_rsk <- merge(loss_fct, siteAge[,.(CLSTR_ID, AGE_DWB, AGE_FLG)],
                      by = c("CLSTR_ID"), all.x = TRUE)
    rm(loss_fct, siteAge)
    tree_rsk[, ':='(ADJ_ID = as.character(NA))]

    # certain areas of the province use specialized loss adjustment factors
    # they are determined by the following code
    tree_rsk[SP0 %in% c("C", "Y", "S", "H") &
               (PROJ_ID == "3432" | TSA == 25),
             ADJ_ID := "QCI"] ## need space or not will be specified
    tree_rsk[BEC_ZONE == "ICH" & BEC_SBZ %in% c("wk", "vk") & BEC_VAR == "1",
             ADJ_ID := "WET"]
    tree_rsk[(TSA == 7 | PROJ_ID == "0071") & BEC_ZONE == "ICH" & BEC_SBZ %in% c("mv", "mk"),
             ADJ_ID := "GLD_NW"]
    tree_rsk[, RISK_GRP := riskGroupDeriver(species = SP0,
                                            pathIndex = PATH_IND,
                                            method = equation)]
    DWBfactors <- DWBGenerator_BEC(DBH = tree_rsk$DBH,
                                   height = tree_rsk$HEIGHT,
                                   species = tree_rsk$SP0,
                                   meanAge = tree_rsk$AGE_DWB,
                                   BEC = tree_rsk$BEC_ZONE,
                                   riskGroup = tree_rsk$RISK_GRP,
                                   adjustID = tree_rsk$ADJ_ID)
    tree_rsk[, ':='(PCT_DCY = DWBfactors$decay,
                    PCT_WST = DWBfactors$waste,
                    PCT_BRK = DWBfactors$breakage)]
    rm(DWBfactors)
    tree_rsk[PROJ_ID == "3472" &
               TYPE_CD == "M" &
               (AGE_DWB %<<% 121 | is.na(AGE_DWB)),
             ':='(PCT_BRK = 2, PCT_ADJ = "Y")]
    tree_rsk[PROJ_ID == "3472" &
               TYPE_CD == "M" &
               (AGE_DWB %>=% 121),
             ':='(PCT_BRK = 4, PCT_ADJ = "Y")]
    tree_rsk <- tree_rsk[,.(uniobs, PCT_DCY, PCT_WST, PCT_BRK,
                            AGE_DWB, AGE_FLG, PATH_IND, RISK_GRP,
                            ADJ_ID)]
    tree_rsk <- tree_rsk[order(uniobs),]
    tree_rsk[, uniobs:=NULL]
    treeMS <- cbind(treeMS[order(uniobs),],
                    tree_rsk)
    treeMS[, uniobs := NULL]
    rm(tree_rsk)
    if(compilationType == "PSP"){
      treeMS[, VOL_NETM := VOL_MER]
      set(treeMS, , c(paste0("LOG_S_", 1:9),
                      paste0("LOG_VM_", 1:9)),
          as.numeric(NA))
      treeMS[,':='(LOG_VM_1 = VOL_MER,
                   LOG_S_1 = 100)]
    }
    treeMS <- applyDWB(treeMS)
    return(treeMS)
  })


#' @export
#' @rdname DWBCompiler
setMethod(
  "DWBCompiler",
  signature = c(compilationType = "character",
                treeMS = "data.table",
                siteAge = "data.table",
                treeLossFactors = "data.table",
                equation = "missing"),
  definition = function(compilationType, treeMS, siteAge, treeLossFactors){
    return(DWBCompiler(compilationType, treeMS, siteAge, treeLossFactors, equation = "KBEC"))
  })
