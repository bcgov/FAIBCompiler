#' Compile decay, waste and breakage for standard tables-VRI specific
#'
#'
#' @description This function compiles decay, waste and breakage for standard tables in VRI compiler. The function
#'              is equivalent to \code{dwb_vri_2017.sas}.
#'
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
           function(treeMS, siteAge, treeLossFactors, equation) {
             standardGeneric("DWBCompiler")
           })

#' @rdname DWBCompiler
setMethod(
  "DWBCompiler",
  signature = c(treeMS = "data.table",
                siteAge = "data.table",
                treeLossFactors = "data.table",
                equation = "character"),
  definition = function(treeMS, siteAge, treeLossFactors, equation){
    # treeMS <- data.table::copy(tree_ms6)
    # siteAge <- data.table::copy(siteAgeTable)
    # treeLossFactors <- data.table::copy(treelossfactors)
    loss_fct <- FAIBBase::merge_dupUpdate(treeMS[, uniobs := 1:nrow(treeMS)],
                                treeLossFactors,
                                by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                                all.x = TRUE)
    rm(treeLossFactors)

    ## REMOVE THE OBSERVATIONS THAT LOSS INDICATORS WITH NO MATCHING TREES
    ##
    loss_fct[MEAS_INTENSE == "H-ENHANCED",
             ':='(PATH_IND = as.character(NA))]
    loss_fct[substr(CLSTR_ID, 11, 11) == "B",
             ':='(MEAS_INTENSE = "B-SAMPLE", # trees are measured dbh and height, without net factoring
                                    PATH_IND = as.character(NA))]
    loss_fct_output <- loss_fct[MEAS_INTENSE %in% c("H-ENHANCED", "B-SAMPLE"),.(uniobs, PCT_DCY = as.numeric(NA),
                                                               PCT_WST = as.numeric(NA),
                                                               PCT_BRK = as.numeric(NA),
                                                               AGE_DWB = as.numeric(NA),
                                                               AGE_FLG = as.character(NA),
                                                               PATH_IND,
                                                               RISK_GRP = as.numeric(NA),
                                                               ADJ_ID = as.character(NA))]
    loss_fct <- loss_fct[MEAS_INTENSE %in% c("FULL", "ENHANCED"),]
    loss_fct[, PATH_IND := "00000000"]
    for(i in 1:8){
      loss_fct[, ':='(tempLOC_FRO = as.numeric(unlist(loss_fct[, paste("LOC", i, "_FRO", sep = ""), with = FALSE])),
                      tempLOSS_IN = unlist(loss_fct[, paste("LOSS", i, "_IN", sep = ""), with = FALSE]))]
      loss_fct[tempLOSS_IN != "" & is.na(tempLOC_FRO), tempLOC_FRO := 0]
      loss_fct[(tempLOC_FRO %<<% H_MERCH | is.na(H_MERCH)) &
                 substr(tempLOSS_IN, 1, 2) %in% c("DD", "DR"),
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
                 tempLOSS_IN %in% c("AFC", "NGC"),
               PATH_IND := paste(substr(PATH_IND, 1, 4), "1", substr(PATH_IND, 6, 8), sep = "")]
      loss_fct[(tempLOC_FRO %<<% H_MERCH | is.na(H_MERCH)) &
                 tempLOSS_IN %in% c("LRB"),
               PATH_IND := paste(substr(PATH_IND, 1, 6), "1", substr(PATH_IND, 8, 8), sep = "")]
      loss_fct[(tempLOC_FRO %<<% H_MERCH | is.na(H_MERCH)) &
                 tempLOSS_IN %in% c("DTP", "BTP"),
               PATH_IND := paste(substr(PATH_IND, 1, 7), "1", sep = "")]
    }

    rm(i)
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
    if(equation == "FIZ"){
      tree_rsk[LV_D == "L" & PATH_IND == "00000000", TR_CLASS := "1"]
      tree_rsk[LV_D == "L" & PATH_IND != "00000000", TR_CLASS := "2"]
      tree_rsk[LV_D == "D", TR_CLASS := "3"]
      # call functions in risk_grp macro in orginal codes
      ## instead of using one function, the following uses several function that inside of risk_grp macro
      ## select data that is valid for process
      tree_rsk[,  SP_N := as.numeric(factor(SP0,
                                            levels = c("F", "C", "H", "B", "S", "Y", "PW",
                                                       "PL", "PY", "L", "AC", "D", "MB", "E", "AT", "PA"),
                                            labels = 1:16))]
      processdata <- tree_rsk[SP_N > 0 & ("A" <= FIZ & FIZ <= "L") &
                                DBH > 0 & AGE_DWB > 0 & HEIGHT > 0 & nchar(PATH_IND) == 8, ]
      unprocessdata <- tree_rsk[!(uniObs %in% processdata$uniObs),] # the data that is not able to be
      # derived a risk group
      processdata[PSYU_BLK == "", PSYU_BLK := "0"]
      processdata[, PSYUB := paste(PSYU, PSYU_BLK, sep = "")]
      # call ageRangeClassifier function, equivalent to age_rng macro
      processdata[, AGE_RNG := ageRangeClassifier(age = AGE_DWB, species = SP0, FIZ = FIZ)]
      # call getDWBSeries function, equivalent to dwb_fct.sas macro
      # get series number from local first baded on species, ageRangeClass and PSYUB
      processdata[, PSY_SER := getDWBSeries(species = SP0,
                                            ageRangeClass = AGE_RNG,
                                            PSYUB = PSYUB,
                                            source = "local")]
      processdata[!is.na(PSY_SER), ':='(SERIES = PSY_SER,
                                        DWB_SRC = "L")]

      ## try series number from zonal for the rest
      processdata[is.na(SERIES), FIZ_SER := getDWBSeries(species = SP0,
                                                         ageRangeClass = AGE_RNG,
                                                         FIZ = FIZ,
                                                         source = "zonal")]
      processdata[is.na(SERIES) & !is.na(FIZ_SER), ':='(SERIES = FIZ_SER,
                                                        DWB_SRC = "Z")]

      ## try reversing zonal method for the rest
      processdata[is.na(SERIES), FIZ_SER_rev := getDWBSeries(species = SP0,
                                                             ageRangeClass = AGE_RNG,
                                                             FIZ = FIZ,
                                                             source = "reversingZonal")]
      processdata[is.na(SERIES) & !is.na(FIZ_SER_rev), ':='(SERIES = FIZ_SER_rev,
                                                            DWB_SRC = "R")]
      processdata[,c("PSY_SER", "FIZ_SER", "FIZ_SER_rev") := NULL]

      ## assign tab_no
      processdata[nchar(as.character(SP_N)) == 1, TAB_NO := paste("0", SP_N, SERIES, sep = "")]
      processdata[nchar(as.character(SP_N)) == 2, TAB_NO := paste(SP_N, SERIES, sep = "")]

      ## assign risk group based on sp0, series, path index and height
      processdata[, RISK_GRP := riskGroupDeriver(species = SP0,
                                                 series = SERIES,
                                                 pathIndex = PATH_IND,
                                                 height = HEIGHT,
                                                 method = "FIZ")]
      processdata[, RG_MAX := as.character(max(as.numeric(RISK_GRP), na.rm = TRUE)),
                  by = c("SP0", "SERIES")]
      processdata[TR_CLASS == "3", RISK_GRP := RG_MAX]
      processdata[, RG_MAX := NULL]
      processdata[, DBH_CL := DBHClassifier(DBH = DBH)]
      DWBfactors <- DWBGenerator_FIZ(DBHClass = processdata$DBH_CL,
                                     tabNumber = processdata$TAB_NO,
                                     riskGroup = processdata$RISK_GRP)
      processdata[, ':='(DECAY = DWBfactors$decay,
                         WASTE = DWBfactors$waste,
                         BREAK = DWBfactors$breakage)]
      rm(DWBfactors)
      set(unprocessdata, , c("PSYUB", "AGE_RNG", "SERIES", "DWB_SRC",
                             "TAB_NO", "RISK_GRP", "DBH_CL", "DECAY", "WASTE", "BREAK"),
          NA)
      tree_rsk <- rbind(processdata, unprocessdata)
      tree_rsk <- tree_rsk[order(uniObs)]
      rm(processdata, unprocessdata)
    } else if (equation == "KBEC"){
      # certain areas of the province use specialized loss adjustment factors
      # they are determined by the following code
      tree_rsk[SP0 %in% c("C", "Y", "S", "H") &
                 (PROJ_ID == "3432" | TSA == 25),
               ADJ_ID := "QCI"] ## need space or not will be specified
      tree_rsk[BGC_ZONE == "ICH" & BGC_SBZN %in% c("wk", "vk") & BGC_VAR == "1",
               ADJ_ID := "WET"]
      tree_rsk[(TSA == 7 | PROJ_ID == "0071") & BGC_ZONE == "ICH" & BGC_SBZN %in% c("mv", "mk"),
               ADJ_ID := "GLD_NW"]
      tree_rsk[, RISK_GRP := riskGroupDeriver(species = SP0,
                                              pathIndex = PATH_IND,
                                              method = equation)]
      DWBfactors <- DWBGenerator_BEC(DBH = tree_rsk$DBH,
                                     height = tree_rsk$HEIGHT,
                                     species = tree_rsk$SP0,
                                     meanAge = tree_rsk$AGE_DWB,
                                     BEC = tree_rsk$BGC_ZONE,
                                     riskGroup = tree_rsk$RISK_GRP,
                                     adjustID = tree_rsk$ADJ_ID)
      tree_rsk[, ':='(PCT_DCY = DWBfactors$decay,
                      PCT_WST = DWBfactors$waste,
                      PCT_BRK = DWBfactors$breakage)]
      rm(DWBfactors)
    }
    tree_rsk[substr(CLSTR_ID, 1, 4) == "3472" &
               substr(CLSTR_ID, 11, 11) == "M" &
               (AGE_DWB %<<% 121 | is.na(AGE_DWB)),
             ':='(PCT_BRK = 2, PCT_ADJ = "Y")]
    tree_rsk[substr(CLSTR_ID, 1, 4) == "3472" &
               substr(CLSTR_ID, 11, 11) == "M" &
               (AGE_DWB %>=% 121),
             ':='(PCT_BRK = 4, PCT_ADJ = "Y")]
    tree_rsk <- rbindlist(list(tree_rsk[,.(uniobs, PCT_DCY, PCT_WST, PCT_BRK,
                                           AGE_DWB, AGE_FLG, PATH_IND, RISK_GRP,
                                           ADJ_ID)],
                               loss_fct_output))
    tree_rsk <- tree_rsk[order(uniobs),]
    tree_rsk[, uniobs:=NULL]
    treeMS <- cbind(treeMS[order(uniobs),],
                    tree_rsk)
    treeMS[, uniobs := NULL]
    rm(tree_rsk)
    treeMS <- applyDWB(treeMS)
    return(treeMS)
  })


#' @export
#' @rdname DWBCompiler
setMethod(
  "DWBCompiler",
  signature = c(treeMS = "data.table",
                siteAge = "data.table",
                treeLossFactors = "data.table",
                equation = "missing"),
  definition = function(treeMS, siteAge, treeLossFactors){
    return(DWBCompiler(treeMS, siteAge, treeLossFactors, equation = "KBEC"))
  })
