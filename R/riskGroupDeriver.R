#' Derive risk group for standard sample compilation/data
#'
#'
#' @description will refine. This function is equivalent to risk_grp.sas for fiz-based process, risk_v3.sas for
#'              bec-based process.
#'
#' @param species character, Tree basic species code, which is SP0 in VRI original data.
#' @param pathIndex character, A character with length of 8, consists of 0 or 1.
#' @param series character, DWB series. It is a lenght of 2 number character and can be derived using
#'                          \code{\link{getDWBSeries}} function.
#' @param height numeric, Total tree height.
#' @param method character, Specifies the method between \code{FIZ} and \code{KBEC} to categorize the risk group. The \code{FIZ} method derives risk
#'                          group by \code{species}, \code{pathIndex}, \code{series} and \code{height}.
#'                          \code{KBEC} method derives the risk group using \code{species} and \code{pathIndex}.
#'
#'
#' @return Risk group, which is character
#'
#' @importFrom data.table data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#'
#'
#' @export
#' @docType methods
#' @rdname riskGroupDeriver
#'
#' @author Yong Luo
#'
setGeneric("riskGroupDeriver",
           function(species, pathIndex, series, height, method) {
             standardGeneric("riskGroupDeriver")
           })

#' @rdname riskGroupDeriver
setMethod(
  "riskGroupDeriver",
  signature = c(species = "character",
                pathIndex = "character",
                series = "character",
                height = "numeric",
                method = "character"),
  definition = function(species, pathIndex, series, height, method){
    worktable <- data.table(uniObs = 1:length(pathIndex), SP0 = species,
                            PATH_IND = pathIndex, RISK_GRP = as.character(NA))
    pathArrayName <- c("CONK", "BCONK", "SCAR", "FRK_CRK", "FRST_CK", "MSLTO", "RTN_BR", "D_BK_TP")
    for (indipath in 1:8){
      if(method == "KBEC" & indipath == 6){
        worktable[, pathArrayName[indipath] := 0]
      } else {
        worktable[, pathArrayName[indipath] := as.numeric(substr(PATH_IND, indipath, indipath))]
      }
    }
    worktable[, ':='(TOT_IND = rowSums(worktable[, pathArrayName, with = FALSE]))]

    if(method == "KBEC"){
      worktable[SP0 %in% c("H", "PY", "PW", "D", "MB", "E",
                           "PA", "PL", "S", "L","F","AC", "B", "AT") &
                  TOT_IND %==% 0,
                RISK_GRP := "1"]
      worktable[SP0 %in% c("H", "PY", "PW", "D", "MB", "E",
                           "PA", "PL", "S", "L","F","AC", "B", "AT") & is.na(RISK_GRP) &
                  !(CONK %==% 1 | BCONK %==% 1),
                RISK_GRP := "2"]
      worktable[SP0 %in% c("H", "PY", "PW", "D", "MB", "E",
                           "PA", "PL", "S", "L","F","AC", "B", "AT") & is.na(RISK_GRP) &
                  (CONK %==% 1 | BCONK %==% 1),
                RISK_GRP := "3"]

      worktable[SP0 %in% c("C", "Y") &
                  TOT_IND %==% 0,
                RISK_GRP := "1"]
      worktable[SP0 %in% c("C", "Y") & is.na(RISK_GRP) &
                  (D_BK_TP %==% 1 | (CONK %==% 1 | BCONK %==% 1)),
                RISK_GRP := "3"]
      worktable[SP0 %in% c("C", "Y") & is.na(RISK_GRP),
                RISK_GRP := "2"]
    } else if (method == "FIZ") {
      worktable[, ':='(SERIES = series, HT = height)]
      worktable[, ':='(SP0SERIES = paste(SP0, SERIES, sep = ""))]
      ## using if statement to speed up the process
      ###########################################
      ###########################################
      ## logic for sp0 = AC cottonwood
      if(nrow(worktable[SP0SERIES %in% c("AC01", "AC02", "AC03", "AC10", "AC11", "AC13"),]) > 0){
        worktable[SP0SERIES %in% c("AC01", "AC02", "AC03", "AC10", "AC11", "AC13"),
                  RISK_GRP := "1"]
      }

      if(nrow(worktable[SP0SERIES %in% c("AC04", "AC12"),]) > 0){
        worktable[SP0SERIES %in% c("AC04", "AC12") &
                    (TOT_IND %==% 0 |
                       (FRK_CRK %==% 1 & TOT_IND %==% 1)),
                  RISK_GRP := "1"]
        worktable[SP0SERIES %in% c("AC04", "AC12") & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }

      if(nrow(worktable[SP0SERIES %in% c("AT01", "AT02", "AT04", "AT12"),]) > 0){ ## logic for AT aspen
        worktable[SP0SERIES %in% c("AT01", "AT02", "AT04", "AT12"),
                  RISK_GRP := "1"]
      }

      if(nrow(worktable[SP0SERIES %in% c("AT03"),]) > 0){
        worktable[SP0SERIES == "AT03" &
                    (TOT_IND %==% 0 |
                       !(CONK %==% 1 | BCONK %==% 1)),
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "AT03" & is.na(RISK_GRP) &
                    (CONK == 1 | BCONK == 1),
                  RISK_GRP := "2"]
      }

      if(nrow(worktable[SP0SERIES %in% c("AT10"),]) > 0){
        worktable[SP0SERIES == "AT10" &
                    TOT_IND %==% 0,
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "AT10" & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }

      if(nrow(worktable[SP0SERIES %in% c("AT11"),]) > 0){
        worktable[SP0SERIES == "AT11" &
                    (TOT_IND %==% 0 |
                       (FRK_CRK %==% 1 & TOT_IND %==% 1)),
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "AT11" & is.na(RISK_GRP) &
                    !(CONK %==% 1 | BCONK %==% 1),
                  RISK_GRP := "2"]
        worktable[SP0SERIES == "AT11" & is.na(RISK_GRP) &
                    (CONK %==% 1 | BCONK %==% 1),
                  RISK_GRP := "3"]

      }


      ##################################
      ##################################
      # logic for B balsam
      if(nrow(worktable[SP0SERIES %in% c("B01", "B03", "B05"),]) > 0){
        worktable[SP0SERIES %in% c("B01", "B03", "B05"),
                  RISK_GRP := "1"]
      }

      if(nrow(worktable[SP0SERIES %in% c("B02", "B04", "B06"),]) > 0){
        worktable[SP0SERIES %in% c("B02", "B04", "B06") &
                    TOT_IND %==% 0,
                  RISK_GRP := "1"]
        worktable[SP0SERIES %in% c("B02", "B04", "B06") & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }

      if(nrow(worktable[SP0SERIES %in% c("B10", "B11", "B12", "B13",
                                         "B14", "B91", "B92", "B93",
                                         "B94", "B95", "B96", "B97",
                                         "B98", "B99"),]) > 0){
        worktable[SP0SERIES %in% c("B10", "B11", "B12", "B13",
                                   "B14", "B91", "B92", "B93",
                                   "B94", "B95", "B96", "B97",
                                   "B98", "B99") &
                    (TOT_IND %==% 0 |
                       (FRK_CRK %==% 1 & TOT_IND %==% 1)),
                  RISK_GRP := "1"]
        worktable[, temppath_tot := rowSums(worktable[,pathArrayName[c(3, 5:8)], with = FALSE])] # **1*1111
        worktable[SP0SERIES %in% c("B10", "B11", "B12", "B13",
                                   "B14", "B91", "B92", "B93",
                                   "B94", "B95", "B96", "B97",
                                   "B98", "B99") & is.na(RISK_GRP) &
                    (((1 %<=% temppath_tot & temppath_tot %<=% 3) |
                        ((1 %<=% temppath_tot & temppath_tot %<=% 2) & FRK_CRK %==% 1)) &
                       !(CONK %==% 1 | BCONK %==% 1) &
                       TOT_IND %<=% 3),
                  RISK_GRP := "2"]
        worktable[, temppath_tot := NULL]
        worktable[SP0SERIES %in% c("B10", "B11", "B12", "B13",
                                   "B14", "B91", "B92", "B93",
                                   "B94", "B95", "B96", "B97",
                                   "B98", "B99") & is.na(RISK_GRP),
                  RISK_GRP := "3"]
      }


      ##############################
      ##############################
      ##  LOGIC FOR SP0=C Cedar
      if(nrow(worktable[SP0SERIES %in% c("C01", "C03"),]) > 0){
        worktable[SP0SERIES %in% c("C01", "C03"),
                  RISK_GRP := "1"]
      }

      if(nrow(worktable[SP0SERIES %in% c("C02", "C04"),]) > 0){
        worktable[SP0SERIES %in% c("C02", "C04") &
                    TOT_IND %==% 0,
                  RISK_GRP := "1"]
        worktable[SP0SERIES %in% c("C02", "C04") & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }

      if(nrow(worktable[SP0SERIES %in% c("C10", "C11", "C12", "C13"),]) > 0){
        worktable[SP0SERIES %in% c("C10", "C11", "C12", "C13") &
                    (TOT_IND %==% 0 |
                       ((FRK_CRK %==% 1 | FRST_CK %==% 1) & TOT_IND %==% 1)),
                  RISK_GRP := "1"]
        worktable[SP0SERIES %in% c("C10", "C11", "C12", "C13") & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }

      if(nrow(worktable[SP0SERIES %in% c("C14"),]) > 0){
        worktable[SP0SERIES == "C14" &
                    (TOT_IND %==% 0 |
                       (RTN_BR %==% 1 & TOT_IND %==% 1)),
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "C14" & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }

      if(nrow(worktable[SP0SERIES %in% c("C91", "C92", "C93", "C94",
                                         "C95", "C97", "C98"),]) > 0){
        worktable[SP0SERIES %in% c("C91", "C92", "C93", "C94",
                                   "C95", "C97", "C98") &
                    (TOT_IND %==% 0 |
                       (RTN_BR %==% 1 & TOT_IND %==% 1)),
                  RISK_GRP := "1"]
        worktable[SP0SERIES %in% c("C91", "C92", "C93", "C94",
                                   "C95", "C97", "C98") & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }

      if(nrow(worktable[SP0SERIES %in% c("C96"),]) > 0){
        worktable[SP0SERIES == "C96" &
                    HT %>=% 40.5,
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "C96" & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }

      if(nrow(worktable[SP0SERIES %in% c("C99"),]) > 0){
        worktable[, temppath_tot := rowSums(worktable[,pathArrayName[c(4,5)], with = FALSE])] # ***11***
        worktable[SP0SERIES == "C99" &
                    (TOT_IND %==% 0 |
                       (temppath_tot %==% 1 & TOT_IND %==% 1)),
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "C99" & is.na(RISK_GRP),
                  RISK_GRP := "2"]
        worktable[, temppath_tot := NULL]
      }

      ###########################################################################
      ###########################################################################
      ### logical for D Alder
      if(nrow(worktable[SP0SERIES %in% c("D01", "D02", "D10"),]) > 0){
        worktable[SP0SERIES %in% c("D01", "D02", "D10"),
                  RISK_GRP := "1"]
      }

      ##############################
      ##############################
      ### LOGIC FOR SP0=E Birch          **/
      if(nrow(worktable[SP0SERIES %in% c("E01", "E02"),]) > 0){
        worktable[SP0SERIES %in% c("E01", "E02"),
                  RISK_GRP := "1"]
      }

      if(nrow(worktable[SP0SERIES %in% c("E10", "E11"),]) > 0){
        worktable[SP0SERIES %in% c("E10", "E11") &
                    TOT_IND %==% 0,
                  RISK_GRP := "1"]
        worktable[SP0SERIES %in% c("E10", "E11") & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }


      ##############################
      ##############################
      ### LOGIC FOR SP0=F Douglas Fir    **/

      if(nrow(worktable[SP0SERIES %in% c("F01", "F02", "F03"),]) > 0){
        worktable[SP0SERIES %in% c("F01", "F02", "F03"),
                  RISK_GRP := "1"]
      }

      if(nrow(worktable[SP0SERIES %in% c("F04"),]) > 0){
        worktable[SP0SERIES == "F04" &
                    TOT_IND %==% 0,
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "F04" & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }

      if(nrow(worktable[SP0SERIES %in% c("F10", "F11"),]) > 0){
        worktable[SP0SERIES %in% c("F10", "F11") &
                    (TOT_IND %==% 0 |
                       ((D_BK_TP %==% 1 | RTN_BR %==% 1 | FRST_CK %==% 1) & TOT_IND %==% 1)),
                  RISK_GRP := "1"]
        worktable[SP0SERIES %in% c("F10", "F11") & is.na(RISK_GRP) &
                    (((MSLTO %==% 1 | FRK_CRK %==% 1 | SCAR %==% 1) & TOT_IND %==% 1) |
                       (TOT_IND %>>% 0 & !(CONK %==% 1 | BCONK %==% 1))),
                  RISK_GRP := "2"]
        worktable[SP0SERIES %in% c("F10", "F11") & is.na(RISK_GRP) &
                    (CONK %==% 1 | BCONK %==% 1),
                  RISK_GRP := "3"]
      }

      if(nrow(worktable[SP0SERIES %in% c("F12"),]) > 0){
        worktable[SP0SERIES == "F12" &
                    (TOT_IND %==% 0 |
                       ((MSLTO %==% 1 | RTN_BR %==% 1 | FRST_CK %==% 1) & TOT_IND %==% 1)),
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "F12" & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }

      if(nrow(worktable[SP0SERIES %in% c("F13"),]) > 0){
        worktable[SP0SERIES == "F13" &
                    (TOT_IND %==% 0 |
                       ((MSLTO %==% 1 | RTN_BR %==% 1) & TOT_IND %==% 1)),
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "F13" & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }

      if(nrow(worktable[SP0SERIES %in% c("F14"),]) > 0){
        worktable[SP0SERIES == "F14" &
                    (TOT_IND %==% 0 |
                       ((MSLTO %==% 1 | D_BK_TP %==% 1 | FRST_CK %==% 1 | RTN_BR %==% 1) & TOT_IND %==% 1)),
                  RISK_GRP := "1"]
        worktable[, ':='(temppath_tot1 = rowSums(worktable[,pathArrayName[c(3,4)], with = FALSE]), # **11****
                         temppath_tot2 = rowSums(worktable[,pathArrayName, with = FALSE]))]        # 11111111
        worktable[SP0SERIES == "F14" & is.na(RISK_GRP) &
                    ((temppath_tot1 %==% 1 & TOT_IND %==% 1) |
                       (temppath_tot2 %>=% 2 & !(CONK %==% 1 | BCONK %==% 1))),
                  RISK_GRP := "2"]
        worktable[, ':='(temppath_tot1 = NULL,
                         temppath_tot2 = NULL)]
        worktable[SP0SERIES == "F14" & is.na(RISK_GRP) &
                    (CONK %==% 1 | BCONK %==% 1),
                  RISK_GRP := "3"]
      }

      if(nrow(worktable[SP0SERIES %in% c("F98", "F99"),]) > 0){
        worktable[, ':='(temppath_tot = rowSums(worktable[,pathArrayName[5:7], with = FALSE]))] # ****111*
        worktable[SP0SERIES %in% c("F98", "F99") &
                    (TOT_IND %==% 0 |
                       (temppath_tot %==% 1 & TOT_IND %==% 1)),
                  RISK_GRP := "1"]
        worktable[SP0SERIES %in% c("F98", "F99") & is.na(RISK_GRP),
                  RISK_GRP := "2"]
        worktable[, temppath_tot := NULL]
      }

      ###########################
      ###########################
      ### LOGIC FOR SP0=H Hemlock
      if(nrow(worktable[SP0SERIES %in% c("H01", "H03"),]) > 0){
        worktable[SP0SERIES %in% c("H01", "H03"),
                  RISK_GRP := "1"]
      }

      if(nrow(worktable[SP0SERIES %in% c("H02", "H04"),]) > 0){
        worktable[SP0SERIES %in% c("H02", "H04") &
                    TOT_IND %==% 0,
                  RISK_GRP := "1"]
        worktable[SP0SERIES %in% c("H02", "H04") & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }

      if(nrow(worktable[SP0SERIES %in% c("H10"),]) > 0){
        worktable[, ':='(temppath_tot = rowSums(worktable[,pathArrayName[3:8], with = FALSE]))] # **111111
        worktable[SP0SERIES == "H10" &
                    (TOT_IND %==% 0 |
                       (temppath_tot %==% 1 & TOT_IND %==% 1)),
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "H10" & is.na(RISK_GRP) &
                    ((temppath_tot %>>% 2 & temppath_tot %<<% 3) & !(CONK %==% 1 | BCONK %==% 1)),
                  RISK_GRP := "2"]
        worktable[SP0SERIES == "H10" & is.na(RISK_GRP),
                  RISK_GRP := "3"]
        worktable[, temppath_tot := NULL]
      }

      if(nrow(worktable[SP0SERIES %in% c("H11", "H14", "H95", "H96", "H97"),]) > 0){
        worktable[, ':='(temppath_tot1 = rowSums(worktable[,pathArrayName[c(6, 8)], with = FALSE]),    # *****1*1
                         temppath_tot2 = rowSums(worktable[,pathArrayName[3:5], with = FALSE]),        # **111***
                         temppath_tot3 = rowSums(worktable[,pathArrayName[c(3:6, 8)], with = FALSE]))] # **1111*1
        worktable[SP0SERIES %in% c("H11", "H14", "H95", "H96", "H97") &
                    (TOT_IND %==% 0 |
                       (temppath_tot1 %==% TOT_IND & TOT_IND %>=% 1 & TOT_IND %<=% 2)),
                  RISK_GRP := "1"]
        worktable[SP0SERIES %in% c("H11", "H14", "H95", "H96", "H97") & is.na(RISK_GRP) &
                    ((temppath_tot2 %==% 1 & TOT_IND %==% 1) |
                       ((2 %<=% temppath_tot3 & temppath_tot3 %<=% 3) & !(RTN_BR %==% 1 | CONK %==% 1 | BCONK %==% 1))),
                  RISK_GRP := "2"]
        worktable[SP0SERIES %in% c("H11", "H14", "H95", "H96", "H97") & is.na(RISK_GRP),
                  RISK_GRP := "3"]
        worktable[, ':='(temppath_tot1 = NULL,
                         temppath_tot2 = NULL,
                         temppath_tot3 = NULL)]
      }

      if(nrow(worktable[SP0SERIES %in% c("H12"),]) > 0){
        worktable[, ':='(temppath_tot1 = rowSums(worktable[,pathArrayName[3:8], with = FALSE]),    # **111111
                         temppath_tot2 = rowSums(worktable[,pathArrayName[1:2], with = FALSE]))]   # 11******
        worktable[SP0SERIES == "H12" &
                    TOT_IND %==% 0,
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "H12" & is.na(RISK_GRP) &
                    ((1 %<=% temppath_tot1 & temppath_tot1 %<=% 3) & temppath_tot2 %==% 0),
                  RISK_GRP := "2"]
        worktable[SP0SERIES == "H12" & is.na(RISK_GRP),
                  RISK_GRP := "3"]
        set(worktable, , c("temppath_tot1", "temppath_tot2"), NULL)
      }

      if(nrow(worktable[SP0SERIES %in% c("H13"),]) > 0){
        worktable[, ':='(temppath_tot1 = rowSums(worktable[,pathArrayName[3:8], with = FALSE]))]   # 11******
        worktable[SP0SERIES == "H13" &
                    TOT_IND %==% 0,
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "H13" & is.na(RISK_GRP) &
                    ((1 %<=% temppath_tot1 & temppath_tot1 %<=% 3) & !(CONK %==% 1 | BCONK %==% 1)),
                  RISK_GRP := "2"]
        worktable[SP0SERIES == "H13" & is.na(RISK_GRP),
                  RISK_GRP := "3"]
        set(worktable, , c("temppath_tot1"), NULL)
      }

      if(nrow(worktable[SP0SERIES %in% c("H84", "H85", "H86", "H87",
                                         "H88", "H89", "H90", "H91",
                                         "H92", "H93", "H94"),]) > 0){
        worktable[, ':='(temppath_tot1 = rowSums(worktable[,pathArrayName[3:8], with = FALSE]),    # **111111
                         temppath_tot2 = rowSums(worktable[,pathArrayName[1:2], with = FALSE]))]   # 11******
        worktable[SP0SERIES %in% c("H84", "H85", "H86", "H87",
                                   "H88", "H89", "H90", "H91",
                                   "H92", "H93", "H94") &
                    TOT_IND %==% 0,
                  RISK_GRP := "1"]
        worktable[SP0SERIES %in% c("H84", "H85", "H86", "H87",
                                   "H88", "H89", "H90", "H91",
                                   "H92", "H93", "H94") & is.na(RISK_GRP) &
                    (1 %<=% temppath_tot1 & temppath_tot1 %<=% 3 & temppath_tot2 %==% 0),
                  RISK_GRP := "2"]
        worktable[SP0SERIES %in% c("H84", "H85", "H86", "H87",
                                   "H88", "H89", "H90", "H91",
                                   "H92", "H93", "H94") & is.na(RISK_GRP),
                  RISK_GRP := "3"]
        worktable[, ':='(temppath_tot1 = NULL,
                         temppath_tot2 = NULL)]
      }

      if(nrow(worktable[SP0SERIES %in% c("H98"),]) > 0){
        worktable[, ':='(temppath_tot1 = rowSums(worktable[,pathArrayName[c(6, 8)], with = FALSE]),    # *****1*1
                         temppath_tot2 = rowSums(worktable[,pathArrayName[c(1:5, 7)], with = FALSE]),  # 11111*1*
                         temppath_tot3 = rowSums(worktable[,pathArrayName[c(3:5, 7)], with = FALSE]),  # **111*1*
                         temppath_tot4 = rowSums(worktable[,pathArrayName[3:8], with = FALSE]))]       # **111111
        worktable[SP0SERIES == "H98" &
                    (TOT_IND %==% 0 |
                       (1 %<=% temppath_tot1 & temppath_tot1 %<=% 2 & temppath_tot2 %==% 0)),
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "H98" & is.na(RISK_GRP) &
                    ((temppath_tot3 %==% 1 & TOT_IND %==% 1) |
                       ((2 %<=% temppath_tot4 & temppath_tot4 %<=% 3) & !(CONK %==% 1 | BCONK %==% 1))),
                  RISK_GRP := "2"]
        worktable[SP0SERIES == "H98" & is.na(RISK_GRP),
                  RISK_GRP := "3"]
        worktable[, ':='(temppath_tot1 = NULL,
                         temppath_tot2 = NULL,
                         temppath_tot3 = NULL,
                         temppath_tot4 = NULL)]
      }

      if(nrow(worktable[SP0SERIES %in% c("H99"),]) > 0){
        worktable[, ':='(temppath_tot1 = rowSums(worktable[,pathArrayName[c(6, 8)], with = FALSE]),    # *****1*1
                         temppath_tot2 = rowSums(worktable[,pathArrayName[c(1:5, 7)], with = FALSE]),  # 11111*1*
                         temppath_tot3 = rowSums(worktable[,pathArrayName[3:5], with = FALSE]),         # **111***
                         temppath_tot4 = rowSums(worktable[,pathArrayName[c(1:2, 7)], with = FALSE]))]  # 11****1*
        worktable[SP0SERIES == "H99" &
                    (TOT_IND %==% 0 |
                       (1 %<=% temppath_tot1 & temppath_tot1 %<=% 2 & temppath_tot2 %==% 0)),
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "H99" & is.na(RISK_GRP) &
                    ((temppath_tot3 %==% 1 & TOT_IND %==% 1)|
                       (temppath_tot4 %==% 0 & 2 %<=% TOT_IND & TOT_IND %<=% 3)),
                  RISK_GRP := "2"]
        worktable[SP0SERIES == "H99" & is.na(RISK_GRP),
                  RISK_GRP := "3"]
        worktable[, ':='(temppath_tot1 = NULL,
                         temppath_tot2 = NULL,
                         temppath_tot3 = NULL,
                         temppath_tot4 = NULL)]
      }

      ###########################
      ###########################
      ##LOGIC FOR SP0=L Larch          **/
      if(nrow(worktable[SP0SERIES %in% c("L01"),]) > 0){
        worktable[SP0SERIES == "L01",
                  RISK_GRP := "1"]
      }

      if(nrow(worktable[SP0SERIES %in% c("L02"),]) > 0){
        worktable[SP0SERIES == "L02" &
                    TOT_IND %==% 0,
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "L02" & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }

      if(nrow(worktable[SP0SERIES %in% c("L10"),]) > 0){
        worktable[SP0SERIES == "L10" &
                    TOT_IND %==% 0,
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "L10" & is.na(RISK_GRP) &
                    !(CONK %==% 1 | BCONK %==% 1),
                  RISK_GRP := "2"]
        worktable[SP0SERIES == "L10" & is.na(RISK_GRP),
                  RISK_GRP := "3"]
      }

      ####################################
      ####################################
      ### LOGIC FOR SP0=MB Maple          **/
      if(nrow(worktable[SP0SERIES %in% c("MB01", "MB02"),]) > 0){
        worktable[SP0SERIES %in% c("MB01", "MB02"),
                  RISK_GRP := "1"]
      }

      if(nrow(worktable[SP0SERIES %in% c("MB10"),]) > 0){
        worktable[SP0SERIES == "MB10" &
                    TOT_IND %==% 0,
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "MB10" & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }

      ########################################
      ########################################
      ## LOGIC FOR SP0=PA White Bark Pine**/
      if(nrow(worktable[SP0SERIES %in% c("PA01", "PA03"),]) > 0){
        worktable[SP0SERIES %in% c("PA01", "PA03"),
                  RISK_GRP := "1"]
      }

      if(nrow(worktable[SP0SERIES %in% c("PA02", "PA04", "PA10", "PA11"),]) > 0){
        worktable[SP0SERIES %in% c("PA02", "PA04", "PA10", "PA11") &
                    TOT_IND %==% 0,
                  RISK_GRP := "1"]
        worktable[SP0SERIES %in% c("PA02", "PA04", "PA10", "PA11") & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }


      ########################################
      ########################################
      ## LOGIC FOR SP0=PL Lodgepole Pine **/
      if(nrow(worktable[SP0SERIES %in% c("PL01"),]) > 0){
        worktable[SP0SERIES == "PL01",
                  RISK_GRP := "1"]
      }

      if(nrow(worktable[SP0SERIES %in% c("PL02", "PL03", "PL04", "PL10", "PL98"),]) > 0){
        worktable[SP0SERIES %in% c("PL02", "PL03", "PL04", "PL10", "PL98") &
                    TOT_IND %==% 0,
                  RISK_GRP := "1"]
        worktable[SP0SERIES %in% c("PL02", "PL03", "PL04", "PL10", "PL98") & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }

      if(nrow(worktable[SP0SERIES %in% c("PL11", "PL12", "PL13", "PL97", "PL99"),]) > 0){
        worktable[SP0SERIES %in% c("PL11", "PL12", "PL13", "PL97", "PL99") &
                    TOT_IND %==% 0,
                  RISK_GRP := "1"]
        worktable[SP0SERIES %in% c("PL11", "PL12", "PL13", "PL97", "PL99") & is.na(RISK_GRP) &
                    (TOT_IND %>>% 0 & !(CONK %==% 1 | BCONK %==% 1)),
                  RISK_GRP := "2"]
        worktable[SP0SERIES %in% c("PL11", "PL12", "PL13", "PL97", "PL99") & is.na(RISK_GRP) &
                    (CONK %==% 1 | BCONK %==% 1),
                  RISK_GRP := "3"]
      }


      ############################################
      ############################################
      ## LOGIC FOR SP0=PW White Pine     **/
      if(nrow(worktable[SP0SERIES %in% c("PW01", "PW03"),]) > 0){
        worktable[SP0SERIES %in% c("PW01", "PW03"),
                  RISK_GRP := "1"]

      }

      if(nrow(worktable[SP0SERIES %in% c("PW02", "PW04", "PW10", "PW11"),]) > 0){
        worktable[SP0SERIES %in% c("PW02", "PW04", "PW10", "PW11") &
                    TOT_IND %==% 0,
                  RISK_GRP := "1"]
        worktable[SP0SERIES %in% c("PW02", "PW04", "PW10", "PW11") & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }


      #######################################
      #######################################
      ## LOGIC FOR SP0=PY Yellow Pine    **/
      if(nrow(worktable[SP0SERIES %in% c("PY01"),]) > 0){
        worktable[SP0SERIES == "PY01",
                  RISK_GRP := "1"]
      }

      if(nrow(worktable[SP0SERIES %in% c("PY02"),]) > 0){
        worktable[SP0SERIES == "PY02" &
                    TOT_IND %==% 0,
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "PY02" & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }

      if(nrow(worktable[SP0SERIES %in% c("PY10"),]) > 0){
        worktable[SP0SERIES == "PY10" &
                    (TOT_IND %==% 0 |
                       (FRK_CRK %==% 1 & TOT_IND %==% 1)),
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "PY10" & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }

      ####################################
      ####################################
      ## LOGIC FOR SP0=S Spruce         **/
      if(nrow(worktable[SP0SERIES %in% c("S01", "S02", "S03"),]) > 0){
        worktable[SP0SERIES %in% c("S01", "S02", "S03"),
                  RISK_GRP := "1"]
      }

      if(nrow(worktable[SP0SERIES %in% c("S04"),]) > 0){
        worktable[SP0SERIES == "S04" &
                    TOT_IND %==% 0,
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "S04" & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }

      if(nrow(worktable[SP0SERIES %in% c("S10"),]) > 0){
        worktable[, ':='(temppath_tot = rowSums(worktable[,pathArrayName[3:8], with = FALSE]))] #**111111
        worktable[SP0SERIES == "S10" &
                    (TOT_IND %==% 0 |
                       (temppath_tot %==% 1 & TOT_IND %==% 1)),
                  RISK_GRP := "1"]
        worktable[SP0SERIES == "S10" & is.na(RISK_GRP) &
                    ((CONK %==% 1 | BCONK %==% 1) |
                       temppath_tot %>=% 2),
                  RISK_GRP := "2"]
        set(worktable, , c("temppath_tot"), NULL)
      }

      if(nrow(worktable[SP0SERIES %in% c("S11", "S12", "S13", "S95",
                                         "S96", "S97", "S98", "S99"),]) > 0){
        worktable[SP0SERIES %in% c("S11", "S12", "S13", "S95",
                                   "S96", "S97", "S98", "S99") &
                    TOT_IND %==% 0,
                  RISK_GRP := "1"]
        worktable[SP0SERIES %in% c("S11", "S12", "S13", "S95",
                                   "S96", "S97", "S98", "S99") & is.na(RISK_GRP) &
                    !(CONK %==% 1 | BCONK %==% 1),
                  RISK_GRP := "2"]
        worktable[SP0SERIES %in% c("S11", "S12", "S13", "S95",
                                   "S96", "S97", "S98", "S99") & is.na(RISK_GRP) &
                    (CONK %==% 1 | BCONK %==% 1),
                  RISK_GRP := "3"]
      }

      ####################################
      ####################################
      ## LOGIC FOR SP0=Y Yellow Cedar   **/
      if(nrow(worktable[SP0SERIES %in% c("Y01"),]) > 0){
        worktable[SP0SERIES == "Y01",
                  RISK_GRP := "1"]
      }

      if(nrow(worktable[SP0SERIES %in% c("Y02", "Y10"),]) > 0){
        worktable[SP0SERIES %in% c("Y02", "Y10") &
                    TOT_IND %==% 0,
                  RISK_GRP := "1"]
        worktable[SP0SERIES %in% c("Y02", "Y10") & is.na(RISK_GRP),
                  RISK_GRP := "2"]
      }
    }
    return(worktable[order(uniObs),]$RISK_GRP)
  })

#' @rdname riskGroupDeriver
setMethod(
  "riskGroupDeriver",
  signature = c(species = "character",
                pathIndex = "character",
                series = "missing",
                height = "missing",
                method = "character"),
  definition = function(species, pathIndex, series, height, method){
    if(method == "FIZ"){
      stop("For FIZ method, series and height must be provided.")
    } else {
      return(riskGroupDeriver(species, pathIndex, series = as.character(NA),
                              height = as.numeric(NA), method))
    }
  })

