#' Adjust log length - VRI specific
#'
#' @description The function is to adjust the log length to fit actual height.
#'              This function is equivalent to \code{log_adj_new} macro in original VRI compiler
#'
#' @param treeData data.table, Must have tree data information. The table is an output of \code{\link{VRIInit_measuredTree}}.
#'
#' @param stumpHeight numeric, Length of stump. As default, this arguement is set as 0.3 m.
#'
#'
#'
#' @return Data table that contains the adjusted log length
#'
#' @importFrom data.table ':='
#'
#' @note Please see Bob for details about input files
#' @export
#' @docType methods
#' @rdname logAdjustment
#'
#' @author Yong Luo
#'
setGeneric("logAdjustment",
           function(treeData, stumpHeight) {
             standardGeneric("logAdjustment")
           })

#' @rdname logAdjustment
setMethod(
  "logAdjustment",
  signature = c(treeData = "data.table",
                stumpHeight = "numeric"),
  definition = function(treeData, stumpHeight){
    treedata_skipped <- treeData[MEAS_INTENSE %in% c("H-ENHANCED", "NON-ENHANCED"),]
    treeData <- treeData[MEAS_INTENSE %in% c("ENHANCED", "FULL"),]
    for(i in 1:9){
      treeData[, paste("LOG_L_", i, sep = "") := as.numeric(unlist(treeData[, paste("LOG_L_", i, sep = ""),
                                                                            with = FALSE]))]
      treeData[, paste("LOG_G_", i, sep = "") := as.character(unlist(treeData[, paste("LOG_G_", i, sep = ""),
                                                                              with = FALSE]))]
      treeData[, paste("LOG_S_", i, sep = "") := as.numeric(unlist(treeData[, paste("LOG_S_", i, sep = ""),                                                                    with = FALSE]))]
    }
    log_len <- paste("LOG_L_", 1:9, sep = "")
    treeData[, SUM_LOG := rowSums(treeData[, log_len, with = FALSE], na.rm = TRUE)]
    treeData[, DIFF := HEIGHT - SUM_LOG]
    ### flag the observations that can not be adjust for log length
    ## 1. tree height information is not available, 2. log length information is missing
    ## 3. number of log information is not correct (i.e., NO_LOGS is NA or 0 or > 8)
    treeData[HEIGHT %in% c(NA, 0) | SUM_LOG %==% 0 |
               NO_LOGS %in% c(NA, 0) | NO_LOGS %>>% 8, LOGADJUST := "FAIL"]

    # print(treeData[LOGADJUST == "FAIL",.(BTOP, HEIGHT, HT_TOTAL, SUM_LOG, NO_LOGS,
    #                                      LOG_L_1, LOG_L_2, LOG_S_1, LOG_S_2,
    #                                      DIFF)])
    #      BTOP HEIGHT   HT_TOTAL SUM_LOG NO_LOGS LOG_L_1 LOG_L_2 LOG_S_1 LOG_S_2
    # 1:   NA     NA   NA    22.0       1    22.0      NA     100      NA
    # 2:   NA     NA   NA    28.3       3     7.0     7.0     100      98
    # 3:   NA    0.0  0.0     0.0       1      NA      NA     100      NA
    # 4:   NA    0.0  0.0     0.0       1      NA      NA     100      NA
    # 5:   NA    0.0  0.0     0.0       1      NA      NA     100      NA
    # 6:    D    9.0 66.7     0.0       2      NA     0.0       5       0
    # 7:    D    5.6 29.7     0.0       2      NA     0.0      90       0
    # 8:    D   19.5 25.4     0.0       2      NA     0.0      45       0
    # 9:    D    5.9 18.7     0.0       2      NA     0.0      NA       0
    # 10:   NA     NA   NA     7.4       1     7.4      NA     100      NA
    # 11:   NA     NA   NA    11.0       1    11.0      NA     100      NA
    # 12:   NA     NA   NA    29.7       2     5.0    24.7     100     100
    # 13:   NA     NA   NA    36.4       2    16.0    20.4     100     100
    # 14:   NA     NA   NA    17.4       2    14.0     3.4      99     100
    # 15:   NA     NA   NA    15.0       2     5.0    10.0     100     100
    # 16:   NA     NA   NA     6.3       1     6.3      NA     100      NA
    # 17:   NA     NA   NA    21.1       2     5.0    16.1     100     100
    # 18:   NA     NA   NA    34.8       3     7.0     6.0      66     100
    # 19:   NA     NA   NA     0.0       1      NA      NA     100      NA

    ##### There are three conditions for the failure trees
    ## a. height information is not available and log length is not available, e.g., row 3
    ### no solution to estimate height and log length.
    ## b. height information is available and log information is not (mostly for btop), e.g., row 6
    ### should we estimate log length? how to estimate?
    ## c. log length information is available and height is not, e.g., row 10
    ### should we estimate tree height by using total length of logs?
    usingSolution <- TRUE
    if(usingSolution){
      ## for condition b, just can adjust for no_logs == 2
      treeData[LOGADJUST == "FAIL" & !is.na(BTOP) & NO_LOGS == 2 & HEIGHT %>>% 0 & HT_TOTAL %>>% 0,
               ':='(LOG_L_1 = HEIGHT, ## USING broken height as lenghth of log 1
                    LOGADJUST = "PASS",
                    DIFF = 0)]
      ## for conditon c, tree height is estimated as total length of log
      treeData[LOGADJUST == "FAIL" & is.na(HT_TOTAL) & SUM_LOG %>>% 0,
               ':='(HT_TOTAL = SUM_LOG,
                    LOGADJUST = "PASS",
                    DIFF = 0)]
      ## leave condition a as is
    }

    treeData[DIFF %==% 0 | LOGADJUST == "FAIL", CAT := "exact"]

    ###***** the original codes should be translated into the following R codes
    ###***** however, the column dibbr_vg and bkht_veg can not be found
    # treeData[is.na(CAT) & is.na(BTOP) & dibbr_vg == 0 & bkht_veg == 0,
    #          LASTLOG := NO_LOGS]
    # CURRENTLY HANDLE THIS WITH THE CODES BELOW:
    exactTreeData <- treeData[CAT == "exact",]
    exactTreeData[is.na(LOGADJUST), LOGADJUST := "PASS"]
    unexactTreeData <- treeData[is.na(CAT),]
    if(nrow(unexactTreeData) > 0){
      #### the next processes are done for unexactTreeData
      unexactTreeData[is.na(BTOP), LASTLOG := NO_LOGS]
      unexactTreeData[!is.na(BTOP), LASTLOG := NO_LOGS-1L]
      lastlogs <- unique(unexactTreeData$LASTLOG)
      for(indilastlog in lastlogs){
        unexactTreeData[LASTLOG == indilastlog,
                        templog := unexactTreeData[LASTLOG == indilastlog,
                                                   paste("LOG_L_",
                                                         indilastlog, sep = ""),
                                                   with = FALSE]]
      }
      rm(lastlogs, indilastlog)
      unexactTreeData[DIFF %<<% -1 | abs(templog - DIFF) %>>% 0,
                      ':='(finalLastloglength = templog + DIFF,
                           LOGADJUST = "PASS")]
      adjusteddata <- unexactTreeData[LOGADJUST == "PASS", ]
      lastlogs <- unique(adjusteddata$LASTLOG)
      for(indilastlog in lastlogs){
        adjusteddata[LASTLOG == indilastlog,
                     paste("LOG_L_",
                           indilastlog, sep = "") := adjusteddata[LASTLOG == indilastlog,
                                                                  "finalLastloglength",
                                                                  with = FALSE]]
      }
      rm(lastlogs, indilastlog)


      unadjusteddata <- unexactTreeData[is.na(LOGADJUST),]
      if(nrow(unadjusteddata) > 0){
        unadjusteddata_BTOPNA <- unadjusteddata[is.na(BTOP),]
        nologs <- unique(unadjusteddata_BTOPNA$NO_LOGS)
        for(indinolog in nologs){
          unadjusteddata_BTOPNA[NO_LOGS == indinolog,
                                paste("LOG_L_", indinolog, sep = "") := 0]
          unadjusteddata_BTOPNA[NO_LOGS == indinolog,
                                paste("LOG_G_", indinolog, sep = "") := NA]
          unadjusteddata_BTOPNA[NO_LOGS == indinolog,
                                paste("LOG_S_", indinolog, sep = "") := 0]
        }
        rm(nologs, indinolog)

        unadjusteddata_BTOP <- unadjusteddata[!is.na(BTOP),]
        nologs <- unique(unadjusteddata_BTOP$NO_LOGS)
        for(indinolog in nologs){
          unadjusteddata_BTOP[NO_LOGS == indinolog,
                              paste("LOG_L_", indinolog-1, sep = "") :=
                                unadjusteddata_BTOP[NO_LOGS == indinolog,
                                                    paste("LOG_L_", indinolog, sep = ""),
                                                    with = FALSE]]
          unadjusteddata_BTOP[NO_LOGS == indinolog,
                              paste("LOG_G_", indinolog-1, sep = "") :=
                                unadjusteddata_BTOP[NO_LOGS == indinolog,
                                                    paste("LOG_G_", indinolog, sep = ""),
                                                    with = FALSE]]
          unadjusteddata_BTOP[NO_LOGS == indinolog,
                              paste("LOG_S_", indinolog-1, sep = "") :=
                                unadjusteddata_BTOP[NO_LOGS == indinolog,
                                                    paste("LOG_S_", indinolog, sep = ""),
                                                    with = FALSE]]
        }

        unadjusteddata <- rbind(unadjusteddata_BTOP,
                                unadjusteddata_BTOPNA)
        rm(unadjusteddata_BTOP,
           unadjusteddata_BTOPNA)

        unadjusteddata[, NO_LOGS := NO_LOGS-1]
      }

      treeData <- rbindlist(list(exactTreeData, adjusteddata,
                                 unadjusteddata[, LOGADJUST := "PASS"]), fill = TRUE)
      rm(exactTreeData, adjusteddata, unadjusteddata)
      treeData[, c("LASTLOG", "templog", "finalLastloglength") := NULL]

    } else {
      treeData <- exactTreeData
    }


    ## the below is to assign the last log length as missing height for btop trees
    treeDataBTOP <- treeData[!is.na(BTOP), ]


    nologs <- unique(treeDataBTOP$NO_LOGS)
    for(indinolog in nologs){
      treeDataBTOP[NO_LOGS == indinolog,
                   paste("LOG_L_", indinolog, sep = "") := HT_TOTAL - HEIGHT]
    }
    rm(nologs, indinolog)

    treeData <- rbind(treeData[is.na(BTOP),], treeDataBTOP)
    rm(treeDataBTOP)
    treeData[, c("SUM_LOG", "DIFF", "CAT") := NULL]
    treeData[(LOG_L_1 %<=% stumpHeight | is.na(LOG_L_1)) &
               LOGADJUST != "FAIL",
             ':='(LOGADJUST = "SMALLFIRSTLOG")]
    treeData[LOG_L_1 %>>% stumpHeight, ':='(LOG_L_0 = stumpHeight,
                                            LOG_L_1 = LOG_L_1 - stumpHeight)]
    treeData <- data.table::rbindlist(list(treeData, treedata_skipped),
                                      fill = TRUE)
    treeData[MEAS_INTENSE == "H-ENHANCED", ':='(NO_LOGS = 1,
                                                LOG_L_0 = stumpHeight,
                                                LOG_L_1 = HEIGHT - stumpHeight)]
    treeData[MEAS_INTENSE == "H-ENHANCED", paste("LOG_L_", 2:9, sep = "") := NA]

    return(treeData)
  })



#' @export
#' @rdname logAdjustment
setMethod("logAdjustment",
          signature = signature(treeData = "data.table",
                                stumpHeight = "missing"),
          definition = function(treeData){
            return(logAdjustment(treeData, stumpHeight = 0.3))
          })
