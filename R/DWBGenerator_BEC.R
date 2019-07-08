#' Collect decay, waste and breakage factor in BEC routine
#' 
#' 
#' @description This function is to collect the dead, waste and breakage factor from lookup table and 
#'              join them into tree data. Instead of reading the lookup table from disk, the function
#'              uses hard-coded the lookup table. This function is equivalent to \code{dwb_v3.sas} macro. 
#'              For \code{FIZ} routine, the decay, waste and breakage are collected using funtion
#'              \code{\link{DWBGenerator_FIZ}}
#'
#' @param DBH numeric, Tree DBH.
#' @param height numeric, Tree height.
#' @param species character, Tree basic species code, which is SP0 in VRI original data. 
#' @param meanAge numeric, Mean site age.
#' @param BEC character, BC BEC zone.
#' @param riskGroup character, Specifies the risk group.  It must be one of \code{1}, \code{2} or \code{3}.
#'                             It can be derived from \code{\link{riskGroupDeriver}}.
#' @param adjustID character, Adjustment identifier. Blank is no adjustment; QCI is queen charlottes; WET is wetbelt
#'                            and GLD_NW golden ?.
#' 
#' @return A list of \code{decay}, \code{waste} and \code{breakage} percentage.
#' 
#' @importFrom data.table data.table
#'
#' 
#' @export
#' @docType methods
#' @rdname DWBGenerator_BEC
#'
#' @author Yong Luo
#'
setGeneric("DWBGenerator_BEC",
           function(DBH, height, species, meanAge, BEC, riskGroup, adjustID) {
             standardGeneric("DWBGenerator_BEC")
           })

#' @rdname DWBGenerator_BEC
setMethod(
  "DWBGenerator_BEC",
  signature = c(DBH = "numeric",
                height = "numeric",
                species = "character",
                meanAge = "numeric",
                BEC = "character",
                riskGroup = "character",
                adjustID = "character"),
  definition = function(DBH, height, species, meanAge, BEC, riskGroup, adjustID){
    worktable <- data.table(uniObs = 1:length(DBH),
                            DBH = DBH, HT = height, SP0 = species, AGEM = meanAge,
                            BEC, RISK_V3 = riskGroup, ADJ_ID = adjustID)
    rm(DBH, height, species, meanAge, BEC, riskGroup, adjustID)
    validdata <- worktable[DBH > 0 & HT > 0 & !is.na(SP0) & !is.na(BEC) & RISK_V3 %in% c("1", "2", "3")]
    invaliddata <- worktable[!(uniObs %in% validdata$uniObs)]
    rm(worktable)
    validdata[, ':='(BEC_I_C = BEC2IC(BEC))]
    needColumns <- c(paste("B0_", 1:3, sep = ""), paste("B1_", 1:3, sep = ""), paste("B2_", 1:3, sep = ""),
                     paste("C0_", 1:3, sep = ""), paste("C1_", 1:3, sep = ""), paste("D0_", 1:3, sep = ""),
                     paste("D00_", 1:3, sep = ""), 
                     paste("D1_", 1:3, sep = ""), paste("D2_", 1:3, sep = ""), paste("D3_", 1:3, sep = ""),
                     paste("DBH5_", 1:3, sep = ""), paste("DBH95_", 1:3, sep = ""),
                     paste("AGE95_", 1:3, sep = ""))
    dcy_v3x <- lookup_dcy_v3x()
    dcy_v3x <- dcy_v3x[, c("ADJ_ID", "SP0", needColumns, "VOL_MULT", "DCY_OFST"),
                       with = FALSE]
    validdata <- merge(validdata, dcy_v3x, by = c("ADJ_ID", "SP0"),
                       all.x = TRUE)
    rm(dcy_v3x)
    adjIDvaliddata <- validdata[!is.na(B0_1), ]
    naadjidvaliddata <- validdata[!(uniObs %in% adjIDvaliddata$uniObs),]
    rm(validdata)
    set(naadjidvaliddata, , needColumns, NULL)
    dcy_v3 <- lookup_dcy_v3()
    dcy_v3 <- dcy_v3[, c("BEC", "SP0", needColumns),
                     with = FALSE]
    naadjidvaliddata <- merge(naadjidvaliddata, dcy_v3, by = c("BEC", "SP0"),
                              all.x = TRUE)
    validdata <- rbindlist(list(naadjidvaliddata,
                                adjIDvaliddata[, names(naadjidvaliddata), with = FALSE]))
    rm(naadjidvaliddata, adjIDvaliddata, dcy_v3)
    validdata[is.na(VOL_MULT),':='(VOL_MULT = 1,
                                   DCY_OFST = 0)]
    validdataAssigned <- validdata[!is.na(B0_1),]
    validdataUnassigned <- validdata[!(uniObs %in% validdataAssigned$uniObs),]
    set(validdataUnassigned, ,needColumns, NULL)
    rm(validdata)
    
    ## need to confirm with Rene about the condition
    ## for risk_v3 %in% c("1", "2", "3")
    validdataAssigned[RISK_V3 %in% c("1", "2", "3"), # or RISK_V3 >= 1, need to confirm
                      XDBH := DBH]
    validdataAssigned[RISK_V3 %in% c("1", "2", "3") & DBH %<<% DBH5_1,
                      ':='(XDBH = DBH5_1,
                           BND = "L")]
    validdataAssigned[RISK_V3 %in% c("1", "2", "3") & DBH %>>% DBH95_1,
                      ':='(XDBH = DBH95_1,
                           BND = "U")]
    validdataAssigned[RISK_V3 %in% c("1", "2", "3"), ':='(AGEM1 = pmin(AGEM, AGE95_1))]
    
    validdataAssigned[RISK_V3 %in% c("1", "2", "3") & AGEM %>=% D00_1, # AGEM >= D00_1
                      LG_HAT := B0_1 + B1_1*log(XDBH/30)+B2_1/AGEM1]
    
    validdataAssigned[RISK_V3 %in% c("1", "2", "3") & (1 %<<% AGEM & AGEM %<<% D00_1) & # 1 < AGEM < D00_1
                        BND == "L",
                      XDBH := DBH]
    validdataAssigned[RISK_V3 %in% c("1", "2", "3") & (1 %<<% AGEM & AGEM %<<% D00_1),
                      LG_HAT := D0_1 + D1_1*log(XDBH/30) + D2_1/AGEM + D3_1*log(XDBH/30)/AGEM]
    
    validdataAssigned[RISK_V3 %in% c("1", "2", "3") & is.na(LG_HAT), # AGEM <= 1
                      LG_HAT := C0_1 + C1_1*log(XDBH/30)]
    set(validdataAssigned, , c("XDBH", "AGEM1"), NULL)
    ## for risk_v3 %in% c("2", "3")
    validdataAssigned[RISK_V3 %in% c("2", "3"), # or RISK_V3 >= 2, need to confirm
                      XDBH := DBH]
    validdataAssigned[RISK_V3 %in% c("2", "3") & DBH %<<% DBH5_2,
                      ':='(XDBH = DBH5_2,
                           BND = "L")]
    validdataAssigned[RISK_V3 %in% c("2", "3") & DBH %>>% DBH95_2,
                      ':='(XDBH = DBH95_2,
                           BND = "U")]
    
    validdataAssigned[RISK_V3 %in% c("2", "3"), ':='(AGEM2 = pmin(AGEM, AGE95_2))]
    
    validdataAssigned[RISK_V3 %in% c("2", "3") & AGEM %>=% D00_2, # AGEM >= D00_2
                      LG_HAT_temp := B0_2 + B1_2*log(XDBH/30)+B2_2/AGEM2]
    
    validdataAssigned[RISK_V3 %in% c("2", "3") & (1 %<<% AGEM & AGEM %<<% D00_2) & # 1 < AGEM < D00_2
                        BND == "L",
                      XDBH := DBH]
    validdataAssigned[RISK_V3 %in% c("2", "3") & (1 %<<% AGEM & AGEM %<<% D00_2),
                      LG_HAT_temp := D0_2 + D1_2*log(XDBH/30) + D2_2/AGEM + D3_2*log(XDBH/30)/AGEM]
    
    validdataAssigned[RISK_V3 %in% c("2", "3") & is.na(LG_HAT_temp), # AGEM <= 1
                      LG_HAT_temp := C0_2 + C1_2*log(XDBH/30)]
    validdataAssigned[!is.na(LG_HAT_temp), LG_HAT := pmax(LG_HAT_temp, LG_HAT)]
    
    set(validdataAssigned, , c("XDBH", "AGEM2", "LG_HAT_temp"), NULL)
    ## for risk_v3 == "3"
    validdataAssigned[RISK_V3 == "3", # or RISK_V3 >= 3, need to confirm
                      XDBH := DBH]
    validdataAssigned[RISK_V3 == "3" & DBH %<<% DBH5_3,
                      ':='(XDBH = DBH5_3,
                           BND = "L")]
    validdataAssigned[RISK_V3 == "3" & DBH %>>% DBH95_3,
                      ':='(XDBH = DBH95_3,
                           BND = "U")]
    
    validdataAssigned[RISK_V3 == "3", ':='(AGEM3 = pmin(AGEM, AGE95_3))]
    
    validdataAssigned[RISK_V3 == "3" & AGEM %>=% D00_3, # AGEM >= D00_3
                      LG_HAT_temp := B0_3 + B1_3*log(XDBH/30)+B2_3/AGEM3]
    
    validdataAssigned[RISK_V3 == "3" & (1 %<<% AGEM & AGEM %<<% D00_3) & # 1 < AGEM < D00_3
                        BND == "L",
                      XDBH := DBH]
    validdataAssigned[RISK_V3 == "3" & (1 %<<% AGEM & AGEM %<<% D00_3),
                      LG_HAT_temp := D0_3 + D1_3*log(XDBH/30) + D2_3/AGEM + D3_3*log(XDBH/30)/AGEM]
    
    validdataAssigned[RISK_V3 == "3" & is.na(LG_HAT_temp), # AGEM <= 1
                      LG_HAT_temp := C0_3 + C1_3*log(XDBH/30)]
    validdataAssigned[!is.na(LG_HAT_temp), LG_HAT := pmax(LG_HAT_temp, LG_HAT)]
    set(validdataAssigned, , c("XDBH", "AGEM3", "LG_HAT_temp"), NULL)
    validdataAssigned[, LG_HAT := LG_HAT + DCY_OFST]
    validdataAssigned[LG_HAT %<<% -10, LG_HAT := -10]
    validdataAssigned[LG_HAT %>>% 10, LG_HAT := 10]
    validdataAssigned[, HAT := exp(LG_HAT)/(1+exp(LG_HAT))]
    validdataAssigned[, PCT_DCY := 100*HAT]
    set(validdataAssigned, ,c(needColumns, "BND", "LG_HAT", "HAT"),
        NULL)
    validdata <- rbindlist(list(validdataAssigned, 
                                validdataUnassigned[, PCT_DCY := as.numeric(NA)]))
    rm(validdataAssigned, validdataUnassigned)
    set(validdata, , c("VOL_MULT", "DCY_OFST"), NULL)
    ##### for breakage
    validdata[AGEM %<=% 40 | is.na(AGEM), AGE40 := 40]
    validdata[AGEM %<=% 120 & is.na(AGE40), AGE40 := 120]
    validdata[is.na(AGE40), AGE40 := 160]
    brk_99 <- lookup_brk_99()
    validdata <- merge(validdata, brk_99, by = c("SP0", "RISK_V3", "BEC_I_C", "AGE40"),
                       all.x = TRUE)
    validdata[, c("AGE40", "BEC_I_C") := NULL]
    rm(brk_99)  
    
    ##### for waste
    wst_v3 <- lookup_wst_v3()[, FITPROG := NULL]
    validdata <- merge(validdata, wst_v3, by = c("SP0"),
                       all.x = TRUE)
    rm(wst_v3)
    validdata[!is.na(W1), FRD := round(PCT_DCY, 1)/100]
    validdata[!is.na(W1), LOGIT := W0 + W1*FRD + W3*log(DBH) + W4*log(HT)]
    validdata[!is.na(W1), PCT_WST := (1-exp(W2*FRD))*(exp(LOGIT)/(1+exp(LOGIT)))*(100 - PCT_DCY)]
    validdata[, c(paste("W", 0:4, sep = ""), "FRD", "LOGIT") := NULL]
    
    validdata[PCT_WST %>>% PCT_DCY, PCT_WST := PCT_DCY]
    validdata[(PCT_WST + PCT_DCY + PCT_BRK) %>=% 100, PCT_BRK := pmax(0, (100 - PCT_WST - PCT_DCY))]
    invaliddata[, c("PCT_WST", "PCT_DCY", "PCT_BRK") := NA]
    alldata <- rbindlist(list(invaliddata,
                              validdata[, names(invaliddata), with = FALSE]))
    alldata <- alldata[order(uniObs),]
    return(list(decay = alldata$PCT_DCY, 
                waste = alldata$PCT_WST,
                breakage = alldata$PCT_BRK))
  })





