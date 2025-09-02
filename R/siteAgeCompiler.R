#' Compile breast age, total age, and site index where possible-VRI specific
#'
#'
#' @description This function takes site age tree data ie., \code{vi_h}, an output of \code{\link{VRIInit_siteTree}} to
#'              compute the breast height age, total age, and site index where possible. This function is
#'              equivalent to site_age.sas. The function heavily depends on site tools program.
#'
#' @param siteAgeData data.table, Site age data with plot header information.
#'                                An output from \code{\link{VRIInit_siteTree}} function.
#' @param compilationType character, Compilation type.
#' @return A data table and a log file.
#'
#' @importFrom data.table ':='
#' @importFrom dplyr left_join
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#' @importFrom SIndexR SIndexR_SpecRemap
#' @importFrom FAIBBase annualGrowthRateCalculator
#'
#' @export
#' @docType methods
#' @rdname siteAgeCompiler
#'
#' @author Yong Luo
#'
setGeneric("siteAgeCompiler",
           function(siteAgeData, compilationType) {
             standardGeneric("siteAgeCompiler")
           })

#' @rdname siteAgeCompiler
setMethod(
  "siteAgeCompiler",
  signature = c(siteAgeData = "data.table",
                compilationType = "character"),
  definition = function(siteAgeData, compilationType){
    displaycol <- c("CLSTR_ID", "PLOT", "TREE_NO")
    siteAgeData <- cbind(data.table(uniObs = 1:nrow(siteAgeData)),
                         siteAgeData)
    siteAgeData[, BARK_TEMP := as.numeric(BARK_THK)]
    ## get ic region from bec zone
    siteAgeData[, REGION_IC := BEC2IC(BEC_ZONE)]

    siteAgeData[is.na(BARK_THK), BARK_TEMP := BNG_DIAM/20]
    siteAgeData[BARK_THK %>>% 0 & BNG_DIAM %>>% 0, BARK_PCT := 100*((BARK_THK*0.2)/BNG_DIAM)]
    ## CALL annualGrowthRataCalculator function
    siteAgeData[!is.na(BNG_DIAM), ':='(RATE_5 = FAIBBase::annualGrowthRateCalculator(boredDiameter = BNG_DIAM,
                                                                           growthIncrement = GROW_5YR,
                                                                           growthYear = 5,
                                                                           barkThickness = BARK_TEMP),
                                       RATE_10 = FAIBBase::annualGrowthRateCalculator(boredDiameter = BNG_DIAM,
                                                                            growthIncrement = GROW_10YR,
                                                                            growthYear = 10,
                                                                            barkThickness = BARK_TEMP),
                                       RATE_20 = FAIBBase::annualGrowthRateCalculator(boredDiameter = BNG_DIAM,
                                                                            growthIncrement = GROW_20YR,
                                                                            growthYear = 20,
                                                                            barkThickness = BARK_TEMP))]
    siteAgeData[BARK_THK %>>% 0 & BNG_DIAM %>>% 0,
                BARK_PCT := 100*(BARK_THK*0.2)/BNG_DIAM]

    siteAgeData[, SP_SINDEX := siteToolsSpeciesConvertor(species = SPECIES)]

    siteAgeData[, SI_SP := SIndexR::SIndexR_SpecRemap(SP_SINDEX, REGION_IC)]

    siteAgeData[, HEIGHT := TREE_LEN]
    siteAgeData[, ':='(HT_OLD = HEIGHT)]
    siteAgeData[HEIGHT %<=% 1.3, ':='(HEIGHT = 1.31)]

    ## assign bored age, first part of the age-ind.sas
    ## need check with rene to make sure the order makes sense
    siteAgeData[, unitreeid := paste0(SITE_IDENTIFIER, "-", PLOT, "-", TREE_NO)]
    siteAgeData[, HT_CALC := BORED_HT]
    siteAgeData[!(BORE_AGE_LAB %in% c(NA, 0)),
                ':='(AGE_BOR = boredAgeCalculator_Bore(officeBoredAge = as.numeric(BORE_AGE_LAB)),
                     BORED_AGE_SOURCE = "Lab Age")]
    siteAgeData[is.na(BORED_AGE_SOURCE) &
                  !(BORE_AGE_FLD %in% c(NA, 0)),
                ':='(AGE_BOR = boredAgeCalculator_Bore(fieldBoredAge = as.numeric(BORE_AGE_FLD)),
                     BORED_AGE_SOURCE = "Field Age")]
    siteAgeData[is.na(BORED_AGE_SOURCE) &
                  !(TOTAL_AG %in% c(NA, 0)),
                ':='(AGE_BOR = boredAgeCalculator_Total(as.numeric(TOTAL_AG)),
                     BORED_AGE_SOURCE = "Total Age")]
    totagetrees <- unique(siteAgeData[BORED_AGE_SOURCE == "Total Age"]$unitreeid)
    siteAgeData[unitreeid %in% totagetrees,
                BORED_AGE_SOURCE := "Total Age"]

    siteAgeData[is.na(BORED_AGE_SOURCE) &
                  !(PHYS_AGE %in% c(NA, 0)),
                ':='(AGE_BOR = boredAgeCalculator_Phys(as.numeric(PHYS_AGE)),
                     BORED_AGE_SOURCE = "Physiologic Age")]
    physagetrees <- unique(siteAgeData[BORED_AGE_SOURCE == "Physiologic Age"]$unitreeid)
    siteAgeData[unitreeid %in% physagetrees,
                BORED_AGE_SOURCE := "Physiologic Age"]


    ## for the rot and crc, if prorated age is missing
    ## use microscope_age as prorated age, if microscope_age is missing
    siteAgeData[AGE_MEASURE_CODE %in% c("ROT", "CRC") &
                  PRO_LEN %>>% 0 &
                  PRO_RING %>>% 0,
                ':='(AGE_BOR = boredAgeCalculator_Prorated(ringLength_prorated = PRO_LEN,
                                                           ringCount_prorated = PRO_RING,
                                                           boreDiameter = BNG_DIAM,
                                                           barkThickness = BARK_TEMP),
                     BORED_AGE_SOURCE = "Prorated Age")]
    proragetrees <- unique(siteAgeData[BORED_AGE_SOURCE == "Prorated Age"]$unitreeid)
    siteAgeData[unitreeid %in% proragetrees,
                BORED_AGE_SOURCE := "Prorated Age"]
    siteAgeData[MEAS_YR == MEAS_YR_REF,
                age_ref := AGE_BOR]
    siteAgeData[, age_ref := mean(age_ref, na.rm = TRUE),
                by = "unitreeid"]
    siteAgeData[BORED_AGE_SOURCE %in% c("Total Age", "Physiologic Age", "Prorated Age"),
                AGE_BOR := age_ref - (MEAS_YR_REF - MEAS_YR)]
    siteAgeData[,':='(BORED_AGE_FINAL = AGE_BOR)]
    ## call boredagecalculator_crted
    siteAgeData[HT_CALC %!=% 1.3 & HT_CALC %!=% 0,
                ':='(AGE_BOR = boredAgeCalculator_Crted(boredAge = AGE_BOR,
                                                        boredHeight = HT_CALC,
                                                        treeHeight = HEIGHT,
                                                        species = SP_SINDEX,
                                                        FIZ = REGION_IC,
                                                        compilationType = compilationType),
                     HT_CALC = 1.3,
                     AGE_ADJUST_TO_BH = "yes")]
    siteAgeData[, ':='(CORR = 0,
                       AGE_TOT = as.numeric(NA),
                       AGE_BH = as.numeric(NA),
                       SI_TREE = as.numeric(NA),
                       AGE_TP = as.numeric(NA))]
    treatdata <- siteAgeData[AGE_BOR %>>% 0 & HEIGHT %>>% 0 & SI_SP %>=% 0, ]
    untreatdata <- siteAgeData[!(uniObs %in% treatdata$uniObs),]
    ## the below is to derive site index based on height, age
    ## (total age is indicated as ageType = 0, breast height age is indicated as ageType = 1)
    treatdata[HT_CALC %==% 0, AGE_TP := 0] # using total age
    treatdata[is.na(AGE_TP), AGE_TP := 1] # using breast height age, assuming bored height is 1.3 if missing
    treatdata_siinf <- SiteTools_HTBoredAge2SI(boredAge = treatdata$AGE_BOR,
                                               height = treatdata$HEIGHT,
                                               species = treatdata$SP_SINDEX,
                                               ICRegion = treatdata$REGION_IC,
                                               ageType = treatdata$AGE_TP,
                                               estType = 1)
    treatdata[, SI_TREE := treatdata_siinf$SI_TREE]
    treatdata[, CURVE_SOURCE := treatdata_siinf$SI_SOURCE]
    treatdata[, CURVE_NAME := treatdata_siinf$CURVE_NAME]
    treatdata[, CORR := SiteTools_Y2BH(species = SP_SINDEX,
                                       ICRegion = REGION_IC,
                                       siteIndex = SI_TREE)]
    treatdata[HT_CALC %==% 0, ':='(AGE_TOT = AGE_BOR)]
    treatdata[HT_CALC %==% 0, ':='(AGE_BH = AGE_TOT - CORR)]
    treatdata[HT_CALC %==% 0 & AGE_BH %<<% 0, ':='(AGE_BH = 0)]
    treatdata[HT_CALC %!=% 0, ':='(AGE_BH = AGE_BOR)]
    treatdata[HT_CALC %!=% 0, ':='(AGE_TOT = CORR + AGE_BH)]
    siteAgeData <- rbindlist(list(treatdata,
                                  untreatdata),
                             fill = TRUE)
    rm(treatdata, untreatdata)
    siteAgeData[, HEIGHT := HT_OLD]
    siteAgeData[, ':='(HT_OLD = NULL)]
    siteAgeData[SUIT_HT == "N" | SUIT_TR == "N", SI_TREE := as.numeric(NA)]
    siteAgeData <- siteAgeData[order(uniObs),.(CLSTR_ID,
                                               PLOT, TREE_NO, SPECIES, SP_SINDEX,
                                               SUIT_TR, SUIT_HT, SUIT_SI, FIZ, BEC_ZONE, REGION_IC,
                                               TH_TREE, TP_TREE, RA_TREE, SP0,
                                               CR_CL, HEIGHT, HT_TOTAL_SOURCE, BNG_DIAM, BARK_THK, BARK_PCT,
                                               GROW_5YR, GROW_10YR, GROW_20YR, RATE_5, RATE_10, RATE_20,
                                               BORED_HT, BORE_AGE_LAB, LAB_AGE_EDIT, BORE_AGE_FLD, TOTAL_AG, PHYS_AGE,
                                               PRO_LEN, PRO_RING, BORED_AGE_FINAL, BORED_AGE_SOURCE, AGE_ADJUST_TO_BH,
                                               SI_SP, AGE_TP, SI_TREE, CURVE_SOURCE, CURVE_NAME,
                                               AGE_BH, AGE_TO_BH = CORR, AGE_TOT, AGE_MEASURE_CODE,
                                               RESIDUAL, BORED_AGE_FLAG)]
    return(siteAgeData)
  })

