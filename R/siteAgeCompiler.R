#' Compile breast age, total age, and site index where possible-VRI specific
#'
#'
#' @description This function takes site age tree data ie., \code{vi_h}, an output of \code{\link{VRIInit_siteTree}} to
#'              compute the breast height age, total age, and site index where possible. This function is
#'              equivalent to site_age.sas. The function heavily depends on site tools program.
#'
#' @param siteAgeData data.table, Site age data with plot header information.
#'                                An output from \code{\link{VRIInit_siteTree}} function.
#' @return A data table and a log file.
#'
#' @importFrom data.table ':='
#' @importFrom dplyr left_join
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#' @importFrom SIndexR SIndexR_SpecRemap
#'
#' @export
#' @docType methods
#' @rdname siteAgeCompiler
#'
#' @author Yong Luo
#'
setGeneric("siteAgeCompiler",
           function(siteAgeData) {
             standardGeneric("siteAgeCompiler")
           })

#' @rdname siteAgeCompiler
setMethod(
  "siteAgeCompiler",
  signature = c(siteAgeData = "data.table"),
  definition = function(siteAgeData){
    displaycol <- c("CLSTR_ID", "PLOT", "TREE_NO")
    siteAgeData <- cbind(data.table(uniObs = 1:nrow(siteAgeData)),
                         siteAgeData)
    siteAgeData[, BARK_TEMP := as.numeric(BARK_THK)]
    siteAgeData[is.na(BARK_THK), BARK_TEMP := BNG_DIAM/20]
    siteAgeData[BARK_THK %>>% 0 & BNG_DIAM %>>% 0, BARK_PCT := 100*((BARK_THK*0.2)/BNG_DIAM)]
    ## CALL annualGrowthRataCalculator function
    siteAgeData[!is.na(BNG_DIAM), ':='(RATE_5 = annualGrowthRateCalculator(boredDiameter = BNG_DIAM,
                                                                           growthIncrement = GROW_5YR,
                                                                           growthYear = 5,
                                                                           barkThickness = BARK_TEMP),
                                       RATE_10 = annualGrowthRateCalculator(boredDiameter = BNG_DIAM,
                                                                            growthIncrement = GROW_10YR,
                                                                            growthYear = 10,
                                                                            barkThickness = BARK_TEMP),
                                       RATE_20 = annualGrowthRateCalculator(boredDiameter = BNG_DIAM,
                                                                            growthIncrement = GROW_20YR,
                                                                            growthYear = 20,
                                                                            barkThickness = BARK_TEMP))]
    siteAgeData[BARK_THK %>>% 0 & BNG_DIAM %>>% 0,
                BARK_PCT := 100*(BARK_THK*0.2)/BNG_DIAM]

    siteAgeData[, SP_SINDEX := siteToolsSpeciesConvertor(species = SPECIES)]

    siteAgeData[, SI_SP := SIndexR::SIndexR_SpecRemap(SP_SINDEX, FIZ)]

    siteAgeData[, HEIGHT := TREE_LEN]
    siteAgeData[, ':='(HT_OLD = HEIGHT)]
    siteAgeData[HEIGHT %<=% 1.3, ':='(HEIGHT = 1.31)]

    ## assign bored age, first part of the age-ind.sas

    ## need check with rene to make sure the order makes sense
    siteAgeData[, HT_CALC := BORED_HT]
    siteAgeData[!(BORE_AGE %in% c(NA, 0)),
                ':='(AGE_BOR = boredAgeCalculator_Bore(officeBoredAge = as.numeric(BORE_AGE)),
                     AGE_BASE = "Bore")]
    siteAgeData[is.na(AGE_BASE) & !(BORAG_FL %in% c(NA, 0)),
                ':='(AGE_BOR = boredAgeCalculator_Bore(fieldBoredAge = as.numeric(BORAG_FL)),
                     AGE_BASE = "Bore")]
    siteAgeData[is.na(AGE_BASE) & !(TOTAL_AG %in% c(NA, 0)) & is.na(AGE_BASE),
                ':='(AGE_BOR = boredAgeCalculator_Total(as.numeric(TOTAL_AG)),
                     AGE_BASE = "Total")]
    siteAgeData[is.na(AGE_BASE) & !(PHYS_AGE %in% c(NA, 0)),
                ':='(AGE_BOR = boredAgeCalculator_Phys(as.numeric(PHYS_AGE)),
                     AGE_BASE = "Phys")]
    siteAgeData[PRO_LEN %>>% 0 & PRO_RING %>>% 0,
                ':='(AGE_BOR = boredAgeCalculator_Prorated(ringLength_prorated = PRO_LEN,
                                                           ringCount_prorated = PRO_RING,
                                                           boreDiameter = BNG_DIAM,
                                                           barkThickness = BARK_TEMP),
                     AGE_BASE = "Pro")]
    ## call boredagecalculator_crted
    siteAgeData[HT_CALC %!=% 1.3 & HT_CALC %!=% 0,
                ':='(AGE_BOR = boredAgeCalculator_Crted(boredAge = AGE_BOR,
                                                        boredHeight = HT_CALC,
                                                        treeHeight = HEIGHT,
                                                        species = SP_SINDEX,
                                                        FIZ = FIZ),
                     HT_CALC = 1.3,
                     AGE_BASE = "Bh_cr")]

    siteAgeData[, ':='(CORR = 0,
                       AGE_TOT = as.numeric(NA),
                       AGE_BH = as.numeric(NA),
                       SI_TREE = as.numeric(NA))]
    treatdata <- siteAgeData[AGE_BOR %>>% 0 & HEIGHT %>>% 0 & SI_SP %>=% 0, ]
    untreatdata <- siteAgeData[!(uniObs %in% treatdata$uniObs),]
    treatdata[, AGE_TP := 1]
    treatdata[, SI_TREE := SiteTools_HTBoredAge2SI(boredAge = AGE_BOR, height = HEIGHT,
                                                   species = SP_SINDEX, ICRegion = FIZ,
                                                   ageType = AGE_TP, estType = 1)]
    treatdata[, CORR := SiteTools_Y2BH(species = SP_SINDEX, ICRegion = FIZ,
                                       siteIndex = SI_TREE)]
    treatdata[HT_CALC %==% 0, ':='(AGE_TOT = AGE_BOR)]
    treatdata[HT_CALC %==% 0, ':='(AGE_BH = AGE_TOT - CORR)]
    treatdata[HT_CALC %==% 0 & AGE_BH %<<% 0, ':='(AGE_BH = 0)]
    treatdata[HT_CALC %!=% 0, ':='(AGE_BH = AGE_BOR)]
    treatdata[HT_CALC %!=% 0, ':='(AGE_TOT = CORR + AGE_BH)]
    siteAgeData <- rbindlist(list(treatdata[, ':='(AGE_TP = NULL)], untreatdata))
    rm(treatdata, untreatdata)
    siteAgeData[, HEIGHT := HT_OLD]
    siteAgeData[, ':='(HT_OLD = NULL)]
    siteAgeData[SUIT_HT == "N" | SUIT_TR == "N", SI_TREE := as.numeric(NA)]
    siteAgeData <- siteAgeData[order(uniObs),.(CLSTR_ID, PLOT, TREE_NO, SPECIES,
                                               SUIT_TR, SUIT_HT, FIZ,
                                               TH_TREE, TP_TREE, RA_TREE,
                                               AGE_BASE, SP0,
                                               GROW_5YR, GROW_10YR, GROW_20YR, AGE_CORR, TOTAL_AG, PHYS_AGE,
                                               CR_CL, TREE_LEN, HEIGHT, BNG_DIAM, BARK_THK, PRO_LEN,
                                               PRO_RING, BORE_AGE, BORED_HT, SI_SP, AGE_BH, AGE_TOT, SI_TREE,
                                               BARK_PCT, RATE_5, RATE_10, RATE_20, SP_SINDEX)]
    return(siteAgeData)
  })

