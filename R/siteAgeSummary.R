#' Summarize site age data by cluster and cluster/species-VRI specific
#'
#'
#' @description This function takes compiled site age tree data, an output of \code{\link{siteAgeCompiler}}, to
#'              derive mean age and height results. The compiled data must have breast height age,
#'              total age, and site index. This function is equivalent to \code{mean_htl.sas}.
#'
#' @param cpldSiteAgeData data.table, Compiled site age tree data, an output of \code{\link{siteAgeCompiler}}.
#'
#' @return Two data tables: cl_ah is the age/height summary at cluster level and spc_ah is the age/height summary
#'         at cluster and species level
#'
#'
#' @importFrom data.table ':='
#' @importFrom FAIBBase merge_dupUpdate
#'
#'
#' @export
#' @docType methods
#' @rdname siteAgeSummary
#'
#' @author Yong Luo
#'
setGeneric("siteAgeSummary",
           function(cpldSiteAgeData) {
             standardGeneric("siteAgeSummary")
           })

#' @rdname siteAgeSummary
setMethod(
  "siteAgeSummary",
  signature = c(cpldSiteAgeData = "data.table"),
  definition = function(cpldSiteAgeData){
    bark <- cpldSiteAgeData[,.(CLSTR_ID, PLOT, TREE_NO, SPECIES,
                               BARK_THK = as.numeric(BARK_THK),
                               BARK_PCT = as.numeric(BARK_PCT))]

    bark_sum <- bark[,.(H_TREES = length(TREE_NO),
                        BARK_THK = mean(BARK_THK, na.rm = TRUE),
                        BARK_PCT = round(mean(BARK_PCT, na.rm = TRUE), 2)),
                     by = c("CLSTR_ID", "SPECIES")]
    bark_sum[BARK_THK %in% c(NaN), BARK_THK := NA]
    bark_sum[BARK_PCT %in% c(NaN), BARK_PCT := NA]
    ah_ra <- cpldSiteAgeData[RA_TREE == "R",.(CLSTR_ID, AGE_RND = AGE_TOT,
                                              HT_RND = HEIGHT, SP_RND = SPECIES)]
    ah_ra <- unique(ah_ra, by = "CLSTR_ID")

    ah_ta <- cpldSiteAgeData[TP_TREE == "T" & PLOT == "I", .(CLSTR_ID, AGE_TOP = AGE_TOT,
                                                             HT_TOP = HEIGHT, SP_TOP = SPECIES)]
    ah_ta <- unique(ah_ta, by = "CLSTR_ID")

    ## as suggested by Rene as per communication on 20211029
    ## the site trees without si suitability
    ## if a site tree suitable for age and height, this is suitable for site index
    cpldSiteAgeData[is.na(SUIT_SI) & SUIT_TR == "Y" &
                      SUIT_HT == "Y", SUIT_SI := "Y"]

    cpldSiteAgeData[is.na(SUIT_SI), SUIT_SI := "N"]

    tlsxo <- cpldSiteAgeData[TH_TREE %in% c("T", "S", "L", "X", "O")][, ':='(N_height = 1, N_age_bh = 1)]
    tlsxo[SUIT_TR == "N", ':='(AGE_BH = as.numeric(NA),
                               AGE_TOT = as.numeric(NA))]
    tlsxo[SUIT_HT == "N", HEIGHT := as.numeric(NA)]
    tlsxo[SUIT_SI == "N", SI_TREE := as.numeric(NA)]
    tlsxo[is.na(HEIGHT), N_height := 0]
    tlsxo[is.na(AGE_BH), N_age_bh := 0]


    tls <- tlsxo[TH_TREE %in% c("T", "S", "L"),]
    ahs_tls <- rbind(tls[,.(SPECIES = as.character(NA),
                            AGEB_TLS = mean(AGE_BH, na.rm = TRUE),
                            AGET_TLS = mean(AGE_TOT, na.rm = TRUE),
                            HT_TLS = mean(HEIGHT, na.rm = TRUE),
                            SI_M_TLS = mean(SI_TREE, na.rm = TRUE),
                            N_HT_TLS = sum(N_height),
                            N_AG_TLS = sum(N_age_bh)),
                         by = "CLSTR_ID"],
                     tls[,.(AGEB_TLS = mean(AGE_BH, na.rm = TRUE),
                            AGET_TLS = mean(AGE_TOT, na.rm = TRUE),
                            HT_TLS = mean(HEIGHT, na.rm = TRUE),
                            SI_M_TLS = mean(SI_TREE, na.rm = TRUE),
                            N_HT_TLS = sum(N_height),
                            N_AG_TLS = sum(N_age_bh)),
                         by = c("CLSTR_ID", "SPECIES")])


    ahs_txo <- rbind(tlsxo[,.(FIZ = unique(FIZ),
                              SPECIES = as.character(NA),
                              AGEB_TXO = mean(AGE_BH, na.rm = TRUE),
                              AGET_TXO = mean(AGE_TOT, na.rm = TRUE),
                              HT_TXO = mean(HEIGHT, na.rm = TRUE),
                              SI_M_TXO = mean(SI_TREE, na.rm = TRUE),
                              N_HT_TXO = sum(N_height),
                              N_AG_TXO = sum(N_age_bh)),
                           by = "CLSTR_ID"],
                     tlsxo[,.(FIZ = unique(FIZ),
                              AGEB_TXO = mean(AGE_BH, na.rm = TRUE),
                              AGET_TXO = mean(AGE_TOT, na.rm = TRUE),
                              HT_TXO = mean(HEIGHT, na.rm = TRUE),
                              SI_M_TXO = mean(SI_TREE, na.rm = TRUE),
                              N_HT_TXO = sum(N_height),
                              N_AG_TXO = sum(N_age_bh)),
                           by = c("CLSTR_ID", "SPECIES")])
    ahs_msp <- FAIBBase::merge_dupUpdate(ahs_txo[!is.na(SPECIES),],
                               ahs_tls[!is.na(SPECIES),],
                               all = TRUE,
                               by = c("CLSTR_ID", "SPECIES"))

    ahs_msp[, ':='(AGEB_TLS = round(AGEB_TLS, 1),
                   AGET_TLS = round(AGET_TLS, 1),
                   HT_TLS = round(HT_TLS, 1),
                   SI_M_TLS = round(SI_M_TLS, 1),
                   AGEB_TXO = round(AGEB_TXO, 1),
                   AGET_TXO = round(AGET_TXO, 1),
                   HT_TXO = round(HT_TXO, 1),
                   SI_M_TXO = round(SI_M_TXO, 1))]
    ahs_msp[, ':='(SP_SINDEX = siteToolsSpeciesConvertor(SPECIES))]
    ahs_msp[AGEB_TLS > 0 & HT_TLS > 0,
            SI_C_TLS := SiteTools_HTBoredAge2SI(boredAge = AGEB_TLS, height = HT_TLS,
                                                species = SP_SINDEX,
                                                ICRegion = FIZ, ageType = 1, estType = 1)]
    ahs_msp[AGEB_TXO > 0 & HT_TXO > 0,
            SI_C_TXO := SiteTools_HTBoredAge2SI(boredAge = AGEB_TXO, height = HT_TXO,
                                                species = SP_SINDEX,
                                                ICRegion = FIZ, ageType = 1, estType = 1)]
    ahs_msp[,':='(SI_C_TLS = round(SI_C_TLS, 1),
                  SI_C_TXO = round(SI_C_TXO, 1))]
    ahs_msp[is.na(N_HT_TLS), N_HT_TLS := 0]
    ahs_msp[is.na(N_HT_TXO), N_HT_TXO := 0]
    ahs_msp[is.na(N_AG_TLS), N_AG_TLS := 0]
    ahs_msp[is.na(N_AG_TXO), N_AG_TXO := 0]

    cl_ah <- FAIBBase::merge_dupUpdate(ah_ra, ah_ta,
                             by = "CLSTR_ID", all = TRUE)
    cl_ah <- FAIBBase::merge_dupUpdate(cl_ah, ahs_txo[is.na(SPECIES)],
                             by = "CLSTR_ID", all = TRUE)
    cl_ah <- FAIBBase::merge_dupUpdate(cl_ah, ahs_tls[is.na(SPECIES)],
                             by = "CLSTR_ID", all = TRUE)
    setnames(cl_ah, c("HT_TXO", "AGET_TXO", "AGEB_TXO"),
             c("HT_M_TXO", "AT_M_TXO", "AB_M_TXO"))
    setnames(cl_ah, c("HT_TLS", "AGET_TLS", "AGEB_TLS"),
             c("HT_M_TLS", "AT_M_TLS", "AB_M_TLS"))

    spc_ah <- FAIBBase::merge_dupUpdate(ahs_msp, bark_sum, by = c("CLSTR_ID", "SPECIES"), all = TRUE)
    return(list(cl_ah = cl_ah, spc_ah = spc_ah))
  })
