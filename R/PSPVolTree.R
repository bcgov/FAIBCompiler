#' Calcualte tree volume-PSP specific
#'
#' @description This function use BEC(or FIZ) and species-specific taper equation to
#'              calculate tree volume. Before calculation, the function adjusts height
#'              for broken top trees: scenario 1 (D scenario): availability of DBH, DIB at broken height;
#'              scenario 2 (H scenario): availability of projected tree height in the field.
#'              This functions also assigns the volume multiplier adjustment.
#'
#' @param treeData data.table, Tree data
#'
#' @param equation character, Specifies which taper equation form will be used to calculate
#'                                     diameter inside bark for a given height.
#'                                     Must be either KBEC or KFIZ3. If missing, default is KBEC
#' @param stumpHeight numeric, Specifies stump height. If missing, 0.3 m will be used.
#' @param breastHeight numeric, Specifies breast height. 1.3 m will be used when this arguement is missing.
#' @param UTOPDIB numeric, Specifies minimum merchantable inside bark diameter. 10 cm is used as a default.
#' @param bestHeightModels data.table, External table that contains the best height/DBH model and
#'                                     coefficients by becsubzone and species.
#' @param HTBTOPModel character, Specifies whether the height estimate for broken top trees either
#'                                from \code{taper} or from \code{height}.
#' @return A data table
#'
#' @importFrom data.table ':='
#' @importFrom dplyr left_join
#' @importFrom fpCompare %>>% %<<% %==% %!=% %<=% %>=%
#' @importFrom FAIBBase heightEstimateForBTOP_D heightEstimateForBTOP_H treeVolCalculator
#'
#'
#' @export
#' @docType methods
#' @rdname PSPVolTree
#'
#' @author Yong Luo
PSPVolTree<- function(treeData, equation, logMinLength,
                      stumpHeight, breastHeight, UTOPDIB,
                      HTEstimateMethod,
                      htDBHCoeff){
  #### please note that this function reorder the process from the original macro
  ## before apply taper equation to calculate tree volume,
  ## 1. check species, FIZ and BEC zone
  equation <- toupper(equation)
  beczones <- c('AT','BWBS','CDF','CWH','ESSF','ICH','IDF','MH',
                'MS','PP','SBPS','SBS','SWB','BG','BAFA','CMA','IMA')
  if(equation == "FIZ"){
    treeData[, ':='(ADJ_ID = " ",
                    VOL_MULT = 1, BEC = BGC_ZONE,
                    BGC_VAR = gsub(" ", "", BGC_VAR))]
    treeData[BGC_ZONE %in% c("BAFA", "CMA", "IMA"), BEC := "AT"]
    treeData[BGC_ZONE %in% c("BG"), BEC := "PP"]
  } else if (equation == "KBEC") {
    treeData[BGC_ZONE %in% beczones, ':='(ADJ_ID = " ",
                                          VOL_MULT = 1,
                                          BEC = BGC_ZONE)]
    treeData[BGC_ZONE %in% c("BAFA", "CMA", "IMA"), BEC := "AT"]
    treeData[BGC_ZONE %in% c("BG"), BEC := "PP"]
    ##### please check the adjustment_id_2011 macro
    ### it just simply assign adj_id as empty space
    ADJIDData <- treeData[ADJ_ID == " ",]
    ADJIDnaData <- treeData[ADJ_ID != " ",]
    if(nrow(ADJIDnaData) > 0){
      dcy_v3x <- dcy_v3lookuptables(type = "dcy_v3x")
      ADJIDnaData <- dplyr::left_join(ADJIDnaData, dcyV3xTable, by = c("ADJ_ID", "SP0"))
    }
    treeData <- rbind(ADJIDData, ADJIDnaData)
    rm(ADJIDData, ADJIDnaData)
  } else {
    stop("equation is not specified correctly. must be one of KFIZ or KBEC")
  }
  treeData[VOL_MULT > 1.2 | VOL_MULT < 0.9, VOL_MULT := 1]

  ## 2. estimate tree height for broken top trees
  ## as suggested by Rene, as the quality of projected height is bad,
  ## even though we have projected height for broken top trees,
  ## we should not use it
  treeData[, HT_PROJ := NA] ## force projected height as NA
  treeData[HT_PROJ > 0 & BROKEN_TOP_IND == "Y",
           BTOP := "H"] ## projected height for broken top tree
  treeData[BROKEN_TOP_IND == "Y" & is.na(BTOP),
           BTOP := "D"] # diameter at broken top
  treeData[is.na(BTOP), HT := round(HEIGHT, 1)] ## HT is total height
  treeData[BTOP == "H",
           ':='(HT = round(FAIBBase::heightEstimateForBTOP_H(HT_PROJ), 1),
                HEIGHT_SOURCE = "Projected_in_field")]
  ## should add a note here
  treeData[BTOP == "H" & HT_BTOP > HT,
           HT_BTOP := round(HT, 1)]
  treeData[BTOP == "D", DOB_BTOP := round(DIAM_BTP, 2)]
  treeData[DOB_BTOP %>=% DBH, DOB_BTOP := round(DOB_BTOP*0.9, 2)]

  treeData[!is.na(BTOP) & HT_BTOP %==% 1.3, HT_BTOP := 1.4]
  treeData[!is.na(DOB_BTOP), DIB_BTOP := round(DOB_BTOP/1.07, 2)]
  treeData[, DOB_BTOP := NULL]

  treeData[DIB_BTOP %<<% 1.1 & DIB_BTOP %>>% 0, DIB_BTOP := 1.1]
  if (HTEstimateMethod == "taper"){
    treeData[BTOP == "D", HT := round(FAIBBase::heightEstimateForBTOP_D(heightBTOP = HT_BTOP,
                                                                        taperEquationForm = equation,
                                                                        DIBBTOP = DIB_BTOP,
                                                                        DBH = DBH,
                                                                        FIZorBEC = BEC,
                                                                        species = SP0,
                                                                        volMultiplier = VOL_MULT),
                                      1)]
    treeData[BTOP == "D" & is.na(HT), BTOP_ESTIMATE_TYPE := 0] # D TREES THAT FAILED TO ESTIMATE TREE HEIGHT
    treeData[BTOP == "D" & !is.na(HT), BTOP_ESTIMATE_TYPE := 1] # D TREES THAT SUCCESS TO ESTIMATE TREE HEIGHT
    treeData[BTOP == "H" & DIB_BTOP > 0, BTOP_ESTIMATE_TYPE := 2] # h TREES THAT HAVE DIAMETER AT BROKEN HEIGHT INFORMATION
    treeData[BTOP == "H" & HT_BTOP > 0, BTOP_ESTIMATE_TYPE := 3] # H TREES THAT HAVE PROJECTED HEIGHT INFORMATION
  } else if (HTEstimateMethod == "bestMEM"){
    treeData[BTOP %in% c("D"),
             HT := round(heightEstimate_byHeightModel(beczone = BGC_ZONE,
                                                      subzone = BGC_SBZN,
                                                      species = SPECIES,
                                                      DBH = DBH,
                                                      heightModels = htDBHCoeff))]

    treeData[BTOP == "D" & is.na(HT), BTOP_ESTIMATE_TYPE := 0] # D TREES THAT FAILED TO ESTIMATE TREE HEIGHT
    treeData[BTOP == "D" & !is.na(HT), BTOP_ESTIMATE_TYPE := 1] # D TREES THAT SUCCESS TO ESTIMATE TREE HEIGHT
    treeData[BTOP == "H" & DIB_BTOP > 0, BTOP_ESTIMATE_TYPE := 2] # h TREES THAT HAVE DIAMETER AT BROKEN HEIGHT INFORMATION
    treeData[BTOP == "H" & HT_BTOP > 0, BTOP_ESTIMATE_TYPE := 3] # H TREES THAT HAVE PROJECTED HEIGHT INFORMATION
    treeData[HT < HEIGHT, ':='(HT = HEIGHT,
                               BTOP_ESTIMATE_TYPE = 1)]
    treeData[is.na(HT), ':='(HT = HEIGHT,
                             BTOP_ESTIMATE_TYPE = 1)]
  } else {
    stop("HTEstimateMethod is not correctly specified.")
  }


  set(treeData, , c(paste0("LOG_L_", 0:9)), NA)
  treeData <- treeData[BEC == "CMA",
                       BEC := "AT"]
  treeVolumes <- FAIBBase::treeVolCalculator(FIZorBEC = treeData$BEC,
                                             species = treeData$SP0,
                                             height = treeData$HT,
                                             DBH = treeData$DBH,
                                             taperEquationForm = equation,
                                             volMultiplier = as.numeric(treeData$VOL_MULT),
                                             stumpHeight = stumpHeight,
                                             breastHeight = breastHeight,
                                             UTOPDIB = UTOPDIB,
                                             BTOPEstimateType = as.integer(treeData$BTOP_ESTIMATE_TYPE),
                                             BTOPHeight = treeData$HT_BTOP,
                                             BTOPDIB = treeData$DIB_BTOP,
                                             logLengthMatrix = treeData[, paste("LOG_L_", 0:9, sep = ""),
                                                                        with = FALSE],
                                             logMinLength = logMinLength)



  treeData <- cbind(treeData, treeVolumes)
  set(treeData, , c(paste0("LOG_L_", 0:9),
                    paste0("LOG_V_", 0:9),
                    paste0("LOG_VM_", 0:9),
                    paste0("LOG_D_", 0:9)),
      NULL)
  set(treeVolumes, , c(paste0("LOG_V_", 0:9),
                       paste0("LOG_VM_", 0:9),
                       paste0("LOG_D_", 0:9)),
      NULL)
  volumenames <- names(treeVolumes)
  rm(treeVolumes)
  NAvolumesForHenhancedtrees <- volumenames[!(volumenames %in% c("HT_STUMP", "DIB_STUMP", "VOL_STUMP", "HT_BH",
                                                                 "DIB_BH", "HT_UTOP", "DIB_UTOP", "VOL_WSV",
                                                                 "VOL_BELOW_UTOP", "VOL_ABOVE_UTOP", "VOL_BELOW_BTOP",
                                                                 "VOL_ABOVE_BTOP"))]
  # treeData[MEAS_INTENSE == "H-ENHANCED",
  #          c(NAvolumesForHenhancedtrees) := NA]
  treeData[,':='(VOL_TOP = VOL_ABOVE_UTOP,
                 VOL_MER = VOL_BELOW_UTOP,
                 VOL_BKT = 0,
                 H_MERCH = HT_UTOP)]
  ## the break is above the utop
  treeData[!is.na(BTOP) &
             VOL_BELOW_UTOP %<<% VOL_BELOW_BTOP,
           ':='(VOL_BKT = VOL_ABOVE_BTOP,
                VOL_TOP = VOL_ABOVE_UTOP-VOL_ABOVE_BTOP)]
  ## the break is below the utop
  treeData[!is.na(BTOP) &
             VOL_BELOW_UTOP %>=% VOL_BELOW_BTOP,
           ':='(VOL_MER = VOL_BELOW_BTOP,
                VOL_BKT = VOL_ABOVE_BTOP,
                VOL_TOP = 0,
                H_MERCH = HT_BTOP)]
  ## for whole stem volume adjustment for btop trees
  treeData[!is.na(BTOP),
           VOL_WSV := VOL_BELOW_BTOP + VOL_STUMP]
  return(treeData)
}
