#' Load the data from ASCII files
#'
#' @description This function is to read the data from ASCII files (in txt format).
#'
#' @param txtLocation character, Specifies the location of ASCII files.
#' @param saveThem logical, Specifies whether the loaded data should be saved or returned.
#'                 The default value is FALSE, which means the function will not save files
#'                 for you.
#' @param savePath character, Specifies the path that directs to the VRI original data soruce, i.e.,
#'                                  \code{//Mayhem/GIS_TIB/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/vri_sa}.
#'
#' @return no files
#' @importFrom data.table data.table ':='
#' @importFrom dplyr '%>%'
#' @importFrom openxlsx write.xlsx
#'
#'
#' @rdname loadASCII
#' @examples
#' \dontrun{
#' loadASCII(txtLocation = "Q:/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/vri_raw",
#'           saveThem = TRUE,
#'           savePath = "F:/vricompilertest/OracleTests")
#'
#' }
#' @author Yong Luo
loadASCII <- function(txtLocation, saveThem = FALSE, savePath){
  allText <- scanAllASCII(txtLocation)
  card011 <- allText[REC_ID == "011",
                     .(REC_ID, PAGE_NO, SEQ, MEAS_DT = measurementDate,
                       CLSTR_ID,
                       PROJ_ID = substr(LINE, 10, 13),
                       SAMP_NO = substr(LINE, 14, 17),
                       PL = substr(LINE, 18, 18),
                       TYPE_CD = substr(LINE, 19, 21),
                       MEAS_DTC = substr(LINE, 22, 30),
                       CREW1 = substr(LINE, 31, 34),
                       CREW2 = substr(LINE, 35, 38),
                       CREW3 = substr(LINE, 39, 42),
                       CREW4 = substr(LINE, 43, 46),
                       NO_PAGES = as.numeric(substr(LINE, 47,  48)),
                       POLYGON = substr(LINE, 49, 62),
                       REF_TREE = as.numeric(substr(LINE, 63, 68)),
                       INTEG = substr(LINE, 69, 69),
                       NORTH = substr(LINE, 70, 70),
                       EAST = substr(LINE, 71, 71),
                       SOUTH = substr(LINE, 72, 72),
                       WEST = substr(LINE, 73, 73),
                       RANGE = substr(LINE, 74, 74),
                       ECO = substr(LINE, 75, 75),
                       CWD = substr(LINE, 76, 76),
                       TREES = substr(LINE, 77, 77),
                       ECOLGY = substr(LINE, 78, 78),
                       NO_PHOTO = as.numeric(substr(LINE, 79, 80)),
                       S1 = substr(LINE, 81, 81),
                       S2 = substr(LINE, 82, 82),
                       S3 = substr(LINE, 83, 83),
                       S4 = substr(LINE, 84, 84),
                       S5 = substr(LINE, 85, 85),
                       S6 = substr(LINE, 86, 86),
                       S7 = substr(LINE, 87, 87),
                       S8 = substr(LINE, 88, 88),
                       S9 = substr(LINE, 89, 89),
                       S10 = substr(LINE, 90, 90))]

  card012 <- allText[REC_ID == "012",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       REC_TYP = substr(LINE, 10, 19),
                       RESP_FD = substr(LINE, 20, 49),
                       CHK_FD = substr(LINE, 50, 59),
                       FLD_DTC = substr(LINE, 80, 88),
                       CHK_OFF = substr(LINE, 89, 118),
                       OFF_DTC = substr(LINE, 119, 127))]
  card012[, ':='(FLD_DT = as.Date(FLD_DTC, '%Y%b%d'),
                 OFF_DT = as.Date(OFF_DTC, '%Y%b%d'))]

  card013 <- allText[REC_ID == "013",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       PHOT_TYP = substr(LINE, 10, 23),
                       PHOT_IND = substr(LINE, 24, 25),
                       NOTES1A = substr(LINE, 25, 152),
                       NOTES1B = substr(LINE, 153, 280))]
  card013[, PHOT_IND := trimws(PHOT_IND, which = "both")]
  card013[, PHOT_IND := substr(PHOT_IND, 1, 1)]


  card021 <- allText[REC_ID == "021",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       TP_GPSID = substr(LINE, 10, 17),
                       TP_UTM = as.numeric(substr(LINE, 18, 19)),
                       TP_NRTH = as.numeric(substr(LINE, 20, 26)),
                       TP_EAST = as.numeric(substr(LINE, 27, 32)),
                       TP_ELEV = as.numeric(substr(LINE, 33, 36)),
                       AP_DSC = substr(LINE, 37, 56),
                       AP_GPSID = substr(LINE, 57,64),
                       AP_NRTH = as.numeric(substr(LINE, 65, 71)),
                       AP_EAST = as.numeric(substr(LINE, 72, 77)),
                       AP_ELEV = as.numeric(substr(LINE, 78, 81)),
                       TP_AZ_IP = as.numeric(substr(LINE, 82, 84)),
                       TP_DT_IP = as.numeric(substr(LINE, 85, 88)),
                       DECLIN = as.numeric(substr(LINE, 89, 91))/10, #DECLIN     3.1
                       TP_AZ_GP = as.numeric(substr(LINE, 92, 94)),
                       TP_DT_GP = as.numeric(substr(LINE, 95, 98))/100,#   4.2
                       TIE_TRNO = as.numeric(substr(LINE, 99, 104)),
                       RP_AZ_PN = as.numeric(substr(LINE, 105, 107)),# rp_az_pn    3.
                       RP_DT_PN = as.numeric(substr(LINE, 108, 111))/100,# rp_dt_pn   4.2
                       NO_PAGES = as.numeric(substr(LINE, 112, 113)))]
  card022 <- allText[REC_ID == "022",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       LOCATN = substr(LINE, 10, 31),
                       MAP_NO = substr(LINE, 32, 38),
                       POLYGON = substr(LINE, 39, 43),
                       FLT_LINE = substr(LINE, 44, 54),
                       PHOTO_NO = substr(LINE, 55, 57))]

  card023 <- allText[REC_ID == "023",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       PT_TREE = substr(LINE, 10, 25),
                       SPECIES = substr(LINE, 26, 28),
                       DBH = as.numeric(substr(LINE, 29, 32))/10,
                       AZ_TREE = as.numeric(substr(LINE, 33, 35)),
                       DISTANCE = as.numeric(substr(LINE, 36, 39))/100)]


  card031 <- allText[REC_ID == "031",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       IP_GPSID = substr(LINE, 10, 17),
                       IP_UTM = as.numeric(substr(LINE, 18, 19)),
                       IP_NRTH = as.numeric(substr(LINE, 20, 26)),# ip_nrth    7.
                       IP_EAST = as.numeric(substr(LINE, 27, 32)),
                       IP_ELEV = as.numeric(substr(LINE, 33, 36)),# ip_elev   4.
                       IP_AZ_PN = as.numeric(substr(LINE, 37, 39)),# ip_az_pn    3.
                       IP_DT_PN = as.numeric(substr(LINE, 40, 43))/100,# ip_dt_pn   4.2
                       IP_AZ_GP = as.numeric(substr(LINE, 44, 46)),# ip_az_gp     3.
                       IP_DT_GP = as.numeric(substr(LINE, 47, 50))/100,# ip_dt_gp    4.2
                       PLT_SPC = as.numeric(substr(LINE, 51, 54))/10,# PLT_SPC    4.1
                       NO_PAGE = as.numeric(substr(LINE, 55, 56)))]

  card041 <- allText[REC_ID == "041",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       PLOT = substr(LINE, 10, 1),
                       MEAS_DTC = substr(LINE, 11, 19),
                       CREW1 = substr(LINE, 20, 23), # CREW1   $4.
                       CREW2 = substr(LINE, 24, 27), # CREW2   $4.
                       TRANSECT = substr(LINE, 28, 28),# TRANSECT $1.
                       NO_PAGES = as.numeric(substr(LINE, 29, 30)), # NO_PAGES  2.
                       AZIMUTH = as.numeric(substr(LINE, 31, 33)), # AZIMUTH   3.
                       OBS_LLEN = as.numeric(substr(LINE, 34, 36))/10,# OBS_LLEN  3.1
                       TOT_LLEN = as.numeric(substr(LINE, 37, 39))/10, # TOT_LLEN  3.1
                       UTIL_X1 = substr(LINE, 40, 40), # UTIL_X1  $1.
                       UTIL_X2 = substr(LINE, 41, 41), # UTIL_X2  $1.
                       UTIL_X3 = substr(LINE, 42, 42), # UTIL_X3  $1.
                       UTIL_X4 = substr(LINE, 43, 43), # UTIL_X4  $1.
                       F_FULL = substr(LINE, 44, 44), # f_full   $1.
                       F_HALF = substr(LINE, 45, 45), # f_half   $1.
                       F_QRTR = substr(LINE, 46, 46), # f_qrtr   $1.
                       GRAM_Q = substr(LINE, 47, 47), # GRAM_Q   $1.
                       FORBS_Q = substr(LINE, 48, 48), # FORBS_Q  $1.
                       MS_GRAM = as.numeric(substr(LINE, 49, 53))/10, # MS_GRAM   5.1
                       MS_FORB = as.numeric(substr(LINE, 54, 58))/10)]
  card041_temp <- data.table::copy(card041)
  card041_temp[,':='(REC_ID = NULL, PAGE_NO = NULL, SEQ = NULL,
                     NO_PAGES = NULL)]
#   1)	Range Measurement [ISMC_RANGE_MEASUREMENT] – CLSTR_ID, F_FULL, F_HALF, F_QRTR, GRAM_Q, FORBS_Q, MS_GRAM, MS_FORB
# 2)	Forage Transect [ISMC_VRI_TRANSECT] – CLSTR_ID, TRANSECT, AZIMUTH, OBS_LLEN, TOT_LLEN (there should be 2 transects)
# 3)	Forage Plot [ISMC_RANGE_MICRO_PLOT] – CLSTR_ID, TRANSECT, PLOT, UTIL_X, (there should be 2 plots per transect)

  write.xlsx(card041_temp[,.(CLSTR_ID, F_FULL, F_HALF, F_QRTR,
                             GRAM_Q, FORBS_Q, MS_GRAM, MS_FORB)],
             file.path(savePath,
                       "card_forage_range_measurement.xlsx"),
             overwrite = TRUE)
  write.xlsx(card041_temp[,.(CLSTR_ID, TRANSECT, PLOT,
                             UTIL_X1, UTIL_X2,
                             UTIL_X3, UTIL_X4)],
             file.path(savePath,
                       "card_forage_range_micro_plot.xlsx"),
             overwrite = TRUE)
  card042 <- allText[REC_ID == "042",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       TRANSECT = substr(LINE, 10, 10),#   $1.
                       ITEM_NO = substr(LINE, 11, 12),#    $2.
                       LAYER = substr(LINE, 13, 14),#      $2.
                       SPECLONG = substr(LINE, 15, 22),#    $7.
                       PHENOLOG = substr(LINE, 23, 24),#   $2.
                       COV1 = as.numeric(substr(LINE, 25, 27))/10, #       3.1
                       COV2 = as.numeric(substr(LINE, 28, 30))/10, #       3.1
                       COV3 = as.numeric(substr(LINE, 31, 33))/10, #       3.1
                       COV4 = as.numeric(substr(LINE, 34, 36))/10, #       3.1
                       COV5 = as.numeric(substr(LINE, 37, 39))/10, #       3.1
                       COV6 = as.numeric(substr(LINE, 40, 42))/10, #       3.1
                       COV7 = as.numeric(substr(LINE, 43, 45))/10, #       3.1
                       COV8 = as.numeric(substr(LINE, 46, 48))/10, #       3.1
                       COV9 = as.numeric(substr(LINE, 49, 51))/10, #       3.1
                       COV10 = as.numeric(substr(LINE, 52, 54))/10, #      3.1
                       COV11 = as.numeric(substr(LINE, 55, 57))/10, #      3.1
                       COV12 = as.numeric(substr(LINE, 58, 60))/10, #      3.1
                       S1 = substr(LINE, 61, 61),#         $1.
                       S2 = substr(LINE, 62, 62), #         $1.
                       S3 = substr(LINE, 63, 63))]


  card061 <- allText[REC_ID == "061",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       PLOT = substr(LINE, 10, 10),#    $1.
                       MEAS_DTC = substr(LINE, 11, 19),#  $9.
                       CREW1 = substr(LINE, 20, 23),#    $4.
                       CREW2 = substr(LINE, 24, 27),#    $4.
                       TRANSECT = substr(LINE, 28, 28),#  $1.
                       NO_PAGES = as.numeric(substr(LINE, 29, 30)),#  2.
                       AZIMUTH = as.numeric(substr(LINE, 31, 33)),#   3.
                       OBS_LLEN = as.numeric(substr(LINE, 34, 36))/10,#  3.1
                       TOT_LLEN = as.numeric(substr(LINE, 37, 39))/10)]
  card062 <- allText[REC_ID == "062",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       TRANSECT = substr(LINE, 10, 10), #   $1.
                       ITEM_NO = substr(LINE, 11, 12),#    $2.
                       SPECIES = substr(LINE, 13, 15),#    $3.
                       DIAMETER = as.numeric(substr(LINE, 16, 19))/10,#   4.1
                       LENGTH = as.numeric(substr(LINE, 20, 22))/10,#     3.1
                       PCT_CLSX = substr(LINE, 23, 24),#   $2.
                       CLS_OTHX = substr(LINE, 25, 25),#    $1.
                       ANGLE = as.numeric(substr(LINE, 26, 27)), #      2.
                       MERCH = substr(LINE, 28, 28),#      $1.
                       REMOVE = substr(LINE, 29, 29),#     $1.
                       DEC_CLS = substr(LINE, 30, 30),#    $1.
                       S1 = substr(LINE, 31, 31),#         $1.
                       S2 = substr(LINE, 32, 32),#         $1.
                       S3 = substr(LINE, 33, 33))]

  card063 <- allText[REC_ID == "063",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       TRANSECT = substr(LINE, 10, 10),#   $1.
                       ITEM_NO = substr(LINE, 11, 12),#    $2.
                       SPECIES = substr(LINE, 13, 15),#    $3.
                       ALENGTH = as.numeric(substr(LINE, 16, 18)),#    3.
                       ADEPTH = as.numeric(substr(LINE, 19, 21)),#     3.
                       LENGTH = as.numeric(substr(LINE, 22, 24))/10,#     3.1
                       PCT_CLSX = substr(LINE, 25, 26),#   $2.
                       CLS_OTHX = substr(LINE, 27, 27),#    $1.
                       MERCH = substr(LINE, 28, 28),#      $1.
                       REMOVE = substr(LINE, 29, 29),#     $1.
                       DEC_CLS = substr(LINE, 30, 30),#    $1.
                       S4 = substr(LINE, 31, 31))]
  # CLSTR_ID "03391-0035-MR1" in Yong's file has an invalid date while Rene's has "2002-07-21"

  cardb <- allText[REC_ID == "081",
                   .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                     PLOT = substr(LINE, 10, 10),#       $1.
                     MEAS_DTC = as.Date(substr(LINE, 11, 19), '%Y%b%d'), #   $9.
                     V_BAF = as.numeric(substr(LINE, 20, 23))/100,#      4.2
                     V_FULL = substr(LINE, 24, 24),#     $1.
                     V_HALF = substr(LINE, 25, 25),#     $1.
                     V_QRTR = substr(LINE, 26, 26),#     $1.
                     V_BDRY = substr(LINE, 27, 27),#     $1.
                     V_SPLT = substr(LINE, 28, 28),#     $1.
                     PRF = as.numeric(substr(LINE, 29, 33))/10000,#        5.4
                     DIOPTER = as.numeric(substr(LINE, 34, 38))/1000,#    5.3
                     AZ_BDRY = as.numeric(substr(LINE, 39, 41)),#    3.0

                     F_RAD = as.numeric(substr(LINE, 42, 45))/100,#      4.2
                     F_FULL = substr(LINE, 46, 46),#     $1.
                     F_HALF = substr(LINE, 47, 47),#     $1.
                     F_QRTR = substr(LINE, 48, 48),#     $1.
                     F_BDRY = substr(LINE, 49, 49),#     $1.
                     F_SPLT = substr(LINE, 50, 50),#     $1.

                     #/* 2017 adding walkthrough plots */

                     WLK_BOUND_NO_1 = as.numeric(substr(LINE, 51, 51)),
                     WLK_BOUND_AZIMUTH_1 = as.numeric(substr(LINE, 52, 54)),#  3.
                     WLK_BOUND_DIST_1 = as.numeric(substr(LINE, 55, 58))/10,#  4.1
                     WLK_BOUND_NO_2 = as.numeric(substr(LINE, 59, 59)),#   1.
                     WLK_BOUND_AZIMUTH_2 = as.numeric(substr(LINE, 60, 62)),#.
                     WLK_BOUND_DIST_2 = as.numeric(substr(LINE, 63, 66))/10,#  4.1

                     WLK_BOUND_NO_3 = as.numeric(substr(LINE, 67, 67)),#    1.
                     WLK_BOUND_AZIMUTH_3 = as.numeric(substr(LINE, 68, 70)),#  3.
                     WLK_BOUND_DIST_3 = as.numeric(substr(LINE, 71, 74))/10,#   4.1

                     WLK_BOUND_NO_4 = as.numeric(substr(LINE, 75, 75)),#   1.
                     WLK_BOUND_AZIMUTH_4 = as.numeric(substr(LINE, 76, 78)),#  3.
                     WLK_BOUND_DIST_4 = as.numeric(substr(LINE, 79, 82))/10,#  4.1
                     WLK_BOUND_NO_5 = as.numeric(substr(LINE, 83, 83)),#    1.
                     WLK_BOUND_AZIMUTH_5 = as.numeric(substr(LINE, 84, 86)),#  3.
                     WLK_BOUND_DIST_5 = as.numeric(substr(LINE, 87, 90))/10,#  4.1
                     WLK_BOUND_NO_6 = as.numeric(substr(LINE, 91, 91)),#    1.
                     WLK_BOUND_AZIMUTH_6 = as.numeric(substr(LINE, 92, 94)),#.
                     WLK_BOUND_DIST_6 = as.numeric(substr(LINE, 95, 98))/10,#  4.1

                     WLK_BOUND_NO_7 = as.numeric(substr(LINE, 99, 99)),#    1.
                     WLK_BOUND_AZIMUTH_7 = as.numeric(substr(LINE, 100, 102)),#.
                     WLK_BOUND_DIST_7 = as.numeric(substr(LINE, 103, 106))/10,#  4.1
                     WLK_BOUND_NO_8 = as.numeric(substr(LINE, 107, 107)),#    1.
                     WLK_BOUND_AZIMUTH_8 = as.numeric(substr(LINE, 108, 110)),#  3.
                     WLK_BOUND_DIST_8 = as.numeric(substr(LINE, 111, 114))/10,#  4.1
                     WLK_BOUND_NO_9 = as.numeric(substr(LINE, 115, 115)),#    1.
                     WLK_BOUND_AZIMUTH_9 = as.numeric(substr(LINE, 116, 118)),#  3.
                     # WLK_BOUND_DIST_9= substr(LINE, 117  4.1
                     WLK_BOUND_DIST_9 = as.numeric(substr(LINE, 117, 120))/10,#  4.1
                     NO_PAGE = as.numeric(substr(LINE, 123, 124)))]

  card111 <- allText[REC_ID == "111",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       PLOT = substr(LINE, 10, 10),#       $1.
                       V_BAF = as.numeric(substr(LINE, 11, 14))/100,#      4.2
                       V_FULL = substr(LINE, 15, 15),#      $1.
                       V_HALF = substr(LINE, 16, 16),#     $1.
                       V_QRTR = substr(LINE, 17, 17),#     $1.
                       V_BDRY = substr(LINE, 18, 18),#     $1.
                       V_SPLT = substr(LINE, 19, 19),#     $1.
                       PRF = as.numeric(substr(LINE, 20, 24))/10000,#        5.4
                       DIOPTER = as.numeric(substr(LINE, 25, 29))/1000,#    5.3
                       AZ_BDRY = as.numeric(substr(LINE, 30, 32)),#    3.0
                       F_RAD = as.numeric(substr(LINE, 33, 36))/100,#      4.2
                       F_FULL = substr(LINE, 37, 37),#     $1.
                       F_HALF = substr(LINE, 38, 38),#     $1.
                       F_QRTR = substr(LINE, 39, 39),#     $1.
                       F_BDRY = substr(LINE, 40, 40),#     $1.
                       F_SPLT = substr(LINE, 41, 41),#     $1.
                       NO_PAGE = as.numeric(substr(LINE, 42, 43)),#     2.
                       AUX_AZ = as.numeric(substr(LINE, 44, 46)),#     3.
                       AUX_DIST = as.numeric(substr(LINE, 47, 50))/100)]
  cardb <- rbindlist(list(cardb, card111), fill = TRUE)
  # "3391-0006-MR1" TREE_NO "563" is "12.5" in Yong's and "4" in Rene's
  # One record has been duplicated in Yong's output CLSTR_ID "CMI5-0333-FR1"
  # TREE_NO "102"…...Rene's output has a single record with totally different attributes.
  cardc <- allText[REC_ID %in% c("082", "114"),
                   .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                     TREE_NO = substr(LINE, 10, 12),#    $3.
                     SPECIES = substr(LINE, 13, 15),#    $3.
                     LV_D = substr(LINE, 16, 16),#       $1.
                     S_F = substr(LINE, 17, 17),#        $1.
                     DBH = as.numeric(substr(LINE, 18, 21))/10,#        4.1
                     MEASEST1 = substr(LINE, 22, 22),#   $1.
                     BARK_PEX = substr(LINE, 23, 24),#   $2.
                     TREE_LEN = as.numeric(substr(LINE, 25, 27))/10,#   3.1
                     MEASEST2 = substr(LINE, 28, 28),#   $1.
                     CR_CL = substr(LINE, 29, 29),#      $1.
                     HT_BRCH_ASC = substr(LINE, 30, 31),# ht_brch_asc     $2.
                     LOG1 = substr(LINE, 32, 32),# log1       $1.
                     LEN1 = as.numeric(substr(LINE, 33, 34)),# len1       2.
                     LSND_1 = substr(LINE, 35, 36),# lsnd_1       $2.
                     LOG2 = substr(LINE, 37, 37),# log2       $1.
                     LEN2 = as.numeric(substr(LINE, 38, 39)),# len2       2.
                     LSND_2 = substr(LINE, 40, 41),# lsnd_2       $2.
                     LOG3 = substr(LINE, 42, 42),# log3       $1.
                     LEN3 = as.numeric(substr(LINE, 43, 44)),# len3       2.
                     LSND_3 = substr(LINE, 45, 46),# lsnd_3       $2.
                     LOG4 = substr(LINE, 47, 47),# log4       $1.
                     LEN4 = as.numeric(substr(LINE, 48, 49)),# len4       2.
                     LSND_4 = substr(LINE, 50, 51),# lsnd_4       $2.
                     WL_APPEA = substr(LINE, 52, 52),# WL_APPEA  $1.
                     WL_CROWN = substr(LINE, 53, 53),# WL_CROWN  $1.
                     WL_BARK = substr(LINE, 54, 54),# WL_BARK   $1.
                     WL_WOOD = substr(LINE, 55, 55),# WL_WOOD   $1.
                     WL_LICHE = substr(LINE, 56, 56),# WL_LICHE  $1.
                     WL_USE = substr(LINE, 57, 58),# WL_USE    $2.

                     DIAM_BTX = substr(LINE, 59, 61),# DIAM_BTX  $3.
                     HT_PROX = substr(LINE, 62, 63),# HT_PROX   $2.
                     S1 = substr(LINE, 64, 64),# S1        $1.   /* plot sector */
                     S2 = substr(LINE, 65, 65),# s2        $1.      /* residual tree */
                     S3 = substr(LINE, 66, 66))]


  card091 <- allText[REC_ID == "091",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       PLOT = substr(LINE, 10, 10),
                       NO_PAGE = substr(LINE, 11, 12))]

  cardd2 <- allText[REC_ID == "115",
                    .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                      TREE_NO = substr(LINE, 10, 12),#     $3.
                      DAM_AGN1 = substr(LINE, 13, 15),# dam_agn1   $3.
                      SEV1 = substr(LINE, 16, 17),# sev1      $2.
                      DAM_AGN2 = substr(LINE, 18, 20),# dam_agn2   $3.
                      SEV2 = substr(LINE, 21, 22),# sev2      $2.

                      LSS1 = substr(LINE, 23, 25),#        $3.
                      LSLN11 = substr(LINE, 26, 26),#      $1.
                      LSLNF1 = as.numeric(substr(LINE, 27, 29))/10,#       3.1
                      LSLN21 = substr(LINE, 30, 30),#      $1.
                      LSLNT1 = as.numeric(substr(LINE, 31, 33))/10,#       3.1
                      LSFQ1 = as.numeric(substr(LINE, 34, 34)),#       1.

                      LSS2 = substr(LINE, 35, 37),#       $3.
                      LSLN12 = substr(LINE, 38, 38),#     $1.
                      LSLNF2 = as.numeric(substr(LINE, 39, 41))/10,#      3.1
                      LSLN22 = substr(LINE, 42, 42),#     $1.
                      LSLNT2 = as.numeric(substr(LINE, 43, 45))/10,#      3.1
                      LSFQ2 = as.numeric(substr(LINE, 46, 46)),#       1.

                      LSS3 = substr(LINE, 47, 49),#       $3.
                      LSLN13 = substr(LINE, 50, 50),#     $1.
                      LSLNF3 = as.numeric(substr(LINE, 51, 53))/10,#      3.1
                      LSLN23 = substr(LINE, 54, 54),#     $1.
                      LSLNT3 = as.numeric(substr(LINE, 55, 57))/10,#      3.1
                      LSFQ3 = as.numeric(substr(LINE, 58, 58)),#      1.

                      S4 = substr(LINE, 59, 59),#         $1.
                      S5 = substr(LINE, 60, 60),#         $1.
                      S6 = substr(LINE, 61, 61),#         $1.
                      S7 = substr(LINE, 62, 62),#         $1.
                      S8 = substr(LINE, 63, 63),#         $1.
                      S9 = substr(LINE, 64, 64),#         $1.
                      S10 = substr(LINE, 65, 65),#        $1.
                      S11 = substr(LINE, 66, 66),#        $1.
                      S12 = substr(LINE, 67, 67))]


  cardd1 <- allText[REC_ID == "092",
                    .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                      TREE_NO= substr(LINE, 10, 12),#     $3.

                      DAM_AGN1 = substr(LINE, 13, 15),# dam_agn1   $3.
                      SEV1 = substr(LINE, 16, 17),# sev1      $2.
                      DAM_AGN2 = substr(LINE, 18, 20),# dam_agn2   $3.
                      SEV2 = substr(LINE, 21, 22),# sev2      $2.

                      LSS1 = substr(LINE, 23, 25),#        $3.
                      LSLN11 = substr(LINE, 26, 26),#      $1.
                      LSLNF1 = as.numeric(substr(LINE, 27, 29))/10,#       3.1
                      LSLN21 = substr(LINE, 30, 30),#      $1.
                      LSLNT1 = as.numeric(substr(LINE, 31, 33))/10,#       3.1
                      LSFQ1 = as.numeric(substr(LINE, 34, 34)),#       1.

                      LSS2 = substr(LINE, 35, 37),#       $3.
                      LSLN12 = substr(LINE, 38, 38),#     $1.
                      LSLNF2 = as.numeric(substr(LINE, 39, 41))/10,#      3.1
                      LSLN22 = substr(LINE, 42, 42),#     $1.
                      LSLNT2 = as.numeric(substr(LINE, 43, 45))/10,#      3.1
                      LSFQ2 = as.numeric(substr(LINE, 46, 46)),#       1.

                      LSS3 = substr(LINE, 47, 49),#       $3.
                      LSLN13 = substr(LINE, 50, 50),#     $1.
                      LSLNF3 = as.numeric(substr(LINE, 51, 53))/10,#      3.1
                      LSLN23 = substr(LINE, 54, 54),#     $1.
                      LSLNT3 = as.numeric(substr(LINE, 55, 57))/10,#      3.1
                      LSFQ3 = as.numeric(substr(LINE, 58, 58)),#      1.

                      AZIMUTH = as.numeric(substr(LINE, 59, 61)),#   3.   /* problem with column 58 */
                      DISTANCE = as.numeric(substr(LINE, 62, 65))/100,#  4.2
                      S1 = substr(LINE, 66, 66),#        $1.
                      S2 = substr(LINE, 67, 67))]
  carde <- allText[REC_ID == "101",
                   .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                     PLOT = substr(LINE, 10, 10),#       $1.
                     F_RAD = as.numeric(substr(LINE, 11, 14))/100,#      4.2
                     F_FULL = substr(LINE, 15, 15),#     $1.
                     F_HALF = substr(LINE, 16, 16),#     $1.
                     F_QRTR = substr(LINE, 17, 17),#     $1.
                     F_BDRY = substr(LINE, 18, 18),#     $1.
                     F_SPLT = substr(LINE, 19, 19), #     $1.
                     ST_RAD = as.numeric(substr(LINE, 20, 23))/100,#     4.2
                     ST_FULL = substr(LINE, 24, 24),#    $1.
                     ST_HALF = substr(LINE, 25, 25),#    $1.
                     ST_QRTR = substr(LINE, 26, 26),#    $1.
                     ST_BDRY = substr(LINE, 27, 27),#    $1.
                     NO_PAGE = as.numeric(substr(LINE, 28, 29)))]

  cardf <- allText[REC_ID == "102",
                   .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                     SPECIES = substr(LINE, 10, 12),#    $3.
                     TOTAL1 = as.numeric(substr(LINE, 13, 14)),#     2.
                     TOTAL2 = as.numeric(substr(LINE, 15, 16)),#     2.
                     TOTAL3 = as.numeric(substr(LINE, 17, 18)),#     2.
                     S1 = substr(LINE, 19, 19),#         $1.
                     S2 = substr(LINE, 20, 20),#         $1.
                     S3 = substr(LINE, 21, 21),#         $1.
                     S4 = substr(LINE, 22, 22))]



  cardg <- allText[REC_ID == "103",
                   .(REC_ID, PAGE_NO, CLSTR_ID,
                     SPECIES = substr(LINE, 10, 12),#    $3.
                     FREQ = as.numeric(substr(LINE, 13, 14)),#        2.
                     DIB = as.numeric(substr(LINE, 15, 18))/10,#        4.1
                     HEIGHT = as.numeric(substr(LINE, 19, 20))/10,#     2.1
                     PCT_SNX = substr(LINE, 21, 22),#    $2.
                     BARK_RET = as.numeric(substr(LINE, 23, 23)),#   1.
                     WL_WOOD = substr(LINE, 24, 24),#    $1.
                     WL_USE = substr(LINE, 25, 26),#     $2.
                     ROOT_ROT = substr(LINE, 27, 29),#   $3.
                     S5 = substr(LINE, 30, 30),#         $1.
                     S6 = substr(LINE, 31, 31),#         $1.
                     S7 = substr(LINE, 32, 32),#         $1.
                     S8 = substr(LINE, 33, 33))]

  ## need to modify according to new revision
  cardh_post2017 <- allText[REC_ID %in% c("104", "113") &
                              measurementDate >= as.Date("2017-01-01"),
                            .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                              PLOT_TYP = substr(LINE, 10, 19),#   $10.
                              TREE_NO = substr(LINE, 20, 22),#    3.
                              TREETYPE = substr(LINE, 23, 23),#   $1.
                              SPECIES = substr(LINE, 24, 26),#    $3.
                              CR_CL = substr(LINE, 27, 27),#      $1.

                              BNG_DIAM = as.numeric(substr(LINE, 28, 31))/10,#   4.1
                              TREE_LEN = as.numeric(substr(LINE, 32, 35))/10,#   4.1
                              SUIT_HT = substr(LINE, 36, 36),#    $1.
                              BARK_THKX = substr(LINE, 37, 39),#  $3.
                              # *r.dejong 2017-oct-18 changes to fix bark_thk read in as a character;
                              # bark_thk = input(compress(bark_thkx,'-'),best12.);
                              BORED_HT = as.numeric(substr(LINE, 40, 41))/10,#   2.1
                              MEAS_COD = substr(LINE, 42, 44),#  $3.
                              BORE_AGE = as.numeric(substr(LINE, 45, 48)),#   4.
                              GROW_5YR = as.numeric(substr(LINE, 49, 50)),#   2.
                              GROW_10YR = as.numeric(substr(LINE, 51, 52)),#   2.
                              GROW_20YR = as.numeric(substr(LINE, 53, 55)),#   3.
                              PRO_LEN = as.numeric(substr(LINE, 56, 59))/10,#    4.1
                              PRO_RING = as.numeric(substr(LINE, 60, 63)),#   4.
                              AGE_CORR = as.numeric(substr(LINE, 64, 65)),#   2.
                              TOTAL_AG = as.numeric(substr(LINE, 66, 68)),#   3.
                              PHYS_AGE = as.numeric(substr(LINE, 69, 71)),#   3.

                              BORAG_FL = as.numeric(substr(LINE, 72, 75)),#   4.
                              # BORAG_FL = substr(LINE, 72, 75),#   4.
                              SUIT_TR = substr(LINE, 76, 76))]

  cardh_pre2017 <- allText[REC_ID %in% c("104", "113") &
                             measurementDate <= as.Date("2016-12-31"),
                           .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                             PLOT_TYP = substr(LINE, 10, 19),#   $10.
                             TREE_NO = substr(LINE, 20, 22),#    3.
                             TREETYPE = substr(LINE, 23, 23),#   $1.
                             SPECIES = substr(LINE, 24, 26),#    $3.
                             CR_CL = substr(LINE, 27, 27),#      $1.

                             BNG_DIAM = as.numeric(substr(LINE, 28, 31))/10,#   4.1
                             TREE_LEN = as.numeric(substr(LINE, 32, 35))/10,#   4.1
                             SUIT_HT = substr(LINE, 36, 36),#    $1.
                             BARK_THKX = substr(LINE, 37, 39),#  $3.
                             # *r.dejong 2017-oct-18 changes to fix bark_thk read in as a character;
                             # bark_thk = input(compress(bark_thkx,'-'),best12.);
                             BORED_HT = as.numeric(substr(LINE, 40, 41))/10,#   2.1
                             MEAS_COD = substr(LINE, 42, 44),#  $3.
                             BORE_AGE = as.numeric(substr(LINE, 45, 48)),#   4.
                             GROW_5YR = as.numeric(substr(LINE, 49, 50)),#   2.
                             GROW_10YR = as.numeric(substr(LINE, 51, 52)),#   2.
                             GROW_20YR = as.numeric(substr(LINE, 53, 55)),#   3.
                             PRO_LEN = as.numeric(substr(LINE, 56, 59))/10,#    4.1
                             PRO_RING = as.numeric(substr(LINE, 60, 62)),#   4.
                             AGE_CORR = as.numeric(substr(LINE, 63, 64)),#   2.
                             TOTAL_AG = as.numeric(substr(LINE, 65, 67)),#   3.
                             PHYS_AGE = as.numeric(substr(LINE, 68, 70)),#   3.
                             BORAG_FL = as.numeric(substr(LINE, 71, 74)),#   4.
                             SUIT_TR = substr(LINE, 75, 75))]

  cardh <- rbind(cardh_post2017, cardh_pre2017)


  ## rec id 112 only exist before 1999 samples, in which only live standing trees
  ## were sampled, the tree number for these samples only have 2 digits
  cardi <- allText[REC_ID == "112",
                   .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                     TREE_NO = substr(LINE, 10, 11),#    2.
                     SPECIES = substr(LINE, 12, 14),#    $3.
                     DBH = as.numeric(substr(LINE, 15, 18))/10)] #        4.1

  # /*****************************************************/
  #   /* CARD 12 /13 Ecological Description 1 and 2 (EP)  */
  #   /*****************************************************/

  card121 <- allText[REC_ID == "121",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       PLOT = substr(LINE, 10, 1),#       $1.
                       POLYGON = substr(LINE, 11, 24),#    $14.
                       MEAS_DTC = substr(LINE, 25, 33),#   $9.
                       CREW1 = substr(LINE, 34, 37),#     $4.
                       CREW2 = substr(LINE, 38, 41),#     $4.
                       NO_PAGES = as.numeric(substr(LINE, 42, 43)),#   2.
                       RADIUS = as.numeric(substr(LINE, 44, 47))/100,#     4.2
                       UFORMITY = as.numeric(substr(LINE, 48, 48)),#   1.
                       BZONE = substr(LINE, 49, 52),#      $4.
                       BSBZONE = substr(LINE, 53, 55),#     $3.
                       BEC_VAR = substr(LINE, 56, 57),#    $2.
                       BEC_P = substr(LINE, 58, 59),#      $2.
                       SLOPE = as.numeric(substr(LINE, 60, 62)),#      3.
                       ASPECT = as.numeric(substr(LINE, 63, 65)),#     3.
                       ELEV = as.numeric(substr(LINE, 66, 69)),#       4.
                       SURFACE = substr(LINE, 70, 71),#    $2.
                       MESO = substr(LINE, 72, 73),#       $1.
                       TOPO = substr(LINE, 73, 74),#       $2.
                       STONESX = substr(LINE, 75, 77),#     $3.
                       BEDROCKX = substr(LINE, 78, 80),#    $3.
                       SF_PLOT = substr(LINE, 81, 81),#    $1.
                       SF_BET = substr(LINE, 82, 83),#     $2.
                       GUL_PLOT = substr(LINE, 84, 84),#   $1.
                       GUL_BET = substr(LINE, 85, 86),#    $2.
                       FLOOD = substr(LINE, 87, 87),#      $1.
                       W_FLOWX = substr(LINE, 88, 90),#     $3.
                       W_STANDX = substr(LINE, 91, 92),#    $3.
                       SURF_1 = substr(LINE, 94, 95),#     $2.
                       SURF_2 = substr(LINE, 96, 97),#     $2.
                       W_TABLE = as.numeric(substr(LINE, 98, 100)),#     3.
                       MOTTLE = as.numeric(substr(LINE, 101, 103)),#     3.
                       ROOT_RES = as.numeric(substr(LINE, 104, 106)),#   3.
                       BEDROCK2 = as.numeric(substr(LINE, 107, 109)),#   3.
                       FROZEN = as.numeric(substr(LINE, 110, 112)),#     3.
                       CARBON = as.numeric(substr(LINE, 113, 115)),#     3.
                       R1 = as.numeric(substr(LINE, 116, 118)),#         3.
                       R2 = as.numeric(substr(LINE, 119, 121)),#         3.
                       SOIL_NA = substr(LINE, 122, 122),#    $1.
                       HUMUS = substr(LINE, 123, 124),#      $2.
                       SOIL_COL = substr(LINE, 125, 126))]


  card122 <- allText[REC_ID == "122",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       LOCAT = substr(LINE, 10, 20),#   $11.
                       SMR = substr(LINE, 21, 21),#     $1.
                       SNR = substr(LINE, 22, 22),#     $1.
                       SERNO = substr(LINE, 23, 26),#   $4.  /* 2017   added 1 char to serno */
                       SER_PER = substr(LINE, 27,29),#  3.
                       LAND_1 = substr(LINE, 30, 30),#   $1.
                       LAND_2 = substr(LINE, 31, 31),#   $1.
                       LAND_3 = substr(LINE, 32, 32),#   $1.
                       LAND_4 = substr(LINE, 33, 34),#   $2.
                       LAND_5 = substr(LINE, 35, 36),#   $2.
                       S1 = substr(LINE, 37, 37),#       $1.
                       S2 = substr(LINE, 38, 38),#       $1.
                       S3 = substr(LINE, 39, 39),#       $1.
                       S4 = substr(LINE, 40, 40),#       $1.
                       S5 = substr(LINE, 41, 41))]



  card131 <- allText[REC_ID == "131",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       PLOT = substr(LINE, 10, 10),#       $1.
                       NO_PAGES = as.numeric(substr(LINE, 11, 12)),#   2.
                       SLOPE = as.numeric(substr(LINE, 13, 15)),#      3.
                       ASPECT = as.numeric(substr(LINE, 16, 18)),#     3.
                       ELEV = as.numeric(substr(LINE, 19, 22)),#       4.
                       SURFACE = substr(LINE, 23, 24),#    $2.
                       MESO = substr(LINE, 25, 25),#       $1.
                       TOPO = substr(LINE, 26, 27),#        $3.
                       STONESX = substr(LINE, 28, 30),#     $3.
                       BEDROCKX = substr(LINE, 31, 33),#    $3.
                       FLOOD = substr(LINE, 34, 34),#      $1.
                       W_FLOWX = substr(LINE, 35, 37),#     $3.
                       W_STANDX = substr(LINE, 38, 40),#    $3.
                       SURF_1 = substr(LINE, 41, 42),#     $2.
                       SURF_2 = substr(LINE, 43, 44),#     $2.
                       W_TABLE = as.numeric(substr(LINE, 45, 47)),#     3.
                       MOTTLE = as.numeric(substr(LINE, 48, 50)),#     3.
                       ROOT_RES = as.numeric(substr(LINE, 51, 53)),#   3.
                       BEDROCK2 = as.numeric(substr(LINE, 54, 56)),#   3.
                       FROZEN = as.numeric(substr(LINE, 57, 59)),#     3.
                       CARBON = as.numeric(substr(LINE, 60, 62)),#     3.
                       R1 = as.numeric(substr(LINE, 63, 65)),#         3.
                       R2 = as.numeric(substr(LINE, 66, 68)),#         3.
                       SOIL_NA = substr(LINE, 69, 69),#        $1.
                       HUMUS = substr(LINE, 70, 71),#      $2.
                       SOIL_COL = substr(LINE, 72, 73))]




  card132 <- allText[REC_ID %in% c("123", "132"),
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       HORIZON = substr(LINE, 10, 12),#    $3.
                       DEPTH = as.numeric(substr(LINE, 13, 16))/10)]


  card133 <- allText[REC_ID %in% c("124", "133"),
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       HORIZON = substr(LINE, 10, 12),#    $3.
                       DEPTH = as.numeric(substr(LINE, 13, 16)),#      4.2
                       TEXTURE = substr(LINE, 17, 20),#    $4.
                       TOTAL = as.numeric(substr(LINE, 21, 23)),#      3.
                       GRAVEL = as.numeric(substr(LINE, 24, 26)),#     3.
                       COBBLES = as.numeric(substr(LINE, 27, 29)),#    3.
                       S1 = substr(LINE, 30, 30),#         $1.
                       S2 = substr(LINE, 31, 31),#         $1.
                       S3 = substr(LINE, 32, 32),#         $1.
                       S4 = substr(LINE, 33, 33))]
  card133[REC_ID == "124", DEPTH := DEPTH/10]
  card133[REC_ID == "133", DEPTH := DEPTH/100]


  card141 <- allText[REC_ID == "141",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       PL = substr(LINE, 10, 10),#        $1.
                       MEAS_DTC = substr(LINE, 11, 19),#  $9.
                       CREW1 = substr(LINE, 20, 23),#    $4.
                       CREW2 = substr(LINE, 24, 27),#    $4.
                       NO_PAGE = as.numeric(substr(LINE, 28, 29)),#   2.
                       LYR_A = substr(LINE, 30, 32),#     $3.
                       LYR_B1 = substr(LINE, 33, 35),#    $3.
                       LYR_B2 = substr(LINE, 36, 38))]


  card142 <- allText[REC_ID == "142",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       ITEM_NO = substr(LINE, 10, 11),#    $2.
                       SPECLONG = substr(LINE, 12, 19),#    $8.
                       CVR_A = substr(LINE, 20, 23),#      $4.
                       CVR_B1 = substr(LINE, 24, 27),#     $4.
                       HT_B1 = as.numeric(substr(LINE, 28, 29))/10,#      2.1
                       CVR_B2 = substr(LINE, 30, 33),#     $4.
                       HT_B2 = as.numeric(substr(LINE, 34, 35))/10,#      2.1
                       AB_RAD = as.numeric(substr(LINE, 36, 39))/100,#     4.2
                       AB_SHP = substr(LINE, 40, 47),#     $8.
                       SOIL_DH = substr(LINE, 48, 50),#    $3.
                       WOOD_DW = substr(LINE, 51, 53),#    $3.
                       ROCK_DR = substr(LINE, 54, 56),#    $3.
                       D_RAD = as.numeric(substr(LINE, 57, 60))/100,#      4.2
                       D_SHP = substr(LINE, 61, 68),#      $8.
                       S1 = substr(LINE, 69, 69),#         $1.
                       S2 = substr(LINE, 70, 70),#         $1.
                       S3 = substr(LINE, 71, 71))]


  card151 <- allText[REC_ID == "151",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       PLOT = substr(LINE, 10, 10),#        $1.
                       IHB = substr(LINE, 11, 13),#         $3.
                       IMS_DH = substr(LINE, 14, 16),#      $3.
                       IMS_DW = substr(LINE, 17, 19),#      $3.
                       IMS_DR = substr(LINE, 20, 22),#      $3.
                       MEAS_DTC = substr(LINE, 23, 31),#     $9.
                       RADIUS = as.numeric(substr(LINE, 32, 35))/100,#       4.2
                       PL_SHP = substr(LINE, 36, 43),#      $8.
                       NO_PAGE = substr(LINE, 44, 45))]



  card152 <- allText[REC_ID == "152",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       ITEM_NO = substr(LINE, 10, 11),#    $2.
                       SPECLONG = substr(LINE, 12, 19),#    $8.
                       HERB = substr(LINE, 20, 22),#       $3.
                       MOSS_DH = substr(LINE, 23, 25),#    $3.
                       MOSS_DW = substr(LINE, 26, 28),#    $3.
                       MOSS_DR = substr(LINE, 29, 31),#    $3.
                       S1 = substr(LINE, 32, 32),#         $1.
                       S2 = substr(LINE, 33, 33),#         $1.
                       S3 = substr(LINE, 34, 34),#         $1.
                       S4 = substr(LINE, 35, 35),#         $1.
                       S5 = substr(LINE, 36, 36),#         $1.
                       S6 = substr(LINE, 37, 37))]




  # /*****************************************************/
  #   /* CARD 16 SUCCESSION INTERPRETATIONS eo              */
  #   /*****************************************************/
  card161 <- allText[REC_ID == "161",
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       PLOT = substr(LINE, 10, 10),#      $1.
                       MEAS_DTC = substr(LINE, 11, 19),#  $9.
                       CREW1 = substr(LINE, 20, 23),#    $4.
                       CREW2 = substr(LINE, 24, 27),#    $4.
                       NO_PAGES = as.numeric(substr(LINE, 28, 29)),#  2.
                       FACTR_1 = substr(LINE, 30, 31),#   $2.
                       FACTR_2 = substr(LINE, 32, 33),#   $2.
                       FACTR_3 = substr(LINE, 34, 35),#   $2.
                       FACTR_4 = substr(LINE, 36, 37),#   $2.
                       SPEC_PR1 = substr(LINE, 38, 40),#  $3.
                       SPEC_PR2 = substr(LINE, 41, 43),#  $3.
                       SPEC_CR1 = substr(LINE, 44, 46),#  $3. ## corrected here
                       SPEC_CR2 = substr(LINE, 47, 49),#  $3.
                       HARVEST = substr(LINE, 50, 50),#   $1.
                       SNAGS = substr(LINE, 51, 51),#     $1.
                       CWD = substr(LINE, 52, 52),#       $1.
                       CANOPY = substr(LINE, 53, 53),#    $1.
                       STRUCT = substr(LINE, 54, 54),#    $1.
                       STABLTY = substr(LINE, 55, 55),#   $1.
                       AGE = substr(LINE, 56, 56),#       $1.
                       SIZE = substr(LINE, 57, 57),#      $1.
                       STAGE = substr(LINE, 58, 59),#     $2.
                       ALV_PCT = as.numeric(substr(LINE, 60, 62)),#   3.
                       OLD_GRW = substr(LINE, 63, 66),#   $4.
                       CR1 = substr(LINE, 71, 90),#        $20.
                       CR2 = substr(LINE, 91, 110),#        $20.
                       CR3 = substr(LINE, 111, 130),#       $20.
                       S1_COD = substr(LINE, 131, 132),#    $2.
                       S1_VAL = substr(LINE, 133, 133),#    $1.
                       S2_COD = substr(LINE, 134, 135),#    $2.
                       S2_VAL = substr(LINE, 136, 136),#    $1.
                       S3_COD = substr(LINE, 145, 146),#    $2.
                       S3_VAL = substr(LINE, 147, 147))]

  cardnot <- allText[REC_ID %in% c('014', '015','024','032','043','064',
                                   '083', '093', '105','116', '125', '134',
                                   '143', '153', '162'),
                     .(REC_ID, PAGE_NO, SEQ, CLSTR_ID,
                       NOTES1A = substr(LINE, 10, 137),
                       NOTES1B = substr(LINE, 138, 265))]
  cardnot[, ':='(NOTES1A = sub("\\s+$", "", NOTES1A),
                 NOTES1B = sub("\\s+$", "", NOTES1B))]

  ## sample header
  card012[,':='(R_SNAM = parse_name(RESP_FD)$lastName,
                R_FIRST = parse_name(RESP_FD)$firstName,
                O_SNAM = parse_name(CHK_OFF)$lastName,
                O_FIRST = parse_name(CHK_OFF)$firstName,
                F_SNAM = parse_name(CHK_FD)$lastName,
                F_FIRST = parse_name(CHK_FD)$firstName)]
  card012[REC_TYP == "TREE DATA ", new_type := "T"]
  card012[REC_TYP == "ECOLOGICAL", new_type := "E"]
  tempnames <- c("R_SNAM",
                 "R_FIRST",
                 "O_SNAM",
                 "O_FIRST",
                 "F_SNAM",
                 "F_FIRST")
  set(card012, , c("SEQ", "REC_TYP", "RESP_FD", "CHK_FD", "CHK_OFF"), NULL)

  card012 <- card012[!duplicated(card012),]
  cardx12 <- reshape(data = card012,
                     v.names = c(tempnames, "FLD_DT", "OFF_DT"),
                     timevar = "new_type",
                     idvar = c("CLSTR_ID", "REC_ID"),
                     direction = "wide",
                     sep = "")
  if("T" %in% unique(card012$new_type)){
    setnames(cardx12,
             paste(tempnames, "T", sep = ""),
             paste("T", tempnames, sep = ""))
    setnames(cardx12,
             paste(c("FLD_DT", "OFF_DT"), "T", sep = ""),
             paste("TR", c("FLDT", "OFFDT"), sep = "_"))
  }
  if ("E" %in% unique(card012$new_type)) {
    setnames(cardx12,
             paste(tempnames, "E", sep = ""),
             paste("E", tempnames, sep = ""))
    setnames(cardx12,
             paste(c("FLD_DT", "OFF_DT"), "E", sep = ""),
             paste("EC", c("FLDT", "OFFDT"), sep = "_"))
  }
  cardx12[,':='(FLD_DTC = NULL,
                OFF_DTC = NULL)]
  rm(tempnames, card012)
  ## compare to original sas codes, the below improves:
  ## keep all the photo type and photo index, while the sas
  ## just keep the first 6 records, and without sort mechanism
  card013 <- card013[order(CLSTR_ID, SEQ),]
  card013[, SEQ_new := 1:length(PHOT_TYP), by = "CLSTR_ID"]
  cardx13 <- reshape(data = card013[,.(CLSTR_ID, PHOT_TYP, PHOT_IND,
                                       SEQ_new)],
                     v.names = c("PHOT_TYP", "PHOT_IND"),
                     timevar = "SEQ_new",
                     idvar = "CLSTR_ID",
                     direction = "wide",
                     sep = "")


  notex13 <- reshape(card013[,.(CLSTR_ID, REC_ID, SEQ, PHOT_TYP, PHOT_IND,
                                NOTES1A, NOTES1B)],
                     varying = list(c("NOTES1A", "NOTES1B")),
                     times = c(1, 2),
                     timevar = "NOTENAME",
                     idvar = c("CLSTR_ID", "SEQ"),
                     ids = "SEQ",
                     v.names = "NOTES",
                     direction = "long")

  notex13 <- notex13[NOTES != "",]
  notex13 <- notex13[order(CLSTR_ID, SEQ, NOTENAME),]
  notex13[, ':='(NOTES = sub("\\s+$", "", NOTES),
                 NOTENAME = NULL)]

  card014 <- cardnot[REC_ID %in% c("014", "015"),]
  ## to keep all notes for multiple records
  card014 <- card014[, .(NOTES1A = paste(NOTES1A, collapse = " "),
                         NOTES1B = paste(NOTES1B, collapse = " ")),
                     by = c("CLSTR_ID", "REC_ID", "SEQ")]

  notex14 <- reshape(card014[,.(CLSTR_ID, REC_ID, SEQ,
                                NOTES1A, NOTES1B)],
                     varying = list(c("NOTES1A", "NOTES1B")),
                     times = c(1, 2),
                     timevar = "NOTENAME",
                     idvar = c("CLSTR_ID", "REC_ID", "SEQ"),
                     ids = c("CLSTR_ID", "REC_ID", "SEQ"),
                     v.names = "NOTES",
                     direction = "long")

  notex14 <- notex14[!(NOTES %in% c("", " ")),]
  notex14 <- notex14[order(REC_ID, SEQ, NOTENAME),]
  notex14[, ':='(NOTES = sub("\\s+$", "", NOTES),
                 NOTENAME = NULL)]

  ### note _ch need to be outputed
  note_ch <- rbindlist(list(notex13, notex14),
                       fill = TRUE)


  card011[,':='(REC_ID = NULL,
                PAGE_NO = NULL,
                SEQ = NULL,
                MEAS_DTC = NULL)]
  cardx12[,':='(REC_ID = NULL,
                PAGE_NO = NULL)]
  carda <- merge(card011, cardx12, by  = "CLSTR_ID",
                 all = TRUE)
  carda <- merge(carda, cardx13, by  = "CLSTR_ID",
                 all = TRUE)
  carda[, ASCII_DT := as.Date(Sys.time())]
  setnames(carda,
           c("S9", "S10"),
           c("STAND_DIS_CAT", "STAND_DIS_SUBCAT"))

  rm(card011, cardx12, cardx13)


  card022[sub("\\s+$", "", LOCATN) == "TIE POINT",
          LOCATN := "TP"]
  card022[sub("\\s+$", "", LOCATN) == "INTEGRATED PLOT CENTRE",
          LOCATN := "IP"]
  tempnames <- c("MAP", "POLY", "FLTLN", "PHOTO")
  cardx22 <- reshape(data = card022[,.(CLSTR_ID,
                                       LOCATN, MAP = MAP_NO, POLY = POLYGON,
                                       FLTLN = FLT_LINE, PHOTO = PHOTO_NO)],
                     v.names = tempnames,
                     idvar = "CLSTR_ID",
                     timevar = "LOCATN",
                     direction = "wide",
                     sep = "")
  setnames(cardx22,
           c(paste(tempnames, "TP", sep = ""), paste(tempnames, "IP", sep = "")),
           c(paste("TP_", tempnames, sep = ""), paste("IP_",tempnames, sep = "")))

  rm(tempnames)

  tempnames <- c("SPEC", "DBH", "AZ_TR", "DT_TR")
  card023[sub("\\s+$", "", PT_TREE) == "TIE POINT DETAIL",
          PT_TREE := "TP"]
  card023[sub("\\s+$", "", PT_TREE) == "15M POINT DETAIL",
          PT_TREE := "RP"]
  cardx23 <- reshape(data = card023[,.(CLSTR_ID, PT_TREE,
                                       SPEC = SPECIES, DBH,
                                       AZ_TR = AZ_TREE, DT_TR = DISTANCE)],
                     v.names = tempnames,
                     timevar = "PT_TREE",
                     idvar = "CLSTR_ID",
                     direction = "wide",
                     sep = "")
  setnames(cardx23,
           c(paste(tempnames, "TP", sep = ""), paste(tempnames, "RP", sep = "")),
           c(paste("TP_", tempnames, sep = ""), paste("RP_",tempnames, sep = "")))
  rm(tempnames)


  not024 <- cardnot[REC_ID == "024"]
  not024 <- not024[,.(NOTES1A = paste(NOTES1A, collapse = " "),
                      NOTES1B = paste(NOTES1B, collapse = " ")),
                   by = c("CLSTR_ID", "REC_ID", "SEQ")]
  note_cp <- reshape(not024[,.(CLSTR_ID, REC_ID, SEQ,
                               NOTES1A, NOTES1B)],
                     varying = list(c("NOTES1A", "NOTES1B")),
                     times = c(1, 2),
                     timevar = "NOTENAME",
                     idvar = c("CLSTR_ID", "REC_ID", "SEQ"),
                     ids = c("CLSTR_ID", "REC_ID", "SEQ"),
                     v.names = "NOTES",
                     direction = "long")

  note_cp <- note_cp[!(NOTES %in% c("", " ")),
                     .(CLSTR_ID, REC_ID, SEQ, NOTES)]

  card021[,c("REC_ID", "PAGE_NO", "SEQ") := NULL]
  card_cp <- merge(card021, cardx22,
                   by = "CLSTR_ID", all = TRUE)
  card_cp <- merge(card_cp, cardx23,
                   by = "CLSTR_ID", all = TRUE)

  cardcl <- card031[,.(CLSTR_ID, IP_GPSID, IP_UTM, IP_NRTH, IP_EAST,
                       IP_ELEV, IP_AZ_PN, IP_DT_PN, IP_AZ_GP,
                       IP_DT_GP, PLT_SPC)]
  card032 <- cardnot[REC_ID == "032",]
  card032 <- card032[,.(NOTES1A = paste(NOTES1A, collapse = " "),
                        NOTES1B = paste(NOTES1B, collapse = " ")),
                     by = c("CLSTR_ID", "REC_ID", "SEQ")]

  note_cl <- reshape(card032[,.(CLSTR_ID, REC_ID, SEQ,
                                NOTES1A, NOTES1B)],
                     varying = list(c("NOTES1A", "NOTES1B")),
                     times = c(1, 2),
                     timevar = "NOTENAME",
                     idvar = c("CLSTR_ID", "REC_ID", "SEQ"),
                     ids = c("CLSTR_ID", "REC_ID", "SEQ"),
                     v.names = "NOTES",
                     direction = "long")
  note_cl <- note_cl[!(NOTES %in% c("", " ")),
                     .(CLSTR_ID, REC_ID, SEQ, NOTES)]

  card041[, CREW1 := gsub(" ", "", CREW1)]
  rng_hdr <- card041[CREW1 != "",.(CLSTR_ID, CREW1, CREW2)]
  trans <- card041[,.(CLSTR_ID, AZIMUTH, TRANSECT, OBS_LLEN, TOT_LLEN)]
  c4forage <- card041[TRANSECT == 1,.(CLSTR_ID, UTIL_X1, UTIL_X2, UTIL_X3, UTIL_X4,
                                      GRAM_Q, FORBS_Q, MS_GRAM, MS_FORB)]
  if(nrow(c4forage) > 0){
    c4forage_long <- reshape(c4forage,
                             varying = list(paste("UTIL_X", 1:4, sep = "")),
                             times = 1:4,
                             timevar = "UTIL",
                             idvar = "CLSTR_ID",
                             v.names = "UTIL_X",
                             direction = "long") %>% data.table


    c4forage_long[UTIL_X == 0, UTIL_CD := 0]
    c4forage_long[UTIL_X == 1, UTIL_CD := 7.5]
    c4forage_long[UTIL_X == 2, UTIL_CD := 26]
    c4forage_long[UTIL_X == 3, UTIL_CD := 46]
    c4forage_long[UTIL_X == 4, UTIL_CD := 74]
    c4forage_long[UTIL_X == 5, UTIL_CD := 90]
    c4forage_long[, UTIL_AVG := mean(UTIL_CD),
                  by = "CLSTR_ID"]

    c4forage <- reshape(c4forage_long,
                        v.names = c("UTIL_X", "UTIL_CD"),
                        timevar = "UTIL",
                        idvar = "CLSTR_ID",
                        direction = "wide",
                        sep = "")
    rm(c4forage_long)
  }
  # c4forage[UTIL_CD1 == 0,
  #          UTIL_CD1 := NA]
  # c4forage[UTIL_CD2 == 0,
  #          UTIL_CD2 := NA]
  # c4forage[UTIL_CD3 == 0,
  #          UTIL_CD3 := NA]
  # c4forage[UTIL_CD4 == 0,
  #          UTIL_CD4 := NA]
  # c4forage[UTIL_AVG == 0,
  #          UTIL_AVG := NA]
  card043 <- cardnot[REC_ID == "043", .(CLSTR_ID, SEQ, NOTES1A, NOTES1B)]
  card043[, TRANSECT := substr(NOTES1A, 1, 1)]
  card043[, NOTES1A := substr(NOTES1A, 2, 128)]
  if(nrow(card043) > 0){
    note_rs <- reshape(card043,
                       varying = list(c("NOTES1A", "NOTES1B")),
                       times = c(1, 2),
                       timevar = "NOTENAME",
                       idvar = c("CLSTR_ID", "TRANSECT", "SEQ"),
                       ids = c("CLSTR_ID", "TRANSECT", "SEQ"),
                       v.names = "NOTES",
                       direction = "long")
    note_rs <- note_rs[!(NOTES %in% c("", "")),]
    note_rs <- note_rs[order(CLSTR_ID, TRANSECT, SEQ, NOTENAME),
                       .(CLSTR_ID, TRANSECT, SEQ, NOTES)]
  } else {
    note_rs <- NA
  }

  cwd_hdr <- card061[TRANSECT == 1, .(CLSTR_ID, CREW1, CREW2)]
  cwd_hdr <- cwd_hdr[!duplicated(cwd_hdr),]
  c6trans <- card061[, .(CLSTR_ID, TRANSECT, AZIMUTH, OBS_LLEN)]
  c6trans <- c6trans[!duplicated(c6trans),]

  cardx6x <- rbindlist(list(card062, card063), fill = TRUE)
  cardx6x[PCT_CLSX %in% c("--", "-"), PCT_CLS1 := 100]
  cardx6x[is.na(PCT_CLS1), PCT_CLS1 := as.numeric(PCT_CLSX)]
  cardx6x[CLS_OTHX == "-", CLS_OTH := 100]
  cardx6x[is.na(CLS_OTH), CLS_OTH := as.numeric(CLS_OTHX)]
  cardx6x <- cardx6x[!(SPECIES == "   " & is.na(LENGTH))]
  cardx6x[REC_ID == "062", CWD_TYPE := "CP"]
  cardx6x[REC_ID == "063", ':='(CWD_TYPE = "CA1",
                                ALENGTH = ALENGTH/100,
                                ADEPTH = ADEPTH/100)]
  cardx6x <- cardx6x[order(CLSTR_ID, TRANSECT, ITEM_NO)]
  c6cwd <- merge(cardx6x, c6trans,
                 by = c("CLSTR_ID", "TRANSECT"))
  c6cwd[,':='(CLS_OTHX = NULL,
              PCT_CLSX = NULL,
              SEQ = NULL)]
  card064 <- cardnot[REC_ID == "064",]
  card064[, TRANSECT := substr(NOTES1A, 1, 1)]
  card064[, NOTES1A := substr(NOTES1A, 2, 128)]
  if(nrow(card064) > 0){
    note_ew <- reshape(card064,
                       varying = list(c("NOTES1A", "NOTES1B")),
                       times = c(1, 2),
                       timevar = "NOTENAME",
                       idvar = c("CLSTR_ID", "TRANSECT", "SEQ"),
                       ids = c("CLSTR_ID", "TRANSECT", "SEQ"),
                       v.names = "NOTES",
                       direction = "long")
    note_ew <- note_ew[!(NOTES %in% c("", "")),]
    note_ew <- note_ew[order(CLSTR_ID, TRANSECT, SEQ, NOTENAME),
                       .(CLSTR_ID, TRANSECT, SEQ, NOTES)]
  } else {
    note_ew <- NA
  }

  card1213 <- rbindlist(list(card121, card131),
                        fill = TRUE)
  card1213[, PLOT_CNT := substr(REC_ID, 1, 2)]
  card1213[, CREW1 := gsub(" ", "", CREW1)]
  c12_hdr <- card1213[CREW1 != "",.(CLSTR_ID, CREW1, CREW2)]

  card1213[STONESX == "---", STONES := 0]
  card1213[is.na(STONES), STONES := as.numeric(STONESX)]

  card1213[W_FLOWX == "---", W_FLOW := 0]
  card1213[is.na(W_FLOW), W_FLOW := as.numeric(W_FLOWX)]

  card1213[W_STANDX == "---", W_STAND := 0]
  card1213[is.na(W_STAND), W_STAND := as.numeric(W_STANDX)]

  card1213[BEDROCKX == "---", BEDROCK := 0]
  card1213[is.na(BEDROCK), BEDROCK := as.numeric(BEDROCKX)]
  c12becz <- card1213[,.(CLSTR_ID, PLOT_CNT, BEC_P, BEC_VAR, BSBZONE, BZONE)]
  c12soil <- card1213[,.(CLSTR_ID, PLOT_CNT, W_TABLE, MOTTLE, ROOT_RES, BEDROCK2,
                         FROZEN, CARBON, R1, R2, SOIL_NA)]
  ecoa <- card1213[,.(CLSTR_ID, PLOT_CNT, ASPECT, BEDROCK, ELEV, FLOOD,
                      GUL_BET, GUL_PLOT, HUMUS, MESO, SF_BET, SF_PLOT,
                      SLOPE, SOIL_COL, STONES, SURFACE, SURF_1, SURF_2,
                      TOPO, UFORMITY, W_FLOW, W_STAND)]

  card122[, PLOT_CNT := substr(REC_ID, 1, 2)]

  card122[sub("\\s+$", "", LOCAT) %in%  c("PLOT CENTRE", "P"),
          plot_new := 1]
  card122[sub("\\s+$", "", LOCAT) %in%  c("SITE TWO", "2"),
          plot_new := 2]
  card122[sub("\\s+$", "", LOCAT) %in%  c("SITE THREE", "3"),
          plot_new := 3]
  setnames(card122, c("SMR", "SNR", "SERNO", "SER_PER",
                      paste("LAND_", 1:5, sep = "")),
           c("MOIST", "NUTR", "SERNO", "SERPER",
             paste("L", 1:5, sep = "")))
  card122 <- card122[,.(CLSTR_ID, MOIST, NUTR, SERNO, SERNO,
                        SERPER, L1, L2, L3, L4, L5, PLOT_CNT, plot_new)]
  if(nrow(card122) > 0){
    card122_wide <- reshape(data = card122,
                            v.names = c("MOIST", "NUTR", "SERNO", "SERPER",
                                        paste("L", 1:5, sep = "")),
                            timevar = "plot_new",
                            idvar = "CLSTR_ID",
                            direction = "wide",
                            sep = "_")
    mnss <- names(card122_wide)
    mnss_needed <- c("CLSTR_ID", "PLOT_CNT", "MOIST_1", "MOIST_2", "MOIST_3",
                     "NUTR_1", "NUTR_2", "NUTR_3", "SERNO_1", "SERNO_2",
                     "SERNO_3", "SERPER_1", "SERPER_2", "SERPER_3")
    mnss_needed <- mnss_needed[mnss_needed %in% mnss]

    ecob <- card122_wide[,mnss_needed, with = FALSE]
    rm(mnss_needed)

    mnss_needed <- c("CLSTR_ID", "PLOT_CNT", paste("L", 1:5, "_1", sep = ""),
                     paste("L", 1:5, "_2", sep = ""),
                     paste("L", 1:5, "_3", sep = ""))
    mnss_needed <- mnss_needed[mnss_needed %in% mnss]

    c12bclnd <- card122_wide[, mnss_needed, with = FALSE]
    c12eco <- merge(ecoa, ecob,
                    by = "CLSTR_ID")
  } else {
    c12eco <- ecoa
  }


  c12horz <- rbindlist(list(card132, card133),
                       fill = TRUE)
  c12horz[!(substr(HORIZON, 1, 1) %in% c("F", "H", "L")),
          DEPTH := (-1)*DEPTH]
  c12horz[HORIZON == "DOP", HORIZON := NA]
  c12horz[,PLOT_CNT := substr(REC_ID, 1, 2)]
  c12horz <- c12horz[order(CLSTR_ID, PLOT_CNT, -DEPTH, -HORIZON),
                     .(CLSTR_ID, PLOT_CNT, HORIZON, DEPTH, TEXTURE,
                       TOTAL, GRAVEL, COBBLES)]

  noteep <- cardnot[REC_ID %in% c("125", "134"),]
  if(nrow(noteep) > 0){
    noteep[, ':='(NOTES1A = paste(NOTES1A, collapse = " "),
                  NOTES1B = paste(NOTES1B, collapse = " ")),
           by = c("CLSTR_ID", "SEQ")]
    noteep <- unique(noteep,
                     by = c("CLSTR_ID", "SEQ"))
    noteep <- reshape(noteep,
                      varying = list(c("NOTES1A", "NOTES1B")),
                      times = c(1, 2),
                      timevar = "NOTENAME",
                      idvar = c("CLSTR_ID", "SEQ"),
                      ids = c("CLSTR_ID", "SEQ"),
                      v.names = "NOTES",
                      direction = "long")
    noteep <- noteep[!(NOTES %in% c("", " ")),]

    noteep <- noteep[order(CLSTR_ID, SEQ, NOTENAME),
                     .(CLSTR_ID, SEQ, NOTES)]

  } else {
    noteep <- NA
  }
  setnames(card142, "SPECLONG", "SPECIES")

  for(i in c("A", "B1", "B2")){
    if(i == "A"){
      card142_all <- data.table::copy(card142[,':='(LYR_CD = i,
                                                    CVR = unlist(card142[, paste("CVR_", i, sep = ""),
                                                                         with = FALSE]),
                                                    AVG_HT = as.numeric(NA))])
      set(card142, , c("LYR_CD", "CVR", "AVG_HT"), NULL)
    } else {
      card142_all <- rbind(card142_all, card142[,':='(LYR_CD = i,
                                                      CVR = unlist(card142[, paste("CVR_", i, sep = ""),
                                                                           with = FALSE]),
                                                      AVG_HT = unlist(card142[,paste("HT_", i, sep = ""),
                                                                              with = FALSE]))])
      set(card142, , c("LYR_CD", "CVR", "AVG_HT"), NULL)
    }
  }
  rm(i)
  card142_all[, ':='(CVR = trimws(CVR, which = "both"),
                     PLOT = "I")]
  card142_all[, FACTR := substr(CVR, nchar(CVR), nchar(CVR))]
  card142_all[FACTR %in% c("H", "T"), CVR := substr(CVR, 1, (nchar(CVR)-1))]

  card142_all[FACTR == "H", CVR_PCT := as.numeric(CVR)/100]
  card142_all[FACTR == "T", CVR_PCT := as.numeric(CVR)/1000]
  card142_all[is.na(CVR_PCT), CVR_PCT := as.numeric(CVR)/10]
  c14veg <- card142_all[CVR_PCT > 0,
                        .(CLSTR_ID, PLOT, ITEM_NO, SPECIES, LYR_CD,
                          CVR_PCT, AVG_HT)]
  plot_14 <- card142_all[,.(CLSTR_ID, AB_RAD, AB_SHP)]
  plot_14 <- plot_14[!duplicated(plot_14),]

  dh_dw_dr <- card142_all[LYR_CD == "A",.(CLSTR_ID, PLOT, ITEM_NO, SPECIES,
                                          SOIL_DH, WOOD_DW, ROCK_DR)]
  rm(card142_all)

  card1414 <- merge(card141, plot_14, by = "CLSTR_ID")
  if(nrow(card1414) > 0){
    for(i in c("A", "B1", "B2")){
      if(i == "A"){
        c14hdr <- card1414[,.(CLSTR_ID, RADIUS = AB_RAD, LYR_CD = i,
                              CVR_PCT = as.numeric(LYR_A)/10)]
      } else if (i == "B1") {
        c14hdr <- rbindlist(list(c14hdr,
                                 card1414[,.(CLSTR_ID, RADIUS = AB_RAD, LYR_CD = i,
                                             CVR_PCT = as.numeric(LYR_B1)/10)]))
      } else {
        c14hdr <- rbindlist(list(c14hdr,
                                 card1414[,.(CLSTR_ID, RADIUS = AB_RAD, LYR_CD = i,
                                             CVR_PCT = as.numeric(LYR_B2)/10)]))
      }
    }
    c14hdr <- c14hdr[CVR_PCT > 0,]
  } else {
    c14hdr <- data.table(LYR_CD = c("A", "B1", "B2"),
                         CVR_PCT = as.numeric(NA))
  }
  ceth_hdr <- card1414[,.(CLSTR_ID, CREW1, CREW2)]

  cd_eti <- data.table::copy(card142)
  setnames(cd_eti, c("CVR_A", "CVR_B1", "CVR_B2"),
           c("x1", "x2", "x3"))
  cd_eti[,':='(PLOT = "I",
               CVR_A = fac_ht(x1),
               CVR_B1 = fac_ht(x2),
               CVR_B2 = fac_ht(x3),
               MOSS_DH = fac_ht(SOIL_DH),
               MOSS_DW = fac_ht(WOOD_DW),
               MOSS_DR = fac_ht(ROCK_DR))]
  cd_eti <- cd_eti[,.(CLSTR_ID, PLOT, SPECIES, ITEM_NO, S1, S2, S3, HT_B1, HT_B2,
                      CVR_A, CVR_B1, CVR_B2, MOSS_DH, MOSS_DW, MOSS_DR,
                      AB_RAD, AB_SHP, D_RAD, D_SHP)]
  cd_eti[, SPECIES := gsub('[0-9]+', '', SPECIES)]

  card143 <- cardnot[REC_ID == "143"]
  if(nrow(card143) > 0){
    card143[, ':='(NOTES1A = paste(NOTES1A, collapse = " "),
                   NOTES1B = paste(NOTES1B, collapse = " ")),
            by = c("CLSTR_ID", "SEQ")]

    noteet <- reshape(card143,
                      varying = list(c("NOTES1A", "NOTES1B")),
                      times = c(1, 2),
                      timevar = "NOTENAME",
                      idvar = c("CLSTR_ID", "SEQ"),
                      ids = c("CLSTR_ID", "SEQ"),
                      v.names = "NOTES",
                      direction = "long")
    noteet <- noteet[!(NOTES %in% c("", " ")),]

    noteet <- noteet[order(CLSTR_ID, SEQ, NOTENAME),
                     .(CLSTR_ID, SEQ, NOTES)]

  } else {
    noteet <- NA
  }
  cd_eth <- data.table::copy(card141)
  setnames(cd_eth, c("LYR_A", "LYR_B1", "LYR_B2"),
           c("x_a", "x_b1", "x_b2"))
  cd_eth[,':='(PLOT = PL,
               LYR_A = fac_ht(x_a),
               LYR_B1 = fac_ht(x_b1),
               LYR_B2 = fac_ht(x_b2))]
  set(cd_eth, , c("REC_ID", "PAGE_NO", "SEQ", "MEAS_DTC",
                  "NO_PAGE", "x_a", "x_b1", "x_b2"), NULL)

  cd_ehi <- data.table::copy(card152)
  setnames(cd_ehi, c("SPECLONG", "MOSS_DH", "MOSS_DW", "MOSS_DR", "HERB"),
           c("SPECIES", "M_DH", "M_DW", "M_DR", "HB"))
  if(nrow(cd_ehi) > 0){
    cd_ehi <- cd_ehi[,.(CLSTR_ID,
                        PLOT = "I",
                        ITEM_NO,
                        SPECIES, S1, S2, S3,
                        MOSS_DH = fac_ht(M_DH),
                        MOSS_DW = fac_ht(M_DW),
                        MOSS_DR = fac_ht(M_DR),
                        HERB = fac_ht(HB))]
  } else {
    cd_ehi <- cd_ehi[,.(CLSTR_ID,
                        PLOT = character(),
                        ITEM_NO,
                        SPECIES, S1, S2, S3,
                        MOSS_DH = fac_ht(M_DH),
                        MOSS_DW = fac_ht(M_DW),
                        MOSS_DR = fac_ht(M_DR),
                        HERB = fac_ht(HB))]
  }
  cd_ehi[, SPECIES := gsub('[0-9]+', '', SPECIES)]
  cd_ehh <- card151[,.(CLSTR_ID, PLOT, RADIUS,
                       IMOSS_DH = fac_ht(IMS_DH),
                       IMOSS_DW = fac_ht(IMS_DW),
                       IMOSS_DR = fac_ht(IMS_DR),
                       IHERB = fac_ht(IHB))]
  card153 <- cardnot[REC_ID == "153",]
  if(nrow(card153) > 0){
    note_eh <- reshape(card153,
                       varying = list(c("NOTES1A", "NOTES1B")),
                       times = c(1, 2),
                       timevar = "NOTENAME",
                       idvar = c("CLSTR_ID", "SEQ"),
                       ids = c("CLSTR_ID", "SEQ"),
                       v.names = "NOTES",
                       direction = "long")
    note_eh <- note_eh[!(NOTES %in% c("", " ")),]

    note_eh <- note_eh[order(CLSTR_ID, SEQ, NOTENAME),
                       .(CLSTR_ID, SEQ, NOTES)]

  } else {
    note_eh <- NA
  }

  card_eo <- data.table::copy(card161)
  set(card_eo, , c("REC_ID", "PAGE_NO", "SEQ", "NO_PAGES",
                   "MEAS_DTC", "CREW1", "CREW2", "PLOT"),
      NULL)
  ceo_hdr <- card161[,.(CLSTR_ID, CREW1, CREW2)]
  card162 <- cardnot[REC_ID == "162",]
  if(nrow(card162) > 0){
    note_eo <- reshape(card162,
                       varying = list(c("NOTES1A", "NOTES1B")),
                       times = c(1, 2),
                       timevar = "NOTENAME",
                       idvar = c("CLSTR_ID", "SEQ"),
                       ids = c("CLSTR_ID", "SEQ"),
                       v.names = "NOTES",
                       direction = "long")
    note_eo <- note_eo[!(NOTES %in% c("", " ")),]
    note_eo <- note_eo[order(CLSTR_ID, SEQ, NOTENAME),
                       .(CLSTR_ID, SEQ, NOTES)]
  } else {
    note_eo <- NA
  }


  cardg[,':='(SPECIES = trimws(toupper(SPECIES), which = "both"),
              WL_WOOD = as.numeric(WL_WOOD))]
  cardg[PCT_SNX == "--", PCT_SND := 100]
  cardg[is.na(PCT_SND), PCT_SND := as.numeric(PCT_SNX)]
  cardg[, PCT_SNX := NULL]

  cardeorg <- data.table::copy(carde)
  carde <- rbindlist(list(cardeorg[,.(CLSTR_ID, PL_ORIG = "SML_TR", PLOT, F_RAD,
                                      F_FULL, F_HALF, F_QRTR, F_BDRY, F_SPLT)],
                          cardeorg[,.(CLSTR_ID, PL_ORIG = "STUMP", PLOT,
                                      F_RAD = ST_RAD,
                                      F_FULL = ST_FULL,
                                      F_HALF = ST_HALF,
                                      F_QRTR = ST_QRTR,
                                      F_BDRY = ST_BDRY,
                                      F_SPLT)]))
  rm(cardeorg)

  cardf[, ':='(REC_ID = NULL,
               PAGE_NO = NULL,
               SEQ = NULL)]

  plot_connect <- cardb[,.(REC_ID, CLSTR_ID, PAGE_NO, PLOT)]
  plot_connect[REC_ID == "081", REC_ID := "082"]
  plot_connect[REC_ID == "111", REC_ID := "114"]
  plot_connect <- plot_connect[!duplicated(plot_connect),]
  cardc[, SPECIES := toupper(trimws(SPECIES, "both"))]

  cardc <- merge(cardc, plot_connect,
                 by = c("CLSTR_ID", "REC_ID", "PAGE_NO"),
                 all.x = TRUE)
  if(nrow(cardc[is.na(PLOT),]) > 0){
    stop("timveg has format issue for plot information in line 081 and 111.")
  }

  cardc[, uniobs := 1:length(DBH)]
  cardi_add <- cardc[is.na(as.numeric(TREE_LEN)) & !is.na(as.numeric(DBH))
                     & SPECIES != ""]
  cardi <- rbindlist(list(cardi[, SPECIES := toupper(trimws(SPECIES, "both"))],
                          cardi_add[]), fill = TRUE)

  cardc <- cardc[!(uniobs %in% cardi_add$uniobs), ]
  cardc[, uniobs := NULL]
  cardi <- cardi[DBH > 0,.(CLSTR_ID, PLOT, TREE_NO, SPECIES, DBH, LV_D, S_F)]

  plot_connect[REC_ID == "082", REC_ID := "104"]
  plot_connect[REC_ID == "114", REC_ID := "113"]
  cardh <- merge(cardh, plot_connect,
                 by = c("CLSTR_ID", "REC_ID", "PAGE_NO"),
                 all.x = TRUE)
  cardh[, ':='(SPECIES = toupper(trimws(SPECIES, "both")),
               BARK_THKX = as.numeric(BARK_THKX),
               REC_ID = NULL,
               PAGE_NO = NULL,
               SEQ = NULL)]

  cardc[BARK_PEX == "--", BARK_PER := 100]
  cardc[is.na(BARK_PER), BARK_PER := as.numeric(BARK_PEX)]

  cardc[HT_BRCH_ASC == "--", HT_BRCH := as.numeric(NA)]
  cardc[HT_BRCH_ASC != "--", HT_BRCH := as.numeric(HT_BRCH_ASC)]
  cardc[, HT_PROJ := as.numeric(HT_PROX)]
  cardc[HT_PROJ <=  0.1, HT_PROJ := as.numeric(NA)]
  cardc[, DIAM_BTP := as.numeric(DIAM_BTX)]
  cardc[DIAM_BTP <=  0.1, DIAM_BTP := as.numeric(NA)]

  ## for the trees that have more than 4 logs, there is an additional line
  ## appended with same rec id and tree number
  ## the appended line can be identified by missing species codes
  cardc_basic <- unique(cardc[SPECIES != "",.(CLSTR_ID, PLOT, TREE_NO, SPECIES, LV_D, S_F,
                                              CR_CL, DBH, TREE_LEN, MEASEST1, MEASEST2,
                                              DIAM_BTP, HT_PROJ, HT_BRCH, BARK_PER,
                                              WL_APPEA, WL_CROWN, WL_BARK, WL_WOOD, WL_LICHE,
                                              WL_USE, S1, S2, S3)],
                        by = c("CLSTR_ID", "PLOT", "TREE_NO"))
  cardc_basic[substr(CLSTR_ID, 1, 4) == "0402", HT_BRCH := HT_BRCH/10]

  cardc_log <- cardc[,.(CLSTR_ID, PLOT, TREE_NO, SPECIES,
                        LEN1, LEN2, LEN3, LEN4,
                        LOG1, LOG2, LOG3, LOG4,
                        LSND_1, LSND_2, LSND_3, LSND_4)]
  cardc_log1 <- cardc_log[SPECIES != "",]
  cardc_log2 <- cardc_log[SPECIES == "",.(CLSTR_ID, PLOT, TREE_NO,
                                          LEN5 = LEN1, LEN6 = LEN2,
                                          LEN7 = LEN3, LEN8 = LEN4,
                                          LOG5 = LOG1, LOG6 = LOG2,
                                          LOG7 = LOG3, LOG8 = LOG4,
                                          LSND_5 = LSND_1, LSND_6 = LSND_2,
                                          LSND_7 = LSND_3, LSND_8 = LSND_4)]
  cardc_log <- merge(cardc_log1, cardc_log2,
                     by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                     all = TRUE)
  rm(cardc_log1, cardc_log2)
  for(i in 1:8){
    cardc_log[, paste("LOG", i, "_LEN", sep = "") := as.numeric(unlist(cardc_log[,paste("LEN", i, sep = ""), with = FALSE]))]
    cardc_log[, paste("LOG", i, "_GRD", sep = "") := unlist(cardc_log[,paste("LOG", i, sep = ""), with = FALSE])]

    cardc_log[, tempcol := unlist(cardc_log[,paste("LSND_", i, sep = ""), with = FALSE])]
    cardc_log[trimws(tempcol, "both") %in% c("-", "--"), tempcol2 := 100]
    cardc_log[is.na(tempcol2), tempcol2 := as.numeric(tempcol)]
    cardc_log[, paste("LOG", i, "_SND", sep = "") := tempcol2]
    cardc_log[, ':='(tempcol = NULL,
                     tempcol2 = NULL)]
  }
  rm(i)
  set(cardc_log, , c(paste("LEN", 1:8, sep = ""),
                     paste("LOG", 1:8, sep = ""),
                     paste("LSND_", 1:8, sep = ""),
                     "SPECIES"),
      NULL)
  cardc_final <- merge(cardc_basic,
                       cardc_log,
                       by = c("CLSTR_ID", "PLOT", "TREE_NO"))
  cardc_final[grepl("\\d", S2) == TRUE,
              SECTOR := S2]
  cardc_final[, ':='(SUMLEN = 0,
                     NO_LOGS = as.numeric(NA))]
  cardc_new <- cardc_final[0,]
  for(i in 1:8){
    cardc_final[, ':='(templength = unlist(cardc_final[, paste("LOG", i, "_LEN", sep = ""),
                                                       with = FALSE]))]
    cardc_final[, ':='(tempgrade = unlist(cardc_final[, paste("LOG", i, "_GRD", sep = ""),
                                                      with = FALSE]))]
    cardc_final[, templength2 := templength]
    cardc_final[templength == 99 | is.na(templength), templength2 := 0]
    cardc_final[, SUMLEN := SUMLEN + templength2]
    cardc_final[templength == 99 | tempgrade == "N", lastlog := TRUE]
    cardc_final[lastlog == TRUE & (DIAM_BTP > 0 | HT_PROJ > 0), btoptree := TRUE]
    if(i == 1){
      cardc_final[btoptree == TRUE, ':='(LOG1_LEN = TREE_LEN,
                                         NO_LOGS = 1)]
    } else {
      cardc_final[btoptree == TRUE,
                  prevlen := unlist(cardc_final[btoptree == TRUE, paste("LOG", i-1, "_LEN", sep = ""),
                                                with = FALSE])]
      cardc_final[is.na(prevlen), prevlen := 0]
      cardc_final[btoptree == TRUE, paste("LOG", i-1, "_LEN", sep = "") := TREE_LEN - SUMLEN + prevlen]
      cardc_final[btoptree == TRUE, paste("LOG", i, "_LEN", sep = "") := 0]
      cardc_final[btoptree == TRUE, ':='(NO_LOGS = i)]
      cardc_final[, ':='(prevlen = NULL)]
    }
    cardc_final[lastlog == TRUE & is.na(btoptree),
                paste("LOG", i, "_LEN", sep = "") := TREE_LEN - SUMLEN]
    cardc_final[lastlog == TRUE & is.na(btoptree),
                NO_LOGS := i]
    cardc_final[,':='(templength = NULL,
                      templength2 = NULL,
                      tempgrade = NULL,
                      lastlog = NULL,
                      btoptree = NULL)]
    donerows <- cardc_final[!is.na(NO_LOGS),]
    set(donerows, , paste("LOG", (i+1):8, "_LEN", sep = ""), NULL)
    cardc_new <- rbindlist(list(cardc_new, donerows), fill = TRUE)
    rm(donerows)
    cardc_final <- cardc_final[is.na(NO_LOGS)]
  }
  cardc <- rbindlist(list(cardc_final, cardc_new))
  rm(cardc_final, i, cardc_log, cardc_new)
  ### this part is in cv_vri.sas
  cardc <- cardc[DBH >= 4,]
  cardc[, LOG1_GRD := trimws(LOG1_GRD)]
  cardc[is.na(NO_LOGS), needchange := TRUE]
  cardc[needchange == TRUE,
        ':='(NO_LOGS = 1,
             LOG1_SND = 100,
             LOG1_LEN = TREE_LEN)] # a flag for H enhanced trees
  cardc[needchange == TRUE & LOG1_GRD == "", LOG1_GRD := "*"]
  cardc[needchange == TRUE & (DIAM_BTP > 0 | HT_PROJ > 0),
        ':='(LOG2_GRD = "*",
             LOG2_LEN = 0,
             LOG2_SND = 0,
             NO_LOGS = 2)]
  cardc[, needchange := NULL]
  cardc[, LV_D := trimws(LV_D, "both")]
  cardc[LV_D == "", LV_D := "L"]
  cardc[,SUMLEN := NULL]



  ### so far, the H enhanced tree have not been identified
  cardd <- rbindlist(list(cardd1, cardd2), fill = TRUE)
  plot_connect[REC_ID == "104", REC_ID := "092"]
  plot_connect[REC_ID == "113", REC_ID := "115"]
  cardd <- merge(cardd, plot_connect,
                 by = c("REC_ID", "CLSTR_ID", "PAGE_NO"),
                 all.x = TRUE)

  cardd <- cardd[order(CLSTR_ID, PLOT, TREE_NO, SEQ),]
  cardd[, seq_new := 1:length(SEQ),
        by = c("CLSTR_ID", "PLOT", "TREE_NO")]
  for(i in c(paste("LSLN1", 1:3, sep = ""),
             paste("LSS", 1:3, sep = ""),
             paste("LSLN2", 1:3, sep = ""),
             paste("LSLNF", 1:3, sep = ""),
             paste("LSLNT", 1:3, sep = ""),
             paste("LSFQ", 1:3, sep = ""),
             paste("SEV", 1:2, sep = ""),
             paste("DAM_AGN", 1:2, sep = ""))){
    cardd[, tempcol := cardd[, i, with = FALSE]]
    cardd[trimws(tempcol) == "", tempcol := NA]
    cardd[, c(i) := tempcol]
    cardd[, tempcol := NULL]
  }
  carddx <- cardd[seq_new == 1,.(CLSTR_ID, PLOT, TREE_NO, AZIMUTH, DISTANCE)]
  for(i in 1:3){
    carddx_app <- cardd[seq_new == i,c("CLSTR_ID", "PLOT", "TREE_NO",
                                       paste("LSLN1", 1:3, sep = ""),
                                       paste("LSS", 1:3, sep = ""),
                                       paste("LSLN2", 1:3, sep = ""),
                                       paste("LSLNF", 1:3, sep = ""),
                                       paste("LSLNT", 1:3, sep = ""),
                                       paste("LSFQ", 1:3, sep = ""),
                                       paste("SEV", 1:2, sep = ""),
                                       paste("DAM_AGN", 1:2, sep = ""))]
    setnames(carddx_app,
             c(paste("LSLN1", 1:3, sep = ""),
               paste("LSS", 1:3, sep = ""),
               paste("LSLN2", 1:3, sep = ""),
               paste("LSLNF", 1:3, sep = ""),
               paste("LSLNT", 1:3, sep = ""),
               paste("LSFQ", 1:3, sep = ""),
               paste("SEV", 1:2, sep = ""),
               paste("DAM_AGN", 1:2, sep = "")),
             c(paste("F_SIGN", (1:3)+3*(i - 1), sep = ""),
               paste("LOSS", (1:3)+3*(i - 1), "_IN", sep = ""),
               paste("T_SIGN", (1:3)+3*(i - 1), sep = ""),
               paste("LOC", (1:3)+3*(i-1), "_FRO", sep = ""),
               paste("LOC", (1:3)+3*(i-1), "_TO", sep = ""),
               paste("FREQ", (1:3)+3*(i-1), sep = ""),
               paste("SEV_", LETTERS[(1:2)+2*(i-1)], sep = ""),
               paste("DAM_AGN", LETTERS[(1:2)+2*(i-1)], sep = "")))
    carddx <- merge(carddx, carddx_app,
                    by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                    all.x = TRUE)
    rm(carddx_app)
  }
  cardd <- carddx
  rm(carddx)

  tree_nt1 <- cardnot[REC_ID %in% c("083", "093", "105", "116")]
  tree_nt1 <- tree_nt1[,.(NOTES1A = paste(NOTES1A, collapse = " "),
                          NOTES1B = paste(NOTES1B, collapse = " ")),
                       by = c("CLSTR_ID", "SEQ")]
  note_tre <- reshape(tree_nt1,
                      varying = list(c("NOTES1A", "NOTES1B")),
                      times = c(1, 2),
                      timevar = "NOTENAME",
                      idvar = c("CLSTR_ID", "SEQ"),
                      ids = c("CLSTR_ID", "SEQ"),
                      v.names = "NOTES",
                      direction = "long")
  note_tre[, NOTES := trimws(NOTES, "both")]
  note_tre <- note_tre[!(NOTES %in% c("", " ")),]

  note_tre <- note_tre[order(CLSTR_ID, SEQ, NOTENAME),
                       .(CLSTR_ID, SEQ, NOTES)]

  if(saveThem){
    saveRDS(cardi, file.path(savePath, "cardi_ascii.rds"))
    saveRDS(cardc, file.path(savePath, "cardc_ascii.rds"))
    saveRDS(note_tre, file.path(savePath, "note_tre_ascii.rds"))
    saveRDS(cardd, file.path(savePath, "cardd_ascii.rds"))
    saveRDS(cardh, file.path(savePath, "cardh_ascii.rds"))
    saveRDS(cardf, file.path(savePath, "cardf_ascii.rds"))
    saveRDS(carde, file.path(savePath, "carde_ascii.rds"))
    saveRDS(cardg, file.path(savePath, "cardg_ascii.rds"))
    saveRDS(cardb, file.path(savePath, "cardb_ascii.rds"))
    saveRDS(ceo_hdr, file.path(savePath, "ceo_hdr_ascii.rds"))
    saveRDS(note_eo, file.path(savePath, "note_eo_ascii.rds"))
    saveRDS(card_eo, file.path(savePath, "card_eo_ascii.rds"))
    saveRDS(note_eh, file.path(savePath, "note_eh_ascii.rds"))
    saveRDS(cd_ehh, file.path(savePath, "cd_ehh_ascii.rds"))
    saveRDS(cd_ehi, file.path(savePath, "cd_ehi_ascii.rds"))
    saveRDS(cd_eth, file.path(savePath, "cd_eth_ascii.rds"))
    saveRDS(noteet, file.path(savePath, "note_et_ascii.rds"))
    saveRDS(cd_eti, file.path(savePath, "cd_eti_ascii.rds"))
    saveRDS(ceth_hdr, file.path(savePath, "ceth_hdr_ascii.rds"))
    saveRDS(noteep, file.path(savePath, "note_ep_ascii.rds"))
    saveRDS(c12horz, file.path(savePath, "c12horz_ascii.rds"))
    saveRDS(c12eco, file.path(savePath, "c12eco_ascii.rds"))
    saveRDS(c12soil, file.path(savePath, "c12soil_ascii.rds"))
    saveRDS(c12_hdr, file.path(savePath, "c12_hdr_ascii.rds"))
    saveRDS(c12becz, file.path(savePath, "c12becz_ascii.rds"))
    saveRDS(note_ew, file.path(savePath, "note_ew_ascii.rds"))
    saveRDS(c6cwd, file.path(savePath, "c6cwd_ascii.rds"))
    saveRDS(cwd_hdr, file.path(savePath, "cwd_hdr_ascii.rds"))
    saveRDS(c6trans, file.path(savePath, "c6trans_ascii.rds"))
    saveRDS(note_rs, file.path(savePath, "note_rs_ascii.rds"))
    saveRDS(rng_hdr, file.path(savePath, "rng_hdr_ascii.rds"))
    saveRDS(c4forage, file.path(savePath, "c4forage_ascii.rds"))
    saveRDS(note_cl, file.path(savePath, "note_cl_ascii.rds"))
    saveRDS(cardcl, file.path(savePath, "card_cl_ascii.rds"))
    saveRDS(card_cp, file.path(savePath, "card_cp_ascii.rds"))
    saveRDS(note_cp, file.path(savePath, "note_cp_ascii.rds"))
    saveRDS(carda, file.path(savePath, "carda_ascii.rds"))
    saveRDS(note_ch, file.path(savePath, "note_ch_ascii.rds"))
    allfiles <- dir(savePath, full.names = FALSE, pattern = ".rds")
    allfiles <- gsub(".rds", "", allfiles)
    for (indifile in allfiles) {
      indidata <- readRDS(file.path(savePath, paste0(indifile, ".rds")))
      allnames <- names(indidata)
      for (indiname in allnames) {
        setnames(indidata, indiname, "tempname")
        if(class(indidata$tempname) == "character" &
           indiname != "NOTES"){
          indidata[, tempname := trimws(tempname, which = "both")]
        }
        setnames(indidata, "tempname", indiname)
      }
      saveRDS(indidata, file.path(savePath, paste0(indifile, ".rds")))
      write.xlsx(indidata, file.path(savePath, paste0(indifile, ".xlsx")),
                 overwrite = TRUE)
    }
  } else if (!saveThem){

  } else {
    stop("saveThem must be logical.")
  }

}

fac_ht <- function(a){
  thetable <- data.table(a = trimws(a, which = "both"))
  thetable[, flag := substr(a, nchar(a), nchar(a))]
  thetable[flag %in% c("H", "T"), a := substr(a, 1, (nchar(a)-1))]
  thetable[flag == "H", output := as.numeric(a)/100]
  thetable[flag == "T", output := as.numeric(a)/1000]
  thetable[!(flag %in% c("H", "T")), output := as.numeric(a)/10]
  return(thetable$output)
}

scanAllASCII <- function(txtPath){
  alltxtfiles <- dir(txtPath, full.names = TRUE)
  alltxtfiletable <- data.table::data.table(alltxtfiles = alltxtfiles)
  alltxtfiletable[, txtlength := nchar(alltxtfiles)]
  alltxtfiletable[, txtbeg := txtlength-3]
  alltxtfiletable <- alltxtfiletable[substr(alltxtfiles, txtbeg, txtlength) %in% c(".txt", ".TXT")]
  alltxtfiles_list <- lapply(alltxtfiletable$alltxtfiles,
                             function(s) readonetxt(s))
  for(i in 1:length(alltxtfiles_list)){
    if(i == 1){
      alltxtfiles <- alltxtfiles_list[[i]]
    } else {
      alltxtfiles <- rbind(alltxtfiles, alltxtfiles_list[[i]])
    }
  }
  if(length(alltxtfiletable$alltxtfiles) != length(unique(alltxtfiles$CLSTR_ID))){
    stop("Duplicated txt files for a cluster in ASCII folder.")
  }
  return(alltxtfiles)
}

readonetxt <- function(fileName){
  onefile <- data.table(LINE = readLines(fileName, skipNul = TRUE))
  onefile[,':='(REC_ID = substr(LINE, 1, 3),
                PAGE_NO = substr(LINE, 4, 5),
                SEQ = as.numeric(substr(LINE, 6, 9)))]
  sampleheader <- onefile[REC_ID == "011",
                          .(PROJ_ID = substr(LINE, 10, 13),
                            SAMP_NO = substr(LINE, 14, 17),
                            TYPE_CD = substr(LINE, 19, 21),
                            MEAS_DTC = substr(LINE, 22, 30))]
  if(nrow(sampleheader) > 1){
    stop(paste("file: ", fileName, " has multiple header line."))
  } else {
    sampleheader[, ':='(MEAS_DT = as.Date(MEAS_DTC, '%Y%b%d'),
                        CLSTR_ID = getClusterID(projID = PROJ_ID,
                                                sampleNO = SAMP_NO,
                                                sampleType = toupper(substr(TYPE_CD, 1, 1)),
                                                intent = toupper(substr(TYPE_CD, 2, 2)),
                                                visit = toupper(substr(TYPE_CD, 3, 3))))]
    measurementDate <- sampleheader$MEAS_DT
    CLSTR_ID <- sampleheader$CLSTR_ID
  }
  onefile <- onefile[,.(REC_ID, PAGE_NO, SEQ, measurementDate,
                        CLSTR_ID, LINE)]
  return(onefile)
}

parse_name <- function(fullName){
  fullName <- sub("\\s+$", "", fullName) # remove space after the last letter
  fullName <- sub("^\\s+", "", fullName) # remove space before the first letter
  firstSpace <- as.numeric(unlist(lapply(fullName,
                                         function(s) unlist(gregexpr(" ", s))[1])))
  firstSpace[firstSpace < 0] <- nchar(fullName[firstSpace < 0])+1
  firstName <- substr(fullName, 1, firstSpace-1)
  firstName[firstName == ""] <- NA
  lastName <- substr(fullName, firstSpace+1, nchar(fullName))
  lastName <- sub("^\\s+", "", lastName)
  lastName[lastName == ""] <- NA
  return(list(firstName = firstName,
              lastName = lastName))
}
