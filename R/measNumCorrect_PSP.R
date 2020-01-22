#' Measurement number correction for raw PSP data
#'
#' @description This function is to correct the measurement number for the remeasured PSP data.
#'              The remeasured PSP data is defined as PSP that sits outside of GYS oracle database.
#'              In those remeasurement data, some measurement number has not been correctly assigned, and
#'              conficts the measurement number for the same samples in GYS oracle database.
#'
#' @param sampleID character, Specifies how the PSP sample ID.
#'
#' @param measureNumber numeric, Specifies measurement number before correction.
#'
#' @return corrected measurement number
#'
#' @importFrom data.table ':=' data.table
#' @note with confirmation by Rene
#' @author Yong Luo
#' @export
#' @docType methods
#' @rdname measNumCorrect_PSP
measNumCorrect_PSP <- function(sampleID,
                               measureNumber){
  thedata <- data.table(obsindex = 1:length(sampleID),
                        sampleID = sampleID,
                        measureNumber = measureNumber)
  #          SAMP_ID MEAS_DATE MEAS_NO
  # 1: 36002 G000003  19720602      00
  # 2: 36002 G000003  19820722      01
  # 3: 36002 G000003  19921120      02
  # 4: 36002 G000003  20150520      02
  thedata[sampleID == "36002 G000003", measureNumber := "03"]

  # 5: 55030 G000002  19930813      00
  # 6: 55030 G000002  20060922      01
  # 7: 55030 G000002  20130908      01
  thedata[sampleID == "55030 G000002", measureNumber := "02"]

  # 8: 56069 G000002  19910909      00
  # 9: 56069 G000002  20010916      01
  # 10: 56069 G000002  20130607      01
  thedata[sampleID == "56069 G000002", measureNumber := "02"]

  # 11: 59071 R000105  19260802      00
  # 12: 59071 R000105  19310817      01
  # 13: 59071 R000105  19340830      02
  # 14: 59071 R000105  19380815      03
  # 15: 59071 R000105  19430731      04
  # 16: 59071 R000105  19480717      05
  # 17: 59071 R000105  19530525      06
  # 18: 59071 R000105  19580811      07
  # 19: 59071 R000105  19630722      08
  # 20: 59071 R000105  19810715      09
  # 21: 59071 R000105  19880623      10
  # 22: 59071 R000105  20120718      10
  # 23: 59071 R000105  19980730      11
  thedata[sampleID == "59071 R000105", measureNumber := "12"]

  # 24: 59071 R000106  19260804      00
  # 25: 59071 R000106  19310907      01
  # 26: 59071 R000106  19340831      02
  # 27: 59071 R000106  19380809      03
  # 28: 59071 R000106  19430804      04
  # 29: 59071 R000106  19480720      05
  # 30: 59071 R000106  19530617      06
  # 31: 59071 R000106  19580623      07
  # 32: 59071 R000106  19630705      08
  # 33: 59071 R000106  19810811      09
  # 34: 59071 R000106  19880710      10
  # 35: 59071 R000106  20120703      10
  # 36: 59071 R000106  19980720      11
  thedata[sampleID == "59071 R000106", measureNumber := "12"]

  # 37: 59071 R000107  19260811      00
  # 38: 59071 R000107  19310908      01
  # 39: 59071 R000107  19340901      02
  # 40: 59071 R000107  19380826      03
  # 41: 59071 R000107  19430805      04
  # 42: 59071 R000107  19480724      05
  # 43: 59071 R000107  19530622      06
  # 44: 59071 R000107  19580903      07
  # 45: 59071 R000107  19630715      08
  # 46: 59071 R000107  19810711      09
  # 47: 59071 R000107  19880628      10
  # 48: 59071 R000107  20120704      10
  # 49: 59071 R000107  19980716      11
  thedata[sampleID == "59071 R000107", measureNumber := "12"]

  return(thedata[order(obsindex),]$measureNumber)
}
