#' Species correction, based on Rene and Dan's correction routine. See emails on
#' May 12th, 2021.
#'
#'
#' @description This function is to correct species codes
#'
#' @param species character, Specifies the original species.
#' @param BEC character, BEC zone.
#' @param BEC_subzone character, BEC subzone.
#'
#' @return corrected species code
#'
#' @importFrom data.table ':=' data.table
#'
#' @rdname speciesCorrection
#' @author Yong Luo
speciesCorrection <- function(species, BEC, BEC_subzone){
  processtable <- data.table(species, BEC, BEC_subzone)
  processtable[species == "A",
               species := "AT"]
  processtable[species == "AX",
               species := "AC"]
  processtable[species == "HX",
               species := "HW"]
  processtable[species == "C",
               species := "CW"]
  processtable[species %in% c("D", "RA"),
               species := "DR"]
  processtable[species %in% c("E", "EXP", "EA"),
               species := "EP"]
  processtable[species %in% c("J"),
               species := "JR"]
  processtable[species %in% c("L"),
               species := "LW"]
  processtable[species %in% c("P", "PLI"),
               species := "PL"]
  processtable[species %in% c("FDI", "FDC"),
               species := "FD"]
  processtable[species %in% c("SX", "SXL", "SXW"),
               species := "SW"]
  processtable[species %in% c("SXE"),
               species := "SE"]
  processtable[species %in% c("T"),
               species := "TW"]
  processtable[species %in% c("X", "XC"),
               species := "XC"]
  processtable[species %in% c("ZH"),
               species := "XH"]
  processtable[nchar(species) > 2,
               species := substr(species, 1, 2)]
  processtable[BEC %in% c("CWH", "CDF", "MH", "CMA"),
               BEC_I_C := "C"]
  processtable[is.na(BEC_I_C),
               BEC_I_C := "I"]

  # *further adjustments;
  # if species in ('S','SE') and bec_i_c = 'I' and bgc_zone not in ('ESSF') then
  # species = 'SW';
  processtable[, process := FALSE]
  processtable[BEC_I_C == "I" &
                 BEC != "ESSF" &
                 species %in% c("S", "SE"),
               ':='(species = "SW",
                    process = TRUE)]
  ## S in ESSF should be SE
  processtable[BEC == "ESSF" &
                 species %in% c("S") &
                 process == FALSE,
               ':='(species = "SE",
                    process = TRUE)]
  # else if species in ('SS') and bec_i_c = 'I' then
  # species = 'SW';
  processtable[BEC_I_C == "I" &
                 species %in% c("SS") &
                 process == FALSE,
               ':='(species = "SW",
                    process = TRUE)]
  # else if species in ('S') and bec_i_c = 'C' then
  # species = 'SS';
  processtable[BEC_I_C == "C" &
                 species %in% c("S") &
                 process == FALSE,
               ':='(species = "SS",
                    process = TRUE)]
  # else if species in ('B','BA') and bec_i_c = 'I' then
  # species = 'BL';
  processtable[BEC_I_C == "I" &
                 species %in% c("B", "BA") &
                 process == FALSE,
               ':='(species = "BL",
                    process = TRUE)]

  # else if species in ('B','BL') and bec_i_c = 'C' and bgc_zone in (‘CWH') and bgc_subzone in (‘mm') then
  # species = 'BG';
  # ‘B’ and ‘BL’ are not legitimate species on the coast and should be changed ‘BG’ in the CWHmm
  # and ‘BA’ in all other coast BGC units. From Dan.
  processtable[BEC %in% c("CWH") &
                 BEC_subzone == "mm" &
                 species %in% c("B", "BL") &
                 process == FALSE,
               ':='(species = "BG",
                    process = TRUE)]

  processtable[BEC_I_C == "C" &
                 species %in% c("B", "BL") &
                 process == FALSE,
               ':='(species = "BA",
                    process = TRUE)]

  # else if species in ('BG') and bec_i_c = 'I' and bgc_zone not in ('ICH') then
  # species = 'BL';
  processtable[BEC_I_C == "I" &
                 BEC != "ICH" &
                 species %in% c("BG") &
                 process == FALSE,
               ':='(species = "BL",
                    process = TRUE)]

  # else if species in ('BG') and bec_i_c = 'C' and bgc_zone not in ('CWH','CDF') then
  # species = 'BA';
  processtable[BEC_I_C == "C" &
                 !(BEC %in% c("CWH", "CDF")) &
                 species %in% c("BG") &
                 process == FALSE,
               ':='(species = "BA",
                    process = TRUE)]

  # else if species in ('H','HXM') and bec_i_c = 'C' and bgc_zone in ('MH') then
  # species = 'HM';
  processtable[BEC == "MH" &
                 species %in% c("H", "HXM") &
                 process == FALSE,
               ':='(species = "HM",
                    process = TRUE)]

  # else if species in ('H','HXM') and bec_i_c = 'C' and bgc_zone not in ('MH') then
  # species = 'HW';
  processtable[BEC_I_C == "C" &
                 species %in% c("H", "HXM") &
                 process == FALSE,
               ':='(species = "HW",
                    process = TRUE)]

  # else if species in ('VP') and bec_i_c = 'C' then
  # species = 'VB';
  processtable[BEC_I_C == "C" &
                 species %in% c("VP") &
                 process == FALSE,
               ':='(species = "VB",
                    process = TRUE)]
  return(processtable$species)
}
