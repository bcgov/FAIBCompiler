#' Site age by experienced forester
#'
#'
#' @description This function derives the site age based on experienced forester. It is equivalent to \code{vri_age} macro in original
#'              SAS compiler.
#'
#' @param projectID character,  Project ID.
#' @param sampleNumber character, Sample number.
#' @param sampleTypeCode character, Sample type code.
#'
#' @return Age provided by experienced forester
#'
#' @importFrom data.table ':=' data.table
#'
#' @note Contact Bob Krahn for details
#'
#' @export
#' @docType methods
#' @rdname ageByForester
#'
#' @author Yong Luo
ageByForester<- function(projectID, sampleNumber, sampleTypeCode){
  worktable <- data.table(uniObs = 1:(max(length(projectID), length(sampleNumber))),
                          projectID, sampleNumber, sampleTypeCode)
  worktable[(projectID == "0301" &
               sampleNumber %in% c("0003","0022","0024","0026","0045","0051",
                                   "0068", "0091","0096","0097","0110") ) |
              (projectID == "0302" &
                 sampleNumber == "0032") |
              (projectID == "0292" &
                 sampleNumber %in% c("0608", "0885", "0960")) |
              (projectID == "3232" &
                 sampleNumber %in% c("0037", "0070", "0079", "0083", "0100", "0114")) |
              (projectID == "3251" &
                 sampleNumber %in% c("0040", "5506", "5510")) |
              (projectID == "3252" &
                 sampleNumber %in% c("0515", "0518", "0540")) |
              (projectID == "3253" &
                 sampleNumber %in% c("1001", "1004", "1005", "1009", "1055", "1074", "1096")) |
              (projectID == "3431" &
                 sampleNumber == "0064") |
              (projectID == "3432" &
                 sampleNumber %in% c("0038", "0046", "0059", "0063", "0072")) |
              (projectID == "4741" &
                 sampleNumber %in% c("0028", "0141")) |
              (projectID == "DCK1" &
                 sampleNumber == "0040") |
              (projectID == "DLL1" &
                 sampleNumber %in% c("0059", "0106", "0126")) |
              (projectID == "DMH1" &
                 sampleNumber == "0016") |
              (projectID == "DMHA" &
                 sampleNumber == "0016") |
              (projectID == "DSC1" &
                 sampleNumber %in% c("0064", "0068")) |
              (projectID == "INT1" &
                 sampleNumber == "0004") |
              (projectID == "3061" &
                 sampleNumber %in% c("0067", "0099")),
            ageByForester := 125]

  worktable[projectID == "3471" & sampleTypeCode == "MO1" & sampleNumber %in% c("0046","0058","0068","0072"),
            ageByForester := 250]

  worktable[projectID == "0111" & sampleNumber == "0053",
            ageByForester := 100]

  worktable[(projectID == "0301" & sampleNumber %in% c("0028","0059","0099","0602")) |
              (projectID == "0292" & sampleNumber == "0825") |
              (projectID == "4721" & sampleNumber == "0055"),
            ageByForester := 85]

  worktable[projectID == "3431" & sampleNumber  =="0004",
            ageByForester := 55]

  worktable[projectID == "3251" & sampleNumber  %in% c("0009","0085"),
            ageByForester := 45]

  worktable[projectID == "3011" & sampleNumber  == "0057",
            ageByForester := 41]

  worktable[(projectID == "3151" & sampleNumber  == "0006") |
              (projectID == "3231" & sampleNumber  == "0200") |
              (projectID == "3253" & sampleNumber %in% c("1025","1033")) |
              (projectID == "3431" & sampleNumber  == "0035"),
            ageByForester := 40]

  worktable[(projectID == "3231" & sampleNumber  == "0167") |
              (projectID == "3251" & sampleNumber  == "0018") |
              (projectID == "3252" & sampleNumber  == "0510") |
              (projectID == "3031" & sampleNumber  == "0005") |
              (projectID == "3431" & sampleNumber  == "0055") |
              (projectID == "3471" & sampleNumber  == "0027") |
              (projectID == "LGM4" & sampleNumber  == "0561") |
              (projectID == "LGMD" & sampleNumber  == "0561"),
            ageByForester := 20]

  worktable[(projectID == "0291" & sampleNumber  == "0554") |
              (projectID == "3251" & sampleNumber  == "0026") |
              (projectID == "DDC2" & sampleNumber  == "0063"),
            ageByForester := 10]

  worktable[projectID == "3231" & sampleNumber  == "0172",
            ageByForester := 5]

  return(worktable[order(uniObs),]$ageByForester)
}
