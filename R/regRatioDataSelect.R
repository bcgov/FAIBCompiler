#' Select the data for regression and ratio
#'
#' @description This function selects the data for fitting regression model between basal area and
#'              whole stem volume, and for ratios to whole stem volume in VRI compiler. The regreesion
#'              method is used for derive whole stem volume for the trees that just have DBH information.
#'              The ratio method is used to derive netted merchantable volume for the trees that
#'              do not have call grading information. The data selection should be done annually at the end of
#'              every March to allow newer and higher quaulity data enter the data.
#'
#' @param sampledata data.table, The data contains the sample level information.
#' @param alltreedata data.table, The is the data pool which contains the full/enhanced trees
#'                    and H-enhanced trees.
#' @param usage character, Specifies compiler name. It supports \code{ismc} and \code{vgis}.
#'
#'
#' @return Selected data for regression and ratio.
#'
#'
#' @importFrom data.table ':=' setnames
#'
#'
#' @export
#' @docType methods
#' @rdname regRatioDataSelect
#'
#' @author Yong Luo
#'
regRatioDataSelect <- function(sampledata, alltreedata, usage){
  if(usage == "ismc"){
    ## remove the delinearity samples also duplicated
    sampledata <- sampledata[!(PROJ_ID %in% c("037A", "DMHA", "DDCX", "DDCA", "DDCY",
                                              "DDCB", "DDCZ", "029A", "DQUE")),]
    ## remove A samples, early YSM
    ## remove NVAF samples
    ## remove B samples
    A_N_B_cls <- unique(sampledata[TYPE_CD %in% c("A", "N", "B"),]$CLSTR_ID)
    sampledata <- sampledata[!(CLSTR_ID %in% A_N_B_cls),]

    ## remove samples in the TAAN project
    tann_cls <- unique(sampledata[(PROJECT_DESCRIPTOR %in% c("TFL 60 Monitoring",
                                                      "TFL 60 Monitoring Remeasurements")),]$CLSTR_ID)
    sampledata <- sampledata[!(CLSTR_ID %in% tann_cls),]

    sampledata[, MEAS_YR_calendar := substr(MEAS_DT, 1, 4)]
    sampledata[, sameMSYear := length(CLSTR_ID),
               by = c("SITE_IDENTIFIER", "PLOT", "MEAS_YR_calendar")]
    multisamp <- sampledata[sameMSYear > 1]
    multisamp[, point_year := paste0(SITE_IDENTIFIER, "_", MEAS_YR_calendar)]
    # fixed area plot has priority
    multisamp_fix <- multisamp[TYPE_CD == "F",]
    # when for a given year, there are two fixed area visit
    # the last one wins
    # good news is there is no such case in the database
    multisamp_fix[, visit_max := max(VISIT_NUMBER), by = "point_year"]
    multisamp_fix <- multisamp_fix[VISIT_NUMBER == visit_max,]

    multisamp_var <- multisamp[!(point_year %in% multisamp_fix$point_year),]
    multisamp_var[, visit_max := max(VISIT_NUMBER), by = "point_year"]
    multisamp_var <- multisamp_var[VISIT_NUMBER == visit_max,]
    multisamp_final <- rbind(multisamp_fix, multisamp_var)
    multisamp_final[,':='(MEAS_YR_calendar = NULL,
                          sameMSYear = NULL,
                          point_year = NULL,
                          visit_max = NULL)]
    sampledata <- sampledata[sameMSYear < 2,]
    sampledata[,':='(MEAS_YR_calendar = NULL,
                     sameMSYear = NULL)]
    sampledata <- rbind(sampledata,
                        multisamp_final)
    rm(multisamp, multisamp_final, multisamp_fix, multisamp_var)
    sampledata[, uniplot := paste0(CLSTR_ID, "-", PLOT)]

    alltreedata[, uniplot := paste0(CLSTR_ID, "-", PLOT)]
    treedata_selected <- alltreedata[uniplot %in% sampledata$uniplot &
                                       MEAS_INTENSE %in% c("FULL", "ENHANCED", "H-ENHANCED"),]
    treedata_selected <- treedata_selected[BROKEN_TOP_IND %in% c("N", NA)]
    treedata_selected[, uniplot := NULL]
  } else if (usage == "vgis"){
    sampledata[, SAMP_POINT := substr(CLSTR_ID, 1, 9)]
    selectedsamples <- sampledata[0, ]
    ## remove the delinearity samples also duplicated
    sampledata <- sampledata[!(PROJ_ID %in% c("037A", "DMHA", "DDCX", "DDCA", "DDCY",
                                              "DDCB", "DDCZ", "029A", "DQUE")),]
    ## remove audit samples
    sampledata <- sampledata[!(substr(CLSTR_ID, 11, 11) == "A" |
                                 substr(CLSTR_ID, 12, 12) == "A" |
                                 substr(CLSTR_ID, 13, 13) == "A"),]

    ## deal with sample points that have NVAF samples
    ##
    nvafsamppoints <- unique(sampledata[substr(CLSTR_ID, 11, 11) == "N"]$SAMP_POINT)

    nvafselected <- unique(sampledata[SAMP_POINT %in% nvafsamppoints &
                                        substr(CLSTR_ID, 11, 11) == "N",],
                           by = c("CLSTR_ID", "PLOT"))
    nvafselected[, LASTTIME := max(VISIT_NUMBER), by = "SAMP_POINT"]
    nvafselected <- nvafselected[VISIT_NUMBER == LASTTIME & PLOT != "I",]
    nvafselected[, LASTTIME := NULL]

    selectedsamples <- rbind(selectedsamples, nvafselected)
    rm(nvafselected)

    nvafselected_forIPC <- unique(sampledata[SAMP_POINT %in% nvafsamppoints &
                                               substr(CLSTR_ID, 11, 11) != "N" &
                                               PLOT == "I",],
                                  by = c("CLSTR_ID"))

    nvafselected_forIPC_Fix <- nvafselected_forIPC[SAMP_TYP == "F",]
    nvafselected_forIPC_Fix[, LASTTIME := max(VISIT_NUMBER),
                            by = "SAMP_POINT"]
    nvafselected_forIPC_Fix <- nvafselected_forIPC_Fix[VISIT_NUMBER == LASTTIME,]
    nvafselected_forIPC_Fix[, LASTTIME := NULL]
    selectedsamples <- rbind(selectedsamples, nvafselected_forIPC_Fix)


    nvafselected_forIPC_Var <- nvafselected_forIPC[!(SAMP_POINT %in%
                                                       unique(nvafselected_forIPC_Fix$SAMP_POINT)),]
    rm(nvafselected_forIPC_Fix)

    nvafselected_forIPC_Var[, LASTTIME := max(VISIT_NUMBER),
                            by = "SAMP_POINT"]
    nvafselected_forIPC_Var <- nvafselected_forIPC_Var[VISIT_NUMBER == LASTTIME,]
    nvafselected_forIPC_Var[, LASTTIME := NULL]
    selectedsamples <- rbind(selectedsamples, nvafselected_forIPC_Var)
    ## check sample point 0031-0005
    testdata <- selectedsamples[SAMP_POINT == "0031-0005",.(CLSTR_ID, PLOT)]
    if(!identical(testdata[order(CLSTR_ID, PLOT),],
                  data.table(CLSTR_ID = c("0031-0005-NO1", "0031-0005-NO1", "0031-0005-NO1",
                                          "0031-0005-NO1", "0031-0005-QO1"),
                             PLOT = c("E", "N", "S", "W", "I")))){
      stop("Sample point with NVAF data is not correctly selected.")
    }
    rm(testdata)


    ## select the sample points that just have one sample
    sampledata <- sampledata[!(SAMP_POINT %in% nvafsamppoints), ]
    sampledata[, sample_length := length(unique(CLSTR_ID)), by = "SAMP_POINT"]

    sampledata_selected <- sampledata[sample_length == 1,]
    sampledata_selected[, sample_length := NULL]

    selectedsamples <- rbind(selectedsamples, sampledata_selected)
    rm(sampledata_selected)
    sampledata <- sampledata[sample_length != 1,]


    ## for sample point have the fixed area plot
    sampledata_fix <- sampledata[SAMP_TYP == "F",]
    sampledata_fix[, LASTTIME := max(VISIT_NUMBER), by = "SAMP_POINT"]
    sampledata_fix <- sampledata_fix[VISIT_NUMBER == LASTTIME,]
    ## going on
    sampledata_fix[, clster_length := length(unique(CLSTR_ID)), by = "SAMP_POINT"]
    if(nrow(sampledata_fix[clster_length > 1]) > 0){
      print(unique(sampledata_fix[clster_length > 1,.(CLSTR_ID, SAMP_TYP, VISIT_NUMBER, SAMP_POINT)],
                   by = "CLSTR_ID"))
      stop("Multiple monitoring samples were found for one sample point at same time.")
    }
    sampledata_fix[, ':='(LASTTIME = NULL,
                          clster_length = NULL,
                          sample_length = NULL)]
    selectedsamples <- rbind(selectedsamples, sampledata_fix)
    sampledata <- sampledata[!(SAMP_POINT %in% unique(sampledata_fix$SAMP_POINT)), ]

    ## last selection for the sample point just have multiple variable plot samples
    sampledata[, LASTTIME := max(VISIT_NUMBER), by = "SAMP_POINT"]
    sampledata <- sampledata[VISIT_NUMBER == LASTTIME,]
    sampledata[, clster_length := length(unique(CLSTR_ID)), by = "SAMP_POINT"]
    if(nrow(sampledata[clster_length > 1]) > 0){
      print(unique(sampledata_fix[clster_length > 1,.(CLSTR_ID, SAMP_TYP, VISIT_NUMBER, SAMP_POINT)],
                   by = "CLSTR_ID"))
      stop("Multiple monitoring samples were found for one sample point at same time.")
    }
    sampledata[, ':='(LASTTIME = NULL,
                      clster_length = NULL,
                      sample_length = NULL)]
    selectedsamples <- rbind(selectedsamples, sampledata)
    selectedsamples[, uniplot := paste0(CLSTR_ID, "-", PLOT)]

    alltreedata[, uniplot := paste0(CLSTR_ID, "-", PLOT)]
    treedata_selected <- alltreedata[uniplot %in% selectedsamples$uniplot &
                                       MEAS_INTENSE %in% c("FULL", "ENHANCED", "H-ENHANCED"),]
  }
  return(treedata_selected)
}
