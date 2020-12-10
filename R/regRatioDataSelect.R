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
    ## in ismc, the measurement time is defined as visit number
    sampledata[, SAMP_POINT := as.numeric(substr(CLSTR_ID, 1, 7))]
    sampledata[, visit_no := 1]
    sampledata[substr(CLSTR_ID, 10, 10) == "R",
                   visit_no_add := as.numeric(substr(CLSTR_ID, 11, 11))]
    sampledata[substr(CLSTR_ID, 10, 10) == "R", visit_no := visit_no + visit_no_add]
    sampledata[, MEAS_DT := visit_no]
    sampledata[,':='(visit_no = NULL,
                     visit_no_add = NULL)]

    selectedsamples <- sampledata[0, ]
    ## remove the delinearity samples also duplicated
    sampledata <- sampledata[!(PROJ_ID %in% c("037A", "DMHA", "DDCX", "DDCA", "DDCY",
                                              "DDCB", "DDCZ", "029A", "DQUE")),]
    ## remove audit samples
    sampledata <- sampledata[!(substr(CLSTR_ID, 9, 9) == "A" |
                                 substr(CLSTR_ID, 10, 10) == "A" |
                                 substr(CLSTR_ID, 11, 11) == "A"),]

    ## deal with sample points that have NVAF samples
    ##
    nvafsamppoints <- unique(sampledata[substr(CLSTR_ID, 9, 9) == "N"]$SAMP_POINT)
    nvafselected <- unique(sampledata[SAMP_POINT %in% nvafsamppoints &
                                        substr(CLSTR_ID, 9, 9) == "N",],
                           by = c("CLSTR_ID", "PLOT"))
    nvafselected[, LASTTIME := max(MEAS_DT), by = "SAMP_POINT"]
    nvafselected <- nvafselected[MEAS_DT == LASTTIME & PLOT != "I",]
    nvafselected[, LASTTIME := NULL]

    selectedsamples <- rbind(selectedsamples, nvafselected)
    rm(nvafselected)

    nvafselected_forIPC <- unique(sampledata[SAMP_POINT %in% nvafsamppoints &
                                               substr(CLSTR_ID, 9, 9) != "N" &
                                               PLOT == "I",],
                                  by = c("CLSTR_ID"))

    nvafselected_forIPC_Fix <- nvafselected_forIPC[SAMP_TYP == "F",]
    nvafselected_forIPC_Fix[, LASTTIME := max(MEAS_DT),
                            by = "SAMP_POINT"]
    nvafselected_forIPC_Fix <- nvafselected_forIPC_Fix[MEAS_DT == LASTTIME,]
    nvafselected_forIPC_Fix[, LASTTIME := NULL]
    selectedsamples <- rbind(selectedsamples, nvafselected_forIPC_Fix)


    nvafselected_forIPC_Var <- nvafselected_forIPC[!(SAMP_POINT %in%
                                                       unique(nvafselected_forIPC_Fix$SAMP_POINT)),]
    rm(nvafselected_forIPC_Fix)

    nvafselected_forIPC_Var[, LASTTIME := max(MEAS_DT),
                            by = "SAMP_POINT"]
    nvafselected_forIPC_Var <- nvafselected_forIPC_Var[MEAS_DT == LASTTIME,]
    nvafselected_forIPC_Var[, LASTTIME := NULL]
    selectedsamples <- rbind(selectedsamples, nvafselected_forIPC_Var)
    ## check sample point 0031-0005
    testdata <- selectedsamples[SAMP_POINT == "0031-0005",.(CLSTR_ID, PLOT)]
    if(!identical(testdata[order(CLSTR_ID, PLOT),],
                  data.table(CLSTR_ID = c("0031-0005-NR1", "0031-0005-NR1", "0031-0005-NR1",
                                          "0031-0005-NR1", "0031-0005-QO1"),
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
    sampledata_fix[, LASTTIME := max(MEAS_DT), by = "SAMP_POINT"]
    sampledata_fix <- sampledata_fix[MEAS_DT == LASTTIME,]
    sampledata_fix <- sampledata_fix[CLSTR_ID != "4742-0104-FO1",] ## need to remove when figure out what is
    ## going on
    sampledata_fix[, clster_length := length(unique(CLSTR_ID)), by = "SAMP_POINT"]
    if(nrow(sampledata_fix[clster_length > 1]) > 0){
      print(unique(sampledata_fix[clster_length > 1,.(CLSTR_ID, SAMP_TYP, MEAS_DT, SAMP_POINT)],
                   by = "CLSTR_ID"))
      stop("Multiple monitoring samples were found for one sample point at same time.")
    }
    sampledata_fix[, ':='(LASTTIME = NULL,
                          clster_length = NULL,
                          sample_length = NULL)]
    selectedsamples <- rbind(selectedsamples, sampledata_fix)
    sampledata <- sampledata[!(SAMP_POINT %in% unique(sampledata_fix$SAMP_POINT)), ]

    ## last selection for the sample point just have multiple variable plot samples
    sampledata[, LASTTIME := max(MEAS_DT), by = "SAMP_POINT"]
    sampledata <- sampledata[MEAS_DT == LASTTIME,]
    sampledata[, clster_length := length(unique(CLSTR_ID)), by = "SAMP_POINT"]
    if(nrow(sampledata[clster_length > 1]) > 0){
      print(unique(sampledata_fix[clster_length > 1,.(CLSTR_ID, SAMP_TYP, MEAS_DT, SAMP_POINT)],
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
    nvafselected[, LASTTIME := max(MEAS_DT), by = "SAMP_POINT"]
    nvafselected <- nvafselected[MEAS_DT == LASTTIME & PLOT != "I",]
    nvafselected[, LASTTIME := NULL]

    selectedsamples <- rbind(selectedsamples, nvafselected)
    rm(nvafselected)

    nvafselected_forIPC <- unique(sampledata[SAMP_POINT %in% nvafsamppoints &
                                               substr(CLSTR_ID, 11, 11) != "N" &
                                               PLOT == "I",],
                                  by = c("CLSTR_ID"))

    nvafselected_forIPC_Fix <- nvafselected_forIPC[SAMP_TYP == "F",]
    nvafselected_forIPC_Fix[, LASTTIME := max(MEAS_DT),
                            by = "SAMP_POINT"]
    nvafselected_forIPC_Fix <- nvafselected_forIPC_Fix[MEAS_DT == LASTTIME,]
    nvafselected_forIPC_Fix[, LASTTIME := NULL]
    selectedsamples <- rbind(selectedsamples, nvafselected_forIPC_Fix)


    nvafselected_forIPC_Var <- nvafselected_forIPC[!(SAMP_POINT %in%
                                                       unique(nvafselected_forIPC_Fix$SAMP_POINT)),]
    rm(nvafselected_forIPC_Fix)

    nvafselected_forIPC_Var[, LASTTIME := max(MEAS_DT),
                            by = "SAMP_POINT"]
    nvafselected_forIPC_Var <- nvafselected_forIPC_Var[MEAS_DT == LASTTIME,]
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
    sampledata_fix[, LASTTIME := max(MEAS_DT), by = "SAMP_POINT"]
    sampledata_fix <- sampledata_fix[MEAS_DT == LASTTIME,]
    sampledata_fix <- sampledata_fix[CLSTR_ID != "4742-0104-FO1",] ## need to remove when figure out what is
    ## going on
    sampledata_fix[, clster_length := length(unique(CLSTR_ID)), by = "SAMP_POINT"]
    if(nrow(sampledata_fix[clster_length > 1]) > 0){
      print(unique(sampledata_fix[clster_length > 1,.(CLSTR_ID, SAMP_TYP, MEAS_DT, SAMP_POINT)],
                   by = "CLSTR_ID"))
      stop("Multiple monitoring samples were found for one sample point at same time.")
    }
    sampledata_fix[, ':='(LASTTIME = NULL,
                          clster_length = NULL,
                          sample_length = NULL)]
    selectedsamples <- rbind(selectedsamples, sampledata_fix)
    sampledata <- sampledata[!(SAMP_POINT %in% unique(sampledata_fix$SAMP_POINT)), ]

    ## last selection for the sample point just have multiple variable plot samples
    sampledata[, LASTTIME := max(MEAS_DT), by = "SAMP_POINT"]
    sampledata <- sampledata[MEAS_DT == LASTTIME,]
    sampledata[, clster_length := length(unique(CLSTR_ID)), by = "SAMP_POINT"]
    if(nrow(sampledata[clster_length > 1]) > 0){
      print(unique(sampledata_fix[clster_length > 1,.(CLSTR_ID, SAMP_TYP, MEAS_DT, SAMP_POINT)],
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
