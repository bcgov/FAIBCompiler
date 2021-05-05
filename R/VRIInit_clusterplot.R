#' Load and select cluster and plot level data- VRI specific
#'
#'
#' @description This function prepares the cluster/plot-level inputs for VRI compiler. Specifically, it standardizes names for
#'              the variables; reports and removes the duplicate observations at cluster, cluster/plot.
#'
#'
#' @param dataSourcePath character, Specifies the path that directs to the VRI original data soruce, i.e.,
#'                                  \code{//Mayhem/GIS_TIB/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/vri_sa}.
#'
#' @return A data table that contains key information at cluster/plot level and compiler log file.
#'
#' @importFrom data.table ':='
#' @importFrom dplyr '%>%'
#' @importFrom FAIBBase merge_dupUpdate
#'
#' @export
#' @docType methods
#' @rdname VRIInit_clusterplot
#'
#' @author Yong Luo
VRIInit_clusterplot <- function(dataSourcePath){
  ## for vi_a
  vi_a <- readRDS(file.path(dataSourcePath, "vi_a.rds")) %>% data.table
  names(vi_a) <- toupper(names(vi_a))
  totalNrow <- nrow(vi_a)
  uniqueNrow <- length(unique(vi_a$CLSTR_ID))

  vi_a <- vi_a[substr(PROJ_ID, 1, 3) != "DEV",]


  vi_a <- vi_a[substr(TYPE_CD, 1, 1) != "E", ]


  # The plots belong to LGMW project, which samples each polygon (a unit of sample)
  # that has one or more plots, however, the plot identity is not unique
  # remove these plot from further compilation
  vi_a <- vi_a[substr(TYPE_CD, 1, 1) != "W",] # double check with Bob and Rene

  vi_a[, PRJ_GRP := prj_ID2Grp(PROJ_ID)]
  vi_a[!(BEC %in% c("AT","BWBS","CDF","CWH","ESSF","ICH","IDF","MH",
                    "MS","PP","SBPS","SBS","SWB","BG","BAFA","CMA","IMA")),
       BEC := prj_ID2BEC(PROJ_ID)]

  vi_a[is.na(FIZ) | FIZ == " ", FIZ := "E"]

  ## keep key columns for further compilation
  vi_a <- vi_a[,.(CLSTR_ID, PROJ_ID, SAMP_NO, TYPE_CD, MEAS_DT, TSA, FIZ, BGC_ZONE = BEC,
                  BGC_SBZN = BEC_SBZ, BGC_VAR = BEC_VAR, PRJ_GRP,
                  SAMPLE_SITE_NAME)]
  vi_b <- readRDS(file.path(dataSourcePath, "vi_b.rds")) %>% data.table
  names(vi_b) <- toupper(names(vi_b))
  totalNrow <- nrow(vi_b)
  uniqueNrow <- nrow(unique(vi_b, by = c("CLSTR_ID", "PLOT")))
  vi_b <- vi_b[CLSTR_ID %in% vi_a$CLSTR_ID,]

  vi_b <- unique(vi_b, by = c("CLSTR_ID", "PLOT"))
  vi_b[V_BAF > 0 & V_FULL == TRUE, PLOT_WT := 1]
  vi_b[V_BAF > 0 & V_HALF == TRUE, PLOT_WT := 2]
  vi_b[V_BAF > 0 & V_QRTR == TRUE, PLOT_WT := 4]
  vi_b[V_BAF > 0, ':='(SAMP_TYP = "V", BLOWUP = V_BAF)]
  # for fixed area plot
  vi_b[is.na(V_BAF) & F_FULL == TRUE, PLOT_WT := 1]
  vi_b[is.na(V_BAF) & F_HALF == TRUE, PLOT_WT := 2]
  vi_b[is.na(V_BAF) & F_QRTR == TRUE, PLOT_WT := 4]
  vi_b[is.na(V_BAF), ':='(SAMP_TYP = "F",
                          BLOWUP = 1 / (3.1415926* F_RAD^2) * 10000)]
  vi_b[, NO_PLOTS := length(PLOT), by = CLSTR_ID]
  vi_b[, PLOT_DED := 1L]
  vi_b <- merge(vi_b, vi_a[,.(CLSTR_ID, MEAS_DT, PROJ_ID)],
                by = "CLSTR_ID",
                all.x = TRUE)
  vi_b[substr(CLSTR_ID, 9, 9) == "N" | (as.numeric(substr(MEAS_DT, 1, 4)) >= 2008) |
         (as.numeric(substr(MEAS_DT, 1, 4)) == 2007 & PROJ_ID %in% c("0141", "014M", "0091")),
       PLOT_DED := NO_PLOTS]
  vi_b <- vi_b[,.(CLSTR_ID, PLOT, SAMP_TYP, PLOT_WT, BLOWUP, NO_PLOTS, PLOT_DED)]
  clusterplot <- FAIBBase::merge_dupUpdate(vi_b, vi_a, by = "CLSTR_ID", all.x = TRUE)
  return(clusterplot)
}

