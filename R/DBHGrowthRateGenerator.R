#' Derive DBH growth rate by DBH class, growth length class, species and bec zone
#' @description Derive DBH growth rate by DBH class, growth length class, species and bec zone
#' @param treesData data.table, Contains both height trees and nonHT trees in PSPs.
#' @param minObs numeric, Specify minimum number of observation for calculating mean growth rate.
#'                        If missing, default is 100.
#'
#'
#' @return Growth rate lookup table
#'
#' @importFrom data.table data.table ':=' set rbindlist setnames setkey
#' @importFrom dplyr '%>%'
#' @export
#' @docType methods
#' @rdname DBHGrowthRateGenerator
#'
#' @author Yong Luo
DBHGrowthRateGenerator<- function(treeData,
                                  minObs = 100){
  treeData[, measYear := as.numeric(substr(MEAS_DT, 1, 4))]
  treeData[, currentYear_cut := as.Date(paste0(measYear, "-06-01"))]
  treeData[MEAS_DT < currentYear_cut,
           measYear := measYear - 1]
  treeData[, currentYear_cut := NULL]
  treeData[, uniTreeID := paste0(SITE_IDENTIFIER, "-",
                                 PLOT, "-", TREE_NO)]
  ## remove observations that do not have DBH, and dead trees
  treeData_valid <- treeData[DBH > 2  & LV_D == "L" & BGC_ZONE != "Unknown",
                             .(BGC_ZONE, uniTreeID, SPECIES, SP0, measYear, VISIT_NUMBER,
                               DBH)]
  treeData_valid <- treeData_valid[order(uniTreeID, measYear),]

  treeData_valid[, ':='(measYear_next = shift(measYear, type = "lead"),
                        DBH_next = shift(DBH, type = "lead")),
                 by = "uniTreeID"]
  treeData_valid <- treeData_valid[!is.na(measYear_next),]
  treeData_valid[,':='(Growth_total = DBH_next - DBH,
                       Growth_length = measYear_next - measYear)]
  treeData_valid <- treeData_valid[Growth_length > 0,]
  treeData_valid[, DBHClass := cut(DBH,
                                   breaks = c(seq(0, 30, by = 5),
                                              seq(40, 60, by = 10),
                                              (max(treeData_valid$DBH)+1)),
                                   labels = c(seq(0, 30, by = 5),
                                              seq(40, 60, by = 10)))]

  treeData_valid[, MLengthClass := cut(Growth_length,
                                       breaks = c(0, 10,
                                                  seq(20, 100, by = 20)),
                                       labels = c(0, 10,
                                                  seq(20, 80, by = 20)))]

  treeData_valid_smry <- treeData_valid[,.(Nobs = length(DBH)),
                                        by = c("BGC_ZONE", "SPECIES", "SP0",
                                               "DBHClass", "MLengthClass")]

  treeData_valid_smry[Nobs >= minObs,
                      GroupMethod := "bec+sp+dbh_class+length_class"]
  treeData_valid_smry[, Nobs1 := sum(Nobs),
                      by = c("BGC_ZONE", "SPECIES", "DBHClass")]
  treeData_valid_smry[Nobs1 >= minObs & is.na(GroupMethod),
                      GroupMethod := "bec+sp+dbh_class"]
  treeData_valid_smry[, Nobs1 := sum(Nobs),
                      by = c("BGC_ZONE", "SP0", "DBHClass")]
  treeData_valid_smry[Nobs1 >= minObs & is.na(GroupMethod),
                      GroupMethod := "bec+sp0+dbh_class"]
  treeData_valid_smry[, Nobs1 := sum(Nobs),
                      by = c("SP0", "DBHClass")]
  treeData_valid_smry[Nobs1 >= minObs & is.na(GroupMethod),
                      GroupMethod := "sp0+dbh_class"]
  treeData_valid_smry[, Nobs1 := sum(Nobs),
                      by = c("SP0")]
  treeData_valid_smry[Nobs1 >= minObs & is.na(GroupMethod),
                      GroupMethod := "sp0"]
  treeData_valid_smry[,':='(Nobs = NULL,
                            Nobs1 = NULL)]

  treeData_valid[, growthRate := Growth_total/Growth_length]

  ## bec+sp+dbh_class+length_class
  treeData_valid[,':='(quantile5 = quantile(growthRate, 0.05),
                       quantile95 = quantile(growthRate, 0.95)),
                 by = c("BGC_ZONE", "SPECIES", "SP0",
                        "DBHClass", "MLengthClass")]
  treeData_valid_temp <- treeData_valid[growthRate >= quantile5 &
                                          growthRate <= quantile95,]
  treeData_valid_temp <- treeData_valid_temp[,.(GrowthRate_mean_temp = mean(growthRate),
                                                nobs_temp = length(growthRate)),
                                             by = c("BGC_ZONE", "SPECIES", "SP0",
                                                    "DBHClass", "MLengthClass")]

  treeData_valid_smry <- merge(treeData_valid_smry,
                               treeData_valid_temp,
                               by = c("BGC_ZONE", "SPECIES", "SP0",
                                      "DBHClass", "MLengthClass"),
                               all.x = TRUE)
  treeData_valid_smry[GroupMethod == "bec+sp+dbh_class+length_class",
                      ':='(GrowthRate_mean = GrowthRate_mean_temp,
                           Nobs = nobs_temp)]
  treeData_valid_smry[, ':='(GrowthRate_mean_temp = NULL,
                             nobs_temp = NULL)]
  rm(treeData_valid_temp)



  # bec+sp+dbh_class
  treeData_valid[,':='(quantile5 = quantile(growthRate, 0.05),
                       quantile95 = quantile(growthRate, 0.95)),
                 by = c("BGC_ZONE", "SPECIES",
                        "DBHClass")]
  treeData_valid_temp <- treeData_valid[growthRate >= quantile5 &
                                          growthRate <= quantile95,]
  treeData_valid_temp <- treeData_valid_temp[,.(GrowthRate_mean_temp = mean(growthRate),
                                                nobs_temp = length(growthRate)),
                                             by = c("BGC_ZONE", "SPECIES",
                                                    "DBHClass")]
  treeData_valid_smry <- merge(treeData_valid_smry,
                               treeData_valid_temp,
                               by = c("BGC_ZONE", "SPECIES",
                                      "DBHClass"),
                               all.x = TRUE)
  treeData_valid_smry[GroupMethod == "bec+sp+dbh_class",
                      ':='(GrowthRate_mean = GrowthRate_mean_temp,
                           Nobs = nobs_temp)]
  treeData_valid_smry[, ':='(GrowthRate_mean_temp = NULL,
                             nobs_temp = NULL)]
  rm(treeData_valid_temp)


  # bec+sp0+dbh_class
  treeData_valid[,':='(quantile5 = quantile(growthRate, 0.05),
                       quantile95 = quantile(growthRate, 0.95)),
                 by = c("BGC_ZONE", "SP0",
                        "DBHClass")]
  treeData_valid_temp <- treeData_valid[growthRate >= quantile5 &
                                          growthRate <= quantile95,]
  treeData_valid_temp <- treeData_valid_temp[,.(GrowthRate_mean_temp = mean(growthRate),
                                                nobs_temp = length(growthRate)),
                                             by = c("BGC_ZONE", "SP0",
                                                    "DBHClass")]
  treeData_valid_smry <- merge(treeData_valid_smry,
                               treeData_valid_temp,
                               by = c("BGC_ZONE", "SP0",
                                      "DBHClass"),
                               all.x = TRUE)
  treeData_valid_smry[GroupMethod == "bec+sp0+dbh_class",
                      ':='(GrowthRate_mean = GrowthRate_mean_temp,
                           Nobs = nobs_temp)]
  treeData_valid_smry[, ':='(GrowthRate_mean_temp = NULL,
                             nobs_temp = NULL)]
  rm(treeData_valid_temp)


  # sp0+dbh_class
  treeData_valid[,':='(quantile5 = quantile(growthRate, 0.05),
                       quantile95 = quantile(growthRate, 0.95)),
                 by = c("SP0",
                        "DBHClass")]
  treeData_valid_temp <- treeData_valid[growthRate >= quantile5 &
                                          growthRate <= quantile95,]
  treeData_valid_temp <- treeData_valid_temp[,.(GrowthRate_mean_temp = mean(growthRate),
                                                nobs_temp = length(growthRate)),
                                             by = c("SP0",
                                                    "DBHClass")]
  treeData_valid_smry <- merge(treeData_valid_smry,
                               treeData_valid_temp,
                               by = c("SP0",
                                      "DBHClass"),
                               all.x = TRUE)
  treeData_valid_smry[GroupMethod == "sp0+dbh_class",
                      ':='(GrowthRate_mean = GrowthRate_mean_temp,
                           Nobs = nobs_temp)]
  treeData_valid_smry[, ':='(GrowthRate_mean_temp = NULL,
                             nobs_temp = NULL)]
  rm(treeData_valid_temp)


  # sp0
  treeData_valid[,':='(quantile5 = quantile(growthRate, 0.05),
                       quantile95 = quantile(growthRate, 0.95)),
                 by = c("SP0")]
  treeData_valid_temp <- treeData_valid[growthRate >= quantile5 &
                                          growthRate <= quantile95,]
  treeData_valid_temp <- treeData_valid_temp[,.(GrowthRate_mean_temp = mean(growthRate),
                                                nobs_temp = length(growthRate)),
                                             by = c("SP0")]
  treeData_valid_smry <- merge(treeData_valid_smry,
                               treeData_valid_temp,
                               by = c("SP0"),
                               all.x = TRUE)
  treeData_valid_smry[GroupMethod == "sp0",
                      ':='(GrowthRate_mean = GrowthRate_mean_temp,
                           Nobs = nobs_temp)]
  treeData_valid_smry[, ':='(GrowthRate_mean_temp = NULL,
                             nobs_temp = NULL)]
  rm(treeData_valid_temp)
  treeData_valid_smry[GrowthRate_mean < 0, GrowthRate_mean := 0]
  return(treeData_valid_smry)
}
