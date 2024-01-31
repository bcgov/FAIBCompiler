#' Merge oracle and ascii data for vi_(a, ..i)
#'
#'
#' @description This function is to merge oracle and ascii data before the VRI compiler
#'
#' @param oracleSourcePath character, Specifies the path that stores data from oracle data base.
#'                                    In VRI compiler, this should be the savePath for \code{\link{loadVGIS}}.
#' @param asciiSourcePath character, Specifies the path that stores data from ascii data base.
#'                                    In VRI compiler, this should be the savePath for \code{\link{loadASCII}}.
#' @param coeffPath character, Specifies the path that stores coefficients and spatial lookup tables.
#'
#' @param fizmapPath character, Specifies the path to forest inventory zone map. By default,
#'                              it is set to \code{//spatialfiles2.bcgov/work/for/vic/hts/dam/workarea/data/infrastructure},
#'                              which is maintained by FAIB employee.
#' @param fizmapName character, Specifies the name of forest inventory zone map. By default,
#'                              it is set to \code{FIZ_REG_COMPARTMENT}, which is maintained by FAIB employee.
#' @param fizmapFormat character, Specifies the format of forest inventory zone map. Currently, it can be specified
#'                                as \code{gdb} for geodatabase format and \code{shp} for shapefile format.
#'                                By default, it is set to \code{gdb}, which is maintained by FAIB employee.
#' @param outputPath character, Specifies the path to save your outputs. If missing, the current working
#'                   directory will be choosed.
#'
#' @return no item returned
#'
#' @importFrom data.table ':=' data.table
#' @importFrom dplyr '%>%'
#' @importFrom FAIBBase getSpatial
#' @export
#' @docType methods
#' @rdname mergeOAData
#' @author Yong Luo
mergeOAData <- function(oracleSourcePath, asciiSourcePath, coeffPath,
                        fizmapPath, fizmapName, fizmapFormat, outputPath){
  if(dir.exists(file.path(outputPath))){
    unlink(file.path(outputPath), recursive = TRUE)
    dir.create(file.path(outputPath))
    warnings("The output path exists and is replaced totally.")
  } else {
    dir.create(file.path(outputPath))
  }
  long2shortspcd <- lookup_species()
  long2shortspcd <- long2shortspcd[!is.na(LONG_SPECIES),.(LONG_SPECIES, SPECIES)]
  # for vi_a
  vi_a_or <- readRDS(file.path(oracleSourcePath, "sample_card_vgis.rds"))
  vi_access_or <- readRDS(file.path(oracleSourcePath, "access_card_vgis.rds"))
  vi_a_or <- merge(vi_a_or, vi_access_or$IPCLocation,
                   by = "CLSTR_ID", all.x = TRUE)
  vi_a_or[, SOURCE := "ORACLE"]

  if(asciiSourcePath != "NONE"){
    vi_a_as <- readRDS(file.path(asciiSourcePath, "carda_ascii.rds"))
    vi_a_as <- vi_a_as[,.(CLSTR_ID, PROJ_ID, SAMP_NO, TYPE_CD, MEAS_DT, ASCII_DT,
                          LOAD_DT = ASCII_DT)]
    vi_access_as <- readRDS(file.path(asciiSourcePath, "card_cl_ascii.rds"))
    vi_a_as <- merge(vi_a_as, vi_access_as,
                     by = "CLSTR_ID", all.x = TRUE)
    vi_a_as[, ':='(SOURCE = "ASCII",
                   PLT_SPC = NULL)]

    vi_a <- rbindlist(list(vi_a_or, vi_a_as), fill = TRUE)

  } else {
    vi_a <- vi_a_or
  }

  vi_a <- unique(vi_a, by = "CLSTR_ID")
  vi_a[, SAMP_POINT := substr(CLSTR_ID, 1, 9)]
  externalSpatial <- TRUE
  if(externalSpatial){
    spatialAttr <- read.table(file.path(coeffPath, "spatiallookup.txt"),
                              sep = ",",
                              header = TRUE,
                              stringsAsFactors = FALSE) %>% data.table
    spatialAttr[,':='(PROJ_ID = NULL,
                      SAMP_NO = NULL)]
    vi_a <- merge(vi_a, spatialAttr, by = "SAMP_POINT", all.x = TRUE)
    vi_a[, SAMP_POINT := NULL]

  } else {
    # assign spatial attributes
    vi_a_Loc <- vi_a[!is.na(IP_UTM) & !is.na(IP_NRTH) & !is.na(IP_EAST),
                     .(CLSTR_ID, SAMP_POINT,
                       IP_UTM, IP_NRTH, IP_EAST)]
    # select 0141-0126-QR1 over 0141-0126-TO1
    vi_a_Loc <- vi_a_Loc[CLSTR_ID != "0141-0126-TO1", ]
    # select 0201-0069-YR1 over 0201-0069-MO1
    vi_a_Loc <- vi_a_Loc[CLSTR_ID != "0201-0069-MO1", ]
    # select 0221-0029-OR1 over 0221-0029-TO1
    vi_a_Loc <- vi_a_Loc[CLSTR_ID != "0221-0029-TO1", ]
    # select 0291-0060-O 1 over 0291-0060-NO1 and 0291-0060-QA1
    vi_a_Loc <- vi_a_Loc[!(CLSTR_ID %in% c("0291-0060-NO1", "0291-0060-QA1")), ]
    # select 4721-0014-DR1 over 4721-0014-N 1 and 4721-0014-Q 1
    vi_a_Loc <- vi_a_Loc[!(CLSTR_ID %in% c("4721-0014-N 1", "4721-0014-Q 1")), ]
    # select CMI4-0436-FR1 over CMI4-0436-FO1
    vi_a_Loc <- vi_a_Loc[CLSTR_ID != "CMI4-0436-FO1", ]
    # select CMI6-0155-FR1 over CMI6-0155-FO1
    vi_a_Loc <- vi_a_Loc[CLSTR_ID != "CMI6-0155-FO1", ]
    # select DDC1-0022-MO1 over DDC1-0022-OO1
    vi_a_Loc <- vi_a_Loc[CLSTR_ID != "DDC1-0022-OO1", ]
    vi_a_Loc <- unique(vi_a_Loc[,.(SAMP_POINT, IP_UTM, IP_NRTH, IP_EAST)],
                       by = "SAMP_POINT")
    vi_a_loc_bec <- getSpatial(pointID = vi_a_Loc$SAMP_POINT,
                               zone = vi_a_Loc$IP_UTM,
                               northing = vi_a_Loc$IP_NRTH,
                               easting = vi_a_Loc$IP_EAST,
                               spatialAttribute = "bec")
    names(vi_a_loc_bec) <- c("SAMP_POINT", "BEC", "BEC_SBZ", "BEC_VAR")
    vi_a_loc_tsa <- getSpatial(pointID = vi_a_Loc$SAMP_POINT,
                               zone = vi_a_Loc$IP_UTM,
                               northing = vi_a_Loc$IP_NRTH,
                               easting = vi_a_Loc$IP_EAST,
                               spatialAttribute = "tsa")
    names(vi_a_loc_tsa) <- c("SAMP_POINT", "TSA", "TSA_DESC")
    vi_a_loc_fiz <- getSpatial(pointID = vi_a_Loc$SAMP_POINT,
                               zone = vi_a_Loc$IP_UTM,
                               northing = vi_a_Loc$IP_NRTH,
                               easting = vi_a_Loc$IP_EAST,
                               spatialAttribute = "fiz",
                               mapPath = fizmapPath,
                               mapName = fizmapName,
                               mapFormat = fizmapFormat)
    names(vi_a_loc_fiz) <- c("SAMP_POINT", "FIZ")
    vi_a <- merge(vi_a, vi_a_loc_bec, by = "SAMP_POINT", all.x = TRUE)
    vi_a <- merge(vi_a, vi_a_loc_tsa, by = "SAMP_POINT", all.x = TRUE)
    vi_a <- merge(vi_a, vi_a_loc_fiz, by = "SAMP_POINT", all.x = TRUE)
    ## add bec zone information mannually based on Bob's lookup table
    ## for the clusters that do not have full UTM information
    vi_a[SAMP_POINT == "0291-0103", ':='(BEC = "IDF", BEC_SBZ = "dw")]
    vi_a[SAMP_POINT == "DCK1-0035", ':='(BEC = "MH", BEC_SBZ = "mm")]
    vi_a[SAMP_POINT == "DDCY-0545", ':='(BEC = "BWBS", BEC_SBZ = "mw")]
    vi_a[, SAMP_POINT := NULL]
  }



  saveRDS(vi_a, file.path(outputPath, "vi_a.rds"))
  if(asciiSourcePath != "NONE"){
    rm(vi_a_or, vi_access_or, vi_a_as, vi_access_as)

  } else {
    rm(vi_a_or, vi_access_or)

  }

  # for vi_b
  vi_b_or <- readRDS(file.path(oracleSourcePath, "plot_card_vgis.rds"))
  set(vi_b_or, , c("PL_QUAL", "PL_TYPE", "SP_TYPE", "POP"), NULL)
  if(asciiSourcePath != "NONE"){
    vi_b_as <- readRDS(file.path(asciiSourcePath, "cardb_ascii.rds"))
    vi_b_as <- vi_b_as[, names(vi_b_or), with = FALSE]
    colums <- names(vi_b_as)[!(names(vi_b_as) %in% c("CLSTR_ID", "PLOT", "V_BAF", "F_RAD"))]
    for(indicol in  colums){
      vi_b_as[, ':='(tempcol = unlist(vi_b_as[, indicol, with = FALSE]),
                     tempcol2 = as.logical(NA))]
      vi_b_as[toupper(tempcol) == "X", tempcol2 := TRUE]
      vi_b_as[, c(indicol) := tempcol2]
      set(vi_b_as, , c("tempcol", "tempcol2"), NULL)
    }
    vi_b <- rbindlist(list(vi_b_or, vi_b_as))
    rm(vi_b_as)
  } else {
    vi_b <- vi_b_or
  }
  vi_b <- unique(vi_b, by = c("CLSTR_ID", "PLOT"))
  saveRDS(vi_b, file.path(outputPath, "vi_b.rds"))
  rm(vi_b_or)

  # for vi_c
  vi_c_or <- readRDS(file.path(oracleSourcePath, "tree_cardc_vgis.rds"))
  vi_c_or <- merge(vi_c_or, long2shortspcd, by = "LONG_SPECIES", all.x = TRUE)
  vi_c_or[, LONG_SPECIES := NULL]
  if(asciiSourcePath != "NONE"){
    vi_c_as <- readRDS(file.path(asciiSourcePath, "cardc_ascii.rds"))
    vi_c <- rbindlist(list(vi_c_or, vi_c_as), fill = TRUE)
    rm(vi_c_as)
  } else {
    vi_c <- vi_c_or
  }
  vi_c <- vi_c[!(CLSTR_ID == "DDC2_0544_QA1")]
  speciestable <- unique(lookup_species(), by = "SPECIES")
  vi_c[is.na(SPECIES), SPECIES := "X"]
  vi_c <- merge(vi_c, speciestable[,.(SPECIES, SP0)],
                by = "SPECIES", all.x = TRUE)
  vi_c <- unique(vi_c, by = c("CLSTR_ID", "PLOT", "TREE_NO"))
  saveRDS(vi_c, file.path(outputPath, "vi_c.rds"))
  rm(vi_c_or)

  # for vi_i
  vi_i_or <- readRDS(file.path(oracleSourcePath, "tree_cardi_vgis.rds"))
  vi_i_or <- merge(vi_i_or, long2shortspcd, by = "LONG_SPECIES", all.x = TRUE)
  vi_i_or[, LONG_SPECIES := NULL]
  if(asciiSourcePath != "NONE"){
    vi_i_as <- readRDS(file.path(asciiSourcePath, "cardi_ascii.rds"))
    vi_i <- rbindlist(list(vi_i_or, vi_i_as), fill = TRUE)
    rm(vi_i_as)
  } else {
    vi_i <- vi_i_or
  }
  vi_i <- vi_i[!(CLSTR_ID == "DDC2_0544_QA1")]
  vi_i[is.na(SPECIES), SPECIES := "X"]
  vi_i <- merge(vi_i, speciestable[,.(SPECIES, SP0)],
                by = "SPECIES", all.x = TRUE)
  vi_i <- unique(vi_i, by = c("CLSTR_ID", "PLOT", "TREE_NO"))
  saveRDS(vi_i, file.path(outputPath, "vi_i.rds"))
  rm(vi_i_or)


  # for vi_d
  vi_d_or <- readRDS(file.path(oracleSourcePath, "lossIndicator_card_vgis.rds"))
  vi_d_or <- merge(vi_d_or, long2shortspcd, by = "LONG_SPECIES", all.x = TRUE)
  vi_d_or[, LONG_SPECIES := NULL]
  if(asciiSourcePath != "NONE"){
    vi_d_as <- readRDS(file.path(asciiSourcePath, "cardd_ascii.rds"))
    vi_d <- rbindlist(list(vi_d_or, vi_d_as), fill = TRUE)
    rm(vi_d_as)
  } else {
    vi_d <- vi_d_or
  }
  vi_d[is.na(SPECIES), SPECIES := "X"]
  vi_d <- merge(vi_d, speciestable[,.(SPECIES, SP0)],
                by = "SPECIES", all.x = TRUE)
  vi_d <- unique(vi_d, by = c("CLSTR_ID", "PLOT", "TREE_NO"))
  saveRDS(vi_d, file.path(outputPath, "vi_d.rds"))
  rm(vi_d_or)

  # for vi_h

  vi_h_or <- readRDS(file.path(oracleSourcePath, "siteTree_card_vgis.rds"))
  vi_h_or <- merge(vi_h_or, long2shortspcd, by = "LONG_SPECIES", all.x = TRUE)
  vi_h_or[, LONG_SPECIES := NULL]
  if(asciiSourcePath != "NONE"){
    vi_h_as <- readRDS(file.path(asciiSourcePath, "cardh_ascii.rds"))
    vi_h <- rbindlist(list(vi_h_or, vi_h_as), fill = TRUE)
    rm(vi_h_as)
  } else {
    vi_h <- vi_h_or
  }
  vi_h[is.na(SPECIES), SPECIES := "X"]
  vi_h <- merge(vi_h, speciestable[,.(SPECIES, SP0)],
                by = "SPECIES", all.x = TRUE)
  vi_h1 <- unique(vi_h, by = c("CLSTR_ID", "PLOT", "TREE_NO"))
  vi_h1[,':='(TREETYPE = NULL)]

  vi_h <- vi_h[,.(CLSTR_ID, PLOT, TREE_NO, TREETYPE)]
  vi_h_th <- unique(vi_h[TREETYPE %in% c("T", "L", "S", "O", "X", "N"),
                  .(CLSTR_ID, PLOT, TREE_NO, TH_TREE_tmp = TREETYPE)])
  vi_h_th <- vi_h_th[order(CLSTR_ID, PLOT, TREE_NO, TH_TREE_tmp),]
  vi_h_th[, nobs := length(TH_TREE_tmp),
          by = c("CLSTR_ID", "PLOT", "TREE_NO")]
  vi_h_th[nobs == 1, TH_TREE := TH_TREE_tmp]
  vi_h_th[nobs > 1, TH_TREE_tmp1 := paste0(TH_TREE_tmp, collapse = "_"),
          by = c("CLSTR_ID", "PLOT", "TREE_NO")]
  vi_h_th[TH_TREE_tmp1 %in% c("L_T", "S_T", "O_T", "N_T"), TH_TREE := "T"]
  vi_h_th[TH_TREE_tmp1 %in% c("T_X"), TH_TREE := "X"]
  vi_h_th[TH_TREE_tmp1 %in% c("L_N"), TH_TREE := "N"]
  vi_h_th <- unique(vi_h_th[,.(CLSTR_ID, PLOT, TREE_NO, TH_TREE)],
                    by = c("CLSTR_ID", "PLOT", "TREE_NO"))
  vi_h_th[PLOT == "I" & TH_TREE == "T", TP_TREE := "T"]
  vi_h1 <- merge(vi_h1, vi_h_th,
                 by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                 all.x = TRUE)
  vi_h_ra <- vi_h[TREETYPE == "R",
                  .(CLSTR_ID, PLOT, TREE_NO, RA_TREE = TREETYPE)]
  vi_h_ra <- vi_h_ra[!duplicated(vi_h_ra),]
  vi_h1 <- merge(vi_h1, vi_h_ra,
                 by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                 all.x = TRUE)
  saveRDS(vi_h1, file.path(outputPath, "vi_h.rds"))
  rm(vi_h_or, vi_h1, vi_h_ra, vi_h_th)

  #for vi_e
  vi_e_or <- readRDS(file.path(oracleSourcePath, "stumpSTree_card_vgis.rds"))
  vi_e_or$stump <- merge(vi_e_or$stump, long2shortspcd, by = "LONG_SPECIES", all.x = TRUE)
  vi_e_or$stump[, LONG_SPECIES := NULL]
  vi_e_or$smalltree <- merge(vi_e_or$smalltree, long2shortspcd, by = "LONG_SPECIES", all.x = TRUE)
  vi_e_or$smalltree[, LONG_SPECIES := NULL]
  if(asciiSourcePath != "NONE"){
    vi_e_as <- readRDS(file.path(asciiSourcePath, "carde_ascii.rds"))
    vi_e <- rbindlist(list(vi_e_or$header, vi_e_as), fill = TRUE)
    rm(vi_e_as)
  } else {
    vi_e <- vi_e_or$header
  }
  saveRDS(vi_e, file.path(outputPath, "vi_e.rds"))

  if(asciiSourcePath != "NONE"){
    vi_f_as <- readRDS(file.path(asciiSourcePath, "cardf_ascii.rds"))
    vi_f <- rbindlist(list(vi_e_or$smalltree, vi_f_as), fill = TRUE)
    rm(vi_f_as)
  } else {
    vi_f <- vi_e_or$smalltree
  }

  saveRDS(vi_f, file.path(outputPath, "vi_f.rds"))

  if(asciiSourcePath != "NONE"){
    vi_g_as <- readRDS(file.path(asciiSourcePath, "cardg_ascii.rds"))
    vi_g <- rbindlist(list(vi_e_or$stump, vi_g_as), fill = TRUE)
    rm(vi_g_as)
  } else {
    vi_g <- vi_e_or$stump
  }
  vi_g <- merge(vi_g, speciestable[,.(SPECIES, SP0)],
                by = "SPECIES", all.x = TRUE)
  saveRDS(vi_g, file.path(outputPath, "vi_g.rds"))
  rm(vi_e_or)
}
