#' To prepare input data for TASS run
#'
#' @description This function takes the compilation outputs and prepare stand age, loss factor,
#'              site index and stem mapping, so that the outputs are ready for TASS run
#'
#' @param inputPath character, The path to the compilation output.
#' @param outputPath character, The path to save the outputs.
#' @param projectName character, The name of a project, currently support \code{YSM}, \code{Taan} or \code{special}.
#' @param clstrIDs character, A list of clsterid in a special project. It will be ignored if the \code{projectName} is \code{YSM}
#'                            or \code{Taan}. As they are rule-based selection of clster id.
#'
#' @param siteIndexTable data.table, The table to contain site index.
#' @param siteIndexTableSource character, Indicates where the site index table from either \code{Rene} or \code{ISMCCompiler}.
#'                                        If the source is ISMCCompiler, the function will ignore the site index table provided.
#'                                        Therefore, it can be missing. Default is \code{ISMCCompiler}.
#' @param siteIndexMethod character, The method to derive site index for repeatedly-visited sites.
#'                                   There are three methods can be used: \code{byvisit}, \code{average}, \code{firstvisit}
#'                                   or \code{closest50}. \code{byvisit} uses the site index for each visit.
#'                                   \code{average} takes the mean site index for a given
#'                                   site and species over multiple visits. \code{firstvisit} uses the site index
#'                                   from the first visit. \code{closest50} uses the site index which has the closest
#'                                   to the stand age of 50 years.
#' @param treeVigorMethod character, Method to derive a tree's vigor,
#'                                   currently support \code{mainsub}.
#' @param vigorAdjust08 logical, Indicates if the mean of mean of the tree height vigor needed to be adjusted to 0.8.
#'                               Default is \code{TRUE}.
#' @param randomSeed numeric, The random seeds for the stem mapping extension. Default is \code{NA}, which
#'                             does not have a seed number.
#' @export
#' @docType methods
#' @note mainsub in treeVigorMethod:
#'       •	For each combination of site_identifier by visit_number by species, select the 6 tallest trees from the main plot and the single tallest tree from the subplot.
#'       •	From this subset, drop all trees with
#'              o	BROKEN_TOP_IND = Y
#'              o	CR_CL = I, S
#'              o	RESIDUAL = Y
#'              o	WALKTHRU = O
#'       •	Compute site_height as the average height of remaining trees
#'       •	If no trees remain, then compute an alternate site_height as the average height of all trees in the plot by site_identifier * visit_number * species (ie., no exclusion for height, broken top, crown class, residual class, or walkthru class.
#'
#' @importFrom data.table ':=' rbindlist setnames
#' @importFrom dplyr '%>%' distinct
#' @importFrom openxlsx write.xlsx
#' @importFrom FAIBBase stemMappingExtension
#' @rdname prepareTASSInputs
#' @author Yong Luo
prepareTASSInputs <- function(inputPath,
                              outputPath,
                              projectName,
                              clstrIDs = NA,
                              siteIndexTable = NA,
                              siteIndexTableSource = "ISMCCompiler",
                              siteIndexMethod,
                              treeVigorMethod,
                              vigorAdjust08 = TRUE,
                              randomSeed = NA){
  if(dir.exists(outputPath)){
    if(length(dir(outputPath)) > 0){
      wanttoremove <- readline("The outputPath contains files, do you want to remove them? (Yes/No)")
      if(toupper(wanttoremove) %in% c("Y", "YES")){
        unlink(outputPath, recursive = TRUE)
        dir.create(outputPath)
      }
    }
  } else {
    dir.create(outputPath)
  }
  if(projectName == "special" & length(clstrIDs) == 1){
    if(is.na(clstrIDs)){
      stop("clstrIDs muse be specified in special project.")
    }
  }
  if(siteIndexTableSource == "Rene"){
    siteIndexTableSource_des <- "Provided by Rene, saved to outputPath for reference"
  } else {
    siteIndexTableSource_des <- paste0("Provided by ISMCCompiler, i.e., ", inputPath, "/compilation_nonPSP_db/Smries_siteAge_byCLSP.rds")
  }


  logfile <- rbind(data.table(Item = "project name",
                              Description = projectName),
                   data.table(Item = "tree and sample data source",
                              Description = inputPath),
                   data.table(Item = "site index table source",
                              Description = siteIndexTableSource_des),
                   data.table(Item = "site index select for a site over multiple visits",
                              Description = siteIndexMethod),
                   data.table(Item = "height vigor calculation method",
                              Description = treeVigorMethod),
                   data.table(Item = "Key variable availability summaries",
                              Description = "see process_table in this file"),
                   data.table(Item = "prepared TASS input with stem mapped in 1 ha",
                              Description = "trees_with_xy_all.csv"),
                   data.table(Item = "prepared TASS input without stem mapped in original plot",
                              Description = "trees_without_xy_all.csv"),
                   data.table(Item = "Vigor adjusted to a mean of 0.8",
                              Description = as.character(vigorAdjust08)),
                   data.table(Item = "random seed",
                              Description = as.character(randomSeed)))
  if(vigorAdjust08){
    logfile <- rbind(logfile,
                     data.table(Item = "prepared TASS input with stem mapped in 1 ha with vigor adjustement",
                                Description = "trees_with_xy_all_withVGadj.csv"),
                     data.table(Item = "prepared TASS input without stem mapped in original plot with vigor adjustement",
                                Description = "trees_without_xy_all_withVGadj.csv"))
  }

  if(length(siteIndexTable) > 1){
    ## provided with Rene's site index table
    write.xlsx(siteIndexTable,
               file.path(outputPath, "siteindextable_rene.xlsx"))
    names(siteIndexTable) <- toupper(names(siteIndexTable))
    ## get site index by sample point and species from compiled age files
    sinames <- paste0("GRDSI_", 1:9)
    sispecies <- paste0("GRDSPC_", 1:9)
    siteindex <- siteIndexTable[,c("CLSTR_ID", "TASS_SRC",
                                   sinames),
                                with = FALSE]
    siteindex <- reshape(data = siteindex,
                         idvar = c("CLSTR_ID"),
                         varying = sinames,
                         v.names = "SITE",
                         timevar = "location",
                         times = sinames,
                         direction = "long")
    siteindex[, location := gsub("GRDSI", "", location)]
    siteindex_sp <- reshape(data = siteIndexTable[,c("CLSTR_ID",
                                                     sispecies),
                                                  with = FALSE],
                            idvar = c("CLSTR_ID"),
                            varying = sispecies,
                            v.names = "SPECIES",
                            timevar = "location",
                            times = sispecies,
                            direction = "long")
    siteindex_sp[, location := gsub("GRDSPC", "", location)]
    siteindex <- merge(siteindex,
                       siteindex_sp,
                       by = c("CLSTR_ID",
                              "location"))
    siteindex <- siteindex[!(SPECIES %in% c(NA, "")),]
    siteindex[,location := NULL]
    setnames(siteindex, "TASS_SRC", "TASS_VERSION")
    rm(siteindex_sp)
  } else {
    if(siteIndexTableSource == "Rene" & is.na(siteIndexTable)){
      stop("Site index table must be provided by Rene.")
    } else if(siteIndexTableSource == "ISMCCompiler"){
      # when source is ISMCCompiler, the site index table will be load from the inputPath
      # and overwrite the site index table provided
      siteIndexTable <- readRDS(file.path(inputPath,
                                          "compilation_nonPSP_db",
                                          "Smries_siteAge_byCLSP.rds"))
      siteindex <- siteIndexTable[,.(CLSTR_ID, SPECIES,
                                     SITE = SI_M_TLSO, SI_M_TXO)]
      siteindex[is.na(SITE) & !is.na(SI_M_TXO),
                SITE := SI_M_TXO]
      siteindex[, ':='(SI_M_TXO = NULL,
                       TASS_VERSION = as.character(NA))]
    }
  }

  rm(siteIndexTable)
  samples <- readRDS(file.path(inputPath,
                               "compilation_nonPSP_db",
                               "sample_msmt_header.rds"))
  siteindex <- merge(siteindex,
                     samples[,.(CLSTR_ID,
                                PROJ_AGE_ADJ = SA_VEGCOMP)],
                     by = "CLSTR_ID",
                     all.x = TRUE)

  spatiallookup <- readRDS(file.path(inputPath,
                                     "compilation_nonPSP_db",
                                     "sample_site_header.rds")) %>% data.table
  samples <- merge(samples,
                   unique(spatiallookup[,.(SITE_IDENTIFIER = SITE_IDENTIFIER,
                                           TFL, IP_UTM, IP_NRTH, IP_EAST,
                                           BEC_ZONE, BEC_SBZ, BEC_VAR,
                                           TSA)],
                          by = "SITE_IDENTIFIER"),
                   by = "SITE_IDENTIFIER",
                   all.x = TRUE)
  rm(spatiallookup)
  samples <- unique(samples, by = "CLSTR_ID")
  if(projectName == "YSM"){
    if(siteIndexTableSource == "Rene"){
      sample_selected <- siteindex$CLSTR_ID
    } else {
      ## need to select the samples for this project
      sample_selected <- "" # must define a rule
    }
  } else if(projectName == "Taan"){
    ## need to select the samples for this project
    vi_a <- readRDS(file.path(inputPath,
                              "compilation_nonPSP_sa", "vi_a.rds"))
    sample_selected <- unique(vi_a[PROJECT_DESCRIPTOR %in% c("TFL 60 Monitoring",
                                                             "TFL 60 Monitoring Remeasurements")]$CLSTR_ID)
    rm(vi_a)
  } else if(projectName == "special"){
    sample_selected <- clstrIDs
  }else {
    stop("please specify this term correctly.")
  }

  samples <- samples[CLSTR_ID %in% sample_selected,]
  ## summarytable
  summarytable <- samples[,.(CLSTR_ID, PROJ_AGE_ADJ = SA_VEGCOMP)]
  summarytable[!is.na(PROJ_AGE_ADJ), age_available := "yes"]
  summarytable[is.na(PROJ_AGE_ADJ),
               age_available := "no"]

  treelist0 <- readRDS(file.path(inputPath,
                                 "compilation_nonPSP_db",
                                 "treelist.rds")) %>% data.table
  treelist1 <- treelist0[CLSTR_ID %in% sample_selected,]
  rm(treelist0)
  summarytable[CLSTR_ID %in% treelist1$CLSTR_ID,
               tree_list_available := "yes"]
  summarytable[is.na(tree_list_available),
               tree_list_available := "no"]
  treelist1 <- treelist1[,.(SITE_IDENTIFIER, CLSTR_ID, PLOT, TREE_NO, ST_FL = S_F,
                            SPECIES, LV_D, DBH, HEIGHT, BROKEN_TOP_IND)]
  treelist1 <- merge(treelist1, samples[,.(CLSTR_ID, MEAS_DT,
                                           BEC = BEC_ZONE,
                                           IP_UTM, IP_NRTH, IP_EAST)],
                     by = "CLSTR_ID", all.x = TRUE)
  treelist1[, MEAS_YR := as.numeric(substr(MEAS_DT, 1, 4))]

  ## get stem mapping trees
  vi_d0 <- readRDS(file.path(inputPath,
                             "compilation_nonPSP_db",
                             "compiled_vi_d.rds")) %>% data.table

  vi_d1 <- vi_d0[CLSTR_ID %in% sample_selected,
                 .(CLSTR_ID, PLOT, TREE_NO, AZIMUTH, DISTANCE)]
  vi_d1 <- vi_d1[, SITE_IDENTIFIER := as.numeric(substr(CLSTR_ID, 1, 7))]
  vi_d1 <- vi_d1[!is.na(AZIMUTH) & !is.na(DISTANCE),]
  vi_d1 <- merge(vi_d1,
                 samples[,.(CLSTR_ID, MEAS_DT)],
                 by = "CLSTR_ID",
                 all.x = TRUE)
  vi_d1[, MEAS_YR := as.numeric(substr(MEAS_DT, 1, 4))]

  # selected the stem mapping in most recent measurement
  vi_d1[, maxDT := max(MEAS_YR),
        by = c("SITE_IDENTIFIER", "PLOT", "TREE_NO")]
  vi_d1 <- unique(vi_d1[MEAS_YR == maxDT,
                        .(SITE_IDENTIFIER, TREE_NO, AZIMUTH, DISTANCE, mapped = TRUE)],
                  by = c("SITE_IDENTIFIER", "TREE_NO"))
  ## assign stem mapping infor from vi_d trees in treelist2, named as treelist3
  treelist3 <- merge(treelist1, vi_d1,
                     by = c("SITE_IDENTIFIER", "TREE_NO"),
                     all.x = TRUE)
  ## select stand trees in treelist3, named as treelist4
  if(projectName == "YSM"){
    treelist4 <- treelist3[ST_FL == "S",]
  } else if(projectName == "Taan"){
    treelist4 <- data.table::copy(treelist3)
  } else if(projectName == "special"){
    treelist4 <- data.table::copy(treelist3)
  }
  treelist4[is.na(mapped), mapped := FALSE]
  treelist4[, SP_org := SPECIES]
  ## remove some projects that do not in YSM, CMI and NFI, named as treelist5
  ## correct species in consistent with Rene's species correction to join site index
  treelist5 <- merge(treelist4, samples[,.(CLSTR_ID, TSA, TFL)],
                     by = "CLSTR_ID", all.x = TRUE)
  siteindex[, SITE_IDENTIFIER := as.numeric(substr(CLSTR_ID, 1, 7))]
  if(siteIndexMethod == "byvisit"){
    ## allows the site index change over visits
    ## site index table stay same
  } else if(siteIndexMethod == "average"){
    ## use average site index for a site by species
    ## need attention
    siteindex[, SITE_ave := mean(SITE, na.rm = TRUE),
              by = c("SITE_IDENTIFIER", "SPECIES")]
    siteindex[, SITE := SITE_ave]

    treelist6 <- merge(treelist5,
                       siteindex[,.(CLSTR_ID, SPECIES, SITE)],
                       by = c("CLSTR_ID", "SPECIES"),
                       all.x = TRUE)
  } else if(siteIndexMethod == "firstvisit"){
    # use the first visit
    siteindex <- merge(siteindex,
                       samples[,.(CLSTR_ID, VISIT_NUMBER)],
                       by = "CLSTR_ID")
    ## in the case a species was not in the first visit see example below
    examples <- data.table(CLSTR_ID = c(rep("examplecls1", 3), rep("examplecls1", 2)),
                           SITE = c(30, 40, 50, 26, 28),
                           SPECIES = c(rep("AC", 3), rep("BC", 2)),
                           PROJ_AGE_ADJ = c(25, 27, 29, 27, 29),
                           SITE_IDENTIFIER = 9000009,
                           VISIT_NUMBER = c(1:3, 2:3))
    # CLSTR_ID SITE SPECIES PROJ_AGE_ADJ SITE_IDENTIFIER VISIT_NUMBER
    # 1: examplecls1   30      AC           25         9000009            1
    # 2: examplecls1   40      AC           27         9000009            2
    # 3: examplecls1   50      AC           29         9000009            3
    # 4: examplecls1   26      BC           27         9000009            2
    # 5: examplecls1   28      BC           29         9000009            3
    siteindex <- rbind(siteindex, examples, fill = TRUE)

    siteindex[, first_visit := min(VISIT_NUMBER),
              by = c("SITE_IDENTIFIER", "SPECIES")]
    siteindex[VISIT_NUMBER == first_visit, SITE_fst := SITE]
    siteindex[,SITE_fst := mean(SITE_fst, na.rm = TRUE),
              by = c("SITE_IDENTIFIER", "SPECIES")]
    siteindex[, SITE := SITE_fst]
    ## check the examples
    # siteindex[CLSTR_ID %in% c("examplecls1", "examplecls2"),
    #           .(CLSTR_ID, SITE)]
    # CLSTR_ID SITE
    # 1: examplecls1   30
    # 2: examplecls1   30
    # 3: examplecls1   30
    # 4: examplecls1   26
    # 5: examplecls1   26
  } else if(siteIndexMethod == "closest50"){
    ## there are some cases that the distances to 50 are equal with
    ## one above and one below
    examples <- data.table(CLSTR_ID = c(rep("examplecls1", 3), rep("examplecls1", 2)),
                           SITE = c(30, 40, 50, 26, 28),
                           SPECIES = c(rep("AC", 3), rep("BC", 2)),
                           PROJ_AGE_ADJ = c(25, 27, 29, 40, 60),
                           SITE_IDENTIFIER = 9000009,
                           VISIT_NUMBER = c(1:3, 2:3))
    siteindex <- rbind(siteindex, examples, fill = TRUE)
    # use the site index with closest distance to stand age of 50
    siteindex[, distance_to50 := abs(50 - PROJ_AGE_ADJ)]
    siteindex[, min_dist := min(distance_to50),
              by = c("SITE_IDENTIFIER", "SPECIES")]
    siteindex[distance_to50 == min_dist,
              site_cls := SITE]
    # siteindex[CLSTR_ID %in% c("examplecls1", "examplecls2"),
    #           .(CLSTR_ID, site_cls)]
    # CLSTR_ID site_cls
    # 1: examplecls1       NA
    # 2: examplecls1       NA
    # 3: examplecls1       50
    # 4: examplecls1       26
    # 5: examplecls1       28
    siteindex[distance_to50 == min_dist,
              age_times := length(unique(PROJ_AGE_ADJ)),
              by = c("SITE_IDENTIFIER", "SPECIES")]
    siteindex[age_times > 1 & PROJ_AGE_ADJ < 50,
              site_cls := as.numeric(NA)]
    # siteindex[CLSTR_ID %in% c("examplecls1", "examplecls2"),
    #           .(CLSTR_ID, site_cls)]
    # CLSTR_ID site_cls
    # 1: examplecls1       NA
    # 2: examplecls1       NA
    # 3: examplecls1       50
    # 4: examplecls1       NA
    # 5: examplecls1       28
    siteindex[,site_cls := mean(site_cls, na.rm = TRUE),
              by = c("SITE_IDENTIFIER", "SPECIES")]
    siteindex[, SITE := site_cls]
  } else {
    stop("siteIndexMethod is not correctly specified.")
  }
  siteindex <- siteindex[CLSTR_ID != "examplecls1",]

  treelist6 <- merge(treelist5,
                     siteindex[,.(CLSTR_ID, SPECIES, SITE, AGE = PROJ_AGE_ADJ)],
                     by = c("CLSTR_ID", "SPECIES"),
                     all.x = TRUE)
  # I noticed from the list of species
  # in our YSM data, we should change
  # the following minor species labels
  # before giving to Ken for running TASS:
  # BG : rename to B
  # LA, LT : rename to LW
  # M, MV : rename to MB
  # XC : rename to Pli
  treelist6[SPECIES == "BG",
            SPECIES := "B"]
  treelist6[SPECIES %in% c("LA", "LT"),
            SPECIES := "LW"]
  treelist6[SPECIES %in% c("M", "MV"),
            SPECIES := "MB"]
  treelist6[SPECIES %in% c("XC"),
            SPECIES := "PL"]
  ## get residual flag, name as treelist7
  vic_sa <- readRDS(file.path(inputPath,
                              "compilation_nonPSP_sa",
                              "vi_c.rds")) %>%
    data.table
  tree_residual <- vic_sa[,.(CLSTR_ID, TREE_NO,
                             HTC = HT_BRCH, RESIDUAL = gsub(" ", "", RESIDUAL),
                             WALKTHRU = WALKTHRU_STATUS,
                             CR_CL)]
  treelist7 <- merge(treelist6, tree_residual,
                     by = c("CLSTR_ID", "TREE_NO"),
                     all.x = TRUE)
  treelist7[RESIDUAL %in% c("Y", "R"),
            RESIDUAL_new := "R"]
  treelist7[, ':='(resi_num = length(unique(RESIDUAL_new)),
                   walk_num = length(unique(WALKTHRU))),
            by = c("SITE_IDENTIFIER", "TREE_NO")]
  ##
  inconsistentwalktrees <- treelist7[walk_num > 1,]
  if(nrow(inconsistentwalktrees) > 0){
    inconsistentwalktrees[, maxYear := max(MEAS_YR),
                          by = c("SITE_IDENTIFIER", "TREE_NO")]
    inconsistentwalktrees <- inconsistentwalktrees[MEAS_YR == maxYear,
                                                   .(SITE_IDENTIFIER, TREE_NO, WALKTHRU_crted = WALKTHRU)]
    treelist7 <- merge(treelist7, inconsistentwalktrees,
                       by = c("SITE_IDENTIFIER", "TREE_NO"),
                       all.x = TRUE)
    treelist7[!is.na(WALKTHRU_crted), WALKTHRU := WALKTHRU_crted]
    treelist7[,':='(WALKTHRU_crted = NULL)]
  }
  treelist7[,':='(resi_num = NULL,
                  walk_num = NULL)]
  treelist7[CR_CL %in% c("D", "C"), a := 1]
  treelist7[is.na(a), a := 0]
  treelist7[,num_of_cd := sum(a),
            by = "CLSTR_ID"]
  ## in case all the trees in a clsuter do not have cr_cl marked, hence, force them all to D
  ## so that the garcia's approach will take the 6 tallest trees
  treelist7[num_of_cd == 0,
            CR_CL := "D"]
  ## get tree height vigor, assign them to treelist7, name as treelist8
  ## rm the residual tree
  ## remove btop trees
  if(treeVigorMethod == "mainsub"){
    htvg0 <- treelist7[!is.na(HEIGHT) &
                       LV_D == "L",]
    ## this method is suggested by Rene to select 6 tallest trees in main plot
    ## if can not find 6 trees, adding 1 tallest tree from subplot
    htvg0[DBH >= 9, plot_source := "Main"]
    htvg0[is.na(plot_source), plot_source := "Subplot"]
    htvg0 <- htvg0[order(CLSTR_ID, SPECIES, plot_source, -HEIGHT),]
    htvg0[, sizeorder := 1:length(TREE_NO),
          by = c("CLSTR_ID", "SPECIES", "plot_source")]
    ## select 6 trees from main plot
    htvg_main <- htvg0[plot_source == "Main" &
                         sizeorder <= 6,]
    htvg_main_smry <- htvg_main[,.(num_site_ht = max(sizeorder)),
                                by = c("CLSTR_ID", "SPECIES")]
    htvg_all_clster <- unique(htvg0[,.(CLSTR_ID, SPECIES)])
    htvg_main_smry <- merge(htvg_all_clster,
                            htvg_main_smry,
                            by = c("CLSTR_ID", "SPECIES"),
                            all.x = TRUE)
    htvg_main_smry[is.na(num_site_ht), num_site_ht := 0]
    clsters_needed <- htvg_main_smry[num_site_ht < 6,]$CLSTR_ID
    # select 1 tree from subplot
    htvg_subplot <- htvg0[plot_source == "Subplot" &
                            sizeorder == 1 &
                            CLSTR_ID %in% clsters_needed,]
    htvg1 <- rbind(htvg_main, htvg_subplot)
    ## round 1: remove btop = Y, cr_cl = I and S, resitual = Y
    ## and calculate site_height
    htvg1_1 <- htvg1[BROKEN_TOP_IND %in% c("N", NA),]
    htvg1_1 <- htvg1_1[CR_CL %in% c("D", "C"),]
    htvg1_1 <- htvg1_1[RESIDUAL %in% c("N"),]
    htvg1_1 <- htvg1_1[WALKTHRU %in% c("W", NA),]
    htvg_smry_garcia <- htvg1_1[,.(SITE_HT_garcia = mean(HEIGHT),
                                   NO_OF_TREE = length(HEIGHT)),
                                by = c("CLSTR_ID", "SPECIES")]
    htvg_smry <- merge(htvg_main_smry,
                       htvg_smry_garcia,
                       by = c("CLSTR_ID", "SPECIES"),
                       all.x = TRUE)
    ## round 2: ease the conditions and select again for the 0 tree summary
    htvg_smry_garcia_round2 <- htvg1[,.(SITE_HT_garcia_add = mean(HEIGHT)),
                                     by = c("CLSTR_ID", "SPECIES")]
    htvg_smry <- merge(htvg_smry,
                       htvg_smry_garcia_round2,
                       by = c("CLSTR_ID", "SPECIES"),
                       all.x = TRUE)
    htvg_smry[is.na(SITE_HT_garcia),
              SITE_HT_garcia := SITE_HT_garcia_add]
    htvg_smry <- htvg_smry[,.(CLSTR_ID, SPECIES, SITE_HT = SITE_HT_garcia)]
  }
  treelist8 <- merge(treelist7, htvg_smry,
                     by = c("CLSTR_ID", "SPECIES"),
                     all.x = TRUE)
  missingage <- unique(treelist8[is.na(AGE) | AGE <= 0,
                                 .(CLSTR_ID)],
                       by = "CLSTR_ID")
  missingpoints <- unique(missingage$CLSTR_ID)
  ## cleanup treelist8, name as treelist9
  treelist9 <- treelist8[AGE > 0,
                         .(MGMT_UNIT = as.character(NA),
                           SAMP_ID = SITE_IDENTIFIER,
                           CLSTR_ID,
                           mapped,
                           YEAR = as.numeric(MEAS_YR),
                           PLOT,
                           TREE_NO, SP = SPECIES,
                           L = LV_D,
                           AZIMUTH, DISTANCE,
                           DBH, HT = HEIGHT, HTC, HTVG = HEIGHT/SITE_HT,
                           AGE, SITE, RESID = RESIDUAL_new,
                           DAM_AGENT = NA, DAM_AGENT_HT = NA,
                           BEC,
                           UTM_ZONE = IP_UTM,
                           UTM_NORTHING = IP_NRTH,
                           UTM_EASTING = IP_EAST,
                           WALKTHRU)]
  # if a tree was saw as residual in one msmt, all measurements were residual
  # (see email on 20240909 from Dan)
  residualtrees <- unique(treelist9[RESID == "R",.(SAMP_ID, PLOT, TREE_NO, RESID_new = "R")])
  treelist9 <- merge(treelist9, residualtrees,
                     by = c("SAMP_ID", "PLOT", "TREE_NO"),
                     all.x = TRUE)
  treelist9[is.na(RESID) & !is.na(RESID_new),
            RESID := RESID_new]
  treelist9[, RESID_new := NULL]
  rm(residualtrees)

  # set htvg of residual trees as 0.8
  treelist9[RESID == "R",
            HTVG := 0.8]
  treelist9[HTVG > 1.2,
            HTVG := 1.2]
  treelist9[is.na(HTVG), HTVG := 1] ## assign the average height is the same as tree height
  no_age_sample_points <- unique(treelist9[is.na(AGE),]$SAMP_ID)

  ## add damage agent data, name as treelist10
  dam_agent <- readRDS(file.path(inputPath,
                                 "compilation_nonPSP_sa",
                                 "vi_d.rds")) %>%
    data.table
  dam_agent <- dam_agent[,.(CLSTR_ID,
                            TREE_NO, DAM_AGNA, SEV_A,
                            DAM_AGNB, SEV_B, DAM_AGNC, SEV_C, DAM_AGND, SEV_D,
                            DAM_AGNE, SEV_E)]
  treelist10 <- merge(treelist9, dam_agent,
                      by = c("CLSTR_ID", "TREE_NO"),
                      all.x = TRUE)
  if(projectName == "YSM"){
    for(indiagent in LETTERS[1:5]){
      treelist10[, tempagentcol := unlist(treelist10[, paste0("DAM_AGN", indiagent),
                                                     with = FALSE])]
      treelist10[, tempagentsevcol := unlist(treelist10[, paste0("SEV_", indiagent),
                                                        with = FALSE])]
      treelist10[tempagentsevcol == "TK", tempagentsevcol := "99"]
      treelist10[, c(paste0("DAM_AGN", indiagent), paste0("SEV_", indiagent)) := NULL]
      treelist10[!(YEAR >= 2017 & toupper(tempagentcol) %in% c("DSC", "DSG")),
                 c("tempagentcol", "tempagentsevcol") := ""]
      treelist10[tempagentsevcol == "BC",
                 c("tempagentcol", "tempagentsevcol") := ""]
      treelist10[tempagentsevcol == "SC",
                 tempagentsevcol := "13"]
      treelist10[, ':='(HTC_DAM = substr(tempagentsevcol, 1, 1),
                        ECC_DAM = substr(tempagentsevcol, 2, 2))]
      treelist10[, tempagentsevcol := NULL]
      treelist10[L == "D", c("tempagentcol", "HTC_DAM", "ECC_DAM") := NA]
      treelist10[tempagentcol == "", tempagentcol := NA]
      treelist10[HTC_DAM == "", HTC_DAM := NA]
      treelist10[ECC_DAM == "", ECC_DAM := NA]
      setnames(treelist10, c("tempagentcol", "HTC_DAM", "ECC_DAM"),
               paste0(c("DAM_AGN", "HTC_DAM_", "ECC_DAM_"), indiagent))
      rm(indiagent)
    }
  } else {
    for(indiagent in LETTERS[1:5]){
      treelist10[, tempagentcol := unlist(treelist10[, paste0("DAM_AGN", indiagent),
                                                     with = FALSE])]
      treelist10[, tempagentsevcol := unlist(treelist10[, paste0("SEV_", indiagent),
                                                        with = FALSE])]
      treelist10[, c(paste0("DAM_AGN", indiagent), paste0("SEV_", indiagent)) := NULL]
      treelist10[tempagentcol != "TT",
                 c("tempagentcol", "tempagentsevcol") := ""]
      treelist10[tempagentsevcol == "BC",
                 c("tempagentcol", "tempagentsevcol") := ""]
      treelist10[, ':='(HTC_DAM = substr(tempagentsevcol, 1, 1),
                        ECC_DAM = as.numeric(tempagentsevcol))]
      treelist10[, tempagentsevcol := NULL]
      treelist10[L == "D", c("tempagentcol", "HTC_DAM", "ECC_DAM") := NA]
      treelist10[tempagentcol == "", tempagentcol := NA]
      treelist10[HTC_DAM == "", HTC_DAM := NA]
      treelist10[ECC_DAM == "", ECC_DAM := NA]
      setnames(treelist10, c("tempagentcol", "HTC_DAM", "ECC_DAM"),
               paste0(c("DAM_AGN", "HTC_DAM_", "ECC_DAM_"), indiagent))
      rm(indiagent)
    }
  }



  # .	The former AT (alpine tundra) zone has been replaced by
  # three zones (CMA, BAFA, IMA).
  # For simplicity, treat CMA as MH, and treat IMA / BAFA as ESSF.
  # .	BG (bunch grass).  Treat this as PP.
  treelist10[BEC == "CMA", BEC := "MH"]
  treelist10[BEC %in% c("IMA", "BAFA"), BEC := "ESSF"]
  treelist10[BEC == "BG", BEC := "PP"]
  mapped_smry <- treelist10[,.(mapped_percentage = sum(mapped)/length(mapped),
                               unmapped = length(mapped)-sum(mapped),
                               MEAS_YR = mean(YEAR)),
                            by = "CLSTR_ID"]
  ## select stem mapped clusters, which is defined as more than or equal to 80% trees mapped
  ## or unmapped trees is less than or equal to 5 trees
  mapped_samples <- mapped_smry[unmapped <= 5 | mapped_percentage >= 0.8,]$CLSTR_ID


  ## prepare for stem extention, name as treelist11
  ## revised based on Rene's suggestions
  ## remove "O" type trees, duplicate "W" type trees and assign with
  ## random stem mapping
  treelist11 <- treelist10[CLSTR_ID %in% mapped_samples & L == "L",]
  treelist11 <- treelist11[WALKTHRU != "O" |
                             is.na(WALKTHRU),]
  treelist12_WTrees <- treelist11[WALKTHRU == "W",]
  treelist12_WTrees[,':='(TREE_NO = paste0(TREE_NO, "-W"),
                          mapped = FALSE)]
  treelist11 <- rbind(treelist11, treelist12_WTrees)
  nforsmallplot <- nrow(treelist11[mapped == FALSE & DBH < 9,])
  nforbigplot <- nrow(treelist11[mapped == FALSE & DBH >= 9,])
  azimuth_smallplot <- sample(seq(0, 359, by = 1), nforsmallplot, replace = TRUE)
  dist_smallplot <- sample(seq(0, 5.6, by = 0.1), nforsmallplot, replace = TRUE)
  azimuth_bigplot <- sample(seq(0, 359, by = 1), nforbigplot, replace = TRUE)
  dist_bigplot <- sample(seq(0, 11.2, by = 0.1), nforbigplot, replace = TRUE)


  treelist11[mapped == FALSE & DBH < 9,
             ':='(AZIMUTH = azimuth_smallplot, DISTANCE = dist_smallplot)]
  treelist11[mapped == FALSE & DBH >= 9,
             ':='(AZIMUTH = azimuth_bigplot, DISTANCE = dist_bigplot)]


  treelist11[, PLOT := "M"]
  treelist11[DBH < 9, PLOT := "S"]
  ## drop trees
  treelist11 <- treelist11[!(DISTANCE > 5.64 & DBH < 9),]
  treelist_smallplot <- treelist11[PLOT == "S",]
  treelist_bigplot <- treelist11[PLOT == "M",]
  allsites <- unique(treelist11$SAMP_ID)

  if(!is.na(randomSeed)){
    set.seed(randomSeed)
  }
  for (indisite in allsites){
    # indicluster <- allclusters[1]
    treelist_smallplot_indisite <- unique(treelist_smallplot[SAMP_ID == indisite,
                                                             .(SAMP_ID, TREE_NO,
                                                               AZIMUTH, DISTANCE)],
                                          by = c("SAMP_ID", "TREE_NO"))
    if(nrow(treelist_smallplot_indisite) > 0){
      treelist_smalltrees <- stemMappingExtension(objectID = treelist_smallplot_indisite$TREE_NO,
                                                  bearing = treelist_smallplot_indisite$AZIMUTH,
                                                  distance = treelist_smallplot_indisite$DISTANCE,
                                                  plotRadius = 5.64,
                                                  randomRotate = TRUE)
    } else {
      treelist_smalltrees <- data.table(hexagonID = numeric(),
                                        objectID = numeric(),
                                        x = numeric(),
                                        y = numeric())
    }
    treelist_bigplot_indisite <- unique(treelist_bigplot[SAMP_ID == indisite,
                                                         .(SAMP_ID, TREE_NO,
                                                           AZIMUTH, DISTANCE)],
                                        by = c("SAMP_ID", "TREE_NO"))
    if(nrow(treelist_bigplot_indisite) > 0){
      treelist_bigtrees <- stemMappingExtension(objectID = treelist_bigplot_indisite$TREE_NO,
                                                bearing = treelist_bigplot_indisite$AZIMUTH,
                                                distance = treelist_bigplot_indisite$DISTANCE,
                                                plotRadius = 11.28,
                                                randomRotate = TRUE)
    } else {
      treelist_bigtrees <- data.table(hexagonID = numeric(),
                                      objectID = numeric(),
                                      x = numeric(),
                                      y = numeric())
    }

    alltree_indisite <- rbind(treelist_smalltrees[, PLOT := "S"],
                              treelist_bigtrees[, PLOT := "M"])
    alltree_indisite[, SAMP_ID := indisite]
    if(indisite == allsites[1]){
      alltree_allsites <- alltree_indisite
    } else {
      alltree_allsites <- rbind(alltree_allsites, alltree_indisite)
    }
  }
  setnames(alltree_allsites, "objectID", "TREE_NO")
  alltree_allsites[, nuPlot := length(unique(PLOT)),
                   by = c("SAMP_ID", "TREE_NO")]
  alltree_allsites[nuPlot > 1]
  alltree_allsites[, nuPlot := NULL]
  alltree_clusters <- merge(unique(treelist11[,.(CLSTR_ID, SAMP_ID)]),
                            alltree_allsites,
                            by = "SAMP_ID",
                            all = TRUE,
                            allow.cartesian = TRUE)
  rm(alltree_allsites)
  treelist12 <- merge(alltree_clusters,
                      treelist11[,.(CLSTR_ID, TREE_NO, PLOT, SP, DBH,
                                    HT, HTC, HTVG, AGE, SITE,
                                    RESID, BEC,
                                    DAM_AGNA, HTC_DAM_A, ECC_DAM_A,
                                    DAM_AGNB, HTC_DAM_B, ECC_DAM_B,
                                    DAM_AGNC, HTC_DAM_C, ECC_DAM_C,
                                    DAM_AGND, HTC_DAM_D, ECC_DAM_D,
                                    DAM_AGNE, HTC_DAM_E, ECC_DAM_E,
                                    L, WALKTHRU, intreelist = TRUE)],
                      by = c("CLSTR_ID", "TREE_NO", "PLOT"),
                      all.x = TRUE)
  treelist12 <- treelist12[intreelist == TRUE,]
  treelist12[, intreelist := NULL]

  ## extend it to 1 ha
  treelist12[, TREE_NO := paste0(hexagonID, "_", TREE_NO)]
  treelist12[, ':='(hexagonID = NULL)]
  treelist12[, ':='(x = x + 50, y = y - 50)]
  treelist12[, L := NULL]
  treelist12[is.na(RESID), RESID := "REG"]
  treelist12 <- treelist12[,.(CLSTR_ID, SP, TREE_NO, x, y, AGE, SITE, HTVG,
                              HT, HTC, DBH, RESID, BEC,
                              DAM_AGNA, HTC_DAM_A, ECC_DAM_A,
                              DAM_AGNB, HTC_DAM_B, ECC_DAM_B,
                              DAM_AGNC, HTC_DAM_C, ECC_DAM_C,
                              DAM_AGND, HTC_DAM_D, ECC_DAM_D,
                              DAM_AGNE, HTC_DAM_E, ECC_DAM_E)]
  treelist12[is.na(HTC), HTC := HT/2]
  allmappedsamples <- unique(treelist12$CLSTR_ID)
  treelist12[BEC %in% c("CDF", "CWH", "MH") &
               SP %in% c("CW", "HW", "FD", "PL"),
             species_new := paste0(substr(SP, 1, 1),
                                   tolower(substr(SP, 2, 2)),
                                   "c")]
  treelist12[is.na(species_new) &
               SP %in% c("CW", "HW", "FD", "PL"),
             species_new := paste0(substr(SP, 1, 1),
                                   tolower(substr(SP, 2, 2)),
                                   "i")]
  treelist12[!is.na(species_new),
             SP := species_new]
  treelist12[, species_new := NULL]
  treelist12 <- treelist12[order(CLSTR_ID),]
  summarytable[CLSTR_ID %in% treelist12$CLSTR_ID,
               output_for_stemmapped := "yes"]
  summarytable[is.na(output_for_stemmapped),
               output_for_stemmapped := "no"]
  samples_for_tassversion <- unique(siteindex[,.(CLSTR_ID,
                                                 TASS_VERSION)])
  trees_with_xy_all <- merge(samples_for_tassversion,
                             treelist12,
                             by = "CLSTR_ID",
                             all.y = TRUE)
  trees_with_xy_all <- trees_with_xy_all[,.(TASS_VERSION, CLSTR_ID, SP, TREE_NO, x, y, AGE, SITE, HTVG,
                                            HT, HTC, DBH, RESID, BEC,
                                            DAM_AGNA, HTC_DAM_A, ECC_DAM_A,
                                            DAM_AGNB, HTC_DAM_B, ECC_DAM_B,
                                            DAM_AGNC, HTC_DAM_C, ECC_DAM_C,
                                            DAM_AGND, HTC_DAM_D, ECC_DAM_D,
                                            DAM_AGNE, HTC_DAM_E, ECC_DAM_E)]
  write.csv(trees_with_xy_all, na = "",
            file.path(outputPath, "trees_with_xy_all.csv"),
            row.names = FALSE)
  if(vigorAdjust08){
    ### make sure the mean of the HTVG is 0.8
    ### a suggesion by Rene on March 17, 2023

    meanHTVG <- trees_with_xy_all[, .(HTVG_mean = mean(HTVG)-0.8),
                                  by = c("CLSTR_ID", "SP")]
    trees_with_xy_all <- merge(trees_with_xy_all,
                               meanHTVG,
                               by = c("CLSTR_ID", "SP"),
                               all.x = TRUE)

    trees_with_xy_all[, HTVG := HTVG - HTVG_mean]
    trees_with_xy_all[, HTVG_mean := NULL]
    trees_with_xy_all <- trees_with_xy_all[,.(TASS_VERSION, CLSTR_ID, SP, TREE_NO, x, y, AGE, SITE, HTVG,
                                              HT, HTC, DBH, RESID, BEC,
                                              DAM_AGNA, HTC_DAM_A, ECC_DAM_A,
                                              DAM_AGNB, HTC_DAM_B, ECC_DAM_B,
                                              DAM_AGNC, HTC_DAM_C, ECC_DAM_C,
                                              DAM_AGND, HTC_DAM_D, ECC_DAM_D,
                                              DAM_AGNE, HTC_DAM_E, ECC_DAM_E)]
    write.csv(trees_with_xy_all, na = "",
              file.path(outputPath, "trees_with_xy_all_withVGadj.csv"),
              row.names = FALSE)
  }

  trees_withoutxy <- data.table::copy(treelist10)
  trees_withoutxy[, PLOT := "M"]
  trees_withoutxy[DBH < 9, PLOT := "S"]

  trees_withoutxy[PLOT == "M", PLOT_AREA := 0.04]
  trees_withoutxy[PLOT == "S", PLOT_AREA := 0.01]

  trees_withoutxy[SP == "PL", UTIL := 12.5]
  trees_withoutxy[SP != "PL", UTIL := 17.5]
  trees_withoutxy[, PLANT_NAT := "Natural"]
  trees_withoutxy[, PLOT := NULL]
  setnames(trees_withoutxy, "PLOT_AREA", "PLOT")
  ## remove "O" walkthrough trees
  ## duplicate "W" trees
  trees_withoutxy_cls_temp <- unique(trees_withoutxy$CLSTR_ID)
  trees_withoutxy <- trees_withoutxy[WALKTHRU != "O" |
                                       is.na(WALKTHRU),]
  summarytable[CLSTR_ID %in% trees_withoutxy_cls_temp &
                 !(CLSTR_ID %in% trees_withoutxy$CLSTR_ID),
               all_trees_out_of_plot := "yes"]
  summarytable[is.na(all_trees_out_of_plot),
               all_trees_out_of_plot := "no"]
  trees_withoutxy_WTrees <- trees_withoutxy[WALKTHRU == "W",]
  trees_withoutxy_WTrees[, TREE_NO := paste0(TREE_NO, "-W")]
  trees_withoutxy <- rbind(trees_withoutxy, trees_withoutxy_WTrees)
  trees_withoutxy[is.na(RESID), RESID := "REG"]
  trees_withoutxy <- trees_withoutxy[,.(MGMT_UNIT, SAMP_ID, MEAS_NO = 0,
                                        CLSTR_ID, YEAR, PLOT, TREE_NO, SP, L,
                                        DBH, HT, HTC, HTVG, AGE, SITE,
                                        RESID, DAM_AGENT,
                                        DAM_AGENT_HT, PLANT_NAT, UTIL, BEC,
                                        DAM_AGNA, HTC_DAM_A, ECC_DAM_A,
                                        DAM_AGNB, HTC_DAM_B, ECC_DAM_B,
                                        DAM_AGNC, HTC_DAM_C, ECC_DAM_C,
                                        DAM_AGND, HTC_DAM_D, ECC_DAM_D,
                                        DAM_AGNE, HTC_DAM_E, ECC_DAM_E)]
  trees_withoutxy[is.na(HTC), HTC := HT/2]
  trees_withoutxy[BEC %in% c("CDF", "CWH", "MH") &
                    SP %in% c("CW", "HW", "FD", "PL"),
                  species_new := paste0(substr(SP, 1, 1),
                                        tolower(substr(SP, 2, 2)),
                                        "c")]
  trees_withoutxy[is.na(species_new) &
                    SP %in% c("CW", "HW", "FD", "PL"),
                  species_new := paste0(substr(SP, 1, 1),
                                        tolower(substr(SP, 2, 2)),
                                        "i")]
  trees_withoutxy[!is.na(species_new),
                  SP := species_new]
  trees_withoutxy[, species_new := NULL]
  trees_withoutxy <- trees_withoutxy[order(CLSTR_ID),]

  summarytable[CLSTR_ID %in% trees_withoutxy$CLSTR_ID,
               output_for_unstemmapped := "yes"]
  summarytable[is.na(output_for_unstemmapped),
               output_for_unstemmapped := "no"]
  summarytable[CLSTR_ID %in% trees_withoutxy[is.na(SITE) &
                                               L == "L"]$CLSTR_ID,
               has_missing_site_index := "yes"]
  summarytable[is.na(has_missing_site_index),
               has_missing_site_index := "no"]
  summarytable <- merge(summarytable,
                        mapped_smry,
                        by = "CLSTR_ID",
                        all.x = TRUE)

  trees_without_xy_all <- merge(samples_for_tassversion,
                                trees_withoutxy,
                                by = "CLSTR_ID",
                                all.y = TRUE)
  trees_without_xy_all <- trees_without_xy_all[,.(TASS_VERSION, MGMT_UNIT, SAMP_ID, MEAS_NO = 0,
                                                  CLSTR_ID, YEAR, PLOT, TREE_NO, SP, L,
                                                  DBH, HT, HTC, HTVG, AGE, SITE,
                                                  RESID, DAM_AGENT,
                                                  DAM_AGENT_HT, PLANT_NAT, UTIL, BEC,
                                                  DAM_AGNA, HTC_DAM_A, ECC_DAM_A,
                                                  DAM_AGNB, HTC_DAM_B, ECC_DAM_B,
                                                  DAM_AGNC, HTC_DAM_C, ECC_DAM_C,
                                                  DAM_AGND, HTC_DAM_D, ECC_DAM_D,
                                                  DAM_AGNE, HTC_DAM_E, ECC_DAM_E)]
  trees_without_xy_all[L == "D",
                       SITE := NA]
  trees_without_xy_all[, c("DAM_AGENT", "DAM_AGENT_HT") := NULL]
  write.csv(trees_without_xy_all,
            file.path(outputPath,
                      "trees_without_xy_all.csv"),
            na = "",
            row.names = FALSE)
  if(vigorAdjust08){
    trees_without_xy_all[, phf_tree := 1/PLOT]
    htgv_mean <- trees_without_xy_all[, .(htgv_mean = sum(HTVG * phf_tree)/sum(phf_tree) - 0.8),
                                      by = c("CLSTR_ID", "SP")]

    trees_without_xy_all <- merge(trees_without_xy_all,
                                  htgv_mean,
                                  by = c("CLSTR_ID", "SP"),
                                  all.x = TRUE)
    trees_without_xy_all[, HTVG := HTVG - htgv_mean]
    ## for check
    htgv_mean_check <- trees_without_xy_all[, .(htgv_mean = sum(HTVG * phf_tree)/sum(phf_tree)),
                                            by = c("CLSTR_ID", "SP")]
    trees_without_xy_all <- trees_without_xy_all[,.(TASS_VERSION, MGMT_UNIT, SAMP_ID, MEAS_NO,
                                                    CLSTR_ID, YEAR, PLOT, TREE_NO, SP, L,
                                                    DBH, HT, HTC, HTVG, AGE, SITE,
                                                    RESID, PLANT_NAT, UTIL, BEC,
                                                    DAM_AGNA, HTC_DAM_A, ECC_DAM_A,
                                                    DAM_AGNB, HTC_DAM_B, ECC_DAM_B,
                                                    DAM_AGNC, HTC_DAM_C, ECC_DAM_C,
                                                    DAM_AGND, HTC_DAM_D, ECC_DAM_D,
                                                    DAM_AGNE, HTC_DAM_E, ECC_DAM_E)]
    write.csv(trees_without_xy_all,
              file.path(outputPath,
                        "trees_without_xy_all_withVGadj.csv"),
              na = "",
              row.names = FALSE)
  }

  processtable <- list(prepare_briefing = logfile,
                       process_table = summarytable)
  write.xlsx(processtable,
             file.path(outputPath, "readme.xlsx"),
             overwrite = TRUE)
}


