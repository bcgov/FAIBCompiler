#' load json data from handheld and save tables into target folder
#'
#'
#' @description This function is to load JSON file data from FAIB new handheld.
#'
#' @param fileName character, JSON file name.
#' @param savePath character, A folder path user wants to save all the outputs.
#'                            Default is the current work directory.
#' @param saveFormat character, Specifies the format user wants to save. Currently, the
#'                              function supports \code{xlsx} and \code{rdata}. Default is
#'                              \code{rdata}.
#' @param saveName character, Specifies the save name.
#' @param overWrite logical, Determine if the file with same name as user specifies
#'                           will be overwritten. Default is \code{FALSE}.
#'
#'
#' @return No value will be returned. All the outputs will be saved in a target folder.
#'
#' @importFrom data.table ':=' 'data.table'
#' @importFrom jsonlite 'fromJSON'
#' @examples
#' \dontrun{
#' ## given, in the current work directory, there is a ismc-file-transfer.json file
#'    icmcjsonFile <- "ismc-file-transfer.json"
#' ## create a folder in current work directory and save the outputs into it
#'    dir.create("ismc_test_folder")
#'
#'    readHandHeld(fileName = icmcjsonFile,
#'                 savePath = file.path(getwd(), "ismc_test_folder"))
#' }
#' @export
#' @docType methods
#' @rdname readHandHeld
#'
#' @author Yong Luo
#'
readHandHeld <- function(fileName, savePath = getwd(),
                         saveFormat = "rdata",
                         saveName = "myHandHeldFile",
                         overWrite = FALSE){
  if(overWrite & file.exists(file.path(savePath, paste0(saveName, ".", saveFormat)))){
    file.remove(file.path(savePath, paste0(saveName, ".", saveFormat)))
    warning("The original file has been overwritten.")
  }

  if(!overWrite & file.exists(file.path(savePath, paste0(saveName, ".", saveFormat)))){
    cat("Please use different file name, as it is in use. Or turn on overWrite arguement to overwrite the current file.")
    thisopt <- options(show.error.messages = FALSE)
    on.exit(options(thisopt))
    stop()
  }

  thedata <- jsonlite::fromJSON(fileName, flatten = FALSE)
  jsondata <- thedata
  cat("The field data was collected in HandHeld verion", thedata$Version, ". \n")
  GroundSampleHumanResources <- thedata$GroundSampleHumanResources
  GroundSampleHumanResources46certificate <- GroundSampleHumanResources$CrewMemberCertifications
  GroundSampleHumanResources <- data.table(GroundSampleHumanResources)
  GroundSampleHumanResources[, CrewMemberCertifications := NULL]
  allcrewcertificates <- data.table(CertificationTypeCode = character(),
                                    CrewMemberCertificationGuid = character(),
                                    GroundSampleHumanRsrceGuid = character())
  for (i in 1:length(GroundSampleHumanResources46certificate)){
    inditable <- GroundSampleHumanResources46certificate[[i]]
    if(nrow(inditable) > 0){
      allcrewcertificates <- rbindlist(list(data.table(allcrewcertificates),
                                            data.table(inditable)),
                                       fill = TRUE)
    }
  }
  rm(i, inditable)
  allcrewcertificates <- unique(allcrewcertificates[, CertificationTypeCode_all := paste(CertificationTypeCode, collapse = ", "),
                                                    by = "GroundSampleHumanRsrceGuid"],
                                by = "GroundSampleHumanRsrceGuid")
  allcrewcertificates[, CertificationTypeCode := NULL]
  GroundSampleHumanResources <- merge(GroundSampleHumanResources, allcrewcertificates,
                                      by = "GroundSampleHumanRsrceGuid",
                                      all.x = TRUE)
  GroundSampleHumanResources <- data.table(GroundSampleHumanResources)
  rm(allcrewcertificates, GroundSampleHumanResources46certificate)

  GroundSampleProjects <- thedata$GroundSampleProjects
  checkHandheldTableColNames("GroundSampleProjects",
                             names(GroundSampleProjects))
  allFileNames <- c("AccessNotes", "GroundSampleCrewActivities",
                    "IntegratedPlotCenter", "LogAssessments", "PlotDetails", "SitePlots",
                    "PointLocation", "ReferencePoint", "SampleMeasurements", "SampleSites",
                    "SampleSiteVisits", "SiteNavigation", "SmallLiveTreeTallies",
                    "StumpTallies", "TiePoint", "TreeDamageOccurrences",
                    "TreeDetails", "TreeLossIndicators", "TreeMeasurements", "Trees",
                    "VriSampleMeasurement", "VriTreeMeasurement")
  SampleSites_all <- GroundSampleProjects$SampleSites
  GroundSampleProjects$SampleSites <- NULL
  rm(thedata)
  for(indiproj in 1:length(SampleSites_all)){
    ## need attension, as it may contain multiple sites
    SampleSites <- SampleSites_all[[indiproj]] ## here
    checkHandheldTableColNames("SampleSites",
                               names(SampleSites))
    AccessNotes <- data.table(AccessNoteGuid = character(),
                              OdometerIncrement = character(),
                              OdometerReading = character(),
                              Remarks = character(),
                              SampleSiteGuid = character(),
                              SequenceNumber = character())
    for (i in 1: length(SampleSites$AccessNotes)){
      indinote <- SampleSites$AccessNotes[[i]]
      if (class(indinote) == "data.frame"){
        AccessNotes <- rbindlist(list(AccessNotes, indinote), fill = TRUE)
      }
    }
    rm(i, indinote)
    SampleSites$AccessNotes <- NULL

    checkHandheldTableColNames("AccessNotes",
                               names(AccessNotes))

    for(i in 1:length(SampleSites$Plots)){
      if(i == 1){
        indiplot <- SampleSites$Plots[[i]]
        for (j in 1:length(indiplot$Trees)){
          inditree <- indiplot$Trees[[j]]
          if(j == 1){
            if(nrow(inditree) > 0){
              Trees <- inditree
            } else {
              Trees <- data.table(TreeGuid = character())
            }
          } else if (nrow(inditree) > 0){
            Trees <- rbindlist(list(Trees, inditree), fill = TRUE)
          }
        }
        rm(j)
        indiplot$Trees <- NULL
        SitePlots <- indiplot
      } else {
        indiplot <- SampleSites$Plots[[i]]
        for (j in 1:length(indiplot$Trees)){
          inditree <- indiplot$Trees[[j]]
          Trees <- rbindlist(list(Trees, inditree), fill = TRUE)
        }
        rm(j)
        indiplot$Trees <- NULL
        SitePlots <- rbindlist(list(SitePlots, indiplot), fill = TRUE)
      }
      rm(i)
    }
    rm(indiplot, inditree)
    SampleSites$Plots <- NULL

    checkHandheldTableColNames("Plots",
                               names(SitePlots))

    checkHandheldTableColNames("Trees",
                               names(Trees))

    PointLocation <- SampleSites$PointLocation
    SampleSites$PointLocation <- NULL
    checkHandheldTableColNames("PointLocation",
                               names(PointLocation))

    ## currently do not support multiple sites or zero site
    if (length(SampleSites$SampleSiteVisits) == 0){
      stop("This site has no visit.")
    }
    for (i in 1:length(SampleSites$SampleSiteVisits)){
      if(i == 1){
        SampleSiteVisits <- SampleSites$SampleSiteVisits[[i]]
        SiteNavigation <- list()
        SiteNavigation[[i]] <- SampleSiteVisits$SiteNavigation
        SampleSiteVisits$SiteNavigation <- NULL
      } else {
        temptable <- SampleSites$SampleSiteVisits[[i]]
        SiteNavigation[[i]] <- temptable$SiteNavigation
        temptable$SiteNavigation <- NULL
        SampleSiteVisits <- rbindlist(list(SampleSiteVisits,
                                           temptable),
                                      fill = TRUE)
        rm(temptable)
      }
    }



    checkHandheldTableColNames("SampleSiteVisits",
                               names(SampleSiteVisits))


    GroundSampleCrewActivities <- SampleSiteVisits$GroundSampleCrewActivities
    for(i in 1:length(GroundSampleCrewActivities)){
      GroundSampleCrewActivities_ind <- GroundSampleCrewActivities[[i]]
      if(i == 1){
        GroundSampleCrewActivities_all <- GroundSampleCrewActivities_ind
      } else {
        GroundSampleCrewActivities_all <- rbindlist(list(GroundSampleCrewActivities_all,
                                                      GroundSampleCrewActivities_ind),
                                                    fill = TRUE)
      }
    }
    GroundSampleCrewActivities <- GroundSampleCrewActivities_all
    rm(GroundSampleCrewActivities_all, GroundSampleCrewActivities_ind,
       i)
    GroundSampleCrewActivities <- merge(GroundSampleCrewActivities,
                                        GroundSampleHumanResources,
                                        by = "GroundSampleHumanRsrceGuid",
                                        all.x = TRUE)
    SampleSiteVisits$GroundSampleCrewActivities <- NULL

    for (i in 1:length(SampleSiteVisits$PlotDetails)){
      if(i == 1){
        PlotDetails <- SampleSiteVisits$PlotDetails[[i]]
      } else {
        PlotDetails <- rbindlist(list(PlotDetails, SampleSiteVisits$PlotDetails[[i]]),
                                 fill = TRUE)
      }
    }
    rm(i)
    SampleSiteVisits$PlotDetails <- NULL
    checkHandheldTableColNames("PlotDetails",
                               names(PlotDetails))

    if(length(SampleSiteVisits$SampleMeasurements) == 0){
      stop("There is no sample measurement.")
    }

    for (i in 1:length(SampleSiteVisits$SampleMeasurements)){
      if(i == 1){
        SampleMeasurements <- SampleSiteVisits$SampleMeasurements[[i]]
        SmallLiveTreeTallies <- SampleMeasurements$SmallLiveTreeTallies
        SampleMeasurements$SmallLiveTreeTallies <- NULL
        TreeMeasurements <- SampleMeasurements$TreeMeasurements
        SampleMeasurements$TreeMeasurements <- NULL
        VriSampleMeasurement <- list()
        VriSampleMeasurement[[i]] <- SampleMeasurements$VriSampleMeasurement
        SampleMeasurements$VriSampleMeasurement <- NULL

      } else {
        temptable <- SampleSiteVisits$SampleMeasurements[[i]]
        SmallLiveTreeTallies <- append(SmallLiveTreeTallies,
                                       temptable$SmallLiveTreeTallies)
        temptable$SmallLiveTreeTallies <- NULL
        TreeMeasurements <- append(TreeMeasurements,
                                   temptable$TreeMeasurements)
        temptable$TreeMeasurements <- NULL
        VriSampleMeasurement[[i]] <- temptable$VriSampleMeasurement
        temptable$VriSampleMeasurement <- NULL

        SampleMeasurements <- rbindlist(list(SampleMeasurements,
                                             temptable),
                                        fill = TRUE)
        rm(temptable)
      }
    }
    SampleSiteVisits$SampleMeasurements <- NULL
    checkHandheldTableColNames("SampleMeasurements",
                               names(SampleMeasurements))

    SmallLiveTreeTallies_all <- data.table(CommentText = character(),
                                           NumberOfTrees = character(),
                                           SampleMeasurementGuid = character(),
                                           SmallLiveTreeTallyGuid = character(),
                                           SmallTreeTallyClassCode = character(),
                                           TreeSpeciesCode = character())
    for(i in 1:length(SmallLiveTreeTallies)){
      if(class(SmallLiveTreeTallies[[i]]) == "data.frame"){
        SmallLiveTreeTallies_all <- rbindlist(list(SmallLiveTreeTallies_all,
                                                   SmallLiveTreeTallies[[i]]),
                                              fill = TRUE)
      }
    }
    SmallLiveTreeTallies <- SmallLiveTreeTallies_all
    rm(i, SmallLiveTreeTallies_all)
    checkHandheldTableColNames("SmallLiveTreeTallies",
                               names(SmallLiveTreeTallies))


    ## I do not know why tree measurements have three tables
    ## for the L type sample, one item in this list
    for(i in 1:length(TreeMeasurements)){
      if(i == 1){
        if(nrow(TreeMeasurements[[i]]) == 0){
          stop("The second and third table have tree measurements data. please take a look.")
        } else {
          TreeMeasurements_all <- TreeMeasurements[[i]]
          TreeDamageOccurrences <- TreeMeasurements_all$TreeDamageOccurrences
          TreeMeasurements_all$TreeDamageOccurrences <- NULL

          TreeLossIndicators <- TreeMeasurements_all$TreeLossIndicators
          TreeMeasurements_all$TreeLossIndicators <- NULL

          VriTreeMeasurement <- TreeMeasurements_all$VriTreeMeasurement
          TreeMeasurements_all$VriTreeMeasurement <- NULL

        }
      } else {
        if(nrow(TreeMeasurements[[i]]) > 0){
          TreeMeasurements_temp <- TreeMeasurements[[i]]
          TreeDamageOccurrences <- append(TreeDamageOccurrences,
                                          TreeMeasurements_temp$TreeDamageOccurrences)
          TreeMeasurements_temp$TreeDamageOccurrences <- NULL

          TreeLossIndicators <- append(TreeLossIndicators,
                                       TreeMeasurements_temp$TreeLossIndicators)
          TreeMeasurements_temp$TreeLossIndicators <- NULL

          VriTreeMeasurement <- rbindlist(list(VriTreeMeasurement,
                                               TreeMeasurements_temp$VriTreeMeasurement),
                                          fill = TRUE)
          TreeMeasurements_temp$VriTreeMeasurement <- NULL

          TreeMeasurements_all <- rbindlist(list(TreeMeasurements_all,
                                                 TreeMeasurements_temp),
                                            fill = TRUE)
          rm(TreeMeasurements_temp)
        }
      }
    }
    TreeMeasurements <- TreeMeasurements_all
    rm(i, TreeMeasurements_all)
    checkHandheldTableColNames("TreeMeasurements",
                               names(TreeMeasurements))

    allnames <- c()
    for(i in 1:length(TreeDamageOccurrences)){
      allnames <- c(allnames, names(TreeDamageOccurrences[[i]]))
    }
    rm(i)
    allnames <- sort(unique(allnames))
    if(!is.null(allnames)){
      checkHandheldTableColNames("TreeDamageOccurrences",
                                 allnames)
    }

    rm(allnames)
    TreeDamageOccurrences_all <- data.table(DamageAgentSeverityGuid = character(),
                                            DamageOrder = character(),
                                            DamageStemHeight = character(),
                                            SeverityRatingValue = character(),
                                            TreeDamageOccurrenceGuid = character(),
                                            TreeMeasurementGuid = character())
    for(i in 1:length(TreeDamageOccurrences)){
      if(class(TreeDamageOccurrences[[i]]) == "data.frame"){
        TreeDamageOccurrences_all <- rbindlist(list(TreeDamageOccurrences_all,
                                                    TreeDamageOccurrences[[i]]),
                                               fill = TRUE)
      }
      rm(i)
    }
    TreeDamageOccurrences <- TreeDamageOccurrences_all
    rm(TreeDamageOccurrences_all)




    allnames <- c()
    for(i in 1:length(TreeLossIndicators)){
      allnames <- c(allnames, names(TreeLossIndicators[[i]]))
    }
    rm(i)
    allnames <- sort(unique(allnames))
    if(!is.null(allnames)){
      checkHandheldTableColNames("TreeLossIndicators",
                                 allnames)
    }
    rm(allnames)

    TreeLossIndicators_all <- data.table(FormCode = character(),
                                         FormSeverityImpactCode = character(),
                                         Frequency = character(),
                                         IndicatorWidth = character(),
                                         LocationFrom = character(),
                                         LocationTo = character(),
                                         ScarOccurrenceCode = character(),
                                         StemsBelowDbh = character(),
                                         StemScarOccurrenceCode = character(),
                                         StemScarSeverityCode = character(),
                                         TreeLossIndicatorCode = character(),
                                         TreeLossIndicatorGuid = character(),
                                         TreeLossIndLocnCode = character(),
                                         TreeMeasurementGuid = character())
    for(i in 1:length(TreeLossIndicators)){
      if(class(TreeLossIndicators[[i]]) == "data.frame"){
        TreeLossIndicators_all <- rbindlist(list(TreeLossIndicators_all,
                                                 TreeLossIndicators[[i]]),
                                            fill = TRUE)
      }
    }
    TreeLossIndicators <- TreeLossIndicators_all
    rm(i, TreeLossIndicators_all)
    onlypsp <- "PSP" %in% unique(SampleSiteVisits$SampleSitePurposeTypeCode)
    if(!onlypsp){

      checkHandheldTableColNames("VriTreeMeasurement",
                                 names(VriTreeMeasurement))


      LogAssessments <- VriTreeMeasurement$LogAssessments
      allnames <- c()
      for(i in 1:length(LogAssessments)){
        allnames <- c(allnames, names(LogAssessments[[i]]))
      }
      rm(i)
      allnames <- sort(unique(allnames))
      if(!is.null(allnames)){
        checkHandheldTableColNames("LogAssessments",
                                   allnames)
      }
      rm(allnames)
      LogAssessments_all <- data.table(LastLogInd = character(),
                                       LogAssessmentGuid = character(),
                                       LogGradeCode = character(),
                                       LogLength = character(),
                                       LogNumber = character(),
                                       PercentSound = character(),
                                       VriTreeMeasurementGuid = character())
      for(i in 1:length(LogAssessments)){
        if(class(LogAssessments[[i]]) == "data.frame"){
          LogAssessments_all <- rbindlist(list(LogAssessments_all,
                                               LogAssessments[[i]]),
                                          fill = TRUE)
        }
      }
      LogAssessments <- LogAssessments_all
      rm(i, LogAssessments_all)
      VriTreeMeasurement$LogAssessments <- NULL
    } else {
      VriTreeMeasurement <- data.table(TreeMeasurementGuid = character(),
                                       VriTreeMeasurementGuid = character())
      LogAssessments <- data.table(VriTreeMeasurementGuid = character())
    }
    TreeMeasurements$VriTreeMeasurement <- NULL

    VriSampleMeasurement_all <- data.table(CommentText = character(),
                                           SampleMeasurementGuid = character(),
                                           VriSampleMeasurementGuid = character())
    StumpTallies <- list()
    for(i in 1:length(VriSampleMeasurement)){
      if(class(VriSampleMeasurement[[i]]) == "data.frame"){
        checkHandheldTableColNames("VriSampleMeasurement",
                                   names(VriSampleMeasurement[[i]]))
        StumpTallies <- append(StumpTallies,
                               VriSampleMeasurement[[i]]$StumpTallies)
        VriSampleMeasurement[[i]]$StumpTallies <- NULL
        VriSampleMeasurement_all <- rbindlist(list(VriSampleMeasurement_all,
                                                   VriSampleMeasurement[[i]]),
                                              fill = TRUE)

      }
    }
    VriSampleMeasurement <- VriSampleMeasurement_all
    rm(VriSampleMeasurement_all)
    ## I do not know why the first item in the list is NULL, second is a list and the last is a table
    StumpTallies_all <- data.table(BarkRetentionCode = character(),
                                   CommentText = character(),
                                   DamageAgentCode = character(),
                                   Diameter = character(),
                                   Length = character(),
                                   SoundWoodPercent = character(),
                                   StumpOccurrenceFrequency = character(),
                                   StumpTallyGuid = character(),
                                   TreeSpeciesCode = character(),
                                   VriSampleMeasurementGuid = character(),
                                   WildlifeUseCode = character(),
                                   WoodConditionCode = character())
    if(length(StumpTallies) > 0){
      for(i in 1:length(StumpTallies)){
        if(class(StumpTallies[[i]]) == "data.frame"){
          StumpTallies_all <- rbind(StumpTallies_all,
                                    StumpTallies[[i]])
        }
      }
    }

    StumpTallies <- StumpTallies_all
    rm(i, StumpTallies_all)
    checkHandheldTableColNames("StumpTallies",
                               names(StumpTallies))


    for(i in 1:length(SiteNavigation)){
      if(i == 1){
        SiteNavigation_all <- SiteNavigation[[i]]
        IntegratedPlotCenter <- SiteNavigation_all$IntegratedPlotCenter
        IntegratedPlotCenter <- merge(IntegratedPlotCenter,
                                      IntegratedPlotCenter$PointLocation,
                                      by = "PointLocationGuid",
                                      all = TRUE)

        SiteNavigation_all$IntegratedPlotCenter <- NULL

        SiteNavigation_PointLocation <- SiteNavigation_all$PointLocation
        SiteNavigation_all$PointLocation <- NULL

        ReferencePoint <- SiteNavigation_all$ReferencePoint
        ReferencePoint <- merge(ReferencePoint,
                                ReferencePoint$ReferenceFeature,
                                by = "ReferencePointGuid",
                                all = TRUE)
        SiteNavigation_all$ReferencePoint <- NULL
        ReferencePoint$ReferenceFeature <- NULL

        TiePoint <- SiteNavigation_all$TiePoint
        if(class(TiePoint$PointLocation) == "data.frame"){
          TiePoint <- merge(TiePoint,
                            TiePoint$PointLocation,
                            by = "PointLocationGuid",
                            all = TRUE)
        } else {
          TiePoint <- merge(TiePoint,
                            IntegratedPlotCenter$PointLocation[0,],
                            by = "PointLocationGuid",
                            all = TRUE)
        }
        TiePoint$PointLocation <- NULL
        PointLocation_template <- IntegratedPlotCenter$PointLocation[0,]
        IntegratedPlotCenter$PointLocation <- NULL

        if(class(TiePoint$ReferenceFeature) == "data.frame"){
          TiePoint <- merge(TiePoint,
                            TiePoint$ReferenceFeature,
                            by = "TiePointGuid",
                            all = TRUE)
        } else {
          TiePoint <- merge(TiePoint,
                            data.table(ReferenceFeatureGuid = character(),
                                       ReferenceFeatureTypeCode = character(),
                                       ReferencePointGuid = character(),
                                       ReferenceTagNumber = character(),
                                       ReferenceTreeDiameter = character(),
                                       TiePointGuid = character(),
                                       TreeSpeciesCode = character()),
                            by = "TiePointGuid",
                            all = TRUE)
        }
        TiePoint$ReferenceFeature <- NULL
        SiteNavigation_all$TiePoint <- NULL
      } else {
        SiteNavigation_ext <- SiteNavigation[[i]]
        IntegratedPlotCenter_ext <- SiteNavigation_ext$IntegratedPlotCenter
        IntegratedPlotCenter_ext <- merge(IntegratedPlotCenter_ext,
                                          IntegratedPlotCenter_ext$PointLocation,
                                          by = "PointLocationGuid",
                                          all = TRUE)
        SiteNavigation_ext$IntegratedPlotCenter <- NULL
        IntegratedPlotCenter_ext$PointLocation <- NULL
        IntegratedPlotCenter <- rbind(IntegratedPlotCenter,
                                      IntegratedPlotCenter_ext)
        rm(IntegratedPlotCenter_ext)

        SiteNavigation_PointLocation <- rbind(SiteNavigation_PointLocation,
                                              SiteNavigation_ext$PointLocation)
        SiteNavigation_ext$PointLocation <- NULL


        ReferencePoint_ext <- SiteNavigation_ext$ReferencePoint
        ReferencePoint_ext <- merge(ReferencePoint_ext,
                                    ReferencePoint_ext$ReferenceFeature,
                                    by = "ReferencePointGuid",
                                    all = TRUE)
        SiteNavigation_ext$ReferencePoint <- NULL
        ReferencePoint_ext$ReferenceFeature <- NULL
        ReferencePoint <- rbind(ReferencePoint,
                                ReferencePoint_ext)
        rm(ReferencePoint_ext)


        TiePoint_ext <- SiteNavigation_ext$TiePoint
        if(class(TiePoint_ext$PointLocation) == "data.frame"){
          TiePoint_ext <- merge(TiePoint_ext,
                                TiePoint_ext$PointLocation,
                                by = "PointLocationGuid",
                                all = TRUE)
        } else {
          TiePoint_ext <- merge(TiePoint_ext,
                                PointLocation_template,
                                by = "PointLocationGuid",
                                all = TRUE)
        }

        TiePoint_ext$PointLocation <- NULL

        if(class(TiePoint_ext$ReferenceFeature) == "data.frame"){
          TiePoint_ext <- merge(TiePoint_ext,
                                TiePoint_ext$ReferenceFeature,
                                by = "TiePointGuid",
                                all = TRUE)
        } else {
          TiePoint_ext <- merge(TiePoint_ext,
                                data.table(ReferenceFeatureGuid = character(),
                                           ReferenceFeatureTypeCode = character(),
                                           ReferencePointGuid = character(),
                                           ReferenceTagNumber = character(),
                                           ReferenceTreeDiameter = character(),
                                           TiePointGuid = character(),
                                           TreeSpeciesCode = character()),
                                by = "TiePointGuid",
                                all = TRUE)
        }
        TiePoint_ext$ReferenceFeature <- NULL
        SiteNavigation_ext$TiePoint <- NULL
        TiePoint <- rbind(TiePoint,
                          TiePoint_ext)

        SiteNavigation_all <- rbind(SiteNavigation_all,
                                    SiteNavigation_ext)
      }
    }

    SiteNavigation <- SiteNavigation_all
    rm(SiteNavigation_all)

    checkHandheldTableColNames("SiteNavigation",
                               names(SiteNavigation))

    checkHandheldTableColNames("IntegratedPlotCenter",
                               names(IntegratedPlotCenter))

    checkHandheldTableColNames("ReferencePoint",
                               names(ReferencePoint))
    checkHandheldTableColNames("TiePoint",
                               names(TiePoint))

    for(i in 1:length(SampleSiteVisits$TreeDetails)){
      if(i == 1){
        TreeDetails <- SampleSiteVisits$TreeDetails[[i]]
        if(SampleSiteVisits$SampleSitePurposeTypeCode[[i]] != "PSP"){

          TreeDetails <- merge(TreeDetails,
                               TreeDetails$ChngMntrngInvTreeDtl,
                               by = "TreeDetailGuid",
                               all = TRUE)
        } else {
          TreeDetails <- merge(TreeDetails,
                               data.table(TreeDetailGuid = NA),
                               by = "TreeDetailGuid",
                               all = TRUE)

        }
        TreeDetails$ChngMntrngInvTreeDtl <- NULL
      } else {
        TreeDetails_ext <- SampleSiteVisits$TreeDetails[[i]]
        if(SampleSiteVisits$SampleSitePurposeTypeCode[[i]] != "PSP"){
          TreeDetails_ext <- merge(TreeDetails_ext,
                                   TreeDetails_ext$ChngMntrngInvTreeDtl,
                                   by = "TreeDetailGuid",
                                   all = TRUE)

        } else {
          TreeDetails_ext <- merge(TreeDetails_ext,
                                   data.table(TreeDetailGuid = NA),
                                   by = "TreeDetailGuid",
                                   all = TRUE)
        }
        TreeDetails_ext$ChngMntrngInvTreeDtl <- NULL
        TreeDetails <- rbindlist(list(TreeDetails,
                                      TreeDetails_ext), fill = TRUE)
        rm(TreeDetails_ext)
      }
    }
    rm(i)

    if(!onlypsp){
      checkHandheldTableColNames("TreeDetails",
                                 names(TreeDetails))
    }
    SampleSiteVisits$TreeDetails <- NULL

    for(i in 1:length(SampleSiteVisits$ValidationResults)){
      if(i == 1){
        ValidationResults <- SampleSiteVisits$ValidationResults[[i]]

      } else {
        ValidationResults <- rbind(ValidationResults,
                                   SampleSiteVisits$ValidationResults[[i]])
      }
    }
    rm(i)
    SampleSiteVisits$ValidationResults <- NULL
    if(length(ValidationResults) > 0){
      ValidationResults <- ValidationResults[[1]]
    } else {
      ValidationResults <- data.table(CommentText = "Not Available")
    }
    SampleSites$SampleSiteVisits <- NULL

    if(indiproj == 1){
      for(indifile in allFileNames){
        indifiledata <- data.table(get(indifile))
        assign(paste0(indifile, "_temp"), indifiledata)
        rm(list = as.character(indifile))
        rm(indifiledata)
      }
    } else {
      for(indifile in allFileNames){
        indifiledata <- data.table(get(paste0(indifile, "_temp")))
        indifiledata_ext <- data.table(get(indifile))
        indifiledata <- rbind(indifiledata,
                              indifiledata_ext, fill = TRUE)
        assign(paste0(indifile, "_temp"), indifiledata)
        rm(list = as.character(indifile))
        rm(indifiledata_ext, indifiledata)
      }
    }
    rm(indiproj, indifile)
  }
  rm(SampleSites_all)
  for(indifile in allFileNames){
    indifiledata <- data.table(get(paste0(indifile, "_temp")))
    assign(indifile, indifiledata)
    rm(list = paste0(indifile, "_temp"))
    rm(indifiledata, indifile)
  }
  ## Joint tables

  GroundSampleProjects <- data.table(GroundSampleProjects)
  SampleSites_lookup <- merge(SampleSiteVisits[,.(GroundSampleProjectGuid,
                                                  SampleSiteGuid)],
                              GroundSampleProjects[,.(GroundSampleProjectGuid, ProjectName,
                                                      ProjectNumber)],
                              all = TRUE)

  SampleSites <- merge(SampleSites_lookup,
                       SampleSites,
                       by = "SampleSiteGuid",
                       all = TRUE)
  SampleSites_lookup <- unique(SampleSites[,.(SampleSiteGuid, PointLocationGuid,
                                              ProjectName, ProjectNumber, SampleSiteName)])



  AccessNotes <- merge(SampleSites_lookup,
                       AccessNotes,
                       by = "SampleSiteGuid",
                       all = TRUE)

  SitePlots <- merge(SampleSites_lookup,
                     SitePlots,
                     by = "SampleSiteGuid",
                     all = TRUE)
  Trees <- merge(SitePlots,
                 Trees,
                 by = "PlotGuid",
                 all = TRUE)
  Trees <- Trees[!is.na(TreeGuid),]


  PointLocation <- merge(SampleSites_lookup,
                         PointLocation,
                         by = "PointLocationGuid",
                         all = TRUE)
  PointLocation <- PointLocation[!is.na(PointLocationGuid)]


  SampleSiteVisits <- merge(SampleSiteVisits,
                            SampleSites_lookup[,.(SampleSiteGuid,
                                                  ProjectName,
                                                  ProjectNumber,
                                                  SampleSiteName)],
                            by = "SampleSiteGuid",
                            all = TRUE)
  SampleSiteVisits$PolygonBoundaries <- NULL
  GroundSampleCrewActivities <- merge(GroundSampleCrewActivities,
                                      SampleSiteVisits[,.(SampleSiteVisitGuid,
                                                          ProjectName, ProjectNumber,
                                                          SampleSiteName,
                                                          SampleSitePurposeTypeCode,
                                                          SampleSiteVisitStartDate,
                                                          SampleSiteVisitEndDate)],
                                      by = "SampleSiteVisitGuid",
                                      all = TRUE)
  PlotDetails <- merge(PlotDetails,
                       SampleSiteVisits[,.(SampleSiteVisitGuid,
                                           ProjectName, ProjectNumber,
                                           SampleSiteName,
                                           SampleSitePurposeTypeCode,
                                           SampleSiteVisitStartDate,
                                           SampleSiteVisitEndDate)],
                       by = "SampleSiteVisitGuid",
                       all = TRUE)

  PlotDetails <- merge(PlotDetails,
                       SitePlots[,.(PlotGuid,
                                    PlotNumber)],
                       by = "PlotGuid",
                       all = TRUE)

  SampleMeasurements <- merge(SampleMeasurements,
                              SampleSiteVisits[,.(SampleSiteVisitGuid,
                                                  ProjectName,
                                                  ProjectNumber,
                                                  SampleSiteName,
                                                  SampleSitePurposeTypeCode,
                                                  SampleSiteVisitStartDate,
                                                  SampleSiteVisitEndDate)],
                              by = "SampleSiteVisitGuid",
                              all = TRUE)

  SmallLiveTreeTallies <- merge(SmallLiveTreeTallies,
                                SampleMeasurements[,.(SampleMeasurementGuid,
                                                      ProjectName,
                                                      ProjectNumber,
                                                      SampleSiteName,
                                                      SampleSitePurposeTypeCode,
                                                      MeasurementDate,
                                                      SampleMeasurementTypeCode)],
                                by = "SampleMeasurementGuid")

  TreeDetails <- merge(TreeDetails,
                       Trees[,.(TreeGuid, ProjectName, ProjectNumber,
                                SampleSiteName, PlotCategoryCode,
                                PlotNumber, FelledInd, NvafTreeInd, TreeNumber,
                                TreePlantedInd)],
                       by = "TreeGuid",
                       all = TRUE)
  TreeDetails$CommentText <- NULL

  TreeMeasurements <- merge(TreeMeasurements,
                            SampleMeasurements[,.(SampleMeasurementGuid,
                                                  ProjectName,
                                                  ProjectNumber,
                                                  SampleSiteName,
                                                  SampleSitePurposeTypeCode,
                                                  MeasurementDate,
                                                  SampleMeasurementTypeCode)],
                            by = "SampleMeasurementGuid",
                            all.x = TRUE)
  TreeMeasurements <- merge(TreeMeasurements,
                            TreeDetails[,.(TreeGuid,
                                           PlotNumber, TreeNumber, TreeSpeciesCode)],
                            by = "TreeGuid",
                            all.x = TRUE)

  TreeMeasurements <- merge(TreeMeasurements,
                            VriTreeMeasurement[, VriTreeMeasurement := TRUE],
                            by = "TreeMeasurementGuid",
                            all.x = TRUE)
  TreeDamageOccurrences <- merge(TreeDamageOccurrences,
                                 TreeMeasurements[,.(TreeMeasurementGuid,
                                                     ProjectName, ProjectNumber,
                                                     SampleSiteName,
                                                     SampleSitePurposeTypeCode,
                                                     PlotNumber, TreeNumber)],
                                 by = "TreeMeasurementGuid",
                                 all.x = TRUE)
  TreeLossIndicators <- merge(TreeLossIndicators,
                              TreeMeasurements[,.(TreeMeasurementGuid,
                                                  ProjectName, ProjectNumber,
                                                  SampleSiteName,
                                                  SampleSitePurposeTypeCode,
                                                  PlotNumber, TreeNumber)],
                              by = "TreeMeasurementGuid",
                              all.x = TRUE)
  TreeLogAssessments <- merge(LogAssessments,
                              TreeMeasurements[,.(VriTreeMeasurementGuid,
                                                  ProjectName, ProjectNumber,
                                                  SampleSiteName,
                                                  SampleSitePurposeTypeCode,
                                                  PlotNumber, TreeNumber)],
                              by = "VriTreeMeasurementGuid",
                              all.x = TRUE)
  VriSampleMeasurement <- merge(VriSampleMeasurement,
                                SampleMeasurements[,.(SampleMeasurementGuid,
                                                      ProjectName,
                                                      ProjectNumber,
                                                      SampleSiteName,
                                                      SampleSitePurposeTypeCode,
                                                      MeasurementDate,
                                                      SampleMeasurementTypeCode)],
                                by = "SampleMeasurementGuid",
                                all.x = TRUE)
  VriSampleMeasurement$CommentText <- NULL

  StumpTallies <- merge(StumpTallies,
                        VriSampleMeasurement,
                        by = "VriSampleMeasurementGuid",
                        all.x = TRUE)


  SiteNavigation <- merge(SiteNavigation,
                          SampleSiteVisits[,.(SampleSiteVisitGuid, ProjectName,
                                              ProjectNumber, SampleSiteName,
                                              SampleSitePurposeTypeCode,
                                              SampleSiteVisitStartDate, SampleSiteVisitEndDate)],
                          by = "SampleSiteVisitGuid",
                          all = TRUE)
  IntegratedPlotCenter <- merge(IntegratedPlotCenter,
                                SiteNavigation[,.(SiteNavigationGuid, ProjectName,
                                                  ProjectNumber, SampleSiteName,
                                                  SampleSitePurposeTypeCode)],
                                by = "SiteNavigationGuid",
                                all.x = TRUE)
  ReferencePoint <- merge(ReferencePoint,
                          SiteNavigation[,.(SiteNavigationGuid, ProjectName,
                                            ProjectNumber, SampleSiteName,
                                            SampleSitePurposeTypeCode)],
                          by = "SiteNavigationGuid",
                          all.x = TRUE)

  TiePoint <- merge(TiePoint,
                    SiteNavigation[,.(SiteNavigationGuid, ProjectName,
                                      ProjectNumber, SampleSiteName,
                                      SampleSitePurposeTypeCode)],
                    by = "SiteNavigationGuid",
                    all.x = TRUE)


  savedFileNames_proj_level <- c("GroundSampleProjects")

  savedFileNames_site_level <- c("SampleSites", "PlotDetails", "AccessNotes",
                                 "SampleSiteVisits", "SampleMeasurements",
                                 "GroundSampleCrewActivities", "SiteNavigation",
                                 "PointLocation", "IntegratedPlotCenter",
                                 "ReferencePoint", "TiePoint", "SmallLiveTreeTallies",
                                 "StumpTallies")
  savedFileNames_tree_level <- c("TreeDetails", "TreeMeasurements",
                                 "TreeLogAssessments", "TreeDamageOccurrences",
                                 "TreeLossIndicators")
  for(indifile in c(savedFileNames_proj_level,
                    savedFileNames_site_level,
                    savedFileNames_tree_level)){
    indifiledata <- get(indifile)
    indifiledata_names <- names(indifiledata)
    if(nrow(indifiledata) == 0){
      indifiledata_ext <- data.table(matrix(rep(NA, length(indifiledata_names)),
                                            nrow = 1))
      names(indifiledata_ext) <- indifiledata_names
      indifiledata <- rbind(indifiledata, indifiledata_ext)
      rm(indifiledata_ext)
    }
    indifiledata_names_noid <- indifiledata_names[-grep("Guid", indifiledata_names)]
    if(indifile %in% savedFileNames_proj_level){
      frontnames <- c("ProjectName", "ProjectNumber")
      restnames <- indifiledata_names_noid[!(indifiledata_names_noid %in% frontnames)]
      indifiledata <- indifiledata[,c(frontnames, restnames), with = FALSE]
      indifiledata <- indifiledata[order(ProjectName, ProjectNumber),]
    } else if (indifile %in% savedFileNames_site_level){
      frontnames <- c("ProjectName", "ProjectNumber", "SampleSiteName")
      restnames <- indifiledata_names_noid[!(indifiledata_names_noid %in% frontnames)]
      indifiledata <- indifiledata[,c(frontnames, restnames), with = FALSE]
      indifiledata <- indifiledata[order(ProjectName, ProjectNumber, SampleSiteName),]
    } else {
      frontnames <- c("ProjectName", "ProjectNumber", "SampleSiteName",
                      "PlotNumber", "TreeNumber")
      restnames <- indifiledata_names_noid[!(indifiledata_names_noid %in% frontnames)]
      indifiledata <- indifiledata[,c(frontnames, restnames), with = FALSE]
      indifiledata <- indifiledata[order(ProjectName, ProjectNumber, SampleSiteName,
                                         PlotNumber, TreeNumber),]
    }

    if(saveFormat == "xlsx"){
      xlsx::write.xlsx(indifiledata,
                       file.path(savePath, paste0(saveName, ".xlsx")),
                       sheetName = indifile,
                       append = TRUE,
                       row.names = FALSE)
    } else if (saveFormat == "rdata"){
      rm(list = indifile)
      assign(indifile, indifiledata)
    } else {
      stop("Output format has not been correctly specified.")
    }
    rm(indifile, indifiledata, indifiledata_names, indifiledata_names_noid,
       frontnames, restnames)
  }
  if(saveFormat == "rdata"){
    save(list = c(savedFileNames_proj_level,
                  savedFileNames_site_level,
                  savedFileNames_tree_level),
         file = file.path(savePath, paste0(saveName, ".rdata")))
  }
}



checkHandheldTableColNames <- function(tableName, tableColumnName){
  if(tableName == "GroundSampleProjects"){
    compareNames <- c("BusinessProgramCode", "CommentText", "EndDate", "FiscalStartYear",
                      "GroundSampleProjectGuid", "GroundSampleProjStsCode", "ProjectDescriptor",
                      "ProjectName", "ProjectNumber", "RegionalProjectNumber", "RegionCode",
                      "SampleSites", "StartDate")
  } else if(tableName == "SampleSites"){
    tableColumnName <- tableColumnName[!(tableColumnName %in% "OrgUnitNo")]
    compareNames <- c("AccessNotes", "AuxiliaryPlotSpacing", "BecZoneCode", "BgcPhase",
                      "BgcSubzone", "BgcVariant", "CommentText", "Elevation", "EstablishmentYear",
                      "ExpectedRemeasurementDate", "MeasurementInterval", "NfiGridNumber",
                      "Plots", "PointLocation", "PointLocationGuid", "PspSampleSiteTypeCode",
                      "SampleSelectionMthdlgyCode", "SampleSiteGuid", "SampleSiteName",
                      "SampleSiteVisits", "SiteConditionCode", "SiteIndex", "SiteRanking",
                      "SiteStatusCode", "SuitableForRemeasureInd", "TflNumber", "TimberSupplyAreaCode",
                      "TimberSupplyBlockCode", "TsaSourceCode")
  } else if(tableName == "AccessNotes"){
    compareNames <- c("AccessNoteGuid", "OdometerIncrement", "OdometerReading", "Remarks",
                      "SampleSiteGuid", "SequenceNumber")
  } else if(tableName == "Plots"){
    compareNames <- c("CommentText", "PlotCategoryCode", "PlotGuid", "PlotNumber", "SampleSiteGuid")
  } else if(tableName == "Trees"){
    compareNames <- c("FelledInd", "NvafTreeInd", "PlotGuid", "TreeGuid", "TreeNumber", "TreePlantedInd")
  } else if(tableName == "PointLocation"){
    compareNames <- c("BcgsMapsheetNumber", "CalcMagneticDeclination", "CalcMagneticDeclinationDate",
                      "CoordinateSourceCode", "DerivedLatitude", "DerivedLongitude",
                      "Elevation", "MagneticDeclination", "PointLocationGuid", "PointLocationTypeCode",
                      "UtmEasting", "UtmNorthing", "UtmZone")
  } else if(tableName == "SampleSiteVisits"){
    compareNames <- c("AirPhotoFlightNumber", "AirPhotoNumber", "BatchNumber", "CallGradesReqdInd",
                      "CenterStakeOkInd", "CollectionSoftware", "CollectionSoftwareVersion",
                      "CommentText", "DistanceToDisturbance", "GroundSampleCrewActivities",
                      "GroundSampleProjectGuid", "GrowingSeasonYear", "GrowthSeasonEndDate",
                      "GrowthSeasonStartDate", "GrowthYearFraction", "NotesToCrew",
                      "Photo70mmInd", "PhotoGridXCoord", "PhotoGridYCoord", "PlotDetails",
                      "PolygonBoundaries", "PolygonNumber", "SampleBreakPoint", "SampleBreakPointType",
                      "SampleMeasurements", "SampleSiteGuid", "SampleSitePurposeTypeCode",
                      "SampleSiteVisitEndDate", "SampleSiteVisitGuid", "SampleSiteVisitStartDate",
                      "SampleSiteVisitStatusCode", "SelectivelyLoggedInd", "SiteEventPurposeCode",
                      "StandDisturbanceCode", "StandStructureCode",
                      "StemMappedInd", "StemMapReqdInd", "StumpsReqdInd", "TreeDetails",
                      "TypicalEcologyInd", "TypicalTreesInd", "UseAuxDeadFallenInd",
                      "ValidationResults", "ValidationRulesetVersion", "ValidationRunDate",
                      "VisitNumber")
  } else if(tableName == "PlotDetails"){
    compareNames <- c("AzimuthBoundarySplit", "CmiIpcWalkthroughInd", "HerbaceousForageDgreCode",
                      "OutOfPolygonInd", "PartialPlotReasonCode", "PlotArea", "PlotAspect",
                      "PlotDetailGuid", "PlotElevation", "PlotGuid", "PlotLength",
                      "PlotRadius", "PlotRadiusFactor", "PlotSegmentCode", "PlotShapeCode",
                      "PlotSlope", "PlotWidth", "SampleSiteVisitGuid", "SmallTreeSubplotRadius",
                      "StemMapBrgToPltCenter", "StemMapOffDistance", "TaggingSectorCount",
                      "TieLineBearing", "TieLineDistance", "VariableBaf", "VariableRadiusInd")
  } else if(tableName == "SampleMeasurements"){
    compareNames <- c("CommentText", "DbhTaggingLimit", "EfrUnitId", "MeasurementCollectedInd",
                      "MeasurementDate", "PlotGuid", "SampleMeasurementGuid", "SampleMeasurementTypeCode",
                      "SampleSiteVisitGuid")
  } else if(tableName == "SmallLiveTreeTallies"){
    compareNames <- c("CommentText", "NumberOfTrees", "SampleMeasurementGuid", "SmallLiveTreeTallyGuid",
                      "SmallTreeTallyClassCode", "TreeSpeciesCode")
  } else if(tableName == "TreeMeasurements"){
    compareNames <- c("AgeCoreTakenInd", "AgeCorrection", "AgeMeasmtHeight", "AgeMeasureCode",
                      "AgeRepresentativeInd", "AgeSourceCode", "BarkThickness", "BenchmarkAgeInd",
                      "BoringAge", "BrokenStemInd", "BrokenTopDiameter", "CommentText",
                      "CrownClassCode", "Diameter", "DiameterAtBoringHeight", "DiameterMeasmtHeight",
                      "DiameterRepresentativeInd", "DiameterSourceCode", "DroppedInd",
                      "ExtraTreeInd", "FallingHazardInd", "HeightCrownSourceCode",
                      "HeightSourceCode", "HeightToBreak", "HeightToLeaningTip", "HeightToLiveCrown",
                      "HtDiamtrCurveUseCode", "LeadingSpeciesTreeInd", "LeaningTreeBearing",
                      "Length", "LichenLoadingRatingCode", "LiveCrownPercent", "MeasurementMsngReasonCode",
                      "MicroscopeAge", "MissedTreeInd", "NewAge", "OtherTreeInd", "PhysiologicalAge",
                      "ProjectedHeight", "ProrateLength", "ProrateRingCount", "RadialIncrementLast10Yr",
                      "RadialIncrementLast20Yr", "RadialIncrementLast5Yr", "RandomTreeInd",
                      "RemainingBarkPercent", "ResidualInd", "SampleMeasurementGuid",
                      "SecondSpeciesTreeInd", "SiteIndex", "SpeciesChangeInd", "SpiralGrainInd",
                      "SuitableForHeightInd", "TagMissingInd", "TopHeightTreeInd",
                      "TotalAge", "TreeAppearanceCode", "TreeClassCode", "TreeExtantCode",
                      "TreeGuid", "TreeMeasurementGuid", "TreeStanceCode", "TreeSuppressionCode",
                      "VertDistanceMeasReqInd")
  } else if(tableName == "TreeDamageOccurrences"){
    compareNames <- c("DamageAgentSeverityGuid", "DamageOrder", "DamageStemHeight",
                      "SeverityRatingValue", "TreeDamageOccurrenceGuid", "TreeMeasurementGuid")
  } else if(tableName == "TreeLossIndicators"){
    compareNames <- c("FormCode", "FormSeverityImpactCode", "Frequency", "IndicatorWidth",
                      "LocationFrom", "LocationTo", "ScarOccurrenceCode", "StemsBelowDbh",
                      "StemScarOccurrenceCode", "StemScarSeverityCode", "TreeLossIndicatorCode",
                      "TreeLossIndicatorGuid", "TreeLossIndLocnCode", "TreeMeasurementGuid")
  } else if(tableName == "VriTreeMeasurement"){
    compareNames <- c("BarkRetentionCode", "CrownConditionCode", "LogAssessments",
                      "SuitableForAgeInd", "SuitableForSiteIndexInd", "TreeMeasurementGuid",
                      "VriTreeMeasurementGuid", "WildlifeUseCode", "WoodConditionCode")
  } else if(tableName == "LogAssessments"){
    compareNames <- c("LastLogInd", "LogAssessmentGuid", "LogGradeCode", "LogLength",
                      "LogNumber", "PercentSound", "VriTreeMeasurementGuid")
  } else if(tableName == "VriSampleMeasurement"){
    compareNames <- c("CommentText", "SampleMeasurementGuid", "StumpTallies",
                      "VriSampleMeasurementGuid")
  } else if(tableName == "StumpTallies"){
    compareNames <- c("BarkRetentionCode", "CommentText", "DamageAgentCode", "Diameter",
                      "Length", "SoundWoodPercent", "StumpOccurrenceFrequency", "StumpTallyGuid",
                      "TreeSpeciesCode", "VriSampleMeasurementGuid", "WildlifeUseCode",
                      "WoodConditionCode")
  } else if(tableName == "SiteNavigation"){
    compareNames <- c("AccessPointToIpcAzimuth", "AccessPointToIpcDistance", "CommentText",
                      "PointLocationGuid",
                      "SampleSiteVisitGuid", "SiteNavigationGuid",
                      "TiePointToIpcAzimuth", "TiePointToIpcDistance")
  } else if(tableName == "IntegratedPlotCenter"){
    compareNames <- c("BcgsMapsheetNumber", "CalcMagneticDeclination", "CalcMagneticDeclinationDate",
                      "CoordinateSourceCode", "DerivedLatitude", "DerivedLongitude",
                      "Elevation", "GpsProcessedFileName", "IntegratedPlotCenterGuid",
                      "MagneticDeclination", "OffsetGpsAzimuth", "OffsetGpsDistance",
                      "OffsetPinToIpcAzimuth", "OffsetPinToIpcDistance", "PointLocationGuid",
                      "PointLocationTypeCode", "SiteNavigationGuid", "UtmEasting",
                      "UtmNorthing", "UtmZone")
  } else if(tableName == "ReferencePoint"){
    compareNames <- c("FeatureToRefPinAzimuth", "FeatureToRefPinDistance", "OffsetPinToRefPtAzimuth",
                      "OffsetPinToRefPtDistance", "ReferenceFeatureGuid", "ReferenceFeatureTypeCode",
                      "ReferencePointGuid", "ReferenceTagNumber", "ReferenceTreeDiameter",
                      "SiteNavigationGuid", "TiePointGuid", "TreeSpeciesCode")
  } else if(tableName == "TiePoint"){
    compareNames <- c("BcgsMapsheetNumber", "CalcMagneticDeclination", "CalcMagneticDeclinationDate",
                      "CoordinateSourceCode", "DerivedLatitude", "DerivedLongitude",
                      "Elevation", "FeatureToTiePointAzimuth", "FeatureToTiePointDistance",
                      "MagneticDeclination", "OffsetGpsAzimuth", "OffsetGpsDistance",
                      "PointLocationGuid", "PointLocationTypeCode",
                      "ReferenceFeatureGuid",
                      "ReferenceFeatureTypeCode", "ReferencePointGuid", "ReferenceTagNumber",
                      "ReferenceTreeDiameter", "SiteNavigationGuid", "TiePointGuid",
                      "TreeSpeciesCode", "UtmEasting", "UtmNorthing", "UtmZone")
  } else if(tableName == "TreeDetails"){
    compareNames <- c("ChngMntrngInvTreeDtlGuid", "CommentText", "NearTreeNumber",
                      "OutOfPlotInd", "OutOfPolygonInd", "SampleSiteVisitGuid", "SiteSectorNumber",
                      "StemMapBearing", "StemMapDistance", "TaggingSectorNumber", "TreeDetailGuid",
                      "TreeGuid", "TreeSpeciesCode", "WalkthroughTreeInd")
  } else {
    stop(paste0("Table ", tableName, "does not exist in Handheld system."))
  }
  # cat(paste0("Check column names for table ", tableName, ":"))
  if(!identical(sort(tableColumnName),
                compareNames)){
    stop(paste0("Column names of ", tableName, " are different from compared names. Please check."))
  } #else {
  # cat(" done. \n")
  #}
}


