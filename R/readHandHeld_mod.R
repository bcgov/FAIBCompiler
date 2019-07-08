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
#'    readHandHeld_mod(fileName = icmcjsonFile,
#'                 savePath = file.path(getwd(), "ismc_test_folder"))
#' }
#' @export
#' @docType methods
#' @rdname readHandHeld_mod
#'
#' @author Yong Luo
#'
readHandHeld_mod <- function(fileName, savePath = getwd(),
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


  allprojs <- GroundSampleProjects$ProjectName
  cat(paste0("*This JSON file has ", length(allprojs), " project(s): \n",
             paste0("  ", allprojs, collapse = "\n"), " \n\n"))

  AccessNotes <- data.table(SampleSiteGuid = character())
  Plots <- data.table(SampleSiteGuid = character())
  Trees <- data.table(TreeGuid = character())
  PointLocation <- data.table(PointLocationGuid = character())
  SampleSiteVisits <- data.table(SampleSiteGuid = character())
  GroundSampleCrewActivities <- data.table(GroundSampleCrewActvtyGuid = character())
  PlotDetails <- data.table(SampleSiteVisitGuid = character())
  TreeDetails <- data.table(TreeGuid = character())

  SampleMeasurements <- data.table(SampleMeasurementGuid = character())
  SmallLiveTreeTallies <- data.table(SampleMeasurementGuid = character())
  TreeMeasurements <- data.table(TreeGuid = character())
  TreeDamageOccurrences <- data.table(TreeMeasurementGuid = character())
  TreeLossIndicators <- data.table(TreeMeasurementGuid = character())
  VriTreeMeasurement <- data.table(TreeMeasurementGuid = character())
  TreeLogAssessments <- data.table(VriTreeMeasurementGuid = character())
  VriSampleMeasurement <- data.table(VriSampleMeasurementGuid = character())
  StumpTallies <- data.table(VriSampleMeasurementGuid = character())


  SiteNavigation <- data.table(PointLocationGuid = character())
  IntegratedPlotCenter <- data.table(PointLocationGuid = character())
  ReferencePoint <- data.table(SiteNavigationGuid = character())
  TiePoint <- data.table(SiteNavigationGuid = character())

  SampleSites <- data.table(SampleSitesGuid = character())


  samplesites_allproj <- GroundSampleProjects$SampleSites
  GroundSampleProjects$SampleSites <- NULL
  rm(thedata)
  for(indiproj in 1:length(allprojs)){
    ## for a project
    samplesites_indiproj <- samplesites_allproj[indiproj][[1]] ## here
    samplesiteNames_indiproj <- samplesites_indiproj$SampleSiteName
    cat(paste0("**For Project ", allprojs[indiproj], ", ", length(samplesiteNames_indiproj), " site(s):  \n"))
    ## for project-point location
    PointLocation <- rbindlist(list(PointLocation, samplesites_indiproj$PointLocation),
                               fill = TRUE)
    samplesites_indiproj$PointLocation <- NULL
    ## end of a project-point location

    for(indisite in 1:length(samplesiteNames_indiproj)){
      ## for a project-site-access note at site level
      indinote <- samplesites_indiproj$AccessNotes[[indisite]]
      if (class(indinote) == "data.frame"){
        AccessNotes <- rbindlist(list(AccessNotes, indinote), fill = TRUE)
      }
      rm(indinote)
      ## end of access note

      ## for a project-site-plots and trees at site level
      indiplot <- samplesites_indiproj$Plots[[indisite]]
      for(j in 1:length(indiplot$Trees)){
        inditree <- indiplot$Trees[[j]]
        if(class(inditree) == "data.frame"){
          Trees <- rbindlist(list(Trees, inditree), fill = TRUE)
        }
        rm(inditree, j)
      }
      indiplot$Trees <- NULL
      Plots <- rbindlist(list(Plots, indiplot), fill = TRUE)
      rm(indiplot)
      ## end of plots and trees


      ## for a project-site-sample site visit
      SampleSiteVisits_indisite <- samplesites_indiproj$SampleSiteVisits[[indisite]]

      ## save a project-site-visit-site navigation
      SiteNavigation_indisite <- SampleSiteVisits_indisite$SiteNavigation
      SampleSiteVisits_indisite$SiteNavigation <- NULL
      ## end of save

      cat(paste0("***Site:  ", samplesiteNames_indiproj[indisite]," \n",
                 "   Type:  ", SampleSiteVisits_indisite$SampleSitePurposeTypeCode," \n",
                 "   Visit: ", SampleSiteVisits_indisite$VisitNumber," \n"))


      ## for a project-site-visit-crew
      visitcrews <- length(SampleSiteVisits_indisite$GroundSampleCrewActivities)
      for(indicrew in 1:visitcrews){
        GroundSampleCrewActivities <- rbindlist(list(GroundSampleCrewActivities,
                                                     SampleSiteVisits_indisite$GroundSampleCrewActivities[[indicrew]]),
                                                fill = TRUE)
      }
      rm(visitcrews, indicrew)
      SampleSiteVisits_indisite$GroundSampleCrewActivities <- NULL
      ## end of project-site-visit-crew

      ## for project-site-visit-plotdetails
      allplotdetails <- length(SampleSiteVisits_indisite$PlotDetails)
      for(indiplotdetail in 1:allplotdetails){
        if(class(SampleSiteVisits_indisite$PlotDetails[[indiplotdetail]]) == "data.frame"){
          PlotDetails <- rbindlist(list(PlotDetails,
                                        SampleSiteVisits_indisite$PlotDetails[[indiplotdetail]]),
                                   fill = TRUE)
        }
      }
      SampleSiteVisits_indisite$PlotDetails <- NULL
      rm(allplotdetails, indiplotdetail)
      ## end of project-site-visit-plotdetails

      ## for project-site-visit-treedetails
      alltreedetails <- length(SampleSiteVisits_indisite$TreeDetails)
      for(inditreedetail in 1:alltreedetails){
        if(class(SampleSiteVisits_indisite$TreeDetails[[inditreedetail]]) == "data.frame"){
          treedetails_temp <- SampleSiteVisits_indisite$TreeDetails[[inditreedetail]]
          treedetails_temp <- merge(treedetails_temp,
                                    treedetails_temp$ChngMntrngInvTreeDtl,
                                    by = "TreeDetailGuid",
                                    all.x = TRUE)
          treedetails_temp$ChngMntrngInvTreeDtl <- NULL
          TreeDetails <- rbindlist(list(TreeDetails,
                                        treedetails_temp),
                                   fill = TRUE)
        }
      }
      rm(alltreedetails, inditreedetail, treedetails_temp)
      SampleSiteVisits_indisite$TreeDetails <- NULL
      ## end of project-site-visit-treedetails

      ## for project-site-visit-samplemeasurement
      allmeasurements <- length(SampleSiteVisits_indisite$SampleMeasurements)
      # SampleSiteVisits_indisite$SampleMeasurements <- NULL
      for(indimeasure in 1:allmeasurements){
        indisamplemeasurement <- SampleSiteVisits_indisite$SampleMeasurements[[indimeasure]]
        ## for project-site-visit-samplemeasurement-smalltreetallies
        allsmalltreetallies <- indisamplemeasurement$SmallLiveTreeTallies
        for(indismalltreetallies in 1:length(allsmalltreetallies)){
          if(class(allsmalltreetallies[[indismalltreetallies]]) == "data.frame"){
            SmallLiveTreeTallies <- rbindlist(list(SmallLiveTreeTallies,
                                                   allsmalltreetallies[[indismalltreetallies]]),
                                              fill = TRUE)
          }
        }
        indisamplemeasurement$SmallLiveTreeTallies <- NULL
        rm(allsmalltreetallies, indismalltreetallies)
        ## end of project-site-visit-samplemeasurement-smalltreetallies

        ## for project-site-visit-samplemeasurement-treemeasurements
        alltreemeasurements <- indisamplemeasurement$TreeMeasurements
        for(inditreemeas in 1:length(alltreemeasurements)){
          inditreemeasurement <- alltreemeasurements[[inditreemeas]]
          ## for project-site-visit-samplemeasurement-treemeasurements-damageoccurence
          for(indioccur in 1:length(inditreemeasurement$TreeDamageOccurrences)){
            if(class(inditreemeasurement$TreeDamageOccurrences[[indioccur]]) == "data.frame"){
              TreeDamageOccurrences <- rbindlist(list(TreeDamageOccurrences,
                                                      inditreemeasurement$TreeDamageOccurrences[[indioccur]]),
                                                 fill = TRUE)
            }
          }
          inditreemeasurement$TreeDamageOccurrences <- NULL
          rm(indioccur)
          ## end of project-site-visit-samplemeasurement-treemeasurements-damageoccurence

          ## for project-site-visit-samplemeasurement-treemeasurements-lossindicator
          for(indiloss in 1:length(inditreemeasurement$TreeLossIndicators)){
            if(class(inditreemeasurement$TreeLossIndicators[[indiloss]]) == "data.frame"){
              TreeLossIndicators <- rbindlist(list(TreeLossIndicators,
                                                   inditreemeasurement$TreeLossIndicators[[indiloss]]),
                                              fill = TRUE)
            }
          }
          inditreemeasurement$TreeLossIndicators <- NULL
          rm(indiloss)
          ## end of project-site-visit-samplemeasurement-treemeasurements-lossindicator

          ## for project-site-visit-samplemeasurement-treemeasurements-vritreemeasurement
          indivrimeasurement <- inditreemeasurement$VriTreeMeasurement
          if(class(indivrimeasurement) == "data.frame"){

            ## for project-site-visit-samplemeasurement-treemeasurements-vritreemeasurement-logassessment
            for(indilog in 1:length(indivrimeasurement$LogAssessments)){
              if(class(indivrimeasurement$LogAssessments[[indilog]]) == "data.frame"){
                TreeLogAssessments <- rbindlist(list(TreeLogAssessments,
                                                     indivrimeasurement$LogAssessments[[indilog]]),
                                                fill = TRUE)
              }
            }
            rm(indilog)
            indivrimeasurement$LogAssessments <- NULL
            ## end of project-site-visit-samplemeasurement-treemeasurements-vritreemeasurement-logassessment

            VriTreeMeasurement <- rbindlist(list(VriTreeMeasurement,
                                                 indivrimeasurement),
                                            fill = TRUE)
            inditreemeasurement$VriTreeMeasurement <- NULL
          }
          ## end of for project-site-visit-samplemeasurement-treemeasurements-vritreemeasurement
          TreeMeasurements <- rbindlist(list(TreeMeasurements,
                                             inditreemeasurement),
                                        fill = TRUE)
          ## end of project-site-visit-samplemeasurement-treemeasurements
        }

        indisamplemeasurement$TreeMeasurements <- NULL


        ## for project-site-visit-samplemeasurement-treemeasurements-vrisamplemeasurement
        indivrisamplemeas <- indisamplemeasurement$VriSampleMeasurement
        if(class(indivrisamplemeas) == "data.frame"){
          for(indistump in 1:length(indivrisamplemeas$StumpTallies)){
            if(class(indivrisamplemeas$StumpTallies[[indistump]]) == "data.frame"){
              StumpTallies <- rbindlist(list(StumpTallies,
                                             indivrisamplemeas$StumpTallies[[indistump]]),
                                        fill = TRUE)
            }
          }
          indivrisamplemeas$StumpTallies <- NULL
          VriSampleMeasurement <- rbindlist(list(VriSampleMeasurement,
                                                 indivrisamplemeas),
                                            fill = TRUE)

        }
        indisamplemeasurement$VriSampleMeasurement <- NULL
        ## end of project-site-visit-samplemeasurement-treemeasurements-vrisamplemeasurement

        SampleMeasurements <- rbindlist(list(SampleMeasurements,
                                             indisamplemeasurement),
                                        fill = TRUE)
      }
      ## end of project-site-visit-samplemeasurement

      ### work on site navigation
      ## for project-site-visit-sitenavigation

      ## for project-site-visit-sitenavigation-ipc
      indiipc <- SiteNavigation_indisite$IntegratedPlotCenter
      indiipc <- merge(indiipc, indiipc$PointLocation,
                       by = "PointLocationGuid",
                       all.x = TRUE)
      indiipc$PointLocation <- NULL
      IntegratedPlotCenter <- rbindlist(list(IntegratedPlotCenter,
                                             indiipc),
                                        fill = TRUE)
      SiteNavigation_indisite$IntegratedPlotCenter <- NULL
      rm(indiipc)
      ## end of project-site-visit-sitenavigation-ipc

      ## for project-site-visit-sitenavigation-referencepoint
      indireferencepoint <- SiteNavigation_indisite$ReferencePoint
      indireferencepoint <- merge(indireferencepoint,
                                  indireferencepoint$ReferenceFeature,
                                  by = "ReferencePointGuid", all.x = TRUE)
      indireferencepoint$ReferenceFeature <- NULL
      ReferencePoint <- rbindlist(list(ReferencePoint,
                                       indireferencepoint),
                                  fill = TRUE)
      rm(indireferencepoint)
      SiteNavigation_indisite$ReferencePoint <- NULL
      ## end of project-site-visit-sitenavigation-referencepoint

      ## for project-site-visit-sitenavigation-tiepoint
      inditiepoint <- SiteNavigation_indisite$TiePoint
      if(class(inditiepoint$PointLocation) == "data.frame"){
        inditiepoint <- merge(inditiepoint,
                              inditiepoint$PointLocation,
                              by = "PointLocationGuid",
                              all.x = TRUE)
      }
      inditiepoint$PointLocation <- NULL
      if(class(inditiepoint$ReferenceFeature) == "data.frame"){
        inditiepoint <- merge(inditiepoint,
                              inditiepoint$ReferenceFeature,
                              by = "TiePointGuid",
                              all.x = TRUE)
      }
      inditiepoint$ReferenceFeature <- NULL
      TiePoint <- rbindlist(list(TiePoint,
                                 inditiepoint),
                            fill = TRUE)
      SiteNavigation_indisite$TiePoint <- NULL
      ## end of project-site-visit-sitenavigation-tiepoint

      ## for project-site-visit-sitenavigation-pointlocation
      if(class(SiteNavigation_indisite$PointLocation) == "data.frame"){
        SiteNavigation_indisite <- merge(SiteNavigation_indisite,
                                         SiteNavigation_indisite$PointLocation,
                                         by = "PointLocationGuid",
                                         all.x = TRUE)
      }
      SiteNavigation_indisite$PointLocation <- NULL
      ## end of project-site-visit-sitenavigation-pointlocation

      SiteNavigation <- rbindlist(list(SiteNavigation,
                                       SiteNavigation_indisite),
                                  fill = TRUE)
      ## end of project-site-visit-sitenavigation

      SampleSiteVisits_indisite$ValidationResults <- NULL
      SampleSiteVisits_indisite$SampleMeasurements <- NULL
      SampleSiteVisits <- rbindlist(list(SampleSiteVisits,
                                         SampleSiteVisits_indisite),
                                    fill = TRUE)
      ## end of project-site-visit
    }
    samplesites_indiproj$AccessNotes <- NULL
    samplesites_indiproj$Plots <- NULL
    samplesites_indiproj$SampleSiteVisits <- NULL
    samplesites_indiproj$Elevation <- NULL

    samplesites_indiproj <- merge(samplesites_indiproj,
                                  PointLocation,
                                  by = "PointLocationGuid",
                                  all.x = TRUE)
    SampleSites <- rbindlist(list(SampleSites,
                                  samplesites_indiproj),
                             fill = TRUE)
    rm(samplesites_indiproj)
    ## end of project-site
    cat("\n")
  }## end of a project




  ## Joint tables

  GroundSampleProjects <- data.table(GroundSampleProjects)
  SampleSites_lookup <- SampleSites[,.(SampleSiteGuid, SampleSiteName)]

  AccessNotes <- merge(AccessNotes,
                       SampleSites_lookup,
                       by = "SampleSiteGuid",
                       all.x = TRUE)


  SampleSiteVisits <- merge(SampleSiteVisits,
                            SampleSites_lookup,
                            by = "SampleSiteGuid",
                            all.x = TRUE)

  GroundSampleProjects[, CommentText := NULL]
  SampleSiteVisits <- merge(SampleSiteVisits,
                            GroundSampleProjects,
                            by = "GroundSampleProjectGuid",
                            all.x = TRUE)
  SampleSiteVisits$PolygonBoundaries <- NULL
  SampleSiteVisits_lookup <- SampleSiteVisits[,.(SampleSiteVisitGuid, SampleSiteName,
                                                 VisitNumber, ProjectName, ProjectNumber)]

  PlotDetails <- merge(PlotDetails,
                       SampleSiteVisits_lookup,
                       by = "SampleSiteVisitGuid",
                       all.x = TRUE)
  PlotDetails <- merge(PlotDetails,
                       Plots[,.(PlotGuid, PlotCategoryCode,
                                PlotNumber)],
                       by = "PlotGuid",
                       all.x = TRUE)
  SampleMeasurements <- merge(SampleMeasurements,
                              SampleSiteVisits_lookup,
                              by = "SampleSiteVisitGuid",
                              all.x = TRUE)

  GroundSampleCrewActivities <- merge(GroundSampleCrewActivities,
                                      SampleSiteVisits_lookup,
                                      by = "SampleSiteVisitGuid",
                                      all.x = TRUE)
  GroundSampleCrewActivities <- merge(GroundSampleCrewActivities,
                                      GroundSampleHumanResources,
                                      by = "GroundSampleHumanRsrceGuid",
                                      all.x = TRUE)

  SiteNavigation <- merge(SiteNavigation,
                          SampleSiteVisits_lookup,
                          by = "SampleSiteVisitGuid",
                          all.x = TRUE)

  IntegratedPlotCenter <- merge(IntegratedPlotCenter,
                                SiteNavigation[,.(SiteNavigationGuid, SampleSiteName,
                                                  VisitNumber, ProjectName, ProjectNumber)],
                                by = "SiteNavigationGuid",
                                all.x = TRUE)
  ReferencePoint <- merge(ReferencePoint,
                          SiteNavigation[,.(SiteNavigationGuid, SampleSiteName,
                                            VisitNumber, ProjectName, ProjectNumber)],
                          by = "SiteNavigationGuid",
                          all.x = TRUE)

  TiePoint <- merge(TiePoint,
                    SiteNavigation[,.(SiteNavigationGuid, SampleSiteName,
                                      VisitNumber, ProjectName, ProjectNumber)],
                    by = "SiteNavigationGuid",
                    all.x = TRUE)


  SmallLiveTreeTallies <- merge(SmallLiveTreeTallies,
                                SampleMeasurements[,.(SampleMeasurementGuid,
                                                      SampleSiteName,
                                                      VisitNumber, ProjectName, ProjectNumber,
                                                      MeasurementDate)],
                                by = "SampleMeasurementGuid",
                                all.x = TRUE)

  if(nrow(VriSampleMeasurement) == 0){
    VriSampleMeasurement <- data.table(SampleMeasurementGuid = character(),
                                       VriSampleMeasurementGuid = character())
  }

  VriSampleMeasurement <- merge(VriSampleMeasurement,
                                SampleMeasurements[,.(SampleMeasurementGuid,
                                                      SampleSiteName,
                                                      VisitNumber, ProjectName, ProjectNumber,
                                                      MeasurementDate)],
                                by = "SampleMeasurementGuid",
                                all.x = TRUE)

  StumpTallies <- merge(StumpTallies,
                        VriSampleMeasurement[, CommentText := NULL],
                        by = "VriSampleMeasurementGuid",
                        all.x = TRUE)




  Plots <- merge(Plots,
                 SampleSites_lookup,
                 by = "SampleSiteGuid",
                 all.x = TRUE)

  Trees <- merge(Trees,
                 Plots,
                 by = "PlotGuid",
                 all.x = TRUE)

  TreeMeasurements <- merge(TreeMeasurements,
                            SampleMeasurements[,.(SampleMeasurementGuid,
                                                  SampleSiteVisitGuid,
                                                  SampleSiteName,
                                                  VisitNumber, ProjectName, ProjectNumber,
                                                  MeasurementDate)],
                            by = "SampleMeasurementGuid",
                            all.x = TRUE)
  TreeMeasurements <- merge(TreeMeasurements,
                            Trees[,.(TreeGuid,
                                     PlotNumber, TreeNumber)],
                            by = "TreeGuid",
                            all.x = TRUE)
  TreeDetails[, CommentText := NULL]
  TreeMeasurements <- merge(TreeMeasurements,
                            TreeDetails,
                            by = c("TreeGuid", "SampleSiteVisitGuid"),
                            all.x = TRUE)
  TreeMeasurements <- merge(TreeMeasurements,
                            VriTreeMeasurement[, VriTreeMeasurement := TRUE],
                            by = "TreeMeasurementGuid",
                            all.x = TRUE)
  TreeDamageOccurrences <- merge(TreeDamageOccurrences,
                                 TreeMeasurements[,.(TreeMeasurementGuid,
                                                     SampleSiteName,
                                                     VisitNumber,
                                                     ProjectName, ProjectNumber,
                                                     MeasurementDate,
                                                     PlotNumber, TreeNumber)],
                                 by = "TreeMeasurementGuid",
                                 all.x = TRUE)
  das <- getLookupDAS()
  TreeDamageOccurrences[, DamageAgentSeverityGuid := gsub("-", "", DamageAgentSeverityGuid)]
  TreeDamageOccurrences <- merge(TreeDamageOccurrences,
                                 das,
                                 by = "DamageAgentSeverityGuid",
                                 all.x = TRUE)

  TreeLossIndicators <- merge(TreeLossIndicators,
                              TreeMeasurements[,.(TreeMeasurementGuid,
                                                  SampleSiteName,
                                                  VisitNumber,
                                                  ProjectName, ProjectNumber,
                                                  MeasurementDate,
                                                  PlotNumber, TreeNumber)],
                              by = "TreeMeasurementGuid",
                              all.x = TRUE)
  TreeLogAssessments <- merge(TreeLogAssessments,
                              TreeMeasurements[,.(VriTreeMeasurementGuid,
                                                  SampleSiteName,
                                                  VisitNumber,
                                                  ProjectName, ProjectNumber,
                                                  MeasurementDate,
                                                  PlotNumber, TreeNumber)],
                              by = "VriTreeMeasurementGuid",
                              all.x = TRUE)

  savedFileNames_site_level <- c("SampleSites", "AccessNotes")
  savedFileNames_site_proj_level <- c("PlotDetails",
                                      "SampleSiteVisits", "SampleMeasurements",
                                      "GroundSampleCrewActivities", "SiteNavigation",
                                      "IntegratedPlotCenter",
                                      "ReferencePoint", "TiePoint", "SmallLiveTreeTallies",
                                      "StumpTallies")
  savedFileNames_tree_level <- c("TreeMeasurements",
                                 "TreeLogAssessments", "TreeDamageOccurrences",
                                 "TreeLossIndicators")
  if(saveFormat == "xlsx"){
    testwb <- openxlsx::createWorkbook()
  }

  for(indifile in c(savedFileNames_site_level,
                    savedFileNames_site_proj_level,
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
    if(indifile %in% savedFileNames_site_level){
      frontnames <- c("SampleSiteName")
      restnames <- indifiledata_names_noid[!(indifiledata_names_noid %in% frontnames)]
      indifiledata <- indifiledata[,c(frontnames, restnames), with = FALSE]
      indifiledata <- indifiledata[order(SampleSiteName),]
    } else if (indifile %in% savedFileNames_site_proj_level){
      frontnames <- c("SampleSiteName", "VisitNumber", "ProjectName", "ProjectNumber")
      restnames <- indifiledata_names_noid[!(indifiledata_names_noid %in% frontnames)]
      indifiledata <- indifiledata[,c(frontnames, restnames), with = FALSE]
      indifiledata <- indifiledata[order(SampleSiteName, VisitNumber, ProjectName, ProjectNumber),]
    } else {
      frontnames <- c("SampleSiteName", "VisitNumber", "ProjectName", "ProjectNumber",
                      "PlotNumber", "TreeNumber")
      restnames <- indifiledata_names_noid[!(indifiledata_names_noid %in% frontnames)]
      indifiledata <- indifiledata[,c(frontnames, restnames), with = FALSE]
      indifiledata <- indifiledata[order(SampleSiteName, VisitNumber, ProjectName, ProjectNumber,
                                         PlotNumber, TreeNumber),]
    }

    if(saveFormat == "xlsx"){
      openxlsx::addWorksheet(testwb, indifile)
      openxlsx::writeData(testwb, indifile, indifiledata)
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
    save(list = c(savedFileNames_site_level,
                  savedFileNames_site_proj_level,
                  savedFileNames_tree_level),
         file = file.path(savePath, paste0(saveName, ".rdata")))
  } else if (saveFormat == "xlsx"){
    openxlsx::saveWorkbook(testwb,
                           file = file.path(savePath, paste0(saveName, ".xlsx")),
                           overwrite = TRUE)
  }
}


getLookupDAS <- function(){
  thedata <- data.table(DamageAgentSeverityGuid = c("44bfd591ff543a71e053e70a0a0a53f9",
                                                    "44bfd591ff553a71e053e70a0a0a53f9", "44bfd591ff563a71e053e70a0a0a53f9",
                                                    "44bfd591ff573a71e053e70a0a0a53f9", "44bfd591ff583a71e053e70a0a0a53f9",
                                                    "44bfd591ff593a71e053e70a0a0a53f9", "44bfd591ff5a3a71e053e70a0a0a53f9",
                                                    "44bfd591ff5b3a71e053e70a0a0a53f9", "44bfd591ff5c3a71e053e70a0a0a53f9",
                                                    "44bfd591ff5d3a71e053e70a0a0a53f9", "44bfd591ff5e3a71e053e70a0a0a53f9",
                                                    "44bfd591ff5f3a71e053e70a0a0a53f9", "44bfd591ff603a71e053e70a0a0a53f9",
                                                    "44bfd591ff613a71e053e70a0a0a53f9", "44bfd591ff623a71e053e70a0a0a53f9",
                                                    "44bfd591ff633a71e053e70a0a0a53f9", "44bfd591ff643a71e053e70a0a0a53f9",
                                                    "44bfd591ff653a71e053e70a0a0a53f9", "44bfd591ff663a71e053e70a0a0a53f9",
                                                    "44bfd591ff673a71e053e70a0a0a53f9", "44bfd591ff683a71e053e70a0a0a53f9",
                                                    "44bfd591ff693a71e053e70a0a0a53f9", "44bfd591ff6a3a71e053e70a0a0a53f9",
                                                    "44bfd591ff6b3a71e053e70a0a0a53f9", "44bfd591ff6c3a71e053e70a0a0a53f9",
                                                    "44bfd591ff6d3a71e053e70a0a0a53f9", "44bfd591ff6e3a71e053e70a0a0a53f9",
                                                    "44bfd591ff6f3a71e053e70a0a0a53f9", "44bfd591ff703a71e053e70a0a0a53f9",
                                                    "44bfd591ff713a71e053e70a0a0a53f9", "44bfd591ff723a71e053e70a0a0a53f9",
                                                    "44bfd591ff733a71e053e70a0a0a53f9", "44bfd591ff743a71e053e70a0a0a53f9",
                                                    "44bfd591ff753a71e053e70a0a0a53f9", "44bfd591ff763a71e053e70a0a0a53f9",
                                                    "44bfd591ff773a71e053e70a0a0a53f9", "44bfd591ff783a71e053e70a0a0a53f9",
                                                    "44bfd591ff793a71e053e70a0a0a53f9", "44bfd591ff7a3a71e053e70a0a0a53f9",
                                                    "44bfd591ff7b3a71e053e70a0a0a53f9", "44bfd591ff7c3a71e053e70a0a0a53f9",
                                                    "44bfd591ff7d3a71e053e70a0a0a53f9", "44bfd591ff7e3a71e053e70a0a0a53f9",
                                                    "44bfd591ff7f3a71e053e70a0a0a53f9", "44bfd591ff803a71e053e70a0a0a53f9",
                                                    "44bfd591ff813a71e053e70a0a0a53f9", "44bfd591ff823a71e053e70a0a0a53f9",
                                                    "44bfd591ff833a71e053e70a0a0a53f9", "44bfd591ff843a71e053e70a0a0a53f9",
                                                    "44bfd591ff853a71e053e70a0a0a53f9", "44bfd591ff863a71e053e70a0a0a53f9",
                                                    "44bfd591ff873a71e053e70a0a0a53f9", "44bfd591ff883a71e053e70a0a0a53f9",
                                                    "6cf8f949c590473ce053e70a0a0a26ce", "44bfd591ff893a71e053e70a0a0a53f9",
                                                    "44bfd591ff8a3a71e053e70a0a0a53f9", "44bfd591ff8b3a71e053e70a0a0a53f9",
                                                    "44bfd591ff8c3a71e053e70a0a0a53f9", "44bfd591ff8d3a71e053e70a0a0a53f9",
                                                    "44bfd591ff8e3a71e053e70a0a0a53f9", "44bfd591ff8f3a71e053e70a0a0a53f9",
                                                    "44bfd591ff903a71e053e70a0a0a53f9", "44bfd591ff913a71e053e70a0a0a53f9",
                                                    "44bfd591ff923a71e053e70a0a0a53f9", "44bfd591ff933a71e053e70a0a0a53f9",
                                                    "44bfd591ff943a71e053e70a0a0a53f9", "44bfd591ff953a71e053e70a0a0a53f9",
                                                    "44bfd591ff963a71e053e70a0a0a53f9", "44bfd591ff973a71e053e70a0a0a53f9",
                                                    "44bfd591ff983a71e053e70a0a0a53f9", "44bfd591ff993a71e053e70a0a0a53f9",
                                                    "44bfd591ff9a3a71e053e70a0a0a53f9", "44bfd591ff9b3a71e053e70a0a0a53f9",
                                                    "44bfd591ff9c3a71e053e70a0a0a53f9", "44bfd591ff9d3a71e053e70a0a0a53f9",
                                                    "44bfd591ff9e3a71e053e70a0a0a53f9", "44bfd591ff9f3a71e053e70a0a0a53f9",
                                                    "44bfd591ffa03a71e053e70a0a0a53f9", "44bfd591ffa13a71e053e70a0a0a53f9",
                                                    "44bfd591ffa23a71e053e70a0a0a53f9", "44bfd591ffa33a71e053e70a0a0a53f9",
                                                    "44bfd591ffa43a71e053e70a0a0a53f9", "44bfd591ffa53a71e053e70a0a0a53f9",
                                                    "44bfd591ffa63a71e053e70a0a0a53f9", "44bfd591ffa73a71e053e70a0a0a53f9",
                                                    "44bfd591ffa83a71e053e70a0a0a53f9", "44bfd591ffa93a71e053e70a0a0a53f9",
                                                    "44bfd591ffaa3a71e053e70a0a0a53f9", "44bfd591ffab3a71e053e70a0a0a53f9",
                                                    "44bfd591ffac3a71e053e70a0a0a53f9", "44bfd591ffad3a71e053e70a0a0a53f9",
                                                    "44bfd591ffae3a71e053e70a0a0a53f9", "44bfd591ffaf3a71e053e70a0a0a53f9",
                                                    "44bfd591ffb03a71e053e70a0a0a53f9", "44bfd591ffb13a71e053e70a0a0a53f9",
                                                    "44bfd591ffb23a71e053e70a0a0a53f9", "44bfd591ffb33a71e053e70a0a0a53f9",
                                                    "44bfd591ffb43a71e053e70a0a0a53f9", "44bfd591ffb53a71e053e70a0a0a53f9",
                                                    "44bfd591ffb63a71e053e70a0a0a53f9", "44bfd591ffb73a71e053e70a0a0a53f9",
                                                    "44bfd591ffb83a71e053e70a0a0a53f9", "44bfd591ffb93a71e053e70a0a0a53f9",
                                                    "44bfd591ffba3a71e053e70a0a0a53f9", "44bfd591ffbb3a71e053e70a0a0a53f9",
                                                    "44bfd591ffbc3a71e053e70a0a0a53f9", "44bfd591ffbd3a71e053e70a0a0a53f9",
                                                    "44bfd591ffbe3a71e053e70a0a0a53f9", "44bfd591ffbf3a71e053e70a0a0a53f9",
                                                    "44bfd591ffc03a71e053e70a0a0a53f9", "44bfd591ffc13a71e053e70a0a0a53f9",
                                                    "44bfd591ffc23a71e053e70a0a0a53f9", "44bfd591ffc33a71e053e70a0a0a53f9",
                                                    "44bfd591ffc43a71e053e70a0a0a53f9", "44bfd591ffc53a71e053e70a0a0a53f9",
                                                    "44bfd591ffc63a71e053e70a0a0a53f9", "44bfd591ffc73a71e053e70a0a0a53f9",
                                                    "44bfd591ffc83a71e053e70a0a0a53f9", "44bfd591ffc93a71e053e70a0a0a53f9",
                                                    "44bfd591ffca3a71e053e70a0a0a53f9", "44bfd591ffcb3a71e053e70a0a0a53f9",
                                                    "44bfd591ffcc3a71e053e70a0a0a53f9", "44bfd591ffcd3a71e053e70a0a0a53f9",
                                                    "44bfd591ffce3a71e053e70a0a0a53f9", "44bfd591ffcf3a71e053e70a0a0a53f9",
                                                    "44bfd591ffd03a71e053e70a0a0a53f9", "44bfd591ffd13a71e053e70a0a0a53f9",
                                                    "44bfd591ffd23a71e053e70a0a0a53f9", "44bfd591ffd33a71e053e70a0a0a53f9",
                                                    "44bfd591ffd43a71e053e70a0a0a53f9", "44bfd591ffd53a71e053e70a0a0a53f9",
                                                    "44bfd591ffd63a71e053e70a0a0a53f9", "44bfd591ffd73a71e053e70a0a0a53f9",
                                                    "44bfd591ffd83a71e053e70a0a0a53f9", "44bfd591ffd93a71e053e70a0a0a53f9",
                                                    "44bfd592001d3a71e053e70a0a0a53f9", "44bfd592001e3a71e053e70a0a0a53f9",
                                                    "44bfd592001f3a71e053e70a0a0a53f9", "44bfd59200203a71e053e70a0a0a53f9",
                                                    "44bfd59200213a71e053e70a0a0a53f9", "44bfd59200223a71e053e70a0a0a53f9",
                                                    "44bfd59200233a71e053e70a0a0a53f9", "44bfd59200243a71e053e70a0a0a53f9",
                                                    "7b99d0971dd55691e053e70a0a0a1dd3", "44bfd591ffda3a71e053e70a0a0a53f9",
                                                    "44bfd591ffdb3a71e053e70a0a0a53f9", "44bfd591ffdc3a71e053e70a0a0a53f9",
                                                    "44bfd591ffdd3a71e053e70a0a0a53f9", "44bfd591ffde3a71e053e70a0a0a53f9",
                                                    "44bfd591ffdf3a71e053e70a0a0a53f9", "44bfd591ffe03a71e053e70a0a0a53f9",
                                                    "44bfd591ffe13a71e053e70a0a0a53f9", "44bfd591ffe23a71e053e70a0a0a53f9",
                                                    "44bfd591ffe33a71e053e70a0a0a53f9", "44bfd591ffe43a71e053e70a0a0a53f9",
                                                    "44bfd591ffe53a71e053e70a0a0a53f9", "44bfd591ffe63a71e053e70a0a0a53f9",
                                                    "44bfd591ffe73a71e053e70a0a0a53f9", "44bfd591ffe83a71e053e70a0a0a53f9",
                                                    "44bfd591ffe93a71e053e70a0a0a53f9", "44bfd591ffea3a71e053e70a0a0a53f9",
                                                    "44bfd591ffeb3a71e053e70a0a0a53f9", "44bfd591ffec3a71e053e70a0a0a53f9",
                                                    "44bfd591ffed3a71e053e70a0a0a53f9", "44bfd591ffee3a71e053e70a0a0a53f9",
                                                    "44bfd591ffef3a71e053e70a0a0a53f9", "44bfd591fff03a71e053e70a0a0a53f9",
                                                    "44bfd591fff13a71e053e70a0a0a53f9", "44bfd591fff23a71e053e70a0a0a53f9",
                                                    "44bfd591fff33a71e053e70a0a0a53f9", "44bfd591fff43a71e053e70a0a0a53f9",
                                                    "44bfd591fff53a71e053e70a0a0a53f9", "44bfd591fff63a71e053e70a0a0a53f9",
                                                    "44bfd591fff73a71e053e70a0a0a53f9", "44bfd591fff83a71e053e70a0a0a53f9",
                                                    "44bfd591fff93a71e053e70a0a0a53f9", "44bfd591fffa3a71e053e70a0a0a53f9",
                                                    "44bfd591fffb3a71e053e70a0a0a53f9", "44bfd591fffc3a71e053e70a0a0a53f9",
                                                    "44bfd591fffd3a71e053e70a0a0a53f9", "44bfd591fffe3a71e053e70a0a0a53f9",
                                                    "44bfd591ffff3a71e053e70a0a0a53f9", "44bfd59200003a71e053e70a0a0a53f9",
                                                    "44bfd59200013a71e053e70a0a0a53f9", "44bfd59200023a71e053e70a0a0a53f9",
                                                    "44bfd59200033a71e053e70a0a0a53f9", "44bfd59200043a71e053e70a0a0a53f9",
                                                    "44bfd59200053a71e053e70a0a0a53f9", "44bfd59200063a71e053e70a0a0a53f9",
                                                    "44bfd59200073a71e053e70a0a0a53f9", "44bfd59200083a71e053e70a0a0a53f9",
                                                    "44bfd59200093a71e053e70a0a0a53f9", "44bfd592000a3a71e053e70a0a0a53f9",
                                                    "44bfd592000b3a71e053e70a0a0a53f9", "44bfd592000c3a71e053e70a0a0a53f9",
                                                    "44bfd592000d3a71e053e70a0a0a53f9", "44bfd592000e3a71e053e70a0a0a53f9",
                                                    "44bfd592000f3a71e053e70a0a0a53f9", "44bfd59200103a71e053e70a0a0a53f9",
                                                    "44bfd59200113a71e053e70a0a0a53f9", "44bfd59200123a71e053e70a0a0a53f9",
                                                    "44bfd59200133a71e053e70a0a0a53f9", "44bfd59200143a71e053e70a0a0a53f9",
                                                    "44bfd59200153a71e053e70a0a0a53f9", "44bfd59200163a71e053e70a0a0a53f9",
                                                    "44bfd59200173a71e053e70a0a0a53f9", "44bfd59200183a71e053e70a0a0a53f9",
                                                    "44bfd59200193a71e053e70a0a0a53f9", "44bfd592001a3a71e053e70a0a0a53f9",
                                                    "44bfd592001b3a71e053e70a0a0a53f9", "44bfd592001c3a71e053e70a0a0a53f9",
                                                    "44bfd591ff113a71e053e70a0a0a53f9", "44bfd591ff123a71e053e70a0a0a53f9",
                                                    "44bfd591ff133a71e053e70a0a0a53f9", "44bfd591ff143a71e053e70a0a0a53f9",
                                                    "44bfd591ff153a71e053e70a0a0a53f9", "44bfd591ff163a71e053e70a0a0a53f9",
                                                    "44bfd591ff173a71e053e70a0a0a53f9", "44bfd591ff183a71e053e70a0a0a53f9",
                                                    "44bfd591ff193a71e053e70a0a0a53f9", "44bfd591ff1a3a71e053e70a0a0a53f9",
                                                    "44bfd591ff1b3a71e053e70a0a0a53f9", "44bfd591ff1c3a71e053e70a0a0a53f9",
                                                    "44bfd591ff1d3a71e053e70a0a0a53f9", "44bfd591ff1e3a71e053e70a0a0a53f9",
                                                    "44bfd591ff1f3a71e053e70a0a0a53f9", "44bfd591ff203a71e053e70a0a0a53f9",
                                                    "44bfd591ff213a71e053e70a0a0a53f9", "44bfd591ff223a71e053e70a0a0a53f9",
                                                    "44bfd591ff233a71e053e70a0a0a53f9", "44bfd591ff243a71e053e70a0a0a53f9",
                                                    "44bfd591ff253a71e053e70a0a0a53f9", "44bfd591ff263a71e053e70a0a0a53f9",
                                                    "44bfd591ff273a71e053e70a0a0a53f9", "44bfd591ff283a71e053e70a0a0a53f9",
                                                    "44bfd591ff293a71e053e70a0a0a53f9", "44bfd591ff2a3a71e053e70a0a0a53f9",
                                                    "44bfd591ff2b3a71e053e70a0a0a53f9", "44bfd591ff2c3a71e053e70a0a0a53f9",
                                                    "44bfd591ff2d3a71e053e70a0a0a53f9", "44bfd591ff2e3a71e053e70a0a0a53f9",
                                                    "44bfd591ff2f3a71e053e70a0a0a53f9", "44bfd591ff303a71e053e70a0a0a53f9",
                                                    "44bfd591ff313a71e053e70a0a0a53f9", "44bfd591ff323a71e053e70a0a0a53f9",
                                                    "44bfd591ff333a71e053e70a0a0a53f9", "44bfd591ff343a71e053e70a0a0a53f9",
                                                    "44bfd591ff353a71e053e70a0a0a53f9", "44bfd591ff363a71e053e70a0a0a53f9",
                                                    "44bfd591ff373a71e053e70a0a0a53f9", "44bfd591ff383a71e053e70a0a0a53f9",
                                                    "44bfd591ff393a71e053e70a0a0a53f9", "44bfd591ff3a3a71e053e70a0a0a53f9",
                                                    "44bfd591ff3b3a71e053e70a0a0a53f9", "44bfd591ff3c3a71e053e70a0a0a53f9",
                                                    "44bfd591ff3d3a71e053e70a0a0a53f9", "44bfd591ff3e3a71e053e70a0a0a53f9",
                                                    "44bfd591ff3f3a71e053e70a0a0a53f9", "44bfd591ff403a71e053e70a0a0a53f9",
                                                    "44bfd591ff413a71e053e70a0a0a53f9", "44bfd591ff423a71e053e70a0a0a53f9",
                                                    "44bfd591ff433a71e053e70a0a0a53f9", "44bfd591ff443a71e053e70a0a0a53f9",
                                                    "44bfd591ff453a71e053e70a0a0a53f9", "44bfd591ff463a71e053e70a0a0a53f9",
                                                    "44bfd591ff473a71e053e70a0a0a53f9", "44bfd591ff483a71e053e70a0a0a53f9",
                                                    "44bfd591ff493a71e053e70a0a0a53f9", "44bfd591ff4a3a71e053e70a0a0a53f9",
                                                    "44bfd591ff4b3a71e053e70a0a0a53f9", "44bfd591ff4c3a71e053e70a0a0a53f9",
                                                    "44bfd591ff4d3a71e053e70a0a0a53f9", "44bfd591ff4e3a71e053e70a0a0a53f9",
                                                    "44bfd591ff4f3a71e053e70a0a0a53f9", "44bfd591ff503a71e053e70a0a0a53f9",
                                                    "44bfd591ff513a71e053e70a0a0a53f9", "44bfd591ff523a71e053e70a0a0a53f9",
                                                    "44bfd591ff533a71e053e70a0a0a53f9"),
                        DamageAgentCode = c("DDG",
                                            "DDH", "DDO", "DDP", "DDQ", "DDS", "DDT", "DF", "DFA", "DFB",
                                            "DFC", "DFD", "DFE", "DFF", "DFG", "DFH", "DFI", "DFJ", "DFK",
                                            "DFL", "DFM", "DFN", "DFO", "DFP", "DFQ", "DFR", "DFS", "DFT",
                                            "DFU", "DFW", "DFX", "DFY", "DFZ", "DL", "DLD", "DLF", "DLK",
                                            "DLP", "DLS", "DLV", "DM", "DMF", "DMH", "DML", "DMP", "DR",
                                            "DRA", "DRB", "DRC", "DRL", "DRN", "DRR", "DRS", "DRS", "DRT",
                                            "DS", "DSA", "DSB", "DSC", "DSE", "DSG", "DSH", "DSP", "DSR",
                                            "DSS", "DST", "DSY", "I", "IA", "IAB", "IAC", "IAG", "IAL", "IAS",
                                            "IB", "IBB", "IBD", "IBE", "IBF", "IBH", "IBI", "IBL", "IBM",
                                            "IBP", "IBR", "IBS", "IBT", "IBW", "ID", "ID1", "ID2", "ID3",
                                            "ID4", "ID5", "ID6", "ID7", "ID8", "ID9", "IDA", "IDB", "IDC",
                                            "IDD", "IDE", "IDF", "IDG", "IDH", "IDI", "IDJ", "IDK", "IDL",
                                            "IDM", "IDN", "IDO", "IDP", "IDQ", "IDR", "IDS", "IDT", "IDU",
                                            "IDV", "IDW", "IDX", "IDY", "IDZ", "IEA", "IEB", "IEC", "IED",
                                            "IEF", "IEG", "IEH", "IEI", "IEJ", "IEK", "IEL", "UCR", "UF",
                                            "USW", "V", "VH", "VP", "VS", "VT", "DFE", "IS", "ISA", "ISB",
                                            "ISC", "ISE", "ISG", "ISP", "ISQ", "ISS", "ISW", "IW", "IWC",
                                            "IWM", "IWP", "IWS", "IWW", "IWY", "IWZ", "M", "N", "NAV", "NB",
                                            "NBP", "NCA", "NCB", "NCY", "ND", "NF", "NG", "NGC", "NGH", "NGK",
                                            "NH", "NK", "NL", "NN", "NR", "NS", "NW", "NWS", "NWT", "NX",
                                            "NY", "NZ", "O", "P", "PAX", "PBC", "PCD", "PCF", "PCP", "PDT",
                                            "PFX", "PPG", "PPX", "PSS", "PTX", "T", "TC", "TL", "TM", "TP",
                                            "TPM", "TR", "TT", "U", "UBT", "A", "AB", "AC", "AD", "AE", "AH",
                                            "AM", "AO", "AP", "AS", "AV", "AX", "AZ", "C", "CAH", "CBC",
                                            "CBX", "CCP", "CDC", "CDD", "CDR", "CDX", "CEA", "CEB", "CEQ",
                                            "CEX", "CFP", "CHX", "CIA", "CIP", "CIR", "CIS", "CIV", "CIX",
                                            "CLO", "CMA", "CMC", "CML", "CMP", "CMR", "CMS", "CMT", "CMX",
                                            "CNP", "CPS", "CRX", "CSN", "CTO", "CTW", "CVP", "CVR", "CYC",
                                            "CYP", "CYS", "CYT", "CYX", "D", "DB", "DBF", "DBS", "DD", "DDA",
                                            "DDB", "DDC", "DDD", "DDE", "DDF"),
                        DamageAgentSeverityCode = c("MORTCOND",
                                                    "MORTCOND", "MORTCOND", "MORTCOND", "MORTCOND", "ROOTROT", "MORTCOND",
                                                    "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "DMISTTL9",
                                                    "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH",
                                                    "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH",
                                                    "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH",
                                                    "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH",
                                                    "PERHYPH", "PERHYPH", "PERHYPH", "DMISTTL9", "DMISTTL9", "DMISTTL9",
                                                    "DMISTTL9", "DMISTTL9", "ROOTROT", "ROOTROT", "ROOTROT", "ROOTROT",
                                                    "ROOTROT", "ROOTROT", "ROOTROT", "ROOTROT", "MORTCOND", "ROOTROT",
                                                    "STEMRUST", "STEMRUST", "STEMRUST", "STEMRUST", "STEMRUST", "STEMRUST",
                                                    "STEMRUST", "STEMRUST", "STEMRUST", "STEMRUST", "STEMRUST", "STEMRUST",
                                                    "MORTCOND", "MORTAPHD", "MORTAPHD", "MORTAPHD", "MORTAPHD", "MORTAPHD",
                                                    "MORTAPHD", "BBEETLE", "BBEETLE", "BBEETLE", "BBEETLE", "BBEETLE",
                                                    "BBEETLE", "BBEETLE", "BBEETLE", "BBEETLE", "BBEETLE", "BBEETLE",
                                                    "BBEETLE", "BBEETLE", "BBEETLE", "PERHYPH", "PERHYPH", "PERHYPH",
                                                    "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH",
                                                    "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH",
                                                    "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH",
                                                    "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH",
                                                    "PERHYPH", "PERHYPH", "DEFOMOTH", "PERHYPH", "PERHYPH", "DEFOBUD",
                                                    "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH",
                                                    "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH",
                                                    "PERHYPH", "PERHYPH", "MRTCNDXD", "MRTCNDXD", "MRTCNDXD", "MORTCOND",
                                                    "MORTCOND", "MORTCOND", "MORTCOND", "MORTCOND", "PERHVIL", "PERHVIL",
                                                    "PERHVIL", "PERHVIL", "PERHVIL", "PERHVIL", "PERHVIL", "PERHVIL",
                                                    "PERHVIL", "PERHVIL", "PERHVIL", "TWEEVIL", "TWEEVIL", "TWEEVIL",
                                                    "TWEEVIL", "TWEEVIL", "TWEEVIL", "TWEEVIL", "TWEEVIL", "MORTCOND",
                                                    "MORTCOND", "MRTCNDXD", "MORTCOND", "MRTCNDXD", "MORTCOND", "MORTCOND",
                                                    "MORTCOND", "MORTCOND", "MORTCOND", "MORTCOND", "MORTCOND", "MORTCOND",
                                                    "MORTCOND", "MORTCOND", "MORTCOND", "MORTCOND", "MORTCOND", "MORTCOND",
                                                    "MORTCOND", "MORTCOND", "MORTCOND", "MORTCOND", "MORTCOND", "MORTCOND",
                                                    "MORTCOND", "MRTCNDXD", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH",
                                                    "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH",
                                                    "PERHYPH", "PERHYPH", "MORTCOND", "MORTCOND", "MORTCOND", "MORTCOND",
                                                    "MORTCOND", "MORTCOND", "MORTCOND", "MORTCOND", "MORTCOND", "MRTCNDXD",
                                                    "MORTMAMM", "MORTMAMM", "MORTMAMM", "MORTMAMM", "MORTMAMM", "MORTMAMM",
                                                    "MORTMAMM", "MORTMAMM", "MORTMAMM", "MORTMAMM", "MORTMAMM", "MORTMAMM",
                                                    "MORTMAMM", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH",
                                                    "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH",
                                                    "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH",
                                                    "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH",
                                                    "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH",
                                                    "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH",
                                                    "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH", "PERHYPH",
                                                    "PERHYPH", "PERHYPH", "MORTCOND", "MORTBRRU", "MORTBRRU", "MORTBRRU",
                                                    "MORTCOND", "MORTCOND", "MORTCOND", "MORTCOND", "MORTCOND", "MORTCOND",
                                                    "MORTCOND"))
  return(thedata)
}





