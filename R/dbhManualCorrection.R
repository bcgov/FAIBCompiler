dbhManualCorrection <- function(treeID,
                                visitNum,
                                diameterOrg){
  thedatatable <- data.table(uniid = 1:length(treeID),
                             unitreeid = treeID,
                             VISIT_NUMBER = visitNum,
                             diameterOrg = diameterOrg)
  thedatatable[, diam_new := as.numeric(NA)]
  thedatatable[(unitreeid == "4000429-1-58" & VISIT_NUMBER == 1) |
                 (unitreeid == "4000884-1-593" & VISIT_NUMBER == 2) |
                 (unitreeid == "4024798-1-111" & VISIT_NUMBER == 1) |
                 (unitreeid == "4032920-1-163" & VISIT_NUMBER == 1) |
                 (unitreeid == "4044252-1-322" & VISIT_NUMBER == 1) |
                 (unitreeid == "4044253-1-2" & VISIT_NUMBER == 1) |
                 (unitreeid == "4044253-1-4" & VISIT_NUMBER == 1) |
                 (unitreeid == "4044253-1-7" & VISIT_NUMBER == 1) |
                 (unitreeid == "4044484-1-172" & VISIT_NUMBER == 1) |
                 (unitreeid == "4045167-1-110" & VISIT_NUMBER == 1) |
                 (unitreeid == "4045167-1-113" & VISIT_NUMBER == 1) |
                 (unitreeid == "4045167-1-120" & VISIT_NUMBER == 1) |
                 (unitreeid == "4045167-1-164" & VISIT_NUMBER == 1) |
                 (unitreeid == "4047241-1-360" & VISIT_NUMBER == 1) |
                 (unitreeid == "4053322-1-185" & VISIT_NUMBER == 1) |
                 (unitreeid == "4001519-1-601" & VISIT_NUMBER == 5) |
                 (unitreeid == "4007504-1-1" & VISIT_NUMBER == 5) |
                 (unitreeid == "4016157-1-264" & VISIT_NUMBER == 3) |
                 (unitreeid == "4024299-1-45" & VISIT_NUMBER == 3) |
                 (unitreeid == "4036820-1-55" & VISIT_NUMBER == 10) |
                 (unitreeid == "4041848-1-107" & VISIT_NUMBER == 2) |
                 (unitreeid == "4042260-1-76" & VISIT_NUMBER == 3) |
                 (unitreeid == "4044764-1-284" & VISIT_NUMBER == 4) |
                 (unitreeid == "4044806-1-91" & VISIT_NUMBER == 4) |
                 (unitreeid == "4045386-1-727" & VISIT_NUMBER == 5) |
                 (unitreeid == "4001520-1-600" & VISIT_NUMBER %in% c(4, 5, 6)) |
                 (unitreeid == "4001595-1-71" & VISIT_NUMBER == 4) |
                 (unitreeid == "4001595-1-97" & VISIT_NUMBER == 4) |
                 (unitreeid == "4001869-1-82" & VISIT_NUMBER %in% c(4, 5)) |
                 (unitreeid == "4001979-1-601" & VISIT_NUMBER == 4) |
                 (unitreeid == "4001979-1-1900" & VISIT_NUMBER == 4) |
                 (unitreeid == "4001979-1-1900" & VISIT_NUMBER == 4),
               diam_new := diameterOrg/10]
  thedatatable[(unitreeid == "4002083-1-105" & VISIT_NUMBER == 5) |
                 (unitreeid == "4002083-1-96" & VISIT_NUMBER == 5) |
                 (unitreeid == "4002110-1-447" & VISIT_NUMBER == 3) |
                 (unitreeid == "4002156-1-176" & VISIT_NUMBER == 4) |
                 (unitreeid == "4009397-1-70" & VISIT_NUMBER == 3) |
                 (unitreeid == "4010667-1-225" & VISIT_NUMBER == 4) |
                 (unitreeid == "4010697-1-276" & VISIT_NUMBER == 5) |
                 (unitreeid == "4011867-1-33" & VISIT_NUMBER == 3) |
                 (unitreeid == "4013568-1-428" & VISIT_NUMBER == 7) |
                 (unitreeid == "4023362-1-216" & VISIT_NUMBER == 5) |
                 (unitreeid == "4023371-1-112" & VISIT_NUMBER == 5) |
                 (unitreeid == "4023569-1-191" & VISIT_NUMBER == 4) |
                 (unitreeid == "4024537-1-615" & VISIT_NUMBER == 2) |
                 (unitreeid == "4032543-1-80" & VISIT_NUMBER == 2) |
                 (unitreeid == "4033590-1-262" & VISIT_NUMBER == 5) |
                 (unitreeid == "4034142-1-242" & VISIT_NUMBER == 3) |
                 (unitreeid == "4041535-1-45" & VISIT_NUMBER == 2) |
                 (unitreeid == "4041819-1-229" & VISIT_NUMBER == 2) |
                 (unitreeid == "4044249-1-127" & VISIT_NUMBER == 2) |
                 (unitreeid == "4044460-1-149" & VISIT_NUMBER == 2) |
                 (unitreeid == "4051120-1-200" & VISIT_NUMBER == 4),
               diam_new := diameterOrg*10]

  ## special cases
  thedatatable[unitreeid == "4028474-1-256" & VISIT_NUMBER == 2,
               diam_new := 28.2]
  thedatatable[unitreeid == "4042078-1-2409" & VISIT_NUMBER == 1,
               diam_new := 18.1]
  thedatatable[unitreeid == "4045316-1-90" & VISIT_NUMBER == 2,
               diam_new := 6.5]
  thedatatable[unitreeid == "4019304-1-101" & VISIT_NUMBER == 3,
               diam_new := 19.1]
  thedatatable[unitreeid == "4021033-1-321" & VISIT_NUMBER == 5,
               diam_new := 2]
  thedatatable[unitreeid == "4030742-1-256" & VISIT_NUMBER == 2,
               diam_new := 7.3]
  thedatatable[unitreeid == "4032542-1-82" & VISIT_NUMBER == 2,
               diam_new := 16.7]
  thedatatable[unitreeid == "4000432-1-266" & VISIT_NUMBER %in% c(2, 3),
               diam_new := 9.3]
  thedatatable[unitreeid == "4000449-1-10" & VISIT_NUMBER == 4,
               diam_new := 34.2]
  thedatatable[unitreeid == "4001870-1-261" & VISIT_NUMBER %in% c(4, 5, 6),
               diam_new := 26.5]



  thedatatable <- thedatatable[order(uniid),]
  return(thedatatable$diam_new)
}
