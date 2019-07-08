#' Group project ID into project group
#' 
#' 
#' @description This function takes lookup table that connect project id to project group and joins project
#'              group by project id. The function uses hardcoded lookup table \code{vri_grp}.  
#'              The function is equivalent to \code{group_prj.sas}. 
#'
#' @param projectID character, Specifies project ID.
#'                                       
#' @return Project group, \code{Unknown} will be return if project id does have any match in lookup table.
#' 
#' @importFrom data.table ':='
#'
#' 
#' @export
#' @docType methods
#' @rdname prj_ID2Grp
#'
#' @author Yong Luo
#'
setGeneric("prj_ID2Grp",
           function(projectID) {
             standardGeneric("prj_ID2Grp")
           })

#' @rdname prj_ID2Grp
setMethod(
  "prj_ID2Grp",
  signature = c(projectID = "character"),
  definition = function(projectID){
    worktable <- data.table(uniObs = 1:length(projectID),
                            projectID)
    lookuptable <- lookup_vri_grp()
    lookuptable <- lookuptable[projectGroup != "TEST"]
    worktable <- merge(worktable, lookuptable, by = "projectID",
                       all.x = TRUE)
    
    worktable[is.na(projectGroup), projectGroup := "Unknown"]
    return(worktable[order(uniObs)]$projectGroup)
  })



