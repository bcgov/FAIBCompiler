#' Assign BEC based on project ID
#' 
#' 
#' @description This function takes lookup table that connects project id to BEC zone and joins BEC zone
#'              by project id. The function uses a hardcoded lookup table \code{vri_bec}.  
#'              The function is equivalent to \code{group_bec.sas}. 
#'
#' @param projectID character, Specifies project ID.
#'                                       
#' @return BEC, \code{Unknown} will be return if project id does have any match in lookup table.
#' 
#' @importFrom data.table ':='
#'
#' 
#' @export
#' @docType methods
#' @rdname prj_ID2BEC
#'
#' @author Yong Luo
#'
setGeneric("prj_ID2BEC",
           function(projectID) {
             standardGeneric("prj_ID2BEC")
           })

#' @rdname prj_ID2BEC
setMethod(
  "prj_ID2BEC",
  signature = c(projectID = "character"),
  definition = function(projectID){
    worktable <- data.table(uniObs = 1:length(projectID),
                            projectID)
    lookuptable <- lookup_vri_bec()
    worktable <- merge(worktable, lookuptable, by = "projectID",
                       all.x = TRUE)
    
    worktable[is.na(BEC), BEC := "Unknown"]
    return(worktable[order(uniObs)]$BEC)
  })
