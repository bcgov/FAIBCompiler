#' Setup an output path of the compiler
#'
#' @description This function does two things: 1. create a folder that will store compiled data;
#'              2. return a path that directs the compiled folder.
#'
#' @param compilationPath character, Specifies a path to store whole compilation process.
#'                              If missing, the current work directory will be used.
#'
#' @return Four paths will be returned as following:
#' \itemize{
#'
#' \item {raw_from_oracle} {Path to save all data that read from both oracle and txt database without merging.}
#'
#' \item {compilation_sa} {Path to save merged data for key tables (i.e., vi_a to vi.g) from both oracle and txt database.}
#'
#' \item {compilation_db} {Path to save compiled outputs.}
#'
#' \item {compilation_archive} {Path to archive all compilation process.}
#' }
#'
#' @note Could overwrite the existing output folder, depending on user's choise, i.e., yes or no.
#'
#' @export
#' @docType methods
#' @rdname compilerPathSetup
#'
#' @author Yong Luo
#'
compilerPathSetup <- function(compilationPath = "."){
  if(!dir.exists(compilationPath)){
    dir.create(compilationPath)
  }
  raw_from_oracle <- file.path(compilationPath, "raw_from_oracle")
  compilation_sa <- file.path(compilationPath, "compilation_sa")
  compilation_db <- file.path(compilationPath, "compilation_db")
  compilation_archive <- file.path(compilationPath,
                           paste("Archive_", gsub("-", "", as.character(as.Date(Sys.time()))), sep = ""))
  if(dir.exists(raw_from_oracle)){
    unlink(raw_from_oracle, recursive = TRUE)
  }
  if(dir.exists(compilation_sa)){
    unlink(compilation_sa, recursive = TRUE)
  }
  if(dir.exists(compilation_db)){
    unlink(compilation_db, recursive = TRUE)
  }
  if(dir.exists(compilation_archive)){
    unlink(compilation_archive, recursive = TRUE)
  }
  dir.create(raw_from_oracle)
  dir.create(compilation_sa)
  dir.create(compilation_db)
  dir.create(compilation_archive)
  return(list(raw_from_oracle = raw_from_oracle,
              compilation_sa = compilation_sa,
              compilation_db = compilation_db,
              compilation_archive = compilation_archive))
}
