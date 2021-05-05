#' Setup output paths of the compiler
#'
#' @description This function does two things: 1. create folders that will store compiled data;
#'              2. return paths that directs the compiled folder.
#'
#' @param compilationPath character, Specifies a path to store whole compilation process.
#'                              If missing, the current work directory will be used.
#' @param compilationDate character, Specifies a compilation date. It should be in format of YYYYMMDD
#'                              It will be used for archive the compilation outputs.
#'
#' @return Seven paths will be returned as following:
#' \itemize{
#'
#' \item {raw_from_oracle} {Path to save all data that read from both oracle and txt database without merging.}
#'
#' \item {compilation_sa} {Path to save merged data for key tables (i.e., vi_a to vi.g) from both oracle and txt database.}
#'
#' \item {compilation_db} {Path to save compiled outputs.}
#'
#' \item {compilation_archive} {Path to archive all compilation process.}
#'
#' \item {compilation_report} {Path to report compilation process.}
#'
#' \item {compilation_map} {Path to archive all maps for compilation process.}
#'
#' \item {compilation_coeff} {Path to archive all coefficients for compilation process.}
#' }
#' \item {compilation_last} {Path to archive last compilation process.}
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
compilerPathSetup <- function(compilationPath = ".",
                              compilationDate){
  if(!dir.exists(compilationPath)){
    dir.create(compilationPath)
  }
  allarchives <- dir(compilationPath, pattern = "Archive_")
  allarchives <- allarchives[nchar(allarchives)==16]
  lastarchive <- paste0("Archive_", max(as.numeric(gsub("Archive_", "", allarchives))))
  last_compilation <- file.path(compilationPath, lastarchive)

  raw_from_oracle <- file.path(compilationPath, "raw_from_oracle")
  compilation_sa <- file.path(compilationPath, "compilation_sa")
  compilation_db <- file.path(compilationPath, "compilation_db")
  compilation_archive <- file.path(compilationPath,
                                   paste("Archive_",
                                         compilationDate,
                                         sep = ""))
  compilation_map <- file.path(compilationPath, "compilation_map")
  compilation_coeff <- file.path(compilationPath, "compilation_coeff")
  compilation_report <- file.path(compilationPath, "compilation_report")
  ## for raw, sa, db, archive and report,
  ## remove the existing ones and create empty folders
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
  ## for coeff and map, create empty one if they do not exist,
  ## otherwise keep them
  if(!dir.exists(compilation_report)){
    dir.create(compilation_report)
  }
  if(!dir.exists(compilation_coeff)){
    dir.create(compilation_coeff)
  }
  if(!dir.exists(compilation_map)){
    dir.create(compilation_map)
  }

  return(list(raw_from_oracle = raw_from_oracle,
              compilation_sa = compilation_sa,
              compilation_db = compilation_db,
              compilation_archive = compilation_archive,
              compilation_report = compilation_report,
              compilation_coeff = compilation_coeff,
              compilation_map = compilation_map,
              compilation_last = last_compilation))
}
