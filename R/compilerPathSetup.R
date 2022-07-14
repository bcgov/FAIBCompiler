#' Setup output paths of the compiler
#'
#' @description This function does two things: 1. create folders that will store compiled data;
#'              2. return paths that directs the compiled folder.
#'
#' @param compilationPath character, Specifies a path to store whole compilation process.
#'                              If missing, the current work directory will be used.
#' @param compilationDate character, Specifies a compilation date. It should be in format of YYYYMMDD
#'                              It will be used for archive the compilation outputs.
#' @param recompile logical, Defines whether we want to use existing data that downloaded
#'                              previously.
#' @param archiveDate character, Defines on which archive date the raw data were downloaded.
#'                             These raw data will be used for recompilation. Format is YYYYMMDD.
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
                              compilationDate,
                              recompile,
                              archiveDate){
  if(!dir.exists(compilationPath)){
    dir.create(compilationPath)
  }
  allarchives <- dir(compilationPath, pattern = "Archive_")
  allarchives <- allarchives[nchar(allarchives)==16]

  allarchives <- sort(allarchives, decreasing = TRUE)
  containDB <- unlist(lapply(allarchives, function(x){dir.exists(file.path(compilationPath, x, "compilation_db"))}))
  emptyarchives <- allarchives[!containDB]
  allarchives <- allarchives[containDB]
  lapply(emptyarchives,
         function(x){unlink(file.path(compilationPath, x),
                            recursive = TRUE)})

  lastarchive <- paste0("Archive_", max(as.numeric(gsub("Archive_", "", allarchives))))
  last_compilation <- file.path(compilationPath, lastarchive)

  if(recompile == FALSE){
    compilation_archive <- file.path(compilationPath,
                                     paste("Archive_",
                                           compilationDate,
                                           sep = ""))

    raw_from_oracle <- file.path(compilationPath, "raw_from_oracle")
    compilation_sa <- file.path(compilationPath, "compilation_sa")
    compilation_db <- file.path(compilationPath, "compilation_db")
    compilation_map <- file.path(compilationPath, "compilation_map")
    compilation_coeff <- file.path(compilationPath, "compilation_coeff")
    compilation_report <- file.path(compilationPath, "compilation_report")
    ## for raw, sa, db, archive and report,
    ## remove the existing ones and create empty folders

    ## for coeff and map, create empty one if they do not exist,
    ## otherwise keep them
    if(dir.exists(raw_from_oracle)){
      unlink(raw_from_oracle, recursive = TRUE)
    }
    if(dir.exists(compilation_sa)){
      unlink(compilation_sa, recursive = TRUE)
    }
    if(dir.exists(compilation_db)){
      unlink(compilation_db, recursive = TRUE)
    }
    if(!dir.exists(raw_from_oracle)){
      dir.create(raw_from_oracle)
    }
    dir.create(compilation_sa)
    dir.create(compilation_db)
    if(!dir.exists(compilation_report)){
      dir.create(compilation_report)
    }
    if(!dir.exists(compilation_coeff)){
      dir.create(compilation_coeff)
    }
    if(!dir.exists(compilation_map)){
      dir.create(compilation_map)
    }
  } else {
    # create a folder under current archivement
    recompile_folder <- file.path(compilationPath,
                                  paste0("Archive_", archiveDate, "_recomp", compilationDate))
    last_compilation <- recompile_folder
    dir.create(recompile_folder)
    # copy raw, map, coeff, and report into subfolder
    archivefolder <- file.path(compilationPath, paste0("Archive_", archiveDate))
    file.copy(from = file.path(archivefolder, "raw_from_oracle"),
              to = recompile_folder,
              recursive = TRUE)
    file.copy(from = file.path(archivefolder, "compilation_map"),
              to = recompile_folder,
              recursive = TRUE)
    file.copy(from = file.path(archivefolder, "compilation_coeff"),
              to = recompile_folder,
              recursive = TRUE)
    file.copy(from = file.path(archivefolder, "compilation_report"),
              to = recompile_folder,
              recursive = TRUE)
    raw_from_oracle <- file.path(recompile_folder, "raw_from_oracle")
    compilation_sa <- file.path(recompile_folder, "compilation_sa")
    compilation_db <- file.path(recompile_folder, "compilation_db")
    compilation_map <- file.path(recompile_folder, "compilation_map")
    compilation_coeff <- file.path(recompile_folder, "compilation_coeff")
    compilation_report <- file.path(recompile_folder, "compilation_report")
    if(dir.exists(compilation_sa)){
      unlink(compilation_sa, recursive = TRUE)
    }
    if(dir.exists(compilation_db)){
      unlink(compilation_db, recursive = TRUE)
    }
    dir.create(compilation_sa)
    dir.create(compilation_db)
    compilation_archive <- NULL
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


#' Setup output paths of the compiler
#'
#' @description This function does two things: 1. create folders that will store compiled data;
#'              2. return paths that directs the compiled folder.
#'
#' @param compilationPath character, Specifies a path to store whole compilation process.
#'                              If missing, the current work directory will be used.
#' @param compilationDate character, Specifies a compilation date. It should be in format of YYYYMMDD
#'                              It will be used for archive the compilation outputs.
#' @param projectType character, either \code{PSP} or \code{nonPSP}. If it is \code{PSP}, it
#'                               is consistent with original PSP compiler, otherwise, it
#'                               is consistent with VRI compiler.
#' @param recompile logical, Defines whether we want to use existing data that downloaded
#'                              previously.
#' @param archiveDate character, Defines on which archive date the raw data were downloaded.
#'                             These raw data will be used for recompilation. Format is YYYYMMDD.
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
#' @rdname compilerPathSetup_new
#'
#' @author Yong Luo
#'
compilerPathSetup_new <- function(compilationPath = ".",
                              compilationDate,
                              projectType,
                              recompile,
                              archiveDate){
  if(!dir.exists(compilationPath)){
    dir.create(compilationPath)
  }
  allarchives <- dir(compilationPath, pattern = paste0("Archive_", projectType))
  containDB <- unlist(lapply(allarchives, function(x){dir.exists(file.path(compilationPath, x, paste0("compilation_", projectType,"_db")))}))
  emptyarchives <- allarchives[!containDB]
  allarchives <- allarchives[containDB]
  lapply(emptyarchives,
         function(x){unlink(file.path(compilationPath, x),
                            recursive = TRUE)})
  archivedates <- gsub(paste0("Archive_", projectType, "_"), "", allarchives)
  archivedates <- archivedates[nchar(archivedates) == 8]
  lastarchive <- paste0("Archive_", projectType, "_",
                        max(as.numeric(archivedates)))
  last_compilation <- file.path(compilationPath, lastarchive)

  if(recompile == FALSE){
    compilation_archive <- file.path(compilationPath,
                                     paste("Archive_",
                                           projectType, "_",
                                           compilationDate,
                                           sep = ""))

    raw_from_oracle <- file.path(compilationPath, paste0("compilation_", projectType, "_raw"))
    compilation_sa <- file.path(compilationPath, paste0("compilation_", projectType, "_sa"))
    compilation_db <- file.path(compilationPath, paste0("compilation_", projectType, "_db"))
    compilation_map <- file.path(compilationPath, "compilation_map") # this will be same for both psp and vri compiler
    compilation_coeff <- file.path(compilationPath, "compilation_coeff")  # this will be same for both psp and vri compiler
    compilation_report <- file.path(compilationPath, paste0("compilation_", projectType, "_report"))
    ## for raw, sa, db, archive and report,
    ## remove the existing ones and create empty folders

    ## for coeff and map, create empty one if they do not exist,
    ## otherwise keep them
    if(dir.exists(raw_from_oracle)){
      unlink(raw_from_oracle, recursive = TRUE)
    }
    if(dir.exists(compilation_sa)){
      unlink(compilation_sa, recursive = TRUE)
    }
    if(dir.exists(compilation_db)){
      unlink(compilation_db, recursive = TRUE)
    }
    if(!dir.exists(raw_from_oracle)){
      dir.create(raw_from_oracle)
    }
    dir.create(compilation_sa)
    dir.create(compilation_db)
    if(!dir.exists(compilation_report)){
      dir.create(compilation_report)
    }
    if(!dir.exists(compilation_coeff)){
      dir.create(compilation_coeff)
    }
    if(!dir.exists(compilation_map)){
      dir.create(compilation_map)
    }
  } else {
    # create a folder under current archivement
    recompile_folder <- file.path(compilationPath,
                                  paste0("Archive_", projectType, "_", archiveDate, "_recomp", compilationDate))
    last_compilation <- recompile_folder
    dir.create(recompile_folder)
    # copy raw, map, coeff, and report into subfolder
    archivefolder <- file.path(compilationPath, paste0("Archive_", projectType, "_", archiveDate))
    file.copy(from = file.path(archivefolder, paste0("compilation_", projectType, "_raw")),
              to = recompile_folder,
              recursive = TRUE)
    file.copy(from = file.path(archivefolder, "compilation_map"),
              to = recompile_folder,
              recursive = TRUE)
    file.copy(from = file.path(archivefolder, "compilation_coeff"),
              to = recompile_folder,
              recursive = TRUE)
    file.copy(from = file.path(archivefolder, paste0("compilation_", projectType, "_report")),
              to = recompile_folder,
              recursive = TRUE)
    raw_from_oracle <- file.path(recompile_folder, paste0("compilation_", projectType, "_raw"))
    compilation_sa <- file.path(recompile_folder, paste0("compilation_", projectType, "_sa"))
    compilation_db <- file.path(recompile_folder, paste0("compilation_", projectType, "_db"))
    compilation_map <- file.path(recompile_folder, "compilation_map")
    compilation_coeff <- file.path(recompile_folder, "compilation_coeff")
    compilation_report <- file.path(recompile_folder, paste0("compilation_", projectType, "_report"))
    if(dir.exists(compilation_sa)){
      unlink(compilation_sa, recursive = TRUE)
    }
    if(dir.exists(compilation_db)){
      unlink(compilation_db, recursive = TRUE)
    }
    dir.create(compilation_sa)
    dir.create(compilation_db)
    compilation_archive <- NULL
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

