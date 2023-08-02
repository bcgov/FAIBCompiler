


writeOutputs <- function(fileTable,
                         compilationType,
                         compilationPaths){
  for (indirow in 1:nrow(fileTable)) {
    thedata <- readRDS(file.path(fileTable$folderName[indirow],
                                 paste0(fileTable$fileName[indirow], ".rds")))
    write.csv(thedata,
              file.path(fileTable$folderName[indirow],
                        paste0(fileTable$fileName[indirow], ".csv")),
              na = "",
              row.names = FALSE)
    # if(fileTable$folderName[indirow] == compilationPaths$compilation_db &
    #    compilationType == "nonPSP"){
    #   write.xlsx(thedata,
    #              file.path(fileTable$folderName[indirow],
    #                        paste0(fileTable$fileName[indirow], ".xlsx")),
    #              overwrite = TRUE)
    # } else if (fileTable$folderName[indirow] == compilationPaths$compilation_db &
    #            compilationType == "PSP" &
    #            !(fileTable$fileName[indirow] %in% c("treelist", "compiled_vi_c"))){
    #   write.xlsx(thedata,
    #              file.path(fileTable$folderName[indirow],
    #                        paste0(fileTable$fileName[indirow], ".xlsx")),
    #              overwrite = TRUE)
    # }
    rm(thedata)
    gc()
  }
}


