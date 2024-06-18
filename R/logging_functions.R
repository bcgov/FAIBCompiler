# logging_functions.R

# Define functions for logging
createLog <- function(logFilePath) {
  # Create a text file for logging with a properly formatted timestamp
  cat("Log created at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n", file=logFilePath, append=FALSE)
  invisible()
}

writeLog <- function(logFilePath, logMessage) {
  # Debugging: Print the parameters to confirm their values
  #print(paste("writeLog called with:", logFilePath, logMessage))

  # Ensure logFilePath is a valid file path
  stopifnot(is.character(logFilePath), length(logFilePath) == 1, nchar(logFilePath) > 0)

  # Open the log file in append mode with a safe file connection mode
  con <- file(logFilePath, open = "a", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  # Format the log message with a timestamp
  formattedLogMessage <- paste(Sys.time(), logMessage, sep = " : ")

  # Write the formatted log message to the file
  cat(formattedLogMessage, "\n", file = con)
}

closeLog <- function(logFilePath) {
  # Close the log file with a properly formatted timestamp
  cat("Log closed at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n", file=logFilePath, append=TRUE)
  invisible()
}

