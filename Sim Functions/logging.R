log_output <- function(log_flag, message, log_file = NULL) {
  if (log_flag) {
    if (is.null(log_file)) {
      warning("No log file specified!")
    } else {
      writeLines(sprintf("[%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), message), log_file)
    }
  }
  
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), message))
  
}
