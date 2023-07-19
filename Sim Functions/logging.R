log_output <- function(log_level, message, log_file = NULL, simulation_log_level=3) {
  if (log_level > simulation_log_level) {
    if (is.null(log_file)) {
      warning("No log file specified!")
    } else {
      writeLines(sprintf("[%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), message), log_file)
    }
    cat(sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), message))
  }
  
}

# Function to add results to simulation list
add_results_to_simulation_list <- function(filename, description, n_i, n_t) {
  # Open the text file in "append" mode
  file_conn <- file("Runs/simulation_list.txt", "a")
  
  # Create the entry string with filename and description
  entry <- sprintf("%s [n_i=%s, n_t=%s results=\"%s\"]", description, n_i,n_t, filename)
  
  # Write the entry to the text file
  cat(entry, file = file_conn, fill = TRUE, append = TRUE)
  
  # Close the text file
  close(file_conn)
}
