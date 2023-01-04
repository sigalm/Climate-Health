### Import probability and risk data ###


multisheet2array <- function(path, range, x_names, y_names) {
  
  require(readxl)
  
  sheets <- excel_sheets(path)  
  tibble <- lapply(sheets, function (x) as.data.frame(read_excel(path, sheet=x, range=range)))
  output <- array(data = unlist(tibble), dim=c(length(x_names), length(y_names), length(sheets)), dimnames=list(paste(x_names), paste(y_names), paste(sheets)))
  
  return(output)
  
}






