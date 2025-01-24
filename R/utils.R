#' @title Combine Sheets into a Single Data Frame
#' @description Merges multiple sheets from the same Excel file.
#' @param file Path to the Excel file.
#' @param sheets Vector of sheet names.
#' @return A combined data frame.
combine_sheets <- function(file, sheets) {
  do.call(rbind, lapply(sheets, function(sheet) read_excel(file, sheet = sheet)))
}

replace_with_colnames_and_col4 <- function(x) {
  for (i in 1:nrow(x)) {
    for (j in 2:(ncol(x) )) {
      if (x[i, j] != 0) {
        x[i, j] <- paste0(colnames(x)[j],",", x[i, 1])
      }
    }
  }
  return(x)
}
