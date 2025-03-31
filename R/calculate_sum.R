#' @title Calculate Sum of Eplet Representation
#' @description Adds columns to the data frame based on allele matches.
#' @param df Eplet data frame with alleles.
#' @param alleles Vector of alleles to match.
#' @param sum_column_name Name of the new column to store the eplet sum.
#' @param return.sum Return the eplet data frame with a new column with the sum (TRUE)
#' or a data frame with a set of new columns binarized by allele.
#' @param return.dose Count unique values (FALSE) or total values/dose of the assay (TRUE)
#' @return Updated data frame with new columns.
calculate_sum <- function(df, 
                          alleles, 
                          sum_column_name, 
                          return.sum = TRUE, 
                          return.dose = FALSE) {
  
  alleles <- alleles[!grepl("N$", alleles)]
  if(return.sum) {
  df[[sum_column_name]] <- sapply(df$Alleles, function(allele_string) {
      # Split the allele string by commas
      alleles_in_row <- unlist(strsplit(allele_string, ","))
      # Count the matches with `grep` for the unique alleles
      if(return.dose) {
        #Discussion was to remove DRB3/4/5 duplicated alleles from dosage
        drb345 <- grepl("^DRB[345]", alleles)
        filtered_alleles <- c(unique(alleles[drb345]), alleles[!drb345])
        length(grep(paste0(alleles_in_row, collapse = "|"), filtered_alleles))
      } else {
        length(grep(paste0("^(", paste0(alleles_in_row, collapse = "|"), ")$"), alleles))
      }
    })
    return(df)
  } else {
    tmp <- df
    for (allele in alleles) {
      match_column <- grepl(paste0("\\b", allele, "\\b"), df$Alleles)
      if (any(match_column)) {
        tmp[[allele]] <- as.integer(match_column)
      }
    }
  return(tmp)
  } 
}
