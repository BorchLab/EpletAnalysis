calculate_sum <- function(df, 
                          alleles, 
                          sum_column_name, 
                          return.sum = TRUE, 
                          return.dose = FALSE) {
  #Ensure no * in alleles - will ruin regex
  df$Alleles <- gsub("[*]", "_", df$Alleles)
  
  # Remove alleles with N suffix
  alleles <- alleles[!grepl("N$", alleles)]
  
  # Escape the input allele list for regex safety
  escaped_alleles <- stringr::str_replace_all(alleles, "([\\^$.|?*+(){}\\[\\]])", "\\\\\\1")
  
  if (return.sum) {
    df[[sum_column_name]] <- sapply(df$Alleles, function(allele_string) {
      # Parse and escape the alleles in this row
      alleles_in_row <- unlist(strsplit(allele_string, ",\\s*"))
      escaped_row_alleles <- stringr::str_replace_all(alleles_in_row, "([\\^$.|?*+(){}\\[\\]])", "\\\\\\1")
      
      if (return.dose) {
        # Deduplicate DRB3/4/5 only once
        drb345 <- grepl("^DRB[345]", alleles)
        filtered <- c(unique(alleles[drb345]), alleles[!drb345])
        return(sum(alleles_in_row %in% filtered))
      } else {
        # Match escaped row alleles to escaped input alleles
        pattern <- paste0("(", paste(escaped_row_alleles, collapse = "|"), ")")
        return(sum(stringr::str_detect(escaped_alleles, pattern)))
      }
    })
    return(df)
    
  } else {
    tmp <- df
    for (allele in alleles) {
      escaped <- stringr::str_replace_all(allele, "([\\^$.|?*+(){}\\[\\]])", "\\\\\\1")
      pattern <- paste0("\\b", escaped, "\\b")
      tmp[[allele]] <- as.integer(stringr::str_detect(df$Alleles, pattern))
    }
    return(tmp)
  }
}
