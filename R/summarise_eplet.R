summarise_eplet <- function(df) {
  
  # Generate a summary of eplets by group
  dff <- df %>%
    transmute(
      Epitope = `Epitope Name`,
      SAB = ifelse(SAB_unique != 0, 1, 0),
      PRA = ifelse(PRA_unique != 0, 1, 0),
      Mix = ifelse(Mix_unique != 0, 1, 0),
      Explex = ifelse(EXPLEX_unique != 0, 1, 0)
    )
  
  # Create the intersection combination string
  dff$intersections <- apply(dff[, c("SAB", "PRA", "Mix", "Explex")], 1, function(row) {
    cols_with_1 <- names(row)[row == 1]
    if (length(cols_with_1) == 0) {
      return("NO_SPI")
    } else {
      return(paste(sort(cols_with_1), collapse = "_"))
    }
  })
  
  # Summarize
  final_results_df <- dff %>%
    group_by(intersections) %>%
    summarise(
      Eplet_Count = n(),
      Eplets = paste(sort(Epitope), collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(desc(Eplet_Count))
  return(final_results_df)
}