summarise_eplet <- function(df) {
  
  # Generate a summary of eplets by group
  dff <- df %>%
    transmute(
      Epitope = Epitope.Name,
      SAB = ifelse(SAB_unique != 0, 1, 0),
      PRA = ifelse(PRA_unique != 0, 1, 0),
      Mix = ifelse(Mix_unique != 0, 1, 0),
      Explex = ifelse(EXPLEX_unique != 0, 1, 0)
    ) %>%
    unique()
  
  # Create the intersection combination string
  dff$intersections <- apply(dff[, c("SAB", "PRA", "Mix", "Explex")], 1, function(row) {
    cols_with_1 <- names(row)[row == 1]
    if (length(cols_with_1) == 0) {
      return("NO_SPI")
    } else {
      return(paste(sort(cols_with_1), collapse = "_"))
    }
  })
  
  # Some duplicated epitopes, assigning priority
  category_priority <- c(
    "Explex_Mix_PRA_SAB" = 1,
    "Mix_PRA_SAB" = 2,
    "Explex_Mix_SAB" = 3,
    "PRA_SAB" = 4,
    "Mix_PRA" = 5,
    "SAB" = 6
  )
  
  dff <- dff %>%
    mutate(priority = category_priority[intersections]) %>%
    group_by(Epitope) %>%
    slice_min(order_by = priority, n = 1) %>%
    ungroup() %>%
    select(-priority)
  
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