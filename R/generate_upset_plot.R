library(ComplexUpset) 
generate_upset_plot <- function(df, filename, pattern) {
  # Dynamically create binary columns based on the provided pattern
  df <- df %>%
    mutate(
      SAB = ifelse(get(paste0("SAB", pattern)) != 0, 1, 0),
      PRA = ifelse(get(paste0("PRA", pattern)) != 0, 1, 0),
      Mix = ifelse(get(paste0("Mix", pattern)) != 0, 1, 0),
      ExPlex = ifelse(get(paste0("EXPLEX", pattern)) != 0, 1, 0)
    )
  
  # Create a binary matrix for the sets
  upset_data <- df %>%
    select(SAB, PRA, Mix, ExPlex)
  
  # Generate the UpSet plot
  upset_plot <- upset(
    data = upset_data,
    intersect = c("SAB", "PRA", "Mix", "ExPlex"),
    base_annotations = list(
      'Intersection Size' = intersection_size()
    ),
    set_sizes=FALSE,
    min_size = 1,
    themes = upset_default_themes(text = element_text(size = 12))
  )
  
  # Save the plot to a file
  ggsave(filename, plot = upset_plot, width = 8, height = 6)
}
