#' @title Generate Venn Diagram
#' @description Creates a Venn diagram of positive findings.
#' @param df Data frame with findings.
#' @param filename Output file name for the diagram.
#' @param pattern Column pattern to extract
generate_venn_diagram <- function(df, filename, pattern) {
  # Dynamically create positive_sets based on the provided pattern
  positive_sets <- list(
    SAB = df %>% filter(get(paste0("SAB", pattern)) != 0) %>% pull(`Epitope Name`),
    PRA = df %>% filter(get(paste0("PRA", pattern)) != 0) %>% pull(`Epitope Name`),
    Mix = df %>% filter(get(paste0("Mix", pattern)) != 0) %>% pull(`Epitope Name`),
    ExPlex = df %>% filter(get(paste0("EXPLEX", pattern)) != 0) %>% pull(`Epitope Name`)
  )
  
  # Generate the Venn diagram
  venn.diagram(
    x = positive_sets,
    category.names = names(positive_sets),
    filename = filename,
    imagetype = "png", height = 3000, width = 3000, resolution = 1200,
    col = c("#440154ff", "#21908dff", "#fde725ff", "#404688ff"),
    fill = sapply(c("#440154ff", "#21908dff", "#fde725ff", "#404688ff"), alpha, 0.3),
    cex = 1, cat.cex = 1, lwd = 1, fontfamily = "sans"
  )
}
