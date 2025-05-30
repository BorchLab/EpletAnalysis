plot_sab_overlap <- function(
    set_LC,
    set_OL,
    prefix_len = 2) {
  
  # ── 1. Build the two sets ────────────────────────────────────────────────
  set_LC <- unique(set_LC)
  set_OL <- unique(set_OL)
  
  venn_list <- list(Lifecodes = set_LC,
                    LABScreen = set_OL)
  
  # ── 2. Venn diagram coloured *by region* ─────────────────────────────────
  p_venn <- ggVennDiagram(
    venn_list,
    label        = "count",
    label_alpha  = 0,               # opaque labels
    label_percent_digit = 0,
    edge_size    = .3) + 
    guides(fill = "none") +
    theme_void(base_size = 10) +
    theme(plot.margin = margin(5.5, 20, 5.5, 5.5))
  
  # ── 3. Prefix-level summaries ────────────────────────────────────────────
  shared   <- intersect(set_LC, set_OL)
  only_LC  <- setdiff(set_LC, set_OL)
  only_OL  <- setdiff(set_OL, set_LC)
  
  prefix_df <- tibble::tibble(
    value  = c(only_LC, only_OL, shared),
    group  = factor(c(rep("Lifecodes only", length(only_LC)),
                      rep("LABScreen only", length(only_OL)),
                      rep("Shared", length(shared))),
                    levels = c("Lifecodes only", "Shared", "LABScreen only")),
    prefix = substr(value, 1, prefix_len)
  )
  
  plot_data <- prefix_df |>
    dplyr::count(group, prefix) |>
    dplyr::mutate(prefix = forcats::fct_reorder(prefix, n, .fun = sum, .desc = TRUE))
  
  # ── 4. Stacked-bar panel ────────────────────────────────────────────────
  p_bar <- ggplot2::ggplot(plot_data, aes(group, n, fill = prefix)) +
    geom_col(width = 0.7, colour = "grey20", linewidth = .25) +
    geom_text(aes(label = n), position = position_stack(vjust = 0.5), size = 3) +
    scale_fill_viridis_d(name = "Locus") +
    labs(x = NULL, y = "Count") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "right")
  
  # ── 5. Assemble & (optionally) save ──────────────────────────────────────
  final_plot <- p_venn | p_bar 
  
  return(final_plot)
}
