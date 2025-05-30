plot_eplet_overlap <- function(v1, v2,
                         n1         = "OL",
                         n2         = "LC",
                         thr        = 1) {              
  
  stopifnot(length(v1) == length(v2))
  
  ## -- 1. binarise & build master data frame ------------------------------
  bin1 <- v1 >= thr
  bin2 <- v2 >= thr
  
  dat  <- tibble(
    pos   = seq_along(v1),
    class = case_when(
      bin1 & bin2             ~ "Both",
      bin1 & !bin2            ~ n1,
      !bin1 & bin2            ~ n2,
      TRUE                    ~ "None")
  ) %>% filter(class != "None")
  
  
  ## -- 2. Venn diagram -----------------------------------------------------
  venn_obj <- list(n1 = dat$pos[dat$class %in% c(n1, "Both")],
                   n2 = dat$pos[dat$class %in% c(n2, "Both")])
  names(venn_obj) <- c(n1,n2)
  
  p_venn <- ggVennDiagram(venn_obj,
                          label        = "count",
                          label_alpha  = 0,               # opaque labels
                          label_percent_digit = 0,
                          edge_size    = .3) + 
    guides(fill = "none")
  return(p_venn)
}
