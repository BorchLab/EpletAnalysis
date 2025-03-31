process_pra_data <- function(pra_file,
                             bead_counts_file,
                             eplet = c("82LR", "80N"),
                             plot_title = "Dose-based Correlation",
                             output_main_plot = TRUE,
                             facet_labels = NULL) {
  
  library(tidyverse)
  library(readxl)
  library(hrbrthemes)
  
  eplet <- match.arg(eplet)
  
  # Load and clean PRA data
  clean_pra <- function(df, is_second_file = FALSE, is_bw6 = FALSE) {
    if (is_bw6) {
      df$BeadID <- str_pad(as.numeric(df$BeadID), width = 2, side = "left", pad = "0")
    } else if (is_second_file) {
      df$BeadID <- str_pad(df$BeadID, width = 2, side = "left", pad = "0")
    } else {
      df$BeadID <- gsub('^.', '', df$BeadID)
    }
    return(df)
  }
  
  is_bw6 <- (eplet == "80N")
  df <- clean_pra(pra_file, is_second_file = FALSE, is_bw6 = is_bw6)
  
  # Separate accession from SampleIDName
  df <- df %>%
    separate(SampleIDName, into = c('accession', 'Name'), sep = "^\\S*\\K\\s+")
  
  # Remove non-Class I beads
  classI_beads_to_remove <- paste0("Bead", c(37:68, 78, 90:94, 96, 98))
  dfI <- df %>% filter(!BeadID %in% classI_beads_to_remove)
  
  # Read bead-level allele counts and filter for eplet
  mydf <- read_excel(bead_counts_file)
  mydf1 <- mydf %>% filter(`Epitope Name` == eplet)
  mydff <- as.data.frame(t(mydf1)) %>%
    rownames_to_column("BeadID") %>%
    slice(-c(1:7)) %>%
    rename(Alleles = 2)
  mydff$BeadID <- gsub("^.{0,4}", "", mydff$BeadID)
  mydff <- mydff %>%
    transmute(BeadID = str_pad(as.numeric(BeadID), 2, pad = "0"),
              Alleles = Alleles) %>%
    arrange(BeadID)
  
  # Join PRA + allele count
  df_combined <- left_join(dfI, mydff, by = "BeadID") %>% drop_na(Alleles)
  df_combined$NormalValue <- as.numeric(df_combined$NormalValue)
  
  # Summary metrics
  df_median <- df_combined %>%
    group_by(accession, Alleles) %>%
    summarise(median_values = round(median(NormalValue)), .groups = "drop")
  
  df_range <- df_combined %>%
    group_by(accession, Alleles) %>%
    summarise(Value = round(range(NormalValue)), .groups = "drop")
  
  df_cor <- df_combined %>%
    group_by(accession) %>%
    summarise(
      Correlation = cor(as.numeric(Alleles), NormalValue, method = 'spearman'),
      p_value = cor.test(as.numeric(Alleles), NormalValue, method = "spearman")$p.value,
      .groups = "drop"
    )
  
  # Plot for one accession (if specified)
  if (output_main_plot) {
    dfp <- df_combined %>% filter(accession == accession_for_main)
    dfp$Alleles <- as.integer(as.factor(dfp$Alleles))
    

    plot <- ggplot(dfp, aes(x = Alleles, y = NormalValue)) +
              geom_point() +
              geom_smooth(method = lm, color = "red", fill = "#69b3a2", se = FALSE) +
              scale_x_continuous(labels = c('0', '1', '2', '3')) +
              theme_ipsum() +
              theme(legend.position = 'none',
                    axis.text = element_text(size = 24),
                    axis.title = element_text(size = 24)) +
              labs(title = plot_title, y = "MFI", x = "Eplet dose per bead")
  } else {
    df_suppl <- df_combined %>% filter(accession != "22-16465") # optional exclusion
    df_suppl$Alleles <- as.integer(as.factor(df_suppl$Alleles))
  
    plot <- ggplot(df_suppl, aes(x = Alleles, y = NormalValue)) +
                geom_point() +
                geom_smooth(aes(group = accession), method = lm, color = "red", fill = "#69b3a2", se = FALSE) +
                theme_ipsum() +
                theme(legend.position = 'none') +
                facet_wrap(~ accession,
                           labeller = labeller(accession = facet_labels),
                           scales = 'free_y') +
                labs(title = plot_title, x = "Eplet dose per bead", y = "MFI") +
                theme_minimal() +
                theme(text = element_text(size = 20))
  }
  
  return(plot)
}
