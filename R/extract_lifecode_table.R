extract_lifecode_tables <- function(pdf_paths,
                                    pages     = 1:2,
                                    row_regex = "^\\s*\\d+\\s+\\d+\\b")
{
  #— dependencies -------------------------------------------------------------
  pkgs <- c("pdftools", "stringr", "dplyr", "tibble", "purrr")
  invisible(lapply(pkgs, requireNamespace, quietly = TRUE))
  
  extract_one <- function(pdf_path, pages = 1:2) {
    
    # 1 read the relevant pages -------------------------------------------------
    text  <- pdftools::pdf_text(pdf_path)[pages]
    lines <- stringr::str_split(text, "\n", simplify = TRUE) |> as.vector()
    
    # 2 normalise ALL kinds of white-space (NBSP, thin-space, tabs …) ----------
    lines <- stringr::str_squish(lines)                  # ICU ‘WHITE_SPACE’
    
    # 3 keep only “row  bead …” lines (works for LSA-1 and LSA-2) --------------
    row_pat <- "^\\d+ \\d+ "                             # two integers at start
    rows    <- stringr::str_subset(lines, row_pat)
    
    # 4 split, rectify, label …  (unchanged) -----------------------------------
    splits  <- stringr::str_split(rows, " ", simplify = FALSE)
    maxlen  <- max(lengths(splits))
    mat     <- vapply(splits, \(x) c(x, rep(NA, maxlen - length(x))),
                      character(maxlen))
    df      <- tibble::as_tibble(t(mat), .name_repair = "minimal")
    
    names(df) <- if (ncol(df) >= 8)      # LSA-1 (Class I)→ eight columns
      c("Row","Bead","Antigen","AlleleChange",
        "Serology","MFICL","MFI","Log10LRA")
    else                    # LSA-2 (Class II) → seven columns
      c("Row","Bead","Antigen", "Serology", "MFICL","MFI","Log10LRA")
    
    num_cols <- c("Row","Bead","MFICL","MFI","Log10LRA")
    dplyr::mutate(df, dplyr::across(all_of(num_cols), readr::parse_number))
  }
  
  # iterate over the vector and bind -----------------------------------------
  out <- purrr::map_dfr(pdf_paths, extract_one,
                        .id = if (length(pdf_paths) > 1) "source_pdf" else NULL)
  
  if(ncol(out) == 7) { #LSA-2 (Class II) → seven columns
    out$Antigen <- gsub("/", ", ", out$Antigen) #comma separate
  }
  
  # renaming the columns to match OneLambda format
  out <- out %>%
    dplyr::rename(Order = Row,
                  `Bead ID` = Bead,
                  `Molecular Typing` = Antigen,
                  `Serological Typing` = Serology)
  

  out
}

pdfs <- c("~/Documents/GitHub/EpletAnalysis/data/LSA1_RS_EN_3015349_EXP_2025-10-17.pdf", 
          "~/Documents/GitHub/EpletAnalysis/data/LSA2_RS_EN_3015327_EXP_2025-10-10.pdf")
names <- c("Life_Code_SA1", "Life_Code_SA2")

for(i in seq_along(pdfs)) {
  tmp <- extract_lifecode_tables(pdfs[i])
  write.csv(tmp, paste0("~/Documents/GitHub/EpletAnalysis/data/", names[i], ".csv"), row.names = FALSE)
}
