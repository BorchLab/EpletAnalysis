#' @title Process SAB Data
#' @description Loads and cleans SAB data from two sheets.
process_sab_data <- function(file, sheet) {
  sab <- combine_sheets(file, sheet)
  sab <- sab %>%
    filter(`Serological Typing` != 'N/A') %>%
    mutate(Molecular.Typing = gsub("\\s+", "", `Molecular Typing`)) %>%
    mutate(Molecular.Typing = str_replace_all(Molecular.Typing,'[*]','_'))
  sab[sab == "NA"] <- NA
  sab <- sab[!is.na(sab$Molecular.Typing),]
  
  sab <- sab %>%
    mutate(Multiple_Types = str_detect(Molecular.Typing, ",")) %>% # Identify rows with multiple molecular types
    separate_rows(Molecular.Typing, sep = ",") %>% # Separate rows by commas in Molecular.Typing
    select(-Multiple_Types) %>%
    as.data.frame()
  
  return(sab)
}

#' @title Process PRA Data
#' @description Loads and preprocesses PRA data.
process_pra_data <- function(file, sheet, bead = FALSE) {
  pra <- read_and_clean_excel(file, sheet)
  
  if(any(grepl("DRB1", pra[,4]))) {
    pra <- pra%>%
      select(2:13) %>%
      slice(-1, -2) %>%
      mutate(`Bead ID` = paste0("Bead ", `Bead ID`)) %>%
      as.data.frame()
  } else {
    pra <- pra%>%
            select(2:9) %>%
            slice(-1, -2) %>%
            mutate(`Bead ID` = paste0("Bead ", `Bead ID`)) %>%
            as.data.frame()
  }
  
  for (i in 1:ncol(pra)){
    pra[,i]=ifelse(pra[,i]=="X",pra[,(i-1)],pra[,i])
  }
  
  #Removing other alleles
  #TODO Think this through a little more as this only takes the first allele
  pra[]=lapply(pra,gsub,pattern="\\/.*$",replacement="") #remove everything after \
  if(!bead) {
    pra=pra%>%select(,-c(,1:2)) #remove extra column and keep only alleles
    
    pra=data.frame(stack(pra)) #stack all alleles
  } else {
    pra=pra%>%select(,-c(2)) #remove extra column
    
    # Pre-processing - beads
    pra=t(pra) #transpose
    pra=pra%>%row_to_names(row_number = 1) #move up first row to column names
    pra=as.data.frame(pra)
    pra=stack(pra)
  }
  pra[pra == "NA"] <- NA
  pra <- pra[!is.na(pra$values),]
  return(pra)
}

#' @title Process Mix Data
#' @description Loads and preprocesses mix data.
process_mix_data <- function(file, sheet) {
  mix <- read_and_clean_excel(file, sheet) %>%
    group_by(Bead) %>%
    summarise(allele_spec = paste0(Specificity, collapse = ","), .groups = "drop") %>%
    distinct(allele_spec)
  mix <- mix[-1,]
  mix=data.frame(str_split(mix$allele_spec,",",simplify = TRUE)) #separate columns
  mix[]=lapply(mix,gsub,pattern="\\/.*$",replacement="")
  
  # Mix homozygosity (mix)
  for (i in 1:ncol(mix)){
    mix[,i]=ifelse(mix[,i]=="-",mix[,(i-1)],mix[,i])
  }
  
  mix=stack(mix)
  mix$values=gsub("[[:space:]]","",mix$values) #remove spaces
  mix <- mix[mix$values != "",]
  mix[mix == "NA"] <- NA
  return(mix)
}

#' @title Process ExPlex Data
#' @description Loads and cleans ExPlex data.
process_explex_data <- function(file, sheet) {
  explex <- read_and_clean_excel(file, sheet)
  explex[explex == "NA"] <- NA
  
  return(explex)
}
  
#' @title Process HLA Registry Data
#' @description Loads and cleans HLA registry data.
process_hla_registry <- function(file, sheet) {
    df <- read_and_clean_excel(file, sheet)
    df <- df %>%
      rowwise() %>%
      mutate(Alleles = paste(unique(na.omit(c_across(10:ncol(df)))), collapse = ",")) %>%
      ungroup() %>%
      select(`AA Position`, `Epitope Name`, Alleles, `Antibody Reactivity`, 
             Class, Exposed, `Sero Group`)
    return(df)
}

#' @title Read and Preprocess Excel Data
#' @description Load data from an Excel file and clean HLA alleles.
#' @param filepath Path to the Excel file.
#' @param sheet Sheet name to load.
#' @return A cleaned data frame.
read_and_clean_excel <- function(filepath, sheet) {
  df <- suppressMessages(read_excel(filepath, sheet = sheet))
  df <- df %>%
    mutate(across(everything(), ~ gsub("\\*", "_", .))) %>% # Replace '*' with '_'
    mutate(across(everything(), ~ gsub("\\s+", "", .))) # Remove spaces
  return(df)
}
