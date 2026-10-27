# R Script used to convert raw data to edgelist

# You can set a working directory or just open this file 
# where there is a subfolder called Raw Data with your spreadsheets
# and a subfolder called Clean Data
# Example: setwd("/Users/jessicalogin/Library/CloudStorage/OneDrive-IndianaUniversity/Indiana/ISCC/Kathleen ISCC/Deliverables/ISCC_Kathleen_1")

# Load necessary packages
library(readxl)
library(tidyverse)

#################### 
##### Function 1 (helper, used in Function 2)
# Helper function to treat similar organization names as the same organization (e.g., Volo Oblast and Volo)
normalize_org <- function(x, keep_type_for = c("Moscow", "Altai")) {
  # Moscow and Altai are the default exceptions
  x <- as.character(x)
  is_na <- is.na(x)
  x[is_na] <- NA_character_
  
  clean1 <- function(s) {
    s %>%
      str_replace_all('[\"“”`]', '') %>%
      str_trim()
  }
  
  x[!is_na] <- vapply(
    x[!is_na],
    FUN.VALUE = character(1),
    USE.NAMES = FALSE,
    FUN = function(val) {
      v <- clean1(val)
      
      # Remove patterns "X City, Y" → keep Y
      v <- str_replace(
        v,
        regex('^.*?\\b(District|City)\\b,\\s*', ignore_case = TRUE),
        ''
      )
      
      # Remove prefix "Republic of ..."
      v_prefix_removed <- str_remove(v, regex('^Republic\\s+of(?:\\s+the)?\\s+', ignore_case = TRUE))
      
      # Remove trailing region-type words
      region_suffix_pattern <- regex('\\s+(Oblast|City|Republic|Krai)\\.?$', ignore_case = TRUE)
      base <- str_replace(v_prefix_removed, region_suffix_pattern, '')
      
      # Exception: keep region-type trailing words if base is in keep_type_for
      keep_pattern <- regex(
        paste0("\\b(", paste(keep_type_for, collapse = "|"), ")\\b"),
        ignore_case = TRUE
      )
      
      if (str_detect(base, keep_pattern)) {
        return(str_to_title(str_squish(v)))
      }
      
      return(str_to_title(str_squish(base)))
    }
  )
  
  x
}


#################### 
##### Function 2 (load, clean, and make 2 orgs per row function)
create_transfers <- function(path, 
                             year_cols=c("Start_year", "End_year", 
                                         "Before_start", "Before_end", 
                                         "After_start", "After_end"),
                             select_cols = c("Last_name", "First_name", "Organization", 
                                             "Start_year", "End_year",
                                             "Before_org", "Before_start", "Before_end", 
                                             "After_org", "After_start", "After_end"),
                             current_org = "Organization",
                             sheet = NULL) {
  # Load in dataframe
  df <- read_excel(path, na = c("", "NA", "N/A", "na"), sheet = sheet)
  
  # Select only relevant columns
  df <- df %>%
    select(all_of(select_cols))
  
  # Make year columns numeric type
  df_clean <- df %>%
    mutate(across(all_of(year_cols), 
                  ~ {
                    x <- as.character(.x)
                    x <- str_trim(x)              # remove whitespace
                    parse_number(x)               # extract numeric portion; returns NA if none
                  } ))
  
  # Make each row just two organizations/transfers (not three)
  df_transfers <- bind_rows(
    df_clean %>%
      transmute(
        row_id = row_number(),
        Last_name,
        First_name,
        From_org   = as.character(Before_org),
        To_org     = as.character(.data[[current_org]]),
        From_start = Before_start,
        From_end   = Before_end,
        To_start   = Start_year,
        To_end     = End_year),
    df_clean %>%
      transmute(
        row_id = row_number(),
        Last_name,
        First_name,
        From_org   = as.character(.data[[current_org]]),
        To_org     = as.character(After_org),
        From_start = Start_year,
        From_end   = End_year,
        To_start   = After_start,
        To_end     = After_end)) %>%
    filter(!is.na(From_org) & !is.na(To_org))
  
  # Clean up/make sure same org is recognized as such (using helper function above)
  df_transfers <- df_transfers %>%
    mutate(From_org = normalize_org(From_org, keep_type_for = c("Moscow", "Altai")),
           To_org   = normalize_org(To_org,   keep_type_for = c("Moscow", "Altai")))
  
  # Remove rows where the from/to organization are the same
  df_transfers <- df_transfers %>%
    filter(From_org != To_org)
  
  # Remove rows that are identical in case the same transfers were recorded multiple times
  df_transfers <- df_transfers %>%
    distinct(Last_name, First_name,
             From_org, To_org,
             From_start, From_end,
             To_start, To_end,
             .keep_all = TRUE)
  
  # Remove rows where the end year of the 1st organization is prior to 2000
  df_transfers_post2000 <- df_transfers %>%
    filter(pmax(From_start, From_end, To_start, To_end, na.rm = TRUE) >= 2000)
  df_transfers_post2000 <- df_transfers_post2000 %>%
    filter(
      !(coalesce(From_start, -Inf) < 2000 &
          coalesce(From_end,   -Inf) < 2000))
  
  # Add transfer year variable (used for snapshot division)
  df_transfers_post2000 <- df_transfers_post2000 %>%
    mutate(transfer_year = case_when(
      !is.na(To_start) & To_start >= 2000 ~ To_start, # default is the 2nd org start year
      TRUE ~ From_end))  # if 2nd org start year is missing or before 2000, use 1st org end year
  
  # Output
  return(df_transfers_post2000)
}

#################### 
##### Perform (use the function above on your data)
assembly <- create_transfers("Raw Data/Federal Assembly, 2000-2024.xlsx")
govt <- create_transfers("Raw Data/Federal Government, 2000-2024.xlsx")
pres <- create_transfers("Raw Data/Presidential Administration, 2000-2024.xlsx")
region_russ <- create_transfers("Raw Data/Regional Heads, 1991-2024.xlsx", 
                                sheet = "Russian Regions",
                                select_cols = c("Last_name", "First_name", "Region", 
                                                "Start_year", "End_year",
                                                "Before_org", "Before_start", "Before_end", 
                                                "After_org", "After_start", "After_end"),
                                current_org = "Region")
region_ukr <- create_transfers("Raw Data/Regional Heads, 1991-2024.xlsx", 
                               sheet = "Occupied Ukraine",
                               select_cols = c("Last_name", "First_name", "Region", 
                                               "Start_year", "End_year",
                                               "Before_org", "Before_start", "Before_end", 
                                               "After_org", "After_start", "After_end"),
                               current_org = "Region")


# Combine all of the new data frames
combined_with_ukr <- bind_rows(assembly, govt, pres, region_russ, region_ukr)

#################### 
##### Function 3 (split into snapshots, make edges people not transfers, convert to edge list)
create_edge <- function(cleandata, startyear, endyear) {
  
  # Delete if you don't want to split into snapshots:
  # Get only intended years
  transfers <- cleandata %>%
    filter(!is.na(transfer_year) & transfer_year >= startyear & transfer_year <= endyear)
  
  # Delete if you want to use transfers (not people):
  # Avoid double counting transfers back and forth between the same two organizations by one person
  undirected <- transfers %>%
    mutate(from = pmin(From_org, To_org),
           to = pmax(From_org, To_org),
           missing_dates = (is.na(From_start) + is.na(From_end) + is.na(To_start) + is.na(To_end))) %>%
    arrange(Last_name, First_name, from, to, missing_dates, From_start, To_start) %>%
    distinct(Last_name, First_name, from, to, .keep_all = TRUE) %>%
    select(-from, -to, -missing_dates)
  
  # Create edgelist
  edgelist <- undirected %>% # replace undirected with transfers if you delete the above section
    mutate(from = pmin(From_org, To_org),
           to = pmax(From_org, To_org)) %>%
    count(from, to, name = "weight", sort = TRUE)
  
  # Output
  return(edgelist)
}

#################### 
##### Perform (use function 3 on combined clean data frames to make snapshot edge lists)
edgelist_2000_2011 <- create_edge(combined_with_ukr, 2000, 2011)
edgelist_2012_2024 <- create_edge(combined_with_ukr, 2012, 2024)

#################### 
##### Save data
saveRDS(edgelist_2000_2011, file = "Clean Data/edgelist_2000_2011.rds")
saveRDS(edgelist_2012_2024, file = "Clean Data/edgelist_2012_2024.rds")


################################################################################ 
################################################################################ 
################### REFERENCE
##### For reference, to load these files in R (can use in another R script file)
edgelist_2000_2011 <- readRDS("Clean Data/edgelist_2000_2011.rds")
edgelist_2012_2024 <- readRDS("Clean Data/edgelist_2012_2024.rds")


