# R Script used to convert raw data to tenure data

# You can set a working directory or just open this file 
# where there is a subfolder called Raw Data with your spreadsheets
# and a subfolder called Clean Data
# Example: setwd("/Users/jessicalogin/Library/CloudStorage/OneDrive-IndianaUniversity/Indiana/ISCC/Kathleen ISCC/Deliverables/ISCC_Kathleen_1")

# Load necessary packages
library(readxl)
library(tidyverse)

#################### 
##### Function 1 (helper, used in Function 3)
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
##### Function 2 (helper to merge overlapping/adjacent spells)

merge_spells <- function(data, group_cols, max_gap = 1) {
  
  # Separate rows with complete dates from rows with missing dates
  complete_spells <- data %>%
    filter(!is.na(start), !is.na(end)) %>%
    arrange(across(all_of(group_cols)), start, end) %>%
    group_by(across(all_of(group_cols))) %>%
    mutate(
      # Furthest end date reached before the current row
      previous_max_end = lag(cummax(end)),
      
      # Start a new spell if gap is greater than max_gap
      new_spell = is.na(previous_max_end) |
        start > previous_max_end + max_gap,
      
      spell_id = cumsum(new_spell)
    ) %>%
    group_by(across(all_of(group_cols)), spell_id) %>%
    summarise(
      start = min(start),
      end = max(end),
      .groups = "drop"
    ) %>%
    select(-spell_id)
  
  # Keep rows with missing start/end dates unchanged
  incomplete_spells <- data %>%
    filter(is.na(start) | is.na(end)) %>%
    select(all_of(group_cols), start, end)
  
  bind_rows(complete_spells, incomplete_spells)
}


#################### 
##### Function 3 (load, clean, and make 1 org per row function) ## each row is a person spending consecutive time in an organization (can be multiple positions)
create_tenure_data <- function(path, 
                             year_cols=c("Start_year", "End_year", 
                                         "Before_start", "Before_end", 
                                         "After_start", "After_end"),
                             select_cols = c("Last_name", "First_name", "Patronimic", "Organization", 
                                             "Start_year", "End_year",
                                             "Before_org", "Before_start", "Before_end", 
                                             "After_org", "After_start", "After_end",
                                             "Org_type", "BeforeOrg_type", "AfterOrg_type"),
                             current_org = "Organization",
                             sheet = NULL,
                             max_gap = 1) { #default is one year difference end to start
  # Load in dataframe
  df <- read_excel(path, na = c(""), sheet = sheet)
  
  # Replace "NA" with 2024
  df$End_year[df$End_year == "NA"] <- 2024
  df$Before_end[df$Before_end == "NA"] <- 2024
  df$After_end[df$After_end == "NA"] <- 2024
  
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
  
  # Make each row just one position (not three)
  positions_long <- bind_rows(
    
    # Creates rows for each current organization
    df_clean %>%
      transmute(
        Last_name,
        First_name,
        Patronimic,
        org = .data[[current_org]],
        type = Org_type,
        start = Start_year,
        end   = End_year),
    
    # Adds rows for each before organization
    df_clean %>%
      transmute(
        Last_name,
        First_name,
        Patronimic,
        org = Before_org,
        type = BeforeOrg_type,
        start = Before_start,
        end   = Before_end),
    
    # Adds rows for each after organization
    df_clean %>%
      transmute(
        Last_name,
        First_name,
        Patronimic,
        org = After_org,
        type = AfterOrg_type,
        start = After_start,
        end   = After_end)) %>%
    
    # Removes organizations that are missing (usually no/before org)
    filter(!is.na(org))
  
  # Use normalization helper function to clean up
  # Clean up/make sure same org is recognized as such (using helper function above)
  positions_long <- positions_long %>%
    mutate(org = normalize_org(org, keep_type_for = c("Moscow", "Altai")))
  
  # Remove rows that are identical in case the same position was recorded multiple times
  positions_long <- positions_long %>%
    distinct(Last_name, First_name, Patronimic,
             org, start, end,
             .keep_all = TRUE)
  
  # Merge overlapping or adjacent spells
  # max_gap = 1 means 2010-2014 + 2015-2018 becomes 2010-2018
  positions_long <- merge_spells(
    positions_long,
    group_cols = c("Last_name", "First_name", "Patronimic", "org", "type"),
    max_gap = max_gap)

  # Output
  return(positions_long)
}

#################### 
##### Perform (use the function above on your data)
assembly <- create_tenure_data("Raw Data/Federal Assembly, 2000-2024.xlsx")
assembly <- assembly %>% 
  mutate(tenure = end - start)

govt <- create_tenure_data("Raw Data/Federal Government, 2000-2024.xlsx")
govt <- govt %>% 
  mutate(tenure = end - start)

pres <- create_tenure_data("Raw Data/Presidential Administration, 2000-2024.xlsx")
pres <- pres %>% 
  mutate(tenure = end - start)

region_russ <- create_tenure_data("Raw Data/Regional Heads, 1991-2024.xlsx", 
                                sheet = "Russian Regions",
                                select_cols = c("Last_name", "First_name", "Patronimic", "Region", 
                                                "Start_year", "End_year",
                                                "Before_org", "Before_start", "Before_end", 
                                                "After_org", "After_start", "After_end",
                                                "Org_type", "BeforeOrg_type", "AfterOrg_type"),
                                current_org = "Region")
region_russ <- region_russ %>% 
  mutate(tenure = end - start)

region_ukr <- create_tenure_data("Raw Data/Regional Heads, 1991-2024.xlsx", 
                               sheet = "Occupied Ukraine",
                               select_cols = c("Last_name", "First_name", "Patronimic", "Region", 
                                               "Start_year", "End_year",
                                               "Before_org", "Before_start", "Before_end", 
                                               "After_org", "After_start", "After_end",
                                               "Org_type", "BeforeOrg_type", "AfterOrg_type"),
                               current_org = "Region")
region_ukr <- region_ukr %>% 
  mutate(tenure = end - start)

# Combine all of the new data frames
combined_tenure <- bind_rows(assembly, govt, pres, region_russ, region_ukr)

##### Save data
saveRDS(combined_tenure, file = "Clean Data/tenure.rds")


#################### #################### #################### #################### 
##### Tenure by organization type
create_tenure_data_type <- function(path, 
                               year_cols=c("Start_year", "End_year", 
                                           "Before_start", "Before_end", 
                                           "After_start", "After_end"),
                               select_cols = c("Last_name", "First_name", "Patronimic", "Organization", 
                                               "Start_year", "End_year",
                                               "Before_start", "Before_end", 
                                               "After_start", "After_end",
                                               "Org_type", "BeforeOrg_type", "AfterOrg_type"),
                               sheet = NULL,
                               max_gap = 1) { #default is one year difference end to start
  # Load in dataframe
  df_type <- read_excel(path, na = c(""), sheet = sheet)
  
  # Replace "NA" with 2024
  df_type$End_year[df_type$End_year == "NA"] <- 2024
  df_type$Before_end[df_type$Before_end == "NA"] <- 2024
  df_type$After_end[df_type$After_end == "NA"] <- 2024
  
  # Select only relevant columns
  df_type <- df_type %>%
    select(all_of(select_cols))
  
  # Make year columns numeric type
  df_clean_type <- df_type %>%
    mutate(across(all_of(year_cols), 
                  ~ {
                    x <- as.character(.x)
                    x <- str_trim(x)              # remove whitespace
                    parse_number(x)               # extract numeric portion; returns NA if none
                  } ))
  
  # Make each row just one position (not three)
  positions_long <- bind_rows(
    
    # Creates rows for each current organization type
    df_clean_type %>%
      transmute(
        Last_name,
        First_name,
        Patronimic,
        type = Org_type,
        start = Start_year,
        end   = End_year),
    
    # Adds rows for each before organization type
    df_clean_type %>%
      transmute(
        Last_name,
        First_name,
        Patronimic,
        type = BeforeOrg_type,
        start = Before_start,
        end   = Before_end),
    
    # Adds rows for each after organization type
    df_clean_type %>%
      transmute(
        Last_name,
        First_name,
        Patronimic,
        type = AfterOrg_type,
        start = After_start,
        end   = After_end)) %>%
    
    # Removes organizations that are missing (usually no/before org)
    filter(!is.na(type))
  
  # Remove rows that are identical in case the same position was recorded multiple times
  positions_long <- positions_long %>%
    distinct(Last_name, First_name, Patronimic,
             type, start, end,
             .keep_all = TRUE)
  
  # Merge overlapping or adjacent spells within the same organization type
  positions_long <- merge_spells(
    positions_long,
    group_cols = c("Last_name", "First_name", "Patronimic", "type"),
    max_gap = max_gap)
  
  # Remove rows where the end year of the organization is prior to 2000
  positions_long_post2000 <- positions_long %>%
    filter(is.na(end) | end >= 2000)
  
  # Output
  return(positions_long_post2000)
}

##### Perform (use the new function above on your data)
assembly_type <- create_tenure_data_type("Raw Data/Federal Assembly, 2000-2024.xlsx")
assembly_type <- assembly_type %>% 
  mutate(tenure = end - start)

govt_type <- create_tenure_data_type("Raw Data/Federal Government, 2000-2024.xlsx")
govt_type <- govt_type %>% 
  mutate(tenure = end - start)

pres_type <- create_tenure_data_type("Raw Data/Presidential Administration, 2000-2024.xlsx")
pres_type <- pres_type %>% 
  mutate(tenure = end - start)

region_russ_type <- create_tenure_data_type("Raw Data/Regional Heads, 1991-2024.xlsx", 
                                  sheet = "Russian Regions",
                                  select_cols = c("Last_name", "First_name", "Patronimic", "Region", 
                                                  "Start_year", "End_year",
                                                  "Before_org", "Before_start", "Before_end", 
                                                  "After_org", "After_start", "After_end",
                                                  "Org_type", "BeforeOrg_type", "AfterOrg_type"))
region_russ_type <- region_russ_type %>% 
  mutate(tenure = end - start)

region_ukr_type <- create_tenure_data_type("Raw Data/Regional Heads, 1991-2024.xlsx", 
                                 sheet = "Occupied Ukraine",
                                 select_cols = c("Last_name", "First_name", "Patronimic", "Region", 
                                                 "Start_year", "End_year",
                                                 "Before_org", "Before_start", "Before_end", 
                                                 "After_org", "After_start", "After_end",
                                                 "Org_type", "BeforeOrg_type", "AfterOrg_type"))
region_ukr_type <- region_ukr_type %>% 
  mutate(tenure = end - start)

# Combine all of the new data frames
combined_tenure_type <- bind_rows(assembly_type, 
                                  govt_type, 
                                  pres_type, 
                                  region_russ_type, 
                                  region_ukr_type)
##### Save data
saveRDS(combined_tenure_type, file = "Clean Data/tenure_type.rds")

#################### #################### #################### #################### 
#### Count distinct number of elites

# Total = 1,085
combined_tenure %>%
  distinct(Last_name, First_name, Patronimic) %>%
  nrow()

# Federal Assembly = 286
assembly %>%
  distinct(Last_name, First_name, Patronimic) %>%
  nrow()

# Federal Government = 380
govt %>%
  distinct(Last_name, First_name, Patronimic) %>%
  nrow()

# Pres Admin = 145
pres %>%
  distinct(Last_name, First_name, Patronimic) %>%
  nrow()

# Russian Regional Heads = 425
region_russ %>%
  distinct(Last_name, First_name, Patronimic) %>%
  nrow()

# Ukraine Regional Heads = 10
region_ukr %>%
  distinct(Last_name, First_name, Patronimic) %>%
  nrow()

