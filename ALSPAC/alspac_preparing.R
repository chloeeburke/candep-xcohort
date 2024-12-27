'''
---
title: "Cross-cohort analysis cannabis depression"
author: Chloe Burke
dataset: ALSPAC
date: "October 2024"
script: "Creating working dataset"
---
'''

# LOAD PACKAGES
##install.packages("haven")
library(haven)
##install.packages("dplyr")
library(dplyr)

# Note: Replace [file_path] with your local directory containing the ALSPAC data files

## ================== Read in data =============================================

# Reading in dataset provided by ALSPAC
setwd("file_path]")
alspac_raw <- read_dta(
  '[file_name]',
  skip = 0, 
  n_max = Inf,
  .name_repair = "unique"
)

## ================== Restricting to took part at CCS ==========================

## Restricting sample to those who took part at CCS (baseline) questionnaire

# Checking coding and sample counts
alspac_raw$ccs0007
table(alspac_raw$ccs0007) 

# Generating indicator variable
alspac_raw <- alspac_raw %>%
  mutate(ccs_y = case_when(
    ccs0007 == 1 ~ 1,
    TRUE ~0))
table(alspac_raw$ccs_y)

# Filtering sample
alspac_ccs <- alspac_raw %>%
  filter(ccs_y ==1) 

## ================== Restricting to took part at T4 ==========================

## Restricting sample to those who took part at T4 (outcome) clinic

# Checking coding and sample counts
alspac_ccs$FJ001c
table(alspac_ccs$FJ001c) 

# Generating indicator variable
alspac_ccs <- alspac_ccs %>%
  mutate(t4_y = case_when(
    FJ001c == 1 ~ 1,
    TRUE ~0))
table(alspac_ccs$t4_y)

# Filtering sample
alspac_t4 <- alspac_ccs %>%
  filter(t4_y ==1) 

## ================== Checking withdrawn consent ===============================

# Function to check for withdrawn consent values
check_withdrawn_consent <- function(data, withdrawn_code = -9999) {
  # Find rows where any column contains the withdrawn code
  withdrawn_rows <- apply(data, 1, function(row) any(row == withdrawn_code, na.rm = TRUE))
  
  # Get the row numbers with withdrawn consent
  withdrawn_indices <- which(withdrawn_rows)
  
  # If any withdrawn cases found, create detailed report
  if(length(withdrawn_indices) > 0) {
    # Find which variables contain withdrawn codes for each case
    withdrawn_vars <- lapply(withdrawn_indices, function(i) {
      names(data)[data[i,] == withdrawn_code]
    })
    names(withdrawn_vars) <- paste("Row", withdrawn_indices)
    
    # Create summary
    result <- list(
      found_withdrawn = TRUE,
      n_withdrawn_cases = length(withdrawn_indices),
      row_numbers = withdrawn_indices,
      variables_by_case = withdrawn_vars
    )
  } else {
    result <- list(
      found_withdrawn = FALSE,
      n_withdrawn_cases = 0
    )
  }
}  


result1 <- check_withdrawn_consent(alspac_t4)
print(result1) #found_withdrawn = FALSE, n_withdrawn_cases = 0


## ================== End of script ============================================

rm(alspac_ccs, alspac_raw, result1, check_withdrawn_consent)

#-----------------#

# Open script called 'alspac_cleaning.R' 
