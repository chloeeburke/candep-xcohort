'''
---
title: "Cross-cohort analysis cannabis depression"
author: Chloe Burke
dataset: LSAC
date: "October, 2024"
script: "Create working dataset"
---
'''

# LOAD PACKAGES
#install.packages("haven")
library(haven)
#install.packages("dplyr")
library(dplyr)

# Note: Replace [file_path] with your local directory containing the LSAC data files

## ================== Read in T0 data ====================================

# Reading in W7 (K16/17)
lsac_k16 <- read_dta("[file_path]/lsacgrk16.dta") 

# Took part in W7 (i.e., valid interview response)
lsac_k16 <- filter(lsac_k16, ichildp == 1) 


## ================== Read in T1 data ====================================

# Reading in W8 (K18/19)
mergew8 <- merge(x = lsac_k16, 
                 read_dta("[file_path]/lsacgrk18.dta",
                          col_select = -c(cohort, wave, stratum, pcodes)),
                 by = "hicid",
                 all = FALSE,
                 suffixes = c("_w7", "_w8"))  # Add wave-specific suffixes 

# Took part in W8 (i.e., valid interview response)
mergew8 <- filter(mergew8, jchildp == 1) 


## ================== Read in pre-baseline data ===========================

# Reading in W1 (K4)
mergew1 <- merge(x = mergew8, 
                 read_dta("[file_path]/lsacgrk4.dta",
                          col_select = -c(cohort, wave, stratum, pcodes)),
                 by = "hicid",
                 all.x = TRUE,
                 suffixes = c("", "_w1")) 


# Reading in W2 (K6)
mergew2 <- merge(x = mergew1, 
                     read_dta("[file_path]/lsacgrk6.dta",
                     col_select = -c(cohort, wave, stratum, pcodes)),
                     by = "hicid",
                     all.x = TRUE,
                     suffixes = c("", "_w2"))

# Reading in W3 (K8)
mergew3 <- merge(x = mergew2, 
                 read_dta("[file_path]/lsacgrk8.dta",
                          col_select = -c(cohort, wave, stratum, pcodes)),
                 by = "hicid",
                 all.x = TRUE,
                 suffixes = c("", "_w3"))

# Reading in W4 (K10)
mergew4 <- merge(x = mergew3, 
                 read_dta("[file_path]/lsacgrk10.dta",
                          col_select = -c(cohort, wave, stratum, pcodes)),
                 by = "hicid",
                 all.x = TRUE,
                 suffixes = c("", "_w4"))

# Reading in W5 (K12)
mergew5 <- merge(x = mergew4, 
                 read_dta("[file_path]/lsacgrk12.dta",
                          col_select = -c(cohort, wave, stratum, pcodes)),
                 by = "hicid",
                 all.x = TRUE,
                 suffixes = c("", "_w5"))

# Reading in W6 (K14)
mergew6 <- merge(x = mergew5, 
                 read_dta("[file_path]/lsacgrk14.dta",
                          col_select = -c(cohort, wave, stratum, pcodes)),
                 by = "hicid",
                 all.x = TRUE,
                 suffixes = c("", "_w6"))

## ================== Clear environment ====================================

rm(list = setdiff(ls(), "mergew6"))

## ================== Save working dataset =================================

saveRDS(mergew6, "[file_path]/lsac_merge.rds")

## ================== End of script ========================================

# Open script called 'lsac_cleaning.R'