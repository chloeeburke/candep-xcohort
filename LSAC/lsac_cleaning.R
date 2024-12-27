'''
---
title: "Cross-cohort analysis cannabis depression"
author: Chloe Burke
dataset: LSAC
date: "October 2024"
script: "Cleaning and processing"
---
'''

# LOAD PACKAGES
#install.packages("haven")
library(haven)
#install.packages("dplyr")
library(dplyr)
#install.packages("survey")
library(survey)
#install.packages("jtools")
library(jtools)
#install.packages("remotes")
library(remotes)
#remotes::install_github("carlganz/svrepmisc")
library(svrepmisc)
#install.packages("labelled")
library(labelled)

# Note: Replace [file_path] with your local directory containing the LSAC data files

## ================== Read in merged data ======================================

# Reading in merged data
lsac <- readRDS("[file_path]/lsac_merge.rds")

# LSAC weights and sample information are contained in lsac file
# PSU = pcodes
# strata = stratum
# weights = iweights

## ================== Prepare variables  =======================================

#------SAMPLE INDICATORS------#

## FOLLOW-UP LENGTH
# Relevant variable(s) = idatint, jdatint
# C: Numeric

table(lsac$idatint)
table(lsac$jdatint)

# Convert the 'idatint' and 'jdatint' columns to Date format
lsac$idatint <- as.Date(lsac$idatint, format = "%Y-%m-%d")
lsac$jdatint <- as.Date(lsac$jdatint, format = "%Y-%m-%d")

# Calculate follow-up length in days
lsac$followup_days <- as.numeric(lsac$jdatint - lsac$idatint)

# Calculate follow-up length in months (approximate, dividing days by 30.44)
lsac$followup_months <- lsac$followup_days / 30.44

# Summary of follow-up lengths
summary(lsac$followup_months)

#-----AUXILIARY VARIABLES-----#

## SEP
# Relevant variable(s) = hsep2
# Q: Z-score for socioeconomic position among all families
# C: Numeric
# M: NA

lsac <- lsac %>%
  mutate(sep_index = if_else(is.na(hsep2), NA_real_, hsep2))
summary(lsac$sep_index)

#---------------------#

## SCHOOL SUSPENSE
# Relevant variable(s) = hse20a12
# Q: "In the last 12 months had you..? Been suspended or expelled from school"
# C: 0 Not at all;1 Once;2 Twice;3 Three times;4 Four times;5 Five or more times
# M: -9, -3, NA

lsac <- lsac %>%
  mutate(scl_susp = factor(case_when(
    hse20a12 == -9 | hse20a12 == -3 ~ NA_real_,
    hse20a12 == 0 ~ 0,
    hse20a12 >=1 ~ 1
  ), levels = c(0, 1), labels = c("No suspense", "Any suspense"))) 

table(lsac$scl_susp)
sum(is.na(lscl_susp))

#---------------------#

## CONTACT WITH JUSTICE SYSTEM
# Relevant variable(s) = hse25a
# Q: "In the last 12 months have you..? Been told to ‘move on’, or been warned or cautioned, by police"
# C: 0 Not at all; 1 Once; 2 Twice; 3 More often
# M: -9, NA

lsac <- lsac %>%
  mutate(pol_warn = factor(case_when(
    hse25a == -9  ~ NA_real_,
    hse25a == 0 ~ 0,
    hse25a >=1 ~ 1
  ), levels = c(0, 1), labels = c("No contact", "Police caution/warning")))  

table(lsac$pol_warn)
sum(is.na(lsac$pol_warn)) 

#---------------------#
## NUMBER OF SIBLINGS
# Relevant variable(s) = hnsib
# Q: "Number of siblings of Study Child in household"
# C: Numeric, range 0 - 8
# M: -9, NA

lsac <- lsac %>%
  mutate(sib_no = factor(case_when(
    hnsib == -9  ~ NA_real_,
    hnsib == 0 ~ 0,
    hnsib == 1 ~ 1,
    hnsib == 2 ~ 2,
    hnsib == 3 ~ 3,
    hnsib >= 4 ~ 4),
    levels = c(0,1,2,3,4), labels = c("None", "One", "Two", "Three", "Four+")))

table(lsac$sib_no)
sum(is.na(lsac$sib_no)) 

#---------------------#
## P1 EMPLOYMENT STATUS
# Relevant variable(s) = haemp
# Q: "P1 employed by Labour Force Survey definition"
# C: 1 Employed; 2 Unemployed; 3 Not in labour force
# M: -9, NA

lsac <- lsac %>%
  mutate(p1_empl = factor(case_when(
    haemp == -9  ~ NA_real_,
    haemp == 1 ~ 0,
    haemp == 2 ~ 1,
    haemp == 3 ~ 2),
    levels = c(0,1,2), labels = c("Employed", "Unemployed", "Not in labour force")))

table(lsac$p1_empl)
sum(is.na(lsac$p1_empl)) 

#---------------------#
## STUDY CHILD CMD
# Relevant variable(s) = hhs17v
# Q: "Does study child have any of these ongoing conditions? Anxiety disorder, Depression"
# C: 0 No; 1 Yes
# M: -9, NA

lsac <- lsac %>%
  mutate(sc_cmd = factor(case_when(
    hhs17v == -9  ~ NA_real_,
    hhs17v == 0 ~ 0,
    hhs17v == 1 ~ 1),
    levels = c(0,1), labels = c("No CMD", "CMD")))

table(lsac$sc_cmd)
sum(is.na(lsac$sc_cmd)) 

#-----EXPOSURE VARIABLES-----#

## EVER CANNABIS USE
# Relevant variable(s) = ihb26c1a
# Incorporates responses across W5+6+7, otherwise would be put down as 'missing' due to legitimate skip
# Q: "Have you ever tried marijuana (cannabis, hash, grass, dope, weed, mull, yarndi, ganga, pot, a bong, a joint)?"
# C: 1 = Yes, 2 = No
# M: -9, -3

lsac <- lsac %>%
  mutate(cannabis_ever = factor(case_when(
    ihb26c1a == -9 | ihb26c1a == -3 ~ NA_real_,
    ihb26c1a == 2 ~ 0,
    ihb26c1a == 1 ~ 1
  ), levels = c(0, 1), labels = c("Never", "Ever")))  

table(lsac$cannabis_ever)
sum(is.na(lsac$cannabis_ever)) 

#---------------------#

## CURRENT CANNABIS USE
# Relevant variable(s) = cannabis_ever, ihb26c3 (if c_ever = 1), ihb26c4 (if ihb26c3 = 1)
# Q: (ihb26c3): "Have you smoked/used marijuana in the last twelve months"?
# C: (ihb26c3): 1 = Yes, 2 = No
# Q: (ihb26c4): "Have you smoked/used marijuana in the last four weeks?"
# C: (ihb26c4): 1 = Yes, 2 = No
# M: -9, -3

lsac <- lsac %>%
  mutate(cannabis_year = case_when(
    cannabis_ever == "Never" | ihb26c3 == 2 ~ 0,
    is.na(cannabis_ever) | ihb26c3 == -9 | ihb26c3 == -3 ~NA,
    ihb26c3 == 1 ~ 1
  ))
table(lsac$cannabis_year)

lsac <- lsac %>%
  mutate(cannabis_month = case_when(
    cannabis_year == 0 | ihb26c4 == 2 ~ 0,
    is.na(cannabis_year) | ihb26c4 == -9 | ihb26c4 == -3 ~ NA,
    ihb26c4 == 1 ~ 1
  ))
table(lsac$cannabis_month)
sum(is.na(lsac$cannabis_month)) 

#---------------------#

# CANNABIS FREQUENCY
# Relevant variable(s) = cannabis_month, ihb26c5, ihb26c6
# Q: (ihb26c5): "Have you smoked/used marijuana in the last week"?
# C: (ihb26c5): 1 = Yes, 2 = No
# Q: (ihb26c6): "How often did you smoke/use marijuana in the last week?"
# C: (ihb26c6): 1 = 1 day; 2 = 2 or 3 days; 3 = 4 or 5 days; 4 = 6 or 7 days
# M: -9, -3

lsac <- lsac %>%
  mutate(cannabis_week = case_when(
    cannabis_month == 0 | ihb26c5 == 2 ~ 0,
    is.na(cannabis_ever) | ihb26c5 == -9 | ihb26c5 == -3 ~NA,
    ihb26c5 == 1 ~ 1
  ))
table(lsac$cannabis_week)

lsac <- lsac %>%
  mutate(cannabis_freq = factor(case_when(
    cannabis_week == 1 | ihb26c6 >=1 ~ 2, # Weekly or more
    cannabis_month == 1 ~ 1, # Occasional
    cannabis_year >=0 | cannabis_ever == "Ever" ~ 0, # Non-use
    ihb26c6 == -9 | ihb26c6 == -3 ~ NA_real_
  ),
  levels = c(0,1,2), 
  labels = c("Non-use", "Occasional use", "Weekly or more")))

table(lsac$cannabis_freq)
sum(is.na(lsac$cannabis_freq)) 

#---------------------#

## EVER SMOKING
# Relevant variable(s) = ihb15c9a
# Incorporates responses across W5+6+7, otherwise would be put down as 'missing' due to legitimate skip
# Q: "Have you ever smoked even part of a cigarette?"
# C: 1 = Yes, 2 = No
# M: -9, -3

lsac <- lsac %>%
  mutate(smok_ever = factor(case_when(
    ihb15c9a == -9 | ihb15c9a == -3 ~ NA_real_,  
    ihb15c9a == 2 ~ 0,
    ihb15c9a == 1 ~ 1
  ),
  levels = c(0, 1),
  labels = c("Never", "Ever")))

table(lsac$smok_ever)
sum(is.na(lsac$smok_ever)) 

#---------------------#

## CURRENT SMOKING
# Relevant variable(s) = smok_ever, ihb15c11 (if s_ever = 1), ihb15c12 (if ihb15c11 = 1)
# Q: (ihb15c11): "Have you smoked cigarettes in the last twelve months"?
# C: (ihb15c11): 1 = Yes, 2 = No
# Q: (ihb15c12): "Have you smoked cigarettes in the last four weeks?"
# C: (ihb15c12): 1 = Yes, 2 = No
# M: -9, -3

lsac <- lsac %>%
  mutate(smok_year = case_when(
    smok_ever == "Never" | ihb15c11 == 2 ~ 0,
    is.na(smok_ever) | ihb15c11 == -9 | ihb15c11 == -3 ~NA,
    ihb15c11 == 1 ~ 1
  ))
table(lsac$smok_year)

lsac <- lsac %>%
  mutate(smok_month = case_when(
    smok_year == 0 | ihb15c12 == 2 ~ 0,
    is.na(smok_year) | ihb15c12 == -9 | ihb15c12 == -3 ~ NA,
    ihb15c12 == 1 ~ 1
  ))
table(lsac$smok_month)
sum(is.na(lsac$smok_month)) 

#---------------------#

# SMOKING FREQUENCY
# Relevant variable(s) = smok_month, ihb15c13
# Q: (ihb15c13): "This question is about the number of cigarettes you had during the last seven days, including yesterday"?
# C: (ihb15c13): 00-99
# M: -9, -3

lsac <- lsac %>%
  mutate(smok_week = case_when(
    smok_month == 0 | ihb15c13 == 0 ~ 0,
    is.na(smok_month) | ihb15c13 == -9 | ihb15c13 == -3 ~NA,
    ihb15c13 >=1 ~ 1
  ))
table(lsac$smok_week)

lsac <- lsac %>%
  mutate(smok_freq = factor(case_when(
    ihb15c13 >=1 ~ 2, # Weekly or more
    ihb15c13 == 0 | smok_month == 1 ~ 1, # Occasional
    smok_year >=0 | smok_ever == "Ever" ~ 0, # Non-use
    ihb15c13 == -9 | ihb15c13 == -3 ~ NA_real_
  ),
  levels = c(0, 1, 2),
  labels = c("Non-use", "Occasional use", "Weekly or more")))

table(lsac$smok_freq)
sum(is.na(lsac$smok_freq)) 

#---------------------#

## CONCURRENT USE
# Relevant variable(s): pastmonth_smok, pastmonth_can
# Want to generate derived variable that summarises (i) non-use, (ii) only tobacco use, (ii) only cannabis use, (iii) concurrent use

lsac <- lsac %>%
  mutate(ct_couse = case_when(
    is.na(smok_month) | is.na(cannabis_month) ~ NA_integer_,
    smok_month == 0 & cannabis_month == 0 ~ 0,
    smok_month == 1 & cannabis_month == 0 ~ 1,
    smok_month == 0 & cannabis_month == 1 ~ 2,
    smok_month == 1 & cannabis_month == 1 ~ 3
  ), 
  ct_couse = factor(ct_couse,
                    levels = c(0,1,2,3),
                    labels = c("Non-use", "Tobacco only", "Cannabis only", "Co-use")))

table(lsac$ct_couse)
sum(is.na(lsac$ct_couse)) 

#-----COVARIATES-----#

## DEPRESSION @ BASELINE
# Relevant variable(s) = ise21c1-ise21c13, ismfq (total score reverse coded)
# Q: "How much have you felt or acted this way in the past two weeks...?"
# C: 1 = Yes, 2 = No
# M: -9, -3

# Reverse coding
lsac <- lsac %>%
  mutate(across(ise21c1:ise21c13, 
                ~ case_when(
                  . == -9 | . == -3 ~ NA_real_, 
                  . == 3 ~ 0,                    
                  . == 2 ~ 1,
                  . == 1 ~ 2,
                  TRUE ~ as.numeric(.)),
                .names = "smfq_{col}"))

# Total score
lsac <- lsac %>%
  mutate(smfq_total = rowSums(across(starts_with("smfq_ise")), na.rm = FALSE))
table(lsac$smfq_total)

#---------------------#

## AGE IN YEARS
# Relevant variable(s): iscagem, if03m1
# Q (iscagem): Study child age in months at time of survey
# C (iscagem): Number
# Q (if03m1): "What was the family member's age last birthday?"
# C (if03m1): Number 
# M: None

lsac <- lsac %>%
  mutate(eligible_age = case_when(
    if03m1 == 16 | if03m1 == 17 ~ 1,
    if03m1 == 15 | if03m1 == 18 ~ 0
  ))
table(lsac$eligible_age) 

lsac <- lsac %>%
  mutate(age_years = iscagem / 12)
summary(lsac$age_years)

#---------------------#

## CHILD SEX
# Relevant variable(s): zf02m1
# Q: "Is the family member male or female?"
# C: 1 = Male; 2 = Female
# M: None

lsac <- lsac %>%
  mutate(sex = case_when(
    zf02m1 == 1 ~ 0, # 0 = Male
    zf02m1 == 2 ~ 1  # 1 = Female
  ),
  sex = factor(sex, levels = c(0, 1), labels = c("Male", "Female")))
table(lsac$sex)
sum(is.na(lsac$sex)) 

#---------------------#

## FAMILY BACKGROUND
# Relevant variable(s): zf09m2, zf09m3, zf09m1, cf11m2, cf11m3, cf11m1, zf12m1
# Q (zf09m2): "In which country was Parent 1 born?"
# C (zf09m2): SACC code
# Q (zf09m3): "In which country was Parent 2 born?"
# C (zf09m3): SACC code
# Q (zf09m1): "In which country was Study Child born?"
# C (zf09m1): SACC code
# Q (cf11m2): "Does Parent 1 speak a language other than English at home? (If more than one, record main language)"
# C (cf11m2): ASCL code
# Q (cf11m3): "Does Parent 2 speak a language other than English at home? (If more than one, record main language)"
# C (cf11m3): ASCL code
# Q (cf11m1): "Does Study Child speak a language other than English at home? (If more than one, record main language)"
# C (cf11m1): ASCL code
# Q (zf12m1): "Is Study Child of Aboriginal or Torres Strait Islander origin?"
# C (zf12m1): 1 = No; 2 = Yes, Aboriginal; 3 = Yes, Torres Strait Islander; 4 = Yes, both


# Aboriginal and Torres Strait Islander status
lsac <- lsac %>%
  mutate(atsi = factor(case_when(
    zf12m1 %in% c(-3, -2) ~ NA_real_, 
    zf12m1 == 1 ~ 0,
    zf12m1 %in% c(2,3,4) ~ 1,
    TRUE ~ NA_real_  # Catch any other unexpected values
  ), 
  levels = c(0,1),
  labels = c("No", "Yes")))

table(lsac$atsi)
sum(is.na(lsac$atsi))

# Parents born overseas
attributes(lsac$zf09m2)
lsac <- lsac %>%
  mutate(
    # Mother born overseas
    P1bornos = factor(case_when(
      zf09m2 %in% c(-2) ~ NA_real_,  # Missing codes only
      zf09m2 == 1101 ~ 0,         # Australia
      !is.na(zf09m2) ~ 1,         # All other countries including confidentialized (0)
      TRUE ~ NA_real_
    ), levels = c(0,1), labels = c("No", "Yes")),
    
    # Father born overseas
    P2bornos = factor(case_when(
      zf09m3 %in% c(-9) ~ NA_real_,  # Missing codes only
      zf09m3 == 1101 ~ 0,         # Australia
      !is.na(zf09m3) ~ 1,         # All other countries including confidentialized (0)
      TRUE ~ NA_real_
    ), levels = c(0,1), labels = c("No", "Yes")),
    
    # Either parent born overseas
    paros = factor(case_when(
      P2bornos == "Yes" | P1bornos == "Yes" ~ 1,
      P2bornos == "No" & P1bornos == "No" ~ 0,
      TRUE ~ NA_real_
    ), levels = c(0,1), labels = c("No", "Yes"))
  )

table(lsac$paros)
sum(is.na(lsac$paros))

# Parent country categories
lsac <- lsac %>%
  mutate(
    # P1 country
    p1country = case_when(
      zf09m2 %in% c(-2) ~ NA_real_,  # Handle negative missing values first
      zf09m2 == 0 ~ NA_real_,            # Handle confidentialized values
      zf09m2 == 1101 ~ 1,
      zf09m2 == 1201 | zf09m2 == 2201 | 
        (zf09m2 > 2099 & zf09m2 < 2200) |
        (zf09m2 > 8099 & zf09m2 < 8105) ~ 2,
      zf09m2 > 2300 & zf09m2 < 4000 ~ 3,
      zf09m2 > 3999 & zf09m2 < 5000 ~ 4,
      zf09m2 > 4999 & zf09m2 < 7000 ~ 5,
      zf09m2 > 6999 & zf09m2 < 8000 ~ 6,
      zf09m2 > 8200 & zf09m2 < 9000 ~ 7,
      (zf09m2 > 9000 & zf09m2 < 10000) | 
        zf09m2 == 4102 | zf09m2 == 4105 ~ 8,
      zf09m2 < 2000 & zf09m2 != 1101 & zf09m2 != 1201 ~ 9,
      TRUE ~ NA_real_  # Catch any other values as missing
    )) %>%
  mutate(p1country = factor(p1country,
                            levels = 1:9,
                            labels = c("Australia", "Anglo or other English speaking country",
                                       "European country", "Western Asia (Middle East)",
                                       "South-east and North-east Asia", "Subcontinent and Central Asia",
                                       "Central/South America", "Africa", "Oceania")))

# Convert to binary category
lsac <- lsac %>%
  mutate(p1countrycat = factor(case_when(
    as.numeric(p1country) <= 3 ~ 0,
    as.numeric(p1country) >= 4 & as.numeric(p1country) <= 9 ~ 1,
    TRUE ~ NA_real_
  ), levels = c(0,1), labels = c("No", "Yes")))
table(lsac$p1country)
sum(is.na(lsac$p1country))

# P2 country categories
lsac <- lsac %>%
  mutate(
    # P2 country
    p2country = case_when(
      zf09m3 %in% c(-9) ~ NA_real_,  # Handle negative missing values first
      zf09m3 == 0 ~ NA_real_,            # Handle confidentialized values
      zf09m3 == 1101 ~ 1,
      zf09m3 == 1201 | zf09m3 == 2201 | 
        (zf09m3 > 2099 & zf09m3 < 2200) |
        (zf09m3 > 8099 & zf09m3 < 8105) ~ 2,
      zf09m3 > 2300 & zf09m3 < 4000 ~ 3,
      zf09m3 > 3999 & zf09m3 < 5000 ~ 4,
      zf09m3 > 4999 & zf09m3 < 7000 ~ 5,
      zf09m3 > 6999 & zf09m3 < 8000 ~ 6,
      zf09m3 > 8200 & zf09m3 < 9000 ~ 7,
      (zf09m3 > 9000 & zf09m3 < 10000) | 
        zf09m3 == 4102 | zf09m3 == 4105 ~ 8,
      zf09m3 < 2000 & zf09m3 != 1101 & zf09m3 != 1201 ~ 9,
      TRUE ~ NA_real_  # Catch any other values as missing
    )) %>%
  mutate(p2country = factor(p2country,
                            levels = 1:9,
                            labels = c("Australia", "Anglo or other English speaking country",
                                       "European country", "Western Asia (Middle East)",
                                       "South-east and North-east Asia", "Subcontinent and Central Asia",
                                       "Central/South America", "Africa", "Oceania")))

# Convert to binary category
lsac <- lsac %>%
  mutate(p2countrycat = factor(case_when(
    as.numeric(p2country) <= 3 ~ 0,
    as.numeric(p2country) >= 4 & as.numeric(p2country) <= 9 ~ 1,
    TRUE ~ NA_real_
  ), levels = c(0,1), labels = c("No", "Yes")))
table(lsac$p2country)
sum(is.na(lsac$p2country))

# Main language (child)
lsac <- lsac %>%
  mutate(
    # Main language full categories
    lang = case_when(
      cf11m1 > 0 & cf11m1 < 2000 ~ "Northern European",
      cf11m1 > 1999 & cf11m1 < 3000 ~ "Southern European",
      cf11m1 > 2999 & cf11m1 < 4000 ~ "Eastern European",
      cf11m1 > 3999 & cf11m1 < 5000 ~ "Southwest and Central Asian",
      cf11m1 > 4999 & cf11m1 < 6000 ~ "Southern Asian",
      cf11m1 > 5999 & cf11m1 < 7000 ~ "Southeast Asian",
      cf11m1 > 6999 & cf11m1 < 8000 ~ "Eastern Asian",
      cf11m1 > 7999 & cf11m1 < 9000 ~ "Indigenous",
      cf11m1 > 8999 & cf11m1 < 10000 ~ "Other",
      cf11m1 == 1201 ~ "English",
      TRUE ~ NA_character_
    ),
    # Binary language category
    langcat = case_when(
      lang %in% c("Northern European", "Southern European", 
                  "Eastern European", "English") ~ "Anglo/Euro",
      !is.na(lang) ~ "Other",
      TRUE ~ NA_character_
    )
  )
table(lsac$lang)
sum(is.na(lsac$lang))

# P1 language
lsac <- lsac %>%
  mutate(
    # P1 language full categories
    p1lang = case_when(
      cf11m2 > 0 & cf11m2 < 2000 ~ "Northern European",
      cf11m2 > 1999 & cf11m2 < 3000 ~ "Southern European",
      cf11m2 > 2999 & cf11m2 < 4000 ~ "Eastern European",
      cf11m2 > 3999 & cf11m2 < 5000 ~ "Southwest and Central Asian",
      cf11m2 > 4999 & cf11m2 < 6000 ~ "Southern Asian",
      cf11m2 > 5999 & cf11m2 < 7000 ~ "Southeast Asian",
      cf11m2 > 6999 & cf11m2 < 8000 ~ "Eastern Asian",
      cf11m2 > 7999 & cf11m2 < 9000 ~ "Indigenous",
      cf11m2 > 8999 & cf11m2 < 10000 ~ "Other",
      cf11m2 == 1201 ~ "English",
      TRUE ~ NA_character_
    ),
    # Binary P1 language category
    p1langcat = case_when(
      p1lang %in% c("Northern European", "Southern European", 
                    "Eastern European", "English") ~ "Anglo/Euro",
      !is.na(p1lang) ~ "Other",
      TRUE ~ NA_character_
    )
  )
table(lsac$p1langcat)
sum(is.na(lsac$p1langcat))

# P2 language
lsac <- lsac %>%
  mutate(
    # P2 language full categories
    p2lang = case_when(
      cf11m3 > 0 & cf11m3 < 2000 ~ "Northern European",
      cf11m3 > 1999 & cf11m3 < 3000 ~ "Southern European",
      cf11m3 > 2999 & cf11m3 < 4000 ~ "Eastern European",
      cf11m3 > 3999 & cf11m3 < 5000 ~ "Southwest and Central Asian",
      cf11m3 > 4999 & cf11m3 < 6000 ~ "Southern Asian",
      cf11m3 > 5999 & cf11m3 < 7000 ~ "Southeast Asian",
      cf11m3 > 6999 & cf11m3 < 8000 ~ "Eastern Asian",
      cf11m3 > 7999 & cf11m3 < 9000 ~ "Indigenous",
      cf11m3 > 8999 & cf11m3 < 10000 ~ "Other",
      cf11m3 == 1201 ~ "English",
      TRUE ~ NA_character_
    ),
    # Binary P2 language category
    p2langcat = case_when(
      p2lang %in% c("Northern European", "Southern European", 
                    "Eastern European", "English") ~ "Anglo/Euro",
      !is.na(p2lang) ~ "Other",
      TRUE ~ NA_character_
    )
  )
table(lsac$p2langcat)
sum(is.na(lsac$p2langcat))

# Now create the ethnicity variable
lsac <- lsac %>%
  mutate(
    # Start with everyone as 0 
    ethnicity = 0,
    # Then replace with 1 for visible minority conditions
    ethnicity = case_when(
      langcat == "Other" | p2langcat == "Other" | p1langcat == "Other" ~ 1,
      p1countrycat == "Yes" | p2countrycat == "Yes" ~ 1,
      TRUE ~ ethnicity),
    # Then replace with 2 for Indigenous
    ethnicity = case_when(
      atsi == "Yes" ~ 2,
      TRUE ~ ethnicity),
    # Missing only if ALL component variables are missing
    ethnicity = case_when(
      is.na(langcat) & is.na(p2langcat) & is.na(p1langcat) & 
        is.na(p1countrycat) & is.na(p2countrycat) & is.na(atsi) ~ NA_real_,
      TRUE ~ ethnicity),
    # Convert to factor
    ethnicity = factor(ethnicity, 
                       levels = 0:2, 
                       labels = c("Anglo/Euro", "Visible minority", "Indigenous"))
  )

# Check distributions
table(lsac$ethnicity, useNA = "ifany")
sum(is.na(lsac$ethnicity)) 

#------------------------------#

## MATERNAL EDUCATION
# Relevant variable: ifd08m1
# Q: "What was the highest year of primary or secondary school you completed?"
# C: 1 = Year 12, 2 = Year 11, 3 = Year 10, 4 = Year 9, 5 = Year 8, 6 = never attended, 7 = Still at school 
# M = -9, -2

# First, code school completion
lsac <- lsac %>%
  mutate(mum_school = case_when(
    ifd08m1 == 1 ~ 1,  # Year 12
    ifd08m1 > 1 & ifd08m1 < 7 ~ 0,  # Less than Year 12 (but attended school)
    ifd08m1 == 7 ~ NA,  # Still at school
    ifd08m1 %in% c(-9, -2) ~ NA
  ))

# Code further education yes/no
lsac <- lsac %>%
  mutate(mum_further = case_when(
    ifd08m2a == 1 ~ 1,  # Yes to further education
    ifd08m2a == 2 ~ 0,  # No further education
    ifd08m2a %in% c(-9, -2) ~ NA
  ))

# Code highest qualification level
lsac <- lsac %>%
  mutate(mum_higher = case_when(
    ifd08m3a %in% c(1, 2, 3) ~ 2,  # Bachelor's or above
    ifd08m3a %in% c(4, 5) ~ 1,  # Certificate/diploma
    ifd08m3a == 6 ~ 1,  # Other qualification
    mum_further == 0 ~ 0,  # No further education
    ifd08m3a %in% c(-9, -2) ~ NA
  ))

# Final categorization following paper definition: doi.org/10.1136/jech-2022-219617)
lsac <- lsac %>%
  mutate(maternal_edu = case_when(
    # High: bachelor's or above
    mum_higher == 2 ~ 2,
    # Medium: certificates/diplomas
    mum_higher == 1 ~ 1,
    # Low: year 12 or below (including those with no further education)
    (mum_school == 1 & mum_further == 0) | 
      (mum_school == 0 & mum_further == 0) ~ 0,
    TRUE ~ NA_real_
  ),
  maternal_edu = factor(maternal_edu,
                        levels = c(0, 1, 2),
                        labels = c("Lower", "Moderate", "Higher")))

# Check the results
print("Distribution of maternal education:")
print(table(lsac$maternal_edu, useNA="ifany"))
print(paste("Number of missing values:", sum(is.na(lsac$maternal_edu)))) 

#------------------------------#

## ILLICIT DRUG USE
# Relevant variable: ihb27c1a
# Q: "Have you ever tried chroming or sniffing to get high (e.g. using glue, petrol, aerosols)?"
# C: 1 Yes; 2 No
# M = -9, -3

lsac <- lsac %>%
  mutate(illegaldrug_v1 = case_when(
    ihb27c1a == -9 | ihb27c1a == -3 ~ NA,
    ihb27c1a == 1 ~1,
    ihb27c1a == 2 ~ 0 
  ))  

table(lsac$illegaldrug_v1)
sum(is.na(lsac$illegaldrug_v1)) #nmiss = 86

# Relevant variable: ihb28c1a
# Q: "Have you ever tried other drugs (e.g. speed, ecstasy, LSD, ice, cocaine, heroin)?"
# C: 1 Yes; 2 No
# M = -9, -3

lsac <- lsac %>%
  mutate(illegaldrug_v2 = case_when(
    ihb28c1a == -9 | ihb28c1a == -3 ~ NA,
    ihb28c1a == 1 ~1,
    ihb28c1a == 2 ~ 0 
  ))  

table(lsac$illegaldrug_v2)
sum(is.na(lsac$illegaldrug_v2)) #nmiss = 114

# Now combining information to create overall indicator of lifetime illegal drug use 
lsac <- lsac %>%
  mutate(ever_drug = case_when(
    illegaldrug_v1 == 1 | illegaldrug_v2 == 1 ~ 1,
    illegaldrug_v1 == 0 & illegaldrug_v2 == 0 ~ 0,
    TRUE ~NA
  ),
  ever_drug = factor(
    ever_drug,
    levels = c(0, 1),
    labels = c("No", "Yes"),
  ))

table(lsac$ever_drug)
sum(is.na(lsac$ever_drug)) 

#------------------------------#
## PROBLEMATIC ALCOHOL USE
# Relevant variable: ihb16c11a
# Q: Have you ever had even part of an alcoholic drink? 
# C: 1 = Yes; 2 = No
# M: -9, -3
# Really high % missing data (~40%) on ever alcohol consumption, has been highlighted is due to a data management error from LSAC: 10.1111/dar.13635

lsac <- lsac %>%
  mutate(ever_alc = case_when(
    ihb16c11a == 1 ~1,
    ihb16c11a == 2 ~0,
    ihb16c11a == -9 | ihb16c11a == -3 ~ NA
  ))  

table(lsac$ever_alc)
sum(is.na(lsac$ever_alc)) 


# Relevant variable: ihb16c13
# Q: Have you had an alcoholic drink in the last twelve months?
# C: 1 = Yes; 2 = No
# M: -9, -3

lsac <- lsac %>%
  mutate(year_alc = case_when(
    ever_alc == 0 | ihb16c13 == 2 ~0,
    ihb16c13 == 1 ~ 1,
    ihb16c13 == -9 | ihb16c13 == -3 ~ NA
  ))  

table(lsac$year_alc)
sum(is.na(lsac$year_alc)) 

# Relevant variable: ihb16c9
# Q: Have you had an alcoholic drink in the last four weeks?
# C: 1 = Yes; 2 = No
# M: -9, -3

lsac <- lsac %>%
  mutate(month_alc = case_when(
    ever_alc == 0 | year_alc == 0 | ihb16c9 == 2 ~0,
    ihb16c9 == 1 ~ 1,
    ihb16c9 == -9  ~ NA
  ))  

table(lsac$month_alc)
sum(is.na(lsac$month_alc)) #nmiss = 32

# Relevant variable: icalcharm
# Q: Sum of ihb16c14a--ihb16c14e which includes impact on school, family, accidents, fights, sexual intercourse
# C: Numeric (higher score, higher harms)
# M: 'NA' 
# Very low % of participants reporting scores above 5 (i.e., no alcohol harms)
# Following approach (10.1111/dar.13635) to create binary indicator of any report of harms 

# Processing ihb16c14a1--ihb16c14e
lsac <- lsac %>%
  mutate(across(
    c(ihb16c14a1, ihb16c14b, ihb16c14c, ihb16c14d, ihb16c14e), 
    ~ case_when(
      . == 1 ~ 0,
      . == 2 | . == 3 ~ 1,
      . == -9 | . == -3 ~ NA_real_
    ),
    .names = "alc_harm_{.col}"  # Creates new columns with names like alc_harm_ihb16c14a1
  )) %>%
  mutate(across(
    starts_with("alc_harm_"), 
    ~ if_else(is.na(.) & month_alc == 0, 0, .),
    .names = "{.col}"  # Keeps the new column names unchanged
  ))

# Indicator of any alcohol harms
lsac <- lsac %>%
  mutate(alc_probs = case_when(
    alc_harm_ihb16c14a1 == 1 | alc_harm_ihb16c14b == 1 | alc_harm_ihb16c14c == 1 | alc_harm_ihb16c14d == 1 | alc_harm_ihb16c14e == 1 ~ 1,
    alc_harm_ihb16c14a1 == 0 & alc_harm_ihb16c14b == 0 & alc_harm_ihb16c14c == 0 & alc_harm_ihb16c14d == 0 & alc_harm_ihb16c14e == 0 ~ 0,
    TRUE ~ NA
  ))

table(lsac$alc_probs)
sum(is.na(lsac$alc_probs)) 

#------------------------------#
## ADEQUATE SLEEP
# Relevant variable: ihs20c3
# Q: "During the last month, do you think you usually got enough sleep?"
# C: 1 = Plenty; 2 = Just enough; 3 = Not quite enough; 4 = Not nearly enough
# M: -9

lsac <- lsac %>%
  mutate(sleep_problems = case_when(
    ihs20c3 == 1 | ihs20c3 == 2 ~0,
    ihs20c3 == 3 | ihs20c3 == 4 ~ 1,
    ihs20c3 == -9  ~ NA
  ),
  sleep_problems = factor(
    sleep_problems,
    levels = c(0, 1),
    labels = c("No", "Yes"),
  ))

table(lsac$sleep_problems)
sum(is.na(lsac$sleep_problems)) 

#------------------------------#
## NEUROTICISM
# Relevant variable: icneuro
# Q: Mean of ise30c4 (reversed) and ise30c9 (taken from Big Five Personality Inventory; BFI-10)
# C: Numeric (1 - 5), higher scores, higher neuroticism
# M: NA

lsac <- lsac %>%
  mutate(icneuro = as.numeric(icneuro))

summary(lsac$icneuro)
sum(is.na(lsac$icneuro)) #nmiss = 15
class(lsac$icneuro)

#------------------------------#
## ANXIETY
# Relevant variables: ise16b1,  ise16b2,  ise16b3,  ise16b4,  ise16b5, ise16b6, ise16b7, ise16b8  
# Q: "Please select the word that shows how often each of these things happen to you..."
# C: 1 = Never, 2 = Sometimes, 3 = Often, 4 = Always
# M: -9, -3

# Processing ise16b1--ise16b8
lsac <- lsac %>%
  mutate(across(
    c(ise16b1, ise16b2, ise16b3, ise16b4, ise16b5, ise16b6, ise16b7, ise16b8), 
    ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == -9 | . == -3 ~ NA_real_
    ),
    .names = "spence_{.col}"  # Creates new columns with names 
  ))

# Creating sum score such that range 0 - 24, with higher scores higher anxiety
lsac <- lsac %>%
  mutate(anxiety_symptoms = rowSums(across(starts_with("spence_i")), na.rm = FALSE))

# Check total score (min = 0, max = 24)
summary(lsac$anxiety_symptoms)
sum(is.na(lsac$anxiety_symptoms)) 

#------------------------------#
## CONDUCT PROBLEMS
# Taking SC reported variable, to align with ALSPAC and Add Health
# Relevant variables: ise03c4a, ise03c4b, ise03c4c, ise03c4f, ise03c4g
# Q: "For each item, please mark the box...Please give your answers on the basis of how things have been for you over the last six months"
# C: 1 Not true; 2 Somewhat true; 3 Certainly true
# M: -9, -3

# Rescaling to match scores for SDQ scale
lsac <- lsac %>%
  mutate(across(ise03c4a:ise03c4g, 
                ~ case_when(
                  . == -9 | . == -3 ~ NA_real_, 
                  . == 1 ~ 0,                    
                  . == 2 ~ 1,
                  . == 3 ~ 2,
                  TRUE ~ as.numeric(.)),
                .names = "sdq_{col}"))

# Reverse coding ise03c4b
lsac <- lsac %>%
  mutate(sdq_ise03c4b = case_when(
    sdq_ise03c4b == 2 ~ 0,
    sdq_ise03c4b == 1 ~ 1,
    sdq_ise03c4b == 0 ~ 2))

# Summing scale (dropping NAs)
lsac <- lsac %>%
  mutate(cd_symptoms = rowSums(select(., sdq_ise03c4a, sdq_ise03c4b, sdq_ise03c4c, sdq_ise03c4f, sdq_ise03c4g)))
table(lsac$cd_symptoms)
sum(is.na(lsac$cd_symptoms)) 

#------------------------------#

## ADHD - across 1-5 (i.e., age 4 - 13 years)

# Define wave prefixes (only waves 1-5 now)
waves <- c("c", "d", "e", "f", "g")  # Removed 'h'

# Loop through waves
for(w in waves) {
  # Create variable names for this wave
  vars <- paste0(w, "se03a2", letters[1:5])
  new_vars <- paste0("sdq_", vars)
  
  # Rescaling
  lsac <- lsac %>%
    mutate(across(all_of(vars), 
                  ~ case_when(
                    . == -9 | . == -3 ~ NA_real_, 
                    . == 1 ~ 0,                    
                    . == 2 ~ 1,
                    . == 3 ~ 2,
                    TRUE ~ as.numeric(.)),
                  .names = "sdq_{col}"))
  
  # Reverse coding items d and e
  lsac <- lsac %>%
    mutate(across(c(!!sym(new_vars[4]), !!sym(new_vars[5])),
                  ~ case_when(
                    . == 2 ~ 0,
                    . == 1 ~ 1,
                    . == 0 ~ 2)))
  
  # Create wave-specific symptom score
  lsac <- lsac %>%
    mutate(!!paste0("adhd_symp_w", w) := 
             rowSums(select(., all_of(new_vars))))
  
  # Create wave-specific diagnosis indicator
  lsac <- lsac %>%
    mutate(!!paste0("adhd_dx_w", w) := 
             case_when(
               get(paste0("adhd_symp_w", w)) >= 7 ~ 1,
               get(paste0("adhd_symp_w", w)) < 7 ~ 0,
               is.na(get(paste0("adhd_symp_w", w))) ~ NA))
}

# Check results for each wave
for(w in waves) {
  print(paste("Wave", w, "ADHD diagnosis:"))
  print(table(lsac[[paste0("adhd_dx_w", w)]], useNA = "ifany"))
  print(paste("Missing:", sum(is.na(lsac[[paste0("adhd_dx_w", w)]]))))
}

# Verify our wave-specific variables exist
adhd_vars <- paste0("adhd_dx_w", waves)
print("Variables we're using:")
print(adhd_vars)

# Modified criteria: 50% of waves (3 out of 5) required
lsac <- lsac %>%
  mutate(
    # Count waves with ADHD and valid measurements
    adhd_dx_count = rowSums(across(all_of(adhd_vars), ~ . == 1), na.rm = TRUE),
    adhd_valid_waves = rowSums(!is.na(across(all_of(adhd_vars)))),
    
    # Modified classification (3+ valid waves required, which is 50% of 5 waves)
    adhd_dx_final = case_when(
      adhd_dx_count >= 2 & adhd_valid_waves >= 3 ~ 1,  # 2+ ADHD indications AND 3+ valid waves
      adhd_dx_count < 2 & adhd_valid_waves >= 3 ~ 0,   # <2 ADHD indications BUT 3+ valid waves
      TRUE ~ NA                                         # Not enough valid waves
    ))

# Check final results
print("Distribution of valid waves per person:")
table(lsac$adhd_valid_waves)
print("Distribution of ADHD indications per person:")
table(lsac$adhd_dx_count)
print("Final ADHD classification (2+ indications AND 3+ valid waves):")
table(lsac$adhd_dx_final, useNA = "ifany")
prop.table(table(lsac$adhd_dx_final, useNA = "ifany")) * 100
print(paste("Missing (less than 3 valid waves):", sum(is.na(lsac$adhd_dx_final)))) 

#------------------------------#
## ACEs 

## PARENT LEGAL PROBLEMS
# For each wave
waves <- c("c", "d", "e", "f", "g", "h")

for(wave in waves) {
  # Create variables
  orig_var <- paste0(wave, "hs27l")
  new_var <- paste0(wave, "legprob")
  miss_var <- paste0("misslegprob", wave)
  
  # Clone and create missing indicator
  lsac <- lsac %>%
    mutate(
      # Clone original but convert missing codes to NA
      !!new_var := case_when(
        get(orig_var) %in% c(-9, -3) ~ NA_real_,
        TRUE ~ get(orig_var)
      ),
      # Create missing indicator
      !!miss_var := case_when(
        is.na(get(new_var)) ~ 1,   # Missing = 1
        TRUE ~ 0                    # All other values = 0
      )
    )
}

# Create any legal problems variable
lsac <- lsac %>%
  mutate(
    anylegprob = 0,  # Start with 0
    anylegprob = case_when(
      is.na(clegprob) | is.na(dlegprob) | is.na(elegprob) | 
        is.na(flegprob) | is.na(glegprob) | is.na(hlegprob) ~ NA_real_,  # Missing if ANY missing
      TRUE ~ anylegprob
    ),
    anylegprob = case_when(
      clegprob == 1 | dlegprob == 1 | elegprob == 1 | 
        flegprob == 1 | glegprob == 1 | hlegprob == 1 ~ 1,  # 1 if ANY 1
      TRUE ~ anylegprob
    ),
    missanylegprob = case_when(
      is.na(anylegprob) ~ 1,  # Missing = 1
      TRUE ~ 0               # All other values = 0
    )
  )

# Sum total allowing partial missing
lsac <- lsac %>%
  mutate(
    legprob_all = rowSums(select(., c("clegprob", "dlegprob", "elegprob", 
                                      "flegprob", "glegprob", "hlegprob")), 
                          na.rm = TRUE)  
  )

# Check distribution
table(lsac$legprob_all, useNA = "ifany")

## HOUSEHOLD SUBSTANCE ABUSE

# For each wave
waves <- c("c", "d", "e", "f", "g", "h")

for(wave in waves) {
  # Create variables
  orig_var <- paste0(wave, "hs27n")
  new_var <- paste0(wave, "subuse")  
  miss_var <- paste0("misssubuse", wave)  
  
  # Clone and create missing indicator
  lsac <- lsac %>%
    mutate(
      # Clone original - ensure only 0/1 values remain
      !!new_var := case_when(
        get(orig_var) == -9 ~ NA_real_,
        get(orig_var) == 1 ~ 1,
        get(orig_var) == 0 ~ 0,
        TRUE ~ NA_real_  # Any other values become NA
      ),
      # Create missing indicator
      !!miss_var := case_when(
        is.na(get(new_var)) ~ 1,   # Missing = 1
        TRUE ~ 0                    # Both 0 and 1 = 0
      )
    )
}

# Create any substance use variable
lsac <- lsac %>%
  mutate(
    anysubuse = 0,  # Start with 0
    anysubuse = case_when(
      is.na(csubuse) | is.na(dsubuse) | is.na(esubuse) | 
        is.na(fsubuse) | is.na(gsubuse) | is.na(hsubuse) ~ NA_real_,  # Missing if ANY missing
      TRUE ~ anysubuse
    ),
    anysubuse = case_when(
      csubuse == 1 | dsubuse == 1 | esubuse == 1 | 
        fsubuse == 1 | gsubuse == 1 | hsubuse == 1 ~ 1,  # 1 if ANY 1
      TRUE ~ anysubuse
    ),
    missanysubuse = case_when(
      is.na(anysubuse) ~ 1,  # Missing = 1
      TRUE ~ 0               # All other values (0,1) = 0
    )
  )

# Sum total allowing partial missing
lsac <- lsac %>%
  mutate(
    subuse_all = rowSums(select(., c("csubuse", "dsubuse", "esubuse", 
                                     "fsubuse", "gsubuse", "hsubuse")), 
                         na.rm = TRUE)  # Sum non-missing values
  )

# Check individual waves first
for(wave in waves) {
  print(paste("Wave", toupper(wave)))
  print(table(lsac[[paste0(wave, "subuse")]], useNA = "ifany"))
}

print("Any substance use problems")
table(lsac$anysubuse, useNA = "ifany")

print("Total waves with substance use problems")
table(lsac$subuse_all, useNA = "ifany")

## PARENTAL SEPARATION

# For each wave
waves <- c("c", "d", "e", "f", "g", "h")

for(wave in waves) {
  # Create variables
  orig_var <- paste0(wave, "hs27o")
  new_var <- paste0(wave, "relsep")  
  miss_var <- paste0("missrelsep", wave)  
  
  # Clone and create missing indicator
  lsac <- lsac %>%
    mutate(
      # Clone original - ensure only 0/1 values remain
      !!new_var := case_when(
        get(orig_var) == -9 | get(orig_var) == -3 ~ NA_real_,
        get(orig_var) == 1 ~ 1,
        get(orig_var) == 0 ~ 0,
        TRUE ~ NA_real_  # Any other values become NA
      ),
      # Create missing indicator
      !!miss_var := case_when(
        is.na(get(new_var)) ~ 1,   # Missing = 1
        TRUE ~ 0                    # Both 0 and 1 = 0
      )
    )
}

# Create any relationship separation variable
lsac <- lsac %>%
  mutate(
    anyrelsep = 0,  # Start with 0
    anyrelsep = case_when(
      is.na(crelsep) | is.na(drelsep) | is.na(erelsep) | 
        is.na(frelsep) | is.na(grelsep) | is.na(hrelsep) ~ NA_real_,  # Missing if ANY missing
      TRUE ~ anyrelsep
    ),
    anyrelsep = case_when(
      crelsep == 1 | drelsep == 1 | erelsep == 1 | 
        frelsep == 1 | grelsep == 1 | hrelsep == 1 ~ 1,  # 1 if ANY 1
      TRUE ~ anyrelsep
    ),
    missanyrelsep = case_when(
      is.na(anyrelsep) ~ 1,  # Missing = 1
      TRUE ~ 0               # All other values (0,1) = 0
    )
  )

# Sum total allowing partial missing
lsac <- lsac %>%
  mutate(
    relsep_all = rowSums(select(., c("crelsep", "drelsep", "erelsep", 
                                     "frelsep", "grelsep", "hrelsep")), 
                         na.rm = TRUE)  # Sum non-missing values
  )

# Check individual waves first
for(wave in waves) {
  print(paste("Wave", toupper(wave)))
  print(table(lsac[[paste0(wave, "relsep")]], useNA = "ifany"))
}

print("Any relationship separation")
table(lsac$anyrelsep, useNA = "ifany")

print("Total waves with relationship separation")
table(lsac$relsep_all, useNA = "ifany")

## FAMILY DEATH

# For each wave
waves <- c("c", "d", "e", "f", "g", "h")
for(wave in waves) {
  # Create variables
  orig_var <- paste0(wave, "hs27c")
  new_var <- paste0(wave, "death")  
  miss_var <- paste0("missdeath", wave)  
  
  # Clone and create missing indicator
  lsac <- lsac %>%
    mutate(
      # Clone original - ensure only 0/1 values remain
      !!new_var := case_when(
        get(orig_var) == -9 | get(orig_var) == -3 ~ NA_real_,
        get(orig_var) == 1 ~ 1,
        get(orig_var) == 0 ~ 0,
        TRUE ~ NA_real_  # Any other values become NA
      ),
      # Create missing indicator
      !!miss_var := case_when(
        is.na(get(new_var)) ~ 1,   # Missing = 1
        TRUE ~ 0                    # Both 0 and 1 = 0
      )
    )
}
# Create any family death variable
lsac <- lsac %>%
  mutate(
    anydeath = 0,  # Start with 0
    anydeath = case_when(
      is.na(cdeath) | is.na(ddeath) | is.na(edeath) | 
        is.na(fdeath) | is.na(gdeath) | is.na(hdeath) ~ NA_real_,  # Missing if ANY missing
      TRUE ~ anydeath
    ),
    anydeath = case_when(
      cdeath == 1 | ddeath == 1 | edeath == 1 | 
        fdeath == 1 | gdeath == 1 | hdeath == 1 ~ 1,  # 1 if ANY 1
      TRUE ~ anydeath
    ),
    missanydeath = case_when(
      is.na(anydeath) ~ 1,  # Missing = 1
      TRUE ~ 0               # All other values (0,1) = 0
    )
  )
# Sum total allowing partial missing
lsac <- lsac %>%
  mutate(
    death_all = rowSums(select(., c("cdeath", "ddeath", "edeath", 
                                    "fdeath", "gdeath", "hdeath")), 
                        na.rm = TRUE)  # Sum non-missing values
  )
# Check individual waves first
for(wave in waves) {
  print(paste("Wave", toupper(wave)))
  print(table(lsac[[paste0(wave, "death")]], useNA = "ifany"))
}
print("Any family member death")
table(lsac$anydeath, useNA = "ifany")
print("Total waves with family member death")
table(lsac$death_all, useNA = "ifany")

## PARTNER VIOLENCE

# (1) Define whether lone parent at each wave

# For each wave
waves <- c("c", "d", "e", "f", "g", "h")

for(wave in waves) {
  # Create variables
  orig_var <- paste0(wave, "partner")
  new_var <- paste0(wave, "lonpn")  
  
  # Create lone parent indicator
  lsac <- lsac %>%
    mutate(
      # Recode partner variable (0=1, 1=0)
      !!new_var := factor(case_when(
        get(orig_var) == 0 ~ 1,
        get(orig_var) == 1 ~ 0,
        TRUE ~ NA_real_
      ), levels = c(0,1), labels = c("Partnered", "Lone parent"))
    )
}

# Check distributions
for(wave in waves) {
  print(paste("Wave", toupper(wave)))
  print(table(lsac[[paste0(wave, "lonpn")]], useNA = "ifany"))
}

# (2) Define presence of violent arguments at each wave

# For each wave
waves <- c("c", "d", "e", "f", "g", "h")

for(wave in waves) {
  # Create variables
  re15a5_var <- paste0(wave, "re15a5")
  re15b5_var <- paste0(wave, "re15b5")
  lonpn_var <- paste0(wave, "lonpn")
  new_var <- paste0(wave, "famviol")
  miss_var <- paste0("missfamviol", wave)
  
  # Create family violence indicator
  lsac <- lsac %>%
    mutate(
      # Handle missing codes first for both variables
      !!re15a5_var := case_when(
        get(re15a5_var) %in% c(-9, -3) ~ NA_real_,
        TRUE ~ get(re15a5_var)
      ),
      !!re15b5_var := case_when(
        get(re15b5_var) %in% c(-9, -3) ~ NA_real_,
        TRUE ~ get(re15b5_var)
      ),
      
      # Create family violence variable
      !!new_var := NA_real_,  # Start with missing
      
      !!new_var := case_when(
        !is.na(get(re15a5_var)) | !is.na(get(re15b5_var)) ~ 0,  # Set to 0 if any data available
        TRUE ~ NA_real_
      ),
      
      !!new_var := case_when(
        get(lonpn_var) == "Lone parent" ~ 0,  # Lone parents coded as 0
        TRUE ~ get(new_var)
      ),
      
      !!new_var := case_when(
        get(re15a5_var) %in% c(2,3,4,5) | get(re15b5_var) %in% c(2,3,4,5) ~ 1,  # Set to 1 if violence reported
        TRUE ~ get(new_var)
      ),
      
      # Create missing indicator
      !!miss_var := case_when(
        is.na(get(new_var)) ~ 1,  # Missing = 1
        TRUE ~ 0                   # All other values = 0
      )
    )
}

# Check distributions
for(wave in waves) {
  print(paste("Wave", toupper(wave)))
  print(table(lsac[[paste0(wave, "famviol")]], useNA = "ifany"))
}

## HOUSEHOLD MEMBER MENTAL ILLNESS

### Parent 1

# For each wave
waves <- c("c", "d", "e", "f", "g", "h")

for(wave in waves) {
  # Create variable names for original variables
  vars <- paste0(wave, c("hs24a1", "hs24a2", "hs24a3", "hs24a4", "hs24a5", "hs24a6"))
  # Create variable names for recoded variables
  vars_r <- paste0(wave, c("hs24a1r", "hs24a2r", "hs24a3r", "hs24a4r", "hs24a5r", "hs24a6r"))
  # Create name for final pmh1 variable
  pmh_var <- paste0(wave, "pmh1")
  
  # First recode all original variables
  for(i in 1:6) {
    lsac <- lsac %>%
      mutate(
        !!vars_r[i] := case_when(
          get(vars[i]) < 0 ~ NA_real_,  # Set negative values to NA
          get(vars[i]) == 1 ~ 4,
          get(vars[i]) == 2 ~ 3,
          get(vars[i]) == 3 ~ 2,
          get(vars[i]) == 4 ~ 1,
          get(vars[i]) == 5 ~ 0,
          TRUE ~ NA_real_
        )
      )
  }
  
  # Then create sum and categorize
  lsac <- lsac %>%
    mutate(
      # Create temporary sum variable
      !!pmh_var := case_when(
        !is.na(get(vars_r[1])) & !is.na(get(vars_r[2])) & 
          !is.na(get(vars_r[3])) & !is.na(get(vars_r[4])) & 
          !is.na(get(vars_r[5])) & !is.na(get(vars_r[6])) ~ 
          get(vars_r[1]) + get(vars_r[2]) + get(vars_r[3]) + 
          get(vars_r[4]) + get(vars_r[5]) + get(vars_r[6]),
        TRUE ~ NA_real_
      ),
      # Recode sum into binary
      !!pmh_var := case_when(
        get(pmh_var) <= 12 ~ 0,
        get(pmh_var) >= 13 ~ 1,
        TRUE ~ NA_real_
      )
    )
}

# Check distributions
for(wave in waves) {
  print(paste("Wave", toupper(wave)))
  print(table(lsac[[paste0(wave, "pmh1")]], useNA = "ifany"))
}

### Parent 2

# For each wave
waves <- c("c", "d", "e", "f", "g", "h")

for(wave in waves) {
  # Create variable names for original variables
  vars <- paste0(wave, c("hs24b1", "hs24b2", "hs24b3", "hs24b4", "hs24b5", "hs24b6"))
  # Create variable names for recoded variables
  vars_r <- paste0(wave, c("hs24b1r", "hs24b2r", "hs24b3r", "hs24b4r", "hs24b5r", "hs24b6r"))
  # Create name for final pmh2 variable
  pmh_var <- paste0(wave, "pmh2")
  
  # First recode all original variables
  for(i in 1:6) {
    lsac <- lsac %>%
      mutate(
        !!vars_r[i] := case_when(
          get(vars[i]) < 0 ~ NA_real_,  # Set negative values to NA
          get(vars[i]) == 1 ~ 4,
          get(vars[i]) == 2 ~ 3,
          get(vars[i]) == 3 ~ 2,
          get(vars[i]) == 4 ~ 1,
          get(vars[i]) == 5 ~ 0,
          TRUE ~ NA_real_
        )
      )
  }
  
  # Then create sum and categorize
  lsac <- lsac %>%
    mutate(
      # Create temporary sum variable
      !!pmh_var := case_when(
        !is.na(get(vars_r[1])) & !is.na(get(vars_r[2])) & 
          !is.na(get(vars_r[3])) & !is.na(get(vars_r[4])) & 
          !is.na(get(vars_r[5])) & !is.na(get(vars_r[6])) ~ 
          get(vars_r[1]) + get(vars_r[2]) + get(vars_r[3]) + 
          get(vars_r[4]) + get(vars_r[5]) + get(vars_r[6]),
        TRUE ~ NA_real_
      ),
      # Recode sum into binary
      !!pmh_var := case_when(
        get(pmh_var) <= 12 ~ 0,
        get(pmh_var) >= 13 ~ 1,
        TRUE ~ NA_real_
      )
    )
}

# Check distributions
for(wave in waves) {
  print(paste("Wave", toupper(wave)))
  print(table(lsac[[paste0(wave, "pmh2")]], useNA = "ifany"))
}

### Both parents

# For each wave
waves <- c("c", "d", "e", "f", "g", "h")

for(wave in waves) {
  # Create variable names
  pmh1_var <- paste0(wave, "pmh1")
  pmh2_var <- paste0(wave, "pmh2")
  pmh_var <- paste0(wave, "pmh")
  miss_var <- paste0("misspmh", wave)
  
  # Create combined psychological distress variable
  lsac <- lsac %>%
    mutate(
      !!pmh_var := case_when(
        get(pmh1_var) == 1 | get(pmh2_var) == 1 ~ 1,  # Either parent has high distress
        (get(pmh1_var) == 0 & get(pmh2_var) == 0) ~ 0,  # Both parents no distress
        (get(pmh1_var) == 0 & is.na(get(pmh2_var))) ~ 0,  # P1 no distress, P2 missing
        (is.na(get(pmh1_var)) & get(pmh2_var) == 0) ~ 0,  # P1 missing, P2 no distress
        TRUE ~ NA_real_
      ),
      
      # Create missing indicator
      !!miss_var := case_when(
        is.na(get(pmh_var)) ~ 1,  # Missing = 1
        TRUE ~ 0                   # All other values = 0
      )
    )
}

# Check distributions
for(wave in waves) {
  print(paste("Wave", toupper(wave)))
  print(table(lsac[[paste0(wave, "pmh")]], useNA = "ifany"))
  print(paste("Missing indicator:"))
  print(table(lsac[[paste0("misspmh", wave)]], useNA = "ifany"))
}

# Create any parent mental health problems variable
lsac <- lsac %>%
  mutate(
    anyparmhd = 0,  # Start with 0
    # Set to missing if ANY wave is missing
    anyparmhd = case_when(
      is.na(cpmh) | is.na(dpmh) | is.na(epmh) | 
        is.na(fpmh) | is.na(gpmh) | is.na(hpmh) ~ NA_real_,
      TRUE ~ anyparmhd
    ),
    # Set to 1 if ANY wave has mental health problems
    anyparmhd = case_when(
      cpmh == 1 | dpmh == 1 | epmh == 1 | 
        fpmh == 1 | gpmh == 1 | hpmh == 1 ~ 1,
      TRUE ~ anyparmhd
    ),
    
    # Create missing indicator
    missanyparmhd = case_when(
      is.na(anyparmhd) ~ 1,
      TRUE ~ 0
    ),
    
    # Create total count allowing partial missing
    parmhd_all = rowSums(select(., c("cpmh", "dpmh", "epmh", 
                                     "fpmh", "gpmh", "hpmh")), 
                         na.rm = TRUE)
  )

# Check distributions
print("Any parent mental health problems")
table(lsac$anyparmhd, useNA = "ifany")

print("Missing indicator")
table(lsac$missanyparmhd, useNA = "ifany")

print("Total waves with parent mental health problems")
table(lsac$parmhd_all, useNA = "ifany")

## BULLIED

# For waves c through h
waves <- c("c", "d", "e", "f", "g", "h")

for(wave in waves) {
  # Create variable names
  se03a5d_var <- paste0(wave, "se03a5d")
  se03t5d_var <- paste0(wave, "se03t5d")
  bully_var <- paste0(wave, "bully")
  miss_var <- paste0("missbull", wave)
  
  # Create bullying and missing indicators
  lsac <- lsac %>%
    mutate(
      # Handle negative values first
      !!se03a5d_var := case_when(
        get(se03a5d_var) < 0 ~ NA_real_,
        TRUE ~ get(se03a5d_var)
      ),
      !!se03t5d_var := case_when(
        get(se03t5d_var) < 0 ~ NA_real_,
        TRUE ~ get(se03t5d_var)
      ),
      
      # Create bullying variable
      !!bully_var := 0,  # Start with 0
      
      !!bully_var := case_when(
        is.na(get(se03a5d_var)) & is.na(get(se03t5d_var)) ~ NA_real_,  # Missing if both missing
        get(se03a5d_var) == 3 | get(se03t5d_var) == 3 ~ 1,  # 1 if either is 3
        TRUE ~ 0
      ),
      
      # Create missing indicator
      !!miss_var := case_when(
        is.na(get(bully_var)) ~ 1,  # Missing = 1
        TRUE ~ 0                     # All other values = 0
      )
    )
}

# Check distributions
for(wave in waves) {
  print(paste("Wave", toupper(wave)))
  print("Bullying:")
  print(table(lsac[[paste0(wave, "bully")]], useNA = "ifany"))
  print("Missing indicator:")
  print(table(lsac[[paste0("missbull", wave)]], useNA = "ifany"))
}

# Create any bullying variable
lsac <- lsac %>%
  mutate(
    anybull = 0,  # Start with 0
    # Set to missing if ANY wave is missing
    anybull = case_when(
      is.na(cbully) | is.na(dbully) | is.na(ebully) | 
        is.na(fbully) | is.na(gbully) | is.na(hbully) ~ NA_real_,
      TRUE ~ anybull
    ),
    # Set to 1 if ANY wave has bullying
    anybull = case_when(
      cbully == 1 | dbully == 1 | ebully == 1 | 
        fbully == 1 | gbully == 1 | hbully == 1 ~ 1,
      TRUE ~ anybull
    ),
    
    # Create missing indicator
    missanybull = case_when(
      is.na(anybull) ~ 1,
      TRUE ~ 0
    ),
    
    # Create total count allowing partial missing
    bull_all = rowSums(select(., c("cbully", "dbully", "ebully", 
                                   "fbully", "gbully", "hbully")), 
                       na.rm = TRUE)
  )

# Check distributions
print("Any bullying")
table(lsac$anybull, useNA = "ifany")

print("Missing indicator")
table(lsac$missanybull, useNA = "ifany")

print("Total waves with bullying")
table(lsac$bull_all, useNA = "ifany")

## UNSAFE NEIGHBOURHOOD

# For waves c through f and h (1-4 coding)
waves_cfh <- c("c", "d", "e", "f", "h")

for(wave in waves_cfh) {
  # Create variable names
  orig_var <- paste0(wave, "ho09a1a1")
  new_var <- paste0(wave, "safeneighb")
  miss_var <- paste0("misssafeneighb", wave)
  
  # Create safe neighborhood variables
  lsac <- lsac %>%
    mutate(
      # Handle negative values and recode
      !!new_var := case_when(
        get(orig_var) < 0 ~ NA_real_,  # Any negative to NA
        get(orig_var) %in% c(1,2) ~ 0,  # Safe
        get(orig_var) %in% c(3,4) ~ 1,  # Not safe
        TRUE ~ NA_real_
      ),
      
      # Create missing indicator
      !!miss_var := case_when(
        is.na(get(new_var)) ~ 1,
        TRUE ~ 0
      )
    )
}

# Handle wave g separately (1-5 coding)
lsac <- lsac %>%
  mutate(
    gsafeneighb = case_when(
      gho09a1a2 < 0 ~ NA_real_,         # Any negative to NA
      gho09a1a2 %in% c(1,2,3) ~ 0,      # Safe
      gho09a1a2 %in% c(4,5) ~ 1,        # Not safe
      TRUE ~ NA_real_
    ),
    misssafeneighbg = case_when(
      is.na(gsafeneighb) ~ 1,
      TRUE ~ 0
    )
  )

# Create any unsafe neighborhood variable
lsac <- lsac %>%
  mutate(
    anynosafeneig = 0,
    # Set to missing if ANY wave is missing
    anynosafeneig = case_when(
      is.na(csafeneighb) | is.na(dsafeneighb) | is.na(esafeneighb) |
        is.na(fsafeneighb) | is.na(gsafeneighb) | is.na(hsafeneighb) ~ NA_real_,
      TRUE ~ anynosafeneig
    ),
    # Set to 1 if ANY wave reports unsafe
    anynosafeneig = case_when(
      csafeneighb == 1 | dsafeneighb == 1 | esafeneighb == 1 |
        fsafeneighb == 1 | gsafeneighb == 1 | hsafeneighb == 1 ~ 1,
      TRUE ~ anynosafeneig
    ),
    
    # Create missing indicator
    missanynosafeneig = case_when(
      is.na(anynosafeneig) ~ 1,
      TRUE ~ 0
    ),
    
    # Create total count allowing partial missing
    nosafeneig_all = rowSums(select(., c("csafeneighb", "dsafeneighb", "esafeneighb",
                                         "fsafeneighb", "gsafeneighb", "hsafeneighb")),
                             na.rm = TRUE)
  )

# Check distributions
for(wave in c("c", "d", "e", "f", "g", "h")) {
  print(paste("Wave", toupper(wave)))
  print(table(lsac[[paste0(wave, "safeneighb")]], useNA = "ifany"))
}

print("Any unsafe neighborhood")
table(lsac$anynosafeneig, useNA = "ifany")

print("Total waves with unsafe neighborhood")
table(lsac$nosafeneig_all, useNA = "ifany")

## HARSH PARENTING

# Parent 1 + 2 @ Wave 2 and 3 #

# Parent 1 and 2 variables for waves d and e
vars_to_recode <- c(
  # Parent 1 variables
  paste0(rep(c("d", "e"), each=3), 
         rep(c("pa04a1", "pa04a2", "pa04a4"), times=2)),
  # Parent 2 variables
  paste0(rep(c("d", "e"), each=3), 
         rep(c("pa04b1", "pa04b2", "pa04b4"), times=2))
)

# Recode all negative values to missing
lsac <- lsac %>%
  mutate(across(all_of(vars_to_recode), 
                ~ifelse(. < 0, NA_real_, .)))

# Calculate means and categories for each wave and parent
waves <- c("d", "e")
parents <- list(
  p1 = list(code = "a", num = "1"),
  p2 = list(code = "b", num = "2")
)

for(wave in waves) {
  for(parent in parents) {
    # Create variable names
    vars <- paste0(wave, "pa04", parent$code, c(1, 2, 4))
    mean_var <- paste0(wave, "p", parent$num, "hostm")
    cat_var <- paste0(wave, "p", parent$num, "hostmcat")
    
    # Calculate mean and create categories
    lsac <- lsac %>%
      mutate(
        # Calculate mean (missing if all inputs missing)
        !!mean_var := rowMeans(select(., all_of(vars)), na.rm = TRUE),
        
        # Set mean to NA if all inputs are NA
        !!mean_var := ifelse(rowSums(is.na(select(., all_of(vars)))) == 3, 
                             NA_real_, 
                             get(mean_var)),
        
        # Create binary variable for top 5%
        !!cat_var := case_when(
          is.na(get(mean_var)) ~ NA_real_,
          get(mean_var) > quantile(get(mean_var), 0.95, na.rm = TRUE) ~ 1,  # Top 5%
          TRUE ~ 0  # Bottom 95%
        )
      )
  }
}

# Check distributions
for(wave in waves) {
  for(parent in parents) {
    print(paste("Wave", toupper(wave), "Parent", parent$num))
    print("Mean hostility score:")
    print(summary(lsac[[paste0(wave, "p", parent$num, "hostm")]]))
    print("Binary category (top 5%):")
    print(table(lsac[[paste0(wave, "p", parent$num, "hostmcat")]], useNA = "ifany"))
    print("---")
  }
}

# Waves 4-6

# Define waves and create variable lists
waves <- c("f", "g", "h")
parents <- list(
  p1 = list(code = "a", num = "1"),
  p2 = list(code = "b", num = "2")
)

# Create list of variables to recode
vars_to_recode <- c(
  # Parent 1 variables
  paste0(rep(waves, each=3), 
         rep(c("pa13a3", "pa13a4", "pa13a6"), times=length(waves))),
  # Parent 2 variables
  paste0(rep(waves, each=3), 
         rep(c("pa13b3", "pa13b4", "pa13b6"), times=length(waves)))
)

# Recode all negative values to missing
lsac <- lsac %>%
  mutate(across(all_of(vars_to_recode), 
                ~ifelse(. < 0, NA_real_, .)))

# Calculate means and categories for each wave and parent
for(wave in waves) {
  for(parent in parents) {
    # Create variable names
    vars <- paste0(wave, "pa13", parent$code, c(3, 4, 6))
    mean_var <- paste0(wave, "p", parent$num, "hostm")
    cat_var <- paste0(wave, "p", parent$num, "hostmcat")
    
    # Calculate mean and create categories
    lsac <- lsac %>%
      mutate(
        # Calculate mean (missing if all inputs missing)
        !!mean_var := rowMeans(select(., all_of(vars)), na.rm = TRUE),
        
        # Set mean to NA if all inputs are NA
        !!mean_var := ifelse(rowSums(is.na(select(., all_of(vars)))) == 3, 
                             NA_real_, 
                             get(mean_var)),
        
        # Create binary variable for top 5%
        !!cat_var := case_when(
          is.na(get(mean_var)) ~ NA_real_,
          get(mean_var) > quantile(get(mean_var), 0.95, na.rm = TRUE) ~ 1,  # Top 5%
          TRUE ~ 0  # Bottom 95%
        )
      )
    
    # Add variable labels if using labelled package
    attr(lsac[[mean_var]], "label") <- 
      sprintf("Mean of parent %s hostility wave %s", parent$num, toupper(wave))
    attr(lsac[[cat_var]], "label") <- 
      sprintf("Top harsh parenting P%s wave %s", parent$num, toupper(wave))
  }
}

# Check distributions
for(wave in waves) {
  for(parent in parents) {
    print(paste("Wave", toupper(wave), "Parent", parent$num))
    print("Mean hostility score:")
    print(summary(lsac[[paste0(wave, "p", parent$num, "hostm")]]))
    print("Binary category (top 5%):")
    print(table(lsac[[paste0(wave, "p", parent$num, "hostmcat")]], useNA = "ifany"))
    print("---")
  }
}

## COMBINE HARSH PARENTING ACROSS WAVES AND PARENTS

# Define all waves we've processed
waves <- c("d", "e", "f", "g", "h")

# Create combined parent variables for each wave
for(wave in waves) {
  parent_var <- paste0(wave, "p12host")
  miss_var <- paste0("missharsh", wave)
  p1_var <- paste0(wave, "p1hostmcat")
  p2_var <- paste0(wave, "p2hostmcat")
  
  lsac <- lsac %>%
    mutate(
      # Combined parent indicator
      !!parent_var := case_when(
        # If either parent has high hostility (==1), mark as 1
        .data[[p1_var]] == 1 | .data[[p2_var]] == 1 ~ 1,
        # If both parents have data and neither has high hostility, mark as 0
        (!is.na(.data[[p1_var]]) | !is.na(.data[[p2_var]])) ~ 0,
        # Otherwise missing
        TRUE ~ NA_real_
      ),
      
      # Create missing indicator (1 = missing, 0 = not missing)
      !!miss_var := case_when(
        is.na(.data[[parent_var]]) ~ 1,
        TRUE ~ 0
      )
    )
  
  # Add variable labels
  attr(lsac[[parent_var]], "label") <- 
    sprintf("Either parent high hostility wave %s", toupper(wave))
  attr(lsac[[miss_var]], "label") <- 
    sprintf("Missing data on harsh parenting wave %s", toupper(wave))
}

# Create indicator for harsh parenting at any wave
wave_vars <- paste0(waves, "p12host")

lsac <- lsac %>%
  mutate(
    # Any harsh parenting across waves
    anyhost = case_when(
      # If all waves are missing, result is missing
      rowSums(!is.na(select(., all_of(wave_vars)))) == 0 ~ NA_real_,
      # If any wave has harsh parenting (==1), mark as 1
      rowSums(select(., all_of(wave_vars)) == 1, na.rm = TRUE) > 0 ~ 1,
      # Otherwise 0
      TRUE ~ 0
    ),
    
    # Missing indicator for any harsh parenting
    missanyhost = case_when(
      is.na(anyhost) ~ 1,
      TRUE ~ 0
    ),
    
    # Count of waves with harsh parenting
    host_all = rowSums(select(., all_of(wave_vars)) == 1, na.rm = TRUE)
  )

# Add variable labels
attr(lsac[["anyhost"]], "label") <- "Harsh parenting at any time"
attr(lsac[["missanyhost"]], "label") <- "Missing data on harsh parenting at any time"
attr(lsac[["host_all"]], "label") <- "Number of waves with harsh parenting"

# Print summaries
cat("\nCombined parent indicators for each wave:\n")
for(wave in waves) {
  print(paste("Wave", toupper(wave)))
  print(table(lsac[[paste0(wave, "p12host")]], useNA = "ifany"))
  print("---")
}

cat("\nHarsh parenting at any wave:\n")
print(table(lsac$anyhost, useNA = "ifany"))

cat("\nNumber of waves with harsh parenting:\n")
print(table(lsac$host_all, useNA = "ifany"))

cat("\nMissing data indicators:\n")
for(wave in waves) {
  print(paste("Wave", toupper(wave)))
  print(table(lsac[[paste0("missharsh", wave)]], useNA = "ifany"))
  print("---")
}

print("Any wave missing:")
print(table(lsac$missanyhost, useNA = "ifany"))

## MULTIPLE ADVERSITY INDICATOR
# Create indicators for each TYPE of ACE (requiring >50% waves present)

lsac <- lsac %>%
  mutate(
    # Legal problems
    ever_legal = as.numeric(case_when(
      rowSums(!is.na(select(., paste0(c("c","d","e","f","g","h"), "legprob")))) < 4 ~ NA_real_,
      rowSums(select(., paste0(c("c","d","e","f","g","h"), "legprob")), na.rm = TRUE) > 0 ~ 1,
      TRUE ~ 0
    )),
    
    # Violence
    ever_violence = as.numeric(case_when(
      rowSums(!is.na(select(., paste0(c("c","d","e","f","g","h"), "famviol")))) < 4 ~ NA_real_,
      rowSums(select(., paste0(c("c","d","e","f","g","h"), "famviol")), na.rm = TRUE) > 0 ~ 1,
      TRUE ~ 0
    )),
    
    # Mental health
    ever_mh = as.numeric(case_when(
      rowSums(!is.na(select(., paste0(c("c","d","e","f","g","h"), "pmh")))) < 4 ~ NA_real_,
      rowSums(select(., paste0(c("c","d","e","f","g","h"), "pmh")), na.rm = TRUE) > 0 ~ 1,
      TRUE ~ 0
    )),
    
    # Substance use
    ever_substance = as.numeric(case_when(
      rowSums(!is.na(select(., paste0(c("c","d","e","f","g","h"), "subuse")))) < 4 ~ NA_real_,
      rowSums(select(., paste0(c("c","d","e","f","g","h"), "subuse")), na.rm = TRUE) > 0 ~ 1,
      TRUE ~ 0
    )),
    
    # Harsh parenting (only waves d-h)
    ever_harsh = as.numeric(case_when(
      rowSums(!is.na(select(., paste0(c("d","e","f","g","h"), "p12host")))) < 3 ~ NA_real_,
      rowSums(select(., paste0(c("d","e","f","g","h"), "p12host")), na.rm = TRUE) > 0 ~ 1,
      TRUE ~ 0
    )),
    
    # Separation
    ever_separation = as.numeric(case_when(
      rowSums(!is.na(select(., paste0(c("c","d","e","f","g","h"), "relsep")))) < 4 ~ NA_real_,
      rowSums(select(., paste0(c("c","d","e","f","g","h"), "relsep")), na.rm = TRUE) > 0 ~ 1,
      TRUE ~ 0
    )),
    
    # Bullying
    ever_bullying = as.numeric(case_when(
      rowSums(!is.na(select(., paste0(c("c","d","e","f","g","h"), "bully")))) < 4 ~ NA_real_,
      rowSums(select(., paste0(c("c","d","e","f","g","h"), "bully")), na.rm = TRUE) > 0 ~ 1,
      TRUE ~ 0
    ))
  )

# Create summary scores
lsac <- lsac %>%
  mutate(
    # Count how many ACE types have valid data
    n_valid_aces = rowSums(!is.na(select(., c("ever_legal", "ever_violence", "ever_mh", 
                                              "ever_substance", "ever_harsh", "ever_separation", 
                                              "ever_bullying")))),
    
    # Create total only if at least 4/7 ACE types have valid data (>50%)
    ace_types_total = case_when(
      n_valid_aces < 4 ~ NA_real_,  # Less than 50% of ACE types
      TRUE ~ ever_legal + ever_violence + ever_mh + ever_substance + 
        ever_harsh + ever_separation + ever_bullying
    ),
    
    # Create categorized version
    ace_types_cat = case_when(
      is.na(ace_types_total) ~ NA_real_,
      ace_types_total == 0 ~ 0,
      ace_types_total == 1 ~ 1,
      ace_types_total == 2 ~ 2,
      ace_types_total == 3 ~ 3,
      ace_types_total >= 4 ~ 4
    )
  ) %>%
  select(-n_valid_aces)  # Remove temporary variable

# Update variable labels
ace_labels <- c(
  ever_legal = "Ever experienced legal problems (>50% waves present)",
  ever_violence = "Ever experienced family violence (>50% waves present)",
  ever_mh = "Ever experienced parent mental health problems (>50% waves present)",
  ever_substance = "Ever experienced substance use problems (>50% waves present)",
  ever_harsh = "Ever experienced harsh parenting (>50% waves present)",
  ever_separation = "Ever experienced parental separation (>50% waves present)",
  ever_bullying = "Ever experienced bullying (>50% waves present)",
  ace_types_total = "Total number of ACE types experienced",
  ace_types_cat = "Categorized number of ACE types (0-4+)"
)

for(var in names(ace_labels)) {
  attr(lsac[[var]], "label") <- ace_labels[var]
}

# Print summaries
cat("\nPrevalence of each type of ACE:\n")
for(var in names(ace_labels)[1:7]) {  # First 7 are the individual ACEs
  print(ace_labels[var])
  print(table(lsac[[var]], useNA = "ifany"))
  print("---")
}

cat("\nTotal number of ACE types:\n")
print(table(lsac$ace_types_total, useNA = "ifany"))

cat("\nCategorized number of ACE types (0-4+):\n")
print(table(lsac$ace_types_cat, useNA = "ifany"))


#-----OUTCOME VARIABLES-----#

## DEPRESSION @ FOLLOW-UP
# Relevant variable(s) = jck10s
# Q: Sum of jhs24c1 to jhs24c10 reverse coded
# C: Numeric, possible range 10-50
# M: 'NA'

# Cut-off >=30
lsac <- lsac %>%
  mutate(k10_cut = factor(case_when(
    is.na(jck10s) ~ NA_real_,
    jck10s >= 30 ~ 1,
    jck10s < 30 ~ 0
  ),
  levels = c(0, 1),
  labels = c("Below cutoff", "Above cutoff")))

table(lsac$k10_cut)
sum(is.na(lsac$k10_cut)) 

## ================== Trim dataset ===============================

# Select prepared dataset
xyz_lsac <- lsac %>%
  select(hicid, jweights, pcodes, stratum, ichildp, jchildp, followup_months,  #Sample indicators and weights
         smok_ever, smok_freq, cannabis_ever, cannabis_freq, ct_couse, #Exposures
         smfq_total, smfq_cut, age_years, sex, ethnicity, maternal_edu, #Covariates
         alc_probs, ever_drug, sleep_problems,
         icneuro_num, anxiety_symptoms, cd_symptoms, adhd_dx_final, ace_types_cat,
         jck10s, k10_cut, # Outcome
         sep_index, scl_susp, pol_warn, sib_no, p1_empl, sc_cmd) # Auxiliary  

# Save prepared dataset
saveRDS(xyz_lsac, "[file_path]/lsac_prepped.rds")

# Tidy environment
rm(lsac, parent, parents)

## ================== End of script ===============================

# Open script called 'lsac_imputation.R'
