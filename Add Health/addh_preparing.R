'''
---
title: "Cross-cohort analysis cannabis depression"
author: Chloe Burke
dataset: Add Health
date: "October, 2024"
script: "Creating working dataset"
---
'''

# LOAD PACKAGES
##install.packages("haven")
library(haven)
##install.packages("dplyr")
library(dplyr)

# Note: Replace [file_path] with your local directory containing the Add Health data files

## ================== Read in sample weights ===================================

# Sample weights for Wave I
setwd("[file_path]")
w1_weights <- read_xpt(
  'Homewt1.xpt',
  col_select = NULL,
  skip = 0, 
  n_max = Inf,
  .name_repair = "unique"
)

# Sample weights for Wave II
setwd("[file_path]")
w2_weights <- read_xpt(
  'Homewt2.xpt',
  col_select = NULL,
  skip = 0, 
  n_max = Inf,
  .name_repair = "unique"
)

## ================== Read in T0 data ==========================================

# Reading in in-home interview data for Wave I
setwd("[file_path]")
w1_interview <- read_xpt(
  'allwave1.xpt',
  col_select = c("AID", "H1GI20", "H1TO1", "H1TO2", "H1TO3", "H1TO5", "H1TO30", "H1TO32", "H1FS1", "H1FS2", "H1FS3", "H1FS4", "H1FS5", "H1FS6", "H1FS7", "H1FS8", "H1FS9", "H1FS10", "H1FS11", "H1FS12", "H1FS13", "H1FS14",
                 "H1FS15", "H1FS16", "H1FS17", "H1FS18", "H1FS19", "H1GI1Y", "H1GI1M", "IMONTH", "IYEAR", "BIO_SEX", "H1GI4", "H1GI6A", "H1GI6B",
                 "H1GI6C", "H1GI6D", "H1GI6E", "H1GI8", "PA55", "PA12", "H1TO12", "H1TO15", "H1TO20", "H1TO21", "H1TO22", "H1TO23", "H1TO24", "H1TO25",
                 "H1TO26", "H1TO27", "H1TO28", "H1TO34", "H1TO37", "H1TO40", "H1GH52", "H1GH59A", "H1GH59B", "H1GH60", "H1DA6", "H1DA5", "H1DA4",
                 "H1PF30", "H1PF32", "H1PF33", "H1PF34", "H1PF35", "H1PF36", "H1GH17", "H1GH18", "H1GH19", "H1GH20", "H1GH21", "H1GH22", "AH_PVT",
                 "H1DS2", "H1DS3", "H1DS5", "H1DS6", "H1DS7", "H1DS8", "H1DS9", "H1DS10", "H1CO10", "H1FV7", "H1FV8", "H1DS11", "H1ED2", "H1CO1", "H1GI18",
                 "H1SU6", "PC49E_2", "PC49E_3", "H1NM3", "H1ED7", "H1ED15", "H1PR5", "H1IR23", "H1DA3", "H1NF3", starts_with("H1HR3"), starts_with("H1HR6")), 
  skip = 0, 
  n_max = Inf,
  .name_repair = "unique"
)

## ================== Merge W1 weights with T0 data ============================

# Joining sample by AID
ahw1 <- full_join(w1_interview, w1_weights, by = "AID")
rm(w1_weights, w1_interview) 

## ================== Read in T1 data ==========================================

## Reading in in-home interview data for Wave 2
merge_w2 <- merge(x = ahw1,
                  read_xpt("[file_path]/wave2.xpt",
                           col_select = c("AID", "IMONTH2", "IYEAR2", "H2FS1", "H2FS2", "H2FS3", "H2FS4", "H2FS5", "H2FS6", "H2FS7", "H2FS8", "H2FS9", "H2FS10",
                                          "H2FS11", "H2FS12", "H2FS13", "H2FS14", "H2FS15", "H2FS16", "H2FS17", "H2FS18", "H2FS19")),
                           by = "AID",
                           all = FALSE) #n = 14736

## ================== Merge W2 weights with T1 data ============================

# Joining sample by AID
ahw2 <- left_join(merge_w2, w2_weights, by = "AID") 

## ================== Merging in other data ====================================  

# Reading in W3
merge_w3 <- merge(
  x = ahw2,
  y = read_xpt("[file_path]/wave3.xpt",
               col_select = c("AID", starts_with("H3RA"), "H3MA1", "H3MA2", "H3OD31")),
  by = "AID",
  all.x = TRUE
)

# Reading in W4
merge_w4 <- merge(
  x = merge_w3,
  y = read_xpt("[file_path]/wave4.xpt",
               col_select = c("AID", "H4MA1", "H4MA2", "H4MA3", "H4MA4", "H4MA5", "H4MA6", "H4WP3",
                              "H4WP5", "H4WP9", "H4WP11", "H4WP16", "H4WP18", "H4WP30", "H4WP32")),
  by = "AID",
  all.x = TRUE
)

## ================== Valid sample weights =====================================  

# Restricting sample to those with valid sample weights at T1 and T2
# Should be n = 13568, https://addhealth.cpc.unc.edu/wp-content/uploads/docs/user_guides/GuidelinesforAnalysisofAddHealthData_202004.pdf

addh <- merge_w4 %>%
  filter(!is.na(PSUSCID.x) & !is.na(REGION.x) & !is.na(GSWGT1) & !is.na(GSWGT2)) 

rm(ahw1, ahw2, merge_w2, merge_w3, merge_w4, w2_weights)

## ================== Check eligibility criteria ===============================

#-----SAMPLE INDICATORS-----#

## Variable: School grade
## Relevant variable(s): H1GI20
## Q: "What grade (ARE/WERE) you in?" 
## C: 7 = 7th, 8 = 8th, 9 = 9th, 10 = tenth, 11 = eleventh, 12 = twelfth
## M: 96 = refused, 97 = legitimate skip (non participating school), 98 = don't know, 99 = school doesn't have grade levels

addh <- addh %>%
  mutate(school_grade = case_when(
    H1GI20 == 97 | H1GI20 == 98 | H1GI20 == 99 ~ NA,
    TRUE ~ H1GI20
  ))

## Want indicator of 12th grade, as varying approach to follow-up for this school year
addh <- addh %>%
  mutate(grade_12 = case_when(
    school_grade == 12 ~ 1,
    TRUE ~ 0
  ))

#------------------------------#

## Variable: Age (years)
## Relevant variable(s): IMONTH, IYEAR, H1GI1M, H1GI1Y
## M: 96 = refused, applicable to H1GI1M and H1GI1Y only

addh <- addh %>%
  mutate(H1GI1Y = na_if(H1GI1Y, 96),
         H1GI1M = na_if(H1GI1M, 96)) # Replace '96' with NA in H1GI1Y and H1GI1M 

addh['interview_months'] = (addh['IYEAR']*12) + (addh['IMONTH']) # Calculate the interview date in months (year * 12 + month)
addh['birth_months'] = (addh['H1GI1Y'] * 12) + addh['H1GI1M'] # Calculate the birth date in months (Year * 12 + Month)
addh['age_in_months'] = addh['interview_months'] - addh['birth_months'] # Calculate the age in months by subtracting birth_months from interview_months
addh['age_in_years'] = addh['age_in_months'] / 12 # Calculate the age in years by dividing the age in months by 12

## Check contents of variables; have 11 - 21 years which aligns with reported age range
summary(addh$age_in_months)
summary(addh$age_in_years) # NA = 9

## Removing these n = 9 from dataset due to issues with imputation model having 'NA' for sample design
# Such a small number unlikely to make a difference!

addh <- addh %>%
  filter(!is.na(age_in_years))

## ================== End of script ============================================

# Open script called 'addh_cleaning.R' 

