'''
---
title: "Cross-cohort analysis cannabis depression"
author: Chloe Burke
dataset: Add Health
date: "October 2024"
script: "Cleaning and processing"
---
'''

# Note: Replace [file_path] with your local directory containing the Add Health data files

## ================== Read in merged data ======================================

# Existing prepared data frame is 'addh'

## ================== Prepare T0 variables  ====================================

#-----AUXILLARY VARIABLES-----#

## Variable: Video games played in last week
## Relevant variable(s): H1DA3
## Q: "	During the past week, how many times did you watch television or videos, or play video games?"
## C: 0 = not at all, 1 = 1 or 2 times, 2 = 3 or 4 times, 3 = 5 or more times
## M: 6 = refused, 9 = don't know

addh <- addh %>%
  mutate(vid_game = case_when(
    H1DA3 == 6 | H1DA3 == 9 ~ NA, # Set specified values
    TRUE ~ H1DA3
  ),
  vid_game = factor(vid_game,
                    levels = c(0, 1, 2, 3),           
                    labels = c("None", "Low", "Moderate", "High"))) # Convert to factor 

table(addh$vid_game)
sum(is.na(addh$vid_game)) 

#------------------------------#

## Variable: Ever been suspended from school
## Relevant variable(s): H1ED7
## Q: "Have you ever received an out-of-school suspension?"
## C: 0 = no; 1 = yes
## M: 6 = refused, 8 = don't know, '.' = missing

addh <- addh %>%
  mutate(scl_susp = case_when(
    H1ED7 == 6 | H1ED7 == 8 | H1ED7 == '.' ~ NA, 
    TRUE ~ H1ED7
  ),
  scl_susp = factor(scl_susp,
                    levels = c(0, 1),           
                    labels = c("No", "Yes"))) 

table(addh$scl_susp)
sum(is.na(addh$scl_susp)) 

#------------------------------#

## Variable: Trouble getting along with teachers
## Relevant variable(s): H1ED15
## Q: "During the 1994-1995 school year, how often have you had trouble getting along with your teachers?"
## C: 0 = never; 1 = just a few times, 2 = about once a week, 3 = almost everyday; 4 = everyday
## M: 6 = refused, 8 = don't know, 7 = legit skip (not in school in past year) 

addh <- addh %>%
  mutate(tch_rel = case_when(
    H1ED15 == 6 | H1ED15 == 8 | H1ED7 == 7 ~ NA, 
    TRUE ~ H1ED15
  ),
  tch_rel = factor(tch_rel,
                   levels = c(0, 1, 2, 3, 4),           
                   labels = c("Never", "Few", "Weekly", "Often", "Everday"))) 

table(addh$tch_rel)
sum(is.na(addh$tch_rel))

#------------------------------#

## Variable: Family understanding/relationship
## Relevant variable(s): H1PR5
## Q: "How much do you feel that people in your family understand you?"
## C: 1 = not at all, 2 = very little, 3 = somewhat, 4 = quite a bit, 5 = very much
## M: 6 = does not apply, 95 = refused, 98 = don't know

addh <- addh %>%
  mutate(fam_rel = case_when(
    H1PR5 == 6 | H1PR5 == 95 | H1PR5 == 98 ~ NA, 
    TRUE ~ H1PR5
  ),
  fam_rel = factor(fam_rel,
                   levels = c(1, 2, 3, 4, 5),           
                   labels = c("Not at all", "Very little", "Somehwat", "Quite a bit", "Very much"))) 

table(addh$fam_rel)
sum(is.na(addh$fam_rel)) 

#------------------------------#

## Variable: Family structure
## Relevant variables are: H1HR3A-T and H1HR6A-E
## Description of coding approach in: https://addhealth.cpc.unc.edu/wp-content/uploads/docs/restricted_use/family_structure.pdf 
## We are collapsing to create indicator of both biologcal parents in household (Y) vs (N)

# Initialize a FAMST2 column with NA
addh$FAMST2 <- NA 

# Count occurrences of 'father' (11) and 'mother' (14) in H1HR3A to H1HR3T
addh$any_parents_count <- apply(addh[, c("H1HR3A", "H1HR3B", "H1HR3C", "H1HR3D", 
                                         "H1HR3E", "H1HR3F", "H1HR3G", "H1HR3H", 
                                         "H1HR3I", "H1HR3J", "H1HR3K", "H1HR3L", 
                                         "H1HR3M", "H1HR3N", "H1HR3O", "H1HR3P", 
                                         "H1HR3Q", "H1HR3R", "H1HR3S", "H1HR3T")], 
                                1, function(x) sum(x %in% c(11, 14), na.rm = TRUE)) # Count any parents

# Count occurrences of biological fathers (1) and biological mothers (7) in H1HR6A to H1HR6T
addh$bio_fathers_count <- apply(addh[, c("H1HR6A", "H1HR6B", "H1HR6C", "H1HR6D", 
                                         "H1HR6E", "H1HR6F", "H1HR6G", "H1HR6H", 
                                         "H1HR6I", "H1HR6J", "H1HR6K", "H1HR6L", 
                                         "H1HR6M", "H1HR6N", "H1HR6O", "H1HR6P", 
                                         "H1HR6Q", "H1HR6R", "H1HR6S", "H1HR6T")], 
                                1, function(x) sum(x == 1, na.rm = TRUE)) # Count biological fathers
addh$bio_mothers_count <- apply(addh[, c("H1HR6A", "H1HR6B", "H1HR6C", "H1HR6D", 
                                         "H1HR6E", "H1HR6F", "H1HR6G", "H1HR6H", 
                                         "H1HR6I", "H1HR6J", "H1HR6K", "H1HR6L", 
                                         "H1HR6M", "H1HR6N", "H1HR6O", "H1HR6P", 
                                         "H1HR6Q", "H1HR6R", "H1HR6S", "H1HR6T")], 
                                1, function(x) sum(x == 7, na.rm = TRUE))  # Count biological mothers

# Assign FAMST5 values based on the counts
addh <- addh %>%
  mutate(FAMST2 = case_when(
    bio_fathers_count >=1 & bio_mothers_count >=1 ~ 0,  # Two Biological Parents
    TRUE ~ 1  # Any other arrangement
  ))

# Display the frequency table for FAMST5
table(addh$FAMST2)

#------EXPOSURES---------#

## Variable: Ever smoked 
## Relevant variable(s): H1TO1
## Q: "Have you ever tried cigarette smoking, even just 1 or 2 puffs?"
## C: 0 = no; 1 = yes
## M: 6 = refused; 8 = don't know; 9 = not applicable

addh <- addh %>%
  mutate(ever_smok = case_when(
    H1TO1 == 6 | H1TO1 == 8 | H1TO1 == 9 ~ NA, # Set specified values
    TRUE ~ H1TO1
  ),
  ever_smok = factor(ever_smok,
                     levels = c(0, 1),           
                     labels = c("No", "Yes"))) # Convert to factor 

table(addh$ever_smok)
sum(is.na(addh$ever_smok)) 

#------------------------------#

## Variable: Current smoker
## Relevant variable(s): H1TO2 + H1To5
## Q (H1TO2): "How old were you when you smoked a whole cigarette for the first time?"
## C (H1TO2): Continuous variable with range 1 - 20; 0 = never smoked a whole cigarette
## M (H1TO2): 96 = refused; 97 = legitimate skip; 98 = don't know
## Q (H1TO5): "During the past 30 days, on how many days did you smoke cigarettes?"
## C (H1TO5): Continuous variable with range 0-30; 96 = refused; 97 = legitimate skip; 98 = don't know; 99 = not applicable; '.' = missing
## Note: Add Health codes not responding on ever smoking (H1TO1) as 'legitimate skip' for H1TO2 and H1TO5, but these are missing,
## ...we want to preserve legitimate skips to code these as 'non-smoker' alongside reporting 0 days of smoking in past 30 days

addh <- addh %>%
  mutate(whole_cig = case_when(
    is.na(ever_smok) | H1TO2 == 96 | H1TO2 == 98 | H1TO2 == '.' ~ NA,
    H1TO2 == 97  | H1TO2 == 0 ~ 0,
    H1TO2 >=1 ~1,
    TRUE ~ H1TO2
  ))

str(addh$H1TO5) 
addh <- addh %>%
  mutate(pastmonth_smok = case_when(
    is.na(ever_smok) | is.na(whole_cig) | H1TO5 == 96 | H1TO5 == 98 | H1TO5 == 99 | H1TO5 == '.' ~ NA,
    H1TO5 == 97 | H1TO5 == 0 ~ 0,
    H1TO5 >=1 ~ 1,
    TRUE ~ H1TO5
  ),
  pastmonth_smok = factor(pastmonth_smok,
                          levels = c(0, 1),           
                          labels = c("Non-smoker", "Current smoker")))

table(addh$pastmonth_smok)
sum(is.na(addh$pastmonth_smok)) 

#------------------------------#

## Variable: Smoking frequency 
## Relevant variable(s): H1To5
## Q: "During the past 30 days, on how many days did you smoke cigarettes?"
## C: Continuous variable with range 0-30
## M: 96 = refused; 97 = legitimate skip; 98 = don't know; 99 = not applicable; '.' = missing

addh <- addh %>%
  mutate(freq_smok = case_when(
    is.na(ever_smok) | is.na(whole_cig) | H1TO5 == 96 | H1TO5 == 98 | H1TO5 == 99 | H1TO5 == '.' ~ NA,
    H1TO5 == 97 | H1TO5 == 0 ~ 0,
    H1TO5 >=1 & H1TO5 <4 ~ 1, # Non-weekly
    H1TO5 >=4 ~2 # Weekly or more
  ),
  freq_smok = factor(freq_smok,
                     levels = c(0, 1, 2),           
                     labels = c("Non-use", "Occasional", "Weekly or more")))

table(addh$freq_smok)
sum(is.na(addh$freq_smok)) 

#------------------------------#

## Variable: Ever used cannabis
## Relevant variable(s): H1TO30
## Q: "How old were you when you tried marijuana for the first time? If you never tried marijuana, enter '0'"
## C: Continuous variable with range 1 - 18; 0 = never tried
## M: 96 = refused; 98 = don't know; 99 = not applicable

addh <- addh %>%
  mutate(ever_cannabis = case_when(
    H1TO30 == 96 | H1TO30 == 98 | H1TO30 == 99 ~ NA,
    H1TO30 == 0 ~ 0, # Never used
    H1TO30 >=1 ~1 # Any use reported 
  ),
  ever_cannabis = factor(ever_cannabis,
                         levels = c(0, 1),           
                         labels = c("Never use", "Ever use")))

table(addh$ever_cannabis)
sum(is.na(addh$ever_cannabis))

#------------------------------#

## Variable: Past-month cannabis use
## Relevant variable(s): H1TO32
## Q: "During the past 30 days, how often did you use marijuana?"
## C: Continuous variable; possible range 0-900 times
## M: 996 = refused; 997 = legitimate skip; 998 = don't know; 999 = not applicable
## Note: Only have legitimate skips here are those reporting 'no' on ever cannabis use...
## ...so want to code these as 'non-use'

addh <- addh %>%
  mutate(pastmonth_can = case_when(
    H1TO32 == 996 | H1TO32 == 998 | H1TO32 == 999 ~ NA,
    H1TO32 == 0 | H1TO32 == 997 ~ 0, # No use past month
    H1TO32 >=1 ~1 # Any use past month
  ),
  pastmonth_can = factor(pastmonth_can,
                         levels = c(0, 1),           
                         labels = c("Non-use", "Current use")))

table(addh$pastmonth_can)
sum(is.na(addh$pastmonth_can)) 

#------------------------------#

## Variable: Cannabis frequency
## Relevant variable(s): H1TO32
## Q: "During the past 30 days, how often did you use marijuana?"
## C: Continuous variable
## M: 996 = refused; 997 = legitimate skip; 998 = don't know; 999 = not applicable
## Only legitimate skips here are those reporting 'no' on ever cannabis use, so want to code these as 'non-use'

addh <- addh %>%
  mutate(freq_can = case_when(
    H1TO32 == 996 | H1TO32 == 998 | H1TO32 == 999 ~ NA,
    H1TO32 == 0 | H1TO32 == 997 ~ 0,
    H1TO32 >=1 & H1TO32 <4 ~ 1,
    H1TO32 >=4 ~2,
    TRUE ~ H1TO32
  ),
  freq_can = factor(freq_can,
                    levels = c(0, 1, 2),           
                    labels = c("Non-use", "Occasional", "Weekly or more")))

table(addh$freq_can)
sum(is.na(addh$freq_can)) 

#------------------------------#

## Variable: Concurrent use 
## Relevant variable(s): pastmonth_smok, pastmonth_can
## Want to generate derived variable that summarises (i) non-use, (ii) only tobacco use, (ii) only cannabis use, (iii) concurrent use

addh <- addh %>%
  mutate(ct_couse = case_when(
    is.na(pastmonth_smok) | is.na(pastmonth_can) ~ NA_integer_,
    pastmonth_smok == "Non-smoker" & pastmonth_can == "Non-use" ~ 0,
    pastmonth_smok == "Current smoker" & pastmonth_can == "Non-use" ~ 1,
    pastmonth_smok == "Non-smoker" & pastmonth_can == "Current use" ~ 2,
    pastmonth_smok == "Current smoker" & pastmonth_can == "Current use" ~ 3
  ), 
  ct_couse = factor(ct_couse,
                    levels = c(0,1,2,3),
                    labels = c("Non-use", "Tobacco only", "Cannabis only", "Co-use")))

table(addh$ct_couse)
sum(is.na(addh$ct_couse)) 

#-------COVARIATES-------#

## Variable: Depression @ baseline (CESD-R-10)
## Relevant variable(s): H1FS1, H1FS3, H1FS4*, H1FS5, H1FS6, H1FS7, H1FS11*, H1FS15*, H1FS16, H1FS17
## Three items need reverse coding: H1FS4, H1FS11 and H1FS15
## Q (H1FS4): "How often was the following true during the past week? You felt you were just as good as other people"
## Q (H1FS11): "How often was the following true during the past week? You were happy"
## Q (H1FS15): "How often was the following true during the past week? You enjoyed life"
## C (ALL): 0 = never of rarely; 1 = sometimes; 2 = a lot of the time; 3 = most of the time or all of the time
## M (ALL): 6 = refused; 8 = don't know; 9 = n/a

## Looping for initial data prep
dep_variables <- paste0("H1FS",1:19)
addh <- addh%>%
  mutate(across(all_of(dep_variables),
                ~ case_when (
                  . == 6 | . == 8 | . == 9 ~ NA_real_,
                  TRUE ~ .
                ),
                .names = "dep_v{col}"))

## Reverse coding H1FS4, H1FS11 and H1FS15
max_value <- 3
addh <- addh%>%
  mutate(
    dep_vH1FS4 = max_value - dep_vH1FS4, #Reverse coding for H1FS4
    dep_vH1FS11 = max_value - dep_vH1FS11, #Reverse coding for H1FS11
    dep_vH1FS15 = max_value - dep_vH1FS15 # Reverse coding for H1FS15
  )

## Summing to create total depression score
addh <- addh%>%
  mutate(total_cesd_bl = rowSums(addh[, c("dep_vH1FS1", "dep_vH1FS3", "dep_vH1FS4",
                                          "dep_vH1FS5", "dep_vH1FS6", "dep_vH1FS7", "dep_vH1FS11",
                                          "dep_vH1FS15", "dep_vH1FS16", "dep_vH1FS17")], 
                                 na.rm = FALSE))    

## Check total score (min = 0, max = 30)
summary(addh$total_cesd_bl)

#------------------------------#

## Variable: Biological sex
## Relevant variable(s): BIO_SEX 
## Q: "Interviewer, please confirm that R's sex is (male) female. (Ask if necessary.)"
## C: (1 = Male, 2 = Female)
## M: 6 = refused, 8 = don't know

addh <- addh %>%
  mutate(bio_sex = case_when(
    BIO_SEX == 6 | BIO_SEX == 8 ~ NA,
    TRUE ~ BIO_SEX
  ),
  bio_sex = factor(bio_sex,
                   levels = c(1, 2),           
                   labels = c("Male", "Female")))

table(addh$bio_sex)
sum(is.na(addh$bio_sex)) 

#------------------------------#


## Variable: Race/ethnicity ##
## Relevant variables are: H1GI4, H1GI6A-E and H1GI8
## First code for self-report of Hispanic or Latino origin (H1GI4)
## Q (H1GI4): "Are you of Hispanic or Spanish/Latino origin?"
## C (H1GI4): 0 = No, 1 = Yes; 6 = Refused; 8 = Don't know
## M: 6 = refused, 8 = don't know

addh <- addh %>%
  mutate(hispanic = case_when(
    H1GI4 == 6 | H1GI4 == 8 ~ NA,
    TRUE ~ H1GI4
  ))

table(addh$hispanic)
sum(is.na(addh$hispanic))

## Next code for self-reported race (H1GI6A - H1GI6E; "What is your race?"; variable response split by racial groups)
## C (H1GI6A-E): 0 = No; 1 = Yes
## M (H1GI6A-E): 6 = Refused; 8 = Don't know; 9 = Not applicable

addh <- addh %>%
  mutate(across(
    c(H1GI6A, H1GI6B, H1GI6C, H1GI6D, H1GI6E),
    ~ case_when(
      . == 6 | . == 8 | . == 9 ~ NA,
      TRUE ~ .
    ),
    .names = "race_{col}"
  ))

## Check updated columns
lapply(addh[, c("race_H1GI6A", "race_H1GI6B", "race_H1GI6C", "race_H1GI6D", "race_H1GI6E")], summary)

## Question open to self-report multiple racial backgrounds, so follow-up question asked in H1GI8 about "Which ONE category best describes your racial background?"
## We'll use this to define participants reporting multiple racial backgrounds
## C (H1GI8): 1 = White; 2 = Black or African American; 3 = American Indian or Native American; 4 = Asian or Pacific Islander; 5 = Other
## M (H1GI8): 6 = Refused; 8 = Don't know; 9 = Not applicable; 7 = legitimate skip

addh <- addh %>%
  mutate(race_multiple = case_when(
    H1GI8 == 6 | H1GI8 == 8 | H1GI8 == 9  ~ NA,
    TRUE ~ H1GI8))

table(addh$race_multiple)
sum(is.na(addh$race_multiple))

## Create indicator variable for participants reporting >1 racial background in H1GI6A-E
addh$multiple_racial_backgrounds <- as.integer(
  rowSums(addh[c("race_H1GI6A", "race_H1GI6B", "race_H1GI6C", "race_H1GI6D", "race_H1GI6E")] == 1, na.rm = TRUE) > 1
)
table(addh$multiple_racial_backgrounds) 

## Populate new variable with self-reported racial background according to approach in doi: 10.1016/j.healthplace.2023.102991
## If H1GI6A = 1 'White'; If H1GI6B = 1 'Black'; If H1GI6C-E = 1 'Other' - except if multiple_racial_backgrounds = 1; and if H1GI4 = 1; 'Hispanic'
addh <- addh %>%
  mutate(
    race_ethnicity = case_when(
      is.na(hispanic) ~ 9, # Preserve missing from Hispanic variable
      race_H1GI6A == 1 & multiple_racial_backgrounds == 0 ~ 1,  # Apply rule for 'White'
      race_H1GI6B == 1 & multiple_racial_backgrounds == 0 ~ 2,  # Apply rule for 'Black',
      hispanic == 1 ~ 3,  # Apply rule for 'Hispanic',
      race_H1GI6C == 1 & multiple_racial_backgrounds == 0 ~ 4,  # Apply rule for 'Other',
      race_H1GI6D == 1 & multiple_racial_backgrounds == 0 ~ 4,  # Apply rule for 'Other',
      race_H1GI6E == 1 & multiple_racial_backgrounds == 0 ~ 4,  # Apply rule for 'Other',
      TRUE ~ NA  # Handle other cases or leave as NA
    )
  ) #n_white = 11918, #n_black = 4400 , #n_hispanic = 1942, #n_other = 1616, #n_missing = 807, 9 = 62 (i.e., preserving missingness on Hispanic variable)

table(addh$race_ethnicity) 
sum(is.na(addh$race_ethnicity))

## Update the race_ethnicity column while ensuring H1GI4 remains NA if it is NA
addh <- addh %>%
  mutate(
    race_ethnicity = case_when(
      # Preserve existing values in race_ethnicity
      !is.na(race_ethnicity) ~ race_ethnicity,
      
      # Assign based on H1GI8 if not already categorized
      race_multiple == 1 ~ 1,
      race_multiple == 2 ~ 2,
      race_multiple == 3 ~ 4,
      race_multiple == 4 ~ 4,
      race_multiple == 5 ~ 4,
      
      # Default case for any unexpected values in H1GI8
      TRUE ~ NA
    )
  )

table(addh$race_ethnicity) 
sum(is.na(addh$race_ethnicity))

## Update race_ethnicity to convert 9 to NA (i.e., kept information on missing response to Hispanic question, so can now turn this to NA)
addh <- addh %>%
  mutate(
    race_ethnicity = case_when(
      race_ethnicity == 9 ~ NA_real_,  # Convert 9 to NA
      TRUE ~ race_ethnicity  # Preserve other values
    ),
    race_ethnicity = factor(
      race_ethnicity,
      levels = c(1, 2, 3, 4),
      labels = c("White", "Black", "Hispanic", "Other")
    )
  )

table(addh$race_ethnicity) 
sum(is.na(addh$race_ethnicity)) 

#------------------------------#

## Variable: Maternal education 
## Relevant variable(s): PA12 
## Q: "How far did you go in school?"
## C: 1 = 8th grade or less, 2 = More than 8th grade, didn't graduate from high school,
## ... 3 = Went to business, trade or vocational school, 4 = High school graduate,
## ... 5 = Completed a GED, 6 = Went to business, trade or vocational school after high school,
## ... 7 = Went to college but didn't graduate, 8 = Graduate from college or university, 
## ... 9 = Professional training beyond a 4-year college or university, 10 = Never went to school
## M: '.' = missing or no parent questionnaire (Section A) and 96 = refused to respond

addh <- addh %>%
  mutate(maternal_edu = case_when(
    PA12 == '.' | PA12 == 96 ~ NA,
    PA12 == 1 | PA12 == 2 | PA12 == 10 | PA12 == 3  ~ 0,  # Apply rule for 'less than high school'
    PA12 == 4 | PA12 == 5 | PA12 == 6  | PA12 == 7  ~ 1,  # Apply rule for 'high school'
    PA12 == 8 | PA12 == 9 ~ 2,  # Apply rule for 'college or higher'
  ),
  maternal_edu = factor(maternal_edu, # formatting as factor
                        levels = c(0, 1, 2), 
                        labels = c("Low", 
                                   "Moderate", 
                                   "High"))
  )

table(addh$maternal_edu) 
sum(is.na(addh$maternal_edu)) 

#------------------------------#

## Variable: Problematic alcohol use 
## Relevant variable(s): H1TO20, H1TO21, H1TO22, H1TO23, H1TO24, H1TO25, H1TO26, H1TO27, H1TO28, H1TO12, H1TO15,
## ...H1TO20-8 are indicators of problematic alcohol consumption (e.g., getting in trouble, problems at school/work)
## Supplementary variables: Also need H1TO12 (i.e., ever use) and H1TO15 (i.e., past-year use) to give information on legitimate skips vs missing data

## Creating variable to indicate lifetime alcohol use (H1TO12)
## Q: "Have you had a drink of beer, wine, or liquor - not just a sip or taste of someone elses drink - more than 2-3 times in your life?"
## C: 0 = No; 1 = Yes
## M: 6 = Refused, 8 = Don't know; 9 = Not applicable

addh <- addh %>%
  mutate(ever_drink = case_when(
    H1TO12 == 6 | H1TO12 == 8 | H1TO12 == 9 ~ NA,
    TRUE ~ H1TO12
  )) 

## Creating variable to indicate past-year alcohol use (H1TO15)
## Q: "During the past 12-months, how often did you drink beer, wine or liqour?"
## C: 1 = every day or almost every day; 2 = three to five days per week; 3 = one or two days per week; 4 = two or three days per month; 5 = once a month or less; ...
## M: 6 = one or two days in past year; 7 = never; 96 = refused; 97 = legitimate skip; 98 = don't know        
## Carry forward NAs in H1To12 to H1To15 as otherwise these are 'legitimate skips' want want to separate non-drinkers (97) from missing data

addh <- addh %>%
  mutate(pastyear_drink = case_when(
    is.na(ever_drink) | H1TO15 == 96 | H1TO15 == 98 ~ NA,
    H1TO15 == 1 | H1TO15 == 2 | H1TO15 == 3 | H1TO15 == 4 | H1TO15 == 5 | H1TO15 == 6 ~ 1,
    H1TO15 == 7 | H1TO15 == 97 ~ 0,
    TRUE ~ H1TO15
  ))

## Anyone reporting no past-year alcohol use is automatically given a problematic alcohol score of 0 based on responses to H1TO20-28
## For the remaining participants (i.e., consumed alcohol in last year), we want to sum scores based on responses to H1TO20-28

## H1To20: "During the past 12-months how often...did you get into trouble with your parents because you had been drinking?"
## C:  0 = never; 1 = once; 2 = twice; 3 = 3-4 times; 4 = 5 or more times
## M: 6 = refused, 8 = don't know, 9 = not applicable
addh <- addh %>%
  mutate(alcprob_v1 = case_when(
    is.na(pastyear_drink) | H1TO20 == 6 | H1TO20 == 8 | H1TO20 == 9 ~ NA,
    H1TO20 == 7 ~ 0, 
    TRUE ~ H1TO20
  ))
table(addh$alcprob_v1) 
## H1To21: "During the past 12-months how often...have you had problems with school or work because you had been drinking?"
## C: 0 = never; 1 = once; 2 = twice; 3 = 3-4 times; 4 = 5 or more times
## M: 6 = refused, 8 = don't know, 9 = not applicable
addh <- addh %>%
  mutate(alcprob_v2 = case_when(
    is.na(pastyear_drink) | H1TO21 == 6 | H1TO21 == 8 | H1TO21 == 9 ~ NA,
    H1TO21 == 7 ~ 0, 
    TRUE ~ H1TO21
  ))
table(addh$alcprob_v2) 
## H1To22: "During the past 12-months how often...you had problems with friends because you had been drinking?"
## C: 0 = never; 1 = once; 2 = twice; 3 = 3-4 times; 4 = 5 or more times
## M: 6 = refused, 8 = don't know, 9 = not applicable  
addh <- addh %>%
  mutate(alcprob_v3 = case_when(
    is.na(pastyear_drink) | H1TO22 == 6 | H1TO22 == 8 | H1TO22 == 9 ~ NA,
    H1TO22 == 7 ~ 0, 
    TRUE ~ H1TO22
  ))
table(addh$alcprob_v3) 
## H1To23: "During the past 12-months how often...you had problems with someone you were dating because you had been drinking?"
## C: 0 = never; 1 = once; 2 = twice; 3 = 3-4 times; 4 = 5 or more times
## M: 6 = refused, 8 = don't know, 9 = not applicable  
addh <- addh %>%
  mutate(alcprob_v4 = case_when(
    is.na(pastyear_drink) | H1TO23 == 6 | H1TO23 == 8 | H1TO23 == 9 ~ NA,
    H1TO23 == 7 ~ 0, 
    TRUE ~ H1TO23
  ))
table(addh$alcprob_v4) 
## H1To24: "During the past 12-months how often...you did something you later regretted because you had been drinking?"
## C: 0 = never; 1 = once; 2 = twice; 3 = 3-4 times; 4 = 5 or more times
## M: 6 = refused, 8 = don't know, 9 = not applicable
addh <- addh %>%
  mutate(alcprob_v5 = case_when(
    is.na(pastyear_drink) | H1TO24 == 6 | H1TO24 == 8 | H1TO24 == 9 ~ NA,
    H1TO24 == 7 ~ 0, 
    TRUE ~ H1TO24
  ))
table(addh$alcprob_v5) # check against data dictionary 
## H1To25: "During the past 12-months how often...were you hungover?"
## C: 0 = never; 1 = once; 2 = twice; 3 = 3-4 times; 4 = 5 or more times
## M: 6 = refused, 8 = don't know, 9 = not applicable
addh <- addh %>%
  mutate(alcprob_v6 = case_when(
    is.na(pastyear_drink) | H1TO25 == 6 | H1TO25 == 8 | H1TO25 == 9 ~ NA,
    H1TO25 == 7 ~ 0, 
    TRUE ~ H1TO25
  ))
table(addh$alcprob_v6) 
## H1To26: "During the past 12-months how often...were you sick to your stomach or threw up after drinking?"
## C: 0 = never; 1 = once; 2 = twice; 3 = 3-4 times; 4 = 5 or more times
## M: 6 = refused, 8 = don't know, 9 = not applicable    
addh <- addh %>%
  mutate(alcprob_v7 = case_when(
    is.na(pastyear_drink) | H1TO26 == 6 | H1TO26 == 8 | H1TO26 == 9 ~ NA,
    H1TO26 == 7 ~ 0, 
    TRUE ~ H1TO26
  ))
table(addh$alcprob_v7) 
## H1To27: "During the past 12-months how often...did you get into a sexual situation you later regretted because you had been drinking?"
## C: 0 = never; 1 = once; 2 = twice; 3 = 3-4 times; 4 = 5 or more times
## M: 6 = refused, 8 = don't know, 9 = not applicable   
addh <- addh %>%
  mutate(alcprob_v8 = case_when(
    is.na(pastyear_drink) | H1TO27 == 6 | H1TO27 == 8 | H1TO27 == 9 ~ NA,
    H1TO27 == 7 ~ 0, 
    TRUE ~ H1TO27
  ))
table(addh$alcprob_v8) 
## H1To28: "During the past 12-months how often...did you get into a physical fight because you had been drinking?"
## C: 0 = never; 1 = once; 2 = twice; 3 = 3-4 times; 4 = 5 or more times
## M: 6 = refused, 8 = don't know, 9 = not applicable 
addh <- addh %>%
  mutate(alcprob_v9 = case_when(
    is.na(pastyear_drink) | H1TO28 == 6 | H1TO28 == 8 | H1TO28 == 9 ~ NA,
    H1TO28 == 7 ~ 0, 
    TRUE ~ H1TO28
  ))  
table(addh$alcprob_v9) 

## Now summing to create variable of total score on problematic alcohol consumption
addh <- addh %>%
  mutate(alcprob_score = rowSums(across(starts_with("alcprob_v")), na.rm = FALSE))

## Check total score (min = 0, max = 36)
summary(addh$alcprob_score)
sum(is.na(addh$alcprob_score)) 

#------------------------------#

## Variable: Other drug use 
## Relevant variables: H1TO34, H1TO37, H1TO40
## Q (H1TO34): "How old were you when you tried any kind of cocaine - including powder, freebase or crack cocaine - for the first time?"
## Q (H1TO37): "How old were you when you tried inhalants, such as glue or solvents, for the first time?"
## Q (H1TO40): "How old were you when you first tried any other type of illegal drug, such as LSD, PCP, ecstasy, mushrooms, speed, ice, heroin or pills, without a doctor's prescription?"
## C (ALL): 0 = never used, 1-18 = age range
## M (ALL): '.' = missing, 96 = refused, 98 = don't know, 99 = not applicable

addh <- addh %>%
  mutate(illegaldrug_v1 = case_when(
    H1TO34 == 96 | H1TO34 == 98 | H1TO34 == 99 | H1TO34 == "." ~ NA,
    H1TO34 >=1 & H1TO34 <=18 ~1,
    TRUE ~ H1TO34
  ))  
addh <- addh %>%
  mutate(illegaldrug_v2 = case_when(
    H1TO37 == 96 | H1TO37 == 98 | H1TO37 == 99 | H1TO37 == "." ~ NA,
    H1TO37 >=1 & H1TO37 <=18 ~1,
    TRUE ~ H1TO37
  )) 
addh <- addh %>%
  mutate(illegaldrug_v3 = case_when(
    H1TO40 == 96 | H1TO40 == 98 | H1TO40 == 99 | H1TO40 == "." ~ NA,
    H1TO40 >=1 & H1TO40 <=18 ~1,
    TRUE ~ H1TO40
  )) 
## Now combine information to create overall indicator of lifetime illegal drug use 
addh <- addh %>%
  mutate(ever_drug = case_when(
    illegaldrug_v1 == 1 | illegaldrug_v2 == 1 | illegaldrug_v3 == 1 ~ 1,
    illegaldrug_v1 == 0 & illegaldrug_v2 == 0 & illegaldrug_v3 == 0 ~ 0,
    TRUE ~NA
  ),
  ever_drug = factor(
    ever_drug,
    levels = c(0, 1),
    labels = c("No", "Yes"),
  ))

table(addh$ever_drug)
sum(is.na(addh$ever_drug)) 

#------------------------------#

## Variable: Poor sleep 
## Relevant variable(s): H1GH52
## Q: "Do you usually get enough sleep?"
## C: 0 = no; 1 = yes
## M: 6 = refused; 8 = don't know

addh <- addh %>%
  mutate(sleep_problems = case_when(
    H1GH52 == 6 | H1GH52 == 8 ~ NA,
    H1GH52 == 1 ~ 0,
    H1GH52 == 0 ~ 1
  ),
  sleep_problems = factor(
    sleep_problems,
    levels = c(0, 1),
    labels = c("No", "Yes"),
  ))

table(addh$sleep_problems)
sum(is.na(addh$sleep_problems)) 


#------------------------------#

## Variable: Neuroticism 
## Relevant variable(s): H1PF30, H1PF32, H1PF33, H1PF34, H1PF35, H1PF36
## Q (H1PF30): "Do you agree or disagree with the following statement? You have a lot of good qualities"
## Q (H1PF32): "Do you agree or disagree with the following statement? You have a lot to be proud of"
## Q (H1PF33): "Do you agree or disagree with the following statement? You like yourself just the way you are"
## Q (H1PF34): "Do you agree or disagree with the following statement? You feel like you are doing everything just about right"   
## Q (H1PF35): "Do you agree or disagree with the following statement? You feel socially accepted"   
## Q (H1PF36): "Do you agree or disagree with the following statement? You feel loved and wanted"
## C (ALL): 1 = strongly agree, 2 = agree, 3 = neither agree nor disagree, 4 = disagree, 5 = strongly disagree
## M (ALL): 6 = refused, 8 = don't know
## Higher scores represent higher neuroticism (i.e., lower emotional stability)

addh <- addh %>%
  mutate(neuro_v1 = case_when(
    H1PF30 == 6 | H1PF30 == 8 ~ NA,
    TRUE ~ H1PF30
  ))  
addh <- addh %>%
  mutate(neuro_v2 = case_when(
    H1PF32 == 6 | H1PF32 == 8 ~ NA,
    TRUE ~ H1PF32
  ))  
addh <- addh %>%
  mutate(neuro_v3 = case_when(
    H1PF33 == 6 | H1PF33 == 8 ~ NA,
    TRUE ~ H1PF33
  ))  
addh <- addh %>%
  mutate(neuro_v4 = case_when(
    H1PF34 == 6 | H1PF34 == 8 ~ NA,
    TRUE ~ H1PF34
  ))  
addh <- addh %>%
  mutate(neuro_v5 = case_when(
    H1PF35 == 6 | H1PF35 == 8 | H1PF35 == 9 ~ NA,
    TRUE ~ H1PF35
  ))  
addh <- addh %>%
  mutate(neuro_v6 = case_when(
    H1PF36 == 6 | H1PF36 == 8 | H1PF36 == 9 ~ NA,
    TRUE ~ H1PF36
  ))  

## Summing to create overall neuroticism score
addh <- addh %>%
  mutate(neuroticism_score = rowSums(across(starts_with("neuro_v")), na.rm = FALSE))

## Check total score (min = 6, max = 30)
summary(addh$neuroticism_score) 

#------------------------------#

## Variable: Anxiety 
## Relevant variable(s): H1GH17, H1GH18, H1GH19, H1GH20, H1GH21, H1GH22
## Q (H1GH17): "In the past 12 months, how often have you had poor appetite?"
## Q (H1GH18): "In the past 12 months, how often have you had trouble falling asleep or staying asleep?"
## Q (H1GH19): "In the past 12 months, how often have you had trouble relaxing?"
## Q (H1GH20): "In the past 12 months, how often have you been moody?"
## Q (H1GH21): "In the past 12 months, how often have you cried frequently?"   
## Q (H1GH22): "In the past 12 months, how often have you felt fearful?"
## C (ALL): 0 = never, 1 = just a few times, 2 = about once a week, 3 = almost every day, 4 = every day
## M (ALL): 6 = refused, 8 = don't know
## Higher scores represent higher anxiety symptoms

addh <- addh %>%
  mutate(anx_v1 = case_when(
    H1GH17 == 6 | H1GH17 == 8 ~ NA,
    TRUE ~ H1GH17
  ))  
addh <- addh %>%
  mutate(anx_v2 = case_when(
    H1GH18 == 6 | H1GH18 == 8 ~ NA,
    TRUE ~ H1GH18
  ))  
addh <- addh %>%
  mutate(anx_v3 = case_when(
    H1GH19 == 6 | H1GH19 == 8 ~ NA,
    TRUE ~ H1GH19
  ))  
addh <- addh %>%
  mutate(anx_v4 = case_when(
    H1GH20 == 6 | H1GH20 == 8 ~ NA,
    TRUE ~ H1GH20
  ))  
addh <- addh %>%
  mutate(anx_v5 = case_when(
    H1GH21 == 6 | H1GH21 == 8 ~ NA,
    TRUE ~ H1GH21
  ))  
addh <- addh %>%
  mutate(anx_v6 = case_when(
    H1GH22 == 6 | H1GH22 == 8 ~ NA,
    TRUE ~ H1GH22
  ))  

## Summing to create overall anxiety score
addh <- addh %>%
  mutate(anxiety_symptoms = rowSums(across(starts_with("anx_v")), na.rm = FALSE))

## Check total score (min = 0, max = 24)
summary(addh$anxiety_symptoms) 

#------------------------------#

## Variable: Conduct disorder 
## Relevant variable(s): H1DS9, H1DS10, H1DS8, H1DS2, H1CO10 (+ H1CO1), H1FV7, H1FV8, H1DS11, H1DS6, H1DS7, H1DS3, H1ED2 (+H1GI18), H1DS5

## Stealing: H1DS9
addh <- addh %>%
  mutate(cd_v1 = case_when(
    H1DS9 == 6 | H1DS9 == 8 | H1DS9 == 9 ~ NA,
    H1DS9 >=1 ~ 1,
    H1DS9 < 1 ~ 0
  )) 
table(addh$cd_v1)
sum(is.na(addh$cd_v1))

## Breaking into something: H1DS10 or H1DS8
addh <- addh %>%
  mutate(cd_v2 = case_when(
    H1DS10 == 6 | H1DS10 == 8 | H1DS10 == 9 ~ NA,
    H1DS10 >=1 ~ 1,
    H1DS10 < 1 ~ 0
  )) 
table(addh$cd_v2)
sum(is.na(addh$cd_v2))

addh <- addh %>%
  mutate(cd_v3 = case_when(
    H1DS8 == 6 | H1DS8 == 8 | H1DS8 == 9 ~ NA,
    H1DS8 >=1 ~ 1,
    H1DS8 < 1 ~ 0
  )) 
table(addh$cd_v3)
sum(is.na(addh$cd_v3))

## Damage to property: H1DS2
addh <- addh %>%
  mutate(cd_v4 = case_when(
    H1DS2 == 6 | H1DS2 == 8 | H1DS2 == 9 ~ NA,
    H1DS2 >=1 ~ 1,
    H1DS2 < 1 ~ 0
  )) 
table(addh$cd_v4)
sum(is.na(addh$cd_v4))

## Forced sexual activity: H1CO1 + H1CO10 (only males)  
addh <- addh %>%
  mutate(ever_sex = case_when(
    H1CO1 == 6 | H1CO1 == 8 | H1CO1 == 9 ~ NA,
    H1CO1 >=1 ~ 1,
    H1CO1 < 1 ~ 0
  )) 
table(addh$ever_sex)
sum(is.na(addh$ever_sex))

addh <- addh %>%
  mutate(cd_v5 = case_when(
    bio_sex == "Female" ~ 0,
    H1CO1 == 0 ~ 0,
    H1CO10 == 6 | H1CO10 == 8 | H1CO10 == 9 ~ NA,
    H1CO1 ==1 ~ 1
  )) 
table(addh$cd_v5)
sum(is.na(addh$cd_v5))

## Use of a weapon: H1FV7, H1FV8
addh <- addh %>%
  mutate(cd_v6 = case_when(
    H1FV7 == 6 | H1FV7 == 8 | H1FV7 == 9 ~ NA,
    H1FV7 >=1 ~ 1,
    H1FV7 < 1 ~ 0
  )) 
table(addh$cd_v6)
sum(is.na(addh$cd_v6))

addh <- addh %>%
  mutate(cd_v7 = case_when(
    H1FV8 == 6 | H1FV8 == 8 | H1FV8 == 9 ~ NA,
    H1FV8 >=1 ~ 1,
    H1FV8 < 1 ~ 0
  )) 
table(addh$cd_v7)
sum(is.na(addh$cd_v7))

## Stealing with confrontation: H1DS11
addh <- addh %>%
  mutate(cd_v8 = case_when(
    H1DS11 == 6 | H1DS11 == 8 | H1DS11 == 9 ~ NA,
    H1DS11 >=1 ~ 1,
    H1DS11 < 1 ~ 0
  )) 
table(addh$cd_v8)
sum(is.na(addh$cd_v8))

## Physically harming others: H1DS6
addh <- addh %>%
  mutate(cd_v9 = case_when(
    H1DS6 == 6 | H1DS6 == 8 | H1DS6 == 9 ~ NA,
    H1DS6 >=1 ~ 1,
    H1DS6 < 1 ~ 0
  )) 
table(addh$cd_v9)
sum(is.na(addh$cd_v9))

## Running away from home: H1DS7 
addh <- addh %>%
  mutate(cd_v10 = case_when(
    H1DS7 == 6 | H1DS7 == 8 | H1DS7 == 9 ~ NA,
    H1DS7 >=2 ~ 1,
    H1DS7 <=1 ~ 0
  )) 
table(addh$cd_v10)
sum(is.na(addh$cd_v10))

## Frequent lying: H1DS3
addh <- addh %>%
  mutate(cd_v11 = case_when(
    H1DS3 == 6 | H1DS3 == 8 | H1DS3 == 9 ~ NA,
    H1DS3 ==3 ~ 1,
    H1DS3 <=2 ~ 0
  )) 
table(addh$cd_v11)
sum(is.na(addh$cd_v11))

## Truancy: H1ED2 (+H1GI18)
addh <- addh %>%
  mutate(cd_v12 = case_when(
    H1GI18 == 0 ~ 0, # Determining legitmtate skips (i.e., not in school in past year)
    H1GI18 == 6 | H1GI18 == 8 | H1ED2 == 996 | H1ED2 == 998 | H1ED2 == 999 | H1ED2 == 997 ~ NA,
    H1ED2 >=10 ~ 1,
    H1ED2 <10 ~ 0
  )) 
table(addh$cd_v12)
sum(is.na(addh$cd_v12))

## Physical fighting: H1DS5
addh <- addh %>%
  mutate(cd_v13 = case_when(
    H1DS3 == 6 | H1DS3 == 8 | H1DS3 == 9 ~ NA,
    H1DS3 >=2 ~ 1,
    H1DS3 <=1 ~ 0
  )) 
table(addh$cd_v13)
sum(is.na(addh$cd_v13))

## Summing to create total CD score
addh <- addh %>%
  mutate(cd_score = rowSums(across(starts_with("cd_v")), na.rm = FALSE))         

## Check total score (min = 0, max = 13)
summary(addh$cd_score) 

## ================== Prepare T1 variables ====================================

## Variable: Depression @ follow-up (CESD-10)
## Relevant variable(s): H2FS1, H2FS3, H2FS4*, H2FS5, H2FS6, H2FS7, H2FS11*, H2FS15*, H2FS16, H2FS17
## Three items need reverse coding: H2FS4, H2FS11, H1FS15
## Q (H2FS4): "How often was the following true during the past week? You felt you were just as good as other people"
## Q (H2FS11): "How often was the following true during the past week? You were happy"
## Q (H2FS15): "How often was the following true during the past week? You enjoyed life"
## C (ALL): 0 = never of rarely; 1 = sometimes; 2 = a lot of the time; 3 = most of the time or all of the time
## M (ALL): 6 = refused; 8 = don't know; 9 = n/a

## Looping for initial data prep
dep_variables <- paste0("H2FS",1:19)
addh <- addh%>%
  mutate(across(all_of(dep_variables),
                ~ case_when (
                  . == 6 | . == 8 | . == 9 ~ NA_real_,
                  TRUE ~ .
                ),
                .names = "dep_v{col}"))

## Reverse coding H2FS4, H2FS11, and H2FS15
max_value <- 3
addh <- addh%>%
  mutate(
    dep_vH2FS4 = max_value - dep_vH2FS4, #Reverse coding for H2FS4
    dep_vH2FS11 = max_value - dep_vH2FS11, #Reverse coding for H2FS11
    dep_vH2FS15 = max_value - dep_vH2FS15 # Reverse coding for H2FS15
  )

## Summing to create total depression score
addh <- addh%>%
  mutate(total_cesd_fu = rowSums(addh[, c("dep_vH2FS1", "dep_vH2FS3", "dep_vH2FS4",
                                          "dep_vH2FS5", "dep_vH2FS6", "dep_vH2FS7", "dep_vH2FS11",
                                          "dep_vH2FS15", "dep_vH2FS16", "dep_vH2FS17")], 
                                 na.rm = FALSE))    

## Check total score (min = 0, max = 27)
summary(addh$total_cesd_fu)
sum(is.na(addh$total_cesd_fu))

## Cut-off of >=11 
addh <- addh %>%
  mutate(fu_dep = case_when(
    is.na(total_cesd_fu) ~ NA_real_,  # Use NA_real_ for proper handling of NA in factor
    total_cesd_fu < 11 ~ 0,
    total_cesd_fu >= 11 ~ 1
  )) %>%
  mutate(fu_dep = factor(fu_dep, levels = c(0, 1), labels = c("Below 11", "Above or Equal 11")))
table(addh$fu_dep)
sum(is.na(addh$fu_dep)) 

## ================== Prepare  WII variables ====================================

## Variable: ADHD
## Relevant variable(s): H3RA1 - H3RA18
## All questions ask about behaviour between ages 5-12 
## C (ALL): 0 = never of rarely; 1 = sometimes; 2 = often; 3 = very often; 
## M (ALL): 6 = refused; 8 = don't know; 9 = not applicable

## Looping for initial data prep
## Categorizing often or very often as (1) and sometimes or rarely as (0)
adhd_variables <- paste0("H3RA",1:18)
addh <- addh%>%
  mutate(across(all_of(adhd_variables),
                ~ case_when (
                  . == 6 | . == 8 | . == 9 ~ NA_real_,
                  . >=2 ~1,
                  . <2 ~0,
                  TRUE ~ .
                ),
                .names = "adhd_v{col}"))

## Threshold = 6 or more symptoms on inattentive or hyperactive subscales
## Only H3RA18 doesn't match DSM criteria = spiteful or vindictive and dropped from variable
## Inattentive = >=6 from H3RA1, H3RA3, H3RA5, H3RA7, H3RA9, H3RA11, H3RA13, H3RA15, H3RA17
## Hyperactive = >=6 from H3RA2, H3RA4, H3RA6, H3RA8, H3RA10, H3RA12, H3RA14, H3RA16, (H3R18)
## Combined = >=6 of each type (i.e., at least 12 symptoms in total)

addh$inatt_scale <- rowSums(addh[, c("adhd_vH3RA1", "adhd_vH3RA3", "adhd_vH3RA5",
                                     "adhd_vH3RA7", "adhd_vH3RA9", "adhd_vH3RA11",
                                     "adhd_vH3RA13", "adhd_vH3RA15", "adhd_vH3RA17")],
                            na.rm = FALSE)
table(addh$inatt_scale)
sum(is.na(addh$inatt_scale))

addh$hypa_scale <- rowSums(addh[, c("adhd_vH3RA2", "adhd_vH3RA4", "adhd_vH3RA6",
                                    "adhd_vH3RA8", "adhd_vH3RA10", "adhd_vH3RA12",
                                    "adhd_vH3RA14", "adhd_vH3RA16")],
                           na.rm = FALSE)
table(addh$hypa_scale)
sum(is.na(addh$hypa_scale)) 

addh <- addh %>%
  mutate(adhd_dx = case_when(
    inatt_scale >= 6 & hypa_scale >= 6 ~ 1,
    inatt_scale >= 6 & hypa_scale < 6 ~ 1, 
    hypa_scale >= 6 & inatt_scale < 6 ~ 1,  
    is.na(inatt_scale) | is.na(hypa_scale) ~ NA_real_,
    TRUE ~ 0
  )) %>%
  mutate(adhd_dx = factor(adhd_dx, levels = c(0, 1), labels = c("None", "ADHD")))

table(addh$adhd_dx)
sum(is.na(addh$adhd_dx)) 

## ================== Prepare WIV + (W1, II, III) variables ====================================

##Variable: ACEs
## Relevant variable(s): H3MA1, H3MA2, H4MA1 (+H4MA2), H4MA3 (+H4MA4), H4MA5 (+H4MA6),
## ...H1SU6, PC49E_2, PC49E_3, H4WP4 (+H4WP6), H4WP9, (+H4WP11), H4WP16, (+H4WP18), H4WP30, (+H4WP32), H1NM3, H1NF3
## All questions ask about behaviour between ages 5-18, date indicators used where applicable

## Neglect (H3MA1, H3MA2)
## Q (H3MA1): "By the time you started 6th grade, how often had your parents or other adult care-givers left you home alone when an adult should have been with you?"
## Q (H3MA2): "How often had your parents or other adult care-givers not taken care of your basic needs, such as keeping you clean or providing food or clothing?"
## C (ALL): 1 = once, 2 = twice, 3 = 3-5 times, 4 = 6-10 times, 5 = 10+ times, 6 = never happened
## M (ALL): '.' = missing, 96 = refused, 98 = don't know, 99 = n/a

addh <- addh %>%
  mutate(neg_1 = case_when(
    H3MA1 == '.' | H3MA1 == 96 | H3MA1 == 98 | H3MA1 == 99 ~ NA,
    H3MA1 == 6 ~ 0,
    H3MA1 >=1 ~ 1
  ))
table(addh$neg_1)
sum(is.na(addh$neg_1))

addh <- addh %>%
  mutate(neg_2 = case_when(
    H3MA2 == '.' | H3MA2 == 96 | H3MA2 == 98 | H3MA2 == 99 ~ NA,
    H3MA2 == 6 ~ 0,
    H3MA2 >=1 ~ 1
  ))
table(addh$neg_2)
sum(is.na(addh$neg_2))

addh <- addh %>%
  mutate(neglect = case_when(
    # If any neglect variable indicates neglect, set score to 1
    neg_1 == 1 | neg_2 == 1 ~ 1,
    
    # If either variable is NA, set score to NA
    is.na(neg_1) | is.na(neg_2) ~ NA_real_,
    
    # If both variables indicate no neglect, set score to 0
    neg_1 == 0 & neg_2 == 0 ~ 0,
    
    # If other cases occur, keep it NA
    TRUE ~ NA_real_
  ))
table(addh$neglect)
sum(is.na(addh$neglect))

## Emotional abuse (H4MA1 (+H4MA2))
## Q (H4MA1): "Before your 18th birthday, how often did a parent or other adult caregiver say things that really hurt your feelings or made you feel like you were not wanted or loved?"
## C (H4MA1): 1 = once, 2 = twice, 3 = 3-5 times, 4 = 6-10 times, 5 = 10+ times, 6 = never happened
## Q (H4MA2): "How old were you the first time this happened?"
## C (H4MA2): Age range, 0 - 17 years
## M (ALL): 96 = refused, 98 = don't know + 97 = legit skip (H4MA2)

addh <- addh %>%
  mutate(emo_abuse = case_when(
    H4MA1 == 96 | H4MA1 == 98 ~ NA,
    H4MA1 == 6 ~ 0,
    H4MA1 >=1 ~ 1
  ))
table(addh$emo_abuse)
sum(is.na(addh$emo_abuse))

addh <- addh %>%
  mutate(emo_age= case_when(
    H4MA2 == 96 | H4MA2 == 98 | H4MA2 == 97 ~ NA,
    TRUE ~ H4MA2))
summary(addh$emo_age) # Max = 17 years (i.e., at or before baseline, mean = 11.64 years)

## Physical abuse (H4MA3 (+H4MA4))
## Q (H4MA3): "Before your 18th birthday, how often did a parent or adult caregiver hit you with a fist, kick you, or throw you down on the floor, into a wall, or down stairs?"
## C (H4MA3): 1 = once, 2 = twice, 3 = 3-5 times, 4 = 6-10 times, 5 = 10+ times, 6 = never happened
## Q (H4MA4): "How old were you the first time this happened?"
## C (H4MA4): Age range, 0 - 17 years
## M (ALL): 96 = refused, 98 = don't know + 97 = legit skip (H4MA4)

addh <- addh %>%
  mutate(phy_abuse = case_when(
    H4MA3 == 96 | H4MA3 == 98 ~ NA,
    H4MA3 == 6 ~ 0,
    H4MA3 >=1 ~ 1
  ))
table(addh$phy_abuse)
sum(is.na(addh$phy_abuse))

addh <- addh %>%
  mutate(phy_age= case_when(
    H4MA4 == 96 | H4MA4 == 98 | H4MA4 == 97 ~ NA,
    TRUE ~ H4MA4))
summary(addh$phy_age) # Max = 17 years (i.e., at or before baseline, mean = 10.69 years)

## Sexual abuse (H4MA5 (+H4MA6))
## Q (H4MA5): "How often did a parent or other adult caregiver touch you in a sexual way, force you to touch him or her in a sexual way, or force you to have sexual relations?"
## C (H4MA5): 1 = once, 2 = twice, 3 = 3-5 times, 4 = 6-10 times, 5 = 10+ times, 6 = never happened
## Q (H4MA6): "How old were you the first time this happened?"
## C (H4MA6): Age range, 0 - 17 years
## M (ALL): 96 = refused, 98 = don't know + 97 = legit skip (H4MA6)

addh <- addh %>%
  mutate(sex_abuse = case_when(
    H4MA5 == 96 | H4MA5 == 98 ~ NA,
    H4MA5 == 6 ~ 0,
    H4MA5 >=1 ~ 1
  ))
table(addh$sex_abuse)
sum(is.na(addh$sex_abuse))

addh <- addh %>%
  mutate(sex_age= case_when(
    H4MA6 == 96 | H4MA6 == 98 | H4MA6 == 97 ~ NA,
    TRUE ~ H4MA6))
summary(addh$sex_age)  # Max = 17 years (i.e., at or before baseline, mean = 8.06 years)

## FH of suicidal behaviour (H1SU6)
## Q: "Have any of your family members tried to kill themselves during the past 12 months?"
## C: 0 = no; 1 = yes
## M: 6 = refused, 8 = don't know, 9 = n/a

addh <- addh %>%
  mutate(fh_suicide = case_when(
    H1SU6 == 6 | H1SU6 == 8 | H1SU6 == 9 ~ NA,
    TRUE ~ H1SU6))
table(addh$fh_suicide)
sum(is.na(addh$fh_suicide))

## Parental alcohol misuse (PC49E_2, PC49E_3)
## Q (PC49E_2): "Does [NAME]'s biological mother currently have the following health problem: Alcoholism?"
## Q (PC49E_3): "Does [NAME]'s biological father currently have the following health problem: Alcoholism?"
## C (ALL): 0 = no, 1 = yes
## M (ALL): '.' = missing, 6 = refused, 8 = don't know

addh <- addh %>%
  mutate(mum_alcohol = case_when(
    PC49E_2 == '.' | PC49E_2 == 6 | PC49E_2 == 8 ~ NA,
    TRUE ~ PC49E_2))
table(addh$mum_alcohol)
sum(is.na(addh$mum_alcohol))

addh <- addh %>%
  mutate(dad_alcohol = case_when(
    PC49E_3 == '.' | PC49E_3 == 6 | PC49E_3 == 8 ~ NA,
    TRUE ~ PC49E_3))
table(addh$dad_alcohol)
sum(is.na(addh$dad_alcohol))

addh <- addh %>%
  mutate(parent_alcohol = case_when(
    # If either parent reports alcohol use, set score to 1
    mum_alcohol == 1 | dad_alcohol == 1 ~ 1,
    
    # If either variable is NA, set score to NA
    is.na(mum_alcohol) | is.na(dad_alcohol) ~ NA_real_,
    
    # If both parents indicate no alcohol use, set score to 0
    mum_alcohol == 0 & dad_alcohol == 0 ~ 0,
    
    # Catch-all for any other cases, set to NA
    TRUE ~ NA_real_
  ))
table(addh$parent_alcohol)
sum(is.na(addh$parent_alcohol))

## Parental incarceration [H4WP3 (+H4WP5), H4WP9, (+H4WP11), H4WP16, (+H4WP18), H4WP30, (+H4WP32)]
## Q (H4WP3,9, 16, 30): "How many times (has/did) your ________ (spent/spend) time in jail or prison?"...
## ...asks across bio mother, bio father, mother figure and father figure
## C (H4WP3,9, 16, 30): 0 = no, 1 = yes
## M (H4WP3,9, 16, 30): 6 = refused, 8 = don't know
## M (H4WP16 & 30): 7 = legitimate skip (i.e., raised by bio father or bio mother)
## Q (H4WP5, 11, 18, 32): "How old were you when your ________ went to jail or prison (the first time)?"...
## ...asks across bio mother, bio father, mother figure and father figure
## C (H4WP4,9, 16, 30): 0 = no, 1 = yes
## C (H4WP5, 11, 18, 32): Age range (0  - 31), 94 = not yet born
## M (H4WP5, 11, 18, 32): 96 = refused, 98 = don't know
## M (H4WP16 & 30): 97 = legitimate skip (i.e., didn't go to jail or not raised by figure)
## Info: https://www.norcalbiostat.com/data/AddHealth_Wave_IV.pdf

# Biological mum was in jail
addh <- addh %>% 
  mutate(biomum_jail = case_when(
    H4WP3 == 6 |  H4WP3 == 8~ NA,
    TRUE ~ H4WP3
  ))
table(addh$biomum_jail)
sum(is.na(addh$biomum_jail))

# Biological mum was in jail before age @ BL
addh <- addh %>%
  mutate(bm_jail18 = case_when(
    is.na(biomum_jail) | H4WP5 == 98 | H4WP5 == 96 ~ NA,
    H4WP5 == 97 ~ 0,
    H4WP5 == 94 | H4WP5 < age_in_years ~ 1,
    H4WP5 > age_in_years ~ 0
  ))
table(addh$bm_jail18)
sum(is.na(addh$bm_jail18))

# Biological dad was in jail
addh <- addh %>% 
  mutate(biodad_jail = case_when(
    H4WP9 == 6 |  H4WP9 == 8~ NA,
    TRUE ~ H4WP9
  ))
table(addh$biodad_jail)
sum(is.na(addh$biodad_jail))

# Biological dad was in jail before age @ BL
addh <- addh %>%
  mutate(bd_jail18 = case_when(
    is.na(biodad_jail) | H4WP11 == 98 | H4WP11 == 96 ~ NA,
    H4WP11 == 97 ~ 0,
    H4WP11 == 94 | H4WP11 < age_in_years ~ 1,
    H4WP11 > age_in_years ~ 0
  ))
table(addh$bd_jail18)
sum(is.na(addh$bd_jail18))

# Mother figure was in jail
addh <- addh %>% 
  mutate(mumfig_jail = case_when(
    H4WP16 == 8 ~ NA,
    H4WP16 == 7 | H4WP16 == 0 ~ 0,
    H4WP16 == 1 ~ 1
  ))
table(addh$mumfig_jail)
sum(is.na(addh$mumfig_jail))

# Mother figure was in jail before age @ BL
addh <- addh %>%
  mutate(mf_jail18 = case_when(
    is.na(mumfig_jail) | H4WP18 == 98 | H4WP18 == 96 ~ NA,
    H4WP18 == 97 ~ 0,
    H4WP18 == 94 | H4WP18 < age_in_years ~ 1,
    H4WP18 > age_in_years ~ 0
  ))
table(addh$mf_jail18)
sum(is.na(addh$mf_jail18))

# Father figure was in jail
addh <- addh %>% 
  mutate(dadfig_jail = case_when(
    H4WP30 == 6 |H4WP30 == 8 ~ NA,
    H4WP30 == 7 | H4WP30 == 0 ~ 0,
    H4WP30 == 1 ~ 1
  ))
table(addh$dadfig_jail)
sum(is.na(addh$dadfig_jail))

# Father figure was in jail before age @ BL
addh <- addh %>%
  mutate(ff_jail18 = case_when(
    is.na(dadfig_jail) | H4WP32 == 98 | H4WP32 == 96 ~ NA,
    H4WP32 == 97 ~ 0,
    H4WP32 == 94 | H4WP32 < age_in_years ~ 1,
    H4WP32 > age_in_years ~ 0
  ))
table(addh$ff_jail18)
sum(is.na(addh$ff_jail18))

## Summing for any parental incarceration before age @ BL
addh <- addh %>%
  mutate(
    parental_incarc= case_when(
      
      # Check if all values are NA, set score to NA
      is.na(bm_jail18) & is.na(bd_jail18) & is.na(mf_jail18) & is.na(ff_jail18) ~ NA_real_,
      
      # Count if any of the parents have been incarcerated
      (bm_jail18 == 1 | bd_jail18 == 1 | mf_jail18 == 1 | ff_jail18 == 1) ~ 1,
      
      # Check for cases where no incarceration is reported and there are no NAs
      (bm_jail18 == 0 & bd_jail18 == 0 & mf_jail18 == 0 & ff_jail18 == 0) ~ 0,
      
      # If any of the values are NA, set score to NA
      TRUE ~ NA_real_
    )
  )
table(addh$parental_incarc)
sum(is.na(addh$parental_incarc))

## Parental death (H1NM3, H1NF3)
## Q (H1NM3): "How old were you when she [biological mother] died?"
## Q (H1NF3): "How old were you when he [biological father] died?"
## C (ALL): Continuous (0-17 age range)
## M (ALL): 96 = refused, 98 = don't know; 97 = legitimate skip (i.e., hasn't died)

# Biological mum has died
addh <- addh %>% 
  mutate(biomum_dead = case_when(
    H1NM3 == 96 |  H1NM3 == 98~ NA,
    H1NM3 == 97 ~ 0,
    H1NM3 >= 0 ~ 1
  ))
table(addh$biomum_dead)
sum(is.na(addh$biomum_dead))

# Biological dad has died
addh <- addh %>% 
  mutate(biodad_dead = case_when(
    H1NF3 == 96 |  H1NF3 == 98~ NA,
    H1NF3 == 97 ~ 0,
    H1NF3 >= 0 ~ 1
  ))
table(addh$biodad_dead)
sum(is.na(addh$biodad_dead))

## Summing for any parental death before T0
addh <- addh %>%
  mutate(
    parent_death= case_when(
      # Check if all values are NA, set score to NA
      is.na(biodad_dead) & is.na(biomum_dead) ~ NA_real_,
      
      # Count if any of the parents have been incarcerated
      (biodad_dead == 1 | biomum_dead == 1) ~ 1,
      
      # Check for cases where no incarceration is reported and there are no NAs
      (biodad_dead == 0 & biomum_dead == 0) ~ 0,
      
      # If any of the values are NA, set score to NA
      TRUE ~ NA_real_
    )
  )
table(addh$parent_death)
sum(is.na(addh$parent_death))

#------- Create ACE sum score ---------#

## Approach to align with total n = 7 indicators in ALSPAC = dropping parental death

addh <- addh %>%
  mutate(ace_sum = rowSums(select(., neglect, emo_abuse, phy_abuse, sex_abuse, 
                                  fh_suicide, parent_alcohol, parental_incarc), 
                           na.rm = TRUE),  # Sum the scores, ignoring NA values
         ace_count = rowSums(!is.na(select(., neglect, emo_abuse, phy_abuse, sex_abuse, 
                                           fh_suicide, parent_alcohol, parental_incarc))) # Count non-NA values
  )

## Setting criteria for valid scores, as >50% of indicators present
addh <- addh %>%
  mutate(ace_score = case_when(
    ace_count >= 4 ~ ace_sum,    # If at least 4/7 indicators are present, keep the sum
    TRUE ~ NA_real_              # Otherwise, set to NA
  ))
table(addh$ace_score)

## Collapsing to 0, 1, 2, 3, 4+
addh <- addh %>%
  mutate(ace_score = case_when(
    is.na(ace_score) ~ NA,
    ace_score == 0 ~ 0,
    ace_score == 1 ~ 1,
    ace_score == 2 ~ 2,
    ace_score == 3 ~ 3,
    ace_score >=4 ~ 4
  ), 
  ace_score = factor(ace_score,
                     levels = c(0,1,2,3,4),
                     labels = c("0", "1", "2", "3", "4+")))
table(addh$ace_score)
sum(is.na(addh$ace_score)) 

## ================== Sample restrictions ======================================

## Creating indicator of people ages 16-17 at baseline (1 = 16 - 17, 0 = all other ages)
addh <- addh %>%
  mutate(age_1617 = ifelse(age_in_years >= 16 & age_in_years < 18, 1, 0)) #16 - 17 years
table(addh$age_1617) 

## ================== Follow-up length =========================================

## Calculating follow-up length ##
## Relevant variables = IMONTH, IYEAR, IMONTH2, IYEAR2

# Calculate the interview date @ w1 in months (year * 12 + month)
addh['w1_months'] = (addh['IYEAR']*12) + (addh['IMONTH'])

# Calculate the interview date @ w2 in months (year * 12 + month)
addh['w2_months'] = (addh['IYEAR2'] * 12) + addh['IMONTH2']

# Calculate f/up length by subtracting w1_months from w2_months
addh['fup_months'] = addh['w2_months'] - addh['w1_months']

# Calculate f/up length in years by dividing the f/up length in months by 12
addh['fup_years'] = addh['fup_months'] / 12

## Check contents of variables (e.g., nonsense variables, expected range) 
# Some participants with longer f/up than expected, but not an error 
# 91880965, 57192886, 92713509, 93713602, 94713503, 95543707 - maybe pilot data collection? 
summary(addh$fup_months) #median = 11 months
summary(addh$fup_years)

## ================== Generating prepared dataset ==============================

xyz_ah <- addh %>%
  select(AID, PSUSCID.x, REGION.x, GSWGT1, grade_12, age_1617, PSUSCID.y, REGION.y, GSWGT2, # Sample indicators and weights
         ever_smok, freq_smok, ever_cannabis, freq_can, ct_couse, # Exposures
         total_cesd_bl, age_in_years, bio_sex, race_ethnicity, maternal_edu, # Covariates
         alcprob_score, ever_drug, sleep_problems,
         neuroticism_score, anxiety_symptoms, cd_score,
         total_cesd_fu, fu_dep, fup_years, adhd_dx, ace_score,
         vid_game, scl_susp, tch_rel, fam_rel, FAMST2) # Auxiliary 

## ================== End of script ============================================

# Open script called 'addh_imputation.R' 
