'''
---
title: "Cross-cohort analysis cannabis depression"
author: Chloe Burke
dataset: ALSPAC
date: "October 2024"
script: "Cleaning and processing"
---
'''
#install.packages("writexl")
library(writexl)

# Note: Replace [file_path] with your local directory containing the ALSPAC data files

## ================== Prepare T0 variables  ====================================

#-----SAMPLE INDICATORS-----#

## Variable: FOLLOW-UP LENGTH
## Relevant variable(s): ccs9991a, FJ003a
## Q: Age of study child at completion (months) at CCS (baseline) and T4 (follow-up)
# Note: no missing or negative values to handle

# Check against codebook
alspac_t4$ccs9991a #Numeric, any negative value = missing
alspac_t4$FJ003a #Numeric, any negative value = missing

# Calculate follow-up length in months
alspac_t4$followup_months <- alspac_t4$FJ003a - alspac_t4$ccs9991a

# Check the first few rows
head(alspac_t4[, c("ccs9991a", "FJ003a", "followup_months")])

# Check summary statistics
summary(alspac_t4$followup_months) #median = 13 months

## Variable: proxy ID
## Relevant variable(s): cidB3820, qlet

alspac_t4$new_id <- as.numeric(factor(paste0(alspac_t4$cidB3820, "_", alspac_t4$qlet)))
table(alspac_t4$qlet)  # See distribution of A/B
length(unique(alspac_t4$new_id))  # Check number of unique new IDs
head(data.frame(cidB3820=alspac_t4$cidB3820, qlet=alspac_t4$qlet, new_id=alspac_t4$new_id))  # Look at first few rows

#-----AUXILIARY----#

## Variable: Home ownership status
## Relevant variable(s): a006
## Q: "Is your home...e.g., rented/bought?"
## C: 1 = Owned, 2 = Council rented, 3 = Rent privately, 4 = Rent privately, 4 = HA rented, 6 = Other, 9 = DK
## M: -1, NA

# Check against codebook
alspac_t4$a006

# Generate indicator of home ownership
alspac_t4 <- alspac_t4 %>%
  mutate(home_own = case_when(
    a006 <0 ~ NA_real_,
    a006 == 0 ~ 0,
    a006 == 1 ~ 1,
    a006 == 2 ~ 2,
    a006 == 3 ~ 3,
    a006 == 4 ~ 3,
    a006 == 5 ~ 4,
    a006 == 6 ~ 5
  ),
  home_own = factor(home_own,
               levels = c(0, 1, 2, 3, 4, 5),           
               labels = c("Mortgated", "Owned", "Council rented", "Private rented",
                          "HA rented", "Other")))

table(alspac_t4$home_own)
sum(is.na(alspac_t4$home_own)) 

#------------------------------#

## Variable: Deliberate self-harm
## Relevant variable(s): ccs6530
## Q: "Have you ever hurt yourself on purpose in any way?"
## C: 1 = Yes, 2 = No
## M: -1, -10

# Check against codebook
alspac_t4$ccs6530

# Generate indicator of ever deliberate self-harm
alspac_t4 <- alspac_t4 %>%
  mutate(dsh = case_when(
    ccs6530 <0 ~ NA_real_,
    ccs6530 == 1 ~ 1,
    ccs6530 == 2 ~ 0
  ),
  dsh = factor(dsh,
                     levels = c(0, 1),           
                     labels = c("Never", "Ever")))

table(alspac_t4$dsh)
sum(is.na(alspac_t4$dsh)) 

#------------------------------#

## Variable: Urban/rural
## Relevant variable(s): f10ur01ind
## Q: 2001 Census urban/rural indicator; F10 file
## C: 1 = Yes, 2 = No
## M: -1, -10

# Check against codebook
alspac_t4$f10ur01ind

# Generate indicator of urbanicity
alspac_t4 <- alspac_t4 %>%
  mutate(urban = case_when(
    f10ur01ind <0 ~ NA_real_,
    f10ur01ind == 1 ~ 1,
    f10ur01ind == 2 ~ 2,
    f10ur01ind == 3 ~ 3,
    f10ur01ind == 4 ~ 4,
  ),
  urban = factor(urban,
               levels = c(1, 2, 3, 4),           
               labels = c("Urban", "Town and Fringe", "Village", "Hamlet/Isolated Dwelling")))

table(alspac_t4$urban)
sum(is.na(alspac_t4$urban)) 

#------------------------------#

## Variable: Childhood antisocial behaviour
## Relevant variable(s): fdaa550
## Q: Child Antisocial activities score: F10
## C: Numeric
## M: -1, -9

# Check against codebook
alspac_t4$fdaa550

# Generate numeric score of childhood antisocial beahviour
alspac_t4$asb_10 <- as.numeric(alspac_t4$fdaa550)
alspac_t4$asb_10[alspac_t4$asb_10 < 0] <- NA

# Check original
table(alspac_t4$fdaa550, useNA="ifany")

# Check new
table(alspac_t4$asb_10, useNA="ifany") 

#------------------------------#

## Variable: Frequent tiredness
## Relevant variable(s): ccs1500
## Q: "Do you have problems with tiredness?"
## C: 1 = Less than usual, 2 = No more than usual, 3 = More than usual, 4 = Much more than usual
## M: -1, -10

# Check against codebook
alspac_t4$ccs1500

# Generate indicator of tiredness severity
alspac_t4 <- alspac_t4 %>%
  mutate(fq_tired = case_when(
    ccs1500 <0 ~ NA_real_,
    ccs1500 == 1 ~ 1,
    ccs1500 == 2 ~ 2,
    ccs1500 == 3 ~ 3,
    ccs1500 == 4 ~ 4,
  ),
  fq_tired = factor(fq_tired,
                 levels = c(1, 2, 3, 4),           
                 labels = c("Less than usual", "No more than ususal", "More than usual", "Much more than usual"))) 

table(alspac_t4$fq_tired)
sum(is.na(alspac_t4$fq_tired)) 

#-----EXPOSURES-----#

## Variable: EVER SMOKED
## Relevant variable(s): ccs4000
## Q: "Have you ever smoked a cigarette (including roll-ups)?"
## C: 1 = Yes, 2 = No
## M: -11, -10 , -1

# Check against codebook
alspac_t4$ccs4000

# Generate ever smoked
alspac_t4 <- alspac_t4 %>%
  mutate(smok_ever = case_when(
    ccs4000 == -1 ~ NA_real_,
    ccs4000 == 1 ~ 1,
    ccs4000 == 2 ~ 0
  ),
  smok_ever = factor(smok_ever,
                     levels = c(0, 1),           
                     labels = c("Never", "Ever")))

table(alspac_t4$smok_ever)
sum(is.na(alspac_t4$smok_ever)) 

#------------------------------#

## Variable: FREQUENCY SMOKED
## Relevant variable(s): ccs4005
## Q: "Please mark the box next to statement which describes you best"
## C: 1 = once or twice, 2 = used to but doesn't now, 3 = less than once a week;
## 4 = 1-6 cigarettes a week, 5 = 6+ cigarettes per week but not daily, 6 = 1 or more cigarettes a day
## M: -11, -10 , -1

# Check against codebook
alspac_t4$ccs4005

# Generate smoking frequency
alspac_t4 <- alspac_t4 %>%
  mutate(smok_freq= case_when(
    is.na(smok_ever) ~ NA_real_,
    smok_ever == "Never" ~ 0,
    ccs4005 == 1 | ccs4005 == 2 ~ 0,
    ccs4005 == 3 ~ 1,
    ccs4005 >=4 ~ 2,
    TRUE ~ NA
  ),
  smok_freq = factor(smok_freq,
                     levels = c(0, 1, 2),           
                     labels = c("Non-use", "Occasional", "Weekly or more")))

table(alspac_t4$smok_freq)
sum(is.na(alspac_t4$smok_freq)) 

#------------------------------#

## Variable: EVER USED CANNABIS
## Relevant variable(s): ccs4060
## Q: "Have you ever tried cannabis (also called marijuana, hash, dope, pot, skunk, puff,grass, draw, ganja, spliff, joints, smoke, weed)?"
## C: 1 = Yes, 2 = No
## M: -11, -10 , -1

# Check against codebook
alspac_t4$ccs4060

# Generate ever used cannabis
alspac_t4 <- alspac_t4 %>%
  mutate(cannabis_ever = case_when(
    ccs4060 == -1 ~ NA_real_,
    ccs4060 == 1 ~ 1,
    ccs4060 == 2 ~ 0
  ),
  cannabis_ever = factor(cannabis_ever,
                         levels = c(0, 1),           
                         labels = c("Never", "Ever")))

table(alspac_t4$cannabis_ever)
sum(is.na(alspac_t4$cannabis_ever)) 

#------------------------------#

## Variable: FREQUENCY CANNABIS USE
## Relevant variable(s): ccs4065
## Q: "Please mark the box next to statement which describes you best"
## C: 1 = once or twice, 2 = used to but doesn't now, 3 = less than once a week;
## 4 = 1-6 times a week, 5 = 6+ times per week but not daily, 6 = Usually uses everyday
## M: -11, -10 , -1

# Check against codebook
alspac_t4$ccs4065

# Generate cannabis frequency
alspac_t4 <- alspac_t4 %>%
  mutate(cannabis_freq= case_when(
    is.na(cannabis_ever) ~ NA_real_,
    cannabis_ever == "Never" ~ 0,
    ccs4065 == 1 | ccs4065 == 2 ~ 0,
    ccs4065 == 3 ~ 1,
    ccs4065 >=4 ~ 2,
    TRUE ~ NA
  ),
  cannabis_freq = factor(cannabis_freq,
                         levels = c(0, 1, 2),           
                         labels = c("Non-use", "Occasional", "Weekly or more")))

table(alspac_t4$cannabis_freq)
sum(is.na(alspac_t4$cannabis_freq)) 

#------------------------------#

## Variable: CONCURRENT USE
## Relevant variable(s): cannabis_freq and smok_freq 
## M: NA

alspac_t4 <- alspac_t4 %>%
  mutate(ct_couse = case_when(
    is.na(smok_freq) | is.na(cannabis_freq) ~ NA_integer_,
    smok_freq == "Non-use" & cannabis_freq == "Non-use" ~ 0,
    smok_freq == "Occasional" & cannabis_freq == "Non-use" ~ 1,
    smok_freq == "Weekly or more" & cannabis_freq == "Non-use" ~ 1,
    smok_freq == "Non-use" & cannabis_freq == "Occasional" ~ 2,
    smok_freq == "Non-use" & cannabis_freq == "Weekly or more" ~ 2,
    smok_freq == "Occasional" & cannabis_freq == "Occasional" ~ 3,
    smok_freq == "Weekly or more" & cannabis_freq == "Weekly or more" ~ 3,
    smok_freq == "Weekly or more" & cannabis_freq == "Occasional" ~ 3,
    smok_freq == "Occasional" & cannabis_freq == "Weekly or more" ~ 3,
  ), 
  ct_couse = factor(ct_couse,
                    levels = c(0,1,2,3),
                    labels = c("Non-use", "Tobacco only", "Cannabis only", "Co-use")))

table(alspac_t4$ct_couse)
sum(is.na(alspac_t4$ct_couse)) 

#-----COVARIATES-----#

## Variable: AGE
## Relevant variable(s): ccs9991b
## Q: DV: Age of study child at completion (weeks) 
## C: Numeric
## M: -11, -10 

# Check against codebook
alspac_t4$ccs9991b

# Generate age in weeks
alspac_t4 <- alspac_t4 %>%
  mutate(age_weeks = case_when(
    ccs9991b == -10 | ccs9991b == -11 ~ NA,
    TRUE ~ ccs9991b
  ))

# Divide by weeks in year (52.143) to get age_years
alspac_t4['age_years'] = alspac_t4['age_weeks'] / 52.143 
alspac_t4$age_years<-as.numeric(alspac_t4$age_years)
summary(alspac_t4$age_years) #mean = 16.7 years 

#------------------------------#

## Variable: SEX
## Relevant variable(s): kz021
## Q: B1: Sex of child
## C: 1 = Male; 2 = Female
## M: -1

# Check against codebook
alspac_t4$kz021

# Generate biological sex
alspac_t4 <- alspac_t4 %>%
  mutate(sex = case_when(
    kz021 == -1 ~ NA_real_,
    TRUE ~ kz021
  ),
  sex = factor(sex,
               levels = c(1, 2),           
               labels = c("Male", "Female")))

table(alspac_t4$sex)
sum(is.na(alspac_t4$sex)) 

#------------------------------#

## Variable: ETHNICITY
## Relevant variable(s): kw9500
## Q: L1: Cultural background of child
## C: 0 = Other, 1 = White, 2 = Mixed, 3 = Asian, 4 = Black, 5 = Other
## M: 9, -11, -10, -1 
# Note: very low sample count on some groups (n <5), will have to collapse to White, non-white 

# Check against codebook
alspac_t4$kw9500

# Generate ethnicity
alspac_t4 <- alspac_t4 %>%
  mutate(ethnicity = case_when(
    kw9500 == -1 | kw9500 == -10 ~ NA_real_,
    kw9500 == 1 ~ 0,
    kw9500 >1 ~ 1
  ),
  ethnicity = factor(ethnicity,
                     levels = c(0, 1),           
                     labels = c("White", "Non-white")))

table(alspac_t4$ethnicity)
sum(is.na(alspac_t4$ethnicity)) 

#------------------------------#

## Variable: MATERNAL EDUCATION
## Relevant variable(s): n4000-n4015
## Q: Checklist of "What educational qualifications do you have?"
## C: 1 = ticked, -1 = unticked
## M: -10
# Note: Aligns with in-house approach to coding maternal education for variable 'c645a'

# Vector of variable names
edu_vars <- c("n4000", "n4001", "n4002", "n4003", "n4004", "n4005", 
              "n4006", "n4007", "n4008", "n4009", "n4010", "n4011",
              "n4012", "n4013", "n4015")

# Following approach in built.pdf we only want to code 'not known' as missing

# Loop through variables and create new recoded versions
for(var in edu_vars) {
  new_var <- paste0(var, "_edu")  # Creates new variable name with "_rec" suffix
  alspac_t4[[new_var]] <- case_when(
    alspac_t4$n4013 == 1 ~ NA_real_,
    alspac_t4[[var]] == -1 | alspac_t4[[var]] == -10 ~ 0,
    alspac_t4[[var]] == 1 ~ 1,
    TRUE ~ NA_real_
  )
}

sum(is.na(alspac_t4$n4000_edu))
sum(is.na(alspac_t4$n4015_edu))

# Generate level of maternal education
alspac_t4 <- alspac_t4 %>%
  mutate(mum_educ645a = case_when(
    is.na(n4000_edu) | is.na(n4001_edu) | is.na(n4002_edu) | is.na(n4003_edu) | is.na(n4004_edu) | 
      is.na(n4005_edu) | is.na(n4006_edu) | is.na(n4007_edu) | is.na(n4008_edu) | is.na(n4009_edu) | 
      is.na(n4010_edu) | is.na(n4011_edu) | is.na(n4012_edu) | is.na(n4013_edu) | is.na(n4015_edu) ~ NA,
    n4011_edu == 1 ~ 5,
    n4002_edu == 1 | n4005_edu == 1 | n4006_edu == 1 | n4008_edu == 1 | n4009_edu == 1 | n4010_edu == 1 ~ 4,
    n4001_edu == 1 ~ 3,
    n4003_edu == 1 | n4004_edu == 1 | n4007_edu == 1 | n4015_edu == 1 ~ 2,
    n4000_edu == 1 ~ 1,
    TRUE ~ 1,
  ),
  mum_educ645a = factor(mum_educ645a,
                        levels = c(1, 2, 3, 4, 5),           
                        labels = c("CSE/none", "Vocational", "O Level", "A Level", "Degree")))

table(alspac_t4$mum_educ645a)
sum(is.na(alspac_t4$mum_educ645a)) 

# Collapse to low, medium, high; as in 10.1136/jech-2022-219617
alspac_t4 <- alspac_t4 %>%
  mutate(maternal_edu = case_when(
    is.na(mum_educ645a) ~ NA,
    mum_educ645a == "CSE/none" ~ 0,
    mum_educ645a == "Vocational" ~ 0,
    mum_educ645a == "O Level" ~ 0,
    mum_educ645a == "A Level" ~ 1,
    mum_educ645a == "Degree" ~ 2
  ),
  maternal_edu = factor(maternal_edu,
                        levels = c(0,1,2),
                        labels = c("Lower", "Moderate", "Higher")))

table(alspac_t4$maternal_edu)
sum(is.na(alspac_t4$maternal_edu)) 

#------------------------------#
## ADEQUATE SLEEP
# Relevant variable: fh5341
# Q: Frequency YP feels that they get enough sleep
# C: 1 = Always, 2 = Usually, 3 = Sometimes, 4 = Rarely, 5 = Never
# M: -10, -6, -1

# Check against codebook
alspac_t4$fh5341

# Generate indicator of sleeping problems
alspac_t4 <- alspac_t4 %>%
  mutate(sleep_problems = case_when(
    fh5341 == 1 | fh5341 == 2 | fh5341 == 3 ~0,
    fh5341 == 4 | fh5341 == 5 ~ 1,
    fh5341 == -10 | fh5341 == -1 | fh5341 == -6 ~ NA
  ),
  sleep_problems = factor(
    sleep_problems,
    levels = c(0, 1),
    labels = c("No", "Yes"),
  ))

table(alspac_t4$sleep_problems)
sum(is.na(alspac_t4$sleep_problems))

#------------------------------#

## NEUROTICISM
# Relevant variable: fg7363
# Q: Items for the neuroticism scale from IPIP; ALSPAC derived summary variable 
# C: Numeric 
# M: -1, -10

# Check against codebook
alspac_t4$fg7363

# Generate neuroticism score
alspac_t4 <- alspac_t4 %>%
  mutate(neuroticism = case_when(
    fg7363 == -1 | fg7363 == -10 ~NA,
    TRUE ~ fg7363))

table(alspac_t4$neuroticism)
sum(is.na(alspac_t4$neuroticism)) 

# Removing haven labels
alspac_t4$neuroticism <- as.numeric(alspac_t4$neuroticism)
class(alspac_t4$neuroticism)
#------------------------------#

## ANXIETY
# Relevant variable: fh6877
# Q: DAWBA: Whether any anxiety disorder present (self-report computer prediction, ICD-10 and DSM-IV, probabilities)
# C: Probability bands ranging from <0.1% --> 50%
# M: -1, -10

# Check against codebook
alspac_t4$fh6877

# Generate anxiety disorder indicator
alspac_t4 <- alspac_t4 %>%
  mutate(anx_dawba = case_when(
    fh6877 == -10 ~NA,
    fh6877 == 1 ~ 0,
    fh6877 == 2 ~ 0,
    fh6877 == 3 ~ 1, 
    fh6877 == 4 ~ 2),
    ,
    anx_dawba = factor(anx_dawba,
                       levels = c(0,1,2),
                       labels = c("Low", "Moderate", "High")))

summary(alspac_t4$anx_dawba)
sum(is.na(alspac_t4$anx_dawba)) 

#------------------------------#

## CONDUCT PROBLEMS
# SC reported variable not available, taking carer reported score
# Relevant variables: ta7025b
# Q: "For each item, please mark the box...Please give your answers on the basis of how things have been for you over the last six months"
# C: 1 Not true; 2 Somewhat true; 3 Certainly true
# M: -9, -3

# Checking against codebook
alspac_t4$ta7025b

# Identifying missing and making numeric
alspac_t4 <- alspac_t4 %>%
  mutate(cd_score = case_when(
    ta7025b == -10 | ta7025b == -1 ~NA,
    TRUE ~ ta7025b))
alspac_t4$cd_score <- as.numeric(alspac_t4$cd_score)

table(alspac_t4$cd_score)
sum(is.na(alspac_t4$cd_score)) 

#------------------------------#

## OTHER ILLICIT DRUG USE
# Relevant variables: ccr840:ccr844, ccr850:ccr857
# Q: [CCR] "Have you ever tried, taken or used any of the following?"
# C: [CCR] 1 = Yes, since my 13th birthday, 2 = Yes, but not since 13th birthday, 3 = No,never

# Check example against codebook
alspac_t4$ccr840

# For each variable in the range
for(var in c(paste0("ccr", 840:844), paste0("ccr", 850:857))) {
  if(var %in% names(alspac_t4)) {
    # Create new variable with _r suffix
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] < 0 ~ NA,
      alspac_t4[[var]] %in% c(1, 2) ~ 1,
      alspac_t4[[var]] == 3 ~ 0,
      TRUE ~ NA
    )
  }
}

# Check original and recoded variables
table(alspac_t4$ccr840, useNA = "ifany")
table(alspac_t4$ccr840_r, useNA = "ifany")

# Coding for any drug use at CCR
alspac_t4$ever_drug13 <- ifelse(
  apply(alspac_t4[, paste0("ccr", c(840:844, 850:857), "_r")], 1, 
        function(x) any(is.na(x))),
  NA,
  ifelse(
    apply(alspac_t4[, paste0("ccr", c(840:844, 850:857), "_r")], 1, 
          function(x) any(x == 1, na.rm = TRUE)),
    1, 0)
)

# Check the results
table(alspac_t4$ever_drug13, useNA = "ifany")

# Relevant variables: ccs4150:ccs4154, ccs4160:ccs4170
# Q: [CCS] "Have you ever tried...since your 15th birthday?
# C: [CCS] 1 = No, 2 = Yes, less than 5 times, 3 = Yes more than 5 times

# Check example against codebook
alspac_t4$ccs4150

# For each variable in the range
for(var in c(paste0("ccs", 4150:4154), paste0("ccs", 4160:4170))) {
  if(var %in% names(alspac_t4)) {
    # Create new variable with _r suffix
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] < 0 ~ NA,
      alspac_t4[[var]] %in% c(2, 3) ~ 1,
      alspac_t4[[var]] == 1 ~ 0,
      TRUE ~ NA
    )
  }
}

# Check original and recoded variables
table(alspac_t4$ccs4150, useNA = "ifany")
table(alspac_t4$ccs4150_r, useNA = "ifany")

# Coding for any past year drug use at CCS
alspac_t4$year_drug16 <- ifelse(
  apply(alspac_t4[, paste0("ccs", c(4150:4154, 4160:4170), "_r")], 1, 
        function(x) any(is.na(x))),
  NA,
  ifelse(
    apply(alspac_t4[, paste0("ccs", c(4150:4154, 4160:4170), "_r")], 1, 
          function(x) any(x == 1, na.rm = TRUE)),
    1, 0)
)

# Check the results
table(alspac_t4$year_drug16, useNA = "ifany")

# Combining variables (i.e., any report of drug use at age 13 years OR 16 years)
alspac_t4 <- alspac_t4 %>%
  mutate(ever_drug = case_when(
    # If we have age 16 data, use it regardless of age 13
    year_drug16 == 1 ~ 1,
    year_drug16 == 0 ~ 0,
    # If age 16 is missing but we have age 13 data, use that
    is.na(year_drug16) & ever_drug13 == 1 ~ 1,
    is.na(year_drug16) & ever_drug13 == 0 ~ 0,
    # Only set to NA if both are missing
    TRUE ~ NA
  ),
  ever_drug = factor(ever_drug,
                     levels = c(0,1),
                     labels = c("Never drug use", "Ever drug use")))
table(alspac_t4$ever_drug)
sum(is.na(alspac_t4$ever_drug)) 

#------------------------------#

## LIFETIME ALCOHOL USE
# Relevant variables: ccs3500
# Q: "Have you ever drunk alcohol?"
# C: 1 = Yes, 2 = No

# Check against codebook
alspac_t4$ccs3500

# Coding for lifetime alcohol use
alspac_t4 <- alspac_t4 %>%
  mutate(ever_drink = case_when(
    ccs3500 <0 ~ NA,
    ccs3500 == 1 ~ 1,
    ccs3500 == 2 ~ 0
  ),
  ever_drink = factor(ever_drink,
                      levels = c(0,1),
                      labels = c("Never alcohol", "Ever alcohol")))
table(alspac_t4$ever_drink)
sum(is.na(alspac_t4$ever_drink)) 

#------------------------------#

## PROBLEMATIC ALCOHOL USE
# Relevant variables: ccs3540:ccs3549
# Q: AUDIT-10 (e.g., "How often do you have six or more units of alcohol on one occasion?")
# C: https://assets.publishing.service.gov.uk/media/6357a7af8fa8f557d85b7c44/Alcohol-use-disorders-identification-test-AUDIT_for-print.pdf

# Stem question = ccs3540 (if answer = 'never'(1) then don't answer AUDIT)
alspac_t4$ccs3540

# Coding for current alcohol use
alspac_t4 <- alspac_t4 %>%
  mutate(current_drink = case_when(
    ever_drink == "Never alcohol" ~ 0,
    ccs3540 <0 ~ NA,
    ccs3540 == 1 ~ 0,
    ccs3540 >=2 ~ 1
  ),
  current_drink = factor(current_drink,
                         levels = c(0,1),
                         labels = c("No", "Yes")))
table(alspac_t4$current_drink)
sum(is.na(alspac_t4$current_drink)) 

# Re-scaling AUDIT items
# For each variable in the range
for(var in c(paste0("ccs", 3540:3547))) {
  if(var %in% names(alspac_t4)) {
    # Create new variable with _r suffix
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4$ever_drink == "Never alcohol" ~ 0,
      alspac_t4$current_drink == "No" ~ 0,
      alspac_t4[[var]] < 0 ~ NA,
      alspac_t4[[var]] == 1 ~ 0,
      alspac_t4[[var]] == 2 ~ 1,
      alspac_t4[[var]] == 3 ~ 2,
      alspac_t4[[var]] == 4 ~ 3,
      alspac_t4[[var]] == 5 ~ 4,
      TRUE ~ NA
    )
  }
}

# Check original and recoded variables
table(alspac_t4$ccs3543, useNA = "ifany")
table(alspac_t4$ccs3543_r, useNA = "ifany")

# Re-scaling AUDIT items
# For each variable in the range
for(var in c(paste0("ccs", 3548:3549))) {
  if(var %in% names(alspac_t4)) {
    # Create new variable with _r suffix
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4$ever_drink == "Never alcohol" ~ 0,
      alspac_t4$current_drink == "No" ~ 0,
      alspac_t4[[var]] < 0 ~ NA,
      alspac_t4[[var]] == 1 ~ 0,
      alspac_t4[[var]] == 2 ~ 2,
      alspac_t4[[var]] == 3 ~ 4,
      TRUE ~ NA
    )
  }
}

# Check original and recoded variables
table(alspac_t4$ccs3548, useNA = "ifany")
table(alspac_t4$ccs3548_r, useNA = "ifany")

# Create AUDIT total score - NA if any component is NA
alspac_t4$audit_total <- as.numeric(apply(
  alspac_t4[, c(paste0("ccs", 3540:3547, "_r"), 
                 paste0("ccs", 3548:3549, "_r"))], 
  1, 
  function(x) if(any(is.na(x))) NA else sum(x)
))

# Check the distribution
summary(alspac_t4$audit_total) 
class(alspac_t4$audit_total)

#------------------------------#

## ADHD DISORDER
# Relevant variables: j556b, kq346b, n8365c, ku706b, kw6601b, ta7025c
# Q: "For each item, please mark the box...Please give your answers on the basis of how things have been for you over the last six months"
# C: 1 Not true; 2 Somewhat true; 3 Certainly true
# M: -5, -1, -6, -1, -10

table(alspac_t4$j556b) # SDQ hyperactivity prorated @ ~4 years
table(alspac_t4$kq346b) # SDQ hyperactivity prorated @ ~7 years
table(alspac_t4$n8365c) # SDQ hyperactivity @ ~8 years 
table(alspac_t4$ku706b) # SDQ hyperactivity prorated @ ~9.5 years
table(alspac_t4$kw6601b) # SDQ hyperactivity prorated @ ~11.5 years
table(alspac_t4$ta7025c) # SDQ hyperactivity prorated @ ~13 years

# Vector of SDQ variables
sdq_vars <- c("kq346b", "ku706b", "kw6601b", "ta7025c", "j556b", "n8365c")

# Select first 5 waves only (matching LSAC approach)
sdq_vars <- sdq_vars[1:5]

# Initial data check
for(var in sdq_vars) {
  print(paste("Distribution of", var))
  print(table(alspac_t4[[var]], useNA = "ifany"))
}

# Recode variables to numeric, handling missing values
for(var in sdq_vars) {
  new_var <- paste0("sdq_", var)
  alspac_t4[[new_var]] <- case_when(
    alspac_t4[[var]] < 0 ~ NA_real_,
    TRUE ~ as.numeric(alspac_t4[[var]])
  )
}

# Create binary diagnosis indicators for each wave
for(var in sdq_vars) {
  new_var <- paste0("adhd_dx_", var)
  alspac_t4[[new_var]] <- case_when(
    is.na(alspac_t4[[paste0("sdq_", var)]]) ~ NA_real_,
    alspac_t4[[paste0("sdq_", var)]] >= 7 ~ 1,
    alspac_t4[[paste0("sdq_", var)]] < 7 ~ 0
  )
}

# Create diagnostic variables
dx_vars <- paste0("adhd_dx_", sdq_vars)

alspac_t4 <- alspac_t4 %>%
  mutate(
    # Count waves with ADHD and valid measurements
    adhd_dx_count = rowSums(across(all_of(dx_vars), ~ . == 1), na.rm = TRUE),
    adhd_valid_waves = rowSums(!is.na(across(all_of(dx_vars)))),
    
    # Classification requiring 50% valid waves (3+ out of 5) and 2+ ADHD indications
    adhd_dx_final = case_when(
      adhd_dx_count >= 2 & adhd_valid_waves >= 3 ~ 1,  # 2+ ADHD indications AND 3+ valid waves
      adhd_dx_count < 2 & adhd_valid_waves >= 3 ~ 0,   # <2 ADHD indications BUT 3+ valid waves
      TRUE ~ NA_real_                                   # Not enough valid waves
    ),
    
    # Create factor version if needed
    adhd_dx_final = factor(adhd_dx_final,
                           levels = c(0, 1),
                           labels = c("No ADHD", "ADHD"))
  )

# Check results
print("Distribution of valid waves per person:")
table(alspac_t4$adhd_valid_waves)

print("Distribution of ADHD indications per person:")
table(alspac_t4$adhd_dx_count)

print("Final ADHD classification (2+ indications AND 3+ valid waves):")
table(alspac_t4$adhd_dx_final, useNA = "ifany")
prop.table(table(alspac_t4$adhd_dx_final, useNA = "ifany")) * 100

print(paste("Missing (less than 3 valid waves):", sum(is.na(alspac_t4$adhd_dx_final)))) 

#------------------------------#

## ACEs

# INTERPARENTAL VIOLENCE
# Checking codebooks
# GROUP 1
alspac_t4$f242 #5 = Did not happen, 1-4 = Yes (varying effect), 9 = don't know/not known, -1 = missing
alspac_t4$g322 #5 = Did not happen, 1-4 = Yes (varying effect), 9 = don't know/not known, 0 = other
alspac_t4$h232 #5 = Did not happen, 1-4 = Yes (varying effect), 9 = don't know/not known, 0 = other, -1 = not stated
alspac_t4$j322 #5 = Did not happen, 1-4 = Yes (varying effect), 9 = don't know/not known, 0 = no/missing
alspac_t4$k4022 #5 = Did not happen, 1-4 = Yes (varying effect), 9 = don't know/not known, 0 = other, -1 = no response, -10 = not completed
alspac_t4$l4022 #5 = Did not happen, 1-4 = Yes (varying effect), 9 = don't know/not known, 0 = other, -1 = no response, -10 = not completed
# GROUP 2
alspac_t4$p2022 #4 = No, did not happen in the past 3 years, 1-3 = Yes (varying times), -1 = no response, -10 = not completed
# GROUP 3 
alspac_t4$n3044 #1 = No, 2 = Sometimes, 3 = Often, 9 = Don't know, -1 = no response, -2 = unresolvable, -10 = not completed 
alspac_t4$n3045
alspac_t4$n3048
alspac_t4$n3049
alspac_t4$n3050
alspac_t4$n3051
alspac_t4$n3052
alspac_t4$n3053
alspac_t4$n3054
alspac_t4$n3055
alspac_t4$n3056
alspac_t4$n3057
alspac_t4$n3058
alspac_t4$n3059

# For variables in Group 1
# Variables to recode
vars <- c("f242", "g322", "h232", "j322", "k4022", "l4022")  

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == 5 ~ 0,         # 5 = didn't happen
      alspac_t4[[var]] %in% 1:4 ~ 1,     # 1-4 = happened
      alspac_t4[[var]] == 0 ~ NA_real_,  # 0 = missing/other
      alspac_t4[[var]] == 9 ~ NA_real_,  # 9 = don't know
      alspac_t4[[var]] < 0 ~ NA_real_,   # Negative values = missing
      TRUE ~ NA_real_                      # Everything else to NA
    )
  }
}

# Check original and recoded variables for one example
cat("Original variable f242:\n")
print(table(alspac_t4$f242, useNA = "ifany"))
cat("\nRecoded variable f242_r:\n")
print(table(alspac_t4$f242_r, useNA = "ifany"))

# For variables in Group 2
alspac_t4$p2022_r <- case_when(
  alspac_t4$p2022 < 0 ~ NA_real_,
  alspac_t4$p2022 == 4 ~ 0,
  alspac_t4$p2022 %in% c(1,2,3) ~ 1,
  TRUE ~ NA_real_
)

# Check original and recoded variables
cat("Original variable:\n")
print(table(alspac_t4$p2022, useNA = "ifany"))
cat("\nRecoded variable:\n")
print(table(alspac_t4$p2022_r, useNA = "ifany"))

# For each variable in Group 3
for(var in c("n3044", "n3045", "n3048", "n3049", "n3050", "n3051", "n3052", 
             "n3053", "n3054", "n3055", "n3056", "n3057", "n3058", "n3059")) {
  if(var %in% names(alspac_t4)) {
    # Create new variable with _r suffix
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] < 0 ~ NA_real_,
      alspac_t4[[var]] == 9 ~ NA_real_,
      alspac_t4[[var]] == 1 ~ 0,
      alspac_t4[[var]] %in% c(2, 3) ~ 1,
      TRUE ~ NA_real_
    )
  }
}

# Check original and recoded variables for one example
cat("Original variable n3044:\n")
print(table(alspac_t4$n3044, useNA = "ifany"))
cat("\nRecoded variable n3044_r:\n")
print(table(alspac_t4$n3044_r, useNA = "ifany"))

# Create vector of recoded variable names
group3_vars_r <- paste0(c("n3044", "n3045", "n3048", "n3049", "n3050", "n3051", "n3052", 
                          "n3053", "n3054", "n3055", "n3056", "n3057", "n3058", "n3059"), "_r")

# Create summary variable
alspac_t4$group3_any_violence <- apply(
  alspac_t4[, group3_vars_r], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_  # If all values are NA, return NA
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If any value is 1, return 1
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no 1s and at least one 0, return 0
      } else {
        NA_real_  # For any other case, return NA
      }
    }
  }
)

# Check the distribution
table(alspac_t4$group3_any_violence, useNA = "ifany")

## OVERALL PARENTAL VIOLENCE ##

# Define variable groups
ipv1_vars_r <- paste0(c("f242", "g322", "h232", "j322", "k4022", "l4022"), "_r")
ipv2_vars_r <- "p2022_r"  # Just one variable
ipv3_vars_r <- "group3_any_violence" # Just one variable

# Count valid responses for each group
alspac_t4$ipv1_n_valid <- apply(alspac_t4[, ipv1_vars_r], 1, function(x) sum(!is.na(x)))
alspac_t4$ipv2_n_valid <- apply(alspac_t4[, ipv2_vars_r, drop = FALSE], 1, function(x) sum(!is.na(x)))
alspac_t4$ipv3_n_valid <- apply(alspac_t4[, ipv3_vars_r, drop = FALSE], 1, function(x) sum(!is.na(x)))

# Sum up valid responses across all groups
alspac_t4$ipv_n_valid <- apply(
  alspac_t4[, c("ipv1_n_valid", "ipv2_n_valid", "ipv3_n_valid")], 
  1, 
  sum
)

# Check distribution
summary(alspac_t4$ipv_n_valid)
table(alspac_t4$ipv_n_valid, useNA = "ifany")

# Combine all indicators into a summary score
alspac_t4$ipv_summary <- apply(
  alspac_t4[, c(paste0(c("f242", "g322", "h232", "j322", "k4022", "l4022"), "_r"), 
                 "p2022_r", 
                 "group3_any_violence")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_  # If all values are NA, return NA
    } else {
      sum(x == 1, na.rm = TRUE)  # Sum number of waves where exposure occurred
    }
  }
)

# Check distribution
table(alspac_t4$ipv_summary, useNA = "ifany")
summary(alspac_t4$ipv_summary)

# Create ipv_valid variable
alspac_t4$ipv_valid <- ifelse(
  alspac_t4$ipv_n_valid >= 4,  # At least 4 valid waves (50% of 8)
  alspac_t4$ipv_summary,
  NA_real_
)

# Check distribution
table(alspac_t4$ipv_valid, useNA = "ifany")

# Create binary exposure from ipv_valid variable 
alspac_t4 <- alspac_t4 %>%
  mutate(
    ipv_binary = case_when(
      ipv_valid >= 1 ~ 1,  # At least one exposure to ipv
      ipv_valid == 0 ~ 0,
      is.na(ipv_valid) ~ NA
    ))

# Check distribution
table(alspac_t4$ipv_binary, useNA = "ifany") 

#----------------------#

# BULLYING VICTIMIZATION

# Checking codebooks
# GROUP 1 (@8 or @10 years)
alspac_t4$f8fp470 #1 = Yes, #2 = No, -1, -2, -9 = not completed/missing
alspac_t4$f8fp475 #1 = Yes, #2 = No, -1, -2, -9 = not completed/missing
alspac_t4$fdfp470 #1 = Yes, #2 = No, -1, -2, -9 = not completed/missing
alspac_t4$fdfp475 #1 = Yes, #2 = No, -1, -2, -9 = not completed/missing
# GROUP 2 (@8 years)
alspac_t4$ccc290 #4 = Never, #1-3 = Yes, varying amounts, 9 = DK, 0 = Other text answer, -1, -8, -10 = not completed/no response


## GROUP 1
# Variables to recode
vars <- c("f8fp470", "f8fp475", "fdfp470", "fdfp475")

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == 2 ~ 0,         
      alspac_t4[[var]] == 1 ~ 1,  
      alspac_t4[[var]] < 0 ~ NA_real_
    )
  }
}

# Check original and recoded variables for one example
cat("Original variable f8fp470:\n")
print(table(alspac_t4$f8fp470, useNA = "ifany"))
cat("\nRecoded variable f8fp470_r:\n")
print(table(alspac_t4$f8fp470_r, useNA = "ifany"))

## GROUP 2
alspac_t4 <- alspac_t4 %>%
  mutate(ccc290_r = case_when(
    ccc290 <0 ~ NA,
    ccc290 == 0 | ccc290 == 9 ~ NA,
    ccc290 == 4 | ccc290 == 3 ~ 0,
    ccc290 == 1 | ccc290 == 2 ~ 1
  ))

# Check original and recoded variables for one example
cat("Original variable ccc290:\n")
print(table(alspac_t4$ccc290, useNA = "ifany"))
cat("\nRecoded variable ccc290_r:\n")
print(table(alspac_t4$ccc290_r, useNA = "ifany"))

## OVERALL BULLYING ##

# For wave 1 (@8 years - f8fp470 and f8fp475)
alspac_t4$bully1_n_valid <- apply(
  alspac_t4[, paste0(c("f8fp470", "f8fp475"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For wave 2 (@10 years - fdfp470 and fdfp475)
alspac_t4$bully2_n_valid <- apply(
  alspac_t4[, paste0(c("fdfp470", "fdfp475"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For wave 3 (ccc290)
alspac_t4$bully3_n_valid <- !is.na(alspac_t4$ccc290_r)

# Sum up valid waves across all groups
alspac_t4$bully_n_valid <- alspac_t4$bully1_n_valid + 
  alspac_t4$bully2_n_valid + 
  alspac_t4$bully3_n_valid
cat("\nTotal valid waves:\n")
table(alspac_t4$bully_n_valid, useNA = "ifany")

# Create wave-specific bullying indicators
# Wave 1 (@8 years - f8fp470 and f8fp475)
alspac_t4$bully1_any <- apply(
  alspac_t4[, paste0(c("f8fp470", "f8fp475"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_  # If both items are NA
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates bullying
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no bullying reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)

# Wave 2 (@10 years - fdfp470 and fdfp475)
alspac_t4$bully2_any <- apply(
  alspac_t4[, paste0(c("fdfp470", "fdfp475"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0
      } else {
        NA_real_
      }
    }
  }
)

# Wave 3 (single item)
alspac_t4$bully3_any <- alspac_t4$ccc290_r

# Check distributions
cat("Wave 1 any bullying:\n")
table(alspac_t4$bully1_any, useNA = "ifany")
cat("\nWave 2 any bullying:\n")
table(alspac_t4$bully2_any, useNA = "ifany")
cat("\nWave 3 any bullying:\n")
table(alspac_t4$bully3_any, useNA = "ifany")

# First create summary score of bullying exposure across waves
alspac_t4$bully_summary <- apply(
  alspac_t4[, c("bully1_any", "bully2_any", "bully3_any")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      sum(x == 1, na.rm = TRUE)  # Sum number of waves where bullying occurred
    }
  }
)

# Then create final valid variable requiring >=2 valid waves
alspac_t4$bully_valid <- ifelse(
  alspac_t4$bully_n_valid >= 2,  # At least 2 valid waves
  alspac_t4$bully_summary,
  NA_real_
)

# Check distributions
cat("Distribution of bully_valid:\n")
table(alspac_t4$bully_valid, useNA = "ifany")

# Create binary exposure from bully_valid variable 
alspac_t4 <- alspac_t4 %>%
  mutate(
    bully_binary = case_when(
      bully_valid >= 1 ~ 1,  # At least one exposure to bullying
      bully_valid == 0 ~ 0,
      is.na(bully_valid) ~ NA
    ))

# Check distribution
table(alspac_t4$bully_binary, useNA = "ifany") 

#------------------------------#

# SEXUAL ABUSE 

# Checking codebooks
#GROUP 1
alspac_t4$kd505a # 5 = Did not happen, 1-4 = Yes (varying impact), 9 = DK, 0 = Other, -1 = Missing
alspac_t4$kl475 # 5 = Did not happen, 1-4 = Yes (varying impact), 9 = DK, 0 = Other, -1 = No response, -10 = Not completed
alspac_t4$kn4005 # 5 = Did not happen, 1-4 = Yes (varying impact), 9 = DK, 0 = Other, -1 = No response, -10 = Not completed
alspac_t4$kt5005 # 5 = Did not happen, 1-4 = Yes (varying impact), 9 = DK, 0 = Other, -1 = No response, -10 = Not completed

#GROUP 2
alspac_t4$kf455a # 1 = Yes, 2 = No, -1 = Missing
alspac_t4$kj465a # 1 = Yes, 2 = No
alspac_t4$kq365a # 1 = Yes, 2 = No, -1 = Not stated; -6 = Section D omitted

# For variables in Group 1
# Variables to recode
vars <- c("kd505a", "kl475", "kn4005", "kt5005")  

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == 5 ~ 0,         # 5 = didn't happen
      alspac_t4[[var]] %in% 1:4 ~ 1,     # 1-4 = happened
      alspac_t4[[var]] == 0 ~ NA_real_,  # 0 = missing/other
      alspac_t4[[var]] == 9 ~ NA_real_,  # 9 = don't know
      alspac_t4[[var]] < 0 ~ NA_real_,   # Negative values = missing
      TRUE ~ NA_real_                      # Everything else to NA
    )
  }
}

# Check original and recoded variables for one example
cat("Original variable kd505a:\n")
print(table(alspac_t4$kd505a, useNA = "ifany"))
cat("\nRecoded variable kd505a_r:\n")
print(table(alspac_t4$kd505a_r, useNA = "ifany"))

# For variables in Group 2
# Variables to recode
vars <- c("kf455a", "kj465a", "kq365a")

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == 2 ~ 0,  
      alspac_t4[[var]] == 1 ~ 1,  
      alspac_t4[[var]] < 0 ~ NA_real_,   # Negative values = missing
      TRUE ~ NA_real_                      # Everything else to NA
    )
  }
}

# Check original and recoded variables for one example
cat("Original variable kf455a:\n")
print(table(alspac_t4$kf455a, useNA = "ifany"))
cat("\nRecoded variable kf455a_r:\n")
print(table(alspac_t4$kf455a_r, useNA = "ifany"))

## OVERALL SEXUAL ABUSE ##

# Define variable groups
sa1_vars_r <- paste0(c("kd505a", "kl475", "kn4005", "kt5005"), "_r")
sa2_vars_r <- paste0(c("kf455a", "kj465a", "kq365a"), "_r")

# Count valid responses for each group
alspac_t4$sa1_n_valid <- apply(alspac_t4[, sa1_vars_r], 1, function(x) sum(!is.na(x)))
alspac_t4$sa2_n_valid <- apply(alspac_t4[, sa2_vars_r], 1, function(x) sum(!is.na(x)))

# Sum up valid responses across all groups
alspac_t4$sa_n_valid <- apply(
  alspac_t4[, c("sa1_n_valid", "sa2_n_valid")], 
  1, 
  sum
)

# Check distribution
summary(alspac_t4$sa_n_valid)
table(alspac_t4$sa_n_valid, useNA = "ifany")

# Combine all indicators into a summary score
alspac_t4$sa_summary <- apply(
  alspac_t4[, paste0(c("kd505a", "kl475", "kn4005", "kt5005", "kf455a", "kj465a", "kq365a"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_  # If all values are NA, return NA
    } else {
      sum(x == 1, na.rm = TRUE)  # Sum number of waves where exposure occurred
    }
  }
)

# Check distribution
table(alspac_t4$sa_summary, useNA = "ifany")
summary(alspac_t4$sa_summary)

# Create sa_valid variable
alspac_t4$sa_valid <- ifelse(
  alspac_t4$sa_n_valid >= 4,  # At least 4 valid waves (>50% of 7)
  alspac_t4$sa_summary,
  NA_real_
)

# Check distribution
table(alspac_t4$sa_valid, useNA = "ifany")

# Create binary exposure from sa_valid variable 
alspac_t4 <- alspac_t4 %>%
  mutate(
    sa_binary = case_when(
      sa_valid >= 1 ~ 1,  # At least one exposure to sa
      sa_valid == 0 ~ 0,
      is.na(sa_valid) ~ NA
    ))

# Check distribution
table(alspac_t4$sa_binary, useNA = "ifany") 

#------------------------------#

# HOUSEHOLD SUBSTANCE ABUSE #

# Checking codebooks
# GROUP 1
alspac_t4$n1057 #3 = No, 0-2 = Yes varying times, 9 = DK, any negative value = missing
alspac_t4$n1058 #3 = No, 0-2 = Yes varying times, 9 = DK, any negative value = missing

# GROUP 2
alspac_t4$p1022 #3 = No, 1-2 = Yes varying source, any negative value = missing
alspac_t4$r2017 #3 = No, 1-2 = Yes varying time, 9 = DK, any negative value = missing
alspac_t4$r2018 #3 = No, 1-2 = Yes varying time, 9 = DK, any negative value = missing
alspac_t4$s1022 #3 = No, 1-2 = Yes varying source, 0 = other, any negative value = missing
alspac_t4$k1022 #3 = No, 1-2 = Yes varying source, 9 = DK, 0 = other, any negative value = missing
alspac_t4$l3022 #3 = No, 1-2 = Yes varying source, 9 = DK, 0 = other, any negative value = missing

# GROUP 3
alspac_t4$e192 #1 = Daily, 2 = Often, 3 = Sometimes, 4 = Once, 5 = Not at all, 9 = DK, 0 = other, any negative value = missing 
alspac_t4$f061 #1 = Daily, 2 = Often, 3 = Sometimes, 4 = Not at all, 9 = DK, 0 = other, any negative value = missing 
alspac_t4$g047 #1 = Daily, 2 = Often, 3 = Sometimes, 4 = Not at all
alspac_t4$h037 #1 = Daily, 2 = Often, 3 = Sometimes, 4 = Not at all, 9 = DK, 0 = other, any negative value = missing 
alspac_t4$j042 #1 = Daily, 2 = Often, 3 = Sometimes, 4 = Not at all, 9 = DK, 0 = other, any negative value = missing 
alspac_t4$k1042 #1 = Daily, 2 = Often, 3 = Sometimes, 4 = Not at all, 5 = Once only, 9 = DK, 0 = other, any negative value = missing 
alspac_t4$l3042 #1 = Daily, 2 = Often, 3 = Sometimes, 4 = Not at all,  5= Once only, 9 = DK, 0 = other, any negative value = missing 
alspac_t4$p1052 #1 = Daily, 2 = Often, 3 = Sometimes, 4 = Not at all,  5= Once only, any negative value = missing 

# GROUP 4
alspac_t4$e203 #1 = Yes, 2 = No, any negative value = missing
alspac_t4$f067a #1 = Yes,2 = No, any negative value = missing
alspac_t4$f069a #1 = Yes,2 = No, any negative value = missing
alspac_t4$g053a #1 = Yes,2 = No, any negative value = missing
alspac_t4$g056a #1 = Yes,2 = No, any negative value = missing
alspac_t4$j048a  #1 = Yes,2 = No, any negative value = missing
alspac_t4$j051a  #1 = Yes,2 = No, any negative value = missing

# GROUP 5
alspac_t4$h043 #4= No, 1-3 = Yes varying frequency, 9 = DK, 0 = other, any negative value = missing
alspac_t4$h046 #4= No, 1-3 = Yes varying frequency, 9 = DK, 0 = other, any negative value = missing
alspac_t4$k1050 #4 = No, 1-3 = Yes varying frequency, 5 = Once, 9 = DK, 0 = other, any negative value = missing
alspac_t4$k1053 #4 = No, 1-3 = Yes varying frequency, 5 = Once, 9 = DK, 0 = other, any negative value = missing
alspac_t4$l3050 #4 = No, 1-3 = Yes varying frequency, 5 = Once, 9 = DK, 0 = other, any negative value = missing
alspac_t4$l3053 #4 = No, 1-3 = Yes varying frequency, 5 = Once, 9 = DK, 0 = other, any negative value = missing
alspac_t4$p1060 #4 = No, 1-3 = Yes varying frequency, 5 = Once, 9 = DK, 0 = other, any negative value = missing
alspac_t4$p1063 #4 = No, 1-3 = Yes varying frequency, 5 = Once, 9 = DK, 0 = other, any negative value = missing

# GROUP 6
alspac_t4$f527 #-2 = No partner, #3 = No, 1-2 = Yes varying source, 9 = DK, 0 = other, any other negative value = missing
alspac_t4$g613 #-2 = No partner, #3 = No, 1-2 = Yes varying source, 9 = DK, 0 = other, any other negative value = missing
alspac_t4$h498 #-2 = No partner, #3 = No, 1-2 = Yes varying source, 9 = DK, 0 = other, any other negative value = missing
alspac_t4$j616 #-2 = No partner, #3 = No, 1-2 = Yes varying source, 9 = DK, 0 = other, any other negative value = missing
alspac_t4$l6032 #Unclear missing partner not using 
alspac_t4$p3032 #Unclear missing partner not using 

## GROUP 1
# Variables to recode
vars <- c("n1057", "n1058")

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == 3 ~ 0,         
      alspac_t4[[var]] %in% 1:2 ~ 1,     
      alspac_t4[[var]] == 0 ~ NA_real_,  
      alspac_t4[[var]] == 9 ~ NA_real_,  
      alspac_t4[[var]] < 0 ~ NA_real_,   
      TRUE ~ NA_real_                     
    )
  }
}
# Check original and recoded variables for one example
cat("Original variable n1057:\n")
print(table(alspac_t4$n1057, useNA = "ifany"))
cat("\nRecoded variable n1057_r:\n")
print(table(alspac_t4$n1057_r, useNA = "ifany"))

## GROUP 2
# Variables to recode
vars <- c("p1022", "r2017", "r2018", "s1022", "k1022", "l3022")

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == 3 ~ 0,         
      alspac_t4[[var]] %in% 0:2 ~ 1,     
      alspac_t4[[var]] == 9 ~ NA_real_,  
      alspac_t4[[var]] < 0 ~ NA_real_,   
      TRUE ~ NA_real_                     
    )
  }
}
# Check original and recoded variables for one example
cat("Original variable p1022:\n")
print(table(alspac_t4$p1022, useNA = "ifany"))
cat("\nRecoded variable p1022_r:\n")
print(table(alspac_t4$p1022_r, useNA = "ifany"))

## GROUP 3
# Variables to recode
vars <- c("e192", "f061", "g047", "h037", "j042", "k1042", "l3042", "p1052")

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == 1 ~ 1,         
      alspac_t4[[var]] %in% 2:5 ~ 0,     
      alspac_t4[[var]] == 9 ~ NA_real_,  
      alspac_t4[[var]] < 0 ~ NA_real_,   
      TRUE ~ NA_real_                     
    )
  }
}
# Check original and recoded variables for one example
cat("Original variable e192:\n")
print(table(alspac_t4$e192, useNA = "ifany"))
cat("\nRecoded variable e192_r:\n")
print(table(alspac_t4$e192_r, useNA = "ifany"))

## GROUP 4
# Variables to recode
vars <- c("e203", "f067a", "f069a", "g053a", "g056a", "j048a", "j051a")

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == 1 ~ 1,         
      alspac_t4[[var]] == 2 ~ 0,  
      alspac_t4[[var]] < 0 ~ NA_real_,   
      TRUE ~ NA_real_                     
    )
  }
}
# Check original and recoded variables for one example
cat("Original variable e203:\n")
print(table(alspac_t4$e203, useNA = "ifany"))
cat("\nRecoded variable e203_r:\n")
print(table(alspac_t4$e203_r, useNA = "ifany"))

## GROUP 5
# Variables to recode
vars <- c("h043", "h046", "k1050", "k1053", "l3050", "l3053", "p1060", "p1063")

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] %in% 1:3 ~ 1,         
      alspac_t4[[var]] == 4 | alspac_t4[[var]] == 5 ~ 0,     
      alspac_t4[[var]] == 0 ~ NA_real_,   
      alspac_t4[[var]] == 9 ~ NA_real_,   
      alspac_t4[[var]] < 0 ~ NA_real_,   
      TRUE ~ NA_real_                     
    )
  }
}
# Check original and recoded variables for one example
cat("Original variable h043:\n")
print(table(alspac_t4$h043, useNA = "ifany"))
cat("\nRecoded variable h043_r:\n")
print(table(alspac_t4$h043_r, useNA = "ifany"))

## GROUP 6
# Variables to recode
vars <- c("f527", "g613", "h498", "j616")

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == -2 ~ 0,         
      alspac_t4[[var]] == 3  ~ 0,     
      alspac_t4[[var]] %in% 1:2 ~ 1,   
      alspac_t4[[var]] == 0 ~ NA_real_,   
      alspac_t4[[var]] == 9 ~ NA_real_,   
      alspac_t4[[var]] < 0 ~ NA_real_,   
      TRUE ~ NA_real_                     
    )
  }
}
# Check original and recoded variables for one example
cat("Original variable f527:\n")
print(table(alspac_t4$f527, useNA = "ifany"))
cat("\nRecoded variable f527_r:\n")
print(table(alspac_t4$f527_r, useNA = "ifany"))

## OVERALL SUSBTANCE ABUSE ##

# For indicator 1 (e192  and e203 )
alspac_t4$sub1_n_valid <- apply(
  alspac_t4[, paste0(c("e192", "e203"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 2 (f061 and f067a and f069a and f527)
alspac_t4$sub2_n_valid <- apply(
  alspac_t4[, paste0(c("f061", "f067a", "f069a", "f527"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 3 (g047 and g053a and g056a and g613)
alspac_t4$sub3_n_valid <- apply(
  alspac_t4[, paste0(c("g047", "g053a", "g056a", "g613"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 4 (h037 and h043  and h046  and h498 )
alspac_t4$sub4_n_valid <- apply(
  alspac_t4[, paste0(c("h037", "h043", "h046", "h498"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 5 (j042  and j048a   and j051a   and j616  )
alspac_t4$sub5_n_valid <- apply(
  alspac_t4[, paste0(c("j042", "j048a", "j051a", "j616"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 6 (k1022  and k1042    and k1050    and k1053   )
alspac_t4$sub6_n_valid <- apply(
  alspac_t4[, paste0(c("k1022", "k1042", "k1050", "k1053"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 7 (l3022 and l3042 and l3050 and l3053)
alspac_t4$sub7_n_valid <- apply(
  alspac_t4[, paste0(c("l3022", "l3042", "l3050", "l3053"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 8 (n1057 and n1058)
alspac_t4$sub8_n_valid <- apply(
  alspac_t4[, paste0(c("n1057", "n1058"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 9 (p1022 and p1052 and p1060 and p1063)
alspac_t4$sub9_n_valid <- apply(
  alspac_t4[, paste0(c("p1022", "p1052", "p1060", "p1063"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 10 (r2017 and r2018)
alspac_t4$sub10_n_valid <- apply(
  alspac_t4[, paste0(c("r2017", "r2018"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 11 (s1022)
alspac_t4$sub11_n_valid <- !is.na(alspac_t4$s1022_r)

# Sum up valid responses across all groups
alspac_t4$sub_n_valid <- apply(
  alspac_t4[, c("sub1_n_valid", "sub2_n_valid", "sub3_n_valid", "sub4_n_valid", "sub5_n_valid", "sub6_n_valid", "sub7_n_valid",
                 "sub8_n_valid", "sub9_n_valid", "sub10_n_valid", "sub11_n_valid"
  )], 
  1, 
  sum
)

# Check distribution
summary(alspac_t4$sub_n_valid)
table(alspac_t4$sub_n_valid, useNA = "ifany")

# Creating indicators of presence/absence of ACE for each "timepoint"/source of ACE information
# For timepoint 1 (e192 and e203)
alspac_t4$sub1_any <- apply(
  alspac_t4[, paste0(c("e192", "e203"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)

# For timepoint 2 (f061 and f067a and f069a and f527 )
alspac_t4$sub2_any <- apply(
  alspac_t4[, paste0(c("f061", "f067a", "f069a", "f527"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)

# For timepoint 3 (g047 and g053a and g056a and g613)
alspac_t4$sub3_any <- apply(
  alspac_t4[, paste0(c("g047", "g053a", "g056a", "g613"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)

# For timepoint 4 (h037 and h043 and h046 and h498)
alspac_t4$sub4_any <- apply(
  alspac_t4[, paste0(c("h037", "h043", "h046", "h498"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)

# For timepoint 5 (j042 and j048a and j051a and j616)
alspac_t4$sub5_any <- apply(
  alspac_t4[, paste0(c("j042", "j048a", "j051a", "j616"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)

# For timepoint 6 (k1022 and k1042 and k1050 and k1053)
alspac_t4$sub6_any <- apply(
  alspac_t4[, paste0(c("k1022", "k1042", "k1050", "k1053"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)

# For timepoint 7 (l3022 and l3042 and l3050 and l3053)
alspac_t4$sub7_any <- apply(
  alspac_t4[, paste0(c("l3022", "l3042", "l3050", "l3053"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)

# For timepoint 8 (n1057 and n1058)
alspac_t4$sub8_any <- apply(
  alspac_t4[, paste0(c("n1057", "n1058"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)

# For timepoint 9 (p1022 and p1052 and p1060 and p1063)
alspac_t4$sub9_any <- apply(
  alspac_t4[, paste0(c("p1022", "p1052", "p1060", "p1063"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)

# For timepoint 10 (r2017 and r2018)
alspac_t4$sub10_any <- apply(
  alspac_t4[, paste0(c("r2017", "r2018"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)

# For single-item timepoints, we just use the recoded variable directly
alspac_t4$sub11_any <- alspac_t4$s1022_r

# Create summary score counting exposures across all timepoints
alspac_t4$sub_summary <- apply(
  alspac_t4[, c("sub1_any", "sub2_any", "sub3_any", "sub4_any", "sub5_any", 
                 "sub6_any", "sub7_any", "sub8_any", "sub9_any", "sub10_any", "sub11_any")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_  # If all values are NA, return NA
    } else {
      sum(x == 1, na.rm = TRUE)  # Sum number of timepoints where exposure occurred
    }
  }
)

# Check distribution
table(alspac_t4$sub_summary, useNA = "ifany")

# Create sub_valid variable
alspac_t4$sub_valid <- ifelse(
  alspac_t4$sub_n_valid >= 6,  # At least 6 valid waves (>50% of 11)
  alspac_t4$sub_summary,
  NA_real_
)

# Check distribution
table(alspac_t4$sub_valid, useNA = "ifany")

# Create binary exposure from sub_valid variable 
alspac_t4 <- alspac_t4 %>%
  mutate(
    sub_binary = case_when(
      sub_valid >= 1 ~ 1,  # At least one exposure to pa
      sub_valid == 0 ~ 0,
      is.na(sub_valid) ~ NA
    ))

# Check distribution
table(alspac_t4$sub_binary, useNA = "ifany") 

#------------------------------#

# HARSH PARENTING #

# Checking codebooks
# GROUP 1
alspac_t4$e438 #5 = Did not happen, 1-4 = Yes varying impact, 0 = other, 9 = DK, any negative value = missing
alspac_t4$f257 #5 = Did not happen, 1-4 = Yes varying impact, 9 = DK, any negative value = missing
alspac_t4$f258 #5 = Did not happen, 1-4 = Yes varying impact, 9 = DK, any negative value = missing
alspac_t4$g337 #5 = Did not happen, 1-4 = Yes varying impact, 0 = other, 9 = DK, any negative value = missing
alspac_t4$g338 #5 = Did not happen, 1-4 = Yes varying impact, 0 = other, 9 = DK, any negative value = missing
alspac_t4$h247 #5 = Did not happen, 1-4 = Yes varying impact, 0 = other, 9 = DK, any negative value = missing
alspac_t4$h248 #5 = Did not happen, 1-4 = Yes varying impact, 0 = other, 9 = DK, any negative value = missing
alspac_t4$j337 #5 = Did not happen, 1-4 = Yes varying impact, 0 = no/missing, 9 = DK, any negative value = missing
alspac_t4$j338  #5 = Did not happen, 1-4 = Yes varying impact, 0 = no/missing, 9 = DK, any negative value = missing
alspac_t4$k4037 #5 = Did not happen, 1-4 = Yes varying impact, 0 = other, 9 = DK, any negative value = missing
alspac_t4$k4038 #5 = Did not happen, 1-4 = Yes varying impact, 0 = other, 9 = DK, any negative value = missing
alspac_t4$l4037 #5 = Did not happen, 1-4 = Yes varying impact, 0 = other, 9 = DK, any negative value = missing
alspac_t4$l4038 #5 = Did not happen, 1-4 = Yes varying impact, 0 = other, 9 = DK, any negative value = missing

alspac_t4$e438

# GROUP 2
alspac_t4$p2037 #4 = Did not happen, 1-3 = Yes varying time, 0 = other, any negative value = missing
alspac_t4$p2038 #4 = Did not happen, 1-3 = Yes varying time, 0 = other, any negative value = missing
alspac_t4$r5037 #4 = Did not happen, 1-3 = Yes varying time, 9 = DK, 0 = other, any negative value = missing
alspac_t4$r5038 #4 = Did not happen, 1-3 = Yes varying time, 9 = DK, 0 = other, any negative value = missing

# For variables in Group 1
# Variables to recode
vars <- c("e438", "f257", "f258", "g337", "g338", "h247", "h248", "j337", "j338", "k4037", "k4038", "l4037", "l4038")  

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == 5 ~ 0,         # 5 = didn't happen
      alspac_t4[[var]] %in% 1:4 ~ 1,     # 1-4 = happened
      alspac_t4[[var]] == 0 ~ NA_real_,  # 0 = missing/other
      alspac_t4[[var]] == 9 ~ NA_real_,  # 9 = don't know
      alspac_t4[[var]] < 0 ~ NA_real_,   # Negative values = missing
      TRUE ~ NA_real_                      # Everything else to NA
    )
  }
}

# Check original and recoded variables for one example
cat("Original variable f257:\n")
print(table(alspac_t4$f257, useNA = "ifany"))
cat("\nRecoded variable f257_r:\n")
print(table(alspac_t4$f257_r, useNA = "ifany"))

# For variables in Group 2
# Variables to recode
vars <- c("p2037", "p2038", "r5037", "r5038")  

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == 4 ~ 0,         # 4 = didn't happen
      alspac_t4[[var]] %in% 1:3 ~ 1,     # 1-3 = happened
      alspac_t4[[var]] == 0 ~ NA_real_,  # 0 = missing/other
      alspac_t4[[var]] == 9 ~ NA_real_,  # 9 = don't know
      alspac_t4[[var]] < 0 ~ NA_real_,   # Negative values = missing
      TRUE ~ NA_real_                      # Everything else to NA
    )
  }
}

# Check original and recoded variables for one example
cat("Original variable p2037:\n")
print(table(alspac_t4$p2037, useNA = "ifany"))
cat("\nRecoded variable p2037_r:\n")
print(table(alspac_t4$p2037_r, useNA = "ifany"))

## OVERALL HARSH PARENTING ##

# For indicator 1 (f257 and f258)
alspac_t4$hp1_n_valid <- apply(
  alspac_t4[, paste0(c("f257", "f258"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 2 (g337 and g338)
alspac_t4$hp2_n_valid <- apply(
  alspac_t4[, paste0(c("g337", "g338"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 3 (h247 and h248)
alspac_t4$hp3_n_valid <- apply(
  alspac_t4[, paste0(c("h247", "h248"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 4 (j337 and j338)
alspac_t4$hp4_n_valid <- apply(
  alspac_t4[, paste0(c("j337", "j338"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 5 (k4037 and k4038)
alspac_t4$hp5_n_valid <- apply(
  alspac_t4[, paste0(c("k4037", "k4038"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 6 (l4037 and l4038)
alspac_t4$hp6_n_valid <- apply(
  alspac_t4[, paste0(c("l4037", "l4038"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 7 (p2037 and p2038)
alspac_t4$hp7_n_valid <- apply(
  alspac_t4[, paste0(c("p2037", "p2038"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 8 (r5037 and r5038)
alspac_t4$hp8_n_valid <- apply(
  alspac_t4[, paste0(c("r5037", "r5038"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 9 (e438)
alspac_t4$hp9_n_valid <- !is.na(alspac_t4$e438)

# Sum up valid responses across all groups
alspac_t4$hp_n_valid <- apply(
  alspac_t4[, c("hp1_n_valid", "hp2_n_valid", "hp3_n_valid", "hp4_n_valid", "hp5_n_valid", "hp6_n_valid", "hp7_n_valid",
                 "hp8_n_valid", "hp9_n_valid"
  )], 
  1, 
  sum
)

# Check distribution
summary(alspac_t4$hp_n_valid)
table(alspac_t4$hp_n_valid, useNA = "ifany")

# Creating indicators of presence/absence of ACE for each "timepoint"/source of ACE information
# For timepoint 1 (f257 and f258)
alspac_t4$hp1_any <- apply(
  alspac_t4[, paste0(c("f257", "f258"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)
# For timepoint 2 (g337 and g338)
alspac_t4$hp2_any <- apply(
  alspac_t4[, paste0(c("g337", "g338"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)
# For timepoint 3 (h247 and h248)
alspac_t4$hp3_any <- apply(
  alspac_t4[, paste0(c("h247", "h248"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)
# For timepoint 4 (j337 and j338)
alspac_t4$hp4_any <- apply(
  alspac_t4[, paste0(c("j337", "j338"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)
# For timepoint 5 (k4037 and k4038)
alspac_t4$hp5_any <- apply(
  alspac_t4[, paste0(c("k4037", "k4038"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)
# For timepoint 6 (l4037 and l4038)
alspac_t4$hp6_any <- apply(
  alspac_t4[, paste0(c("l4037", "l4038"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)
# For timepoint 7 (p2037 and p2038)
alspac_t4$hp7_any <- apply(
  alspac_t4[, paste0(c("p2037", "p2038"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)
# For timepoint 8 (r5037 and r5038)
alspac_t4$hp8_any <- apply(
  alspac_t4[, paste0(c("r5037", "r5038"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)
# For single-item timepoints, we just use the recoded variable directly
alspac_t4$hp9_any <- alspac_t4$e438_r

# Check distributions
cat("Distribution at timepoint 1:\n")
table(alspac_t4$hp1_any, useNA = "ifany")
cat("\nDistribution at timepoint 9 (single item):\n")
table(alspac_t4$hp9_any, useNA = "ifany")

# Create summary score counting exposures across all timepoints
alspac_t4$hp_summary <- apply(
  alspac_t4[, c("hp1_any", "hp2_any", "hp3_any", "hp4_any", "hp5_any", 
                 "hp6_any", "hp7_any", "hp8_any", "hp9_any")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_  # If all values are NA, return NA
    } else {
      sum(x == 1, na.rm = TRUE)  # Sum number of timepoints where exposure occurred
    }
  }
)

# Check distribution
table(alspac_t4$hp_summary, useNA = "ifany")

# Create hp_valid variable
alspac_t4$hp_valid <- ifelse(
  alspac_t4$hp_n_valid >= 5,  # At least 5 valid waves (>50% of 9)
  alspac_t4$hp_summary,
  NA_real_
)

# Check distribution
table(alspac_t4$hp_valid, useNA = "ifany")

# Create binary exposure from pa_valid variable 
alspac_t4 <- alspac_t4 %>%
  mutate(
    hp_binary = case_when(
      hp_valid >= 1 ~ 1,  # At least one exposure to hp
      hp_valid == 0 ~ 0,
      is.na(hp_valid) ~ NA
    ))

# Check distribution
table(alspac_t4$hp_binary, useNA = "ifany") 

#------------------------------#

# PHYSICAL ABUSE #

# Checking codebooks
#GROUP 1
alspac_t4$f246 #5 = Did not happen, 1-4 = Yes varying impact, 9 = DK, -1 = Missing
alspac_t4$f247 #5 = Did not happen, 1-4 = Yes varying impact, 9 = DK, -1 = Missing
alspac_t4$g326 #5 = Did not happen, 1-4 = Yes varying impact, 9 = DK, 0 = Other
alspac_t4$g327 #5 = Did not happen, 1-4 = Yes varying impact, 9 = DK, 0 = Other
alspac_t4$h236 #5 = Did not happen, 1-4 = Yes varying impact, 9 = DK, 0 = Other, -1 = Not stated
alspac_t4$h237 #5 = Did not happen, 1-4 = Yes varying impact, 9 = DK, 0 = Other, -1 = Not stated
alspac_t4$j326 #5 = Did not happen, 1-4 = Yes varying impact, 0 = No/Missing
alspac_t4$j327 #5 = Did not happen, 1-4 = Yes varying impact, 0 = No/Missing
alspac_t4$kd504a #5 = Did not happen, 1-4 = Yes varying impact, 9 = DK, 0 = Other, -1 = Missing
alspac_t4$kl474 #5 = Did not happen, 1-4 = Yes varying impact, -1 = Missing, -10 = Not completed, -9 = Don't know
alspac_t4$kn4004 #5 = Did not happen, 1-4 = Yes varying impact, 9 = DK, 0 = Other, -1 = Missing, -10 = Not completed, -9 = Don't know
alspac_t4$kt5004 #5 = No, 1-4 = Yes varying impact, 0 = Other, 9 = DK, -1 = No response, -10 = Not completed
alspac_t4$k4026 #5 = Did not happen, 1-4 = Yes varying impact, 0 = Other, -1 = No response, -10 = Not completed
alspac_t4$k4027 #5 = Did not happen, 1-4 = Yes varying impact, 0 = Other, -1 = No response, -10 = Not completed

#GROUP 2
alspac_t4$kf454a #1 = Yes, 2 = No, -1 = Missing
alspac_t4$kj464a #1 = Yes, 2 = No
alspac_t4$kq364a #1 = Yes, 2 = No, -1 = Not stated

#GROUP 3
alspac_t4$p2026 #4 = No, did not happen, 1-3 = Yes, varying ages, -1 = No response, -10 = Not completed
alspac_t4$p2027 #4 = No, did not happen, 1-3 = Yes, varying ages, -1 = No response, -10 = Not completed
alspac_t4$r5026 #4 = No, did not happen, 1-3 = Yes, varying ages, 9 = DK, 0 = Other text, -1 = No response, -10 = Not completed
alspac_t4$r5027 #4 = No, did not happen, 1-3 = Yes, varying ages, 9 = DK, 0 = Other text, -1 = No response, -10 = Not completed

# For variables in Group 1
# Variables to recode
vars <- c("f246", "f247", "g326", "g327", "h236", "h237", "j326", "j327", "kd504a", "kl474", "kn4004", "kt5004", "k4026", "k4027")  

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == 5 ~ 0,         # 5 = didn't happen
      alspac_t4[[var]] %in% 1:4 ~ 1,     # 1-4 = happened
      alspac_t4[[var]] == 0 ~ NA_real_,  # 0 = missing/other
      alspac_t4[[var]] == 9 ~ NA_real_,  # 9 = don't know
      alspac_t4[[var]] < 0 ~ NA_real_,   # Negative values = missing
      TRUE ~ NA_real_                      # Everything else to NA
    )
  }
}

# Check original and recoded variables for one example
cat("Original variable f246:\n")
print(table(alspac_t4$f246, useNA = "ifany"))
cat("\nRecoded variable f246_r:\n")
print(table(alspac_t4$f246_r, useNA = "ifany"))

# For variables in Group 2
# Variables to recode
vars <- c("kf454a", "kj464a", "kq364a")

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == 2 ~ 0,         
      alspac_t4[[var]] == 1 ~ 1,     
      alspac_t4[[var]] < 0 ~ NA_real_,   # Negative values = missing
      TRUE ~ NA_real_                      # Everything else to NA
    )
  }
}

# Check original and recoded variables for one example
cat("Original variable kf454a:\n")
print(table(alspac_t4$kf454a, useNA = "ifany"))
cat("\nRecoded variable kf454a_r:\n")
print(table(alspac_t4$kf454a_r, useNA = "ifany"))

# For variables in Group 3
# Variables to recode
vars <- c("p2026", "p2027", "r5026", "r5027")  

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == 4 ~ 0,         # 4 = didn't happen
      alspac_t4[[var]] %in% 1:3 ~ 1,     # 1-3 = happened
      alspac_t4[[var]] == 0 ~ NA_real_,  # 0 = missing/other
      alspac_t4[[var]] == 9 ~ NA_real_,  # 9 = don't know
      alspac_t4[[var]] < 0 ~ NA_real_,   # Negative values = missing
      TRUE ~ NA_real_                      # Everything else to NA
    )
  }
}

# Check original and recoded variables for one example
cat("Original variable p2026:\n")
print(table(alspac_t4$p2026, useNA = "ifany"))
cat("\nRecoded variable p2026_r:\n")
print(table(alspac_t4$p2026_r, useNA = "ifany"))

## OVERALL PHYSICAL ABUSE ##

# For indicator 1 (f246 and f247)
alspac_t4$pa1_n_valid <- apply(
  alspac_t4[, paste0(c("f246", "f247"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 2 (g326 and g327)
alspac_t4$pa2_n_valid <- apply(
  alspac_t4[, paste0(c("g326", "g327"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 3 (h236 and h237)
alspac_t4$pa3_n_valid <- apply(
  alspac_t4[, paste0(c("h236", "h237"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 4 (j326 and j327)
alspac_t4$pa4_n_valid <- apply(
  alspac_t4[, paste0(c("j326", "j327"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 5 (k4026 and k4027)
alspac_t4$pa5_n_valid <- apply(
  alspac_t4[, paste0(c("k4026", "k4027"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 6 (p2026 and p2027)
alspac_t4$pa6_n_valid <- apply(
  alspac_t4[, paste0(c("p2026", "p2027"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 7 (r5026 and r5027)
alspac_t4$pa7_n_valid <- apply(
  alspac_t4[, paste0(c("r5026", "r5027"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0  # Valid if either item is not NA
)

# For indicator 8 (kd504a)
alspac_t4$pa8_n_valid <- !is.na(alspac_t4$kd504a_r)
# For indicator 9 (kl474)
alspac_t4$pa9_n_valid <- !is.na(alspac_t4$kl474_r)
# For indicator 10 (kn4004)
alspac_t4$pa10_n_valid <- !is.na(alspac_t4$kn4004_r)
# For indicator 11 (kt5004)
alspac_t4$pa11_n_valid <- !is.na(alspac_t4$kt5004_r)
# For indicator 12 (kf454a)
alspac_t4$pa12_n_valid <- !is.na(alspac_t4$kf454a_r)
# For indicator 13 (kj464a)
alspac_t4$pa13_n_valid <- !is.na(alspac_t4$kj464a_r)
# For indicator 14 (kq364a)
alspac_t4$pa14_n_valid <- !is.na(alspac_t4$kq364a_r)

# Sum up valid responses across all groups
alspac_t4$pa_n_valid <- apply(
  alspac_t4[, c("pa1_n_valid", "pa2_n_valid", "pa3_n_valid", "pa4_n_valid", "pa5_n_valid", "pa6_n_valid", "pa7_n_valid",
                 "pa8_n_valid", "pa9_n_valid", "pa10_n_valid", "pa11_n_valid", "pa12_n_valid", "pa13_n_valid", "pa14_n_valid"
  )], 
  1, 
  sum
)

# Check distribution
summary(alspac_t4$pa_n_valid)
table(alspac_t4$pa_n_valid, useNA = "ifany")

# Creating indicators of presence/absence of ACE for each "timepoint"/source of ACE information
# For timepoint 1 (f246 and f247)
alspac_t4$pa1_any <- apply(
  alspac_t4[, paste0(c("f246", "f247"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)
# For timepoint 2 (g326 and g327)
alspac_t4$pa2_any <- apply(
  alspac_t4[, paste0(c("g326", "g327"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)
# For timepoint 3 (h236 and h237)
alspac_t4$pa3_any <- apply(
  alspac_t4[, paste0(c("h236", "h237"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)
# For timepoint 4 (j326 and j327)
alspac_t4$pa4_any <- apply(
  alspac_t4[, paste0(c("j326", "j327"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)
# For timepoint 5 (k4026 and k4027)
alspac_t4$pa5_any <- apply(
  alspac_t4[, paste0(c("k4026", "k4027"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)
# For timepoint 6 (p2026 and p2027)
alspac_t4$pa6_any <- apply(
  alspac_t4[, paste0(c("p2026", "p2027"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)
# For timepoint 7 (r5026 and r5027)
alspac_t4$pa7_any <- apply(
  alspac_t4[, paste0(c("r5026", "r5027"), "_r")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1  # If either item indicates exposure
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0  # If no exposure reported and at least one valid response
      } else {
        NA_real_
      }
    }
  }
)
# For single-item timepoints, we just use the recoded variable directly
alspac_t4$pa8_any <- alspac_t4$kd504a_r
alspac_t4$pa9_any <- alspac_t4$kl474_r
alspac_t4$pa10_any <- alspac_t4$kn4004_r
alspac_t4$pa11_any <- alspac_t4$kt5004_r
alspac_t4$pa12_any <- alspac_t4$kf454a_r
alspac_t4$pa13_any <- alspac_t4$kj464a_r
alspac_t4$pa14_any <- alspac_t4$kq364a_r

# Check distributions
cat("Distribution at timepoint 1:\n")
table(alspac_t4$pa1_any, useNA = "ifany")
cat("\nDistribution at timepoint 9 (single item):\n")
table(alspac_t4$pa9_any, useNA = "ifany")

# Create summary score counting exposures across all timepoints
alspac_t4$pa_summary <- apply(
  alspac_t4[, c("pa1_any", "pa2_any", "pa3_any", "pa4_any", "pa5_any", 
                 "pa6_any", "pa7_any", "pa8_any", "pa9_any", "pa10_any", 
                 "pa11_any", "pa12_any", "pa13_any", "pa14_any")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_  # If all values are NA, return NA
    } else {
      sum(x == 1, na.rm = TRUE)  # Sum number of timepoints where exposure occurred
    }
  }
)

# Check distribution
table(alspac_t4$pa_summary, useNA = "ifany")

# Create pa_valid variable
alspac_t4$pa_valid <- ifelse(
  alspac_t4$pa_n_valid >= 7,  # At least 7 valid waves (50% of 14)
  alspac_t4$pa_summary,
  NA_real_
)

# Check distribution
table(alspac_t4$pa_valid, useNA = "ifany")

# Create binary exposure from pa_valid variable 
alspac_t4 <- alspac_t4 %>%
  mutate(
    pa_binary = case_when(
      pa_valid >= 1 ~ 1,  # At least one exposure to pa
      pa_valid == 0 ~ 0,
      is.na(pa_valid) ~ NA
    ))

# Check distribution
table(alspac_t4$pa_binary, useNA = "ifany") 

#------------------------------#

# HOUSEHOLD MEMBER MENTAL ILLNESS #
# Partner data wasn't sent over with ALSPAC file

# Maternal suicide
# Checking codebook
# GROUP 1:
alspac_t4$f248a #1 = Yes, 2 = No, -1 = missing
alspac_t4$g328a #1 = Yes, 2 = No
alspac_t4$h238a #1 = Yes, 2 = No, -1 = not stated
alspac_t4$j328a #1 = Yes, 0 = No

# GROUP 2:
alspac_t4$k4028 #5 = No, 1-4 = Yes, varying impact, 0 = Other, -1 = No response, - 10 = Not completed
alspac_t4$l4028 #5 = No, 1-4 = Yes, varying impact, 9 = DK, 0 = Other, -1 = No response, - 10 = Not completed
alspac_t4$t3327 #5 = No, 1-4 = Yes, varying impact, -1 = No response, - 10 = Not completed

# GROUP 3: 
alspac_t4$p2028 #4 = No, 1-3 = Yes, varying impact, -1 = No response, - 10 = Not completed
alspac_t4$r5028 #4 = No, 1-3 = Yes, varying impact, 0 = Other, 9 = DK, -1 = No response, - 10 = Not completed

# For variables in Group 1
# Variables to recode
vars <- c("f248a", "g328a", "h238a", "j328a")

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == 1 ~ 1,         # 1 = attempted suicide  
      alspac_t4[[var]] == 0 | alspac_t4[[var]] == 2 ~ 0,  # 0 = didn't attempt suicide
      alspac_t4[[var]] < 0 ~ NA_real_,   # Negative values = missing
      TRUE ~ NA_real_                      # Everything else to NA
    )
  }
}

# Check original and recoded variables for one example
cat("Original variable f248a:\n")
print(table(alspac_t4$f248a, useNA = "ifany"))
cat("\nRecoded variable f248a_r:\n")
print(table(alspac_t4$f248a_r, useNA = "ifany"))

# For variables in Group 2
# Variables to recode
vars <- c("k4028", "l4028", "t3327")  

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == 5 ~ 0,         # 5 = didn't happen
      alspac_t4[[var]] %in% 1:4 ~ 1,     # 1-4 = happened
      alspac_t4[[var]] == 0 ~ NA_real_,  # 0 = missing/other
      alspac_t4[[var]] == 9 ~ NA_real_,  # 9 = don't know
      alspac_t4[[var]] < 0 ~ NA_real_,   # Negative values = missing
      TRUE ~ NA_real_                      # Everything else to NA
    )
  }
}

# Check original and recoded variables for one example
cat("Original variable k4028:\n")
print(table(alspac_t4$k4028, useNA = "ifany"))
cat("\nRecoded variable k4028_r:\n")
print(table(alspac_t4$k4028_r, useNA = "ifany"))

# For variables in Group 3
# Variables to recode
vars <- c("p2028", "r5028")  

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == 4 ~ 0,         # 4 = didn't happen
      alspac_t4[[var]] %in% 1:3 ~ 1,     # 1-3 = happened
      alspac_t4[[var]] == 0 ~ NA_real_,  # 0 = missing/other
      alspac_t4[[var]] == 9 ~ NA_real_,  # 9 = don't know
      alspac_t4[[var]] < 0 ~ NA_real_,   # Negative values = missing
      TRUE ~ NA_real_                      # Everything else to NA
    )
  }
}

# Check original and recoded variables for one example
cat("Original variable r5028:\n")
print(table(alspac_t4$r5028, useNA = "ifany"))
cat("\nRecoded variable r5028_r:\n")
print(table(alspac_t4$r5028_r, useNA = "ifany"))

# Self-report psychiatric condition (i.e., schizophrenia, depression, anxiety)
# Checking codebook
# GROUP 1:
alspac_t4$f020 #3 = No/never, 1-2 = Yes, varying source, 9 = DK, 0 = Other, any minus number = missing
alspac_t4$f021 #3 = No/never, 1-2 = Yes, varying source, 9 = DK, 0 = Other, any minus number = missing
alspac_t4$g020 #3 = No/never, 1-2 = Yes, varying source, any minus number = missing
alspac_t4$g021 #3 = No/never, 1-2 = Yes, varying source, any minus number = missing
alspac_t4$h012 #3 = No/never, 1-2 = Yes, varying source, 9 = DK, 0 = Other, any minus number = missing
alspac_t4$h013 #3 = No/never, 1-2 = Yes, varying source, 9 = DK, 0 = Other, any minus number = missing
alspac_t4$j011 #3 = No/never, 1-2 = Yes, varying source, 9 = DK, 0 = Other, any minus number = missing
alspac_t4$j012 #3 = No/never, 1-2 = Yes, varying source, 9 = DK, 0 = Other, any minus number = missing
alspac_t4$k1010 #3 = No/never, 1-2 = Yes, varying source, 9 = DK, 0 = Other, any minus number = missing
alspac_t4$k1011 #3 = No/never, 1-2 = Yes, varying source, 9 = DK, 0 = Other, any minus number = missing
alspac_t4$k1020 #3 = No/never, 1-2 = Yes, varying source, 9 = DK, 0 = Other, any minus number = missing
alspac_t4$l3010 #3 = No/never, 1-2 = Yes, varying source, 9 = DK, 0 = Other, any minus number = missing
alspac_t4$l3011 #3 = No/never, 1-2 = Yes, varying source, 9 = DK, 0 = Other, any minus number = missing
alspac_t4$l3020 #3 = No/never, 1-2 = Yes, varying source, 9 = DK, 0 = Other, any minus number = missing
alspac_t4$n1062 #3 = No/never, 1-2 = Yes, varying times, 9 = DK, 0 = Other, any minus number = missing
alspac_t4$p1010 #3 = No/never, 1-2 = Yes, varying source, any minus number = missing
alspac_t4$p1011 #3 = No/never, 1-2 = Yes, varying source, any minus number = missing
alspac_t4$p1020 #3 = No/never, 1-2 = Yes, varying source, 9 = DK, any minus number = missing
alspac_t4$r2019 #3 = No/never, 1-2 = Yes, varying source, 9 = DK, 0 = Other, any minus number = missing
alspac_t4$r2021 #3 = No/never, 1-2 = Yes, varying times, 9 = DK, any minus number = missing
alspac_t4$r2022 #3 = No/never, 1-2 = Yes, varying times, 9 = DK, 0 = Other, any minus number = missing
alspac_t4$s1010 #3 = No/never, 1-2 = Yes, varying source, 0 = Other, any minus number = missing
alspac_t4$s1011 #3 = No/never, 1-2 = Yes, varying source, 0 = Other, any minus number = missing
alspac_t4$s1020 #3 = No/never, 1-2 = Yes, varying source, 0 = Other, any minus number = missing

# GROUP 2:
alspac_t4$n1059 #3 = No, never, 0-2 = Yes, varying time, 9 = DK, any minus number = missing

# For variables in Group 1
# Variables to recode
vars <- c("f020","f021", "g020", "g021", "h012","h013", "j011", "j012", "k1010", "k1011", "k1020", "l3010", "l3011", "l3020", "n1062",
          "p1010", "p1011", "p1020", "r2019",
          "r2021", "r2022", "s1010", "s1011", "s1020")  

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == 3 ~ 0,         # 3 = no/never
      alspac_t4[[var]] %in% 1:2 ~ 1,     # 1-2 = yes but varying time or source
      alspac_t4[[var]] == 0 ~ NA_real_,  # 0 = missing/other
      alspac_t4[[var]] == 9 ~ NA_real_,  # 9 = don't know
      alspac_t4[[var]] < 0 ~ NA_real_,   # Negative values = missing
      TRUE ~ NA_real_                      # Everything else to NA
    )
  }
}

# Check original and recoded variables for one example
cat("Original variable f020:\n")
print(table(alspac_t4$f020, useNA = "ifany"))
cat("\nRecoded variable f020_r:\n")
print(table(alspac_t4$f020_r, useNA = "ifany"))

# For variables in Group 2
# Variables to recode
vars <- c("n1059")  

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == 3 ~ 0,         # 3 = no/never
      alspac_t4[[var]] %in% 0:2 ~ 1,     # 0-2 = yes but varying time or source 
      alspac_t4[[var]] == 9 ~ NA_real_,  # 9 = don't know
      alspac_t4[[var]] < 0 ~ NA_real_,   # Negative values = missing
      TRUE ~ NA_real_                      # Everything else to NA
    )
  }
}

# Check original and recoded variable
cat("Original variable n1059:\n")
print(table(alspac_t4$n1059, useNA = "ifany"))
cat("\nRecoded variable n1059_r:\n")
print(table(alspac_t4$n1059_r, useNA = "ifany"))

# Self-report psychiatric medication (i.e., anti-depressant)
# Checking codebook
# GROUP 1: 
alspac_t4$e327 #1 = Yes, 2 = No, any negative value = missing
alspac_t4$f063a #1 = Yes, 2 = No, any negative value = missing
alspac_t4$g049a #1 = Yes, 2 = No, any negative value = missing
alspac_t4$h039a #1 = Yes, 2 = No, any negative value = missing
alspac_t4$j044a #1 = Yes, 2 = No, any negative value = missing

# GROUP 2: 
alspac_t4$k1044 #1-3 = Yes, varying frequency, 4-5 = no, 9 = don't know, 0 = other, any negative value = missing
alspac_t4$l3044 #1-3 = Yes, varying frequency, 4-5 = no, 9 = don't know, 0 = other, any negative value = missing
alspac_t4$p1054 #1-3 = Yes, varying frequency, 4-5 = no, 9 = don't know, 0 = other, any negative value = missing

# For variables in Group 1
# Variables to recode
vars <- c("e327", "f063a", "g049a", "h039a", "j044a")

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == 2 ~ 0,         # 3 = no/never
      alspac_t4[[var]] == 1 ~ 1,  # 1 = yes # 9 = don't know
      alspac_t4[[var]] < 0 ~ NA_real_,   # Negative values = missing
      TRUE ~ NA_real_                      # Everything else to NA
    )
  }
}

# Check original and recoded variable
cat("Original variable e327:\n")
print(table(alspac_t4$e327, useNA = "ifany"))
cat("\nRecoded variable e327_r:\n")
print(table(alspac_t4$e327_r, useNA = "ifany"))

# For variables in Group 2
# Variables to recode
vars <- c("k1044", "l3044", "p1054")  

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[var]] == 4 | alspac_t4[[var]] == 5 ~ 0,         # 4 or 5 = no/never
      alspac_t4[[var]] %in% 1:3 ~ 1,                 # 1 -3 = varying frequency
      alspac_t4[[var]] == 0 ~ NA_real_, #0 = other
      alspac_t4[[var]] == 9 ~ NA_real_,  # 9 = don't know
      alspac_t4[[var]] < 0 ~ NA_real_,   # Negative values = missing
      TRUE ~ NA_real_                      # Everything else to NA
    )
  }
}

# Check original and recoded variable
cat("Original variable k1044:\n")
print(table(alspac_t4$k1044, useNA = "ifany"))
cat("\nRecoded variable k1044_r:\n")
print(table(alspac_t4$k1044_r, useNA = "ifany"))

# Edinburgh Postnatal Depression Score (EPDS)
# Check against codebook
# GROUP 1:
alspac_t4$e390 # Numeric, any negative = missing
alspac_t4$f200 # Numeric, any negative = missing
alspac_t4$g290 # Numeric, any negative = missing
alspac_t4$h200a # Numeric, any negative = missing

vars <- c("e390", "f200", "g290", "h200a")

# Create new recoded variables
for(var in vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_r")
    alspac_t4[[new_var]] <- ifelse(
      alspac_t4[[var]] < 0,
      NA_real_,
      alspac_t4[[var]]
    )
  }
}

# Check original and recoded variables
for(var in vars) {
  cat("\nOriginal variable", var, ":\n")
  print(table(alspac_t4[[var]], useNA = "ifany"))
  cat("\nRecoded variable", paste0(var, "_r"), ":\n")
  print(table(alspac_t4[[paste0(var, "_r")]], useNA = "ifany"))
  cat("\n-------------------\n")
}

# For EPDS variables, create threshold indicators
epds_vars <- c("e390", "f200", "g290", "h200a")

# Create new threshold variables
for(var in epds_vars) {
  if(var %in% names(alspac_t4)) {
    new_var <- paste0(var, "_t")  # '_t' for threshold
    alspac_t4[[new_var]] <- case_when(
      alspac_t4[[paste0(var, "_r")]] >= 13 ~ 1,     # Score of 13 or more
      alspac_t4[[paste0(var, "_r")]] < 13 ~ 0,      # Score less than 13
      is.na(alspac_t4[[paste0(var, "_r")]]) ~ NA_real_  # NA if original is NA
    )
  }
}

# Check distributions
for(var in epds_vars) {
  cat("\nEPDS threshold for", var, ":\n")
  print(table(alspac_t4[[paste0(var, "_t")]], useNA = "ifany"))
}

## OVERALL HOUSEHOLD MENTAL ILLNESS#

# MISSING INDICATOR #

# E wave
alspac_t4$mh_e_valid <- apply(
  alspac_t4[, paste0(c("e327", "e390"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0
)

# F wave
alspac_t4$mh_f_valid <- apply(
  alspac_t4[, paste0(c("f248a", "f020", "f021", "f063a", "f200"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0
)

# G wave
alspac_t4$mh_g_valid <- apply(
  alspac_t4[, paste0(c("g328a", "g020", "g021", "g049a", "g290"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0
)

# H wave
alspac_t4$mh_h_valid <- apply(
  alspac_t4[, paste0(c("h238a", "h012", "h013", "h039a", "h200a"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0
)

# J wave
alspac_t4$mh_j_valid <- apply(
  alspac_t4[, paste0(c("j328a", "j011", "j012", "j044a"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0
)

# K wave
alspac_t4$mh_k_valid <- apply(
  alspac_t4[, paste0(c("k4028", "k1010", "k1011", "k1020", "k1044"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0
)

# L wave
alspac_t4$mh_l_valid <- apply(
  alspac_t4[, paste0(c("l4028", "l3010", "l3011", "l3020", "l3044"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0
)

# N wave
alspac_t4$mh_n_valid <- apply(
  alspac_t4[, paste0(c("n1059", "n1062"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0
)

# P wave
alspac_t4$mh_p_valid <- apply(
  alspac_t4[, paste0(c("p2028", "p1010", "p1011", "p1020", "p1054"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0
)

# R wave
alspac_t4$mh_r_valid <- apply(
  alspac_t4[, paste0(c("r5028", "r2019", "r2021", "r2022"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0
)

# S wave
alspac_t4$mh_s_valid <- apply(
  alspac_t4[, paste0(c("s1010", "s1011", "s1020"), "_r")], 
  1, 
  function(x) if(any(!is.na(x))) 1 else 0
)

# T wave
alspac_t4$mh_t_valid <- !is.na(alspac_t4$t3327_r)

# Sum up total number of valid waves
alspac_t4$mh_total_valid <- rowSums(alspac_t4[, paste0("mh_", c("e", "f", "g", "h", "j", "k", "l", "n", "p", "r", "s", "t"), "_valid")])

# Check distributions
for(wave in c("e", "f", "g", "h", "j", "k", "l", "n", "p", "r", "s", "t")) {
  cat("\nValid responses in wave", wave, ":\n")
  print(table(alspac_t4[[paste0("mh_", wave, "_valid")]], useNA = "ifany"))
}

cat("\nTotal number of valid waves:\n")
print(table(alspac_t4$mh_total_valid, useNA = "ifany"))

# SUMMARY SCORE #

# E wave any mental health problem
alspac_t4$mh_e_any <- apply(
  alspac_t4[, c("e327_r", "e390_t")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0
      } else {
        NA_real_
      }
    }
  }
)

# F wave any mental health problem
alspac_t4$mh_f_any <- apply(
  alspac_t4[, c("f248a_r", "f020_r", "f021_r", "f063a_r", "f200_t")], 
  1,
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0
      } else {
        NA_real_
      }
    }
  }
)

# G wave any mental health problem
alspac_t4$mh_g_any <- apply(
  alspac_t4[, c("g328a_r", "g020_r", "g021_r", "g049a_r", "g290_t")], 
  1,
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0
      } else {
        NA_real_
      }
    }
  }
)

# H wave any mental health problem
alspac_t4$mh_h_any <- apply(
  alspac_t4[, c("h238a_r", "h012_r", "h013_r", "h039a_r", "h200a_t")], 
  1,
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0
      } else {
        NA_real_
      }
    }
  }
)

# J wave any mental health problem (no EPDS)
alspac_t4$mh_j_any <- apply(
  alspac_t4[, c("j328a_r", "j011_r", "j012_r", "j044a_r")], 
  1,
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0
      } else {
        NA_real_
      }
    }
  }
)

# K wave any mental health problem (no EPDS)
alspac_t4$mh_k_any <- apply(
  alspac_t4[, c("k4028_r", "k1010_r", "k1011_r", "k1020_r", "k1044_r")], 
  1,
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0
      } else {
        NA_real_
      }
    }
  }
)

# L wave any mental health problem (no EPDS)
alspac_t4$mh_l_any <- apply(
  alspac_t4[, c("l4028_r", "l3010_r", "l3011_r", "l3020_r", "l3044_r")], 
  1,
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0
      } else {
        NA_real_
      }
    }
  }
)

# N wave any mental health problem (no EPDS)
alspac_t4$mh_n_any <- apply(
  alspac_t4[, c("n1059_r", "n1062_r")], 
  1,
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0
      } else {
        NA_real_
      }
    }
  }
)

# P wave any mental health problem (no EPDS)
alspac_t4$mh_p_any <- apply(
  alspac_t4[, c("p2028_r", "p1010_r", "p1011_r", "p1020_r", "p1054_r")], 
  1,
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0
      } else {
        NA_real_
      }
    }
  }
)

# R wave any mental health problem (no EPDS)
alspac_t4$mh_r_any <- apply(
  alspac_t4[, c("r5028_r", "r2019_r", "r2021_r", "r2022_r")], 
  1,
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0
      } else {
        NA_real_
      }
    }
  }
)

# S wave any mental health problem (no EPDS)
alspac_t4$mh_s_any <- apply(
  alspac_t4[, c("s1010_r", "s1011_r", "s1020_r")], 
  1,
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      if(any(x == 1, na.rm = TRUE)) {
        1
      } else if(all(x == 0 | is.na(x)) && any(x == 0)) {
        0
      } else {
        NA_real_
      }
    }
  }
)

# T wave any mental health problem (single variable, no EPDS)
alspac_t4$mh_t_any <- alspac_t4$t3327_r

# Create summary score across all waves
alspac_t4$mh_summary <- apply(
  alspac_t4[, paste0("mh_", c("e", "f", "g", "h", "j", "k", "l", "n", "p", "r", "s", "t"), "_any")], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      NA_real_
    } else {
      sum(x == 1, na.rm = TRUE)
    }
  }
)

# Check distributions
table(alspac_t4$mh_summary, useNA = "ifany")

## VALID INDIACTOR ##

# Create mh_valid variable
alspac_t4$mh_valid <- ifelse(
  alspac_t4$mh_total_valid >= 6,  # At least 6 valid waves (50% of 12)
  alspac_t4$mh_summary,
  NA_real_
)

# Check distribution
table(alspac_t4$mh_valid, useNA = "ifany")

# Create binary exposure from pa_valid variable 
alspac_t4 <- alspac_t4 %>%
  mutate(
    mh_binary = case_when(
      mh_valid >= 1 ~ 1,  # At least one exposure to mh
      mh_valid == 0 ~ 0,
      is.na(mh_valid) ~ NA
    ))

# Check distribution
table(alspac_t4$mh_binary, useNA = "ifany") 

#------------------------------#

### OVERALL ACE SUMMARY SCORE ###

# Create ACE score summing across all binary indicators
alspac_t4$ace_summary <- apply(
  alspac_t4[, c("hp_binary", "pa_binary", "mh_binary", "ipv_binary", 
                 "bully_binary", "sa_binary", "sub_binary")], 
  1, 
  function(x) {
    # Count number of valid responses
    n_valid <- sum(!is.na(x))
    
    if(n_valid >= 4) {  # If 4 or more valid indicators
      sum(x == 1, na.rm = TRUE)  # Sum the number of ACEs present
    } else {
      NA_real_  # If fewer than 4 valid indicators, return NA
    }
  }
)

# Check distribution
table(alspac_t4$ace_summary, useNA = "ifany") #NA = 299

# Create collapsed ACE score
alspac_t4$ace_score <- case_when(
  is.na(alspac_t4$ace_summary) ~ NA_real_,
  alspac_t4$ace_summary == 0 ~ 0,
  alspac_t4$ace_summary == 1 ~ 1,
  alspac_t4$ace_summary == 2 ~ 2,
  alspac_t4$ace_summary == 3 ~ 3,
  alspac_t4$ace_summary >= 4 ~ 4
)

# Check distributions of both variables
cat("Original ACE summary:\n")
print(table(alspac_t4$ace_summary, useNA = "ifany"))

cat("\nCollapsed ACE score:\n")
print(table(alspac_t4$ace_score, useNA = "ifany"))

#------------------------------#

## DEPRESSION @ BASELINE
# Relevant variable(s) = ccs4500, ccs4501, ccs4503, ccs4504, ccs4505, ccs4506, ccs4508, ccs4509, ccs4511, ccs4512, ccs4513, ccs4514, ccs4515
# Q: "How much have you felt or acted this way in the past two weeks...?"
# C: 1 = True, 2 = Sometimes True, 3 = Not true
# M: -1

# Define sMFQ-13 items
alspac_t4 <- alspac_t4 %>%
  mutate(across(c(ccs4500, ccs4501, ccs4503, ccs4504, ccs4505, ccs4506, 
                  ccs4508, ccs4509, ccs4511, ccs4512, ccs4513, ccs4514, ccs4515),
                ~ case_when(
                  . == -1 ~ NA_real_,
                  . == 1 ~ 2,                    
                  . == 2 ~ 1,
                  . == 3 ~ 0,
                  TRUE ~ as.numeric(.)),
                .names = "smfq_bl_{.col}"))

# Calculate total score
alspac_t4 <- alspac_t4 %>%
  mutate(smfq_bl_total = rowSums(across(starts_with("smfq_bl_")), na.rm = FALSE))

#-----OUTCOME-----#

## DEPRESSION @ FOLLOW-UP
# Relevant variable(s) = FJCI1003 (CIS-R)
# Q: DV:Diagsymp:Participant has symptoms or diagnosis of depression [F17]
# C: 1 = True, 2 = Sometimes True, 3 = Not true
# M: -1

# Recoding
alspac_t4 <- alspac_t4 %>%
  mutate(cisr_dep = factor(case_when(
    FJCI1003 <0 ~ NA_real_,
    FJCI1003 == 1 ~ 1,
    FJCI1003 == 0 ~ 0
  ),
  levels = c(0, 1),
  labels = c("No depression", "Any depression")))

table(alspac_t4$cisr_dep)
sum(is.na(alspac_t4$cisr_dep)) 

## ================== Trim dataset ===============================

# Select prepared dataset
xyz_alspac <- alspac_t4 %>%
  select(new_id, followup_months,  #Sample indicators
         smok_ever, smok_freq, cannabis_ever, cannabis_freq, ct_couse, #Exposures
         age_years, sex, ethnicity, maternal_edu, #Covariates
         neuroticism, adhd_dx_final, ace_score, smfq_bl_total,
         ever_drug, audit_total, anx_dawba, cd_score, sleep_problems,
         cisr_dep, #Outcome
         home_own, dsh, urban, asb_10, fq_tired) # Auxiliary

# Save prepared dataset
saveRDS(xyz_alspac, "[file_path]")

# Tidy environment
rm(list = ls()[!grepl("xyz_alspac", ls())])

## ================== End of script ===============================

# Open script called 'alspac_imputation.R'



#---------SUPPLEMENTARY INFORMATION--------#

## Variable: CO-ADMINISTRATION
## Relevant variable(s): ccs4100
## Q: "If you have ever smoked joints/spliffs, or used a pipe or bong, was the cannabis most commonly mixed with tobacco?"
## C: 1 = Most commonly mixed with tobacco, 2 = most commonly smoked by itself, 3 = never smoked cannabis, 9 = don't know
## M: -11, -10 , -1

# Check against codebook
alspac_t4$ccs4100
alspac_t4$ccs4090

# Generate ROA indicator
alspac_t4 <- alspac_t4 %>%
  mutate(cannabis_roa= case_when(
    is.na(cannabis_ever) ~ NA,
    cannabis_ever == "Never" ~ 0, #Never used
    ccs4090 == 1 | ccs4090 == 2 ~ 4, #Smoked
    ccs4090 == 3 ~ 3, #Eaten
    ccs4090 == 4 ~ 2, #Other
    ccs4090 == 9 ~ 1, #Don't know
    TRUE ~ NA
  ),
  cannabis_roa = factor(cannabis_roa,
                        levels = c(0, 1, 2, 3, 4),           
                        labels = c("Never used", "Unsure of ROA", "Other method", "Eaten", "Smoked")))

table(alspac_t4$cannabis_roa)
sum(is.na(alspac_t4$cannabis_roa)) 

# Generate co-administration indicator
alspac_t4 <- alspac_t4 %>%
  mutate(coadmin = case_when(
    is.na(cannabis_ever) ~ NA,
    is.na(cannabis_roa) ~ NA,
    cannabis_ever == "Never" ~ 0, #Never used
    cannabis_roa == 1 | cannabis_roa == 2 | cannabis_roa == 3 | ccs4100 == 3 ~ 1, #Used but not commonly smoked or don't know
    ccs4100 == 2 ~ 2, # Commonly smoked without tobacco 
    ccs4100 == 1 ~ 3, # Commonly smoked with tobacco 
    TRUE ~ NA
  ),
  coadmin = factor(coadmin,
                   levels = c(0, 1, 2, 3),           
                   labels = c("Never used", "Not smoked or don't know", "Smoked without tobacco", "Smoked with tobacco")))

table(alspac_t4$coadmin)
sum(is.na(alspac_t4$coadmin)) 

# Descriptive tables
coadmin_table <- alspac_t4 %>%
  tbl_summary(
    include = c(coadmin, cannabis_roa, cannabis_ever),
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  modify_header(label = "Cannabis Use Characteristics") %>%
  bold_labels()

stratified_coadmin <- alspac_t4 %>%
  tbl_summary(
    include = c(coadmin, cannabis_roa),
    by = cannabis_ever,
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  modify_header(label = "Cannabis Use Characteristics") %>%
  add_overall() %>%  # Adds total column
  add_n() %>%  # Adds sample size
  add_p() %>%  # Adds p-value for group differences
  bold_labels()

# Raw counts and percentages for co-administration by cannabis use
cat("\nCo-administration by cannabis use status:\n")
print(table(alspac_t4$cannabis_ever, alspac_t4$coadmin, useNA = "ifany"))
cat("\nPercentages:\n")
print(prop.table(table(alspac_t4$cannabis_ever, alspac_t4$coadmin, useNA = "ifany"), margin = 1) * 100)

# Raw counts and percentages for ROA by cannabis use
cat("\nROA by cannabis use status:\n")
print(table(alspac_t4$cannabis_ever, alspac_t4$cannabis_roa, useNA = "ifany"))
cat("\nPercentages:\n")
print(prop.table(table(alspac_t4$cannabis_ever, alspac_t4$cannabis_roa, useNA = "ifany"), margin = 1) * 100)

#---Save into Word Documents---#
setwd("[file_path]")

# Save as Word documents
coadmin_table %>%
  as_flex_table() %>%
  save_as_docx(path = "coadmin_descriptive_table.docx")

stratified_coadmin %>%
  as_flex_table() %>%
  save_as_docx(path = "stratified_coadmin_table.docx")

# Save raw counts and percentages as Excel
# Create data frames from the tables
coadmin_by_use <- as.data.frame.matrix(table(alspac_t4$cannabis_ever, alspac_t4$coadmin, useNA = "ifany"))
coadmin_by_use_pct <- as.data.frame.matrix(prop.table(table(alspac_t4$cannabis_ever, alspac_t4$coadmin, useNA = "ifany"), margin = 1) * 100)

roa_by_use <- as.data.frame.matrix(table(alspac_t4$cannabis_ever, alspac_t4$cannabis_roa, useNA = "ifany"))
roa_by_use_pct <- as.data.frame.matrix(prop.table(table(alspac_t4$cannabis_ever, alspac_t4$cannabis_roa, useNA = "ifany"), margin = 1) * 100)

# Combine into a list for Excel export
tables_list <- list(
  "Coadmin Counts" = coadmin_by_use,
  "Coadmin Percentages" = coadmin_by_use_pct,
  "ROA Counts" = roa_by_use,
  "ROA Percentages" = roa_by_use_pct
)

# Save to Excel
write_xlsx(tables_list, "cannabis_use_tables.xlsx")

#---------------------#
