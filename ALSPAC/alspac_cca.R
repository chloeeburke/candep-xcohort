'''
---
title: "Cross-cohort analysis cannabis depression"
author: Chloe Burke
dataset: ALSPAC
date: "October 2024"
script: "Complete-case analysis"
---
'''

# LOAD PACKAGES
#install.packages("twang")
library(twang)
#install.packages("car")
library(car)
##install.packages("survey")
library(survey)
##install.packages("svydiags")
library(svydiags)
##install.packages("glue")
library(glue)
##install.packages("gtsummary")
library(gtsummary)
##install.packages("officer")
library(officer)
##install.packages("flextable")
library(flextable)

# Note: Replace [file_path] with your local directory containing the ALSPAC data files

## ================= Restricting sample for CCA ================================

# Filtering dataset
alspac_cca <- xyz_alspac %>% 
  filter(cca_status == 1) 

# Using the svydesign function to tell R about design elements of the survey (i.e., null for ALSPAC)
alspac_d_cca<- svydesign(id=~new_id, weights= NULL, strata = NULL, nest = FALSE, data=alspac_cca)

# Checking column names
colnames(alspac_cca)

#----EVER USE---#

# EVER SMOKED #
# Perform survey-weighted logistic regression
# Unadjusted
m1 <- svyglm(cisr_dep ~ smok_ever, design = alspac_d_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(smok_ever ~ "Ever smoked")
  )%>%
  modify_column_hide(p.value)
print(m1)

# Model 4: Fully-adjusted (i.e., pre-baseline Z, other substance use, baseline depression)
m2c <- svyglm(cisr_dep ~ smok_ever + sex + ethnicity +
                maternal_edu + neuroticism + adhd_dx_final + ace_score + ever_drug + audit_total + cannabis_ever +
                smfq_bl_total, design = alspac_d_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = smok_ever,
    label = list(smok_ever ~ "Ever smoked")
  )%>%
  modify_column_hide(p.value)
print(m2c)

# Model 2: Pre-baseline confounding
m2a <- svyglm(cisr_dep ~ smok_ever + sex + ethnicity +
                maternal_edu + neuroticism + adhd_dx_final + ace_score, design = alspac_d_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = smok_ever,
    label = list(smok_ever ~ "Ever smoked")
  )%>%
  modify_column_hide(p.value)
print(m2a)

# Model 3: Pre-baseline + baseline depression 
m2b <- svyglm(cisr_dep ~ smok_ever + sex + ethnicity +
                maternal_edu + neuroticism + adhd_dx_final + ace_score + smfq_bl_total, design = alspac_d_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = smok_ever,
    label = list(smok_ever ~ "Ever smoked")
  )%>%
  modify_column_hide(p.value)
print(m2b)

# Combine the tables into one side by side
evers_comb <- tbl_merge(
  tbls = list(m1, m2a, m2b, m2c), 
  tab_spanner = c("Model 1", "Model 2", "Model 3", "Model 4")  # Optional: add headers to the columns
)

# Print the combined table
print(evers_comb)

#------------------#

# EVER USED CANNABIS #
# Perform survey-weighted logistic regression
# Unadjusted

m3 <- svyglm(cisr_dep ~ cannabis_ever, design = alspac_d_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(cannabis_ever ~ "Ever used cannabis")
  )%>%
  modify_column_hide(p.value)
print(m3)

# Model 4: Fully-adjusted (i.e., pre-baseline Z, other substance use, baseline depression)
m4c <- svyglm(cisr_dep ~ cannabis_ever + sex + ethnicity +
                maternal_edu + neuroticism + adhd_dx_final + ace_score + ever_drug + audit_total + smok_ever +
                smfq_bl_total, design = alspac_d_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = cannabis_ever,
    label = list(cannabis_ever ~ "Ever used cannabis")
  )%>%
  modify_column_hide(p.value)
print(m4c)

# Model 2: Pre-baseline confounding
m4a <- svyglm(cisr_dep ~ cannabis_ever + sex + ethnicity +
                maternal_edu + neuroticism + adhd_dx_final + ace_score, design = alspac_d_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = cannabis_ever,
    label = list(cannabis_ever ~ "Ever used cannabis")
  )%>%
  modify_column_hide(p.value)
print(m4a)

# Model 3: Pre-baseline + baseline depression 
m4b <- svyglm(cisr_dep ~ cannabis_ever + sex + ethnicity +
                maternal_edu + neuroticism + adhd_dx_final + ace_score + smfq_bl_total, design = alspac_d_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = cannabis_ever,
    label = list(cannabis_ever ~ "Ever used cannabis")
  )%>%
  modify_column_hide(p.value)
print(m4b)

# Combine the tables into one side by side
everc_comb <- tbl_merge(
  tbls = list(m3, m4a, m4b, m4c), 
  tab_spanner = c("Model 1", "Model 2", "Model 3", "Model 4")  # Optional: add headers to the columns
)

# Print the combined table
print(everc_comb)

#----FREQUENCY VARIABLES ---#

# FREQUENCY SMOKED #
# Perform survey-weighted logistic regression
# Unadjusted

m5 <- svyglm(cisr_dep ~ smok_freq, design = alspac_d_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(smok_freq ~ "Smoking frequency")
  )%>%
  modify_column_hide(p.value)
print(m5)

#  Model 4: Fully-adjusted (i.e., pre-baseline Z, other substance use, baseline depression)
m6c <- svyglm(cisr_dep ~ smok_freq + sex + ethnicity +
                maternal_edu + neuroticism + adhd_dx_final + ace_score + ever_drug + audit_total + cannabis_freq +
                smfq_bl_total, design = alspac_d_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = smok_freq,
    label = list(smok_freq ~ "Smoking frequency")
  )%>%
  modify_column_hide(p.value)
print(m6c)


# Model 2: Pre-baseline confounding
m6a <- svyglm(cisr_dep ~ smok_freq + sex + ethnicity +
                maternal_edu + neuroticism + adhd_dx_final + ace_score, design = alspac_d_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = smok_freq,
    label = list(smok_freq ~ "Smoking frequency")
  )%>%
  modify_column_hide(p.value)
print(m6a)


# Model 3: Pre-baseline + baseline depression 
m6b <- svyglm(cisr_dep ~ smok_freq + sex + ethnicity +
                maternal_edu + neuroticism + adhd_dx_final + ace_score + smfq_bl_total, design = alspac_d_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = smok_freq,
    label = list(smok_freq ~ "Smoking frequency")
  )%>%
  modify_column_hide(p.value)
print(m6b)

# Combine the tables into one side by side
freqs_comb <- tbl_merge(
  tbls = list(m5, m6a, m6b, m6c), 
  tab_spanner = c("Model 1", "Model 2", "Model 3", "Model 4")  # Optional: add headers to the columns
)

# Print the combined table
print(freqs_comb)

#------------------------#

# FREQUENCY CANNABIS USE #
# Perform survey-weighted logistic regression
# Unadjusted

m7 <- svyglm(cisr_dep ~ cannabis_freq, design = alspac_d_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(cannabis_freq ~ "Cannabis use frequency")
  )%>%
  modify_column_hide(p.value)
print(m7)

# Model 4: Fully-adjusted (i.e., pre-baseline Z, other substance use, baseline depression)
m8c <- svyglm(cisr_dep ~ cannabis_freq + sex + ethnicity +
                maternal_edu + neuroticism + adhd_dx_final + ace_score + ever_drug + audit_total + smok_freq +
                smfq_bl_total, design = alspac_d_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = cannabis_freq,
    label = list(cannabis_freq ~ "Cannabis use frequency")
  )%>%
  modify_column_hide(p.value)
print(m8c)

# Model 2: Pre-baseline confounding
m8a <- svyglm(cisr_dep ~ cannabis_freq + sex + ethnicity +
                maternal_edu + neuroticism + adhd_dx_final + ace_score, design = alspac_d_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = cannabis_freq,
    label = list(cannabis_freq ~ "Cannabis use frequency")
  )%>%
  modify_column_hide(p.value)
print(m8a)

# Model 3: Pre-baseline + baseline depression 
m8b <- svyglm(cisr_dep ~ cannabis_freq + sex + ethnicity +
                maternal_edu + neuroticism + adhd_dx_final + ace_score + smfq_bl_total, design = alspac_d_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = cannabis_freq,
    label = list(cannabis_freq ~ "Cannabis use frequency")
  )%>%
  modify_column_hide(p.value)
print(m8b)

# Combine the tables into one side by side
freqc_comb <- tbl_merge(
  tbls = list(m7, m8a, m8b, m8c), 
  tab_spanner = c("Model 1", "Model 2", "Model 3", "Model 4")  # Optional: add headers to the columns
)

# Print the combined table
print(freqc_comb)

#----CO-USE---#

# CONCURRENT #
# Perform survey-weighted logistic regression
# Unadjusted

m9 <- svyglm(cisr_dep ~ ct_couse, design = alspac_d_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(ct_couse ~ "Concurrent use")
  )%>%
  modify_column_hide(p.value)
print(m9)

# Model 4: Fully-adjusted (i.e., pre-baseline Z, other substance use, baseline depression)
m10c <- svyglm(cisr_dep ~ ct_couse + sex + ethnicity +
                maternal_edu + neuroticism + adhd_dx_final + ace_score + ever_drug + audit_total +
                smfq_bl_total, design = alspac_d_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = ct_couse,
    label = list(ct_couse ~ "Concurrent use")
  )%>%
  modify_column_hide(p.value)
print(m10c)

# Model 2: Pre-baseline confounding
m10a <- svyglm(cisr_dep ~ ct_couse + sex + ethnicity +
                maternal_edu + neuroticism + adhd_dx_final + ace_score, design = alspac_d_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = ct_couse,
    label = list(ct_couse ~ "Concurrent use")
  )%>%
  modify_column_hide(p.value)
print(m10a)

# Model 3: Pre-baseline + baseline depression 
m10b <- svyglm(cisr_dep ~ ct_couse + sex + ethnicity +
                maternal_edu + neuroticism + adhd_dx_final + ace_score + smfq_bl_total, design = alspac_d_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = ct_couse,
    label = list(ct_couse ~ "Concurrent use")
  )

print(m10b)

# Combine the tables into one side by side
couse_comb <- tbl_merge(
  tbls = list(m9, m10a, m10b, m10c), 
  tab_spanner = c("Model 1", "Model 2", "Model 3", "Model 4")  # Optional: add headers to the columns
)

# Print the combined table
print(couse_comb)

## =============== Merging all tables ==========================================

# Merge all regression tables
cca_regs <- tbl_stack(
  tbls = list(evers_comb, freqs_comb, everc_comb, freqc_comb, couse_comb)
)
print(cca_regs)

# Removing p-value
cca_regs %>%
  modify_column_hide(p.value_4) %>%
  as_flex_table() -> cca_regs_flex

# Save combined CCA regression tables
setwd("[file_path]")
doc <- read_docx() %>%
  body_add_flextable(cca_regs_flex)
print(doc, target = "cca_regs_t4.docx")

## ================== End of script ============================================
