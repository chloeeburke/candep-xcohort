'''
---
title: "Cross-cohort analysis cannabis depression"
author: Chloe Burke
dataset: LSAC
date: "October, 2024"
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

# Note: Replace [file_path] with your local directory containing the LSAC data files

## ================== Clear environment ========================================

rm(list = setdiff(ls(), "xyz_lsac"))


## ================= Defining survey design for WII ============================

# Change pcodes to numeric and strata to factors
xyz_lsac$stratum <- haven::as_factor(xyz_lsac$stratum)
xyz_lsac$pcodes <- as.numeric(xyz_lsac$pcodes)

# Specify survey design
lsac_sd <- svydesign(id=~pcodes, weights=~jweights, strata =~stratum, nest = TRUE, survey.lonely.psu = "adjust", data=xyz_lsac)
lsac_cca <- subset(lsac_sd, cca_status == 1)

## ================= Performing regression analysis ============================

#----EVER USE---#

# EVER SMOKED #
# Perform survey-weighted logistic regression
# Unadjusted
m1 <- svyglm(k10_cut ~ smok_ever, design = lsac_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(smok_ever ~ "Ever smoked")
  ) %>%
  modify_column_hide(p.value) 
print(m1)

# Adjusted (pre-baseline + other substance use + bl dep)
m2c <- svyglm(k10_cut ~ smok_ever + sex + ethnicity +
               maternal_edu + adhd_dx_final + ace_types_cat + icneuro_num 
              + cannabis_ever + ever_drug + alc_probs + 
                + smfq_total, design = lsac_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = smok_ever,
    label = list(smok_ever ~ "Ever smoked")
  )%>%
  modify_column_hide(p.value) 
print(m2c)

# Adjusted (bl)
m2a <- svyglm(k10_cut ~ smok_ever + sex + ethnicity +
                maternal_edu + adhd_dx_final + ace_types_cat + icneuro_num, design = lsac_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = smok_ever,
    label = list(smok_ever ~ "Ever smoked")
  )%>%
  modify_column_hide(p.value) 
print(m2a)

# Adjusted (bl + baseline depression)
m2b <- svyglm(k10_cut ~ smok_ever + sex + ethnicity +
                maternal_edu + adhd_dx_final + ace_types_cat + icneuro_num + smfq_total, design = lsac_cca, family = "quasibinomial") %>%
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

#-------------------#

# EVER USED CANNABIS #
# Perform survey-weighted logistic regression
# Unadjusted
m3 <- svyglm(k10_cut ~ cannabis_ever, design = lsac_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(cannabis_ever ~ "Ever used cannabis")
  )%>%
  modify_column_hide(p.value) 
print(m3)

# Adjusted (all)
m4c <- svyglm(k10_cut ~ cannabis_ever + sex + ethnicity +
                maternal_edu + adhd_dx_final + ace_types_cat + icneuro_num 
              + smok_ever + ever_drug + alc_probs + 
                + smfq_total, design = lsac_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = cannabis_ever,
    label = list(cannabis_ever ~ "Ever used cannabis")
  )%>%
  modify_column_hide(p.value) 
print(m4c)

# Adjusted (bl)
m4a <- svyglm(k10_cut ~ cannabis_ever + sex + ethnicity +
                maternal_edu + adhd_dx_final + ace_types_cat + icneuro_num, design = lsac_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = cannabis_ever,
    label = list(cannabis_ever ~ "Ever used cannabis")
  )%>%
  modify_column_hide(p.value) 
print(m4a)

# Adjusted (bl + baseline depression)
m4b <- svyglm(k10_cut ~ cannabis_ever + sex + ethnicity +
                maternal_edu + adhd_dx_final + ace_types_cat + icneuro_num + smfq_total, design = lsac_cca, family = "quasibinomial") %>%
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

#----FREQUENCY OF USE---#

# FREQUENCY SMOKED #
# Perform survey-weighted logistic regression
# Unadjusted
m5 <- svyglm(k10_cut ~ smok_freq, design = lsac_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(smok_freq ~ "Smoking frequency")
  )%>%
  modify_column_hide(p.value) 
print(m5)

# Adjusted (all)
m6c <- svyglm(k10_cut ~ smok_freq + sex + ethnicity +
                maternal_edu + adhd_dx_final + ace_types_cat + icneuro_num 
              + cannabis_freq + ever_drug + alc_probs + 
                + smfq_total, design = lsac_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = smok_freq,
    label = list(smok_freq ~ "Smoking frequency")
  )%>%
  modify_column_hide(p.value) 
print(m6c)

# Adjusted (bl)
m6a <- svyglm(k10_cut ~ smok_freq + sex + ethnicity +
                maternal_edu + adhd_dx_final + ace_types_cat + icneuro_num, design = lsac_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = smok_freq,
    label = list(smok_freq ~ "Smoking frequency")
  )%>%
  modify_column_hide(p.value) 
print(m6a)

# Adjusted (bl + baseline depression)
m6b <- svyglm(k10_cut ~ smok_freq + sex + ethnicity +
                maternal_edu + adhd_dx_final + ace_types_cat + icneuro_num + smfq_total, design = lsac_cca, family = "quasibinomial") %>%
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
m7 <- svyglm(k10_cut ~ cannabis_freq, design = lsac_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(cannabis_freq ~ "Cannabis use frequency")
  )%>%
  modify_column_hide(p.value) 
print(m7)

# Adjusted (all)
m8c <- svyglm(k10_cut ~ cannabis_freq + sex + ethnicity +
                maternal_edu + adhd_dx_final + ace_types_cat + icneuro_num 
              + smok_freq + ever_drug + alc_probs + 
                + smfq_total, design = lsac_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = cannabis_freq,
    label = list(cannabis_freq ~ "Cannabis use frequency")
  )%>%
  modify_column_hide(p.value) 
print(m8c)

# Adjusted (bl)
m8a <- svyglm(k10_cut ~ cannabis_freq + sex + ethnicity +
                maternal_edu + adhd_dx_final + ace_types_cat + icneuro_num, design = lsac_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = cannabis_freq,
    label = list(cannabis_freq ~ "Cannabis use frequency")
  )%>%
  modify_column_hide(p.value) 
print(m8a)

# Adjusted (bl + baseline depression)
m8b <- svyglm(k10_cut ~ cannabis_freq + sex + ethnicity +
                maternal_edu + adhd_dx_final + ace_types_cat + icneuro_num + smfq_total, design = lsac_cca, family = "quasibinomial") %>%
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
m9 <- svyglm(k10_cut ~ ct_couse, design = lsac_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(ct_couse ~ "Concurrent use")
  )%>%
  modify_column_hide(p.value) 
print(m9)

# Adjusted (all)
m10c <- svyglm(k10_cut ~ ct_couse + sex + ethnicity +
                 maternal_edu + adhd_dx_final + ace_types_cat + icneuro_num 
               + ever_drug + alc_probs 
                 + smfq_total, design = lsac_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = ct_couse,
    label = list(ct_couse ~ "Concurrent use")
  )%>%
  modify_column_hide(p.value) 
print(m10c)

# Adjusted (bl)
m10a <- svyglm(k10_cut ~ ct_couse + sex + ethnicity +
                maternal_edu + adhd_dx_final + ace_types_cat + icneuro_num, design = lsac_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = ct_couse,
    label = list(ct_couse ~ "Concurrent use")
  )%>%
  modify_column_hide(p.value) 
print(m10a)

# Adjusted (bl + baseline depression)
m10b <- svyglm(k10_cut ~ ct_couse + sex + ethnicity +
                maternal_edu + adhd_dx_final + ace_types_cat + icneuro_num + smfq_total, design = lsac_cca, family = "quasibinomial") %>%
  tbl_regression(
    exponentiate = TRUE,
    include = ct_couse,
    label = list(ct_couse ~ "Concurrent use")
  )%>%
  modify_column_hide(p.value) 
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

# Convert to flex table, hide p-values and add footnote
cca_regs_flex <- cca_regs %>%
  modify_column_hide(starts_with("p.value")) %>%
  as_flex_table() %>%
  add_footer_lines("Note. Models: (1) Unadjusted; (2) Adjusted for sex, ethnicity, maternal education, ADHD, ACEs and neuroticism; (3) Additionally adjusted for baseline depressive symptoms; (4) Additionally adjusted for cannabis/tobacco use, other illicit drug use and alcohol-related harms. OR: Odds Ratio; aOR: Adjusted Odds Ratio; CI: Confidence Interval. Analysis based on N=2066 complete cases with weights from LSAC K cohort.")


# Save combined CCA regression tables
setwd("[file_path]")
doc <- read_docx() %>%
  body_add_flextable(cca_regs_flex)
print(doc, target = "cca_regs.docx")

## ====================== End of script ========================================
