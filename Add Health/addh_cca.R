'''
---
title: "Cross-cohort analysis cannabis depression"
author: Chloe Burke
dataset: Add Health
date: "October 2024"
script: "Complete-case analysis"
---
'''

# LOAD PACKAGES
#install.packages("twang")
library(twang)
#install.packages("car")
library(car)
##install.packages("svydiags")
library(svydiags)
##install.packages("gtsummary")
library(gtsummary)
##install.packages("officer")
library(officer)
##install.packages("flextable")
library(flextable)


## ================== Read in data =============================================

# Existing prepared data frame is 'xyz_ah'

## =============== Defining survey design for WII ==============================

## Using the svydesign function to tell R about design elements of the survey 
addh_d <- svydesign(id=~PSUSCID.y, weights=~GSWGT2, strata =~REGION.y, 
                    nest = TRUE, survey.lonely.psu = "adjust", data=xyz_ah)

# Applying eligible subset, including complete case status
ah_cca <- subset(addh_d, age_1617 == 1 & grade_12 == 0 & cca_overall ==1)

## =============== Regression analysis =========================================

#------Ever Smoked-------#

# Run unadjusted regression considering survey design 
m1 <- svyglm(fu_dep ~ ever_smok, 
             design = ah_cca, 
             family = "quasibinomial")

# Extract results
m1_tbl <- tbl_regression(m1,
                         exponentiate = TRUE,
                         label = list(ever_smok ~ "Ever smoked")
)%>%
  modify_column_hide(p.value)
print(m1_tbl)

# Model 4: Fully-adjusted
m2c <- svyglm(fu_dep ~ ever_smok + ever_cannabis + bio_sex + race_ethnicity +
                maternal_edu + ever_drug + alcprob_score + 
                neuroticism_score + total_cesd_bl +
                ace_score + adhd_dx, 
              design = ah_cca, 
              family = "quasibinomial")

m2c_tbl <- tbl_regression(m2c,
                          exponentiate = TRUE,
                          include = ever_smok,
                          label = list(ever_smok ~ "Ever smoked")
)%>%
  modify_column_hide(p.value)
print(m2c_tbl)

# Model 2: Pre-baseline confounding
m2a <- svyglm(fu_dep ~ ever_smok + bio_sex + race_ethnicity +
                maternal_edu + ace_score + adhd_dx + neuroticism_score, 
              design = ah_cca, 
              family = "quasibinomial")

m2a_tbl <- tbl_regression(m2a,
                          exponentiate = TRUE,
                          include = ever_smok,
                          label = list(ever_smok ~ "Ever smoked")
)%>%
  modify_column_hide(p.value)
print(m2a_tbl)

# Model 3: Pre-baseline + baseline depression 
m2b <- svyglm(fu_dep ~ ever_smok + bio_sex + race_ethnicity +
                maternal_edu + ace_score + adhd_dx + neuroticism_score + total_cesd_bl, 
              design = ah_cca, 
              family = "quasibinomial")

m2b_tbl <- tbl_regression(m2b,
                          exponentiate = TRUE,
                          include = ever_smok,
                          label = list(ever_smok ~ "Ever smoked")
)%>%
  modify_column_hide(p.value)
print(m2b_tbl)

# Combine the tables into one side by side
evers_comb <- tbl_merge(
  tbls = list(m1_tbl, m2a_tbl, m2b_tbl, m2c_tbl), 
  tab_spanner = c("Model 1", "Model 2", "Model 3", "Model 4")  
)

# Print the combined table
print(evers_comb)

#------Ever Cannabis-------#

# Run unadjusted regression considering survey design 
m3 <- svyglm(fu_dep ~ ever_cannabis, 
             design = ah_cca, 
             family = "quasibinomial")

m3_tbl <- tbl_regression(m3,
                         exponentiate = TRUE,
                         label = list(ever_cannabis ~ "Ever cannabis use")
)%>%
  modify_column_hide(p.value)
print(m3_tbl)

# MODEL 4A-C #
# Full adjusted model
m4c <- svyglm(fu_dep ~ ever_cannabis + ever_smok + bio_sex + race_ethnicity +
                maternal_edu + ever_drug + alcprob_score +
                neuroticism_score + ace_score + adhd_dx + total_cesd_bl, 
              design = ah_cca, 
              family = "quasibinomial")

m4c_tbl <- tbl_regression(m4c,
                          exponentiate = TRUE,
                          include = ever_cannabis,
                          label = list(ever_cannabis ~ "Ever cannabis use")
)%>%
  modify_column_hide(p.value)
print(m4c_tbl)

# Model 2: Pre-baseline confounding
m4a <- svyglm(fu_dep ~ ever_cannabis + bio_sex + race_ethnicity +
                maternal_edu + ace_score + adhd_dx + neuroticism_score, 
              design = ah_cca, 
              family = "quasibinomial")

m4a_tbl <- tbl_regression(m4a,
                          exponentiate = TRUE,
                          include = ever_cannabis,
                          label = list(ever_cannabis ~ "Ever cannabis use")
)%>%
  modify_column_hide(p.value)
print(m4a_tbl)

# Model 3: Pre-baseline + baseline depression 
m4b <- svyglm(fu_dep ~ ever_cannabis + bio_sex + race_ethnicity +
                maternal_edu + ace_score + adhd_dx + neuroticism_score + total_cesd_bl, 
              design = ah_cca, 
              family = "quasibinomial")

m4b_tbl <- tbl_regression(m4b,
                          exponentiate = TRUE,
                          include = ever_cannabis,
                          label = list(ever_cannabis ~ "Ever cannabis use")
)%>%
  modify_column_hide(p.value)
print(m4b_tbl)

# Combine the tables into one side by side
everc_comb <- tbl_merge(
  tbls = list(m3_tbl, m4a_tbl, m4b_tbl, m4c_tbl), 
  tab_spanner = c("Model 1", "Model 2", "Model 3", "Model 4")  
)

# Print the combined table
print(everc_comb)

#-----Smoking Frequency----#

# Run unadjusted regression considering survey design 
m5 <- svyglm(fu_dep ~ freq_smok, 
             design = ah_cca, 
             family = "quasibinomial")

m5_tbl <- tbl_regression(m5,
                         exponentiate = TRUE,
                         label = list(freq_smok ~ "Smoking frequency")
)%>%
  modify_column_hide(p.value)
print(m5_tbl)

# Model 4: Fully-adjusted
m6c <- svyglm(fu_dep ~ freq_smok + freq_can + bio_sex + race_ethnicity +
                maternal_edu + ever_drug + alcprob_score +
                neuroticism_score + ace_score + adhd_dx + total_cesd_bl, 
              design = ah_cca, 
              family = "quasibinomial")

m6c_tbl <- tbl_regression(m6c,
                          exponentiate = TRUE,
                          include = freq_smok,
                          label = list(freq_smok ~ "Smoking frequency")
)%>%
  modify_column_hide(p.value)
print(m6c_tbl)

# Model 2: Pre-baseline confounding
m6a <- svyglm(fu_dep ~ freq_smok + bio_sex + race_ethnicity +
                maternal_edu + ace_score + adhd_dx + neuroticism_score, 
              design = ah_cca, 
              family = "quasibinomial")

m6a_tbl <- tbl_regression(m6a,
                          exponentiate = TRUE,
                          include = freq_smok,
                          label = list(freq_smok ~ "Smoking frequency")
)%>%
  modify_column_hide(p.value)
print(m6a_tbl)

# Model 3: Pre-baseline + baseline depression 
m6b <- svyglm(fu_dep ~ freq_smok + bio_sex + race_ethnicity +
                maternal_edu + ace_score + adhd_dx + neuroticism_score + total_cesd_bl, 
              design = ah_cca, 
              family = "quasibinomial")

m6b_tbl <- tbl_regression(m6b,
                          exponentiate = TRUE,
                          include = freq_smok,
                          label = list(freq_smok ~ "Smoking frequency")
)%>%
  modify_column_hide(p.value)
print(m6b_tbl)

# Combine the tables into one side by side
freqs_comb <- tbl_merge(
  tbls = list(m5_tbl, m6a_tbl, m6b_tbl, m6c_tbl), 
  tab_spanner = c("Model 1", "Model 2", "Model 3", "Model 4")  
)

# Print the combined table
print(freqs_comb)

#-------Cannabis Frequency------#

# Run unadjusted regression considering survey design 
m7 <- svyglm(fu_dep ~ freq_can, 
             design = ah_cca, 
             family = "quasibinomial")

m7_tbl <- tbl_regression(m7,
                         exponentiate = TRUE,
                         label = list(freq_can ~ "Cannabis use frequency")
)%>%
  modify_column_hide(p.value)
print(m7_tbl)

# Model 4: Fully-adjusted
m8c <- svyglm(fu_dep ~ freq_can + freq_smok + bio_sex + race_ethnicity +
                maternal_edu + ever_drug + alcprob_score +
                neuroticism_score + ace_score + adhd_dx + total_cesd_bl, 
              design = ah_cca, 
              family = "quasibinomial")

m8c_tbl <- tbl_regression(m8c,
                          exponentiate = TRUE,
                          include = freq_can,
                          label = list(freq_can ~ "Cannabis use frequency")
)%>%
  modify_column_hide(p.value)
print(m8c_tbl)

# Model 2: Pre-baseline confounding
m8a <- svyglm(fu_dep ~ freq_can + bio_sex + race_ethnicity +
                maternal_edu + ace_score + adhd_dx + neuroticism_score, 
              design = ah_cca, 
              family = "quasibinomial")

m8a_tbl <- tbl_regression(m8a,
                          exponentiate = TRUE,
                          include = freq_can,
                          label = list(freq_can ~ "Cannabis use frequency")
)%>%
  modify_column_hide(p.value)
print(m8a_tbl)

# Model 3: Pre-baseline + baseline depression 
m8b <- svyglm(fu_dep ~ freq_can + bio_sex + race_ethnicity +
                maternal_edu + ace_score + adhd_dx + neuroticism_score + total_cesd_bl, 
              design = ah_cca, 
              family = "quasibinomial")

m8b_tbl <- tbl_regression(m8b,
                          exponentiate = TRUE,
                          include = freq_can,
                          label = list(freq_can ~ "Cannabis use frequency")
)%>%
  modify_column_hide(p.value)
print(m8b_tbl)

# Combine the tables into one side by side
freqc_comb <- tbl_merge(
  tbls = list(m7_tbl, m8a_tbl, m8b_tbl, m8c_tbl), 
  tab_spanner = c("Model 1", "Model 2", "Model 3", "Model 4")  
)

# Print the combined table
print(freqc_comb)

#-----Concurrent use----#

# Run unadjusted regression considering survey design 
m9 <- svyglm(fu_dep ~ ct_couse, 
             design = ah_cca, 
             family = "quasibinomial")

m9_tbl <- tbl_regression(m9,
                         exponentiate = TRUE,
                         label = list(ct_couse ~ "Concurrent use")
)%>%
  modify_column_hide(p.value)
print(m9_tbl)

# Model 4: Fully-adjusted
m10c <- svyglm(fu_dep ~ ct_couse + bio_sex + race_ethnicity +
                 maternal_edu + ever_drug + alcprob_score +
                 neuroticism_score + ace_score + adhd_dx + total_cesd_bl, 
               design = ah_cca, 
               family = "quasibinomial")

m10c_tbl <- tbl_regression(m10c,
                           exponentiate = TRUE,
                           include = ct_couse,
                           label = list(ct_couse ~ "Concurrent use")
)%>%
  modify_column_hide(p.value)
print(m10c_tbl)

# Model 2: Pre-baseline confounding
m10a <- svyglm(fu_dep ~ ct_couse + bio_sex + race_ethnicity +
                 maternal_edu + ace_score + adhd_dx + neuroticism_score, 
               design = ah_cca, 
               family = "quasibinomial")

m10a_tbl <- tbl_regression(m10a,
                           exponentiate = TRUE,
                           include = ct_couse,
                           label = list(ct_couse ~ "Concurrent use")
)%>%
  modify_column_hide(p.value)
print(m10a_tbl)

# Model 3: Pre-baseline + baseline depression 
m10b <- svyglm(fu_dep ~ ct_couse + bio_sex + race_ethnicity +
                 maternal_edu + ace_score + adhd_dx + neuroticism_score + total_cesd_bl, 
               design = ah_cca, 
               family = "quasibinomial")

m10b_tbl <- tbl_regression(m10b,
                           exponentiate = TRUE,
                           include = ct_couse,
                           label = list(ct_couse ~ "Concurrent use")
)%>%
  modify_column_hide(p.value)
print(m10b_tbl)

# Combine the tables into one side by side
couse_comb <- tbl_merge(
  tbls = list(m9_tbl, m10a_tbl, m10b_tbl, m10c_tbl), 
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

# Save combined CCA regression tables
#setwd("[file_path]")
cca_regs_flex <- as_flex_table(cca_regs)%>%
  add_footer_lines("Note. Models: (1) Unadjusted; (2) Adjusted for sex, ethnicity, maternal education, ADHD, ACEs and neuroticism; (3) Additionally adjusted for baseline depressive symptoms; (4) Additionally adjusted for cannabis/tobacco use, other illicit drug use and alcohol-related harms. OR: Odds Ratio; aOR: Adjusted Odds Ratio; CI: Confidence Interval. Analysis based on complete case sample (n = 2985) with sample weights from Add Health cohort.")

doc <- read_docx() %>%
  body_add_flextable(cca_regs_flex)
print(doc, target = "cca_regs.docx")

## ================== End of script ============================================
