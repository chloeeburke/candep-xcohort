'''
---
title: "Cross-cohort analysis cannabis depression"
analyst: Chloe Burke
dataset: Add Health
date: October, 2024
script: "Imputation and analysis"
---
'''

# LOAD PACKAGES
#install.packages("mice")
library(mice)
#install.packages("howManyImputations")
library(howManyImputations)
#install.packages("reshape2")
library(reshape2)
#install.packages("RColorBrewer")
library(RColorBrewer)
# install.packages("devtools")
devtools::install_github("ellessenne/mice.mcerror")
library(mice.mcerror)
#install.packages("mitools")
library(mitools)
#install.packages("skimr")
library(skimr)

#### NOTE ####
# Code merged to reflect two separate analysis files - may contain errors;
# Scheduled quality check January 2025

## ================== Clear environment ========================================

rm(list = setdiff(ls(), "xyz_ah"))

## ================== Imputation  ==============================================

# Imputing separately for different models, to reduce collinearity between variables derived from each other 
# Running as transform then impute https://bookdown.org/rwnahhas/RMPH/mi-fitting.html

# A subset of variables is selected
ever_vars <- c("AID", "PSUSCID.y", "REGION.y", "GSWGT2", "grade_12", "age_1617", "age_in_years", "ever_smok", "ever_cannabis", "total_cesd_bl", "bio_sex", "race_ethnicity",
               "maternal_edu", "alcprob_score", "ever_drug", "sleep_problems",
               "neuroticism_score", "anxiety_symptoms", "cd_score", "adhd_dx", "ace_score", "fu_dep", 
               "vid_game", "scl_susp", "tch_rel", "fam_rel", "FAMST2")

# Restricting df to analysis variables
impdata_es <- xyz_ah[,ever_vars]

# Check variable class
sapply(impdata_es, class)

# Change variable formats
impdata_es$REGION.y <- as.factor(impdata_es$REGION.y)
impdata_es$PSUSCID.y <- as.numeric(impdata_es$PSUSCID.y) 
impdata_es$GSWGT2 <- as.numeric(impdata_es$GSWGT2) 
impdata_es$age_1617 <- as.factor(impdata_es$age_1617) 
impdata_es$grade_12 <- as.factor(impdata_es$grade_12) 
impdata_es$FAMST2 <- as.factor(impdata_es$FAMST2) 

# Checking missing values for all variables
skim(impdata_es)

# Define the list of variables to be imputed and regular variables
imputed_vars <- c("ever_smok", "ever_cannabis", "total_cesd_bl", "race_ethnicity",
                  "maternal_edu", "alcprob_score", "ever_drug", "sleep_problems",
                  "neuroticism_score", "anxiety_symptoms", "cd_score", "adhd_dx", "ace_score", "fu_dep",
                  "vid_game", "scl_susp", "tch_rel", "fam_rel")  
regular_vars <- c("bio_sex", "PSUSCID.y", "REGION.y", "GSWGT2", "FAMST2", "age_in_years")
id_vars <- c("AID", "age_1617", "grade_12") 

# Set up predictor matrix
predictor_matrix_e <- make.predictorMatrix(impdata_es)

# Handle ID variables
for (var in id_vars) {
  predictor_matrix_e[var, ] <- 0   
  predictor_matrix_e[, var] <- 0   
}

# Set up regular predictors
for (var in regular_vars) {
  predictor_matrix_e[var, ] <- 0     # Don't impute regular vars
}

# Set predictors for all imputed variables (including auxiliaries)
for (var in imputed_vars) {
  predictor_matrix_e[var, regular_vars] <- 1  # Use regular vars as predictors
  predictor_matrix_e[var, setdiff(imputed_vars, var)] <- 1  # Use other imputed vars as predictors
}

print(predictor_matrix_e)

# Run test imputation to generate methods list
imp_es <- mice(impdata_es, m = 1, maxit = 5, predictorMatrix = predictor_matrix_e, seed = 123, print = FALSE)

# Set methods
methods_e <- imp_es$method
for (var in c(id_vars, regular_vars)) {  
  methods_e[var] <- ""  # Set to no imputation
}

print(methods_e)

# Run test imputation
imp_es <- mice(impdata_es, 
               m = 20, 
               maxit = 5, 
               predictorMatrix = predictor_matrix_e, 
               method = methods_e, 
               seed = 123, 
               print = FALSE)

# Create list of survey designs for each imputed dataset, with subsetting
svy_designs_e <- lapply(1:20, function(i) {
  data_i <- complete(imp_es20, i)
  design_i <- svydesign(
    id = ~PSUSCID.y,         
    weights = ~GSWGT2,      
    strata = ~REGION.y,      
    nest = TRUE,
    survey.lonely.psu = "adjust",
    data = data_i
  )
  # Apply the subsetting
  subset(design_i, age_1617 == 1 & grade_12 == 0)
})

# Test of fully adjusted model
m2_results <- lapply(1:20, function(m) {
  svyglm(fu_dep ~ ever_smok + ever_cannabis + bio_sex + race_ethnicity +
           maternal_edu + ever_drug + alcprob_score + mvpa_high +
           sleep_problems + neuroticism_score + anxiety_symptoms +
           cd_score + ace_score + adhd_dx, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})

m2_mira <- list(analyses = m2_results)
class(m2_mira) <- "mira"

m2_imp <- tbl_regression(m2_mira,
                         exponentiate = TRUE,
                         include = ever_smok,
                         label = list(ever_smok ~ "Ever smoked")
)
print(m2_imp)

# Checking FMI
how_many_imputations(m2_results, cv = 0.05, alpha = 0.05) # When model run using 20 imputations suggests = 29

#------Frequency variables ------#

# A subset of variables is selected
freq_vars <- c("AID", "PSUSCID.y", "REGION.y", "GSWGT2", "grade_12", "age_1617", "age_in_years", "freq_smok", "freq_can", "total_cesd_bl", "bio_sex", "race_ethnicity",
               "maternal_edu", "alcprob_score", "ever_drug", "sleep_problems",
               "neuroticism_score", "anxiety_symptoms", "cd_score", "adhd_dx", "ace_score", "fu_dep",
               "vid_game", "scl_susp", "tch_rel", "fam_rel", "FAMST2")

# Restricting df to analysis variables
impdata_fs <- xyz_ah[,freq_vars]

# Check variable class
sapply(impdata_fs, class)

# Change variable formats
impdata_fs$REGION.y <- as.factor(impdata_fs$REGION.y)
impdata_fs$PSUSCID.y <- as.numeric(impdata_fs$PSUSCID.y) 
impdata_fs$GSWGT2 <- as.numeric(impdata_fs$GSWGT2) 
impdata_fs$age_1617 <- as.factor(impdata_fs$age_1617) 
impdata_fs$grade_12 <- as.factor(impdata_fs$grade_12)
impdata_fs$FAMST2 <- as.factor(impdata_fs$FAMST2) 

# Checking missing values for all variables
skim(impdata_fs)

# Define the list of variables to be imputed and regular variables
imputed_vars <- c("freq_smok", "freq_can", "total_cesd_bl", "race_ethnicity",
                  "maternal_edu", "alcprob_score", "ever_drug", "sleep_problems",
                  "neuroticism_score", "anxiety_symptoms", "cd_score", "adhd_dx", "ace_score", "fu_dep",
                  "vid_game", "scl_susp", "tch_rel", "fam_rel")  
regular_vars <- c("bio_sex", "PSUSCID.y", "REGION.y", "GSWGT2", "FAMST2", "age_in_years")
id_vars <- c("AID", "age_1617", "grade_12") 

# Set up predictor matrix
predictor_matrix_f <- make.predictorMatrix(impdata_fs)

# Handle ID variables
for (var in id_vars) {
  predictor_matrix_f[var, ] <- 0   
  predictor_matrix_f[, var] <- 0   
}

# Set up regular predictors
for (var in regular_vars) {
  predictor_matrix_f[var, ] <- 0     # Don't impute regular vars
}

# Set predictors for all imputed variables (including auxiliaries)
for (var in imputed_vars) {
  predictor_matrix_f[var, regular_vars] <- 1  # Use regular vars as predictors
  predictor_matrix_f[var, setdiff(imputed_vars, var)] <- 1  # Use other imputed vars as predictors
}

print(predictor_matrix_f)

# Run test imputation to generate methods list
imp_fs <- mice(impdata_fs, m = 1, maxit = 5, predictorMatrix = predictor_matrix_f, seed = 123, print = FALSE)

# Set methods
imp_fs$method
methods_f <- imp_fs$method
for (var in c(id_vars, regular_vars)) {
  methods_f[var] <- ""  # Set to no imputation
}
print(methods_f)

# Run imputation model to check FMI 
imp_fs <- mice(impdata_fs, m = 20, maxit = 5, predictorMatrix = predictor_matrix_f, method = methods_f, seed = 123, print = FALSE)

# Check summary of imputed dataset
summary(imp_fs)

# Looking at logged events
imp_fs$loggedEvents

# Convert to long format 
long_data <- complete(imp_fs, action = "long", include = TRUE)

# Check for remaining missing values
remaining_missing <- long_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) #Only .imp = 0 (i.e., observed dataset)

# Create list of survey designs for each imputed dataset, with subsetting
svy_designs_f <- lapply(1:20, function(i) {
  data_i <- complete(imp_fs20, i)
  design_i <- svydesign(
    id = ~PSUSCID.y,         
    weights = ~GSWGT2,      
    strata = ~REGION.y,      
    nest = TRUE,
    survey.lonely.psu = "adjust",
    data = data_i
  )
  # Apply the subsetting
  subset(design_i, age_1617 == 1 & grade_12 == 0)
})

# Test of fully adjusted model
m4_results <- lapply(1:20, function(m) {
  svyglm(fu_dep ~ freq_smok + freq_can + bio_sex + race_ethnicity +
           maternal_edu + ever_drug + alcprob_score + mvpa_high +
           sleep_problems + neuroticism_score + anxiety_symptoms +
           cd_score + ace_score + adhd_dx, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})

m4_mira <- list(analyses = m4_results)
class(m4_mira) <- "mira"

m4_imp <- tbl_regression(m4_mira,
                         exponentiate = TRUE,
                         include = freq_smok,
                         label = list(freq_smok ~ "Smoking frequency")
)
print(m4_imp)

# Checking FMI
how_many_imputations(m4_results, cv = 0.05, alpha = 0.05) # When model run using 20 imputations suggests = 42

#------Co-use-------#

# A subset of variables is selected
couse_vars <- c("AID", "PSUSCID.y", "REGION.y", "GSWGT2", "grade_12", "age_1617", "age_in_years", "ct_couse","total_cesd_bl", "bio_sex", "race_ethnicity",
                "maternal_edu", "alcprob_score", "ever_drug", "sleep_problems",
                "neuroticism_score", "anxiety_symptoms", "cd_score", "adhd_dx", "ace_score", "fu_dep",
                "vid_game", "scl_susp", "tch_rel", "fam_rel", "FAMST2")

# Restricting df to analysis variables
impdata_cu <- xyz_ah[,couse_vars]

# Check variable class
sapply(impdata_cu, class)

# Change variable formats
impdata_cu$REGION.y <- as.factor(impdata_cu$REGION.y)
impdata_cu$PSUSCID.y <- as.numeric(impdata_cu$PSUSCID.y) 
impdata_cu$GSWGT2 <- as.numeric(impdata_cu$GSWGT2) 
impdata_cu$age_1617 <- as.factor(impdata_cu$age_1617) 
impdata_cu$grade_12 <- as.factor(impdata_cu$grade_12) 
impdata_cu$FAMST2 <- as.factor(impdata_cu$FAMST2) 

# Checking missing values for all variables
skim(impdata_cu)

# Define the list of variables to be imputed and regular variables
imputed_vars <- c("ct_couse", "total_cesd_bl", "race_ethnicity",
                  "maternal_edu", "alcprob_score", "ever_drug", "sleep_problems", 
                  "neuroticism_score", "anxiety_symptoms", "cd_score", "adhd_dx", "ace_score", "fu_dep",
                  "vid_game", "scl_susp", "tch_rel", "fam_rel")  
regular_vars <- c("bio_sex", "PSUSCID.y", "REGION.y", "GSWGT2", "FAMST2", "age_in_years")
id_vars <- c("AID", "age_1617", "grade_12") 

# Set up predictor matrix
predictor_matrix_cu <- make.predictorMatrix(impdata_cu)

# Handle ID variables
for (var in id_vars) {
  predictor_matrix_cu[var, ] <- 0   
  predictor_matrix_cu[, var] <- 0   
}

# Set up regular predictors
for (var in regular_vars) {
  predictor_matrix_cu[var, ] <- 0     # Don't impute regular vars
}

# Set predictors for all imputed variables (including auxiliaries)
for (var in imputed_vars) {
  predictor_matrix_cu[var, regular_vars] <- 1  # Use regular vars as predictors
  predictor_matrix_cu[var, setdiff(imputed_vars, var)] <- 1  # Use other imputed vars as predictors
}

print(predictor_matrix_cu)

# Run test imputation to generate methods list
imp_cu <- mice(impdata_cu, m = 1, maxit = 5, predictorMatrix = predictor_matrix_cu, seed = 123, print = FALSE)

# Set methods
imp_cu$method
methods_cu <- imp_cu$method
for (var in c(id_vars, regular_vars)) {
  methods_cu[var] <- ""  # Set to no imputation
}
print(methods_cu)

# Run imputation model to check FMI 
imp_cu <- mice(impdata_cu, m = 20, maxit = 5, predictorMatrix = predictor_matrix_cu, method = methods_cu, seed = 123, print = FALSE)

# Create list of survey designs for each imputed dataset, with subsetting
svy_designs_cu <- lapply(1:20, function(i) {
  data_i <- complete(imp_cu20, i)
  design_i <- svydesign(
    id = ~PSUSCID.y,         
    weights = ~GSWGT2,      
    strata = ~REGION.y,      
    nest = TRUE,
    survey.lonely.psu = "adjust",
    data = data_i
  )
  # Apply the subsetting
  subset(design_i, age_1617 == 1 & grade_12 == 0)
})

# Fully adjusted model
m10_results <- lapply(1:20, function(m) {
  svyglm(fu_dep ~ ct_couse + bio_sex + race_ethnicity +
           maternal_edu + ever_drug + alcprob_score + mvpa_high +
           sleep_problems + neuroticism_score + anxiety_symptoms +
           cd_score + ace_score + adhd_dx, 
         design = svy_designs_cu[[m]], 
         family = "quasibinomial")
})

m10_mira <- list(analyses = m10_results)
class(m10_mira) <- "mira"

m10_imp <- tbl_regression(m10_mira,
                          exponentiate = TRUE,
                          include = ct_couse,
                          label = list(ct_couse ~ "Concurrent use")
)
print(m10_imp)

# Checking FMI
how_many_imputations(m10_results, cv = 0.05, alpha = 0.05) # When model run using 20 imputations suggests = 24

## ================== Batch run imputation models ==============================

# Run imputation model 
imp_es50 <- mice(impdata_es, m = 50, maxit = 25, predictorMatrix = predictor_matrix_e, method = methods_e, seed = 123, print = FALSE)

# Run imputation model 
imp_fs50 <- mice(impdata_fs, m = 50, maxit = 25, predictorMatrix = predictor_matrix_f, method = methods_f, seed = 123, print = FALSE)

# Run imputation model 
imp_cu50 <- mice(impdata_cu, m = 50, maxit = 25, predictorMatrix = predictor_matrix_cu, method = methods_cu, seed = 123, print = FALSE)

## ================== Check models  ============================================

#------Ever Use-------#

# Check summary of imputed dataset
summary(imp_es50)

# Looking at logged events
imp_es50$loggedEvents #NULL

# Convert to long format 
long_data_e <- complete(imp_es50, action = "long", include = TRUE)

# Check for remaining missing values
remaining_missing_e <- long_data_e %>%
  summarise(across(everything(), ~ sum(is.na(.)))) 

# Looking at convergence: https://nerler.github.io/EP16_Multiple_Imputation/slide/07_convergence_and_diagnostics.pdf
plot(imp_es50)
densityplot(imp_es50)

# Load propplot.R function
propplot(imp_es50)

#------Frequency variables ------#

# Check summary of imputed dataset
summary(imp_fs50)

# Looking at logged events
imp_fs50$loggedEvents

# Convert to long format 
long_data_f <- complete(imp_fs50, action = "long", include = TRUE)

# Check for remaining missing values
remaining_missing_f <- long_data_f %>%
  summarise(across(everything(), ~ sum(is.na(.)))) #Only .imp = 0 (i.e., observed dataset)

# Looking at convergence: https://nerler.github.io/EP16_Multiple_Imputation/slide/07_convergence_and_diagnostics.pdf
plot(imp_fs50)
densityplot(imp_fs50)

# Load propplot.R function
propplot(imp_fs50)

#------Co-use-------#

# Check summary of imputed dataset
summary(imp_cu50)

# Looking at logged events
imp_cu50$loggedEvents
imp_cu50$method

# Convert to long format 
long_data_cu <- complete(imp_cu50, action = "long", include = TRUE)

# Check for remaining missing values
remaining_missing_cu <- long_data_cu %>%
  summarise(across(everything(), ~ sum(is.na(.)))) #Only .imp = 0 (i.e., observed dataset)

# Looking at convergence: https://nerler.github.io/EP16_Multiple_Imputation/slide/07_convergence_and_diagnostics.pdf
plot(imp_cu50)
densityplot(imp_cu50)

# Load propplot.R function
propplot(imp_cu50)

## ==================  Analysis on imputed data ===============================

#------Ever Use-------#

# Create list of survey designs for each imputed dataset, with subsetting
svy_designs_e <- lapply(1:50, function(i) {
  data_i <- complete(imp_es50, i)
  design_i <- svydesign(
    id = ~PSUSCID.y,         
    weights = ~GSWGT2,      
    strata = ~REGION.y,      
    nest = TRUE,
    survey.lonely.psu = "adjust",
    data = data_i
  )
  # Apply the subsetting
  subset(design_i, age_1617 == 1 & grade_12 == 0)
})

#------Ever Smoked-------#

# MODEL 1 #
# Run unadjusted regression considering survey design 
m1_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ ever_smok, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})

# Structure it as a mice object
m1_mira <- list(analyses = m1_results)
class(m1_mira) <- "mira"

# Then extracting using tbl_regression
m1_imp <- tbl_regression(m1_mira,
                         exponentiate = TRUE,
                         label = list(ever_smok ~ "Ever smoked")
)%>%
  modify_column_hide(p.value)
print(m1_imp)

# MODEL 2A-C #
# Model 4: Fully-adjusted (i.e., pre-baseline Z, other substance use, baseline depression)
m2c_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ ever_smok + ever_cannabis + bio_sex + race_ethnicity +
           maternal_edu + ever_drug + alcprob_score
          + neuroticism_score + total_cesd_bl
          + ace_score + adhd_dx, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})

m2c_mira <- list(analyses = m2c_results)
class(m2c_mira) <- "mira"

m2c_imp <- tbl_regression(m2c_mira,
                         exponentiate = TRUE,
                         include = ever_smok,
                         label = list(ever_smok ~ "Ever smoked")
)%>%
  modify_column_hide(p.value)
print(m2c_imp)

# Model 2: Pre-baseline confounding
m2a_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ ever_smok + bio_sex + race_ethnicity +
           maternal_edu + ace_score + adhd_dx + neuroticism_score, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})

m2a_mira <- list(analyses = m2a_results)
class(m2a_mira) <- "mira"

m2a_imp <- tbl_regression(m2a_mira,
                          exponentiate = TRUE,
                          include = ever_smok,
                          label = list(ever_smok ~ "Ever smoked")
)%>%
  modify_column_hide(p.value)
print(m2a_imp)

# Model 3: Pre-baseline + baseline depression 
m2b_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ ever_smok + bio_sex + race_ethnicity +
           maternal_edu + ace_score + adhd_dx + neuroticism_score + total_cesd_bl, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})

m2b_mira <- list(analyses = m2b_results)
class(m2b_mira) <- "mira"

m2b_imp <- tbl_regression(m2b_mira,
                          exponentiate = TRUE,
                          include = ever_smok,
                          label = list(ever_smok ~ "Ever smoked")
)%>%
  modify_column_hide(p.value)
print(m2b_imp)

# Combine the tables into one side by side
evers_comb_imp <- tbl_merge(
  tbls = list(m1_imp, m2a_imp, m2b_imp, m2c_imp), 
  tab_spanner = c("Model 1", "Model 2", "Model 3", "Model 4")  
)

# Print the combined table
print(evers_comb_imp)

#------Ever Cannabis-------#

# MODEL 3 #
# Run unadjusted regression considering survey design 
m3_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ ever_cannabis, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})

# Structure it as a mice object
m3_mira <- list(analyses = m3_results)
class(m3_mira) <- "mira"

# Then extracting using tbl_regression
m3_imp <- tbl_regression(m3_mira,
                         exponentiate = TRUE,
                         label = list(ever_cannabis ~ "Ever cannabis use")
)%>%
  modify_column_hide(p.value)
print(m3_imp)

# MODEL 4A-C #
# Full adjusted model
m4c_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ ever_cannabis + ever_smok + bio_sex + race_ethnicity +
           maternal_edu + ever_drug + alcprob_score +
           neuroticism_score
          + ace_score + adhd_dx + total_cesd_bl, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})

m4c_mira <- list(analyses = m4c_results)
class(m4c_mira) <- "mira"

m4c_imp <- tbl_regression(m4c_mira,
                         exponentiate = TRUE,
                         include = ever_cannabis,
                         label = list(ever_cannabis ~ "Ever cannabis use")
)%>%
  modify_column_hide(p.value)
print(m4c_imp)

# Model 2: Pre-baseline confounding
m4a_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ ever_cannabis + bio_sex + race_ethnicity +
           maternal_edu + ace_score + adhd_dx + neuroticism_score, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})

m4a_mira <- list(analyses = m4a_results)
class(m4a_mira) <- "mira"

m4a_imp <- tbl_regression(m4a_mira,
                          exponentiate = TRUE,
                          include = ever_cannabis,
                          label = list(ever_cannabis ~ "Ever cannabis use")
)%>%
  modify_column_hide(p.value)
print(m4a_imp)

# Model 3: Pre-baseline + baseline depression 
m4b_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ ever_cannabis + bio_sex + race_ethnicity +
           maternal_edu + ace_score + adhd_dx + neuroticism_score + total_cesd_bl, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})

m4b_mira <- list(analyses = m4b_results)
class(m4b_mira) <- "mira"

m4b_imp <- tbl_regression(m4b_mira,
                          exponentiate = TRUE,
                          include = ever_cannabis,
                          label = list(ever_cannabis ~ "Ever cannabis use")
)%>%
  modify_column_hide(p.value)
print(m4b_imp)

# Combine the tables into one side by side
everc_comb_imp <- tbl_merge(
  tbls = list(m3_imp, m4a_imp, m4b_imp, m4c_imp), 
  tab_spanner = c("Model 1", "Model 2", "Model 3", "Model 4")  
)

# Print the combined table
print(everc_comb_imp)

#------Frequency Variables-------#

# Create list of survey designs for each imputed dataset, with subsetting
svy_designs_f <- lapply(1:50, function(i) {
  data_i <- complete(imp_fs50, i)
  design_i <- svydesign(
    id = ~PSUSCID.y,         
    weights = ~GSWGT2,      
    strata = ~REGION.y,      
    nest = TRUE,
    survey.lonely.psu = "adjust",
    data = data_i
  )
  # Apply the subsetting
  subset(design_i, age_1617 == 1 & grade_12 == 0)
})


#-----Smoking Frequency----#

# MODEL 5 #
# Run unadjusted regression considering survey design 
m5_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ freq_smok, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})

# Structure it as a mice object
m5_mira <- list(analyses = m5_results)
class(m5_mira) <- "mira"

# Then extracting using tbl_regression
m5_imp <- tbl_regression(m5_mira,
                         exponentiate = TRUE,
                         label = list(freq_smok ~ "Smoking frequency")
)%>%
  modify_column_hide(p.value)
print(m5_imp)

# Model 4: Fully-adjusted (i.e., pre-baseline Z, other substance use, baseline depression)
m6c_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ freq_smok + freq_can + bio_sex + race_ethnicity +
           maternal_edu + ever_drug + alcprob_score +
          neuroticism_score +
          ace_score + adhd_dx + total_cesd_bl, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})

m6c_mira <- list(analyses = m6c_results)
class(m6c_mira) <- "mira"

m6c_imp <- tbl_regression(m6c_mira,
                         exponentiate = TRUE,
                         include = freq_smok,
                         label = list(freq_smok ~ "Smoking frequency")
)%>%
  modify_column_hide(p.value)
print(m6c_imp)

# Model 2: Pre-baseline confounding
m6a_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ freq_smok + bio_sex + race_ethnicity +
           maternal_edu + ace_score + adhd_dx + neuroticism_score, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})

m6a_mira <- list(analyses = m6a_results)
class(m6a_mira) <- "mira"

m6a_imp <- tbl_regression(m6a_mira,
                          exponentiate = TRUE,
                          include = freq_smok,
                          label = list(freq_smok ~ "Smoking frequency")
)%>%
  modify_column_hide(p.value)
print(m6a_imp)

# Model 3: Pre-baseline + baseline depression 
m6b_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ freq_smok + bio_sex + race_ethnicity +
           maternal_edu + ace_score + adhd_dx + neuroticism_score + total_cesd_bl, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})

m6b_mira <- list(analyses = m6b_results)
class(m6b_mira) <- "mira"

m6b_imp <- tbl_regression(m6b_mira,
                          exponentiate = TRUE,
                          include = freq_smok,
                          label = list(freq_smok ~ "Smoking frequency")
)%>%
  modify_column_hide(p.value)
print(m6b_imp)

# Combine the tables into one side by side
freqs_comb_imp <- tbl_merge(
  tbls = list(m5_imp, m6a_imp, m6b_imp, m6c_imp), 
  tab_spanner = c("Model 1", "Model 2", "Model 3", "Model 4")  
)

# Print the combined table
print(freqs_comb_imp)

#-------Cannabis Frequency------#

# MODEL 1 #
# Run unadjusted regression considering survey design 
m7_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ freq_can, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})

# Structure it as a mice object
m7_mira <- list(analyses = m7_results)
class(m7_mira) <- "mira"

# Then extracting using tbl_regression
m7_imp <- tbl_regression(m7_mira,
                         exponentiate = TRUE,
                         label = list(freq_can ~ "Cannabis use frequency")
)%>%
  modify_column_hide(p.value)
print(m7_imp)

# Model 4: Fully-adjusted (i.e., pre-baseline Z, other substance use, baseline depression)
m8c_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ freq_can + freq_smok + bio_sex + race_ethnicity +
           maternal_edu + ever_drug + alcprob_score +
           neuroticism_score  +
           ace_score + adhd_dx + total_cesd_bl, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})

m8c_mira <- list(analyses = m8c_results)
class(m8c_mira) <- "mira"

m8c_imp <- tbl_regression(m8c_mira,
                         exponentiate = TRUE,
                         include = freq_can,
                         label = list(freq_can ~ "Cannabis use frequency")
)%>%
  modify_column_hide(p.value)
print(m8c_imp)

# Model 2: Pre-baseline confounding
m8a_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ freq_can + bio_sex + race_ethnicity +
           maternal_edu + ace_score + adhd_dx + neuroticism_score, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})

m8a_mira <- list(analyses = m8a_results)
class(m8a_mira) <- "mira"

m8a_imp <- tbl_regression(m8a_mira,
                          exponentiate = TRUE,
                          include = freq_can,
                          label = list(freq_can ~ "Cannabis use frequency")
)%>%
  modify_column_hide(p.value)
print(m8a_imp)

# Model 3: Pre-baseline + baseline depression 
m8b_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ freq_can + bio_sex + race_ethnicity +
           maternal_edu + ace_score + adhd_dx + neuroticism_score + total_cesd_bl, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})

m8b_mira <- list(analyses = m8b_results)
class(m8b_mira) <- "mira"

m8b_imp <- tbl_regression(m8b_mira,
                          exponentiate = TRUE,
                          include = freq_can,
                          label = list(freq_can ~ "Cannabis use frequency")
)%>%
  modify_column_hide(p.value)
print(m8b_imp)

# Combine the tables into one side by side
freqc_comb_imp <- tbl_merge(
  tbls = list(m7_imp, m8a_imp, m8b_imp, m8c_imp), 
  tab_spanner = c("Model 1", "Model 2", "Model 3", "Model 4")  
)

# Print the combined table
print(freqc_comb_imp)

#------Co-use-------#

# Create list of survey designs for each imputed dataset, with subsetting
svy_designs_cu <- lapply(1:50, function(i) {
  data_i <- complete(imp_cu50, i)
  design_i <- svydesign(
    id = ~PSUSCID.y,         
    weights = ~GSWGT2,      
    strata = ~REGION.y,      
    nest = TRUE,
    survey.lonely.psu = "adjust",
    data = data_i
  )
  # Apply the subsetting
  subset(design_i, age_1617 == 1 & grade_12 == 0)
})

#-----Concurrent use----#

# MODEL 1 #
# Run unadjusted regression considering survey design 
m9_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ ct_couse, 
         design = svy_designs_cu[[m]], 
         family = "quasibinomial")
})

# Structure it as a mice object
m9_mira <- list(analyses = m9_results)
class(m9_mira) <- "mira"

# Then extracting using tbl_regression
m9_imp <- tbl_regression(m9_mira,
                         exponentiate = TRUE,
                         label = list(ct_couse ~ "Concurrent use")
)%>%
  modify_column_hide(p.value)
print(m9_imp)

# Model 4: Fully-adjusted (i.e., pre-baseline Z, other substance use, baseline depression)
m10c_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ ct_couse + bio_sex + race_ethnicity +
           maternal_edu + ever_drug + alcprob_score +
          neuroticism_score + 
          ace_score + adhd_dx + total_cesd_bl, 
         design = svy_designs_cu[[m]], 
         family = "quasibinomial")
})

m10c_mira <- list(analyses = m10c_results)
class(m10c_mira) <- "mira"

m10c_imp <- tbl_regression(m10c_mira,
                          exponentiate = TRUE,
                          include = ct_couse,
                          label = list(ct_couse ~ "Concurrent use")
)%>%
  modify_column_hide(p.value)
print(m10c_imp)

# Model 2: Pre-baseline confounding
m10a_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ ct_couse + bio_sex + race_ethnicity +
           maternal_edu + ace_score + adhd_dx + neuroticism_score, 
         design = svy_designs_cu[[m]], 
         family = "quasibinomial")
})

m10a_mira <- list(analyses = m10a_results)
class(m10a_mira) <- "mira"

m10a_imp <- tbl_regression(m10a_mira,
                           exponentiate = TRUE,
                           include = ct_couse,
                           label = list(ct_couse ~ "Concurrent use")
)%>%
  modify_column_hide(p.value)
print(m10a_imp)

# Model 3: Pre-baseline + baseline depression 
m10b_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ ct_couse + bio_sex + race_ethnicity +
           maternal_edu + ace_score + adhd_dx + neuroticism_score + total_cesd_bl, 
         design = svy_designs_cu[[m]], 
         family = "quasibinomial")
})

m10b_mira <- list(analyses = m10b_results)
class(m10b_mira) <- "mira"

m10b_imp <- tbl_regression(m10b_mira,
                           exponentiate = TRUE,
                           include = ct_couse,
                           label = list(ct_couse ~ "Concurrent use")
)%>%
  modify_column_hide(p.value)
print(m10b_imp)

# Combine the two tables into one side by side
couse_comb_imp <- tbl_merge(
  tbls = list(m9_imp, m10a_imp, m10b_imp, m10c_imp), 
  tab_spanner = c("Model 1", "Model 2", "Model 3", "Model 4")  
)

# Print the combined table
print(couse_comb_imp)

## ================== Combining analyses =======================================

# Merge all regression tables
imp_regs <- tbl_stack(
  tbls = list(evers_comb_imp, freqs_comb_imp, everc_comb_imp, freqc_comb_imp, couse_comb_imp)
)
print(imp_regs)

# Save combined IMP regression tables
#setwd("[file_path]")
imp_regs_flex <- as_flex_table(imp_regs) %>%
  add_footer_lines("Note. Models: (1) Unadjusted; (2) Adjusted for sex, ethnicity, maternal education, ADHD, ACEs and neuroticism; (3) Additionally adjusted for baseline depressive symptoms; (4) Additionally adjusted for cannabis/tobacco use, other illicit drug use and alcohol-related harms. OR: Odds Ratio; aOR: Adjusted Odds Ratio; CI: Confidence Interval. Analysis based on imputed sample (n = 5390) with sample weights from Add Health cohort.")

doc <- read_docx() %>%
  body_add_flextable(imp_regs_flex)
print(doc, target = "imp_regs50.docx")

## ================== Sensitivity analyses =====================================

# EVER SMOKED
# Model 5: Model 4 plus sleep difficulties, anxiety symptoms and conduct disorder score
m11a_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ ever_smok + ever_cannabis + bio_sex + race_ethnicity +
           maternal_edu + ever_drug + alcprob_score
         + neuroticism_score + total_cesd_bl
         + ace_score + adhd_dx + sleep_problems + anxiety_symptoms + cd_score, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})

m11a_mira <- list(analyses = m11a_results)
class(m11a_mira) <- "mira"

m11a_imp <- tbl_regression(m11a_mira,
                          exponentiate = TRUE,
                          include = ever_smok,
                          label = list(ever_smok ~ "Ever smoked")
)%>%
  modify_column_hide(p.value)
print(m11a_imp)

# EVER CANNABIS
# Model 5: Model 4 plus sleep difficulties, anxiety symptoms and conduct disorder score
m11b_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ ever_cannabis + ever_smok + bio_sex + race_ethnicity +
           maternal_edu + ever_drug + alcprob_score +
           neuroticism_score
         + ace_score + adhd_dx + total_cesd_bl + sleep_problems + anxiety_symptoms + cd_score, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})

m11b_mira <- list(analyses = m11b_results)
class(m11b_mira) <- "mira"

m11b_imp <- tbl_regression(m11b_mira,
                          exponentiate = TRUE,
                          include = ever_cannabis,
                          label = list(ever_cannabis ~ "Ever cannabis use")
)%>%
  modify_column_hide(p.value)
print(m11b_imp)

# SMOKING FREQUENCY
# Model 5: Model 4 plus sleep difficulties, anxiety symptoms and conduct disorder score
m11c_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ freq_smok + freq_can + bio_sex + race_ethnicity +
           maternal_edu + ever_drug + alcprob_score +
           neuroticism_score +
           ace_score + adhd_dx + total_cesd_bl + sleep_problems + anxiety_symptoms + cd_score, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})

m11c_mira <- list(analyses = m11c_results)
class(m11c_mira) <- "mira"

m11c_imp <- tbl_regression(m11c_mira,
                          exponentiate = TRUE,
                          include = freq_smok,
                          label = list(freq_smok ~ "Smoking frequency")
)%>%
  modify_column_hide(p.value)
print(m11c_imp)

# CANNABIS FREQUENCY
# Model 5: Model 4 plus sleep difficulties, anxiety symptoms and conduct disorder score
m11d_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ freq_can + freq_smok + bio_sex + race_ethnicity +
           maternal_edu + ever_drug + alcprob_score +
           neuroticism_score  +
           ace_score + adhd_dx + total_cesd_bl + sleep_problems + anxiety_symptoms + cd_score, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})

m11d_mira <- list(analyses = m11d_results)
class(m11d_mira) <- "mira"

m11d_imp <- tbl_regression(m11d_mira,
                          exponentiate = TRUE,
                          include = freq_can,
                          label = list(freq_can ~ "Cannabis use frequency")
)%>%
  modify_column_hide(p.value)
print(m11d_imp)

# CONCURRENT USE
# Model 5: Model 4 plus sleep difficulties, anxiety symptoms and conduct disorder score
m11e_results <- lapply(1:50, function(m) {
  svyglm(fu_dep ~ ct_couse + bio_sex + race_ethnicity +
           maternal_edu + ever_drug + alcprob_score +
           neuroticism_score + 
           ace_score + adhd_dx + total_cesd_bl + sleep_problems + anxiety_symptoms + cd_score, 
         design = svy_designs_cu[[m]], 
         family = "quasibinomial")
})

m11e_mira <- list(analyses = m11e_results)
class(m11e_mira) <- "mira"

m11e_imp <- tbl_regression(m11e_mira,
                           exponentiate = TRUE,
                           include = ct_couse,
                           label = list(ct_couse ~ "Concurrent use")
)%>%
  modify_column_hide(p.value)
print(m11e_imp)

# Combine the tables into one side by side
sens_imp50 <- tbl_stack(
  tbls = list(m11a_imp, m11b_imp, m11c_imp, m11d_imp, m11e_imp)
)

# Print the combined table
print(sens_imp50)

# Save combined SENS regression tables
#setwd("[file_path]")
sens_imp50_flex <- as_flex_table(sens_imp50) 

doc <- read_docx() %>%
  body_add_flextable(sens_imp50_flex)
print(doc, target = "sens_regs50.docx")


## ================== VIF check ================================================

#install.packages("car")
library(car)

# Create function to check VIF for all models
check_vif_all_models <- function() {
  # List to store results
  all_vif_results <- list()
  
  # Model 2c - Ever smoking
  m2c_vifs <- lapply(1:50, function(m) {
    model <- svyglm(fu_dep ~ ever_smok + bio_sex + race_ethnicity +
                      maternal_edu + adhd_dx + ace_score + neuroticism_score 
                    + ever_cannabis + ever_drug + alcprob_score + 
                      + total_cesd_bl, 
                    design = svy_designs_e[[m]], 
                    family = "quasibinomial")
    return(as.data.frame(vif(model)))
  })
  
  # Model 4c - Ever cannabis
  m4c_vifs <- lapply(1:50, function(m) {
    model <- svyglm(fu_dep ~ ever_cannabis + bio_sex + race_ethnicity +
                      maternal_edu + adhd_dx + ace_score + neuroticism_score 
                    + ever_smok + ever_drug + alcprob_score + 
                      + total_cesd_bl,
                    design = svy_designs_e[[m]], 
                    family = "quasibinomial")
    return(as.data.frame(vif(model)))
  })
  
  # Model 6c - Smoking frequency
  m6c_vifs <- lapply(1:50, function(m) {
    model <- svyglm(fu_dep ~ freq_smok + bio_sex + race_ethnicity +
                      maternal_edu + adhd_dx + ace_score + neuroticism_score 
                    + freq_can + ever_drug + alcprob_score + 
                      + total_cesd_bl, 
                    design = svy_designs_f[[m]], 
                    family = "quasibinomial")
    return(as.data.frame(vif(model)))
  })
  
  # Model 8c - Cannabis frequency
  m8c_vifs <- lapply(1:50, function(m) {
    model <- svyglm(fu_dep ~ freq_can + bio_sex + race_ethnicity +
                      maternal_edu + adhd_dx + ace_score + neuroticism_score 
                    + freq_smok + ever_drug + alcprob_score + 
                      + total_cesd_bl, 
                    design = svy_designs_f[[m]], 
                    family = "quasibinomial")
    return(as.data.frame(vif(model)))
  })
  
  # Model 10c - Concurrent use
  m10c_vifs <- lapply(1:50, function(m) {
    model <- svyglm(fu_dep ~ ct_couse + bio_sex + race_ethnicity +
                      maternal_edu + adhd_dx + ace_score + neuroticism_score 
                    + ever_drug + alcprob_score + 
                      + total_cesd_bl, 
                    design = svy_designs_cu[[m]], 
                    family = "quasibinomial")
    return(as.data.frame(vif(model)))
  })
  
  # Calculate ranges for each model
  process_vifs <- function(vif_list, model_name) {
    vif_ranges <- data.frame(
      min = do.call(pmin, vif_list),
      max = do.call(pmax, vif_list)
    )
    return(vif_ranges)
  }
  
  all_vif_results[["Model 2c - Ever smoking"]] <- process_vifs(m2c_vifs)
  all_vif_results[["Model 4c - Ever cannabis"]] <- process_vifs(m4c_vifs)
  all_vif_results[["Model 6c - Smoking frequency"]] <- process_vifs(m6c_vifs)
  all_vif_results[["Model 8c - Cannabis frequency"]] <- process_vifs(m8c_vifs)
  all_vif_results[["Model 10c - Concurrent use"]] <- process_vifs(m10c_vifs)
  
  return(all_vif_results)
}

# Run the analysis
vif_results <- check_vif_all_models()

# Print results for each model
for(model_name in names(vif_results)) {
  cat("\n======================")
  cat("\nResults for", model_name, ":\n")
  print(vif_results[[model_name]])
  cat("\n======================\n")
}

#---Exporting results---#

# Modified export code
vif_results_combined <- do.call(rbind, lapply(names(vif_results), function(model_name) {
  model_data <- vif_results[[model_name]]
  data.frame(
    model = rep(model_name, nrow(model_data)),
    variable = rownames(model_data),
    min_GVIF = model_data$min.GVIF,
    min_Df = model_data$min.Df,
    min_GVIF_adjusted = model_data$min.GVIF..1..2.Df..,
    max_GVIF = model_data$max.GVIF,
    max_Df = model_data$max.Df,
    max_GVIF_adjusted = model_data$max.GVIF..1..2.Df..,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}))

# Export to CSV
write.csv(vif_results_combined, "vif_results_all_models.csv", row.names = FALSE)

## ================== End of script ============================================

# Open script called 'addh_cca.R' 
