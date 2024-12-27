'''
---
title: "Cross-cohort analysis cannabis depression"
analyst: Chloe Burke
dataset: ALSPAC
date: October, 2024
script: "Imputation and analysis"
---
'''

# LOAD PACKAGES
#install.packages("mice")
library(mice)
#install.packages("survey")
library(survey)
#install.packages("howManyImputations")
library(howManyImputations)
#install.packages("reshape2")
library(reshape2)
#install.packages("gtsummary")
library(gtsummary)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("mitools")
library(mitools)
#install.packages("skimr")
library(skimr)
#install.packages("flextable")
library(flextable)
#install.packages("officer")
library(officer)

# Note: Replace [file_path] with your local directory containing the ALSPAC data files

## ================== Clear environment ========================================

rm(list = setdiff(ls(), "xyz_alspac"))

## ================== Model parameter tests ====================================

#------Ever Use-------#

# A subset of variables is selected
ever_vars <- c("new_id", "age_years", "followup_months", 
               "smok_ever", "cannabis_ever", "smfq_bl_total", "ethnicity", "sex",
               "maternal_edu", "audit_total", "ever_drug", "sleep_problems",
               "neuroticism", "anx_dawba", "cd_score", "adhd_dx_final", "ace_score", "cisr_dep", 
               "home_own", "dsh", "urban", "asb_10", "fq_tired")

# Restricting df to analysis variables
impdata_es <- xyz_alspac[,ever_vars]

# Check variable class
sapply(impdata_es, class)

# Change variable formats
impdata_es$ace_score <- as.factor(impdata_es$ace_score) 

# Checking missing values for all variables
skim(impdata_es)

# Define the list of variables to be imputed and regular variables
imputed_vars <- c("smok_ever", "cannabis_ever", "smfq_bl_total", "ethnicity",
                  "maternal_edu", "audit_total", "ever_drug", "sleep_problems",
                  "neuroticism", "anx_dawba", "cd_score", "adhd_dx_final", "ace_score", "cisr_dep",
                  "home_own", "dsh", "urban", "asb_10", "fq_tired")  
regular_vars <- c("followup_months", "age_years")
id_vars <- c("new_id") 

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

# Convert to long format 
long_data <- complete(imp_es, action = "long", include = TRUE)

# Check for remaining missing values
remaining_missing <- long_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# Set methods
methods_e <- imp_es$method
for (var in c(id_vars, regular_vars)) {  
  methods_e[var] <- ""  # Set to no imputation
}

print(methods_e)

# Run test imputation
imp_es20 <- mice(impdata_es, 
                 m = 20, 
                 maxit = 5, 
                 predictorMatrix = predictor_matrix_e, 
                 method = methods_e, 
                 seed = 123, 
                 print = FALSE)

# Convert to long format 
long_data <- complete(imp_es20, action = "long", include = TRUE)

# Check for remaining missing values
remaining_missing <- long_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# Create list of survey designs for each imputed dataset, with subsetting
svy_designs_e <- lapply(1:20, function(i) {
  data_i <- complete(imp_es20, i)
  design_i <- svydesign(
    id = ~new_id,         
    weights = NULL,      
    strata = NULL,      
    nest = FALSE,
    data = data_i
  )
})

# Full adjusted model
m2_results <- lapply(1:20, function(m) {
  svyglm(cisr_dep ~ smok_ever + cannabis_ever + sex + ethnicity +
           maternal_edu + ever_drug + audit_total + smfq_bl_total +
           sleep_problems + neuroticism + anx_dawba +
           cd_score + ace_score + adhd_dx_final, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})

m2_mira <- list(analyses = m2_results)
class(m2_mira) <- "mira"

m2_imp <- tbl_regression(m2_mira,
                         exponentiate = TRUE,
                         include = smok_ever,
                         label = list(smok_ever ~ "Ever smoked")
)
print(m2_imp)

# Checking FMI
how_many_imputations(m2_results, cv = 0.05, alpha = 0.05) #n = 57

#------Frequency Variables -------#

# A subset of variables is selected
freq_vars <- c("new_id", "age_years", "followup_months", 
               "smok_freq", "cannabis_freq", "smfq_bl_total", "ethnicity", "sex",
               "maternal_edu", "audit_total", "ever_drug", "sleep_problems",
               "neuroticism", "anx_dawba", "cd_score", "adhd_dx_final", "ace_score", "cisr_dep", 
               "home_own", "dsh", "urban", "asb_10", "fq_tired")

# Restricting df to analysis variables
impdata_fs <- xyz_alspac[,freq_vars]

# Check variable class
sapply(impdata_fs, class)

# Change variable formats
impdata_fs$ace_score <- as.factor(impdata_es$ace_score) 

# Checking missing values for all variables
skim(impdata_fs)

# Define the list of variables to be imputed and regular variables
imputed_vars <- c("smok_freq", "cannabis_freq", "smfq_bl_total", "ethnicity",
                  "maternal_edu", "audit_total", "ever_drug", "sleep_problems",
                  "neuroticism", "anx_dawba", "cd_score", "adhd_dx_final", "ace_score", "cisr_dep",
                  "home_own", "dsh", "urban", "asb_10", "fq_tired")  
regular_vars <- c("followup_months", "age_years")
id_vars <- c("new_id") 

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

# Convert to long format 
long_data <- complete(imp_fs, action = "long", include = TRUE)

# Check for remaining missing values
remaining_missing <- long_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# Set methods
methods_f <- imp_fs$method
for (var in c(id_vars, regular_vars)) {  
  methods_f[var] <- ""  # Set to no imputation
}

print(methods_f)

# Run test imputation
imp_fs20 <- mice(impdata_fs, 
                 m = 20, 
                 maxit = 5, 
                 predictorMatrix = predictor_matrix_f, 
                 method = methods_f, 
                 seed = 123, 
                 print = FALSE)

# Convert to long format 
long_data <- complete(imp_fs20, action = "long", include = TRUE)

# Check for remaining missing values
remaining_missing <- long_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# Create list of survey designs for each imputed dataset, with subsetting
svy_designs_f <- lapply(1:20, function(i) {
  data_i <- complete(imp_fs20, i)
  design_i <- svydesign(
    id = ~new_id,         
    weights = NULL,      
    strata = NULL,      
    nest = FALSE,
    data = data_i
  )
})

# Full adjusted model
m4c_results <- lapply(1:20, function(m) {
  svyglm(cisr_dep ~ smok_freq + cannabis_freq + sex + ethnicity +
           maternal_edu + ever_drug + audit_total +smfq_bl_total +
           sleep_problems + neuroticism + anx_dawba +
           cd_score + ace_score + adhd_dx_final, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})

m4c_mira <- list(analyses = m4c_results)
class(m4c_mira) <- "mira"

m4c_imp <- tbl_regression(m4c_mira,
                          exponentiate = TRUE,
                          include = smok_freq,
                          label = list(smok_freq ~ "Smoking frequency")
)
print(m4c_imp)

how_many_imputations(m4c_results, cv = 0.05, alpha = 0.05) # n = 38

#------Concurrent Use -------#

# A subset of variables is selected
couse_vars <- c("new_id", "age_years", "followup_months", 
                "ct_couse", "smfq_bl_total", "ethnicity", "sex",
                "maternal_edu", "audit_total", "ever_drug", "sleep_problems",
                "neuroticism", "anx_dawba", "cd_score", "adhd_dx_final", "ace_score", "cisr_dep", 
                "home_own", "dsh", "urban", "asb_10", "fq_tired")

# Restricting df to analysis variables
impdata_cu <- xyz_alspac[,couse_vars]

# Check variable class
sapply(impdata_cu, class)

# Change variable formats
impdata_cu$ace_score <- as.factor(impdata_cu$ace_score) 

# Checking missing values for all variables
skim(impdata_cu)

# Define the list of variables to be imputed and regular variables
imputed_vars <- c("ct_couse", "smfq_bl_total", "ethnicity",
                  "maternal_edu", "audit_total", "ever_drug", "sleep_problems",
                  "neuroticism", "anx_dawba", "cd_score", "adhd_dx_final", "ace_score", "cisr_dep",
                  "home_own", "dsh", "urban", "asb_10", "fq_tired")  
regular_vars <- c("followup_months", "age_years")
id_vars <- c("new_id") 

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

# Convert to long format 
long_data <- complete(imp_cu, action = "long", include = TRUE)

# Check for remaining missing values
remaining_missing <- long_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# Set methods
methods_cu <- imp_cu$method
for (var in c(id_vars, regular_vars)) {  
  methods_cu[var] <- ""  # Set to no imputation
}

print(methods_cu)

# Run test imputation
imp_cu20 <- mice(impdata_cu, 
                 m = 20, 
                 maxit = 5, 
                 predictorMatrix = predictor_matrix_cu, 
                 method = methods_cu, 
                 seed = 123, 
                 print = FALSE)

# Convert to long format 
long_data <- complete(imp_cu20, action = "long", include = TRUE)

# Check for remaining missing values
remaining_missing <- long_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# Create list of survey designs for each imputed dataset, with subsetting
svy_designs_cu <- lapply(1:20, function(i) {
  data_i <- complete(imp_cu20, i)
  design_i <- svydesign(
    id = ~new_id,         
    weights = NULL,      
    strata = NULL,      
    nest = FALSE,
    data = data_i
  )
})

# Full adjusted model
m10c_results <- lapply(1:20, function(m) {
  svyglm(cisr_dep ~ ct_couse + sex + ethnicity +
           maternal_edu + ever_drug + audit_total + smfq_bl_total +
           sleep_problems + neuroticism + anx_dawba +
           cd_score + ace_score + adhd_dx_final, 
         design = svy_designs_cu[[m]], 
         family = "quasibinomial")
})

m10c_mira <- list(analyses = m10c_results)
class(m10c_mira) <- "mira"

m10c_imp <- tbl_regression(m10c_mira,
                           exponentiate = TRUE,
                           include = ct_couse,
                           label = list(ct_couse ~ "Concurrent use")
)
print(m10c_imp)

how_many_imputations(m10c_results, cv = 0.05, alpha = 0.05) #n = 48

#------Save------#

setwd("[file_path]")
saveRDS(imp_es20, "ever_imp20.rds")
saveRDS(imp_fs20, "freq_imp20.rds")
saveRDS(imp_cu20, "couse_imp20.rds")

## ================== Perform imputation =======================================

#---Batch run final models---#

imp_es60 <- mice(impdata_es, 
                 m = 60, 
                 maxit = 30, 
                 predictorMatrix = predictor_matrix_e, 
                 method = methods_e, 
                 seed = 123, 
                 print = FALSE)

imp_fs60 <- mice(impdata_fs, 
                 m = 60, 
                 maxit = 30, 
                 predictorMatrix = predictor_matrix_f, 
                 method = methods_f, 
                 seed = 123, 
                 print = FALSE)

imp_cu60 <- mice(impdata_cu, 
                 m = 60, 
                 maxit = 30, 
                 predictorMatrix = predictor_matrix_cu, 
                 method = methods_cu, 
                 seed = 123, 
                 print = FALSE)

#------Save------#

setwd("[file_path]")
#saveRDS(imp_es60, "ever_imp60.rds")
#saveRDS(imp_fs60, "freq_imp60.rds")
#saveRDS(imp_cu60, "couse_imp60.rds")
imp_es60 <- readRDS("ever_imp60.rds")
imp_fs60 <- readRDS("freq_imp60.rds")
imp_cu60 <- readRDS("couse_imp60.rds")

## ================== Check models =============================================

#---Check summaries of imputation---#

# EVER USE #
summary(imp_es60)
imp_es60$loggedEvents

# FREQUENCY VARIABLES #
summary(imp_fs60)
imp_fs60$loggedEvents

# CO USE # 
summary(imp_cu60)
imp_cu60$loggedEvents

#---Check no remaining missing data---#

# EVER USE #
long_data <- complete(imp_es60, action = "long", include = TRUE)
remaining_missing <- long_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# FREQUENCY VARIABLES #
long_data <- complete(imp_fs60, action = "long", include = TRUE)
remaining_missing <- long_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# CO USE # 
long_data <- complete(imp_cu60, action = "long", include = TRUE)
remaining_missing <- long_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

#---Look at model convergence---#
# Load propplot.R function
source("[file_path]/propplot.R")

# EVER USE #
plot(imp_es60)
densityplot(imp_es60)
propplot(imp_es60)

# FREQUENCY VARIABLES #
plot(imp_fs60)
densityplot(imp_fs60)
propplot(imp_fs60)

# CO USE # 
plot(imp_cu60)
densityplot(imp_cu60)
propplot(imp_cu60)

## ================== Perform analysis =========================================

#-------EVER USE------#

# Import the imputed object
setwd("[file_path]")
imp_es60<- readRDS("ever_imp60.rds")

# Create list of survey designs for each imputed dataset
svy_designs_e <- lapply(1:60, function(i) {
  data_i <- complete(imp_es60, i)
  design_i <- svydesign(
    id = ~new_id,        
    weights = NULL,      
    strata = NULL,      
    nest = FALSE,
    data = data_i
  )
})

#----------------------#

# Ever smoked #

# Model 1: Unadjusted
m1_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ smok_ever, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})
m1_mira <- list(analyses = m1_results)
class(m1_mira) <- "mira"
m1_imp <- tbl_regression(m1_mira,
                         exponentiate = TRUE,
                         label = list(smok_ever ~ "Ever smoked")
)%>%
  modify_column_hide(p.value)
print(m1_imp)

# Model 4: Fully-adjusted (i.e., pre-baseline Z, other substance use, baseline depression)
m2c_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ smok_ever + sex + ethnicity +
           maternal_edu + adhd_dx_final + ace_score + neuroticism 
         + cannabis_ever + ever_drug + audit_total + 
           + smfq_bl_total, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})
m2c_mira <- list(analyses = m2c_results)
class(m2c_mira) <- "mira"
m2c_imp <- tbl_regression(m2c_mira,
                          exponentiate = TRUE,
                          include = smok_ever,
                          label = list(smok_ever ~ "Ever smoked")
)%>%
  modify_column_hide(p.value)
print(m2c_imp)

# Model 2: Pre-baseline confounding
m2a_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ smok_ever + sex + ethnicity +
           maternal_edu + adhd_dx_final + ace_score + neuroticism, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})
m2a_mira <- list(analyses = m2a_results)
class(m2a_mira) <- "mira"
m2a_imp <- tbl_regression(m2a_mira,
                          exponentiate = TRUE,
                          include = smok_ever,
                          label = list(smok_ever ~ "Ever smoked")
)%>%
  modify_column_hide(p.value)
print(m2a_imp)

# Model 3: Pre-baseline + baseline depression 
m2b_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ smok_ever + sex + ethnicity +
           maternal_edu + adhd_dx_final + ace_score + neuroticism + smfq_bl_total, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})
m2b_mira <- list(analyses = m2b_results)
class(m2b_mira) <- "mira"
m2b_imp <- tbl_regression(m2b_mira,
                          exponentiate = TRUE,
                          include = smok_ever,
                          label = list(smok_ever ~ "Ever smoked")
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

#----------------------#

# Ever used cannabis #

# Model 1: Unadjusted
m3_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ cannabis_ever, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})
m3_mira <- list(analyses = m3_results)
class(m3_mira) <- "mira"
m3_imp <- tbl_regression(m3_mira,
                         exponentiate = TRUE,
                         label = list(cannabis_ever ~ "Ever used cannabis")
)%>%
  modify_column_hide(p.value)
print(m3_imp)

# Model 4: Fully-adjusted (i.e., pre-baseline Z, other substance use, baseline depression)
m4c_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ cannabis_ever + sex + ethnicity +
           maternal_edu + adhd_dx_final + ace_score + neuroticism 
         + smok_ever + ever_drug + audit_total + 
           + smfq_bl_total,
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})
m4c_mira <- list(analyses = m4c_results)
class(m4c_mira) <- "mira"
m4c_imp <- tbl_regression(m4c_mira,
                          exponentiate = TRUE,
                          include = cannabis_ever,
                          label = list(cannabis_ever ~ "Ever used cannabis")
)%>%
  modify_column_hide(p.value)
print(m4c_imp)

# Model 2: Pre-baseline confounding
m4a_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ cannabis_ever + sex + ethnicity +
           maternal_edu + adhd_dx_final + ace_score + neuroticism, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})
m4a_mira <- list(analyses = m4a_results)
class(m4a_mira) <- "mira"
m4a_imp <- tbl_regression(m4a_mira,
                          exponentiate = TRUE,
                          include = cannabis_ever,
                          label = list(cannabis_ever ~ "Ever used cannabis")
)%>%
  modify_column_hide(p.value)
print(m4a_imp)

# Model 3: Pre-baseline + baseline depression 
m4b_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ cannabis_ever + sex + ethnicity +
           maternal_edu + adhd_dx_final + ace_score + neuroticism + smfq_bl_total, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})
m4b_mira <- list(analyses = m4b_results)
class(m4b_mira) <- "mira"
m4b_imp <- tbl_regression(m4b_mira,
                          exponentiate = TRUE,
                          include = cannabis_ever,
                          label = list(cannabis_ever ~ "Ever used cannabis")
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

#-------FREQUENCY VARIABLES------#

# Import the imputed object
setwd("[file_path]")
imp_fs60 <- readRDS("freq_imp60.rds")

# Create list of survey designs for each imputed dataset
svy_designs_f <- lapply(1:60, function(i) {
  data_i <- complete(imp_fs60, i)
  design_i <- svydesign(
    id = ~new_id,         
    weights = NULL,      
    strata = NULL,      
    nest = FALSE,
    data = data_i
  )
})

#----------------------#

# Smoking frequency #

# Model 1: Unadjusted
m5_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ smok_freq, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})
m5_mira <- list(analyses = m5_results)
class(m5_mira) <- "mira"
m5_imp <- tbl_regression(m5_mira,
                         exponentiate = TRUE,
                         label = list(smok_freq ~ "Smoking frequency")
)%>%
  modify_column_hide(p.value)
print(m5_imp)

# Model 4: Fully-adjusted (i.e., pre-baseline Z, other substance use, baseline depression)
m6c_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ smok_freq + sex + ethnicity +
           maternal_edu + adhd_dx_final + ace_score + neuroticism 
         + cannabis_freq + ever_drug + audit_total + 
           + smfq_bl_total, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})
m6c_mira <- list(analyses = m6c_results)
class(m6c_mira) <- "mira"
m6c_imp <- tbl_regression(m6c_mira,
                          exponentiate = TRUE,
                          include = smok_freq,
                          label = list(smok_freq ~ "Smoking frequency")
)%>%
  modify_column_hide(p.value)
print(m6c_imp)

# Model 2: Pre-baseline confounding
m6a_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ smok_freq + sex + ethnicity +
           maternal_edu + ace_score + adhd_dx_final + neuroticism, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})
m6a_mira <- list(analyses = m6a_results)
class(m6a_mira) <- "mira"
m6a_imp <- tbl_regression(m6a_mira,
                          exponentiate = TRUE,
                          include = smok_freq,
                          label = list(smok_freq ~ "Smoking frequency")
)%>%
  modify_column_hide(p.value)
print(m6a_imp)

# Model 3: Pre-baseline + baseline depression
m6b_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ smok_freq + sex + ethnicity +
           maternal_edu + ace_score + adhd_dx_final + smfq_bl_total, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})
m6b_mira <- list(analyses = m6b_results)
class(m6b_mira) <- "mira"
m6b_imp <- tbl_regression(m6b_mira,
                          exponentiate = TRUE,
                          include = smok_freq,
                          label = list(smok_freq ~ "Smoking frequency")
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

#----------------------#

# Cannabis frequency #

# Model 1: Unadjusted
m7_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ cannabis_freq, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})
m7_mira <- list(analyses = m7_results)
class(m7_mira) <- "mira"
m7_imp <- tbl_regression(m7_mira,
                         exponentiate = TRUE,
                         label = list(cannabis_freq ~ "Cannabis use frequency")
)%>%
  modify_column_hide(p.value)
print(m7_imp)

# Model 4: Fully-adjusted (i.e., pre-baseline Z, other substance use, baseline depression)
m8c_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ cannabis_freq + sex + ethnicity +
           maternal_edu + adhd_dx_final + ace_score + neuroticism 
         + smok_freq + ever_drug + audit_total + 
           + smfq_bl_total, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})
m8c_mira <- list(analyses = m8c_results)
class(m8c_mira) <- "mira"
m8c_imp <- tbl_regression(m8c_mira,
                          exponentiate = TRUE,
                          include = cannabis_freq,
                          label = list(cannabis_freq ~ "Cannabis use frequency")
)%>%
  modify_column_hide(p.value)
print(m8c_imp)

# Model 2: Pre-baseline confounding
m8a_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ cannabis_freq + sex + ethnicity +
           maternal_edu + ace_score + adhd_dx_final + neuroticism, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})
m8a_mira <- list(analyses = m8a_results)
class(m8a_mira) <- "mira"
m8a_imp <- tbl_regression(m8a_mira,
                          exponentiate = TRUE,
                          include = cannabis_freq,
                          label = list(cannabis_freq ~ "Cannabis use frequency")
)%>%
  modify_column_hide(p.value)
print(m8a_imp)

# Model 3: Pre-baseline + baseline depression
m8b_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ cannabis_freq + sex + ethnicity +
           maternal_edu + ace_score + adhd_dx_final + smfq_bl_total, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})
m8b_mira <- list(analyses = m8b_results)
class(m8b_mira) <- "mira"
m8b_imp <- tbl_regression(m8b_mira,
                          exponentiate = TRUE,
                          include = cannabis_freq,
                          label = list(cannabis_freq ~ "Cannabis use frequency")
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

#-------COUSE VARIABLES------#

# Import the imputed object
setwd("[file_path]")
imp_cu60 <- readRDS("couse_imp60.rds")

# Create list of survey designs for each imputed dataset
svy_designs_cu <- lapply(1:60, function(i) {
  data_i <- complete(imp_cu60, i)
  design_i <- svydesign(
    id = ~new_id,         
    weights = NULL,      
    strata = NULL,      
    nest = FALSE,
    data = data_i
  )
})

#----------------------#

# Concurrent use #

# Model 1: Unadjusted
m9_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ ct_couse, 
         design = svy_designs_cu[[m]], 
         family = "quasibinomial")
})
m9_mira <- list(analyses = m9_results)
class(m9_mira) <- "mira"
m9_imp <- tbl_regression(m9_mira,
                         exponentiate = TRUE,
                         label = list(ct_couse ~ "Concurrent use")
)
print(m9_imp)

# Model 4: Fully-adjusted (i.e., pre-baseline Z, other substance use, baseline depression)
m10c_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ ct_couse + sex + ethnicity +
           maternal_edu + adhd_dx_final + ace_score + neuroticism 
         + ever_drug + audit_total + 
           + smfq_bl_total, 
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
m10a_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ ct_couse + sex + ethnicity +
           maternal_edu + ace_score + adhd_dx_final + neuroticism, 
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
m10b_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ ct_couse + sex + ethnicity +
           maternal_edu + ace_score + adhd_dx_final + smfq_bl_total, 
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
setwd("[file_path]")
imp_regs_flex <- as_flex_table(imp_regs)%>%
add_footer_lines("Note. Models: (1) Unadjusted; (2) Adjusted for sex, ethnicity, maternal education, ADHD, ACEs and neuroticism; (3) Additionally adjusted for baseline depressive symptoms; (4) Additionally adjusted for cannabis/tobacco use, other illicit drug use and hazardous alcohol use. OR: Odds Ratio; aOR: Adjusted Odds Ratio; CI: Confidence Interval. Analysis based on imputed sample (n = 3557) from ALSPAC cohort.")

doc <- read_docx() %>%
  body_add_flextable(imp_regs_flex)
print(doc, target = "imp_regs60.docx")

## ================== Sensitivity analyses =====================================

# EVER SMOKED
# Model 5: Model 4 plus sleep difficulties, anxiety symptoms and conduct disorder score
m11a_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ smok_ever + cannabis_ever + sex + ethnicity +
           maternal_edu + ever_drug + audit_total
         + neuroticism + smfq_bl_total
         + ace_score + adhd_dx_final + sleep_problems + anx_dawba + cd_score, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})

m11a_mira <- list(analyses = m11a_results)
class(m11a_mira) <- "mira"

m11a_imp <- tbl_regression(m11a_mira,
                           exponentiate = TRUE,
                           include = smok_ever,
                           label = list(smok_ever ~ "Ever smoked")
)%>%
  modify_column_hide(p.value)
print(m11a_imp)

# EVER CANNABIS
# Model 5: Model 4 plus sleep difficulties, anxiety symptoms and conduct disorder score
m11b_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ cannabis_ever + smok_ever + sex + ethnicity +
           maternal_edu + ever_drug + audit_total +
           neuroticism
         + ace_score + adhd_dx_final + smfq_bl_total + sleep_problems + anx_dawba + cd_score, 
         design = svy_designs_e[[m]], 
         family = "quasibinomial")
})

m11b_mira <- list(analyses = m11b_results)
class(m11b_mira) <- "mira"

m11b_imp <- tbl_regression(m11b_mira,
                           exponentiate = TRUE,
                           include = cannabis_ever,
                           label = list(cannabis_ever ~ "Ever cannabis use")
)%>%
  modify_column_hide(p.value)
print(m11b_imp)

# SMOKING FREQUENCY
# Model 5: Model 4 plus sleep difficulties, anxiety symptoms and conduct disorder score
m11c_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ smok_freq + cannabis_freq + sex + ethnicity +
           maternal_edu + ever_drug + audit_total +
           neuroticism +
           ace_score + adhd_dx_final + smfq_bl_total + sleep_problems + anx_dawba + cd_score, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})

m11c_mira <- list(analyses = m11c_results)
class(m11c_mira) <- "mira"

m11c_imp <- tbl_regression(m11c_mira,
                           exponentiate = TRUE,
                           include = smok_freq,
                           label = list(smok_freq ~ "Smoking frequency")
)%>%
  modify_column_hide(p.value)
print(m11c_imp)

# CANNABIS FREQUENCY
# Model 5: Model 4 plus sleep difficulties, anxiety symptoms and conduct disorder score
m11d_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ cannabis_freq + smok_freq + sex + ethnicity +
           maternal_edu + ever_drug + audit_total +
           neuroticism  +
           ace_score + adhd_dx_final + smfq_bl_total + sleep_problems + anx_dawba + cd_score, 
         design = svy_designs_f[[m]], 
         family = "quasibinomial")
})

m11d_mira <- list(analyses = m11d_results)
class(m11d_mira) <- "mira"

m11d_imp <- tbl_regression(m11d_mira,
                           exponentiate = TRUE,
                           include = cannabis_freq,
                           label = list(cannabis_freq ~ "Cannabis use frequency")
)%>%
  modify_column_hide(p.value)
print(m11d_imp)

# CONCURRENT USE
# Model 5: Model 4 plus sleep difficulties, anxiety symptoms and conduct disorder score
m11e_results <- lapply(1:60, function(m) {
  svyglm(cisr_dep ~ ct_couse + sex + ethnicity +
           maternal_edu + ever_drug + audit_total +
           neuroticism + 
           ace_score + adhd_dx_final + smfq_bl_total + sleep_problems + anx_dawba + cd_score, 
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
sens_imp60 <- tbl_stack(
  tbls = list(m11a_imp, m11b_imp, m11c_imp, m11d_imp, m11e_imp)
)

# Print the combined table
print(sens_imp60)

# Save combined SENS regression tables
setwd("[file_path]")
sens_imp60_flex <- as_flex_table(sens_imp60)%>%
  add_footer_lines("Note. Adjusted for sex, ethnicity, maternal education, ADHD, ACEs and neuroticism, baseline depressive symptoms, cannabis/tobacco use, other illicit drug use, alcohol-related harms, symptoms of anxiety, sleep difficulties and symptoms of conduct disorder. OR: Odds Ratio; aOR: Adjusted Odds Ratio; CI: Confidence Interval. Analysis based on imputed sample (n = 3557) from ALSPAC cohort.")
doc <- read_docx() %>%
  body_add_flextable(sens_imp60_flex)
print(doc, target = "sens_regs60.docx")


## ================== VIF check ================================================

#install.packages("car")
library(car)

# Create function to check VIF for all models
check_vif_all_models <- function() {
  # List to store results
  all_vif_results <- list()
  
  # Model 2c - Ever smoking
  m2c_vifs <- lapply(1:60, function(m) {
    model <- svyglm(cisr_dep ~ smok_ever + sex + ethnicity +
                      maternal_edu + adhd_dx_final + ace_score + neuroticism 
                    + cannabis_ever + ever_drug + audit_total + 
                      + smfq_bl_total, 
                    design = svy_designs_e[[m]], 
                    family = "quasibinomial")
    return(as.data.frame(vif(model)))
  })
  
  # Model 4c - Ever cannabis
  m4c_vifs <- lapply(1:60, function(m) {
    model <- svyglm(cisr_dep ~ cannabis_ever + sex + ethnicity +
                      maternal_edu + adhd_dx_final + ace_score + neuroticism 
                    + smok_ever + ever_drug + audit_total + 
                      + smfq_bl_total,
                    design = svy_designs_e[[m]], 
                    family = "quasibinomial")
    return(as.data.frame(vif(model)))
  })
  
  # Model 6c - Smoking frequency
  m6c_vifs <- lapply(1:60, function(m) {
    model <- svyglm(cisr_dep ~ smok_freq + sex + ethnicity +
                      maternal_edu + adhd_dx_final + ace_score + neuroticism 
                    + cannabis_freq + ever_drug + audit_total + 
                      + smfq_bl_total, 
                    design = svy_designs_f[[m]], 
                    family = "quasibinomial")
    return(as.data.frame(vif(model)))
  })
  
  # Model 8c - Cannabis frequency
  m8c_vifs <- lapply(1:60, function(m) {
    model <- svyglm(cisr_dep ~ cannabis_freq + sex + ethnicity +
                      maternal_edu + adhd_dx_final + ace_score + neuroticism 
                    + smok_freq + ever_drug + audit_total + 
                      + smfq_bl_total, 
                    design = svy_designs_f[[m]], 
                    family = "quasibinomial")
    return(as.data.frame(vif(model)))
  })
  
  # Model 10c - Concurrent use
  m10c_vifs <- lapply(1:60, function(m) {
    model <- svyglm(cisr_dep ~ ct_couse + sex + ethnicity +
                      maternal_edu + adhd_dx_final + ace_score + neuroticism 
                    + ever_drug + audit_total + 
                      + smfq_bl_total, 
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

## ====================== End of script ========================================

# Open script called 'alspac_cca.R'