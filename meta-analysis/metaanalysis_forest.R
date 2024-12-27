'''
---
title: "Cross-cohort analysis cannabis depression"
analyst: Chloe Burke
dataset: ALSPAC, LSAC, Add Health
date: October, 2024
script: "Meta-analysis"
---
'''

#---Load packages---#

# Required packages
#install.packages("tidyverse")
library(tidyverse)
#install.packages("openxlsx")
library(openxlsx)
#install.packages("metafor")
library(metafor)
#install.packages("forcats")
library(forcats)

#---Import Data---#

# Import data 'allresults_m1'; ORs and 95% CIs from unadjusted analysis
# Import data 'allresults_m4'; ORs and 95% CIs from adjusted analysis

#---Unadjusted Analysis---#

# MODEL 1 #
# Read and process data with ordered exposures
df_m1 <- allresults_m1 %>%
  as_tibble() %>%
  mutate(
    exposure = factor(exposure, 
                      levels = c("Ever smoked", "Ever cannabis",
                                 "Weekly smoking", "Weekly cannabis", 
                                 "Co-use"),
                      ordered = TRUE)
  )

# Function to run meta-analysis
run_meta_detailed <- function(data, exp) {
  subset_data <- filter(data, exposure == exp)
  
  meta <- rma(
    yi = log(beta),
    sei = (log(upper) - log(lower))/(2*1.96),
    data = subset_data,
    method = "REML"
  )
  
  results <- data.frame(
    exposure = exp,
    study = "Meta-analysis",
    beta = exp(meta$b),
    lower = exp(meta$ci.lb),
    upper = exp(meta$ci.ub),
    p_value = meta$pval,
    I2 = meta$I2,
    tau2 = meta$tau2,
    Q_stat = meta$QE,
    Q_p = meta$QEp,
    k = meta$k
  )
  
  return(results)
}

# Run meta-analyses and combine results
m1_results <- lapply(levels(df_m1$exposure), function(exp) {
  run_meta_detailed(df_m1, exp)
})
m1 <- bind_rows(m1_results) %>%
  mutate(exposure = factor(exposure,
                           levels = c("Ever smoked", "Ever cannabis",
                                      "Weekly smoking", "Weekly cannabis", 
                                      "Co-use"),
                           ordered = TRUE))
# Update x_limit
x_limit <- 3.0

# In the plot data preparation 
m1_combined <- bind_rows(
  df_m1,
  select(m1, exposure, study, beta, lower, upper)
) %>%
  mutate(
    study = factor(study, levels = c("Meta-analysis", "ALSPAC", "LSAC", "Add Health")),
    exposure = factor(exposure,
                      levels = c("Ever smoked", "Ever cannabis",
                                 "Weekly smoking", "Weekly cannabis", 
                                 "Co-use"),
                      ordered = TRUE),
    exceeds_limit = upper > x_limit,
    upper_plot = pmin(upper, x_limit)
  )

# Update the plot code
p1 <- ggplot(m1_combined, aes(y = fct_rev(exposure), x = beta, color = study)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = lower, xmax = pmin(upper, 3.0), shape = study),
                  position = position_dodge(width = 0.5)) +
  geom_segment(data = subset(m1_combined, upper > 3.0),
               aes(x = 2.9, xend = 3.0, 
                   y = as.numeric(fct_rev(exposure)) + 
                     (as.numeric(study) - 2.5) * 0.5),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               linewidth = 0.5) +
  scale_shape_manual(values = c(18, 16, 16, 16)) +
  scale_color_manual(values = c("black", "#d95f02", "#1b9e77", "#7570b3")) +
  scale_x_continuous(breaks = seq(0.5, 3.0, by = 0.5), 
                     limits = c(0.5, 3.0)) +
  coord_cartesian(xlim = c(0.5, 3.0), clip = "off") +
  labs(x = "Odds ratio (95% CI)", y = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_blank(),
    legend.key.size = unit(1, "cm"),
    # Add this line to prevent arrows in legend:
    legend.key.height = unit(0.5, "cm")
  )

# Print plot
print(p1)

# Save detailed results
write.xlsx(m1, "meta_analysis_results_m1.xlsx")

# Print summary table of results
print("Summary of Meta-Analysis Results:")
print(m1 %>%
        select(exposure, beta, lower, upper, I2, p_value))

ggsave("forest_plotm1.png", p1, width = 10, height = 8, dpi = 700)
# For PDF:
ggsave("forest_plotm1.pdf", p1, width = 10, height = 8)

#---Adjusted Analysis---#


# MODEL 4 #
# Read and process data with ordered exposures
df_m4 <- allresults_m4 %>%
  as_tibble() %>%
  mutate(
    exposure = factor(exposure, 
                      levels = c("Ever smoked", "Ever cannabis",
                                 "Weekly smoking", "Weekly cannabis", 
                                 "Co-use"),
                      ordered = TRUE)
  )

# Function to run meta-analysis
run_meta_detailed <- function(data, exp) {
  subset_data <- filter(data, exposure == exp)
  
  meta <- rma(
    yi = log(beta),
    sei = (log(upper) - log(lower))/(2*1.96),
    data = subset_data,
    method = "REML"
  )
  
  results <- data.frame(
    exposure = exp,
    study = "Meta-analysis",
    beta = exp(meta$b),
    lower = exp(meta$ci.lb),
    upper = exp(meta$ci.ub),
    p_value = meta$pval,
    I2 = meta$I2,
    tau2 = meta$tau2,
    Q_stat = meta$QE,
    Q_p = meta$QEp,
    k = meta$k
  )
  
  return(results)
}

# Run meta-analyses and combine results
m4_results <- lapply(levels(df_m4$exposure), function(exp) {
  run_meta_detailed(df_m4, exp)
})
m1 <- bind_rows(m1_results) %>%
  mutate(exposure = factor(exposure,
                           levels = c("Ever smoked", "Ever cannabis",
                                      "Weekly smoking", "Weekly cannabis", 
                                      "Co-use"),
                           ordered = TRUE))
# Update x_limit
x_limit <- 3.0

# In the plot data preparation 
m4_combined <- bind_rows(
  df_m4,
  select(m4, exposure, study, beta, lower, upper)
) %>%
  mutate(
    study = factor(study, levels = c("Meta-analysis", "ALSPAC", "LSAC", "Add Health")),
    exposure = factor(exposure,
                      levels = c("Ever smoked", "Ever cannabis",
                                 "Weekly smoking", "Weekly cannabis", 
                                 "Co-use"),
                      ordered = TRUE),
    exceeds_limit = upper > x_limit,
    upper_plot = pmin(upper, x_limit)
  )

# Update the plot code
p4 <- ggplot(m4_combined, aes(y = fct_rev(exposure), x = beta, color = study)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = lower, xmax = pmin(upper, 3.0), shape = study),
                  position = position_dodge(width = 0.5)) +
  geom_segment(data = subset(m4_combined, upper > 3.0),
               aes(x = 2.9, xend = 3.0, 
                   y = as.numeric(fct_rev(exposure)) + 
                     (as.numeric(study) - 2.5) * 0.5),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               linewidth = 0.5) +
  scale_shape_manual(values = c(18, 16, 16, 16)) +
  scale_color_manual(values = c("black", "#d95f02", "#1b9e77", "#7570b3")) +
  scale_x_continuous(breaks = seq(0.5, 3.0, by = 0.5), 
                     limits = c(0.5, 3.0)) +
  coord_cartesian(xlim = c(0.5, 3.0), clip = "off") +
  labs(x = "Odds ratio (95% CI)", y = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_blank(),
    legend.key.size = unit(1, "cm"),
    # Add this line to prevent arrows in legend:
    legend.key.height = unit(0.5, "cm")
  )

# Print plot
print(p4)

# Save detailed results
write.xlsx(m1, "meta_analysis_results_m4.xlsx")

# Print summary table of results
print("Summary of Meta-Analysis Results:")
print(m4 %>%
        select(exposure, beta, lower, upper, I2, p_value))

ggsave("forest_plotp4.png", p4, width = 10, height = 8, dpi = 800)
# For PDF:
ggsave("forest_plotp4.pdf", p4, width = 10, height = 8)