#!/usr/bin/env Rscript

# Load required libraries
required_packages <- c(
  "tidyverse", "ggplot2", "car", "nnet", "lmtest", 
  "sandwich", "margins", "stargazer", "pROC", "gridExtra",
  "ggcorrplot", "vcd", "effectsize", "sjPlot", "sjlabelled",
  "survey", "weights", "corrplot", "DescTools", "rms"
)

# Install and load packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)

# Set working directory and load imputed data
setwd("C:/Users/uniab/FREELANCING/2025-DISSERTATION/QLFS-HARKEERAT/Coding")
qlfs_2015_imputed <- readRDS("qlfs_2015_imputed.rds")

# Create output directory for plots
dir.create("analysis_output", showWarnings = FALSE)

# 1. DESCRIPTIVE STATISTICS
# ------------------------

# 1.1 Descriptive Statistics for Categorical Variables
cat_vars <- c("SEX", "MARSTA3R", "HIQUL15D", "ETHUK7R", "ILODEFR", 
              "STAT3R", "FTPTWK", "NSECMJ3R", "GOVTOF2", "AGEEULR")

# Create frequency tables for categorical variables
cat_stats <- lapply(cat_vars, function(var) {
  freq_table <- table(qlfs_2015_imputed[[var]])
  prop_table <- prop.table(freq_table) * 100
  data.frame(
    Variable = var,
    Category = names(freq_table),
    Frequency = as.vector(freq_table),
    Percentage = round(as.vector(prop_table), 2)
  )
})

# Save categorical statistics
write.csv(do.call(rbind, cat_stats), "analysis_output/categorical_statistics.csv", row.names = FALSE)

# 1.2 Descriptive Statistics for Numerical Variables
num_vars <- c("TOTHRS", "PWT14", "PWT14R")

# Calculate summary statistics for numerical variables
num_stats <- lapply(num_vars, function(var) {
  data.frame(
    Variable = var,
    Mean = mean(qlfs_2015_imputed[[var]], na.rm = TRUE),
    Median = median(qlfs_2015_imputed[[var]], na.rm = TRUE),
    SD = sd(qlfs_2015_imputed[[var]], na.rm = TRUE),
    Min = min(qlfs_2015_imputed[[var]], na.rm = TRUE),
    Max = max(qlfs_2015_imputed[[var]], na.rm = TRUE)
  )
})

# Save numerical statistics
write.csv(do.call(rbind, num_stats), "analysis_output/numerical_statistics.csv", row.names = FALSE)

# 2. CROSS-TABULATION ANALYSIS
# ---------------------------

# Create cross-tabulations for key variables
key_cross_tabs <- list(
  "Employment Status by Region" = table(qlfs_2015_imputed$STAT3R, qlfs_2015_imputed$GOVTOF2),
  "Employment Status by Education" = table(qlfs_2015_imputed$STAT3R, qlfs_2015_imputed$HIQUL15D),
  "Employment Status by Socio-economic Class" = table(qlfs_2015_imputed$STAT3R, qlfs_2015_imputed$NSECMJ3R)
)

# Save cross-tabulations
sink("analysis_output/cross_tabulations.txt")
for (name in names(key_cross_tabs)) {
  cat("\n", name, "\n")
  print(key_cross_tabs[[name]])
  cat("\n Chi-square test:\n")
  print(chisq.test(key_cross_tabs[[name]]))
  cat("\n")
}
sink()

# 3. MEAN HOURS WORKED ANALYSIS
# ---------------------------

# Calculate mean hours worked by various categories
hours_by_region <- aggregate(TOTHRS ~ GOVTOF2, data = qlfs_2015_imputed, FUN = mean)
hours_by_status <- aggregate(TOTHRS ~ STAT3R, data = qlfs_2015_imputed, FUN = mean)
hours_by_nsec <- aggregate(TOTHRS ~ NSECMJ3R, data = qlfs_2015_imputed, FUN = mean)

# Save mean hours analysis
write.csv(hours_by_region, "analysis_output/hours_by_region.csv", row.names = FALSE)
write.csv(hours_by_status, "analysis_output/hours_by_status.csv", row.names = FALSE)
write.csv(hours_by_nsec, "analysis_output/hours_by_nsec.csv", row.names = FALSE)

# 4. CORRELATION ANALYSIS
# ----------------------

# Create correlation matrix for numeric variables
cor_matrix <- cor(qlfs_2015_imputed[, num_vars], use = "complete.obs")

# Save correlation matrix
write.csv(cor_matrix, "analysis_output/correlation_matrix.csv")

# 5. VISUALIZATION
# --------------

# 5.1 Univariate Visualizations
pdf("analysis_output/univariate_plots.pdf")

# Function for categorical variable plots
plot_categorical <- function(data, var_name) {
  ggplot(data, aes(x = .data[[var_name]])) +
    geom_bar(fill = "steelblue") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("Distribution of", var_name),
         x = var_name,
         y = "Count")
}

# Function for numerical variable plots
plot_numerical <- function(data, var_name) {
  ggplot(data, aes(x = .data[[var_name]])) +
    geom_histogram(fill = "steelblue", bins = 30) +
    geom_density(color = "red") +
    theme_minimal() +
    labs(title = paste("Distribution of", var_name),
         x = var_name,
         y = "Count")
}

# Create plots for all variables
for (var in cat_vars) {
  print(plot_categorical(qlfs_2015_imputed, var))
}

for (var in num_vars) {
  print(plot_numerical(qlfs_2015_imputed, var))
}
dev.off()

# 5.2 Bivariate Visualizations
pdf("analysis_output/bivariate_plots.pdf")

# Employment Status by Region
ggplot(qlfs_2015_imputed, aes(x = GOVTOF2, fill = STAT3R)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Employment Status Distribution by Region",
       x = "Region",
       y = "Proportion")

# Hours Worked by Employment Status
ggplot(qlfs_2015_imputed, aes(x = STAT3R, y = TOTHRS)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Hours Worked by Employment Status",
       x = "Employment Status",
       y = "Total Hours Worked")

# Hours Worked by Education Level
ggplot(qlfs_2015_imputed, aes(x = HIQUL15D, y = TOTHRS)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Hours Worked by Education Level",
       x = "Education Level",
       y = "Total Hours Worked")
dev.off()

# 6. STATISTICAL TESTS
# -------------------

# 6.1 Chi-square Tests
sink("analysis_output/statistical_tests.txt")
cat("Chi-square Tests\n")
cat("===============\n\n")

# Chi-square tests for key relationships
key_relationships <- list(
  "Employment Status vs Region" = chisq.test(table(qlfs_2015_imputed$STAT3R, qlfs_2015_imputed$GOVTOF2)),
  "Employment Status vs Education" = chisq.test(table(qlfs_2015_imputed$STAT3R, qlfs_2015_imputed$HIQUL15D)),
  "Employment Status vs Socio-economic Class" = chisq.test(table(qlfs_2015_imputed$STAT3R, qlfs_2015_imputed$NSECMJ3R))
)

for (name in names(key_relationships)) {
  cat("\n", name, "\n")
  print(key_relationships[[name]])
  cat("\n")
}

# 6.2 ANOVA Tests
cat("\nANOVA Tests\n")
cat("===========\n\n")

# ANOVA for hours worked by various categories
anova_tests <- list(
  "Hours by Employment Status" = aov(TOTHRS ~ STAT3R, data = qlfs_2015_imputed),
  "Hours by Education" = aov(TOTHRS ~ HIQUL15D, data = qlfs_2015_imputed),
  "Hours by Region" = aov(TOTHRS ~ GOVTOF2, data = qlfs_2015_imputed)
)

for (name in names(anova_tests)) {
  cat("\n", name, "\n")
  print(summary(anova_tests[[name]]))
  cat("\n")
}

# 6.3 Tukey's HSD Tests
cat("\nTukey's HSD Tests\n")
cat("================\n\n")

for (name in names(anova_tests)) {
  cat("\n", name, "\n")
  print(TukeyHSD(anova_tests[[name]]))
  cat("\n")
}
sink()

# 7. MODELLING
# -----------

# 7.1 Primary Model - Multinomial Logistic Regression
# Create binary employment status (employed vs not employed)
qlfs_2015_imputed$employed_binary <- ifelse(qlfs_2015_imputed$STAT3R == "Employee", 1, 0)

# Fit multinomial logistic regression
multinom_model <- multinom(STAT3R ~ HIQUL15D + ILODEFR + NSECMJ3R + GOVTOF2 + 
                          SEX + AGEEULR + MARSTA3R + FTPTWK + ETHUK7R, 
                          data = qlfs_2015_imputed)

# Save model results
sink("analysis_output/multinomial_model.txt")
cat("Multinomial Logistic Regression Results\n")
cat("=====================================\n\n")
print(summary(multinom_model))
sink()

# 7.2 Secondary Model - Binary Logistic Regression
binary_model <- glm(employed_binary ~ HIQUL15D + ILODEFR + NSECMJ3R + GOVTOF2 + 
                    SEX + AGEEULR + MARSTA3R + FTPTWK + ETHUK7R, 
                    family = binomial(link = "logit"), 
                    data = qlfs_2015_imputed)

# Save binary model results
sink("analysis_output/binary_model.txt")
cat("Binary Logistic Regression Results\n")
cat("================================\n\n")
print(summary(binary_model))
sink()

# 7.3 Regional Analysis Models
regional_models <- lapply(unique(qlfs_2015_imputed$GOVTOF2), function(region) {
  region_data <- subset(qlfs_2015_imputed, GOVTOF2 == region)
  glm(employed_binary ~ HIQUL15D + ILODEFR + NSECMJ3R + 
      SEX + AGEEULR + MARSTA3R + FTPTWK + ETHUK7R, 
      family = binomial(link = "logit"), 
      data = region_data)
})

# Save regional model results
sink("analysis_output/regional_models.txt")
cat("Regional Logistic Regression Results\n")
cat("==================================\n\n")
for (i in seq_along(regional_models)) {
  cat("\nRegion:", unique(qlfs_2015_imputed$GOVTOF2)[i], "\n")
  print(summary(regional_models[[i]]))
  cat("\n")
}
sink()

# 8. MODEL DIAGNOSTICS
# -------------------

# 8.1 Multicollinearity Check
vif_results <- vif(binary_model)
write.csv(vif_results, "analysis_output/vif_results.csv")

# 8.2 Heteroskedasticity Test
hetero_test <- bptest(binary_model)
sink("analysis_output/model_diagnostics.txt")
cat("Heteroskedasticity Test\n")
cat("=====================\n\n")
print(hetero_test)
sink()

# 8.3 Robust Standard Errors
robust_se <- sqrt(diag(vcovHC(binary_model, type = "HC1")))
write.csv(robust_se, "analysis_output/robust_standard_errors.csv")

# 9. FEATURE IMPORTANCE
# -------------------

# Calculate feature importance using odds ratios
odds_ratios <- exp(coef(binary_model))
odds_ratios_df <- data.frame(
  Variable = names(odds_ratios),
  Odds_Ratio = odds_ratios
)
write.csv(odds_ratios_df, "analysis_output/odds_ratios.csv")

# Create feature importance plot
pdf("analysis_output/feature_importance.pdf")
ggplot(odds_ratios_df, aes(x = reorder(Variable, Odds_Ratio), y = Odds_Ratio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Feature Importance (Odds Ratios)",
       x = "Variable",
       y = "Odds Ratio")
dev.off()

# 10. MODEL COMPARISON
# ------------------

# Calculate AIC and BIC for model comparison
model_comparison <- data.frame(
  Model = c("Binary Logistic", "Multinomial Logistic"),
  AIC = c(AIC(binary_model), AIC(multinom_model)),
  BIC = c(BIC(binary_model), BIC(multinom_model))
)
write.csv(model_comparison, "analysis_output/model_comparison.csv")

# 11. EMPLOYMENT RATE BY QUALIFICATION
# ---------------------------------

# Calculate employment rates
employment_rates <- qlfs_2015_imputed %>%
  group_by(HIQUL15D) %>%
  summarise(
    total = n(),
    employed = sum(employed_binary),
    employment_rate = (employed / total) * 100
  )

# Create employment rate plot
pdf("analysis_output/employment_rate_by_qualification.pdf")
ggplot(employment_rates, aes(x = HIQUL15D, y = employment_rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Employment Rate by Highest Qualification",
       x = "Highest Qualification",
       y = "Employment Rate (%)")
dev.off()

# Save employment rates
write.csv(employment_rates, "analysis_output/employment_rates_by_qualification.csv", row.names = FALSE)

# Print completion message
print("\nAnalysis completed! Check the 'analysis_output' directory for results.")
