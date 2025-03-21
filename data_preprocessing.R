#!/usr/bin/env Rscript

# Load required libraries
required_packages <- c(
  "tidyverse", "haven", "ggplot2", "mice", "VIM", 
  "labelled", "forcats", "skimr", "DataExplorer",
  "naniar", "visdat", "sjmisc", "sjlabelled"  # Added sjmisc and sjlabelled for better variable handling
)

# Install and load packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)

# Set working directory and load data
setwd("C:/Users/uniab/FREELANCING/2025-DISSERTATION/QLFS-HARKEERAT/Coding")
qlfs_2015 <- read_sav("C:/Users/uniab/FREELANCING/2025-DISSERTATION/QLFS-HARKEERAT/DATASET&INFO/7912spss_7d945e3edc896a3b1bf44b3ca6a5290f/UKDA-7912-spss/spss/spss19/lfsp_jm15_teaching_final2.sav")

# Step 1: Remove CASENEW variable and select required variables
qlfs_2015 <- qlfs_2015 %>%
  select(-CASENEW)

# Step 2: Detailed Missing Value Analysis
print("Initial Missing Value Analysis:")
missing_summary <- data.frame(
  Variable = names(qlfs_2015),
  Missing_Count = colSums(is.na(qlfs_2015)),
  Missing_Percentage = round(colSums(is.na(qlfs_2015)) / nrow(qlfs_2015) * 100, 2)
)

# Create visualizations of missing values
pdf("missing_values_analysis.pdf")
# Plot 1: Missing values pattern
vis_miss(qlfs_2015) + 
  ggtitle("Missing Values Pattern in QLFS 2015")

# Plot 2: Missing values correlation
gg_miss_upset(qlfs_2015)

# Plot 3: Missing values by variable
ggplot(missing_summary, aes(x = reorder(Variable, Missing_Percentage), y = Missing_Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Percentage of Missing Values by Variable",
       x = "Variables",
       y = "Missing Percentage")
dev.off()

# Step 3: Replace SPSS missing values (-8, -9) with NA and create proper labels
# Define variable types based on data dictionary
numeric_vars <- c("PWT14", "TOTHRS", "PWT14R")
categorical_vars <- c("SEX", "MARSTA3R", "HIQUL15D", "ETHUK7R", "ILODEFR", 
                     "STAT3R", "FTPTWK", "NSECMJ3R", "GOVTOF2")
ordinal_vars <- c("AGEEULR")

# Function to get mode for categorical variables
get_mode <- function(x) {
  if (all(is.na(x))) return(NA)
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x[!is.na(x)], ux)))]
}

# Replace special missing values and convert to appropriate types
qlfs_2015_clean <- qlfs_2015 %>%
  # Replace special missing values
  mutate(across(everything(), ~if_else(. %in% c(-8, -9), NA_real_, as.numeric(.)))) %>%
  # Convert variables to appropriate types with labels
  mutate(
    # Weights
    PWT14 = as.numeric(PWT14),
    PWT14R = as.numeric(PWT14R),
    
    # Demographic variables
    SEX = factor(SEX, levels = c(1, 2), 
                labels = c("Male", "Female")),
    
    MARSTA3R = factor(MARSTA3R, levels = 1:3, 
                     labels = c("Single, never married", 
                              "Married/Cohabiting/Civil Partner",
                              "Divorced/Widowed/Previously in Civil Partnership")),
    
    AGEEULR = factor(AGEEULR, levels = 1:12,
                     labels = c("15-19", "20-24", "25-29", "30-34", "35-39",
                              "40-44", "45-49", "50-54", "55-59", "60-64",
                              "65-69", "70 and over")),
    
    # Education and ethnicity
    HIQUL15D = factor(HIQUL15D, levels = 1:7,
                      labels = c("Degree or equivalent", "Higher education",
                               "GCE, A-level or equivalent", "GCSE grades A*-C or equivalent",
                               "Other qualifications", "No qualification", "Don't know")),
    
    ETHUK7R = factor(ETHUK7R, levels = 1:7,
                     labels = c("White", "Mixed/Multiple ethnic groups", "Indian",
                              "Pakistani or Bangladeshi", 
                              "Chinese or any other Asian background",
                              "Black/African/Caribbean/Black British",
                              "Other ethnic group")),
    
    # Employment variables
    ILODEFR = factor(ILODEFR, levels = 1:4,
                     labels = c("In employment", "ILO unemployed", 
                              "Inactive", "Under 16")),
    
    STAT3R = factor(STAT3R, levels = 1:3,
                    labels = c("Employee", "Self-employed",
                             "Government scheme or unpaid family worker")),
    
    FTPTWK = factor(FTPTWK, levels = 1:2,
                    labels = c("Full-time", "Part-time")),
    
    TOTHRS = as.numeric(if_else(TOTHRS == 97, NA_real_, as.numeric(TOTHRS))), # Handle 97+ hours special case
    
    NSECMJ3R = factor(NSECMJ3R, levels = 1:4,
                      labels = c("Higher managerial, administrative and professional occupations",
                               "Intermediate occupations and small employers",
                               "Routine and manual occupations",
                               "Never worked, unemployed, and nec")),
    
    # Geographic variable
    GOVTOF2 = factor(GOVTOF2, levels = 1:13,
                     labels = c("North East", "North West (inc Merseyside)",
                              "Yorkshire and Humberside", "East Midlands",
                              "West Midlands", "Eastern", "London",
                              "South East", "South West", "Wales",
                              "Scotland", "Northern Ireland", "Does not apply"))
  )

# Step 4: Create additional derived variables for enhanced analysis
qlfs_2015_clean <- qlfs_2015_clean %>%
  mutate(
    # Create simplified education variable (3 categories)
    education_level = case_when(
      HIQUL15D %in% c("Degree or equivalent", "Higher education") ~ "Higher education",
      HIQUL15D %in% c("GCE, A-level or equivalent", "GCSE grades A*-C or equivalent") ~ "Secondary education",
      HIQUL15D %in% c("Other qualifications", "No qualification", "Don't know") ~ "Below secondary/Other",
      TRUE ~ NA_character_
    ) %>% factor(),
    
    # Create age groups (working age vs. non-working age)
    working_age = case_when(
      AGEEULR %in% c("15-19", "20-24", "25-29", "30-34", "35-39", 
                    "40-44", "45-49", "50-54", "55-59", "60-64") ~ "Working age",
      TRUE ~ "Non-working age"
    ) %>% factor(),
    
    # Create regional groupings
    region_group = case_when(
      GOVTOF2 %in% c("North East", "North West (inc Merseyside)", 
                    "Yorkshire and Humberside") ~ "Northern England",
      GOVTOF2 %in% c("East Midlands", "West Midlands") ~ "Midlands",
      GOVTOF2 %in% c("Eastern", "London", "South East", "South West") ~ "Southern England",
      GOVTOF2 %in% c("Wales", "Scotland", "Northern Ireland") ~ "Wales/Scotland/NI",
      TRUE ~ NA_character_
    ) %>% factor(),
    
    # Create binary employment status variables for different analyses
    is_employee = ifelse(STAT3R == "Employee", 1, 0),
    is_self_employed = ifelse(STAT3R == "Self-employed", 1, 0),
    is_govt_scheme = ifelse(STAT3R == "Government scheme or unpaid family worker", 1, 0),
    
    # Create interaction variables for key relationships
    edu_region = interaction(education_level, region_group),
    gender_edu = interaction(SEX, education_level)
  )

# Step 5: Imputation Strategy
# Create imputed dataset
qlfs_2015_imputed <- qlfs_2015_clean %>%
  mutate(
    # Numeric variables: median imputation
    across(all_of(numeric_vars), ~if_else(is.na(.), median(., na.rm = TRUE), .)),
    
    # Categorical and ordinal variables: mode imputation
    across(all_of(c(categorical_vars, ordinal_vars)), function(x) {
      if_else(is.na(x), 
              as.character(get_mode(x)), 
              as.character(x)) %>%
        factor(levels = levels(x), labels = levels(x))
    })
  )

# Ensure derived variables are also imputed
derived_vars <- c("education_level", "working_age", "region_group", 
                 "is_employee", "is_self_employed", "is_govt_scheme",
                 "edu_region", "gender_edu")

qlfs_2015_imputed <- qlfs_2015_imputed %>%
  mutate(
    across(all_of(derived_vars), function(x) {
      if(is.factor(x)) {
        if_else(is.na(x), 
                as.character(get_mode(x)), 
                as.character(x)) %>%
          factor(levels = levels(x), labels = levels(x))
      } else if(is.numeric(x)) {
        if_else(is.na(x), median(x, na.rm = TRUE), x)
      } else {
        x
      }
    })
  )

# Verify the imputation worked
print("Checking variable types after imputation:")
print(sapply(qlfs_2015_imputed, class))

# Step 6: Verification and Quality Checks
# Check missing values after imputation
print("\nMissing values after imputation:")
print(colSums(is.na(qlfs_2015_imputed)))

# Generate summary statistics
summary_stats <- skim(qlfs_2015_imputed)
capture.output(summary_stats, file = "data_summary.txt")

# Create comparison plots
pdf("imputation_comparison_plots.pdf")
# For numeric variables
for(var in numeric_vars) {
  if(var %in% names(qlfs_2015_clean)) {
    p <- ggplot() +
      geom_density(data = qlfs_2015_clean, aes(x = .data[[var]], fill = "Original"), alpha = 0.5) +
      geom_density(data = qlfs_2015_imputed, aes(x = .data[[var]], fill = "Imputed"), alpha = 0.5) +
      scale_fill_manual(values = c("Original" = "blue", "Imputed" = "red")) +
      theme_minimal() +
      labs(title = paste("Distribution Comparison for", var),
           x = var,
           y = "Density")
    print(p)
  }
}

# For categorical variables
for(var in c(categorical_vars, ordinal_vars)) {
  if(var %in% names(qlfs_2015_clean)) {
    p <- ggplot() +
      geom_bar(data = qlfs_2015_clean, aes(x = .data[[var]], fill = "Original"), alpha = 0.5, position = "dodge") +
      geom_bar(data = qlfs_2015_imputed, aes(x = .data[[var]], fill = "Imputed"), alpha = 0.5, position = "dodge") +
      scale_fill_manual(values = c("Original" = "blue", "Imputed" = "red")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste("Distribution Comparison for", var),
           x = var,
           y = "Count")
    print(p)
  }
}

# Add plots for derived variables
for(var in derived_vars) {
  if(is.factor(qlfs_2015_imputed[[var]])) {
    p <- ggplot(qlfs_2015_imputed, aes(x = .data[[var]])) +
      geom_bar(fill = "darkgreen") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste("Distribution of Derived Variable:", var),
           x = var,
           y = "Count")
    print(p)
  } else if(is.numeric(qlfs_2015_imputed[[var]])) {
    p <- ggplot(qlfs_2015_imputed, aes(x = .data[[var]])) +
      geom_histogram(fill = "darkgreen", bins = 30) +
      theme_minimal() +
      labs(title = paste("Distribution of Derived Variable:", var),
           x = var,
           y = "Count")
    print(p)
  }
}
dev.off()

# Save the processed datasets
saveRDS(qlfs_2015_clean, "qlfs_2015_clean.rds")  # Pre-imputation
saveRDS(qlfs_2015_imputed, "qlfs_2015_imputed.rds")  # Post-imputation

# Create detailed summary report
sink("imputation_report.txt")
cat("UK Labour Force Survey 2015 Data Processing Report\n")
cat("================================================\n\n")

cat("1. Initial Data Summary:\n")
cat("------------------------\n")
print(missing_summary)

cat("\n2. Variables Processed:\n")
cat("----------------------\n")
cat("Numeric Variables:", paste(numeric_vars, collapse = ", "), "\n")
cat("Categorical Variables:", paste(categorical_vars, collapse = ", "), "\n")
cat("Ordinal Variables:", paste(ordinal_vars, collapse = ", "), "\n")
cat("Derived Variables:", paste(derived_vars, collapse = ", "), "\n")

cat("\n3. Imputation Results:\n")
cat("--------------------\n")
comparison <- data.frame(
  Variable = names(qlfs_2015_clean),
  Original_Missing = colSums(is.na(qlfs_2015_clean)),
  Imputed_Missing = colSums(is.na(qlfs_2015_imputed))
)
print(comparison)

cat("\n4. Variable Summaries After Imputation:\n")
cat("-----------------------------------\n")
print(summary(qlfs_2015_imputed))

sink()

# Print confirmation
print("\nData preprocessing completed!")
print("Files saved:")
print("1. qlfs_2015_clean.rds - Clean dataset before imputation")
print("2. qlfs_2015_imputed.rds - Final imputed dataset")
print("3. data_summary.txt - Summary statistics")
print("4. missing_values_analysis.pdf - Missing values visualization")
print("5. imputation_comparison_plots.pdf - Distribution comparisons")
print("6. imputation_report.txt - Detailed processing report") 