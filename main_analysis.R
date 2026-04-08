# =================================================================
# PROJECT: Olive Fruit Fly (Bactrocera oleae) Population Analysis
# SCRIPT: Main Execution & Data Pre-processing
# AUTHOR: Andreas Kalogeropoulos
# =================================================================

# --- 1. Load Required Libraries ---
library(ggplot2)

# --- 2. Load Datasets ---
# Note: Ensure all CSV files are in the /data folder
fly_counts   <- read.csv("data/fly_counts.csv", encoding = 'UTF-8')
sampling     <- read.csv("data/sampling_data.csv", encoding = 'UTF-8')
treatments   <- read.csv("data/insecticide_applications.csv", encoding = 'UTF-8')
weather      <- read.csv("data/weather_data.csv", encoding = 'UTF-8')

# --- 3. Data Cleaning & Pre-processing ---

# A. Format Fly Counts Data (Adults)
# Remove whitespaces from dates and convert to Date objects
fly_counts$CHANGE_DATE <- gsub(" ", "", fly_counts$CHANGE_DATE)
fly_counts$CHANGE_DATE <- as.Date(fly_counts$CHANGE_DATE, format = "%d/%m/%y")

# Handle missing values (NA) by replacing them with 0
fly_counts[is.na(fly_counts)] <- 0

# B. Format Sampling Data (Infestations)
sampling$DEIGMA_DATE <- gsub(" ", "", sampling$DEIGMA_DATE)
sampling$DEIGMA_DATE <- as.Date(sampling$DEIGMA_DATE, format = "%d/%m/%y")

# C. Format Treatment Data (Insecticide Applications)
treatments$SPRAY_STDATE  <- as.Date(treatments$SPRAY_STDATE, format = "%d/%m/%y")
treatments$SPRAY_ENDATE  <- as.Date(treatments$SPRAY_ENDATE, format = "%d/%m/%y")

# D. Format Weather Data
weather$DATE <- as.Date(weather$DATE, format = "%m/%d/%y")
# Clean temperature/humidity columns if they contain non-numeric characters
# (Common when exporting from Greek Excel versions)
weather$MEAN_TEMP <- as.numeric(gsub(",", ".", weather$MEAN_TEMP))

# --- 4. Source Function Scripts ---
source("scripts/correlation_analysis.R")
source("scripts/pest_infestation_treatment_plots.R")
source("scripts/weather_pest_correlation.R")
source("scripts/infestation_type_plots.R")
source("scripts/gender_distribution_plots.R")

# --- 5. Execute Analysis ---

# 5a. Run Correlation Analysis
correlation_results <- run_correlation_analysis(fly_counts, sampling)

# 5b. Generate Impact Plots (Adults, Infestations, Treatments)
generate_pest_impact_plots(fly_counts, sampling, treatments)

# 5c. Generate Weather Correlation Plots
generate_weather_pest_plots(fly_counts, sampling, treatments, weather)

# 5d. Generate Infestation Type Plots (Area charts with ggplot2)
generate_infestation_type_plots(fly_counts, sampling)

# 5e. Generate Gender Distribution and Weather (Wind/Rain) Plots
# Comparing Male/Female populations and environmental impact
generate_gender_weather_plots(fly_counts, sampling, treatments, weather)

print("All analyses and plots have been generated successfully.")
