# CAS Summer Internship - Pricing Competition
# Step 1: Environment Setup and Exploratory Data Analysis (EDA)

#--------------------------------------------------------------------------
# 1. SETUP: Install and Load Libraries
#--------------------------------------------------------------------------
# This section ensures you have all the necessary tools for our analysis.

# List of required packages
packages <- c("tidyverse", "ggplot2", "dplyr", "readxl")

# Install packages if they are not already installed
install.packages(setdiff(packages, rownames(installed.packages())))

# Load the packages into our R session
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)

#--------------------------------------------------------------------------
# 2. LOAD DATA: Read the historical policy data
#--------------------------------------------------------------------------
# We'll set the working directory to where you've cloned the repo
# and then load the main dataset from the Excel file.

# Set your working directory
# Make sure the path "D:/CAS-Summer-Internship" matches exactly where you
# have the files on your computer.
setwd("D:/CAS-Summer-Internship")

# Load the student dataset from the specified sheet in the Excel file.
data <- read_excel("2025 CAS East Asia Summer Program Dataset.xlsx", sheet = "Dataset for Student")

# Let's take a quick look at the first few rows to see what we're working with
head(data)
# Get a summary of the data structure
str(data)


#--------------------------------------------------------------------------
# 3. DATA PREPARATION: Clean and transform variables for modeling
#--------------------------------------------------------------------------
# Raw data is rarely ready for modeling. We need to convert some variables
# into a more useful format.

# Convert categorical variables to factors. This tells R to treat them
# as distinct groups rather than numbers or text.
data$Power <- as.factor(data$Power)
data$Brand <- as.factor(data$Brand)
data$Gas <- as.factor(data$Gas)
data$Region <- as.factor(data$Region)

# Create a binary flag for whether a claim occurred. This will be our
# target for the frequency model.
data$has_claim <- as.numeric(data$ClaimNb > 0)


#--------------------------------------------------------------------------
# 4. FEATURE ENGINEERING: Create new variables from existing ones
#--------------------------------------------------------------------------
# We can often improve model accuracy by creating more predictive features.
# Here, we'll group continuous variables like age into bins.

# Create Driver Age bands. This helps the model see non-linear trends,
# e.g., both young and very old drivers being higher risk.
data$DriverAgeBand <- cut(data$DriverAge,
                          breaks = c(17, 21, 26, 31, 41, 51, 71, Inf),
                          labels = c("18-21", "22-25", "26-30", "31-40", "41-50", "51-70", "71+"),
                          right = TRUE,
                          include.lowest = TRUE)

# Create Car Age bands.
data$CarAgeBand <- cut(data$CarAge,
                       breaks = c(-Inf, 0, 1, 10, Inf), # Use -Inf to include 0
                       labels = c("New [0]", "Moderate (0-1]", "Established (1-10]", "Old (10+)"),
                       right = TRUE,
                       include.lowest = TRUE)


#--------------------------------------------------------------------------
# 5. EXPLORATORY ANALYSIS (EDA): Visualize the data
#--------------------------------------------------------------------------
# Now we create plots to understand the relationships between our variables
# and the risk of claims. This is essential for building a good model.

# --- Distribution of Key Numerical Variables ---

# Distribution of Driver Age
ggplot(data, aes(x = DriverAge)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  ggtitle("Distribution of Driver Age") +
  theme_minimal()

# Distribution of Car Age
ggplot(data, aes(x = CarAge)) +
  geom_density(fill = "salmon", alpha = 0.7) +
  ggtitle("Distribution of Car Age") +
  theme_minimal()

# Distribution of Density - Notice it's highly skewed
ggplot(data, aes(x = Density)) +
  geom_density(fill = "lightgreen", alpha = 0.7) +
  ggtitle("Distribution of Population Density") +
  theme_minimal()

# The skew in Density can be a problem for models. A log transform helps normalize it.
data$LogDensity <- log(data$Density)
ggplot(data, aes(x = LogDensity)) +
  geom_density(fill = "gold", alpha = 0.7) +
  ggtitle("Distribution of Log-Transformed Density") +
  theme_minimal()


# --- Frequency Analysis: How often do claims happen for different groups? ---

# Claim Frequency by Driver Age Band
data %>%
  group_by(DriverAgeBand) %>%
  summarise(ClaimFrequency = mean(has_claim)) %>%
  ggplot(aes(x = DriverAgeBand, y = ClaimFrequency, fill = DriverAgeBand)) +
  geom_bar(stat = "identity") +
  ggtitle("Average Claim Frequency by Driver Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Claim Frequency by Car Age Band
data %>%
  group_by(CarAgeBand) %>%
  summarise(ClaimFrequency = mean(has_claim)) %>%
  ggplot(aes(x = CarAgeBand, y = ClaimFrequency, fill = CarAgeBand)) +
  geom_bar(stat = "identity") +
  ggtitle("Average Claim Frequency by Car Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Claim Frequency by Brand
data %>%
  group_by(Brand) %>%
  summarise(ClaimFrequency = mean(has_claim)) %>%
  ggplot(aes(x = reorder(Brand, -ClaimFrequency), y = ClaimFrequency, fill = Brand)) +
  geom_bar(stat = "identity") +
  ggtitle("Average Claim Frequency by Brand") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# --- Severity Analysis: How expensive are claims when they happen? ---

# Filter for policies that actually had a claim
claims_only_data <- data %>% filter(has_claim == 1)

# Claim Severity by Driver Age Band
claims_only_data %>%
  group_by(DriverAgeBand) %>%
  summarise(AverageSeverity = mean(ClaimAmount)) %>%
  ggplot(aes(x = DriverAgeBand, y = AverageSeverity, fill = DriverAgeBand)) +
  geom_bar(stat = "identity") +
  ggtitle("Average Claim Severity by Driver Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print("Step 1 (EDA) is complete. Review the plots to understand the data.")
print("Next, we will use these insights to build our predictive model.")

