# CAS Summer Internship - Pricing Competition
# Step 3: Pricing the Policies and Creating the Submission File

#--------------------------------------------------------------------------
# PRE-REQUISITE: Ensure Steps 1 and 2 have been run
#--------------------------------------------------------------------------
# This script assumes that the 'data', 'model_freq', and 'model_sev'
# objects are all loaded in your R environment from the previous steps.

#--------------------------------------------------------------------------
# 1. LOAD AND PREPARE THE PRICING DATA
#--------------------------------------------------------------------------
# This is the new dataset of 10,000 policies we need to price.
# We MUST perform the exact same transformations as we did in Step 1.

print("Loading and preparing the new pricing data...")

# Load the pricing dataset from the correct Excel sheet.
# This is the key change to fix the issue.
library(readxl)
pricing_data <- read_excel("2025 CAS East Asia Summer Program Dataset.xlsx", sheet = "Pricing Data")


# Feature Engineering (must match Step 1 exactly)
pricing_data$Power <- as.factor(pricing_data$Power)
pricing_data$Brand <- as.factor(pricing_data$Brand)
pricing_data$Gas <- as.factor(pricing_data$Gas)
pricing_data$Region <- as.factor(pricing_data$Region)
pricing_data$LogDensity <- log(pricing_data$Density)

pricing_data$DriverAgeBand <- cut(pricing_data$DriverAge,
                                  breaks = c(17, 21, 26, 31, 41, 51, 71, Inf),
                                  labels = c("18-21", "22-25", "26-30", "31-40", "41-50", "51-70", "71+"),
                                  right = TRUE,
                                  include.lowest = TRUE)

pricing_data$CarAgeBand <- cut(pricing_data$CarAge,
                               breaks = c(-Inf, 0, 1, 10, Inf),
                               labels = c("New [0]", "Moderate (0-1]", "Established (1-10]", "Old (10+)"),
                               right = TRUE,
                               include.lowest = TRUE)

print("Pricing data prepared successfully.")

#--------------------------------------------------------------------------
# 2. PREDICT FREQUENCY AND SEVERITY
#--------------------------------------------------------------------------
# We use our two models to predict the two components of risk for each policy.

print("Predicting frequency and severity for each policy...")

# Predict the expected number of claims (frequency)
# The 'type = "response"' gives us the prediction on the original scale (e.g., 0.05 claims)
predicted_freq <- predict(model_freq, newdata = pricing_data, type = "response")

# Predict the expected cost per claim (severity)
predicted_sev <- predict(model_sev, newdata = pricing_data, type = "response")

print("Predictions complete.")

#--------------------------------------------------------------------------
# 3. CALCULATE PURE PREMIUM AND FINAL PREMIUM
#--------------------------------------------------------------------------
# Pure Premium is the amount needed to cover losses.
# Final Premium includes loadings for expenses and profit.

print("Calculating pure and final premiums...")

# Calculate Pure Premium = Predicted Frequency * Predicted Severity
pricing_data$PurePremium <- predicted_freq * predicted_sev

# Define Expense and Profit Loadings
# These are key strategic assumptions.
# Based on the research doc, we'll use:
expense_ratio <- 0.25  # 25% for all company expenses
profit_margin <- 0.10  # 10% target profit

# Calculate Final Premium
# Formula: Pure Premium / (1 - Expense Ratio - Profit Margin)
pricing_data$Premium <- pricing_data$PurePremium / (1 - expense_ratio - profit_margin)

# Let's look at the first few calculated premiums
head(pricing_data %>% select(PolicyID, PurePremium, Premium))


#--------------------------------------------------------------------------
# 4. CREATE AND SAVE THE SUBMISSION FILE
#--------------------------------------------------------------------------
# The final step is to create a clean CSV file in the required format.

print("Creating the final submission file...")

# Select only the required columns: PolicyID and the final Premium
submission_df <- pricing_data %>% select(PolicyID, Premium)

# Save the dataframe to a CSV file.
# This file will be created in your "D:/CAS-Summer-Internship" folder.
write.csv(submission_df, "My_Pricing_Submission2.csv", row.names = FALSE)

print("SUCCESS! Your submission file 'My_Pricing_Submission.csv' has been created.")
print("You have successfully priced all policies. Well done!")
