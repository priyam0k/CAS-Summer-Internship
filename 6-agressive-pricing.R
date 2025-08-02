# CAS Summer Internship - Pricing Competition
# Step 5: Final Round Aggressive Pricing Strategy

#--------------------------------------------------------------------------
# PRE-REQUISITE: Ensure Steps 1, 2, and 3 have been run
#--------------------------------------------------------------------------
# This script assumes that the 'pricing_data' dataframe, which includes
# the 'PurePremium' for each policy, is loaded in your R environment.

print("Starting Step 5: Implementing Aggressive Final Round Pricing...")

#--------------------------------------------------------------------------
# 1. DEFINE RISK SEGMENTS
#--------------------------------------------------------------------------
# We will segment the 10,000 policies into three risk tiers based on their
# calculated PurePremium. This allows us to apply different profit margins.

# Calculate the quantile thresholds to define our risk segments.
# - Low Risk: The 40% of policies with the lowest pure premium.
# - Medium Risk: The next 40% of policies.
# - High Risk: The 20% of policies with the highest pure premium.
risk_thresholds <- quantile(pricing_data$PurePremium, probs = c(0.4, 0.8))

low_risk_cutoff <- risk_thresholds[1]
medium_risk_cutoff <- risk_thresholds[2]

print(paste("Low Risk Cutoff (Pure Premium <=", round(low_risk_cutoff, 2), ")"))
print(paste("Medium Risk Cutoff (Pure Premium <=", round(medium_risk_cutoff, 2), ")"))
print("High Risk is anything above the Medium Risk Cutoff.")


#--------------------------------------------------------------------------
# 2. APPLY DYNAMIC PROFIT MARGINS
#--------------------------------------------------------------------------
# Now we apply our aggressive strategy. We will use a different profit
# margin for each risk segment.

expense_ratio <- 0.25 # This remains constant

pricing_data <- pricing_data %>%
  mutate(
    FinalPremium_Aggressive = case_when(
      # LOW RISK (GROWTH SEGMENT): Be aggressive with a 2% profit margin.
      PurePremium <= low_risk_cutoff ~ PurePremium / (1 - expense_ratio - 0.02),
      
      # MEDIUM RISK (CORE SEGMENT): Use a standard 8% profit margin.
      PurePremium <= medium_risk_cutoff ~ PurePremium / (1 - expense_ratio - 0.08),
      
      # HIGH RISK (PROFITABILITY SEGMENT): Be conservative with a 15% profit margin.
      TRUE ~ PurePremium / (1 - expense_ratio - 0.15)
    )
  )

print("Aggressive final premiums have been calculated.")

#--------------------------------------------------------------------------
# 3. VALIDATION: Check the New Portfolio
#--------------------------------------------------------------------------
# Let's see how our new strategy impacts our overall profitability.
# We expect the overall loss ratio to be higher than 65% because we are
# being more aggressive on a large portion of the policies.

total_premium_agg <- sum(pricing_data$FinalPremium_Aggressive)
total_pure_premium_agg <- sum(pricing_data$PurePremium) # This doesn't change
expected_loss_ratio_agg <- total_pure_premium_agg / total_premium_agg

print("--- Aggressive Strategy Portfolio Check ---")
print(paste("Total Premium (Aggressive): $", round(total_premium_agg, 2)))
print(paste("Expected Portfolio Loss Ratio (Aggressive): ", round(expected_loss_ratio_agg * 100, 2), "%"))

# Compare with our original, more conservative pricing
print("--- Original Strategy Portfolio Check ---")
print(paste("Total Premium (Original): $", round(sum(pricing_data$Premium), 2)))
print(paste("Expected Portfolio Loss Ratio (Original): 65.0 %"))


#--------------------------------------------------------------------------
# 4. CREATE THE FINAL SUBMISSION FILE
#--------------------------------------------------------------------------
# This creates the new submission file with your aggressive prices.

print("Creating the final aggressive submission file...")

# Select only the required columns: PolicyID and the new aggressive Premium
submission_df_aggressive <- pricing_data %>%
  select(PolicyID, Premium = FinalPremium_Aggressive) # Rename column to 'Premium'

# Save the dataframe to a new CSV file.
write.csv(submission_df_aggressive, "My_Final_Round_Submission.csv", row.names = FALSE)

print("SUCCESS! Your final round submission file 'My_Final_Round_Submission.csv' has been created.")
print("This file contains your aggressive prices. Good luck in the final round!")
