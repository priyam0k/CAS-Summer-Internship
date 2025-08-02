# CAS Summer Internship - Pricing Competition
# Step 4: Validating Your Prices

#--------------------------------------------------------------------------
# PRE-REQUISITE: Ensure Steps 1, 2, and 3 have been run
#--------------------------------------------------------------------------
# This script assumes that all objects from the previous steps, especially
# 'pricing_data', 'test_data', 'model_freq', and 'model_sev' are in your
# R environment.

print("Starting Step 4: Validation...")

#--------------------------------------------------------------------------
# 1. SANITY CHECK: Review Highest and Lowest Premiums
#--------------------------------------------------------------------------
# Does our pricing make intuitive sense? Let's check the extremes.

print("--- Top 5 Highest Premiums ---")
print(head(pricing_data %>% arrange(desc(Premium)) %>% select(DriverAge, CarAge, Brand, Region, Density, Premium), 5))

print("--- Top 5 Lowest Premiums ---")
print(head(pricing_data %>% arrange(Premium) %>% select(DriverAge, CarAge, Brand, Region, Density, Premium), 5))


#--------------------------------------------------------------------------
# 2. DISTRIBUTION CHECK: Visualize the Premium Distribution
#--------------------------------------------------------------------------
# A histogram helps us understand the spread of our prices and spot outliers.

ggplot(pricing_data, aes(x = Premium)) +
  geom_histogram(bins = 50, fill = "blue", alpha = 0.7) +
  ggtitle("Distribution of Final Premiums") +
  xlab("Premium") +
  ylab("Number of Policies") +
  theme_minimal()


#--------------------------------------------------------------------------
# 3. MODEL LIFT CHART: Does our model accurately rank risk?
#--------------------------------------------------------------------------
# This is a powerful test using the 'test_data' that the model has not seen.

# First, predict the pure premium for the test dataset
test_data$PredictedFreq <- predict(model_freq, newdata = test_data, type = "response")
test_data$PredictedSev <- predict(model_sev, newdata = test_data, type = "response")
test_data$PredictedPurePremium <- test_data$PredictedFreq * test_data$PredictedSev

# Second, create risk buckets (deciles) based on the model's prediction
test_data <- test_data %>%
  mutate(RiskDecile = ntile(-PredictedPurePremium, 10)) # ntile(-x) creates deciles from highest to lowest

# Third, calculate the actual vs. predicted pure premium in each bucket
lift_chart_data <- test_data %>%
  group_by(RiskDecile) %>%
  summarise(
    AvgPredictedPurePremium = mean(PredictedPurePremium),
    ActualPurePremium = mean(ClaimAmount), # In a large dataset, mean(ClaimAmount) is the actual loss cost
    NumberOfPolicies = n()
  )

print("--- Model Lift Chart Data ---")
print(lift_chart_data)

# Finally, plot the results. A good model will show the bars decreasing from left to right.
ggplot(lift_chart_data, aes(x = as.factor(RiskDecile))) +
  geom_bar(aes(y = AvgPredictedPurePremium, fill = "Predicted"), stat = "identity", alpha = 0.7) +
  geom_bar(aes(y = ActualPurePremium, fill = "Actual"), stat = "identity", alpha = 0.7) +
  ggtitle("Model Lift Chart: Predicted vs. Actual Pure Premium") +
  xlab("Risk Decile (1 = Highest Risk)") +
  ylab("Average Pure Premium") +
  scale_fill_manual(name = "Metric", values = c("Predicted" = "skyblue", "Actual" = "darkred")) +
  theme_minimal()


#--------------------------------------------------------------------------
# 4. OVERALL PROFITABILITY CHECK
#--------------------------------------------------------------------------
# Let's check if our overall math is correct.

total_premium <- sum(pricing_data$Premium)
total_pure_premium <- sum(pricing_data$PurePremium)
expected_loss_ratio <- total_pure_premium / total_premium

print(paste("Total Premium for all 10,000 policies: $", round(total_premium, 2)))
print(paste("Total Expected Loss for all 10,000 policies: $", round(total_pure_premium, 2)))
print(paste("Expected Portfolio Loss Ratio: ", round(expected_loss_ratio * 100, 2), "%"))
print(paste("Target Loss Ratio (1 - expense - profit): ", (1 - expense_ratio - profit_margin) * 100, "%"))

print("Step 4 (Validation) is complete.")

