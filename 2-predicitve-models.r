# CAS Summer Internship - Pricing Competition
# Step 2: Building the Frequency and Severity Models

#--------------------------------------------------------------------------
# PRE-REQUISITE: Ensure Step 1 has been run
#--------------------------------------------------------------------------
# This script assumes that the 'data' dataframe is already loaded and
# prepared in your R environment from running the Step 1 script.

#--------------------------------------------------------------------------
# 1. SETUP: Load libraries and split data
#--------------------------------------------------------------------------
# We will split our data into a training set (70%) and a testing set (30%).
# This is a crucial step to ensure our model is not just memorizing the
# data, but can actually make accurate predictions on new, unseen data.

# Set a seed for reproducibility. This ensures that the "random" split
# is the same every time we run the code.
set.seed(123)

# Create an index for the training data
train_index <- sample(1:nrow(data), 0.7 * nrow(data))

# Create the training and testing datasets
train_data <- data[train_index, ]
test_data  <- data[-train_index, ]

print("Data successfully split into training and testing sets.")


#--------------------------------------------------------------------------
# 2. FREQUENCY MODEL: Predicting the number of claims
#--------------------------------------------------------------------------
# We will use a Poisson GLM. The goal is to model the ClaimNb (0, 1, 2, etc.).
# We include the variables that our EDA suggested were important.

# Build the model
# The formula "ClaimNb ~ ..." means we are predicting ClaimNb using the
# variables on the right side.
# family = poisson tells the GLM to use the Poisson distribution.
model_freq <- glm(ClaimNb ~ DriverAgeBand + CarAgeBand + LogDensity + Power + Brand + Region + Gas,
                  data = train_data,
                  family = poisson(link = "log"))

# Display the model summary
# This is the most important output! It tells us which variables are
# statistically significant predictors of claim frequency.
# Look for low p-values (Pr(>|z|)), typically less than 0.05.
print("--- Frequency Model Summary ---")
summary(model_freq)


#--------------------------------------------------------------------------
# 3. SEVERITY MODEL: Predicting the cost of claims
#--------------------------------------------------------------------------
# Now we model the ClaimAmount. This model is only built on policies
# that actually had a claim, so we need to filter our data first.

# Create a training dataset for severity (only policies with claims)
train_data_sev <- train_data %>% filter(ClaimNb > 0)

# We will use a Gamma GLM. This is the industry standard for severity.
# The 'weights = ClaimNb' argument is important. It gives more weight
# to policies that had multiple claims, as they provide more reliable
# information about the average cost.
model_sev <- glm(ClaimAmount ~ DriverAgeBand + CarAgeBand + LogDensity + Power + Brand + Region + Gas,
                 data = train_data_sev,
                 family = Gamma(link = "log"),
                 weights = ClaimNb)

# Display the model summary
# This tells us which variables are significant predictors of claim cost.
print("--- Severity Model Summary ---")
summary(model_sev)


print("Step 2 (Modeling) is complete.")
print("Next, we will use these two models to calculate the final premium for each policy.")
