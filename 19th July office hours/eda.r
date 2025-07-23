library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)      
library(tidyverse)  

# Download the Excel file from your GitHub repo (replace with your actual URL)
download.file(
  "https://raw.githubusercontent.com/your-username/your-repo/main/2025%20CAS%20East%20Asia%20Summer%20Program%20Dataset.xlsx",
  destfile = "2025 CAS East Asia Summer Program Dataset.xlsx",
  mode = "wb"
)

data <- read_excel("2025 CAS East Asia Summer Program Dataset.xlsx", sheet = "Dataset for Student")  # Sheet name or number


data$Power <- as.factor(data$Power)
data$Brand <- as.factor(data$Brand)
data$Gas <- as.factor(data$Gas)
data$Region <- as.factor(data$Region)

data$DriverAgeBand <- cut(data$DriverAge,
                          breaks = c(17, 21, 26, 31, 41, 51, 71, Inf),
                          labels = c("18-21", "22-25", "26-30", "31-40", "41-50", "51-70", "71+"),
                          right = TRUE,
                          include.lowest = TRUE)
table(data$DriverAgeBand, useNA = "ifany")


data$CarAgeBand <- cut(data$CarAge,
  			breaks = c(0, 1, 10, Inf),
  			labels = c("New [0-1)", "Moderate [1-10]", "Old (10+)"),
  			right = TRUE,
  			include.lowest = TRUE
)
table(data$CarAgeBand, useNA = "ifany")

plot_distribution(data, var = "DriverAge", type = "density")
plot_distribution(data, var = "CarAge", type = "density")
plot_distribution(data, var = "Density", type = "density")

data$LogDensity <- log(data$Density)
plot_distribution(data, var = "LogDensity", type = "density")

plot_distribution(data, var = "DriverAge", group = "Power", type = "density")
plot_distribution(data, var = "CarAge", group = "Power", type = "density")

freq_driver_age <- calculate_frequency(data, var = "DriverAgeBand")
plot_frequency(freq_driver_age, var = "DriverAgeBand")

freq_car_age <- calculate_frequency(data, var = "CarAgeBand")
plot_frequency(freq_driver_age, var = "CarAgeBand")

freq_brand <- calculate_frequency(data, var = "Brand")
plot_frequency(freq_brand, var = "Brand")

freq_power <- calculate_frequency(data, var = "Power")
plot_frequency(freq_driver_age, var = "Power")