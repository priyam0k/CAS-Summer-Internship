Pricing_data <- read_excel("2025 CAS East Asia Summer Program Dataset.xlsx", sheet = "Pricing Data")  # Sheet name or number


Pricing_data$DriverAgeBand <- cut(Pricing_data$DriverAge,
                          breaks = c(17, 21, 26, 31, 41, 51, 71, Inf),
                          labels = c("18-21", "22-25", "26-30", "31-40", "41-50", "51-70", "71+"),
                          right = TRUE,
                          include.lowest = TRUE)

Pricing_data$CarAgeBand <- cut(Pricing_data$CarAge,
  			breaks = c(0, 1, 10, Inf),
  			labels = c("New [0-1)", "Moderate [1-10]", "Old (10+)"),
  			right = TRUE,
  			include.lowest = TRUE
)


Pricing_data$LogDensity <- log(Pricing_data$Density)


Pricing_data$pred_glm <- predict(model_glm, newdata = Pricing_data, type = "response")

avg_claim_nonzero <- mean(data$ClaimAmount[data$ClaimAmount > 0])

Pricing_data$Premium <- Pricing_data$pred_glm * avg_claim_nonzero

plot_distribution(Pricing_data, var = "Premium", type = "density")

Pricing_data$Premium_capped <- pmax(Pricing_data$Premium, 70)

write_xlsx(
  Pricing_data %>% select(PolicyID, Premium_capped),
  path = "Group X output.xlsx"
)