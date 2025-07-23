library(caret)

set.seed(123)  # For reproducibility
train_index <- createDataPartition(data$has_claim, p = 0.7, list = FALSE)

train_data <- data[train_index, ]
test_data  <- data[-train_index, ]


model_glm <- glm(has_claim ~ DriverAgeBand + CarAgeBand + LogDensity + Power + Brand + Region + Gas,                
			 data = train_data,                   
			family = binomial)

model_zip <- zeroinfl(has_claim ~ DriverAgeBand + CarAgeBand ,
                      data = train_data,
                      dist = "poisson")

summary(model_freq)
summary(model_zip)

test_data$pred_glm <- predict(model_glm, newdata = test_data, type = "response")
test_data$pred_zip <- predict(model_zip, newdata = test_data, type = "response")

test_data <- test_data %>%
  mutate(bin_pred_glm = ntile(-pred_glm, 10),  # - for descending
         bin_pred_zip = ntile(-pred_zip, 10))

lift_data_1 <- test_data %>%
  group_by(bin_pred_glm) %>%
  summarise(avg_pred = mean(pred_glm),
            actual_rate = mean(has_claim),
            count = n()) %>%
  mutate(model = "Model 1")

lift_data_2 <- test_data %>%
  group_by(bin_pred_zip) %>%
  summarise(avg_pred = mean(pred_zip),
            actual_rate = mean(has_claim),
            count = n()) %>%
  mutate(model = "Model 2")

lift_data <- bind_rows(
  lift_data_glm %>% rename(bin = bin_pred_glm),
  lift_data_zip %>% rename(bin = bin_pred_zip)
)

ggplot(lift_data, aes(x = as.factor(bin), y = actual_rate, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Lift Chart (10 bins)",
       x = "Predicted Risk Decile (1 = highest)",
       y = "Actual Claim Rate") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()


plot_distribution(test_data, var = "pred_glm", type = "density")
plot_distribution(test_data, var = "pred_zip", type = "density")
