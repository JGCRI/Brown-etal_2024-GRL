ggplot() +
  geom_line(data = hector_result,
            aes(
              x = year,
              y = value
            ), color = "black") +
  geom_line(data = subset(model_result$Baseline_Emergent_constraints, variable =="gmst" & run_number == "5890"),
            aes(
              x = year,
              y = value
            ), color = "red") +
  geom_line(data = temp_hist,
            aes(
              x = year,
              y = value
            ), color = "blue")

mean(temp_hist$value)
test_rmse <- ensemble_gmst_rmse$Baseline
View(test_rmse)
test_rmse$percentage <- (test_rmse$rmse_value / 0.119) * 100

test_df <- subset(model_result$Baseline_Emergent_constraints, variable == "gmst" & year > 1849 & year < 2024 & run_number == "5890")

sqrt(mean(((temp_hist$value - test_df$value)/1)^2))

