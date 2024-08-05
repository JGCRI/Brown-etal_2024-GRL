# Merge constrained results with weights and normalize
weighted_ensemble_normalized <- Map(function(ensemble, weights) {

  # Merge by "run_number"
  merged_data <- merge(ensemble, weights, by = "run_number")

  # Calculate total weight and normalize
  total_weight <- sum(merged_data$mc_weight)
  merged_data$weight_norm <- merged_data$mc_weight / total_weight

  return(merged_data)

}, constrained_result, constrained_weights)


# Check if weights sum to 1 for each model's ensemble
sapply(weighted_ensemble_normalized, function(df) sum(df$weight_norm))
