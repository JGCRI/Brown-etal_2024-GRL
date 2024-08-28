## Helper functions

# Normalizing data to reference period
normalize_dat <- function(data, ref_start, ref_end) {

  # filter data for the reference period
  ref_period <- subset(
    data,
    Year >= ref_start &
      Year <= ref_end
  )

  # calculate mean for the reference period
  mean_ref_dat <- mean(ref_period$value, na.rm = T)

  # Normalize values by dividing each by the mean_ref_period
  norm_dat <- data$value / mean_ref_dat

  # return the new data frame
  return(norm_dat)

}


# Recoding scenario names for the plot
recode_scenarios <- function(data) {

  data$scenario <- recode_factor(
    data$scenario,
    Baseline = "Baseline",
    No_Historical = "No Historical",
    No_Process = "No Process",
    No_Paleoclimate = "No Paleoclimate",
    Baseline_Emergent_constraints = "Baseline + Emergent constraints")

  return(data)
}

# Compute data summary (median and 5-95% CI)

data_summary <- function(data){

  gsat_metric_stats <-
    data %>%
    group_by(scenario) %>%
    summarize(
      median = weighted.quantile(metric_result, w = weight_norm, probs = 0.5),
      lower = weighted.quantile(metric_result, w = weight_norm, probs = 0.05),
      upper = weighted.quantile(metric_result, w = weight_norm, probs = 0.95),
      .groups = "drop")

  return(gsat_metric_stats)

}


# Computing Kernal Density Estimates

kde_values <- function(data, split_by, variable, weights) {

  split_df <- split(data, split_by)

  density_result <- lapply(names(split_df), function(df_name) {
    # store data based on name of the current df
    # this way we can add a scenario column to the final results
    df <- split_df[[df_name]]

    # use that df to compute density estimates weight by `weights`
    density_values <- density(variable, weights = weights)

    # build data frame with coordinates we want to plot
    density_estimate <- data.frame(scenario = df_name,
                                   value = density_values$x,
                                   density = density_values$y)

    return(density_estimate)
  })

  kde_values_result <- do.call(rbind, density_result)

  return(kde_values_result)

}

kde_test <- kde_values(ecs_plot_df,
                       split_by = ecs_plot_df$scenario,
                       variable = ecs_plot_df$ECS,
                       weights = ecs_plot_df$weight_norm)
