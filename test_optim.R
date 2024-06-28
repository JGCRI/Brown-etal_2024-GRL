# Observed quantile values
observed_quantiles <- c(2.2550000000000026,
                        2.4050000000000082,
                        2.5549999999999997,
                        2.615000000000002,
                        2.7050000000000054,
                        3.105000000000011,
                        3.625000000000007,
                        3.7749999999999986,
                        3.884999999999998,
                        4.2350000000000065,
                        4.685000000000009)

# Corresponding quantile positions (proportions)
quantile_positions <- c(0.05, 0.10, 0.17, 0.20, 0.25, 0.50, 0.75, 0.80, 0.83, 0.90, 0.95)

# Objective function to minimize
objective_function <- function(params, observed_quantiles, quantile_positions) {
  shape <- params[1]
  rate <- params[2]

  # Compute the predicted quantiles for the given parameters
  predicted_quantiles <- qgamma(quantile_positions, shape = shape, rate = rate)

  # Calculate the sum of squared differences
  sum((observed_quantiles - predicted_quantiles)^2)
}

# Initial parameter guesses
initial_params <- c(shape = 17.62, rate = 5.41)

# Use optim to minimize the objective function
fit <- optim(par = initial_params,
             fn = objective_function,
             observed_quantiles = observed_quantiles,
             quantile_positions = quantile_positions)

# Extract the optimized parameters
optimized_params <- fit$par

# Print the optimized parameters
print(optimized_params)

# Sample from the optimized gamma distribution
set.seed(123)
samples <- rgamma(15000, shape = optimized_params[1], rate = optimized_params[2])

# Plot the histogram of the samples
ggplot() +
  geom_density(data = samples,
               aes(x = ecs),
               fill = "blue")
