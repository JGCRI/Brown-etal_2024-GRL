## Helper functions

#
#' Objective function for optimizing skew-normal hyperparameters
#'
#' This function calculates the error between the quantiles of a skew-normal distribution
#' (defined by \code{xi}, \code{omega}, and \code{alpha}) and desired target quantiles.
#' It is used for parameter optimization.
#'
#' @param params A numeric vector of length 3, containing parameters \code{xi} (location),
#' \code{omega} (scale), and \code{alpha} (shape) of the skew-normal distribution.
#'
#' @return A single numeric value representing the squared error between the computed and
#' desired quantiles (5th, 50th, and 95th percentiles).
#'
#' @export
#'
#' @examples
#' params <- c(2.5, 1.0, 0.5)
#' objective_function(params)
#'
objective_function <- function(params) {
  xi <- params[1]
  omega <- params[2]
  alpha <- params[3]

  # Calculate the quantiles for the given parameters
  q5 <- qsn(0.05, xi = xi, omega = omega, alpha = alpha)
  q50 <- qsn(0.50, xi = xi, omega = omega, alpha = alpha)
  q95 <- qsn(0.95, xi = xi, omega = omega, alpha = alpha)

  # Desired quantiles
  desired_q5 <- 2.0
  desired_q50 <- 3.0
  desired_q95 <- 5.0

  # Calculate the squared differences
  error <- (q5 - desired_q5)^2 + (q50 - desired_q50)^2 + (q95 - desired_q95)^2

  # Print intermediate values for debugging
  cat("xi:", xi, "omega:", omega, "alpha:", alpha, "error:", error, "\n")

  return(error)
}

#' Normalize data to a reference period
#'
#' This function normalizes time-series data by subtracting the mean value of a specified
#' reference period from all observations. This is useful for analyzing anomalies relative
#' to a baseline period.
#'
#' @param data A data frame containing at least two columns:
#'  \code{year} (numeric) and \code{value} (numeric).
#'  \code{year} represents the time variable, and \code{value} contains
#'  the data to be normalized.
#' @param ref_start An integer specifying the first year of the reference period
#' @param ref_end An integer specifying the last year of the reference period
#'
#' @return A numeric vector containing the normalized values, where each data point
#' is adjusted by subtracting the mean of the reference period.
#'
#' @export
#'
#' @examples
#' # Example dataset
#' data <- data.frame(year = 1990:2020, value = rnorm(31, mean = 2, sd = 0.5))
#'
#' # Normalize data using the reference period 1995-2014
#' normalized_values <- normalize_dat(data, ref_start = 1995, ref_end = 2014)
#' head(normalized_values)
normalize_dat <- function(data, ref_start, ref_end) {

  # Filter data for the reference period
  ref_period <- subset(
    data,
    year >= ref_start &
      year <= ref_end
  )

  # Calculate the mean for the reference period
  mean_ref_dat <- mean(ref_period$value, na.rm = TRUE)

  # Normalize values by subtracting the mean_ref_period
  norm_dat <- data$value - mean_ref_dat

  # Return the normalized data
  return(norm_dat)
}

#' Normalizing historical temperature data
#'
#' This function normalizes historical data by subtracting the mean temperature of a specified
#' reference period from all observations. Unlike \code{normalize_dat()}, which returns only a
#' numeric vector of normalized values, this function returns the entire modified data frame,
#' preserving the structure for further analysis.
#'
#' @param data A data frame containing at least two columns:
#'   \code{year} (numeric) and \code{value} (numeric), where \code{year}
#'   represents the time variable and \code{value} contains the temperature data.
#' @param reference_years A numeric vector specifying the years to use as the
#'   reference period for calculating the mean temperature.
#'
#' @return A data frame with the same structure as \code{data}, but with the
#'   \code{value} column adjusted to represent anomalies relative to the reference period.
#'
#' @export
#'
#' @examples
#' # Example dataset
#' data <- data.frame(year = 1980:2020, value = rnorm(41, mean = 15, sd = 0.5))
#'
#' # Normalize data using the reference period 1995-2014
#' normalized_data <- normalize_historical(data, reference_years = 1995:2014)
#' head(normalized_data)
normalize_historical <- function(data, reference_years) {

    reference_data <- data[data$year %in% reference_years, ]

    mean_reference_period <- mean(reference_data$value)

    normalize_values <- data$value - mean_reference_period

    data$value <- normalize_values

    return(data)

  }


#' Recode scenario names for plotting
#'
#' This function modifies scenario names in a data set to ensure they are formatted consistently
#' for plotting. It replaces existing scenario names with more readable or standardized labels.
#'
#' @param data A data frame containing a column named \code{scenario}, which holds character
#'  or factor values representing different scenario configurations.
#'
#' @return A data frame with the \code{scenario} column recoded to use standardized scenario
#' lables.
#'
#' @note This function is useful for preparing data for visualizations, ensuring that scenario
#' labels are clear and consistent in plots.
#'
#' @export
#'
#' @examples
#' library(dplyr)  # Ensure dplyr is available for recode_factor()
#'
#' # Example dataset
#' data <- data.frame(scenario = c("IPCC", "Baseline", "No_Historical",
#'                                 "No_Process", "No_Paleoclimate",
#'                                 "Baseline_Emergent_constraints"))
#'
#' # Recode scenario names
#' recoded_data <- recode_scenarios(data)
#' print(recoded_data)
recode_scenarios <- function(data) {

  data$scenario <- recode_factor(
    data$scenario,
    IPCC = "IPCC AR6*",
    Baseline = "Baseline",
    No_Historical = "No Historical",
    No_Process = "No Process",
    No_Paleoclimate = "No Paleoclimate",
    Baseline_Emergent_constraints = "Baseline + Emergent constraints")

  return(data)
}

# Compute data summary (median and 5-95% CI)

#' Compute summary statistics for GSAT metrics (median and 5-95% CI)
#'
#' This functions calculates the median and 5-95% confidence interval (CI) for
#' global surface air temperature (GSAT) metric results, grouped by scenario.
#' This function uses weighted quantiles to account for probabilistic distributions.
#'
#' @param data A data frame containing at least three columns:
#'   \code{scenario} (character or factor),
#'   \code{metric_result} (numeric values representing GSAT results), and
#'   \code{norm_weight} (numeric weights for computing weighted quantiles).
#'
#' @return A data frame with summary statistics, including:
#'   \item{scenario}{The scenario name.}
#'   \item{median}{The weighted median of \code{metric_result}.}
#'   \item{lower}{The 5th percentile (lower bound of the confidence interval).}
#'   \item{upper}{The 95th percentile (upper bound of the confidence interval).}
#'
#' @note This function assumes that \code{metric_result} is numeric and
#'   that \code{norm_weight} represents valid weights for computing
#'   weighted quantiles.
#'
#' @export
#'
data_summary <- function(data){

  gsat_metric_stats <-
    data %>%
    group_by(scenario) %>%
    summarize(
      median = weighted.quantile(metric_result, w = norm_weight, probs = 0.5),
      lower = weighted.quantile(metric_result, w = norm_weight, probs = 0.05),
      upper = weighted.quantile(metric_result, w = norm_weight, probs = 0.95),
      .groups = "drop")

  return(gsat_metric_stats)

}

#' Compute Kolmogorov-Smirnov (KS) test for a pair of distributions
#'
#' This function performs a two-sample KS test to compare two empirical distributions.
#' The test analyzes whether two samples could come from the same hypothetical
#' distribution.
#'
#' @param pair A numeric vector of length 2, containing the indices of the two
#'   distributions to compare. These indices correspond to entries in
#'   \code{names_distributions} and \code{metric_values}.
#'
#' @return A list containing:
#'   \item{pair}{A character vector with the names of the two compared distributions.}
#'   \item{ks_test}{An object of class \code{htest} containing the results of
#'   the KS test, including the test statistic and p-value.}
#'
#' @export
#'
compute_ks_test <- function(pair) {
  # Extract distribution names and data
  name1 <- names_distributions[pair[1]]
  name2 <- names_distributions[pair[2]]
  dist1 <- metric_values[[pair[1]]]
  dist2 <- metric_values[[pair[2]]]

  # Perform the KS test
  ks_results <- ks.test(dist1, dist2)

  # Return a list with pair names and test results
  return(list(pair = c(name1, name2), ks_test = ks_results))
}

#' Compute Kernal Density Estimates (KDE) with weights
#'
#' This function computes KDE for a specified variable, splitting the data by a
#' categorical grouping variable. It allows for weighted density estimation to account
#' for varying data importance.
#'
#' @param data A data frame containing the data to be analyzed.
#' @param split_by A factor or character vector indicating how the data should be
#'   split into groups before KDE computation (e.g., different scenarios or categories).
#' @param variable A numeric vector representing the variable for which the kernel
#'   density estimate is computed.
#' @param weights A numeric vector of the same length as \code{variable}, specifying
#'   the weights for weighted density estimation.
#'
#' @return A data frame containing three columns:
#'   \item{scenario}{The grouping variable (from \code{split_by}).}
#'   \item{value}{The x-coordinates of the estimated density function.}
#'   \item{density}{The corresponding density estimates.}
#'
#' @export
#'
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


