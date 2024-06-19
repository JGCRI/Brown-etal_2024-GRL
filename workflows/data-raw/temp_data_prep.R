## Preparing GMST data
##
## This temperature data includes historical temperature anomaly values downloaded
## on 06/13/2024 from https://github.com/ClimateIndicator/data/blob/main/data/global_mean_temperatures/annual_averages.csv
##
## Full citation:

library(tidyverse)
library(matilda)

# Read in data
hist_temp <- read.csv("workflows/data-raw/annual_gmst_averages.csv", stringsAsFactors = F)

newnames <- c(year = "timebound_lower", value = "gmst")

hist_temp <- hist_temp %>%
  rename(all_of(newnames)) %>%
  select(year, value)


## Write function for normalizing data

#' Normalizing data to specific reference period
#'
#' @param observed_data data from historical period as a data frame.
#' @param modeled_data data from a hector result that contains value being normalized.
#' hector data is available as matilda:::.
#' @param reference_start_year numerical. start year of reference period.
#' @param reference_end_year numerical. end year of reference period.
#'
normalize_values <- function(observed_data, modeled_data, reference_start_year, reference_end_year) {

  # Filter modeled data for the reference period
  modeled_reference_period <- subset(
    modeled_data,
    year >= reference_start_year &
      year <= reference_end_year

    )

  # Calculate the mean value for the modeled reference period
  mean_modeled_value <- mean(modeled_reference_period$value)

  # Calculate normalized value for each year in the observed data
  normalized_value <- observed_data$value - mean_modeled_value

  # Create a new data frame with the normalized values
  normalized_data <- data.frame(year = observed_data$year, value = normalized_value)

  return(normalized_data)
}

## Running normalization on historical gmst to align with Hector output
hist_temp_norm <- normalize_values(hist_temp, matilda::hector_result, 1961, 1990)

## Saving .csv of normalized gmst
write.csv(hist_temp_norm, "workflows/data-raw//annual_gmst_normalized.csv", quote = FALSE, row.names = FALSE)

