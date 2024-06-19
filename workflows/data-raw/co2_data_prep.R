## Preparing CO2 concentration data
##
## This CO2 concentration data includes values downloaded
## on 06/14/2024 from https://github.com/ClimateIndicator/forcing-timeseries/tree/main/data/ghg_concentrations/ar6_updated
##
## Full citation:

library(tidyverse)
library(matilda)

# Read in data
co2_conc <- read.csv("workflows/data-raw/ipcc_ar6_wg1_annual_co2.csv", stringsAsFactors = F)

newnames <- c(year = "YYYY", value = "CO2")

co2_conc <- co2_conc %>%
  rename(all_of(newnames))

# write new data file
write.csv(co2_conc, "workflows/data-raw/annual_co2_concentration.csv", quote = FALSE, row.names = FALSE)
