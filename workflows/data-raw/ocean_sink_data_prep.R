## Preparing Ocean Carbon Uptake data
##
## This ocean sink data includes values downloaded
## on 06/18/2024 from
##
## Full citation:

library(tidyverse)
library(matilda)

gcp_ocean_sink <- read.csv("workflows/data-raw/gcp_ocean_uptake_data.csv", stringsAsFactors = F)

ocean_uptake_df <- gcp_ocean_sink %>%
  select(Year, ocean_sink) %>%
  rename(value = ocean_sink,
         year = Year) %>%
  filter(year > 1849) %>%
  na.omit()

write.csv(ocean_uptake_df, "workflows/data-raw/annual_ocean_c_uptake.csv", quote = FALSE, row.names = FALSE)
