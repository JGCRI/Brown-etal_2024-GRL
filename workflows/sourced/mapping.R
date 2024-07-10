## Mapping data to help with renaming and ordering factor levels

# Mapping to rename bin levels

bins_mapping <- c(
  "(0,1]" = "0 to 1.0 C",
  "(1,1.5]" = "1.0 to 1.5 C",
  "(1.5,2]" = "1.5 to 2.0 C",
  "(2,2.5]" = "2.0 to 2.5 C",
  "(2.5,3]" = "2.5 to 3.0 C",
  "(3,3.5]" = "3.0 to 3.5 C",
  "(3.5,4]" = "3.5 to 4.0 C",
  "(4,4.5]" = "4.0 to 4.5 C",
  "(4.5,Inf]" = "4.5+ C"
)

# Scenario order for plotting

scenario_order <-
  c("Baseline",
    "No Process",
    "No Historical",
    "No Paleoclimate",
    "Baseline + Emergent constraints")
