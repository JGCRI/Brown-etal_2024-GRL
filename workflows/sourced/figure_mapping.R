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

# Mapping to change scenario order for plotting

scenario_order <-
  c("IPCC AR6*",
    "Baseline",
    "No Process",
    "No Historical",
    "No Paleoclimate",
    "Baseline + Emergent constraints")

# Mapping for ECS configuration colors

ECS_COLORS <-
  c("IPCC AR6*" = "#003466",
    "Baseline" =  "#00a9cf",
    "No Process" = "#BDC881",
    "No Historical" = "#EBCC2A",
    "No Paleoclimate" = "#F21A00",
    "Baseline + Emergent constraints" =  "#550307"
  )

# Mapping for temperature probability colors

TEMP_PROBABILITY_COLORS <-
  c(
    "0 to 1.0 C" = "navyblue",
    "1.0 to 1.5 C" = "#2166AC",
    "1.5 to 2.0 C" = "#4393C3",
    "2.0 to 2.5 C" = "#D1E5f0",
    "2.5 to 3.0 C" = "#FDDBC7",
    "3.0 to 3.5 C" = "#F4A582",
    "3.5 to 4.0 C" = "#D6604D",
    "4.0 to 4.5 C" = "#B2182B",
    "4.5+ C" = "#67001F"
  )

# Mapping for ECS posterior distirbution color

ECS_POSTERIOR_COLORS <-
  c( "#4393C3",
     "#4393C3",
     "#4393C3",
     "#4393C3",
     "#4393C3",
     "#4393C3")

# Mapping for line type in cumulative density function (CDF) figure

LINE_TYPE <-
  c("solid",
    "solid",
    "solid",
    "solid",
    "solid",
    "dotdash")
