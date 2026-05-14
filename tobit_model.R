library(AER)

# Tobit: left-censored at 0, right-censored at 1
tobit_main <- tobit(
  poolconstit ~ polyarchy + gdp_cap + trade + globalization +
    alliances + number + political + social +
    hh_gdp + polyarchy_sd + ideal_sd,
  data  = igo_analysis,
  left  = 0,
  right = 1
)
summary(tobit_main)


# Add Tobit to robustness output
library(AER)

tobit_models <- list(
  "Polyarchy"  = tobit_main
)

modelsummary(
  tobit_models,
  stars     = TRUE,
  coef_map  = cm,
  coef_omit = "Intercept|Log",
  gof_map   = c("nobs", "logLik"),
  title     = "Robustness: Tobit Models (Left-Censored at 0)",
  notes     = "Tobit models with left-censoring at 0 and right-censoring at 1. Addresses bounded DV distribution.",
  output    = "~/Desktop/igo_flexibility/results/tableA2_tobit.tex",
  booktabs  = TRUE,
  escape    = FALSE
)
