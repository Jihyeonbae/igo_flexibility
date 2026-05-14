library(modelsummary)

# Coefficient map for hurdle models
cm_hurdle <- c(
  'polyarchy'    = 'Polyarchy (Mean)',
  'gdp_cap'      = 'GDP Per Capita',
  'trade'        = 'Trade',
  'globalization' = 'Globalization',
  'alliances'    = 'Alliances',
  'number'       = 'Number',
  'political'    = 'Political',
  'social'       = 'Social',
  'hh_gdp'       = 'Economy HHI',
  'polyarchy_sd' = 'Political Heterogeneity',
  'ideal_sd'     = 'Preference Heterogeneity'
)

hurdle_models <- list(
  "Stage 1: Any Flexibility (Logit)" = hurdle_s1,
  "Stage 2: Degree of Flexibility (OLS)" = hurdle_s2
)

hurdle_vcovs <- list(
  vcovHC(hurdle_s1, type = "HC1"),
  vcovHC(hurdle_s2, type = "HC1")
)

modelsummary(
  hurdle_models,
  vcov      = hurdle_vcovs,
  stars     = TRUE,
  coef_map  = cm_hurdle,
  coef_omit = "Intercept",
  gof_map   = c("nobs", "r.squared"),
  title     = "Robustness: Hurdle Model",
  notes     = "Stage 1 models whether an IGO has any amendment flexibility (logit). Stage 2 models the degree of flexibility conditional on being nonzero (OLS). Robust standard errors.",
  output    = "~/Desktop/igo_flexibility/results/tableA4_hurdle.tex",
  booktabs  = TRUE,
  escape    = FALSE
)
cat("Hurdle table saved\n")