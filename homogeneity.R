
p_load(plyr, dplyr, tidyr, ggplot2, tidyverse, RColorBrewer, readxl,
       readr, haven, countrycode,
       rqog, igoR, modelsummary, knitr, kableExtra, flextable, unvotes)

igo_dataset <- read_rds("~/Desktop/igo_flexibility/igo_dataset_for_analysis.rds")

igo_analysis<-igo_dataset

# ── Homogeneity plot ──────────────────────────────────────────────────────────
p_homogeneity <- ggplot(igo_analysis, aes(x = polyarchy, y = ideal_sd)) +
  geom_point(alpha = 0.2, color = "gray50") +
  geom_smooth(method = "loess", color = "steelblue", se = TRUE) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.6) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 13)) +
  labs(
    x       = "Mean Polyarchy Score (IGO Level)",
    y       = "Preference Heterogeneity (SD of Ideal Points)",
    caption = "Red dashed line at 0.5 indicates AIGO/DIGO threshold.\nLoess smoother with 95% confidence interval. IGO-year observations."
  )

print(p_homogeneity)

ggsave(
  "~/Desktop/igo_flexibility/results/figure_homogeneity.png",
  plot   = p_homogeneity,
  width  = 7,
  height = 5,
  device = "png",
  dpi    = 300
)

# ── Polyarchy_sd version (if you want both) ───────────────────────────────────
p_polsd <- ggplot(igo_analysis, aes(x = polyarchy, y = polyarchy_sd)) +
  geom_point(alpha = 0.2, color = "gray50") +
  geom_smooth(method = "loess", color = "steelblue", se = TRUE) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.6) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 13)) +
  labs(
    x       = "Mean Polyarchy Score (IGO Level)",
    y       = "Political Heterogeneity (SD of Polyarchy)",
    caption = "Red dashed line at 0.5 indicates AIGO/DIGO threshold.\nLoess smoother with 95% confidence interval. IGO-year observations."
  )

print(p_polsd)

ggsave(
  "~/Desktop/igo_flexibility/results/figure_polsd.png",
  plot   = p_polsd,
  width  = 7,
  height = 5,
  device = "png",
  dpi    = 300
)