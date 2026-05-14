
igo_dataset <- read_rds("~/Desktop/igo_flexibility/igo_dataset_for_analysis.rds")

library(gplots)
library(ggplot2)
library(gridExtra)

# Panel (a): variation across time
p_time <- igo_dataset %>%
  dplyr::filter(!is.na(poolconstit)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    mean_pool = mean(poolconstit, na.rm = TRUE),
    se_pool   = sd(poolconstit, na.rm = TRUE) / sqrt(n()),
    .groups   = "drop"
  ) %>%
  ggplot(aes(x = year, y = mean_pool)) +
  geom_line(color = "steelblue") +
  geom_ribbon(aes(ymin = mean_pool - 1.96*se_pool,
                  ymax = mean_pool + 1.96*se_pool),
              alpha = 0.2, fill = "steelblue") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 11)) +
  labs(x = "Year", y = "Mean Pooling Score",
       title = "(a) Variation across Time")

# Panel (b): variation across IGOs
p_igo <- igo_dataset %>%
  dplyr::filter(!is.na(poolconstit), !is.na(ioname)) %>%
  dplyr::group_by(ioname) %>%
  dplyr::summarise(
    mean_pool = mean(poolconstit, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  dplyr::arrange(desc(mean_pool)) %>%
  ggplot(aes(x = reorder(ioname, mean_pool), y = mean_pool)) +
  geom_point(color = "steelblue", size = 1) +
  coord_flip() +
  theme_bw() +
  theme(text         = element_text(family = "Times New Roman", size = 11),
        axis.text.y  = element_text(size = 6)) +
  labs(x = NULL, y = "Mean Pooling Score",
       title = "(b) Variation across IGOs")

# Combine and save
fig1 <- gridExtra::grid.arrange(p_time, p_igo, ncol = 2)
ggsave(
  "~/Desktop/igo_flexibility/results/figure1_variation.png",
  plot   = fig1,
  width  = 10,
  height = 5,
  dpi    = 300
)