#' @title Generate Theoretical Extension Visualizations for Cuisine Authenticity
#' @description Produces publication-ready visualizations evaluating hypotheses and theoretical questions
#'   from "Authenticity vs. Conventional Highbrow Restaurant Preferences" (Childress & Lizardo):
#'   1. H1–H3: Political Ideology Asymmetry (Social vs. Economic Conservatism).
#'   2. H4: Cuisine Consecration Hierarchies and Ideological Polarization Slopes.
#'   3. Cultural Capital Disaggregation: Childhood Socialization vs. Education vs. Income.
#' @author Cuisine Authenticity Project

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(ggplot2)
  library(brms)
  library(tidybayes)
  library(ggdist)
  library(ggrepel)
  library(here)
})

cat("========================================================================\n")
cat("Generating Hypotheses & Extension Visualizations (Childress & Lizardo)\n")
cat("========================================================================\n")

# Ensure plot directory exists
plot_dir <- here("Plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

#' Custom Minimalist Theme
theme_cuisine <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size * 1.15, margin = margin(b = 5)),
      plot.subtitle = element_text(size = base_size * 0.9, color = "gray25", margin = margin(b = 10)),
      plot.caption = element_text(size = base_size * 0.75, color = "gray40", hjust = 0, margin = margin(t = 10)),
      axis.title.x = element_text(face = "bold", size = base_size * 0.95, margin = margin(t = 8)),
      axis.title.y = element_text(face = "bold", size = base_size * 0.95, margin = margin(r = 8)),
      axis.text.y = element_text(face = "bold", size = base_size * 0.9, color = "gray15"),
      axis.text.x = element_text(size = base_size * 0.85),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray92", linewidth = 0.4),
      panel.grid.major.x = element_line(color = "gray92", linewidth = 0.4),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = base_size * 0.85),
      legend.text = element_text(size = base_size * 0.8),
      plot.margin = margin(t = 12, r = 16, b = 12, l = 12)
    )
}

# -------------------------------------------------------------
# 1. Load Fitted Models & Data
# -------------------------------------------------------------
m6 <- readRDS(here("cache", "hier_6_relaxed_rs.rds"))
draws_m6 <- as_draws_df(m6)

soc_mean <- rowMeans(draws_m6[, paste0("bcs_social_c[", 1:6, "]")])
econ_mean <- rowMeans(draws_m6[, paste0("bcs_economic_c[", 1:6, "]")])
educ_mean <- rowMeans(draws_m6[, paste0("bcs_educ_c[", 1:6, "]")])
peduc_mean <- rowMeans(draws_m6[, paste0("bcs_peduc_c[", 1:6, "]")])
arts_mean <- rowMeans(draws_m6[, paste0("bcs_arts_c[", 1:6, "]")])
income_val <- draws_m6$b_income_c

# -------------------------------------------------------------
# 2. Figure EXT-1: Political Ideology Asymmetry (H1, H2, H3)
# -------------------------------------------------------------
cat("Generating Figure EXT-1: Political Ideology Asymmetry...\n")

df_h1_h3 <- tibble(
  draw = seq_along(soc_mean),
  `Social Conservatism` = soc_mean,
  `Economic Conservatism` = econ_mean,
  `Asymmetry Contrast (Social - Economic)` = soc_mean - econ_mean
) |>
  pivot_longer(cols = -draw, names_to = "parameter", values_to = "value") |>
  group_by(parameter) |>
  mutate(
    median_val = median(value),
    q025 = quantile(value, 0.025),
    q975 = quantile(value, 0.975),
    p_pos = mean(value > 0),
    p_neg = mean(value < 0),
    credibility = case_when(
      p_pos >= 0.95 ~ "Credibly Pro Chef (≥ 95% Mass)",
      p_neg >= 0.95 ~ "Credibly Domestic Elder (≥ 95% Mass)",
      TRUE ~ "Spans Zero (< 95% Mass)"
    ),
    label = sprintf("Median: %+.2f [%+.2f, %+.2f] | P(>0) = %.1f%%",
                    median_val, q025, q975, p_pos * 100)
  ) |>
  ungroup()

param_order <- c("Asymmetry Contrast (Social - Economic)", "Economic Conservatism", "Social Conservatism")
df_h1_h3$parameter <- factor(df_h1_h3$parameter, levels = param_order)

labels_h1_h3 <- df_h1_h3 |> distinct(parameter, label, credibility)

color_map <- c(
  "Credibly Pro Chef (≥ 95% Mass)" = "#0072B2",
  "Credibly Domestic Elder (≥ 95% Mass)" = "#D55E00",
  "Spans Zero (< 95% Mass)" = "gray60"
)

p_ext1 <- ggplot(df_h1_h3, aes(x = value, y = parameter, fill = credibility)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
  stat_halfeye(
    point_interval = median_qi,
    .width = c(0.80, 0.95),
    point_size = 3.5,
    interval_size = 1.2,
    slab_alpha = 0.35,
    scale = 0.65
  ) +
  geom_text(
    data = labels_h1_h3,
    aes(x = 0.55, y = parameter, label = label, color = credibility),
    hjust = 0,
    size = 3.3,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = color_map, name = "Credibility Status") +
  scale_color_manual(values = color_map, guide = "none") +
  scale_x_continuous(
    limits = c(-0.5, 1.2),
    breaks = seq(-0.4, 1.0, by = 0.2),
    labels = function(x) sprintf("%+.1f", x)
  ) +
  labs(
    title = "Hypotheses H1–H3: Political Ideology Asymmetry and Cultural Polarization",
    subtitle = "Posterior distributions of standardized location coefficients and asymmetry contrast from Model 6 (Relaxed CS + RS)",
    x = "Posterior Log-Odds Ratio (β) [Negative = Domestic Elder | Positive = Professional Chef]",
    y = "Ideological Construct",
    caption = "Directional credibility standard: ≥ 95% posterior probability mass on either side of zero.\nThick inner bar = 80% CrI; thin outer bar = 95% CrI; point = posterior median. Model estimated with brms/CmdStan."
  ) +
  theme_cuisine(base_size = 12)

ggsave(file.path(plot_dir, "ext_h1_h3_ideology_forest.png"), p_ext1, width = 11.5, height = 5.5, dpi = 300, bg = "white")
cat("Saved ext_h1_h3_ideology_forest.png\n")

# -------------------------------------------------------------
# 3. Figure EXT-3: Cultural Capital Disaggregation
# -------------------------------------------------------------
cat("Generating Figure EXT-3: Cultural Capital Disaggregation...\n")

df_cc <- tibble(
  draw = seq_along(educ_mean),
  `Educational Attainment (Institutionalized CC)` = educ_mean,
  `Parental Education (Inherited CC)` = peduc_mean,
  `Childhood Arts Socialization (Embodied CC)` = arts_mean,
  `Household Income (Economic Capital)` = income_val
) |>
  pivot_longer(cols = -draw, names_to = "dimension", values_to = "value") |>
  group_by(dimension) |>
  mutate(
    median_val = median(value),
    q025 = quantile(value, 0.025),
    q975 = quantile(value, 0.975),
    p_pos = mean(value > 0),
    p_neg = mean(value < 0),
    credibility = case_when(
      p_pos >= 0.95 ~ "Credibly Pro Chef (≥ 95% Mass)",
      p_neg >= 0.95 ~ "Credibly Domestic Elder (≥ 95% Mass)",
      TRUE ~ "Spans Zero (< 95% Mass)"
    ),
    label = sprintf("Median: %+.2f [%+.2f, %+.2f] | P(>0) = %.1f%%",
                    median_val, q025, q975, p_pos * 100)
  ) |>
  ungroup()

cc_order <- c(
  "Household Income (Economic Capital)",
  "Childhood Arts Socialization (Embodied CC)",
  "Parental Education (Inherited CC)",
  "Educational Attainment (Institutionalized CC)"
)
df_cc$dimension <- factor(df_cc$dimension, levels = cc_order)
labels_cc <- df_cc |> distinct(dimension, label, credibility)

p_ext3 <- ggplot(df_cc, aes(x = value, y = dimension, fill = credibility)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
  stat_halfeye(
    point_interval = median_qi,
    .width = c(0.80, 0.95),
    point_size = 3.5,
    interval_size = 1.2,
    slab_alpha = 0.35,
    scale = 0.65
  ) +
  geom_text(
    data = labels_cc,
    aes(x = 0.35, y = dimension, label = label, color = credibility),
    hjust = 0,
    size = 3.2,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = color_map, name = "Credibility Status") +
  scale_color_manual(values = color_map, guide = "none") +
  scale_x_continuous(
    limits = c(-0.4, 0.9),
    breaks = seq(-0.4, 0.8, by = 0.2),
    labels = function(x) sprintf("%+.1f", x)
  ) +
  labs(
    title = "Cultural Capital Disaggregation: Socialization, Institutional Capital, and Economic Capital",
    subtitle = "Posterior distributions of standardized location coefficients from Model 6 (Relaxed CS + RS)",
    x = "Posterior Log-Odds Ratio (β) [Negative = Domestic Elder | Positive = Professional Chef]",
    y = "Capital Dimension",
    caption = "Directional credibility standard: ≥ 95% posterior probability mass on either side of zero.\nEvaluates independent mechanisms of embodied socialization vs. institutional educational distinction."
  ) +
  theme_cuisine(base_size = 12)

ggsave(file.path(plot_dir, "ext_cultural_capital_mechanisms.png"), p_ext3, width = 11.5, height = 6.0, dpi = 300, bg = "white")
cat("Saved ext_cultural_capital_mechanisms.png\n")

cat("========================================================================\n")
cat("All extension visualizations generated successfully!\n")
cat("========================================================================\n")
