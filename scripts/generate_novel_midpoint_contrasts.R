#' @title Generate Midpoint Contrast Plots for Novel Substantive Domains & Consensus Credible Variables
#' @description Computes category-specific cumulative contrast log-odds relative to the neutral
#'   Likert midpoint (Category 4) for all majority-credible variables across the 4 substantive domains
#'   (Ideology, Cultural Capital, Dining Practices, Taste Dispositions, Cosmopolitan Capital).

suppressPackageStartupMessages({
  library(brms)
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(tidybayes)
  library(here)
})

# -------------------------------------------------------------------------
# 1. Styling & Plot Theme
# -------------------------------------------------------------------------
theme_cuisine <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2, margin = margin(b = 4)),
      plot.subtitle = element_text(color = "gray30", size = base_size, margin = margin(b = 8)),
      plot.caption = element_text(color = "gray40", size = base_size - 3, hjust = 0, margin = margin(t = 8)),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray92", linewidth = 0.5),
      panel.grid.major.y = element_line(color = "gray92", linewidth = 0.5),
      axis.title = element_text(face = "bold", size = base_size - 1),
      axis.text = element_text(color = "gray20", size = base_size - 2),
      legend.title = element_text(face = "bold", size = base_size - 1),
      legend.text = element_text(size = base_size - 2),
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = base_size - 1),
      plot.margin = margin(12, 16, 12, 12)
    )
}

plot_dir <- here("Plots")
pfile <- function(name) file.path(plot_dir, paste0(name, ".png"))

# -------------------------------------------------------------------------
# 2. Midpoint Contrast Helper Function
# -------------------------------------------------------------------------
compute_midpoint_contrasts <- function(df, var_prefix, pred_name, domain_name) {
  var_col <- paste0("bcs_", var_prefix)
  w_df <- df %>%
    select(.chain, .iteration, .draw, threshold, !!sym(var_col)) %>%
    pivot_wider(names_from = threshold, values_from = !!sym(var_col), names_prefix = "t_")
  
  contrasts <- w_df %>%
    mutate(
      `Cat 1 (Elder)` = -(t_1 + t_2 + t_3),
      `Cat 2`         = -(t_2 + t_3),
      `Cat 3`         = -t_3,
      `Cat 5`         = t_4,
      `Cat 6`         = t_4 + t_5,
      `Cat 7 (Chef)`  = t_4 + t_5 + t_6
    ) %>%
    select(.chain, .iteration, .draw, starts_with("Cat")) %>%
    pivot_longer(cols = starts_with("Cat"), names_to = "Category", values_to = "contrast_log_odds") %>%
    mutate(
      Predictor = pred_name,
      Domain = domain_name,
      Category = factor(Category, levels = c("Cat 1 (Elder)", "Cat 2", "Cat 3", "Cat 5", "Cat 6", "Cat 7 (Chef)"))
    )
  return(contrasts)
}

# -------------------------------------------------------------------------
# 3. Load Models
# -------------------------------------------------------------------------
cat("Loading fitted relaxed models...\n")
m_base_rs  <- readRDS(here("cache", "hier_base_relaxed_rs.rds"))
m_prac_rs  <- readRDS(here("cache", "hier_practices_relaxed_rs.rds"))
m_disp_rs  <- readRDS(here("cache", "hier_dispositions_relaxed_rs.rds"))
m_cosmo_rs <- readRDS(here("cache", "hier_cosmopolitan_relaxed_rs.rds"))

# -------------------------------------------------------------------------
# 4. Compute Contrasts for Novel Domains
# -------------------------------------------------------------------------

# 4A. Dining Practices Domain
cat("Computing Practices Midpoint Contrasts...\n")
draws_prac_cs <- m_prac_rs %>%
  spread_draws(bcs_highbrow_arts_c[threshold], bcs_fancy_rest_c[threshold], bcs_fast_food_c[threshold])

prac_mid <- bind_rows(
  compute_midpoint_contrasts(draws_prac_cs, "highbrow_arts_c", "Adult Highbrow Arts Attendance", "Dining & Arts Practices"),
  compute_midpoint_contrasts(draws_prac_cs, "fancy_rest_c", "Fine Dining Frequency", "Dining & Arts Practices"),
  compute_midpoint_contrasts(draws_prac_cs, "fast_food_c", "Fast Food Frequency", "Dining & Arts Practices")
)

prac_colors <- c(
  "Adult Highbrow Arts Attendance" = "#7570b3",
  "Fine Dining Frequency"          = "#d95f02",
  "Fast Food Frequency"            = "#66a61e"
)

p_prac_mid <- ggplot(prac_mid, aes(x = Category, y = contrast_log_odds, color = Predictor, group = Predictor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", linewidth = 0.75) +
  stat_pointinterval(
    point_interval = median_qi,
    .width = c(0.80, 0.95),
    position = position_dodge(width = 0.5),
    point_size = 3.2,
    interval_size = 1.1
  ) +
  scale_color_manual(values = prac_colors, name = "Dining & Arts Practice") +
  labs(
    title = "Dining & Cultural Practices: Category Shifts Relative to Neutral Midpoint",
    subtitle = "Cumulative contrast log-odds of selecting given category vs. neutral midpoint (Cat 4) per 1 SD increase",
    x = "Response Scale Position (Relative to Midpoint = 4)",
    y = "Contrast Log-Odds (vs. Category 4)",
    caption = "Derived from Dining Practices Relaxed CS + RS Model (4,000 MCMC draws).\nPositive values indicate increased likelihood of choosing that category relative to the midpoint."
  ) +
  theme_cuisine(base_size = 12) +
  theme(axis.text.x = element_text(face = "bold", size = 10))

ggsave(pfile("practices_cs_midpoint_effects"), p_prac_mid, width = 10, height = 6.2, dpi = 300, bg = "white")

# 4B. Taste Dispositions Domain
cat("Computing Taste Dispositions Midpoint Contrasts...\n")
draws_disp_cs <- m_disp_rs %>%
  spread_draws(
    bcs_taste_authentic_c[threshold],
    bcs_taste_familiar_c[threshold],
    bcs_taste_light_c[threshold],
    bcs_taste_rich_c[threshold]
  )

disp_mid <- bind_rows(
  compute_midpoint_contrasts(draws_disp_cs, "taste_authentic_c", "Exotic / Authentic", "Taste Dispositions"),
  compute_midpoint_contrasts(draws_disp_cs, "taste_familiar_c", "Familiar / Comfort", "Taste Dispositions"),
  compute_midpoint_contrasts(draws_disp_cs, "taste_light_c", "Light / Fresh", "Taste Dispositions"),
  compute_midpoint_contrasts(draws_disp_cs, "taste_rich_c", "Rich / Hearty", "Taste Dispositions")
)

disp_colors <- c(
  "Exotic / Authentic" = "#D55E00",
  "Familiar / Comfort" = "#E69F00",
  "Light / Fresh"      = "#009E73",
  "Rich / Hearty"      = "#56B4E9"
)

p_disp_mid <- ggplot(disp_mid, aes(x = Category, y = contrast_log_odds, color = Predictor, group = Predictor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", linewidth = 0.75) +
  stat_pointinterval(
    point_interval = median_qi,
    .width = c(0.80, 0.95),
    position = position_dodge(width = 0.55),
    point_size = 3.2,
    interval_size = 1.1
  ) +
  scale_color_manual(values = disp_colors, name = "Taste Disposition Dimension") +
  labs(
    title = "Taste Dispositions: Category Shifts Relative to Neutral Midpoint",
    subtitle = "Cumulative contrast log-odds of selecting given category vs. neutral midpoint (Cat 4) per 1 SD increase",
    x = "Response Scale Position (Relative to Midpoint = 4)",
    y = "Contrast Log-Odds (vs. Category 4)",
    caption = "Derived from Taste Dispositions Relaxed CS + RS Model (4,000 MCMC draws).\nPositive values indicate increased likelihood of choosing that category relative to the midpoint."
  ) +
  theme_cuisine(base_size = 12) +
  theme(axis.text.x = element_text(face = "bold", size = 10))

ggsave(pfile("dispositions_cs_midpoint_effects"), p_disp_mid, width = 10.5, height = 6.2, dpi = 300, bg = "white")

# 4C. Cosmopolitan Capital Domain
cat("Computing Cosmopolitan Capital Midpoint Contrasts...\n")
draws_cosmo_cs <- m_cosmo_rs %>%
  spread_draws(bcs_network_diversity_c[threshold], bcs_cosmo_global_c[threshold])

cosmo_mid <- bind_rows(
  compute_midpoint_contrasts(draws_cosmo_cs, "network_diversity_c", "Friendship Network Diversity", "Cosmopolitan Capital"),
  compute_midpoint_contrasts(draws_cosmo_cs, "cosmo_global_c", "Global Citizen Identity", "Cosmopolitan Capital")
)

cosmo_colors <- c(
  "Friendship Network Diversity" = "#0072B2",
  "Global Citizen Identity"      = "#CC79A7"
)

p_cosmo_mid <- ggplot(cosmo_mid, aes(x = Category, y = contrast_log_odds, color = Predictor, group = Predictor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", linewidth = 0.75) +
  stat_pointinterval(
    point_interval = median_qi,
    .width = c(0.80, 0.95),
    position = position_dodge(width = 0.45),
    point_size = 3.5,
    interval_size = 1.2
  ) +
  scale_color_manual(values = cosmo_colors, name = "Cosmopolitan Dimension") +
  labs(
    title = "Cosmopolitan Capital: Category Shifts Relative to Neutral Midpoint",
    subtitle = "Cumulative contrast log-odds of selecting given category vs. neutral midpoint (Cat 4) per 1 SD increase",
    x = "Response Scale Position (Relative to Midpoint = 4)",
    y = "Contrast Log-Odds (vs. Category 4)",
    caption = "Derived from Cosmopolitan Capital Relaxed CS + RS Model (4,000 MCMC draws).\nPositive values indicate increased likelihood of choosing that category relative to the midpoint."
  ) +
  theme_cuisine(base_size = 12) +
  theme(axis.text.x = element_text(face = "bold", size = 10))

ggsave(pfile("cosmopolitan_cs_midpoint_effects"), p_cosmo_mid, width = 9.5, height = 6.2, dpi = 300, bg = "white")

# -------------------------------------------------------------------------
# 5. Consolidated Multi-Panel Consensus Credible Midpoint Contrasts Plot
# -------------------------------------------------------------------------
cat("Compiling Master Multi-Panel Consensus Credible Midpoint Contrasts...\n")

# Base domain draws
draws_base_cs <- m_base_rs %>%
  spread_draws(bcs_social_c[threshold], bcs_economic_c[threshold], bcs_educ_c[threshold], bcs_arts_c[threshold])

soc_mid    <- compute_midpoint_contrasts(draws_base_cs, "social_c", "Social Conservatism", "1. Political Ideology")
educ_mid   <- compute_midpoint_contrasts(draws_base_cs, "educ_c", "Educational Attainment", "2. Cultural Capital")
arts_mid   <- compute_midpoint_contrasts(draws_base_cs, "arts_c", "Childhood Arts Socialization", "2. Cultural Capital")
high_mid   <- compute_midpoint_contrasts(draws_prac_cs, "highbrow_arts_c", "Adult Highbrow Arts Attendance", "3. Dining & Arts Practices")
fine_mid   <- compute_midpoint_contrasts(draws_prac_cs, "fancy_rest_c", "Fine Dining Frequency", "3. Dining & Arts Practices")
auth_mid   <- compute_midpoint_contrasts(draws_disp_cs, "taste_authentic_c", "Dispositions: Exotic / Authentic", "4. Taste Dispositions")
net_mid    <- compute_midpoint_contrasts(draws_cosmo_cs, "network_diversity_c", "Friendship Network Diversity", "5. Cosmopolitan Capital")

master_credible_mid <- bind_rows(
  soc_mid, educ_mid, arts_mid, high_mid, fine_mid, auth_mid, net_mid
) %>%
  mutate(
    Direction_Type = case_when(
      Predictor %in% c("Childhood Arts Socialization", "Dispositions: Exotic / Authentic") ~ "Elder Authenticity Anchor",
      TRUE ~ "Professional Chef Anchor"
    ),
    Predictor_F = factor(Predictor, levels = c(
      "Social Conservatism",
      "Educational Attainment",
      "Childhood Arts Socialization",
      "Adult Highbrow Arts Attendance",
      "Fine Dining Frequency",
      "Dispositions: Exotic / Authentic",
      "Friendship Network Diversity"
    ))
  )

p_master_mid <- ggplot(master_credible_mid, aes(x = Category, y = contrast_log_odds, color = Direction_Type, group = Predictor_F)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", linewidth = 0.6) +
  stat_pointinterval(
    point_interval = median_qi,
    .width = c(0.80, 0.95),
    point_size = 2.6,
    interval_size = 0.95
  ) +
  scale_color_manual(
    values = c(
      "Professional Chef Anchor" = "#0072B2",
      "Elder Authenticity Anchor" = "#D55E00"
    ),
    name = "Consensus Theoretical Orientation"
  ) +
  scale_y_continuous(
    breaks = seq(-0.8, 1.6, by = 0.4)
  ) +
  coord_cartesian(ylim = c(-0.9, 1.75)) +
  facet_wrap(~ Predictor_F, ncol = 3) +
  labs(
    title = "Consensus Credible Predictors: Cumulative Likert Category Shifts vs. Midpoint (Common Scale)",
    subtitle = "Category-specific contrast log-odds relative to Category 4 (Neutral Midpoint) per 1 SD increase across focal mechanisms",
    x = "Likert Response Scale Category (Elder Tradition 1 \u2190 \u2192 Chef Restaurant 7)",
    y = "Contrast Log-Odds vs. Category 4 (Shared Scale across all panels)",
    caption = "Derived from optimal Relaxed Category-Specific + Random Slopes models (4,000 MCMC draws each).\nAll panels share a standardized vertical scale [-0.90, +1.75] to enable direct cross-domain effect size comparisons.\nBlue indicates pro-chef escalation across categories 5–7; Vermillion indicates domestic elder tradition escalation across categories 1–3."
  ) +
  theme_cuisine(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, size = 8.5),
    strip.text = element_text(face = "bold", size = 9.5),
    legend.position = "bottom"
  )

ggsave(pfile("consensus_credible_midpoint_contrasts"), p_master_mid, width = 11.5, height = 8.5, dpi = 300, bg = "white")

cat("========================================================================\n")
cat("Novel midpoint contrast plots successfully generated and saved to Plots/:\n")
cat("  - practices_cs_midpoint_effects.png\n")
cat("  - dispositions_cs_midpoint_effects.png\n")
cat("  - cosmopolitan_cs_midpoint_effects.png\n")
cat("  - consensus_credible_midpoint_contrasts.png\n")
cat("========================================================================\n")
