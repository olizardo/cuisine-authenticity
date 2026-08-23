#' @title Generate Consensus Midpoint Contrast Plots with Half-Eye Distributions
#' @description Computes category-specific cumulative contrast log-odds relative to the neutral
#'   Likert midpoint (Category 4) using multi-model pooled posterior draws across all relaxed
#'   category-specific specifications (10 models in the factorial taxonomy + meta models).
#'   Renders publication-grade half-eye posterior distributions (ggdist::stat_halfeye) for all substantive domains:
#'   - Figure 4: Plots/ideology_cs_midpoint_effects.png (Ideology)
#'   - Figure 5: Plots/cultural_cs_midpoint_effects.png (Cultural Capital)
#'   - Figure 6: Plots/practices_cs_midpoint_effects.png (Dining Practices)
#'   - Figure 7: Plots/dispositions_cs_midpoint_effects.png (Taste Dispositions)
#'   - Figure 8: Plots/cosmopolitan_cs_midpoint_effects.png (Cosmopolitan Capital)
#'   - Master Synthesis: Plots/consensus_credible_midpoint_contrasts.png (7 Consensus Credible Variables on Common Scale)

suppressPackageStartupMessages({
  library(brms)
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(tidybayes)
  library(ggdist)
  library(here)
})

source(here("scripts", "model_registry.R"))

plot_dir <- here("Plots")
cache_dir <- here("cache")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

pfile <- function(name) file.path(plot_dir, paste0(name, ".png"))

# -------------------------------------------------------------------------
# 1. Styling & Plot Theme
# -------------------------------------------------------------------------
theme_cuisine <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size * 1.15, margin = margin(b = 5)),
      plot.subtitle = element_text(size = base_size * 0.88, color = "gray25", margin = margin(b = 10)),
      plot.caption = element_text(size = base_size * 0.72, color = "gray40", hjust = 0, margin = margin(t = 10)),
      axis.title.x = element_text(face = "bold", size = base_size * 0.95, margin = margin(t = 8)),
      axis.title.y = element_text(face = "bold", size = base_size * 0.95, margin = margin(r = 8)),
      axis.text.y = element_text(size = base_size * 0.85),
      axis.text.x = element_text(face = "bold", size = base_size * 0.88, color = "gray15"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray92", linewidth = 0.4),
      panel.grid.major.x = element_line(color = "gray92", linewidth = 0.4),
      strip.text = element_text(face = "bold", size = base_size * 0.92, color = "gray15"),
      strip.background = element_rect(fill = "gray95", color = NA),
      panel.spacing = unit(0.9, "lines"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = base_size * 0.85),
      legend.text = element_text(size = base_size * 0.8),
      plot.margin = margin(t = 12, r = 16, b = 12, l = 12)
    )
}

# -------------------------------------------------------------------------
# 2. Extract Category-Specific Draws Across All Relaxed Models
# -------------------------------------------------------------------------
cat("=== Extracting Midpoint Contrast Draws Across All Relaxed Models ===\n")

relaxed_models <- MODEL_REGISTRY %>% filter(threshold == "relaxed")

all_mid_draws_list <- list()

for (i in seq_len(nrow(relaxed_models))) {
  row <- relaxed_models[i, ]
  m_path <- file.path(cache_dir, row$systematic_file)
  if (!file.exists(m_path)) {
    m_path <- file.path(cache_dir, row$legacy_file)
  }
  if (!file.exists(m_path)) next
  
  cat(sprintf("Loading [%s | %s]: %s...\n", row$domain, row$re, basename(m_path)))
  m <- readRDS(m_path)
  
  vars <- grep("^bcs_", variables(m), value = TRUE)
  clean_vars <- unique(gsub("\\[[0-9]+\\]", "", gsub("^bcs_", "", vars)))
  
  dr <- as_draws_df(m)
  
  for (v in clean_vars) {
    col_names <- paste0("bcs_", v, "[", 1:6, "]")
    if (all(col_names %in% variables(m))) {
      w <- dr %>%
        as_tibble() %>%
        select(all_of(col_names)) %>%
        rename_with(~ paste0("t_", 1:6), all_of(col_names)) %>%
        mutate(
          `Cat 1 (Elder)` = -(t_1 + t_2 + t_3),
          `Cat 2`         = -(t_2 + t_3),
          `Cat 3`         = -t_3,
          `Cat 5`         = t_4,
          `Cat 6`         = t_4 + t_5,
          `Cat 7 (Chef)`  = t_4 + t_5 + t_6
        ) %>%
        select(starts_with("Cat")) %>%
        pivot_longer(cols = everything(), names_to = "Category", values_to = "contrast_log_odds") %>%
        mutate(
          var_name = v,
          model_domain = row$domain,
          model_re = row$re,
          model_file = row$systematic_file,
          Category = factor(Category, levels = c("Cat 1 (Elder)", "Cat 2", "Cat 3", "Cat 5", "Cat 6", "Cat 7 (Chef)"))
        )
      all_mid_draws_list[[length(all_mid_draws_list) + 1]] <- w
    }
  }
}

full_mid_df <- bind_rows(all_mid_draws_list)
cat(sprintf("Total extracted midpoint contrast draws: %d rows across %d relaxed models\n\n", 
            nrow(full_mid_df), n_distinct(full_mid_df$model_file)))

# -------------------------------------------------------------------------
# 3. Domain-Specific Plots with Multi-Model Consensus Half-Eyes
# -------------------------------------------------------------------------

# 3A. Figure 4: Political Ideology
cat("Generating Figure 4: Ideology Midpoint Contrasts (Consensus Half-Eyes)...\n")
df_ideo <- full_mid_df %>%
  filter(var_name %in% c("social_c", "economic_c")) %>%
  mutate(
    Predictor = factor(
      ifelse(var_name == "social_c", "Social Conservatism", "Economic Conservatism"),
      levels = c("Social Conservatism", "Economic Conservatism")
    )
  )

k_ideo_models <- n_distinct(df_ideo$model_file)

p_ideo <- ggplot(df_ideo, aes(x = Category, y = contrast_log_odds, fill = Predictor, color = Predictor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", linewidth = 0.75) +
  stat_halfeye(
    position = position_dodge(width = 0.65),
    alpha = 0.60,
    .width = c(0.80, 0.95),
    point_interval = median_qi,
    scale = 0.45,
    slab_linewidth = 0.35,
    point_size = 2.4,
    interval_size = 1.0
  ) +
  scale_fill_manual(values = c("Social Conservatism" = "#0072B2", "Economic Conservatism" = "#D55E00"), name = "Ideological Dimension") +
  scale_color_manual(values = c("Social Conservatism" = "#0072B2", "Economic Conservatism" = "#D55E00"), name = "Ideological Dimension") +
  labs(
    title = "Political Ideology: Consensus Category Shifts Relative to Neutral Midpoint",
    subtitle = sprintf("Multi-model consensus posterior distributions (half-eyes) pooled across %d relaxed category-specific specifications", k_ideo_models),
    x = "Likert Response Scale Position (Relative to Midpoint = 4)",
    y = "Contrast Log-Odds (vs. Category 4)",
    caption = sprintf("Synthesized across %d relaxed category-specific models (%s pooled post-warmup MCMC draws).\nThick bars: 80%% CrI; thin lines: 95%% CrI; dots: posterior medians; shaded slabs: empirical posterior densities.",
                      k_ideo_models, format(nrow(df_ideo) / 2, big.mark = ","))
  ) +
  theme_cuisine(base_size = 11)

ggsave(pfile("ideology_cs_midpoint_effects"), p_ideo, width = 10, height = 6.2, dpi = 300, bg = "white")


# 3B. Figure 5: Cultural Capital & Socialization
cat("Generating Figure 5: Cultural Capital Midpoint Contrasts (Consensus Half-Eyes)...\n")
df_cult <- full_mid_df %>%
  filter(var_name %in% c("educ_c", "peduc_c", "arts_c")) %>%
  mutate(
    Predictor = factor(
      case_when(
        var_name == "educ_c"  ~ "Educational Attainment",
        var_name == "peduc_c" ~ "Parental Education",
        var_name == "arts_c"  ~ "Childhood Arts Socialization"
      ),
      levels = c("Educational Attainment", "Parental Education", "Childhood Arts Socialization")
    )
  )

k_cult_models <- n_distinct(df_cult$model_file)

cult_colors <- c(
  "Educational Attainment"        = "#0072B2", # Okabe-Ito Blue (Chef)
  "Parental Education"           = "#E69F00", # Yellow-orange
  "Childhood Arts Socialization" = "#D55E00"  # Vermillion (Elder)
)

p_cult <- ggplot(df_cult, aes(x = Category, y = contrast_log_odds, fill = Predictor, color = Predictor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", linewidth = 0.75) +
  stat_halfeye(
    position = position_dodge(width = 0.70),
    alpha = 0.60,
    .width = c(0.80, 0.95),
    point_interval = median_qi,
    scale = 0.45,
    slab_linewidth = 0.35,
    point_size = 2.3,
    interval_size = 1.0
  ) +
  scale_fill_manual(values = cult_colors, name = "Cultural Capital Dimension") +
  scale_color_manual(values = cult_colors, name = "Cultural Capital Dimension") +
  labs(
    title = "Cultural Capital & Socialization: Consensus Shifts Relative to Neutral Midpoint",
    subtitle = sprintf("Multi-model consensus posterior distributions (half-eyes) pooled across %d relaxed category-specific specifications", k_cult_models),
    x = "Likert Response Scale Position (Relative to Midpoint = 4)",
    y = "Contrast Log-Odds (vs. Category 4)",
    caption = sprintf("Synthesized across %d relaxed category-specific models (%s pooled post-warmup MCMC draws).\nThick bars: 80%% CrI; thin lines: 95%% CrI; dots: posterior medians; shaded slabs: empirical posterior densities.",
                      k_cult_models, format(nrow(df_cult) / 3, big.mark = ","))
  ) +
  theme_cuisine(base_size = 11)

ggsave(pfile("cultural_cs_midpoint_effects"), p_cult, width = 10.5, height = 6.2, dpi = 300, bg = "white")


# 3C. Figure 6: Dining Practices
cat("Generating Figure 6: Dining Practices Midpoint Contrasts (Consensus Half-Eyes)...\n")
df_prac <- full_mid_df %>%
  filter(var_name %in% c("highbrow_arts_c", "fancy_rest_c", "fast_food_c")) %>%
  mutate(
    Predictor = factor(
      case_when(
        var_name == "highbrow_arts_c" ~ "Adult Highbrow Arts Attendance",
        var_name == "fancy_rest_c"    ~ "Fine Dining Frequency",
        var_name == "fast_food_c"     ~ "Fast Food Frequency"
      ),
      levels = c("Adult Highbrow Arts Attendance", "Fine Dining Frequency", "Fast Food Frequency")
    )
  )

k_prac_models <- n_distinct(df_prac$model_file)

prac_colors <- c(
  "Adult Highbrow Arts Attendance" = "#7570b3",
  "Fine Dining Frequency"          = "#d95f02",
  "Fast Food Frequency"            = "#66a61e"
)

p_prac <- ggplot(df_prac, aes(x = Category, y = contrast_log_odds, fill = Predictor, color = Predictor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", linewidth = 0.75) +
  stat_halfeye(
    position = position_dodge(width = 0.70),
    alpha = 0.60,
    .width = c(0.80, 0.95),
    point_interval = median_qi,
    scale = 0.45,
    slab_linewidth = 0.35,
    point_size = 2.3,
    interval_size = 1.0
  ) +
  scale_fill_manual(values = prac_colors, name = "Dining & Arts Practice") +
  scale_color_manual(values = prac_colors, name = "Dining & Arts Practice") +
  labs(
    title = "Dining & Cultural Practices: Consensus Shifts Relative to Neutral Midpoint",
    subtitle = sprintf("Multi-model consensus posterior distributions (half-eyes) pooled across %d relaxed specifications (Domain + Meta)", k_prac_models),
    x = "Likert Response Scale Position (Relative to Midpoint = 4)",
    y = "Contrast Log-Odds (vs. Category 4)",
    caption = sprintf("Synthesized across %d relaxed models containing dining practices (%s pooled post-warmup MCMC draws).\nThick bars: 80%% CrI; thin lines: 95%% CrI; dots: posterior medians; shaded slabs: empirical posterior densities.",
                      k_prac_models, format(nrow(df_prac) / 3, big.mark = ","))
  ) +
  theme_cuisine(base_size = 11)

ggsave(pfile("practices_cs_midpoint_effects"), p_prac, width = 10.5, height = 6.2, dpi = 300, bg = "white")


# 3D. Figure 7: Taste Dispositions
cat("Generating Figure 7: Taste Dispositions Midpoint Contrasts (Consensus Half-Eyes)...\n")
df_disp <- full_mid_df %>%
  filter(var_name %in% c("taste_authentic_c", "taste_familiar_c", "taste_light_c", "taste_rich_c")) %>%
  mutate(
    Predictor = factor(
      case_when(
        var_name == "taste_authentic_c" ~ "Exotic / Authentic",
        var_name == "taste_familiar_c"  ~ "Familiar / Comfort",
        var_name == "taste_light_c"     ~ "Light / Fresh",
        var_name == "taste_rich_c"      ~ "Rich / Hearty"
      ),
      levels = c("Exotic / Authentic", "Familiar / Comfort", "Light / Fresh", "Rich / Hearty")
    )
  )

k_disp_models <- n_distinct(df_disp$model_file)

disp_colors <- c(
  "Exotic / Authentic" = "#D55E00",
  "Familiar / Comfort" = "#E69F00",
  "Light / Fresh"      = "#009E73",
  "Rich / Hearty"      = "#56B4E9"
)

p_disp <- ggplot(df_disp, aes(x = Category, y = contrast_log_odds, fill = Predictor, color = Predictor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", linewidth = 0.75) +
  stat_halfeye(
    position = position_dodge(width = 0.75),
    alpha = 0.60,
    .width = c(0.80, 0.95),
    point_interval = median_qi,
    scale = 0.45,
    slab_linewidth = 0.35,
    point_size = 2.2,
    interval_size = 0.95
  ) +
  scale_fill_manual(values = disp_colors, name = "Taste Disposition Dimension") +
  scale_color_manual(values = disp_colors, name = "Taste Disposition Dimension") +
  labs(
    title = "Taste Dispositions: Consensus Shifts Relative to Neutral Midpoint",
    subtitle = sprintf("Multi-model consensus posterior distributions (half-eyes) pooled across %d relaxed specifications (Domain + Meta)", k_disp_models),
    x = "Likert Response Scale Position (Relative to Midpoint = 4)",
    y = "Contrast Log-Odds (vs. Category 4)",
    caption = sprintf("Synthesized across %d relaxed models containing taste dispositions (%s pooled post-warmup MCMC draws).\nThick bars: 80%% CrI; thin lines: 95%% CrI; dots: posterior medians; shaded slabs: empirical posterior densities.",
                      k_disp_models, format(nrow(df_disp) / 4, big.mark = ","))
  ) +
  theme_cuisine(base_size = 11)

ggsave(pfile("dispositions_cs_midpoint_effects"), p_disp, width = 11, height = 6.2, dpi = 300, bg = "white")


# 3E. Figure 8: Cosmopolitan Capital
cat("Generating Figure 8: Cosmopolitan Capital Midpoint Contrasts (Consensus Half-Eyes)...\n")
df_cosmo <- full_mid_df %>%
  filter(var_name %in% c("network_diversity_c", "cosmo_global_c")) %>%
  mutate(
    Predictor = factor(
      case_when(
        var_name == "network_diversity_c" ~ "Friendship Network Diversity",
        var_name == "cosmo_global_c"     ~ "Global Citizen Identity"
      ),
      levels = c("Friendship Network Diversity", "Global Citizen Identity")
    )
  )

k_cosmo_models <- n_distinct(df_cosmo$model_file)

cosmo_colors <- c(
  "Friendship Network Diversity" = "#0072B2",
  "Global Citizen Identity"      = "#CC79A7"
)

p_cosmo <- ggplot(df_cosmo, aes(x = Category, y = contrast_log_odds, fill = Predictor, color = Predictor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", linewidth = 0.75) +
  stat_halfeye(
    position = position_dodge(width = 0.65),
    alpha = 0.60,
    .width = c(0.80, 0.95),
    point_interval = median_qi,
    scale = 0.45,
    slab_linewidth = 0.35,
    point_size = 2.4,
    interval_size = 1.0
  ) +
  scale_fill_manual(values = cosmo_colors, name = "Cosmopolitan Dimension") +
  scale_color_manual(values = cosmo_colors, name = "Cosmopolitan Dimension") +
  labs(
    title = "Cosmopolitan Capital: Consensus Shifts Relative to Neutral Midpoint",
    subtitle = sprintf("Multi-model consensus posterior distributions (half-eyes) pooled across %d relaxed specifications (Domain + Meta)", k_cosmo_models),
    x = "Likert Response Scale Position (Relative to Midpoint = 4)",
    y = "Contrast Log-Odds (vs. Category 4)",
    caption = sprintf("Synthesized across %d relaxed models containing cosmopolitan capital (%s pooled post-warmup MCMC draws).\nThick bars: 80%% CrI; thin lines: 95%% CrI; dots: posterior medians; shaded slabs: empirical posterior densities.",
                      k_cosmo_models, format(nrow(df_cosmo) / 2, big.mark = ","))
  ) +
  theme_cuisine(base_size = 11)

ggsave(pfile("cosmopolitan_cs_midpoint_effects"), p_cosmo, width = 10, height = 6.2, dpi = 300, bg = "white")


# -------------------------------------------------------------------------
# 4. Master Multi-Panel Consensus Credible Midpoint Contrasts (Common Scale)
# -------------------------------------------------------------------------
cat("Generating Master Multi-Panel Consensus Credible Midpoint Contrasts (Consensus Half-Eyes)...\n")

df_master <- full_mid_df %>%
  filter(var_name %in% c(
    "social_c", "educ_c", "arts_c", 
    "highbrow_arts_c", "fancy_rest_c", 
    "taste_authentic_c", "network_diversity_c"
  )) %>%
  mutate(
    Predictor = case_when(
      var_name == "social_c"            ~ "Social Conservatism",
      var_name == "educ_c"              ~ "Educational Attainment",
      var_name == "arts_c"              ~ "Childhood Arts Socialization",
      var_name == "highbrow_arts_c"     ~ "Adult Highbrow Arts Attendance",
      var_name == "fancy_rest_c"        ~ "Fine Dining Frequency",
      var_name == "taste_authentic_c"   ~ "Dispositions: Exotic / Authentic",
      var_name == "network_diversity_c" ~ "Friendship Network Diversity"
    ),
    Direction_Type = case_when(
      var_name %in% c("arts_c", "taste_authentic_c") ~ "Elder Authenticity Anchor",
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
    )),
    Direction_Type = factor(Direction_Type, levels = c("Professional Chef Anchor", "Elder Authenticity Anchor"))
  )

p_master <- ggplot(df_master, aes(x = Category, y = contrast_log_odds, fill = Direction_Type, color = Direction_Type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", linewidth = 0.6) +
  stat_halfeye(
    alpha = 0.60,
    .width = c(0.80, 0.95),
    point_interval = median_qi,
    scale = 0.65,
    slab_linewidth = 0.35,
    point_size = 2.2,
    interval_size = 0.95
  ) +
  scale_fill_manual(
    values = c(
      "Professional Chef Anchor" = "#0072B2",
      "Elder Authenticity Anchor" = "#D55E00"
    ),
    name = "Consensus Theoretical Orientation"
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
    title = "Consensus Credible Predictors: Category Shifts Relative to Neutral Midpoint (Common Scale)",
    subtitle = "Multi-model consensus posterior distributions (half-eyes) across all relaxed category-specific models in the factorial taxonomy + meta models",
    x = "Likert Response Scale Category (Elder Tradition 1 \u2190 \u2192 Chef Restaurant 7)",
    y = "Contrast Log-Odds vs. Category 4 (Shared Scale across all panels)",
    caption = "Synthesized across all relevant relaxed category-specific models (16,000 to 40,000 pooled post-warmup MCMC draws per variable).\nAll panels share a standardized vertical scale [-0.90, +1.75] to enable direct cross-domain effect size comparisons.\nBlue indicates pro-chef escalation across categories 5–7; Vermillion indicates domestic elder tradition escalation across categories 1–3."
  ) +
  theme_cuisine(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, size = 8.5),
    strip.text = element_text(face = "bold", size = 9.5),
    legend.position = "bottom"
  )

ggsave(pfile("consensus_credible_midpoint_contrasts"), p_master, width = 11.5, height = 8.5, dpi = 300, bg = "white")

cat("========================================================================\n")
cat("Consensus Half-Eye Midpoint Contrast plots successfully generated in Plots/:\n")
cat("  - ideology_cs_midpoint_effects.png (Figure 4)\n")
cat("  - cultural_cs_midpoint_effects.png (Figure 5)\n")
cat("  - practices_cs_midpoint_effects.png (Figure 6)\n")
cat("  - dispositions_cs_midpoint_effects.png (Figure 7)\n")
cat("  - cosmopolitan_cs_midpoint_effects.png (Figure 8)\n")
cat("  - consensus_credible_midpoint_contrasts.png (Master Synthesis)\n")
cat("========================================================================\n")
