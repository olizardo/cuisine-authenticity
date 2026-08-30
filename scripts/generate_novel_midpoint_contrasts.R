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
# 1. Styling, Theme & Color Palette
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
      legend.box = "horizontal",
      legend.box.just = "center",
      legend.spacing.x = unit(0.4, "cm"),
      legend.margin = margin(t = 6, b = 2),
      plot.margin = margin(t = 12, r = 16, b = 12, l = 12)
    )
}

color_credibility <- c(
  "Credibly Positive (≥ 95% Mass)" = "#0072B2", # Okabe-Ito Blue
  "Credibly Negative (≥ 95% Mass)" = "#D55E00", # Okabe-Ito Vermillion
  "Spans Zero (< 95% Mass)"        = "gray60"
)

# -------------------------------------------------------------------------
# 2. Extract Category-Specific Draws Across All Relaxed Models
# -------------------------------------------------------------------------
mid_cache_file <- file.path(cache_dir, "midpoint_contrast_draws.rds")

if (file.exists(mid_cache_file)) {
  cat("Loading cached midpoint contrast draws from cache/midpoint_contrast_draws.rds...\n")
  full_mid_df <- readRDS(mid_cache_file)
} else {
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
            `1 (Elder)` = -(t_1 + t_2 + t_3),
            `2`         = -(t_2 + t_3),
            `3`         = -t_3,
            `5`         = t_4,
            `6`         = t_4 + t_5,
            `7 (Chef)`  = t_4 + t_5 + t_6
          ) %>%
          select(`1 (Elder)`, `2`, `3`, `5`, `6`, `7 (Chef)`) %>%
          pivot_longer(cols = everything(), names_to = "Category", values_to = "contrast_log_odds") %>%
          mutate(
            var_name = v,
            model_domain = row$domain,
            model_re = row$re,
            model_file = row$systematic_file,
            Category = factor(Category, levels = c("1 (Elder)", "2", "3", "5", "6", "7 (Chef)"))
          )
        all_mid_draws_list[[length(all_mid_draws_list) + 1]] <- w
      }
    }
  }

  full_mid_df <- bind_rows(all_mid_draws_list)
  saveRDS(full_mid_df, mid_cache_file)
  cat("Saved midpoint draws to cache/midpoint_contrast_draws.rds\n")
}

# -------------------------------------------------------------------------
# 3. Standard Domain-Specific Midpoint Plotting Function
# -------------------------------------------------------------------------
plot_midpoint_domain <- function(df, vars, var_labels, title, subtitle, filename, ncol = 2, width = 10.5, height = 5.2) {
  df_sub <- df %>%
    filter(var_name %in% vars) %>%
    mutate(
      Predictor = factor(var_labels[var_name], levels = unname(var_labels))
    )
  
  k_models <- n_distinct(df_sub$model_file)
  
  # Compute statistical credibility per predictor and category
  cred_df <- df_sub %>%
    group_by(Predictor, Category) %>%
    summarise(
      p_pos = mean(contrast_log_odds > 0),
      p_neg = mean(contrast_log_odds < 0),
      .groups = "drop"
    ) %>%
    mutate(
      cred_status = case_when(
        p_pos >= 0.95 ~ "Credibly Positive (≥ 95% Mass)",
        p_neg >= 0.95 ~ "Credibly Negative (≥ 95% Mass)",
        TRUE ~ "Spans Zero (< 95% Mass)"
      ),
      cred_status = factor(cred_status, levels = c(
        "Credibly Positive (≥ 95% Mass)",
        "Credibly Negative (≥ 95% Mass)",
        "Spans Zero (< 95% Mass)"
      ))
    )
  
  df_plot <- df_sub %>%
    left_join(cred_df %>% select(Predictor, Category, cred_status), by = c("Predictor", "Category"))
  
  p <- ggplot(df_plot, aes(x = Category, y = contrast_log_odds, fill = cred_status, color = cred_status)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.7) +
    stat_halfeye(
      point_interval = median_qi,
      .width = c(0.80, 0.95),
      point_size = 3.2,
      interval_size_range = c(0.75, 1.9),
      slab_alpha = 0.15,
      scale = 0.65
    ) +
    scale_fill_manual(values = color_credibility, name = "Consensus Credibility (≥ 95% Posterior Mass)") +
    scale_color_manual(values = color_credibility, name = "Consensus Credibility (≥ 95% Posterior Mass)") +
    facet_wrap(~ Predictor, ncol = ncol) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Likert Category (1 = Domestic Elder \u2190 \u2192 7 = Professional Chef; vs. Midpoint = 4)",
      y = "Contrast Log-Odds (vs. Category 4)",
      caption = sprintf("Synthesized across %d relaxed category-specific models (%s pooled post-warmup MCMC draws per variable).\nPoints indicate posterior medians; thick and thin bars represent 80%% and 95%% credible intervals; shaded slabs represent posterior densities.\nBlue = Credibly Positive (pro-chef shift); Vermillion = Credibly Negative (domestic elder shift); Gray = Spans Zero.",
                        k_models, format(nrow(df_sub) / length(vars), big.mark = ","))
    ) +
    theme_cuisine(base_size = 11) +
    theme(
      axis.text.x = element_text(size = 8.5),
      strip.text = element_text(face = "bold", size = 10, color = "gray15"),
      strip.background = element_rect(fill = "gray95", color = NA),
      panel.spacing = unit(1.0, "lines"),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(nrow = 1), color = guide_legend(nrow = 1))
  
  out_path <- file.path(plot_dir, filename)
  ggsave(out_path, p, width = width, height = height, dpi = 300, bg = "white")
  cat(sprintf("Saved: %s\n", out_path))
  return(p)
}

# -------------------------------------------------------------------------
# 4. Generate Section 7 Figures
# -------------------------------------------------------------------------

# Figure 4: Political Ideology
cat("Generating Figure 4: Ideology Midpoint Contrasts...\n")
plot_midpoint_domain(
  full_mid_df,
  vars = c("social_c", "economic_c"),
  var_labels = c("social_c" = "Social Conservatism", "economic_c" = "Economic Conservatism"),
  title = "Political Ideology: Consensus Category Shifts Relative to Neutral Midpoint",
  subtitle = "Multi-model consensus posterior distributions (half-eyes) across relaxed category-specific specifications",
  filename = "ideology_cs_midpoint_effects.png",
  ncol = 2, width = 10.5, height = 5.2
)

# Figure 5: Cultural Capital
cat("Generating Figure 5: Cultural Capital Midpoint Contrasts...\n")
plot_midpoint_domain(
  full_mid_df,
  vars = c("educ_c", "peduc_c", "arts_c"),
  var_labels = c(
    "educ_c"  = "Educational Attainment",
    "peduc_c" = "Parental Education",
    "arts_c"  = "Childhood Arts Socialization"
  ),
  title = "Cultural Capital & Socialization: Consensus Shifts Relative to Neutral Midpoint",
  subtitle = "Multi-model consensus posterior distributions (half-eyes) across relaxed category-specific specifications",
  filename = "cultural_cs_midpoint_effects.png",
  ncol = 3, width = 12.0, height = 5.2
)

# Figure 6: Dining Practices
cat("Generating Figure 6: Dining Practices Midpoint Contrasts...\n")
plot_midpoint_domain(
  full_mid_df,
  vars = c("highbrow_arts_c", "fancy_rest_c", "fast_food_c"),
  var_labels = c(
    "highbrow_arts_c" = "Adult Highbrow Arts Attendance",
    "fancy_rest_c"    = "Fine Dining Frequency",
    "fast_food_c"     = "Fast Food Frequency"
  ),
  title = "Dining & Cultural Practices: Consensus Shifts Relative to Neutral Midpoint",
  subtitle = "Multi-model consensus posterior distributions (half-eyes) across relaxed specifications (Domain + Meta)",
  filename = "practices_cs_midpoint_effects.png",
  ncol = 3, width = 12.0, height = 5.2
)

# Figure 7: Taste Dispositions
cat("Generating Figure 7: Taste Dispositions Midpoint Contrasts...\n")
plot_midpoint_domain(
  full_mid_df,
  vars = c("taste_authentic_c", "taste_familiar_c", "taste_light_c", "taste_rich_c"),
  var_labels = c(
    "taste_authentic_c" = "Dispositions: Exotic / Authentic",
    "taste_familiar_c"  = "Dispositions: Familiar Comfort",
    "taste_light_c"     = "Dispositions: Light / Fresh",
    "taste_rich_c"      = "Dispositions: Rich / Hearty"
  ),
  title = "Taste Dispositions: Consensus Shifts Relative to Neutral Midpoint",
  subtitle = "Multi-model consensus posterior distributions (half-eyes) across relaxed specifications (Domain + Meta)",
  filename = "dispositions_cs_midpoint_effects.png",
  ncol = 2, width = 10.5, height = 7.5
)

# Figure 8: Cosmopolitan Capital
cat("Generating Figure 8: Cosmopolitan Capital Midpoint Contrasts...\n")
plot_midpoint_domain(
  full_mid_df,
  vars = c("network_diversity_c", "cosmo_global_c"),
  var_labels = c(
    "network_diversity_c" = "Friendship Network Diversity",
    "cosmo_global_c"     = "Global Citizen Identity"
  ),
  title = "Cosmopolitan Capital: Consensus Shifts Relative to Neutral Midpoint",
  subtitle = "Multi-model consensus posterior distributions (half-eyes) across relaxed specifications (Domain + Meta)",
  filename = "cosmopolitan_cs_midpoint_effects.png",
  ncol = 2, width = 10.5, height = 5.2
)

cat("========================================================================\n")
cat("Consensus Half-Eye Midpoint Contrast plots successfully generated in Plots/:\n")
cat("  - ideology_cs_midpoint_effects.png (Figure 4)\n")
cat("  - cultural_cs_midpoint_effects.png (Figure 5)\n")
cat("  - practices_cs_midpoint_effects.png (Figure 6)\n")
cat("  - dispositions_cs_midpoint_effects.png (Figure 7)\n")
cat("  - cosmopolitan_cs_midpoint_effects.png (Figure 8)\n")
cat("========================================================================\n")
