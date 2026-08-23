#!/usr/bin/env Rscript
#' @title Generate Publication-Ready Bayesian Visualizations for Cuisine Authenticity
#' @description Extracts posterior distributions from fitted hierarchical ordinal Bayesian models
#'   (brms ACAT models) and generates publication-grade visualizations following the
#'   analytical visual standards of Childress & Lizardo (2024).
#' @details Evaluates directional credibility based on ≥ 95% posterior probability mass
#'   on either side of zero: P(theta > 0 | Data) >= 0.95 or P(theta < 0 | Data) >= 0.95.
#'   Generates:
#'   1. Plots/model_fit_comparison.png (WAIC Model Fit Comparison)
#'   2. Plots/demographic_fixed_effects.png (Global Fixed Effects on Authenticity Location - Model 6)
#'   3. Plots/cuisine_random_effects.png (Baseline Cuisine Random Intercepts by 95% Mass Status - Model 5)
#'   4. Plots/cuisine_2d_consensus.png (2D Location vs. Consensus/Discrimination Space - Model 5)
#'   5. Plots/demographic_variance_effects_forest.png (Demographic Effects on Agreement/Consensus - Model 5)
#'   6. Plots/rs_cuisine_slopes_ideology.png (Cuisine-Specific Random Slopes for Ideology - Model 5)
#'   7. Plots/rs_cuisine_slopes_cultural.png (Cuisine-Specific Random Slopes for Cultural Capital - Model 5)
#'   8. Plots/rs_variance_ideology.png (Cuisine-Specific Random Slopes on Consensus for Ideology - Model 5)
#'   9. Plots/rs_variance_cultural.png (Cuisine-Specific Random Slopes on Consensus for Cultural Capital - Model 5)
#'   10. Plots/ideology_cs_effects.png (Threshold-Specific Shifts for Political Ideology - Model 2)
#'   11. Plots/ideology_cs_midpoint_effects.png (Midpoint Contrast Shifts for Political Ideology - Model 2)
#'   12. Plots/cultural_cs_effects.png (Threshold-Specific Shifts for Cultural Capital - Model 2)
#'   13. Plots/cultural_cs_midpoint_effects.png (Midpoint Contrast Shifts for Cultural Capital - Model 2)

suppressPackageStartupMessages({
  library(tidyverse)
  library(brms)
  library(tidybayes)
  library(ggdist)
  library(ggrepel)
  library(here)
})

cat("========================================================================\n")
cat("Generating Publication-Ready Figures: Cuisine Authenticity\n")
cat("Directional Credibility Criterion: >= 95% Posterior Probability Mass\n")
cat("========================================================================\n")

# Ensure output directory exists
plot_dir <- here("Plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

cfile <- function(m) here("cache", paste0("hier_", m, ".rds"))
pfile <- function(p) file.path(plot_dir, paste0(p, ".png"))

#' Custom Minimalist Theme matching Childress & Lizardo Visual Guidelines
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

# Color palettes (Okabe-Ito standards)
color_credibility <- c(
  "Credibly Pro Chef (≥ 95% Mass)"            = "#0072B2", # Blue
  "Credibly Domestic Elder (≥ 95% Mass)"      = "#D55E00", # Vermillion
  "Credibly Positive (≥ 95% Mass)"            = "#0072B2",
  "Credibly Negative (≥ 95% Mass)"            = "#D55E00",
  "Credibly Increases Consensus (≥ 95% Mass)" = "#0072B2",
  "Credibly Decreases Consensus (≥ 95% Mass)" = "#D55E00",
  "Spans Zero (< 95% Mass)"                   = "gray60",
  "Not Credible (< 95% Mass)"                 = "gray60"
)

# -------------------------------------------------------------
# 1. Load Fitted Models
# -------------------------------------------------------------
cat("Loading cached models...\n")
load_model <- function(m_name) {
  path <- cfile(m_name)
  if (file.exists(path)) {
    cat(sprintf("  - Found cached model: %s\n", m_name))
    return(tryCatch(readRDS(path), error = function(e) NULL))
  }
  cat(sprintf("  - Model not found: %s\n", m_name))
  return(NULL)
}

m1 <- load_model("1_baseline")
m2 <- load_model("2_relaxed")
m3 <- load_model("3_rs")
m4 <- load_model("4_var")
m5 <- load_model("5_var_rs")
m6 <- load_model("6_relaxed_rs")

# -------------------------------------------------------------
# 2. WAIC Model Fit Comparison
# -------------------------------------------------------------
cat("\n1. Generating WAIC Model Fit Comparison...\n")
models_to_check <- list(
  "Model 1: Baseline Strict"               = m1,
  "Model 2: Relaxed CS"                    = m2,
  "Model 3: Random Slopes Strict"          = m3,
  "Model 4: Variance Strict"               = m4,
  "Model 5: Variance + Random Slopes"      = m5,
  "Model 6: Relaxed CS + Random Slopes"    = m6
)

waic_list <- list()
for (n in names(models_to_check)) {
  m <- models_to_check[[n]]
  if (!is.null(m) && !is.null(m$criteria$waic)) {
    w <- m$criteria$waic$estimates["waic", ]
    waic_list[[n]] <- data.frame(Model = n, WAIC = w["Estimate"], SE = w["SE"])
  }
}

if (length(waic_list) > 0) {
  waic_df <- bind_rows(waic_list) %>%
    mutate(
      Delta_WAIC = WAIC - min(WAIC),
      Rank = rank(WAIC),
      IsBest = ifelse(Rank == 1, "Best Fitting Model", "Alternative Models")
    ) %>%
    arrange(WAIC)
  
  saveRDS(waic_df, here("cache", "cuisine_waic_comparison.rds"))
  
  p_waic <- ggplot(waic_df, aes(x = reorder(Model, -WAIC), y = WAIC, color = IsBest)) +
    geom_pointrange(aes(ymin = WAIC - SE, ymax = WAIC + SE), linewidth = 0.9, size = 0.8) +
    geom_text(aes(label = sprintf("Δ %.1f", Delta_WAIC)), hjust = -0.3, vjust = -0.5, size = 3.5, fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = c("Best Fitting Model" = "#0072B2", "Alternative Models" = "gray45"), name = NULL) +
    coord_flip() +
    labs(
      title = "Bayesian Model Fit Comparison (WAIC)",
      subtitle = "Information criteria comparison across six hierarchical ordinal specifications (N = 18,180 ratings across 1,212 respondents)",
      x = NULL,
      y = "Watanabe-Akaike Information Criterion (WAIC ± 1 SE; Lower is Better)",
      caption = "WAIC computed across 4,000 post-warmup MCMC draws. Points represent WAIC estimates; error bars indicate ± 1 standard error.\nΔ values reflect difference in WAIC relative to the top-performing model (Model 5)."
    ) +
    theme_cuisine(base_size = 12) +
    theme(
      axis.text.y = element_text(face = "bold", size = 10),
      legend.position = "top",
      legend.justification = "left"
    )
  
  ggsave(pfile("model_fit_comparison"), p_waic, width = 10, height = 5.5, dpi = 300, bg = "white")
}

# -------------------------------------------------------------
# 3. Global Demographic Fixed Effects (Model 6: Relaxed CS + Random Slopes)
# -------------------------------------------------------------
if (!is.null(m6)) {
  cat("2. Generating Demographic Fixed Effects Plot (Model 6: Relaxed CS + Random Slopes with Credibility Estimates)...\n")
  
  non_cs_vars <- c("b_income_c", "b_age_c", "b_gend.fWoman", "b_gend.fNonbinaryDOther",
                   "b_race.fAsian", "b_race.fBlack", "b_race.fHispanic", 
                   "b_race.fMixedOther", "b_race.fMixedWhite")
  non_cs_vars <- intersect(non_cs_vars, variables(m6))
  
  draws_non_cs <- m6 %>%
    gather_draws(!!!syms(non_cs_vars)) %>%
    mutate(
      Predictor = case_when(
        .variable == "b_income_c" ~ "Household Income",
        .variable == "b_age_c" ~ "Age",
        .variable == "b_gend.fWoman" ~ "Woman",
        .variable == "b_gend.fNonbinaryDOther" ~ "Nonbinary / Other Gender",
        .variable == "b_race.fAsian" ~ "Asian",
        .variable == "b_race.fBlack" ~ "Black",
        .variable == "b_race.fHispanic" ~ "Hispanic",
        .variable == "b_race.fMixedOther" ~ "Mixed / Other Race",
        .variable == "b_race.fMixedWhite" ~ "Mixed White"
      )
    )
  
  # Category-specific predictors: average across 6 thresholds
  cs_draws_list <- list(
    m6 %>% spread_draws(bcs_social_c[threshold]) %>% group_by(.chain, .iteration, .draw) %>% 
      summarize(.value = mean(bcs_social_c), .groups = "drop") %>% mutate(Predictor = "Social Conservatism"),
    m6 %>% spread_draws(bcs_economic_c[threshold]) %>% group_by(.chain, .iteration, .draw) %>% 
      summarize(.value = mean(bcs_economic_c), .groups = "drop") %>% mutate(Predictor = "Economic Conservatism"),
    m6 %>% spread_draws(bcs_educ_c[threshold]) %>% group_by(.chain, .iteration, .draw) %>% 
      summarize(.value = mean(bcs_educ_c), .groups = "drop") %>% mutate(Predictor = "Education"),
    m6 %>% spread_draws(bcs_peduc_c[threshold]) %>% group_by(.chain, .iteration, .draw) %>% 
      summarize(.value = mean(bcs_peduc_c), .groups = "drop") %>% mutate(Predictor = "Parental Education"),
    m6 %>% spread_draws(bcs_arts_c[threshold]) %>% group_by(.chain, .iteration, .draw) %>% 
      summarize(.value = mean(bcs_arts_c), .groups = "drop") %>% mutate(Predictor = "Childhood Arts Exposure")
  )
  
  all_draws_fixed <- bind_rows(draws_non_cs, bind_rows(cs_draws_list))
  
  summary_fixed <- all_draws_fixed %>%
    group_by(Predictor) %>%
    summarize(
      median = median(.value),
      q2.5 = quantile(.value, 0.025),
      q97.5 = quantile(.value, 0.975),
      p_pos = mean(.value > 0),
      p_neg = mean(.value < 0),
      p_dir = max(p_pos, p_neg),
      cred_status = case_when(
        p_pos >= 0.95 ~ "Credibly Pro Chef (≥ 95% Mass)",
        p_neg >= 0.95 ~ "Credibly Domestic Elder (≥ 95% Mass)",
        TRUE ~ "Spans Zero (< 95% Mass)"
      ),
      cred_label = case_when(
        p_pos >= 0.95 ~ sprintf("%+.2f [%+.2f, %+.2f]  |  P(β > 0) = %.1f%% (Credible)", median, q2.5, q97.5, p_pos * 100),
        p_neg >= 0.95 ~ sprintf("%+.2f [%+.2f, %+.2f]  |  P(β < 0) = %.1f%% (Credible)", median, q2.5, q97.5, p_neg * 100),
        TRUE ~ sprintf("%+.2f [%+.2f, %+.2f]  |  P(β %s 0) = %.1f%% (Uncertain)", median, q2.5, q97.5, ifelse(median > 0, ">", "<"), p_dir * 100)
      ),
      .groups = "drop"
    )
  
  pred_order <- summary_fixed %>% arrange(median) %>% pull(Predictor)
  
  all_draws_plot <- all_draws_fixed %>%
    left_join(summary_fixed %>% select(Predictor, cred_status), by = "Predictor") %>%
    mutate(
      Predictor = factor(Predictor, levels = pred_order),
      cred_status = factor(cred_status, levels = c(
        "Credibly Pro Chef (≥ 95% Mass)",
        "Credibly Domestic Elder (≥ 95% Mass)",
        "Spans Zero (< 95% Mass)"
      ))
    )
  
  summary_fixed <- summary_fixed %>%
    mutate(
      Predictor = factor(Predictor, levels = pred_order),
      cred_status = factor(cred_status, levels = c(
        "Credibly Pro Chef (≥ 95% Mass)",
        "Credibly Domestic Elder (≥ 95% Mass)",
        "Spans Zero (< 95% Mass)"
      ))
    )
  
  p_fixed <- ggplot(all_draws_plot, aes(x = .value, y = Predictor, fill = cred_status, color = cred_status)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.75) +
    stat_halfeye(
      point_interval = median_qi,
      .width = c(0.80, 0.95),
      point_size = 3.5,
      interval_size = 1.2,
      slab_alpha = 0.35,
      scale = 0.68
    ) +
    geom_text(
      data = summary_fixed,
      aes(x = 0.25, y = Predictor, label = cred_label),
      hjust = 0,
      size = 3.2,
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_fill_manual(values = color_credibility, name = "Directional Credibility (≥ 95% Posterior Mass)") +
    scale_color_manual(values = color_credibility, name = "Directional Credibility (≥ 95% Posterior Mass)") +
    coord_cartesian(xlim = c(-0.95, 1.55)) +
    labs(
      title = "Demographic Fixed Effects on Cuisine Authenticity",
      subtitle = "Bayesian posterior distributions from Relaxed CS + Random Slopes model (Model 6)\nPositive shifts favor 'Professional Chef' (7) | Negative shifts favor 'Traditional Domestic Elder' (1)",
      x = "Effect Size (Log-Odds Step-Wise Shift)",
      y = NULL,
      caption = "Bayesian crossed random coefficients ACAT model with category-specific effects (Model 6; 4,000 MCMC draws, N = 18,180).\nPoints indicate posterior medians; thick and thin bars represent 80% and 95% credible intervals.\nText annotations report posterior medians, 95% credible intervals, and directional posterior probability mass P(β ≷ 0)."
    ) +
    theme_cuisine(base_size = 12) +
    theme(
      axis.text.y = element_text(face = "bold", size = 10, color = "gray15"),
      panel.grid.major.y = element_line(color = "gray92", linewidth = 0.4),
      legend.position = "bottom"
    )
  
  ggsave(pfile("demographic_fixed_effects"), p_fixed, width = 11.5, height = 8, dpi = 300, bg = "white")
}

# -------------------------------------------------------------
# 4. Cuisine Random Intercepts by 95% Posterior Probability Mass (Model 5)
# -------------------------------------------------------------
if (!is.null(m5)) {
  cat("3. Generating Cuisine Random Intercepts Plot (Model 5: ≥ 95% Posterior Mass Criterion)...\n")
  cuisine_re <- m5 %>%
    spread_draws(r_cuisine[cuisine, term]) %>%
    filter(term == "Intercept") %>%
    mutate(cuisine_label = str_to_title(str_replace_all(cuisine, "_", " ")))
  
  cuisine_summary <- cuisine_re %>%
    group_by(cuisine_label) %>%
    summarize(
      median = median(r_cuisine),
      p_pos = mean(r_cuisine > 0),
      p_neg = mean(r_cuisine < 0),
      cred_status = case_when(
        p_pos >= 0.95 ~ "Credibly Pro Chef (≥ 95% Mass)",
        p_neg >= 0.95 ~ "Credibly Domestic Elder (≥ 95% Mass)",
        TRUE ~ "Spans Zero (< 95% Mass)"
      ),
      .groups = "drop"
    )
  
  cuisine_order <- cuisine_summary %>% arrange(median) %>% pull(cuisine_label)
  
  cuisine_plot_df <- cuisine_re %>%
    left_join(cuisine_summary %>% select(cuisine_label, cred_status), by = "cuisine_label") %>%
    mutate(
      cuisine_label = factor(cuisine_label, levels = cuisine_order),
      cred_status = factor(cred_status, levels = c("Credibly Pro Chef (≥ 95% Mass)", "Credibly Domestic Elder (≥ 95% Mass)", "Spans Zero (< 95% Mass)"))
    )
  
  p_re <- ggplot(cuisine_plot_df, aes(x = r_cuisine, y = cuisine_label, fill = cred_status, color = cred_status)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.75) +
    stat_halfeye(
      point_interval = median_qi,
      .width = c(0.80, 0.95),
      point_size = 3.5,
      interval_size = 1.2,
      slab_alpha = 0.35,
      scale = 0.65
    ) +
    scale_fill_manual(values = color_credibility, name = "Baseline Orientation (≥ 95% Posterior Mass)") +
    scale_color_manual(values = color_credibility, name = "Baseline Orientation (≥ 95% Posterior Mass)") +
    labs(
      title = "Baseline Cuisine Authenticity Orientations (Random Intercepts)",
      subtitle = "Tradition-specific deviations (u0) from average rating when holding demographic covariates at sample means",
      x = "Cuisine Random Intercept Deviation (Log-Odds Shift)",
      y = NULL,
      caption = "Bayesian crossed random effects ACAT model (4,000 post-warmup draws across 15 cuisines).\nColor classified by ≥ 95% posterior probability mass on either side of zero: Blue = P(u0 > 0) ≥ 0.95 (Chef); Vermillion = P(u0 < 0) ≥ 0.95 (Elder)."
    ) +
    theme_cuisine(base_size = 12) +
    theme(
      axis.text.y = element_text(face = "bold", size = 10.5, color = "gray15"),
      panel.grid.major.y = element_line(color = "gray92", linewidth = 0.4),
      legend.position = "bottom"
    )
  
  ggsave(pfile("cuisine_random_effects"), p_re, width = 9.5, height = 7.5, dpi = 300, bg = "white")

  # -------------------------------------------------------------
  # 5. 2D Consensus vs. Authenticity Space (Model 5)
  # -------------------------------------------------------------
  cat("4. Generating 2D Consensus & Preparation Space Plot (Model 5)...\n")
  draws_loc <- m5 %>%
    spread_draws(r_cuisine[cuisine, term]) %>%
    filter(term == "Intercept") %>%
    rename(loc_effect = r_cuisine)
  
  draws_disc <- m5 %>%
    spread_draws(r_cuisine__disc[cuisine, term]) %>%
    filter(term == "Intercept") %>%
    rename(disc_effect = r_cuisine__disc)
  
  summary_2d <- draws_loc %>%
    left_join(draws_disc, by = c(".chain", ".iteration", ".draw", "cuisine")) %>%
    group_by(cuisine) %>%
    summarize(
      loc_med = median(loc_effect),
      loc_lower = quantile(loc_effect, 0.025),
      loc_upper = quantile(loc_effect, 0.975),
      disc_med = median(disc_effect),
      disc_lower = quantile(disc_effect, 0.025),
      disc_upper = quantile(disc_effect, 0.975),
      .groups = "drop"
    ) %>%
    mutate(
      cuisine_label = str_to_title(str_replace_all(cuisine, "_", " ")),
      Quadrant = case_when(
        loc_med < 0 & disc_med > 0 ~ "Elder Consensus (High Agreement)",
        loc_med < 0 & disc_med <= 0 ~ "Elder Contested (Low Agreement)",
        loc_med >= 0 & disc_med > 0 ~ "Chef Consensus (High Agreement)",
        loc_med >= 0 & disc_med <= 0 ~ "Chef Contested (Low Agreement)"
      )
    )
  
  quad_colors <- c(
    "Elder Consensus (High Agreement)"  = "#1b9e77",
    "Elder Contested (Low Agreement)"   = "#D55E00",
    "Chef Consensus (High Agreement)"   = "#0072B2",
    "Chef Contested (Low Agreement)"    = "#7570b3"
  )
  
  p_2d <- ggplot(summary_2d, aes(x = loc_med, y = disc_med)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray55", linewidth = 0.6) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray55", linewidth = 0.6) +
    geom_errorbar(aes(ymin = disc_lower, ymax = disc_upper, color = Quadrant), width = 0, alpha = 0.45, linewidth = 0.6) +
    geom_errorbar(aes(xmin = loc_lower, xmax = loc_upper, color = Quadrant), orientation = "y", width = 0, alpha = 0.45, linewidth = 0.6) +
    geom_point(aes(color = Quadrant), size = 4, alpha = 0.9) +
    geom_label_repel(
      aes(label = cuisine_label, fill = Quadrant),
      color = "white",
      fontface = "bold",
      size = 3.8,
      box.padding = 0.45,
      point.padding = 0.3,
      max.overlaps = 25,
      seed = 42,
      show.legend = FALSE
    ) +
    scale_color_manual(values = quad_colors, name = "Culinary Profile") +
    scale_fill_manual(values = quad_colors, name = "Culinary Profile") +
    coord_cartesian(xlim = c(-3.3, 4.4), ylim = c(-1.6, 1.05)) +
    labs(
      title = "Cuisine Authenticity: Baseline Preparation Orientation vs. Public Consensus",
      subtitle = "Joint posterior medians and full 95% credible intervals for cuisine location (u0) and discrimination (u0_disc) random effects",
      x = "← Traditional Domestic Elder          Location Intercept (u0)          Professional Chef →",
      y = "← High Public Disagreement          Consensus Parameter (u0_disc)          High Public Agreement →",
      caption = "Location-scale ACAT hierarchical model (Model 5: Variance + Random Slopes; 4,000 post-warmup draws across 15 cuisines).\nHorizontal axis captures baseline preparation orientation; vertical axis captures public agreement/consensus (higher disc = lower dispersion)."
    ) +
    theme_cuisine(base_size = 12) +
    theme(
      axis.title.x = element_text(margin = margin(t = 12), face = "bold", size = 10.5),
      axis.title.y = element_text(margin = margin(r = 12), face = "bold", size = 10.5),
      legend.position = "bottom"
    )
  
  ggsave(pfile("cuisine_2d_consensus"), p_2d, width = 10, height = 8, dpi = 300, bg = "white")

  # -------------------------------------------------------------
  # 6. Demographic Predictors of Consensus (Discrimination, Section 7.2)
  # -------------------------------------------------------------
  cat("5. Generating Demographic Predictors of Consensus Plot (Section 7.2 with Credibility Estimates)...\n")
  available_var_vars <- variables(m5)
  var_vars_to_extract <- c("b_disc_educ_c", "b_disc_peduc_c", "b_disc_arts_c", "b_disc_social_c", "b_disc_economic_c")
  var_vars_to_extract <- intersect(var_vars_to_extract, available_var_vars)
  
  draws_disc_fixed <- m5 %>%
    gather_draws(!!!syms(var_vars_to_extract)) %>%
    mutate(
      Predictor = case_when(
        .variable == "b_disc_social_c" ~ "Social Conservatism",
        .variable == "b_disc_economic_c" ~ "Economic Conservatism",
        .variable == "b_disc_educ_c" ~ "Education",
        .variable == "b_disc_peduc_c" ~ "Parental Education",
        .variable == "b_disc_arts_c" ~ "Childhood Arts Exposure",
        TRUE ~ .variable
      )
    )
  
  disc_fixed_summary <- draws_disc_fixed %>%
    group_by(Predictor) %>%
    summarize(
      median = median(.value),
      q2.5 = quantile(.value, 0.025),
      q97.5 = quantile(.value, 0.975),
      p_pos = mean(.value > 0),
      p_neg = mean(.value < 0),
      p_dir = max(p_pos, p_neg),
      cred_status = case_when(
        p_pos >= 0.95 ~ "Credibly Increases Consensus (≥ 95% Mass)",
        p_neg >= 0.95 ~ "Credibly Decreases Consensus (≥ 95% Mass)",
        TRUE ~ "Spans Zero (< 95% Mass)"
      ),
      cred_label = case_when(
        p_pos >= 0.95 ~ sprintf("Median: %+.2f [%+.2f, %+.2f]  |  P(β > 0) = %.1f%% (Credible)", median, q2.5, q97.5, p_pos * 100),
        p_neg >= 0.95 ~ sprintf("Median: %+.2f [%+.2f, %+.2f]  |  P(β < 0) = %.1f%% (Credible)", median, q2.5, q97.5, p_neg * 100),
        TRUE ~ sprintf("Median: %+.2f [%+.2f, %+.2f]  |  P(β %s 0) = %.1f%% (Uncertain)", median, q2.5, q97.5, ifelse(median > 0, ">", "<"), p_dir * 100)
      ),
      .groups = "drop"
    )
  
  pred_order_disc <- disc_fixed_summary %>% arrange(median) %>% pull(Predictor)
  
  draws_disc_plot <- draws_disc_fixed %>%
    left_join(disc_fixed_summary %>% select(Predictor, cred_status), by = "Predictor") %>%
    mutate(
      Predictor = factor(Predictor, levels = pred_order_disc),
      cred_status = factor(cred_status, levels = c(
        "Credibly Increases Consensus (≥ 95% Mass)",
        "Credibly Decreases Consensus (≥ 95% Mass)",
        "Spans Zero (< 95% Mass)"
      ))
    )
  
  disc_fixed_summary <- disc_fixed_summary %>%
    mutate(
      Predictor = factor(Predictor, levels = pred_order_disc),
      cred_status = factor(cred_status, levels = c(
        "Credibly Increases Consensus (≥ 95% Mass)",
        "Credibly Decreases Consensus (≥ 95% Mass)",
        "Spans Zero (< 95% Mass)"
      ))
    )
  
  p_disc <- ggplot(draws_disc_plot, aes(x = .value, y = Predictor, fill = cred_status, color = cred_status)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.75) +
    stat_halfeye(
      point_interval = median_qi,
      .width = c(0.80, 0.95),
      point_size = 3.5,
      interval_size = 1.2,
      slab_alpha = 0.35,
      scale = 0.65
    ) +
    geom_text(
      data = disc_fixed_summary,
      aes(x = 0.26, y = Predictor, label = cred_label),
      hjust = 0,
      size = 3.5,
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_fill_manual(values = color_credibility, name = "Directional Credibility (≥ 95% Posterior Mass)") +
    scale_color_manual(values = color_credibility, name = "Directional Credibility (≥ 95% Posterior Mass)") +
    coord_cartesian(xlim = c(-0.45, 0.90)) +
    labs(
      title = "Demographic and Ideological Predictors of Authenticity Consensus",
      subtitle = "Fixed effects on the log-discrimination parameter (disc). Positive = Higher Consensus | Negative = Greater Disagreement",
      x = "Effect on Log-Discrimination Parameter (β_disc)",
      y = NULL,
      caption = "Posterior parameter estimates from Model 5 (Variance + Random Slopes; 4,000 MCMC draws, N = 18,180).\nPoints indicate posterior medians; thick and thin bars represent 80% and 95% credible intervals.\nText annotations report posterior medians, 95% credible intervals, and directional posterior probability mass P(β ≷ 0)."
    ) +
    theme_cuisine(base_size = 12) +
    theme(
      axis.text.y = element_text(face = "bold", size = 10.5, color = "gray15"),
      panel.grid.major.y = element_line(color = "gray92", linewidth = 0.4),
      legend.position = "bottom"
    )
  
  ggsave(pfile("demographic_variance_effects_forest"), p_disc, width = 11, height = 5.5, dpi = 300, bg = "white")

  # -------------------------------------------------------------
  # 7. Cuisine Random Slopes: Location (Section 6)
  # -------------------------------------------------------------
  cat("6. Generating Cuisine Random Slopes for Location (Model 5: ≥ 95% Posterior Mass Criterion)...\n")
  
  plot_cuisine_location_slopes <- function(model, vars, var_labels, title, filename) {
    tb_list <- list()
    for (v in vars) {
      fixed_var <- paste0("b_", v)
      re_draws <- model %>%
        spread_draws(r_cuisine[cuisine, term], !!sym(fixed_var)) %>%
        filter(term == v) %>%
        mutate(
          cuisine_label = str_to_title(str_replace_all(cuisine, "_", " ")),
          total_slope = !!sym(fixed_var) + r_cuisine,
          predictor_label = var_labels[v]
        )
      tb_list[[v]] <- re_draws
    }
    
    all_slopes <- bind_rows(tb_list)
    
    slope_summary <- all_slopes %>%
      group_by(predictor_label, cuisine_label) %>%
      summarize(
        median = median(total_slope),
        p_pos = mean(total_slope > 0),
        p_neg = mean(total_slope < 0),
        cred_status = case_when(
          p_pos >= 0.95 ~ "Credibly Positive (≥ 95% Mass)",
          p_neg >= 0.95 ~ "Credibly Negative (≥ 95% Mass)",
          TRUE ~ "Spans Zero (< 95% Mass)"
        ),
        .groups = "drop"
      )
    
    cuis_order <- slope_summary %>%
      group_by(cuisine_label) %>%
      summarize(mean_med = mean(median), .groups = "drop") %>%
      arrange(mean_med) %>%
      pull(cuisine_label)
    
    all_slopes <- all_slopes %>%
      left_join(slope_summary %>% select(predictor_label, cuisine_label, cred_status), by = c("predictor_label", "cuisine_label")) %>%
      mutate(
        cuisine_label = factor(cuisine_label, levels = cuis_order),
        cred_status = factor(cred_status, levels = c("Credibly Positive (≥ 95% Mass)", "Credibly Negative (≥ 95% Mass)", "Spans Zero (< 95% Mass)"))
      )
    
    p <- ggplot(all_slopes, aes(x = total_slope, y = cuisine_label, fill = cred_status, color = cred_status)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.7) +
      stat_halfeye(
        point_interval = median_qi,
        .width = c(0.80, 0.95),
        point_size = 3.0,
        interval_size = 1.0,
        slab_alpha = 0.35,
        scale = 0.65
      ) +
      scale_fill_manual(values = color_credibility, name = "Slope Credibility (≥ 95% Posterior Mass)") +
      scale_color_manual(values = color_credibility, name = "Slope Credibility (≥ 95% Posterior Mass)") +
      facet_wrap(~ predictor_label, scales = "free_x") +
      labs(
        title = title,
        subtitle = "Net slope estimates (Global Fixed Effect + Cuisine-Specific Random Deviation u1)\nPositive slopes increase professional chef endorsement per 1 SD increase in predictor",
        x = "Net Effect (Log-Odds Shift per 1 SD Increase)",
        y = NULL,
        caption = "Hierarchical ACAT model with cuisine random slopes (Model 5; 4,000 MCMC draws).\nColor classified by ≥ 95% posterior probability mass on either side of zero. Cuisines ordered by aggregate effect size."
      ) +
      theme_cuisine(base_size = 11) +
      theme(
        strip.text = element_text(face = "bold", size = 11),
        strip.background = element_rect(fill = "gray95", color = NA),
        axis.text.y = element_text(face = "bold", size = 9.5, color = "gray15"),
        panel.spacing = unit(1.2, "lines"),
        legend.position = "bottom"
      )
    
    ggsave(pfile(filename), p, width = 11, height = 7.5, dpi = 300, bg = "white")
  }
  
  # Plot Ideology Slopes (Location)
  plot_cuisine_location_slopes(
    m5,
    vars = c("social_c", "economic_c"),
    var_labels = c("social_c" = "Social Conservatism", "economic_c" = "Economic Conservatism"),
    title = "Cuisine-Specific Slopes: Political Ideology",
    filename = "rs_cuisine_slopes_ideology"
  )
  
  # Plot Cultural Capital Slopes (Location)
  plot_cuisine_location_slopes(
    m5,
    vars = c("educ_c", "peduc_c", "arts_c"),
    var_labels = c("educ_c" = "Educational Attainment", "peduc_c" = "Parental Education", "arts_c" = "Childhood Arts Exposure"),
    title = "Cuisine-Specific Slopes: Cultural Capital & Socialization",
    filename = "rs_cuisine_slopes_cultural"
  )

  # -------------------------------------------------------------
  # 8. Cuisine Random Slopes: Consensus / Variance (Section 8)
  # -------------------------------------------------------------
  cat("7. Generating Cuisine-Specific Heterogeneity in Consensus Plots (Section 8)...\n")
  
  plot_cuisine_variance_slopes <- function(model, vars, var_labels, title, filename) {
    tb_list <- list()
    for (v in vars) {
      fixed_var <- paste0("b_disc_", v)
      re_draws <- model %>%
        spread_draws(r_cuisine__disc[cuisine, term], !!sym(fixed_var)) %>%
        filter(term == v) %>%
        mutate(
          cuisine_label = str_to_title(str_replace_all(cuisine, "_", " ")),
          total_slope = !!sym(fixed_var) + r_cuisine__disc,
          predictor_label = var_labels[v]
        )
      tb_list[[v]] <- re_draws
    }
    
    all_slopes <- bind_rows(tb_list)
    
    slope_summary <- all_slopes %>%
      group_by(predictor_label, cuisine_label) %>%
      summarize(
        median = median(total_slope),
        p_pos = mean(total_slope > 0),
        p_neg = mean(total_slope < 0),
        cred_status = case_when(
          p_pos >= 0.95 ~ "Credibly Increases Consensus (≥ 95% Mass)",
          p_neg >= 0.95 ~ "Credibly Decreases Consensus (≥ 95% Mass)",
          TRUE ~ "Spans Zero (< 95% Mass)"
        ),
        .groups = "drop"
      )
    
    cuis_order <- slope_summary %>%
      group_by(cuisine_label) %>%
      summarize(mean_med = mean(median), .groups = "drop") %>%
      arrange(mean_med) %>%
      pull(cuisine_label)
    
    all_slopes <- all_slopes %>%
      left_join(slope_summary %>% select(predictor_label, cuisine_label, cred_status), by = c("predictor_label", "cuisine_label")) %>%
      mutate(
        cuisine_label = factor(cuisine_label, levels = cuis_order),
        cred_status = factor(cred_status, levels = c("Credibly Increases Consensus (≥ 95% Mass)", "Credibly Decreases Consensus (≥ 95% Mass)", "Spans Zero (< 95% Mass)"))
      )
    
    p <- ggplot(all_slopes, aes(x = total_slope, y = cuisine_label, fill = cred_status, color = cred_status)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.7) +
      stat_halfeye(
        point_interval = median_qi,
        .width = c(0.80, 0.95),
        point_size = 3.0,
        interval_size = 1.0,
        slab_alpha = 0.35,
        scale = 0.65
      ) +
      scale_fill_manual(values = color_credibility, name = "Consensus Shift (≥ 95% Posterior Mass)") +
      scale_color_manual(values = color_credibility, name = "Consensus Shift (≥ 95% Posterior Mass)") +
      facet_wrap(~ predictor_label, scales = "free_x") +
      labs(
        title = title,
        subtitle = "Net slope on log-discrimination parameter (Global Fixed Effect + Cuisine Deviation u1_disc)\nPositive values increase public agreement/consensus; Negative values increase contestation/dispersion",
        x = "Net Effect on Log-Discrimination (Consensus Shift per 1 SD Increase)",
        y = NULL,
        caption = "Location-scale ACAT model with cuisine random slopes on variance (Model 5; 4,000 MCMC draws).\nColor classified by ≥ 95% posterior probability mass on either side of zero. Cuisines ordered by aggregate effect size."
      ) +
      theme_cuisine(base_size = 11) +
      theme(
        strip.text = element_text(face = "bold", size = 11),
        strip.background = element_rect(fill = "gray95", color = NA),
        axis.text.y = element_text(face = "bold", size = 9.5, color = "gray15"),
        panel.spacing = unit(1.2, "lines"),
        legend.position = "bottom"
      )
    
    ggsave(pfile(filename), p, width = 11, height = 7.5, dpi = 300, bg = "white")
  }
  
  # Plot Ideology Slopes on Consensus (Section 8.1)
  plot_cuisine_variance_slopes(
    m5,
    vars = c("social_c", "economic_c"),
    var_labels = c("social_c" = "Social Conservatism", "economic_c" = "Economic Conservatism"),
    title = "Cuisine-Specific Heterogeneity in Consensus: Political Ideology",
    filename = "rs_variance_ideology"
  )
  
  # Plot Cultural Capital Slopes on Consensus (Section 8.2)
  plot_cuisine_variance_slopes(
    m5,
    vars = c("educ_c", "peduc_c", "arts_c"),
    var_labels = c("educ_c" = "Educational Attainment", "peduc_c" = "Parental Education", "arts_c" = "Childhood Arts Exposure"),
    title = "Cuisine-Specific Heterogeneity in Consensus: Cultural Capital",
    filename = "rs_variance_cultural"
  )
}

# -------------------------------------------------------------
# 9. Category-Specific (CS) Threshold & Midpoint Effects (Model 2)
# -------------------------------------------------------------
if (!is.null(m2)) {
  cat("8. Generating Category-Specific Threshold and Midpoint Plots (Model 2)...\n")
  
  # Extract CS draws for Ideology
  draws_cs_ideology <- m2 %>%
    spread_draws(
      bcs_social_c[threshold],
      bcs_economic_c[threshold]
    ) %>%
    pivot_longer(
      cols = c(bcs_social_c, bcs_economic_c),
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(
      Predictor = ifelse(variable == "bcs_social_c", "Social Conservatism", "Economic Conservatism"),
      Transition = factor(
        threshold,
        levels = 1:6,
        labels = c("1 → 2\n(Trad. Elder)", "2 → 3", "3 → 4\n(Midpoint)", "4 → 5", "5 → 6", "6 → 7\n(Pro Chef)")
      )
    )
  
  # 9A. Ideology Threshold Transitions
  p_cs_ideology <- ggplot(draws_cs_ideology, aes(x = Transition, y = value, color = Predictor, group = Predictor)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", linewidth = 0.75) +
    stat_pointinterval(
      point_interval = median_qi,
      .width = c(0.80, 0.95),
      position = position_dodge(width = 0.4),
      point_size = 3.5,
      interval_size = 1.2
    ) +
    scale_color_manual(values = c("Social Conservatism" = "#1b9e77", "Economic Conservatism" = "#d95f02"), name = "Ideological Dimension") +
    labs(
      title = "Category-Specific Ideological Effects Across Rating Thresholds",
      subtitle = "Posterior distributions of transition parameters (β_k) from Relaxed Category-Specific Model (Model 2)",
      x = "Ordinal Rating Transition (k → k+1)",
      y = "Transition Coefficient (Log-Odds Shift)",
      caption = "Bayesian adjacent category model with category-specific effects (Model 2; 4,000 MCMC draws).\nPoints indicate posterior medians; thick and thin bars denote 80% and 95% credible intervals."
    ) +
    theme_cuisine(base_size = 12) +
    theme(
      axis.text.x = element_text(face = "bold", size = 10),
      legend.position = "bottom"
    )
  ggsave(pfile("ideology_cs_effects"), p_cs_ideology, width = 9.5, height = 6, dpi = 300, bg = "white")
  
  # 9B. Ideology Midpoint Contrast Effects
  draws_cs_wide <- m2 %>%
    spread_draws(bcs_social_c[threshold], bcs_economic_c[threshold])
  
  compute_midpoint_contrasts <- function(df, var_prefix, pred_name) {
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
        Category = factor(Category, levels = c("Cat 1 (Elder)", "Cat 2", "Cat 3", "Cat 5", "Cat 6", "Cat 7 (Chef)"))
      )
    return(contrasts)
  }
  
  ideology_midpoint <- bind_rows(
    compute_midpoint_contrasts(draws_cs_wide, "social_c", "Social Conservatism"),
    compute_midpoint_contrasts(draws_cs_wide, "economic_c", "Economic Conservatism")
  )
  
  p_ideology_mid <- ggplot(ideology_midpoint, aes(x = Category, y = contrast_log_odds, color = Predictor, group = Predictor)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", linewidth = 0.75) +
    stat_pointinterval(
      point_interval = median_qi,
      .width = c(0.80, 0.95),
      position = position_dodge(width = 0.45),
      point_size = 3.5,
      interval_size = 1.2
    ) +
    scale_color_manual(values = c("Social Conservatism" = "#1b9e77", "Economic Conservatism" = "#d95f02"), name = "Ideological Dimension") +
    labs(
      title = "Ideological Effects Relative to Neutral Scale Midpoint (Category 4)",
      subtitle = "Cumulative contrast log-odds of selecting given category vs. neutral midpoint per 1 SD increase in ideology",
      x = "Response Scale Position (Relative to Midpoint = 4)",
      y = "Contrast Log-Odds (vs. Category 4)",
      caption = "Derived from Relaxed CS Model (Model 2; 4,000 MCMC draws).\nPositive values indicate increased likelihood of choosing that category relative to the midpoint."
    ) +
    theme_cuisine(base_size = 12) +
    theme(
      axis.text.x = element_text(face = "bold", size = 10),
      legend.position = "bottom"
    )
  ggsave(pfile("ideology_cs_midpoint_effects"), p_ideology_mid, width = 9.5, height = 6, dpi = 300, bg = "white")
  
  # 9C. Cultural Capital Threshold Transitions
  draws_cs_cultural <- m2 %>%
    spread_draws(
      bcs_educ_c[threshold],
      bcs_peduc_c[threshold],
      bcs_arts_c[threshold]
    ) %>%
    pivot_longer(
      cols = c(bcs_educ_c, bcs_peduc_c, bcs_arts_c),
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(
      Predictor = case_when(
        variable == "bcs_educ_c" ~ "Educational Attainment",
        variable == "bcs_peduc_c" ~ "Parental Education",
        variable == "bcs_arts_c" ~ "Childhood Arts Exposure"
      ),
      Transition = factor(
        threshold,
        levels = 1:6,
        labels = c("1 → 2\n(Trad. Elder)", "2 → 3", "3 → 4\n(Midpoint)", "4 → 5", "5 → 6", "6 → 7\n(Pro Chef)")
      )
    )
  
  cult_colors <- c(
    "Educational Attainment"   = "#7570b3",
    "Parental Education"       = "#1b9e77",
    "Childhood Arts Exposure"  = "#d95f02"
  )
  
  p_cs_cult <- ggplot(draws_cs_cultural, aes(x = Transition, y = value, color = Predictor, group = Predictor)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", linewidth = 0.75) +
    stat_pointinterval(
      point_interval = median_qi,
      .width = c(0.80, 0.95),
      position = position_dodge(width = 0.5),
      point_size = 3.5,
      interval_size = 1.2
    ) +
    scale_color_manual(values = cult_colors, name = "Cultural Capital Dimension") +
    labs(
      title = "Category-Specific Cultural Capital Effects Across Rating Thresholds",
      subtitle = "Posterior distributions of transition parameters (β_k) from Relaxed Category-Specific Model (Model 2)",
      x = "Ordinal Rating Transition (k → k+1)",
      y = "Transition Coefficient (Log-Odds Shift)",
      caption = "Bayesian adjacent category model with category-specific effects (Model 2; 4,000 MCMC draws).\nPoints indicate posterior medians; thick and thin bars denote 80% and 95% credible intervals."
    ) +
    theme_cuisine(base_size = 12) +
    theme(
      axis.text.x = element_text(face = "bold", size = 10),
      legend.position = "bottom"
    )
  ggsave(pfile("cultural_cs_effects"), p_cs_cult, width = 10, height = 6, dpi = 300, bg = "white")
  
  # 9D. Cultural Capital Midpoint Contrasts
  draws_cs_cult_wide <- m2 %>%
    spread_draws(bcs_educ_c[threshold], bcs_peduc_c[threshold], bcs_arts_c[threshold])
  
  cult_midpoint <- bind_rows(
    compute_midpoint_contrasts(draws_cs_cult_wide, "educ_c", "Educational Attainment"),
    compute_midpoint_contrasts(draws_cs_cult_wide, "peduc_c", "Parental Education"),
    compute_midpoint_contrasts(draws_cs_cult_wide, "arts_c", "Childhood Arts Exposure")
  )
  
  p_cult_mid <- ggplot(cult_midpoint, aes(x = Category, y = contrast_log_odds, color = Predictor, group = Predictor)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", linewidth = 0.75) +
    stat_pointinterval(
      point_interval = median_qi,
      .width = c(0.80, 0.95),
      position = position_dodge(width = 0.5),
      point_size = 3.5,
      interval_size = 1.2
    ) +
    scale_color_manual(values = cult_colors, name = "Cultural Capital Dimension") +
    labs(
      title = "Cultural Capital Effects Relative to Neutral Scale Midpoint (Category 4)",
      subtitle = "Cumulative contrast log-odds of selecting given category vs. neutral midpoint per 1 SD increase in predictor",
      x = "Response Scale Position (Relative to Midpoint = 4)",
      y = "Contrast Log-Odds (vs. Category 4)",
      caption = "Derived from Relaxed CS Model (Model 2; 4,000 MCMC draws).\nPositive values indicate increased likelihood of choosing that category relative to the midpoint."
    ) +
    theme_cuisine(base_size = 12) +
    theme(
      axis.text.x = element_text(face = "bold", size = 10),
      legend.position = "bottom"
    )
  ggsave(pfile("cultural_cs_midpoint_effects"), p_cult_mid, width = 10, height = 6, dpi = 300, bg = "white")
}

cat("\n========================================================================\n")
cat("SUCCESS: All figures generated and saved cleanly to Plots/\n")
cat("========================================================================\n")
