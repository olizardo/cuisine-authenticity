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
#'   3. Plots/cuisine_random_effects.png (Baseline Cuisine Random Intercepts - Model 6)
#'   4. Plots/rs_cuisine_slopes_ideology.png (Cuisine-Specific Random Slopes for Ideology - Model 3/6)
#'   5. Plots/rs_cuisine_slopes_cultural.png (Cuisine-Specific Random Slopes for Cultural Capital - Model 3/6)
#'   6. Plots/ideology_cs_effects.png (Threshold-Specific Shifts for Political Ideology - Model 2)
#'   7. Plots/ideology_cs_midpoint_effects.png (Midpoint Contrast Shifts for Political Ideology - Model 2)
#'   8. Plots/cultural_cs_effects.png (Threshold-Specific Shifts for Cultural Capital - Model 2)
#'   9. Plots/cultural_cs_midpoint_effects.png (Midpoint Contrast Shifts for Cultural Capital - Model 2)
#' @author Cuisine Authenticity Project

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(stringr)
  library(ggplot2)
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
  "Credibly Pro Chef (≥ 95% Mass)"       = "#0072B2", # Blue
  "Credibly Domestic Elder (≥ 95% Mass)" = "#D55E00", # Vermillion
  "Credibly Positive (≥ 95% Mass)"       = "#0072B2",
  "Credibly Negative (≥ 95% Mass)"       = "#D55E00",
  "Spans Zero (< 95% Mass)"              = "gray60",
  "Not Credible (< 95% Mass)"            = "gray60"
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
m6 <- load_model("6_relaxed_rs")

# -------------------------------------------------------------
# 2. WAIC Model Fit Comparison across all 18 Taxonomy Models
# -------------------------------------------------------------
cat("\n1. Generating WAIC Model Fit Comparison across all 18 taxonomy models...\n")

fit_comp_path <- here("cache", "full_model_fit_comparison.rds")
if (file.exists(fit_comp_path)) {
  full_fit_df <- readRDS(fit_comp_path)
  
  base_strict_ri_waic <- full_fit_df %>%
    filter(Domain == "base", Threshold == "strict", Random_Effects == "ri") %>%
    pull(WAIC)
  
  domain_clean_labels <- c(
    "base"         = "Cultural Capital (Base)",
    "practices"    = "Dining Practices",
    "dispositions" = "Taste Dispositions",
    "cosmopolitan" = "Cosmopolitan Capital",
    "meta"         = "Omnibus Meta"
  )
  
  domain_colors <- c(
    "Cultural Capital (Base)" = "#7570b3",
    "Dining Practices"        = "#d95f02",
    "Taste Dispositions"      = "#1b9e77",
    "Cosmopolitan Capital"    = "#0072B2",
    "Omnibus Meta"            = "#E7298A"
  )
  
  full_plot_df <- full_fit_df %>%
    mutate(
      Domain_Clean = domain_clean_labels[Domain],
      Thresh_Label = ifelse(Threshold == "relaxed", "Relaxed CS", "Strict PO"),
      RE_Label = ifelse(Random_Effects == "rs", "Crossed Slopes", "Random Intercepts"),
      Model_Title = sprintf("%s [%s, %s]", Domain_Clean, Thresh_Label, RE_Label),
      Delta_Base = WAIC - base_strict_ri_waic,
      Delta_Label = case_when(
        abs(Delta_Base) < 0.001 ~ "0.0 (Ref)",
        Delta_Base < 0 ~ sprintf("%.1f", Delta_Base),
        TRUE ~ sprintf("+%.1f", Delta_Base)
      )
    ) %>%
    arrange(Delta_Base)
  
  p_waic <- ggplot(full_plot_df, aes(x = Delta_Base, y = reorder(Model_Title, -Delta_Base), color = Domain_Clean)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
    geom_segment(aes(x = 0, xend = Delta_Base, y = reorder(Model_Title, -Delta_Base), yend = reorder(Model_Title, -Delta_Base)), linewidth = 0.9, alpha = 0.7) +
    geom_point(size = 3.5) +
    geom_text(
      aes(
        label = Delta_Label,
        hjust = ifelse(Delta_Base < -30, 1.2, -0.25)
      ),
      vjust = 0.38,
      size = 3.3,
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_color_manual(values = domain_colors, name = "Substantive Domain") +
    coord_cartesian(xlim = c(-1480, 120)) +
    scale_x_continuous(
      breaks = seq(-1400, 0, by = 200)
    ) +
    labs(
      title = "Bayesian Model Fit Comparison Relative to Baseline Model 1",
      subtitle = expression(bold(Delta * "WAIC relative to Cultural Capital Strict RI") ~ " (N = 18,180 ratings across 1,212 respondents)"),
      x = expression(Delta * "WAIC vs. Reference Baseline (Negative = Predictive Improvement; Positive = Fit Degradation)"),
      y = NULL,
      caption = "Reference Baseline (0.0): Cultural Capital (Strict Proportional Odds, Random Intercepts; WAIC = 55,310.59).\nNegative values indicate out-of-sample predictive gain over baseline; all 18 models ordered from largest gain (top) to baseline (bottom)."
    ) +
    theme_cuisine(base_size = 11) +
    theme(
      axis.text.y = element_text(size = 9.5, face = "bold"),
      legend.position = "bottom"
    )
  
  ggsave(pfile("model_fit_comparison"), p_waic, width = 10.5, height = 7.5, dpi = 300, bg = "white")
}

# -------------------------------------------------------------
# 3. Global Demographic Fixed Effects (Model 6: Relaxed CS + Random Slopes)
# -------------------------------------------------------------
if (!is.null(m6)) {
  cat("2. Generating Demographic Fixed Effects Plot (Model 6: Relaxed CS + Random Slopes)...\n")
  
  non_cs_vars <- c("b_income_c", "b_age_c", "b_gend.fWoman",
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
      summarize(.value = mean(bcs_educ_c), .groups = "drop") %>% mutate(Predictor = "Educational Attainment"),
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
      .groups = "drop"
    )
  
  # Logical ordering by variable type (Race/Ethnicity at bottom, Demographics, Cultural/Socioeconomic Capital, Ideology at top)
  pred_order <- c(
    # Race / Ethnicity
    "Mixed / Other Race",
    "Mixed White",
    "Hispanic",
    "Black",
    "Asian",
    # Demographics
    "Woman",
    "Age",
    # Cultural & Socioeconomic Capital
    "Household Income",
    "Childhood Arts Exposure",
    "Parental Education",
    "Educational Attainment",
    # Political Ideology
    "Economic Conservatism",
    "Social Conservatism"
  )
  
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
  
  p_fixed <- ggplot(all_draws_plot, aes(x = .value, y = Predictor, fill = cred_status, color = cred_status)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.75) +
    stat_halfeye(
      point_interval = median_qi,
      .width = c(0.80, 0.95),
      point_size = 3.2,
      interval_size = 1.1,
      slab_alpha = 0.40,
      scale = 0.68
    ) +
    scale_fill_manual(values = color_credibility, name = "Directional Credibility (≥ 95% Posterior Mass)") +
    scale_color_manual(values = color_credibility, name = "Directional Credibility (≥ 95% Posterior Mass)") +
    scale_x_continuous(
      breaks = seq(-0.5, 0.3, by = 0.1),
      labels = function(x) sprintf("%+.1f", x)
    ) +
    coord_cartesian(xlim = c(-0.52, 0.35)) +
    labs(
      title = "Demographic Fixed Effects on Cuisine Authenticity",
      subtitle = "Bayesian posterior distributions from Relaxed CS + Random Slopes model (Model 6)\nPositive shifts favor 'Professional Chef' (7) | Negative shifts favor 'Traditional Domestic Elder' (1)",
      x = "Effect Size (Log-Odds Step-Wise Shift)",
      y = NULL,
      caption = "Bayesian crossed random coefficients ACAT model with category-specific effects (Model 6; 4,000 MCMC draws, N = 18,180).\nPoints indicate posterior medians; thick and thin bars represent 80% and 95% credible intervals.\nPredictors organized by substantive domain: Political Ideology, Cultural Capital & Socioeconomic Status, Demographics, and Ethnoracial Identity."
    ) +
    theme_cuisine(base_size = 12) +
    theme(
      axis.text.y = element_text(face = "bold", size = 10.5, color = "gray15"),
      panel.grid.major.y = element_line(color = "gray92", linewidth = 0.4),
      legend.position = "bottom"
    )
  
  ggsave(pfile("demographic_fixed_effects"), p_fixed, width = 10.0, height = 7.5, dpi = 300, bg = "white")
}

# -------------------------------------------------------------
# 4. Baseline Cuisine Random Intercepts (Model 6)
# -------------------------------------------------------------
if (!is.null(m6)) {
  cat("3. Generating Baseline Cuisine Random Intercepts Consensus Plot across all 18 models...\n")
  
  source(here("scripts", "model_registry.R"))
  
  cuisines_list <- c("japanese", "french", "italian", "mexican", "moroccan", 
                     "korean", "peruvian", "native_american", "swedish", 
                     "pakistani", "ethiopian", "vietnamese", "nigerian", 
                     "jamaican", "lebanese")
  
  cuisine_labels <- c(
    "japanese" = "Japanese", "french" = "French", "italian" = "Italian",
    "mexican" = "Mexican", "moroccan" = "Moroccan", "korean" = "Korean",
    "peruvian" = "Peruvian", "native_american" = "Native American", "swedish" = "Swedish",
    "pakistani" = "Pakistani", "ethiopian" = "Ethiopian", "vietnamese" = "Vietnamese",
    "nigerian" = "Nigerian", "jamaican" = "Jamaican", "lebanese" = "Lebanese"
  )
  
  cache_dir <- here("cache")
  all_cuisine_draws <- list()
  
  for (i in seq_len(nrow(MODEL_REGISTRY))) {
    row <- MODEL_REGISTRY[i, ]
    sys_path <- file.path(cache_dir, row$systematic_file)
    leg_path <- file.path(cache_dir, row$legacy_file)
    
    target_path <- NULL
    if (file.exists(sys_path)) target_path <- sys_path
    else if (file.exists(leg_path)) target_path <- leg_path
    
    if (!is.null(target_path)) {
      m <- tryCatch(readRDS(target_path), error = function(e) NULL)
      if (!is.null(m) && inherits(m, "brmsfit")) {
        dr <- tryCatch({
          m %>%
            spread_draws(r_cuisine[cuisine, term]) %>%
            filter(term == "Intercept") %>%
            mutate(
              model_key = row$systematic_file,
              domain = row$domain,
              domain_label = row$domain_label,
              threshold = row$threshold,
              re = row$re,
              model_label = sprintf("%s [%s, %s]", row$domain_label, ifelse(row$threshold=="relaxed","CS","PO"), toupper(row$re))
            )
        }, error = function(e) NULL)
        
        if (!is.null(dr)) {
          all_cuisine_draws[[length(all_cuisine_draws) + 1]] <- dr
        }
      }
    }
  }
  
  full_cuisine_df <- bind_rows(all_cuisine_draws) %>%
    mutate(cuisine_label = cuisine_labels[as.character(cuisine)])
  
  cuisine_model_summary <- full_cuisine_df %>%
    group_by(cuisine_label, model_key, model_label) %>%
    summarise(
      mean = mean(r_cuisine),
      median = median(r_cuisine),
      q025 = quantile(r_cuisine, 0.025),
      q975 = quantile(r_cuisine, 0.975),
      p_gt_0 = mean(r_cuisine > 0),
      p_lt_0 = mean(r_cuisine < 0),
      .groups = "drop"
    )
  
  cuisine_stability <- full_cuisine_df %>%
    group_by(cuisine_label) %>%
    summarise(
      k_models = n_distinct(model_key),
      mean_effect = mean(r_cuisine),
      grand_median = median(r_cuisine),
      min_median = min(tapply(r_cuisine, model_key, median)),
      max_median = max(tapply(r_cuisine, model_key, median)),
      min_q025 = min(tapply(r_cuisine, model_key, function(x) quantile(x, 0.025))),
      max_q975 = max(tapply(r_cuisine, model_key, function(x) quantile(x, 0.975))),
      min_p_gt_0 = min(tapply(r_cuisine, model_key, function(x) mean(x > 0))),
      max_p_gt_0 = max(tapply(r_cuisine, model_key, function(x) mean(x > 0))),
      min_p_lt_0 = min(tapply(r_cuisine, model_key, function(x) mean(x < 0))),
      max_p_lt_0 = max(tapply(r_cuisine, model_key, function(x) mean(x < 0))),
      .groups = "drop"
    ) %>%
    mutate(
      cred_short = case_when(
        min_p_gt_0 >= 0.95 ~ "Pro-Chef Anchor",
        min_p_lt_0 >= 0.95 ~ "Elder Authenticity Anchor",
        TRUE ~ "Spans Zero / Neutral"
      )
    ) %>%
    arrange(mean_effect)
  
  saveRDS(cuisine_stability, here("cache", "cuisine_stability_summary.rds"))
  readr::write_csv(cuisine_stability, here("cache", "cuisine_stability_summary.csv"))
  
  cuisine_order <- cuisine_stability %>% pull(cuisine_label)
  
  cuisine_stability <- cuisine_stability %>%
    mutate(
      cuisine_label = factor(cuisine_label, levels = cuisine_order),
      cred_short = factor(cred_short, levels = c("Pro-Chef Anchor", "Elder Authenticity Anchor", "Spans Zero / Neutral"))
    )
  
  cuisine_model_summary <- cuisine_model_summary %>%
    left_join(cuisine_stability %>% select(cuisine_label, cred_short) %>% mutate(cuisine_label = as.character(cuisine_label)), by = "cuisine_label") %>%
    mutate(
      cuisine_label = factor(cuisine_label, levels = cuisine_order),
      cred_short = factor(cred_short, levels = c("Pro-Chef Anchor", "Elder Authenticity Anchor", "Spans Zero / Neutral"))
    )
  
  plot_cuisine_draws <- full_cuisine_df %>%
    left_join(cuisine_stability %>% select(cuisine_label, cred_short) %>% mutate(cuisine_label = as.character(cuisine_label)), by = "cuisine_label") %>%
    mutate(
      cuisine_label = factor(cuisine_label, levels = cuisine_order),
      cred_short = factor(cred_short, levels = c("Pro-Chef Anchor", "Elder Authenticity Anchor", "Spans Zero / Neutral"))
    )
  
  color_cred_short <- c(
    "Pro-Chef Anchor"           = "#0072B2",
    "Elder Authenticity Anchor" = "#D55E00",
    "Spans Zero / Neutral"      = "gray60"
  )
  
  p_re <- ggplot(plot_cuisine_draws, aes(x = r_cuisine, y = cuisine_label, fill = cred_short, color = cred_short)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.75) +
    stat_halfeye(
      point_interval = median_qi,
      .width = c(0.80, 0.95),
      point_size = 2.8,
      interval_size = 1.0,
      slab_alpha = 0.45,
      scale = 0.65
    ) +
    geom_point(
      data = cuisine_model_summary,
      aes(x = median, y = cuisine_label),
      color = "gray15",
      size = 1.5,
      alpha = 0.50,
      shape = 3,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(
      values = color_cred_short,
      name = "Consensus Baseline Orientation:"
    ) +
    scale_color_manual(
      values = color_cred_short,
      name = "Consensus Baseline Orientation:"
    ) +
    scale_x_continuous(
      breaks = seq(-0.6, 1.0, by = 0.2),
      labels = function(x) sprintf("%+.1f", x)
    ) +
    coord_cartesian(xlim = c(-0.65, 1.05)) +
    labs(
      title = "Baseline Cuisine Authenticity Hierarchy: Cross-Specification Consensus",
      subtitle = "Posterior distributions (half-eyes) for cuisine random intercepts across all 18 completed models, ordered by mean effect size\nTick marks (+) represent individual model posterior medians demonstrating specification stability",
      x = "Cuisine Random Intercept Shift (Log-Odds Deviation vs. Average Cuisine)",
      y = NULL,
      caption = "Half-eye density slabs, posterior medians, and 80%/95% CrIs synthesized across all 18 completed Bayesian model specifications (N = 18,180 ratings).\nTick marks (+) indicate individual model posterior medians (k = 18 models per cuisine).\nCuisines ordered strictly along the y-axis by posterior mean effect size. Blue = Credibly Pro-Chef; Vermillion = Credibly Domestic Elder; Gray = Spans Zero."
    ) +
    theme_cuisine(base_size = 11) +
    theme(
      axis.text.y = element_text(face = "bold", size = 10, color = "gray15"),
      panel.grid.major.y = element_line(color = "gray92", linewidth = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9.5),
      legend.box = "horizontal",
      legend.margin = margin(t = 2, b = 2)
    ) +
    guides(fill = guide_legend(nrow = 1), color = guide_legend(nrow = 1))
  
  ggsave(pfile("cuisine_random_effects"), p_re, width = 10.0, height = 7.0, dpi = 300, bg = "white")
}

# -------------------------------------------------------------
# 5. Cuisine Random Slopes: Cross-Model Consensus (Ideology & Cultural Capital)
# -------------------------------------------------------------
cat("4. Generating Cuisine Random Slopes Consensus Plots (Ideology & Cultural Capital)...\n")
source(here("scripts", "extract_random_slopes_stability.R"))

# -------------------------------------------------------------
# 6. Midpoint Contrast Effects: Multi-Model Consensus Half-Eyes
# -------------------------------------------------------------
cat("5. Generating Midpoint Contrast Consensus Plots with Half-Eyes...\n")
source(here("scripts", "generate_novel_midpoint_contrasts.R"))

cat("========================================================================\n")
cat("All figures generated successfully across all 18 models!\n")
cat("========================================================================\n")
