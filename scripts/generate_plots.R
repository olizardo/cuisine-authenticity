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
# 2. WAIC Model Fit Comparison across all 16 Taxonomy Models
# -------------------------------------------------------------
cat("\n1. Generating WAIC Model Fit Comparison across all 16 taxonomy models...\n")

fit_comp_path <- here("cache", "full_model_fit_comparison.rds")
if (file.exists(fit_comp_path)) {
  full_fit_df <- readRDS(fit_comp_path)
  
  domain_clean_labels <- c(
    "base"         = "Cultural Capital (Base)",
    "practices"    = "Dining Practices",
    "dispositions" = "Taste Dispositions",
    "cosmopolitan" = "Cosmopolitan Capital"
  )
  
  domain_colors <- c(
    "Cultural Capital (Base)" = "#7570b3",
    "Dining Practices"        = "#d95f02",
    "Taste Dispositions"      = "#1b9e77",
    "Cosmopolitan Capital"    = "#0072B2"
  )
  
  full_plot_df <- full_fit_df %>%
    mutate(
      Domain_Clean = domain_clean_labels[Domain],
      Thresh_Label = ifelse(Threshold == "relaxed", "Relaxed CS", "Strict PO"),
      RE_Label = ifelse(Random_Effects == "rs", "Crossed Slopes", "Random Intercepts"),
      Model_Title = sprintf("%s [%s, %s]", Domain_Clean, Thresh_Label, RE_Label),
      Delta_Label = ifelse(Delta_WAIC == 0, "Top Fit (0.0)", sprintf("+%.1f", Delta_WAIC))
    ) %>%
    arrange(WAIC)
  
  p_waic <- ggplot(full_plot_df, aes(x = reorder(Model_Title, -WAIC), y = WAIC, color = Domain_Clean)) +
    geom_pointrange(aes(ymin = WAIC - SE, ymax = WAIC + SE), linewidth = 0.85, size = 0.65) +
    geom_text(
      aes(label = Delta_Label),
      hjust = -0.25,
      vjust = -0.3,
      size = 3.2,
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_color_manual(values = domain_colors, name = "Substantive Domain") +
    coord_flip(ylim = c(54000, 55800)) +
    labs(
      title = "Bayesian Model Fit Comparison Across Full Factorial Taxonomy",
      subtitle = "WAIC Comparison Across 16 Hierarchical Adjacent Category Models (N = 18,180 ratings across 1,212 respondents)",
      x = NULL,
      y = "Watanabe-Akaike Information Criterion (WAIC ± 1 SE; Lower is Better)",
      caption = "WAIC computed across 4,000 post-warmup MCMC draws per model. Points represent posterior WAIC estimates; error bars indicate ± 1 standard error.\nΔ values reflect difference in WAIC relative to the top-performing model (Taste Dispositions [Relaxed CS, Crossed Slopes])."
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
  cat("3. Generating Cuisine Random Intercepts Plot (Model 6: ≥ 95% Posterior Mass Criterion)...\n")
  cuisine_re <- m6 %>%
    spread_draws(r_cuisine[cuisine, term]) %>%
    filter(term == "Intercept") %>%
    mutate(cuisine_label = str_to_title(str_replace_all(cuisine, "_", " ")))
  
  cuisine_summary <- cuisine_re %>%
    group_by(cuisine_label) %>%
    summarize(
      median = median(r_cuisine),
      q2.5 = quantile(r_cuisine, 0.025),
      q97.5 = quantile(r_cuisine, 0.975),
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
  
  cuisine_summary$cuisine_label <- factor(cuisine_summary$cuisine_label, levels = cuisine_order)
  
  p_re <- ggplot(cuisine_plot_df, aes(x = r_cuisine, y = cuisine_label, fill = cred_status, color = cred_status)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.75) +
    stat_halfeye(
      point_interval = median_qi,
      .width = c(0.80, 0.95),
      point_size = 3.5,
      interval_size = 1.2,
      slab_alpha = 0.40,
      scale = 0.65
    ) +
    scale_fill_manual(values = color_credibility, name = "Baseline Orientation (≥ 95% Posterior Mass)") +
    scale_color_manual(values = color_credibility, name = "Baseline Orientation (≥ 95% Posterior Mass)") +
    scale_x_continuous(
      breaks = seq(-0.6, 1.0, by = 0.2),
      labels = function(x) sprintf("%+.1f", x)
    ) +
    coord_cartesian(xlim = c(-0.65, 1.05)) +
    labs(
      title = "Baseline Cuisine Authenticity Orientations (Random Intercepts)",
      subtitle = "Tradition-specific deviations (u0) from average rating when holding demographic covariates at sample means",
      x = "Cuisine Random Intercept Deviation (Log-Odds Shift)",
      y = NULL,
      caption = "Bayesian crossed random effects ACAT model (Model 6; 4,000 post-warmup draws across 15 cuisines).\nColor classified by ≥ 95% posterior probability mass: Blue = P(u0 > 0) ≥ 0.95 (Chef); Vermillion = P(u0 < 0) ≥ 0.95 (Elder)."
    ) +
    theme_cuisine(base_size = 12) +
    theme(
      axis.text.y = element_text(face = "bold", size = 10.5, color = "gray15"),
      panel.grid.major.y = element_line(color = "gray92", linewidth = 0.4),
      legend.position = "bottom"
    )
  
  ggsave(pfile("cuisine_random_effects"), p_re, width = 10.0, height = 7.5, dpi = 300, bg = "white")
}

# -------------------------------------------------------------
# 5. Cuisine Random Slopes: Location (Ideology & Cultural Capital - Model 3/6)
# -------------------------------------------------------------
model_rs <- if (!is.null(m6)) m6 else m3
if (!is.null(model_rs)) {
  cat("4. Generating Cuisine Random Slopes for Location (Model 6/3)...\n")
  
  plot_cuisine_location_slopes <- function(model, vars, var_labels, title, filename, order_by_var = NULL) {
    tb_list <- list()
    for (v in vars) {
      cs_param <- paste0("bcs_", v)
      if (cs_param %in% variables(model) || paste0("bcs_", v, "[1]") %in% variables(model)) {
        # Model 6 category-specific: average fixed effect across thresholds
        cs_sym <- rlang::sym(cs_param)
        fe_draws <- model %>% 
          spread_draws((!!cs_sym)[threshold]) %>% 
          group_by(.chain, .iteration, .draw) %>% 
          summarize(fe_val = mean(!!cs_sym), .groups = "drop")
        
        re_draws <- model %>%
          spread_draws(r_cuisine[cuisine, term]) %>%
          filter(term == v) %>%
          left_join(fe_draws, by = c(".chain", ".iteration", ".draw")) %>%
          mutate(
            cuisine_label = str_to_title(str_replace_all(cuisine, "_", " ")),
            total_slope = fe_val + r_cuisine,
            predictor_label = var_labels[v]
          )
      } else {
        fixed_var <- paste0("b_", v)
        re_draws <- model %>%
          spread_draws(r_cuisine[cuisine, term], !!sym(fixed_var)) %>%
          filter(term == v) %>%
          mutate(
            cuisine_label = str_to_title(str_replace_all(cuisine, "_", " ")),
            total_slope = !!sym(fixed_var) + r_cuisine,
            predictor_label = var_labels[v]
          )
      }
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
    
    if (!is.null(order_by_var) && order_by_var %in% names(var_labels)) {
      target_label <- var_labels[order_by_var]
      cuis_order <- slope_summary %>%
        filter(predictor_label == target_label) %>%
        arrange(median) %>%
        pull(cuisine_label)
    } else {
      cuis_order <- slope_summary %>%
        group_by(cuisine_label) %>%
        summarize(mean_med = mean(median), .groups = "drop") %>%
        arrange(mean_med) %>%
        pull(cuisine_label)
    }
    
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
        caption = "Hierarchical ACAT model with cuisine random slopes (Model 6; 4,000 MCMC draws).\nColor classified by ≥ 95% posterior probability mass on either side of zero."
      ) +
      theme_cuisine(base_size = 11) +
      theme(
        strip.text = element_text(face = "bold", size = 11),
        strip.background = element_rect(fill = "gray95", color = NA),
        axis.text.y = element_text(face = "bold", size = 9.5, color = "gray15"),
        panel.spacing = unit(1.2, "lines"),
        legend.position = "bottom"
      )
    
    ggsave(filename, p, width = 11.5, height = 7.5, dpi = 300, bg = "white")
    return(p)
  }
  
  vars_ideo <- c("social_c", "economic_c")
  var_labels_ideo <- c("social_c" = "Social Conservatism", "economic_c" = "Economic Conservatism")
  plot_cuisine_location_slopes(
    model_rs, vars_ideo, var_labels_ideo,
    "Cuisine-Specific Ideological Effects on Authenticity Orientation",
    pfile("rs_cuisine_slopes_ideology"),
    order_by_var = "social_c"
  )
  
  vars_cult <- c("educ_c", "peduc_c", "arts_c")
  var_labels_cult <- c("educ_c" = "Educational Attainment", "peduc_c" = "Parental Education", "arts_c" = "Childhood Arts Exposure")
  plot_cuisine_location_slopes(
    model_rs, vars_cult, var_labels_cult,
    "Cuisine-Specific Cultural Capital Effects on Authenticity Orientation",
    pfile("rs_cuisine_slopes_cultural"),
    order_by_var = "educ_c"
  )
}

# -------------------------------------------------------------
# 6. Midpoint Contrast Effects (Model 2)
# -------------------------------------------------------------
if (!is.null(m2)) {
  cat("5. Generating Midpoint Contrast Plots for Ideology and Cultural Capital (Model 2)...\n")
  
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
  
  # 6A. Ideology Midpoint Contrast Effects
  draws_cs_wide <- m2 %>%
    spread_draws(bcs_social_c[threshold], bcs_economic_c[threshold])
  
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
  
  # 6B. Cultural Capital Midpoint Contrast Effects
  draws_cs_cult_wide <- m2 %>%
    spread_draws(bcs_educ_c[threshold], bcs_peduc_c[threshold], bcs_arts_c[threshold])
  
  cult_colors <- c("Educational Attainment" = "#7570b3", "Parental Education" = "#e7298a", "Childhood Arts Exposure" = "#66a61e")
  
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
      position = position_dodge(width = 0.45),
      point_size = 3.5,
      interval_size = 1.2
    ) +
    scale_color_manual(values = cult_colors, name = "Cultural Capital Indicator") +
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
  ggsave(pfile("cultural_cs_midpoint_effects"), p_cult_mid, width = 9.5, height = 6, dpi = 300, bg = "white")
}

cat("========================================================================\n")
cat("All figures generated successfully!\n")
cat("========================================================================\n")
