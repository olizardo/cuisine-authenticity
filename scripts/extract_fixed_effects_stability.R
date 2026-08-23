#' @title Extract Fixed Effects & Cross-Specification Stability Range
#' @description Iterates across all fitted models in the factorial registry (Base, Practices,
#'   Dispositions, Cosmopolitan, Meta) to extract posterior draws for all fixed effects,
#'   compute the cross-specification stability envelope (min/max medians, credibility intervals),
#'   and output publication-ready synthesis tables and forest plots with half-eye posterior distributions.

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

source(here("scripts/model_registry.R"))

# -------------------------------------------------------------------------
# 1. Configuration & Variable Dictionaries
# -------------------------------------------------------------------------

PREDICTOR_LABELS <- c(
  "social_c"              = "Social Conservatism",
  "economic_c"            = "Economic Conservatism",
  "educ_c"                = "Educational Attainment",
  "peduc_c"               = "Parental Education",
  "arts_c"                = "Childhood Arts Socialization",
  "highbrow_arts_c"       = "Adult Highbrow Arts Attendance",
  "fancy_rest_c"          = "Fine Dining Frequency",
  "fast_food_c"           = "Fast Food Frequency",
  "taste_authentic_c"     = "Dispositions: Exotic / Authentic",
  "taste_familiar_c"      = "Dispositions: Familiar Comfort",
  "taste_light_c"         = "Dispositions: Light / Fresh",
  "taste_rich_c"          = "Dispositions: Rich / Hearty",
  "cosmo_global_c"        = "Global Citizen Identity",
  "network_diversity_c"   = "Friendship Network Diversity",
  "income_c"              = "Household Income",
  "age_c"                 = "Age",
  "gend.fWoman"           = "Gender: Woman",
  "gend.fNonbinaryDOther" = "Gender: Nonbinary / Other",
  "race.fAsian"           = "Ethnoracial: Asian",
  "race.fBlack"           = "Ethnoracial: Black",
  "race.fHispanic"        = "Ethnoracial: Hispanic / Latino",
  "race.fMixedOther"      = "Ethnoracial: Mixed Other",
  "race.fMixedWhite"      = "Ethnoracial: Mixed White"
)

PREDICTOR_DOMAINS <- c(
  "social_c"              = "Ideology",
  "economic_c"            = "Ideology",
  "educ_c"                = "Cultural Capital",
  "peduc_c"               = "Cultural Capital",
  "arts_c"                = "Cultural Capital",
  "highbrow_arts_c"       = "Dining & Arts Practices",
  "fancy_rest_c"          = "Dining & Arts Practices",
  "fast_food_c"           = "Dining & Arts Practices",
  "taste_authentic_c"     = "Taste Dispositions",
  "taste_familiar_c"      = "Taste Dispositions",
  "taste_light_c"         = "Taste Dispositions",
  "taste_rich_c"          = "Taste Dispositions",
  "cosmo_global_c"        = "Cosmopolitan Capital",
  "network_diversity_c"   = "Cosmopolitan Capital",
  "income_c"              = "Demographics",
  "age_c"                 = "Demographics",
  "gend.fWoman"           = "Demographics",
  "gend.fNonbinaryDOther" = "Demographics",
  "race.fAsian"           = "Ethnoracial Identification",
  "race.fBlack"           = "Ethnoracial Identification",
  "race.fHispanic"        = "Ethnoracial Identification",
  "race.fMixedOther"      = "Ethnoracial Identification",
  "race.fMixedWhite"      = "Ethnoracial Identification"
)

# -------------------------------------------------------------------------
# 2. Extraction Pipeline Across Factorial Registry
# -------------------------------------------------------------------------

extract_all_model_stability <- function() {
  cache_dir <- here("cache")
  completed_models <- list()
  all_fixed_list <- list()
  all_draws_list <- list()
  
  message("=== Scanning Factorial Model Registry for Completed Models ===")
  
  for (i in seq_len(nrow(MODEL_REGISTRY))) {
    row <- MODEL_REGISTRY[i, ]
    m_path <- get_model_path(row$domain, row$threshold, row$re, cache_dir)
    
    if (!is.null(m_path) && file.exists(m_path)) {
      message(sprintf("Loading [%s | %s | %s]: %s", 
                      row$domain, row$threshold, row$re, basename(m_path)))
      
      model_obj <- tryCatch(readRDS(m_path), error = function(e) NULL)
      
      if (!is.null(model_obj) && inherits(model_obj, "brmsfit")) {
        completed_models[[length(completed_models) + 1]] <- row$systematic_file
        dr <- as_draws_df(model_obj)
        all_cols <- names(dr)
        b_cols <- all_cols[grepl("^b_", all_cols) & !grepl("^b_Intercept", all_cols)]
        bcs_cols <- all_cols[grepl("^bcs_", all_cols)]
        
        # Standard scalar fixed effects
        for (col in b_cols) {
          vals <- dr[[col]]
          var_clean <- sub("^b_", "", col)
          
          if (var_clean %in% names(PREDICTOR_LABELS)) {
            all_draws_list[[length(all_draws_list) + 1]] <- tibble(
              model_key = row$systematic_file,
              domain = row$domain,
              term = var_clean,
              label = PREDICTOR_LABELS[var_clean],
              value = vals
            )
            
            all_fixed_list[[length(all_fixed_list) + 1]] <- tibble(
              domain = row$domain,
              domain_label = row$domain_label,
              threshold = row$threshold,
              threshold_label = row$threshold_label,
              re = row$re,
              re_label = row$re_label,
              model_file = row$systematic_file,
              term_raw = col,
              term = var_clean,
              label = unname(PREDICTOR_LABELS[var_clean] %||% var_clean),
              domain_group = unname(PREDICTOR_DOMAINS[var_clean] %||% "Other"),
              param_type = "scalar",
              transition = "global",
              median = median(vals),
              mean = mean(vals),
              sd = sd(vals),
              q025 = quantile(vals, 0.025, names = FALSE),
              q05 = quantile(vals, 0.05, names = FALSE),
              q95 = quantile(vals, 0.95, names = FALSE),
              q975 = quantile(vals, 0.975, names = FALSE),
              p_gt_0 = mean(vals > 0),
              p_lt_0 = mean(vals < 0)
            )
          }
        }
        
        # Category-specific effects
        if (length(bcs_cols) > 0) {
          cs_vars <- unique(sub("\\[[0-9]+\\]$", "", bcs_cols))
          
          for (cs_var in cs_vars) {
            var_clean <- sub("^bcs_", "", cs_var)
            cs_sub_cols <- bcs_cols[grepl(paste0("^", cs_var, "\\["), bcs_cols)]
            cs_matrix <- as.matrix(dr[, cs_sub_cols])
            avg_vals <- rowMeans(cs_matrix)
            
            if (var_clean %in% names(PREDICTOR_LABELS)) {
              all_draws_list[[length(all_draws_list) + 1]] <- tibble(
                model_key = row$systematic_file,
                domain = row$domain,
                term = var_clean,
                label = PREDICTOR_LABELS[var_clean],
                value = avg_vals
              )
              
              all_fixed_list[[length(all_fixed_list) + 1]] <- tibble(
                domain = row$domain,
                domain_label = row$domain_label,
                threshold = row$threshold,
                threshold_label = row$threshold_label,
                re = row$re,
                re_label = row$re_label,
                model_file = row$systematic_file,
                term_raw = paste0(cs_var, "_avg"),
                term = var_clean,
                label = unname(PREDICTOR_LABELS[var_clean] %||% var_clean),
                domain_group = unname(PREDICTOR_DOMAINS[var_clean] %||% "Other"),
                param_type = "cs_average",
                transition = "mean_1_6",
                median = median(avg_vals),
                mean = mean(avg_vals),
                sd = sd(avg_vals),
                q025 = quantile(avg_vals, 0.025, names = FALSE),
                q05 = quantile(avg_vals, 0.05, names = FALSE),
                q95 = quantile(avg_vals, 0.95, names = FALSE),
                q975 = quantile(avg_vals, 0.975, names = FALSE),
                p_gt_0 = mean(avg_vals > 0),
                p_lt_0 = mean(avg_vals < 0)
              )
            }
          }
        }
      }
    } else {
      message(sprintf("Skipping (not found yet): %s", row$systematic_file))
    }
  }
  
  all_fixed_df <- bind_rows(all_fixed_list)
  full_draws_df <- bind_rows(all_draws_list)
  
  saveRDS(all_fixed_df, here("cache/fixed_effects_all_models.rds"))
  write_csv(all_fixed_df, here("cache/fixed_effects_all_models.csv"))
  message(sprintf("Saved granular fixed effects: cache/fixed_effects_all_models.rds (%d rows across %d model specifications)", 
                  nrow(all_fixed_df), length(completed_models)))
  
  # Stability Summary
  primary_estimates <- all_fixed_df |>
    filter(param_type %in% c("scalar", "cs_average"))
  
  stability_summary <- primary_estimates |>
    group_by(term, label, domain_group) |>
    summarize(
      k_models = n(),
      models_evaluated = paste(unique(domain), collapse = ", "),
      min_median = min(median),
      max_median = max(median),
      grand_mean_median = mean(median),
      min_q025 = min(q025),
      max_q975 = max(q975),
      min_p_gt_0 = min(p_gt_0),
      max_p_gt_0 = max(p_gt_0),
      consensus_direction = case_when(
        all(p_gt_0 >= 0.95) | (mean(p_gt_0 >= 0.95) >= 0.70) ~ "Credibly Positive (>=95%)",
        all(p_lt_0 >= 0.95) | (mean(p_lt_0 >= 0.95) >= 0.70) ~ "Credibly Negative (>=95%)",
        all(p_gt_0 > 0.05 & p_gt_0 < 0.95) ~ "Consistently Spans Zero",
        TRUE ~ "Specification Contingent"
      ),
      .groups = "drop"
    ) |>
    arrange(grand_mean_median)
  
  saveRDS(stability_summary, here("cache/fixed_effects_stability_summary.rds"))
  write_csv(stability_summary, here("cache/fixed_effects_stability_summary.csv"))
  message("Saved consensus summary: cache/fixed_effects_stability_summary.rds")
  
  # Render Stability Forest Plot with Half-Eye Information
  generate_stability_plot(full_draws_df, stability_summary)
  
  return(list(granular = all_fixed_df, summary = stability_summary, draws = full_draws_df))
}

# -------------------------------------------------------------------------
# 3. Plotting Function with Half-Eye Posteriors & Specification Stability
# -------------------------------------------------------------------------

generate_stability_plot <- function(full_draws_df, stability_summary) {
  # Sort term_order strictly by grand mean effect size from bottom to top
  term_order_df <- stability_summary |>
    filter(!term %in% c("gend.fNonbinaryDOther")) |>
    arrange(grand_mean_median)
  
  term_order <- term_order_df$label
  
  # Compute pooled credibility mass across all posterior draws
  var_draw_stats <- full_draws_df |>
    filter(!term %in% c("gend.fNonbinaryDOther")) |>
    group_by(label) |>
    summarise(
      overall_p_pos = mean(value > 0),
      overall_p_neg = mean(value < 0),
      .groups = "drop"
    )
  
  var_colors <- stability_summary |>
    filter(!term %in% c("gend.fNonbinaryDOther")) |>
    mutate(
      color_cat = case_when(
        label %in% c("Social Conservatism", "Educational Attainment", "Adult Highbrow Arts Attendance", 
                     "Fine Dining Frequency", "Friendship Network Diversity") ~ "Pro-Chef Anchor",
        label %in% c("Ethnoracial: Mixed White", "Gender: Woman", "Dispositions: Exotic / Authentic", 
                     "Childhood Arts Socialization", "Age") ~ "Domestic Elder Anchor",
        TRUE ~ "Spans Zero / Neutral"
      ),
      color_cat = factor(color_cat, levels = c("Pro-Chef Anchor", "Domestic Elder Anchor", "Spans Zero / Neutral"))
    ) |>
    select(label, color_cat)
  
  plot_draws_df <- full_draws_df |>
    filter(!term %in% c("gend.fNonbinaryDOther")) |>
    left_join(var_colors, by = "label") |>
    mutate(
      label = factor(label, levels = term_order)
    )
  
  model_medians <- full_draws_df |>
    filter(!term %in% c("gend.fNonbinaryDOther")) |>
    group_by(label, model_key) |>
    summarise(median_val = median(value), .groups = "drop") |>
    mutate(label = factor(label, levels = term_order))
  
  color_map <- c(
    "Pro-Chef Anchor"       = "#0072B2",
    "Domestic Elder Anchor" = "#D55E00",
    "Spans Zero / Neutral"  = "gray60"
  )
  
  p <- ggplot(plot_draws_df, aes(x = value, y = label, fill = color_cat, color = color_cat)) +
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
      data = model_medians,
      aes(x = median_val, y = label),
      color = "gray15",
      size = 1.5,
      alpha = 0.50,
      shape = 3,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(
      values = color_map,
      name = "Consensus Credibility (≥ 95% Mass):"
    ) +
    scale_color_manual(
      values = color_map,
      name = "Consensus Credibility (≥ 95% Mass):"
    ) +
    scale_x_continuous(
      breaks = seq(-0.5, 0.4, by = 0.1),
      labels = function(x) sprintf("%+.1f", x)
    ) +
    coord_cartesian(xlim = c(-0.52, 0.38)) +
    labs(
      title = "Cross-Specification Stability Envelope: Consensus Posterior Distributions",
      subtitle = "Posterior distributions (half-eyes) synthesized across all 17 completed Bayesian models, ordered by mean effect size\nTick marks (+) represent individual model posterior medians demonstrating specification stability",
      x = "Estimated Shift in Log-Odds (Toward Professional Chef)",
      y = NULL,
      caption = "Half-eye density slabs, posterior medians, and 80%/95% CrIs synthesized across 17 completed Bayesian model specifications.\nTick marks (+) show individual model posterior medians (k = 17 for core predictors, k = 5 for domain extension predictors).\nPredictors ordered strictly along the y-axis by posterior mean effect size."
    ) +
    theme_minimal(base_family = "sans", base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12.5),
      plot.subtitle = element_text(color = "gray30", size = 9.5, margin = margin(b = 6)),
      plot.caption = element_text(color = "gray40", size = 8.5, margin = margin(t = 6)),
      axis.text.y = element_text(face = "bold", size = 9.2),
      legend.position = "bottom",
      legend.title = element_text(size = 9.5, face = "bold"),
      legend.text = element_text(size = 9),
      legend.box = "horizontal",
      legend.margin = margin(t = 2, b = 2),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5)
    ) +
    guides(fill = guide_legend(nrow = 1), color = guide_legend(nrow = 1))
  
  plot_path <- here("Plots/fixed_effects_stability_forest.png")
  dir.create(dirname(plot_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(plot_path, plot = p, width = 10.5, height = 9.5, dpi = 300, bg = "white")
  message(sprintf("Generated stability forest plot: %s", plot_path))
}

# Run pipeline if executed as script
if (!interactive() || identical(environment(), globalenv())) {
  results <- extract_all_model_stability()
  
  cat("\n=== Cross-Specification Fixed Effects Stability Summary ===\n\n")
  print(results$summary |> select(domain_group, label, k_models, min_median, max_median, grand_mean_median, consensus_direction), n = 30)
}
