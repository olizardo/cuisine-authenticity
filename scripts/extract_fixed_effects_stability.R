#' @title Extract Fixed Effects & Cross-Specification Stability Range
#' @description Iterates across all fitted models in the factorial registry (Base, Practices,
#'   Dispositions, Cosmopolitan, Meta) to extract posterior draws for all fixed effects,
#'   compute the cross-specification stability envelope (min/max medians, credibility intervals),
#'   and output publication-ready synthesis tables and forest plots.

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
# 2. Extraction Functions
# -------------------------------------------------------------------------

#' Extract fixed effect draws and summaries from a single brmsfit model
extract_single_model_fixed <- function(model_obj, meta_row) {
  draws <- as_draws_df(model_obj)
  all_cols <- names(draws)
  
  # Standard fixed effects
  b_cols <- all_cols[grepl("^b_", all_cols) & !grepl("^b_Intercept", all_cols)]
  
  # Category-specific effects
  bcs_cols <- all_cols[grepl("^bcs_", all_cols)]
  
  res_list <- list()
  
  # Process standard scalar fixed effects
  for (col in b_cols) {
    vals <- draws[[col]]
    var_clean <- sub("^b_", "", col)
    
    res_list[[length(res_list) + 1]] <- tibble(
      domain = meta_row$domain,
      domain_label = meta_row$domain_label,
      threshold = meta_row$threshold,
      threshold_label = meta_row$threshold_label,
      re = meta_row$re,
      re_label = meta_row$re_label,
      model_file = meta_row$systematic_file,
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
  
  # Process CS effects if present
  if (length(bcs_cols) > 0) {
    cs_vars <- unique(sub("\\[[0-9]+\\]$", "", bcs_cols))
    
    for (cs_var in cs_vars) {
      var_clean <- sub("^bcs_", "", cs_var)
      cs_sub_cols <- bcs_cols[grepl(paste0("^", cs_var, "\\["), bcs_cols)]
      
      # Average across CS transitions per draw (representative summary)
      cs_matrix <- as.matrix(draws[, cs_sub_cols])
      avg_vals <- rowMeans(cs_matrix)
      
      res_list[[length(res_list) + 1]] <- tibble(
        domain = meta_row$domain,
        domain_label = meta_row$domain_label,
        threshold = meta_row$threshold,
        threshold_label = meta_row$threshold_label,
        re = meta_row$re,
        re_label = meta_row$re_label,
        model_file = meta_row$systematic_file,
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
      
      # Cutpoint transitions (1 through 6)
      for (col in cs_sub_cols) {
        step <- sub(".*\\[([0-9]+)\\]$", "\\1", col)
        vals <- draws[[col]]
        res_list[[length(res_list) + 1]] <- tibble(
          domain = meta_row$domain,
          domain_label = meta_row$domain_label,
          threshold = meta_row$threshold,
          threshold_label = meta_row$threshold_label,
          re = meta_row$re,
          re_label = meta_row$re_label,
          model_file = meta_row$systematic_file,
          term_raw = col,
          term = var_clean,
          label = unname(PREDICTOR_LABELS[var_clean] %||% var_clean),
          domain_group = unname(PREDICTOR_DOMAINS[var_clean] %||% "Other"),
          param_type = "cs_step",
          transition = step,
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
  }
  
  bind_rows(res_list)
}

# -------------------------------------------------------------------------
# 3. Main Extraction Pipeline
# -------------------------------------------------------------------------

extract_all_model_stability <- function() {
  message("=== Scanning Factorial Model Registry for Completed Models ===")
  
  completed_models <- list()
  
  for (i in seq_len(nrow(MODEL_REGISTRY))) {
    row <- MODEL_REGISTRY[i, ]
    path <- get_model_path(row$systematic_file)
    
    if (file.exists(path)) {
      message(sprintf("Loading [%s | %s | %s]: %s", row$domain, row$threshold, row$re, basename(path)))
      m <- readRDS(path)
      df_fixed <- extract_single_model_fixed(m, row)
      completed_models[[length(completed_models) + 1]] <- df_fixed
      rm(m); gc(verbose = FALSE)
    } else {
      message(sprintf("Skipping (not found yet): %s", row$systematic_file))
    }
  }
  
  if (length(completed_models) == 0) {
    stop("No cached models found in cache/")
  }
  
  all_fixed_df <- bind_rows(completed_models)
  
  # Save granular long format
  saveRDS(all_fixed_df, here("cache/fixed_effects_all_models.rds"))
  write_csv(all_fixed_df, here("cache/fixed_effects_all_models.csv"))
  message(sprintf("Saved granular fixed effects: cache/fixed_effects_all_models.rds (%d rows across %d model specifications)", 
                  nrow(all_fixed_df), length(completed_models)))
  
  # -----------------------------------------------------------------------
  # 4. Compute Stability Summary (Global / Transition-Averaged)
  # -----------------------------------------------------------------------
  
  # Filter to primary comparable scalar / transition-averaged estimates
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
        all(p_gt_0 >= 0.95) ~ "Credibly Positive (>=95%)",
        all(p_lt_0 >= 0.95) ~ "Credibly Negative (>=95%)",
        all(p_gt_0 > 0.05 & p_gt_0 < 0.95) ~ "Consistently Spans Zero",
        TRUE ~ "Specification Contingent"
      ),
      .groups = "drop"
    ) |>
    arrange(domain_group, desc(abs(grand_mean_median)))
  
  saveRDS(stability_summary, here("cache/fixed_effects_stability_summary.rds"))
  write_csv(stability_summary, here("cache/fixed_effects_stability_summary.csv"))
  message("Saved consensus summary: cache/fixed_effects_stability_summary.rds")
  
  # -----------------------------------------------------------------------
  # 5. Render Stability Forest Plot
  # -----------------------------------------------------------------------
  generate_stability_plot(primary_estimates, stability_summary)
  
  return(list(granular = all_fixed_df, summary = stability_summary))
}

# -------------------------------------------------------------------------
# 6. Plotting Function
# -------------------------------------------------------------------------

generate_stability_plot <- function(primary_estimates, stability_summary) {
  # Ordering of domain groups from bottom to top
  domain_levels <- c(
    "Ethnoracial Identification",
    "Demographics",
    "Cultural Capital",
    "Dining & Arts Practices",
    "Taste Dispositions",
    "Cosmopolitan Capital",
    "Ideology"
  )
  
  # Clean up categories for plotting
  plot_df <- primary_estimates |>
    filter(!term %in% c("gend.fNonbinaryDOther")) |>
    mutate(
      domain_group = factor(domain_group, levels = domain_levels),
      spec_label = sprintf("%s (%s, %s)", domain_label, threshold, toupper(re)),
      color_cat = case_when(
        p_gt_0 >= 0.95 ~ "Pro-Chef Anchor",
        p_lt_0 >= 0.95 ~ "Domestic Elder Anchor",
        TRUE ~ "Spans Zero / Neutral"
      )
    )
  
  # Within each domain group, sort by grand mean median
  term_order_df <- stability_summary |>
    filter(!term %in% c("gend.fNonbinaryDOther")) |>
    mutate(domain_group = factor(domain_group, levels = domain_levels)) |>
    arrange(domain_group, grand_mean_median)
  
  term_order <- term_order_df$label
  
  plot_df <- plot_df |>
    mutate(
      label = factor(label, levels = term_order),
      color_cat = factor(color_cat, levels = c(
        "Pro-Chef Anchor",
        "Domestic Elder Anchor",
        "Spans Zero / Neutral"
      ))
    )
  
  p <- ggplot(plot_df, aes(x = median, y = label, xmin = q025, xmax = q975, color = color_cat)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.7) +
    geom_pointrange(
      position = position_identity(),
      alpha = 0.55,
      size = 0.45,
      linewidth = 0.6
    ) +
    scale_color_manual(
      values = c(
        "Pro-Chef Anchor"          = "#0072B2",
        "Domestic Elder Anchor"    = "#D55E00",
        "Spans Zero / Neutral"     = "gray55"
      ),
      name = "Consensus Credibility (≥ 95% Mass):"
    ) +
    scale_x_continuous(
      breaks = seq(-0.5, 0.4, by = 0.1),
      labels = function(x) sprintf("%+.1f", x)
    ) +
    coord_cartesian(xlim = c(-0.52, 0.38)) +
    labs(
      title = "Cross-Specification Stability Envelope: Fixed Effects",
      subtitle = "Posterior medians and 95% credible intervals across estimated Bayesian model specifications\nAll specifications overlaid along the single horizontal gridline for each predictor",
      x = "Estimated Shift in Log-Odds (Toward Professional Chef)",
      y = NULL,
      caption = "Points represent posterior medians across estimated models (k = 16 for core predictors, k = 4 for domain extensions).\nEstimates for relaxed models reflect transition-averaged category-specific parameters."
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
    guides(color = guide_legend(nrow = 1))
  
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
