#' @title Extract Cuisine Random Slopes Stability & Generate Consensus Plots
#' @description Iterates across all fitted random slope models in the factorial registry
#'   to extract posterior draws for cuisine random slopes (net slopes = global fixed effect + cuisine deviation),
#'   computes consensus posterior summaries across all models containing each slope,
#'   and generates publication-ready consensus half-eye forest plots for Section 8 of the report.
#' @details Generates:
#'   - Figure 9: Plots/rs_cuisine_slopes_ideology.png (Social & Economic Conservatism)
#'   - Figure 10: Plots/rs_cuisine_slopes_cultural.png (Education, Parental Education, Childhood Arts)
#'   - Figure 11: Plots/rs_cuisine_slopes_practices.png (Highbrow Arts, Fine Dining, Fast Food)
#'   - Figure 12: Plots/rs_cuisine_slopes_dispositions.png (Exotic/Authentic, Familiar/Comfort)
#'   - Figure 13: Plots/rs_cuisine_slopes_cosmopolitan.png (Global Citizen, Friendship Network Diversity)
#'   - cache/random_slopes_stability_summary.csv & .rds

suppressPackageStartupMessages({
  library(brms)
  library(tidybayes)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(ggdist)
  library(readr)
  library(here)
})

source(here("scripts", "model_registry.R"))

plot_dir <- here("Plots")
cache_dir <- here("cache")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

# Consecration hierarchy mapping
cuisine_groups_map <- c(
  "French"          = "Consecrated",
  "Japanese"        = "Consecrated",
  "Swedish"         = "Consecrated",
  "Korean"          = "Intermediate",
  "Italian"         = "Intermediate",
  "Vietnamese"      = "Intermediate",
  "Peruvian"        = "Intermediate",
  "Moroccan"        = "Intermediate",
  "Lebanese"        = "Intermediate",
  "Pakistani"       = "Subaltern",
  "Mexican"         = "Subaltern",
  "Ethiopian"       = "Subaltern",
  "Jamaican"        = "Subaltern",
  "Nigerian"        = "Subaltern",
  "Native American" = "Subaltern"
)
group_levels <- c("Consecrated", "Intermediate", "Subaltern")

cuisine_labels_clean <- c(
  "japanese"        = "Japanese",
  "french"          = "French",
  "italian"         = "Italian",
  "mexican"         = "Mexican",
  "moroccan"        = "Moroccan",
  "korean"          = "Korean",
  "peruvian"        = "Peruvian",
  "native_american" = "Native American",
  "swedish"         = "Swedish",
  "pakistani"       = "Pakistani",
  "ethiopian"       = "Ethiopian",
  "vietnamese"      = "Vietnamese",
  "nigerian"        = "Nigerian",
  "jamaican"        = "Jamaican",
  "lebanese"        = "Lebanese"
)

color_credibility <- c(
  "Credibly Positive (≥ 95% Mass)" = "#0072B2", # Okabe-Ito Blue
  "Credibly Negative (≥ 95% Mass)" = "#D55E00", # Okabe-Ito Vermillion
  "Spans Zero (< 95% Mass)"        = "gray60"
)

theme_cuisine <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size * 1.15, margin = margin(b = 5)),
      plot.subtitle = element_text(size = base_size * 0.88, color = "gray25", margin = margin(b = 10)),
      plot.caption = element_text(size = base_size * 0.72, color = "gray40", hjust = 0, margin = margin(t = 10)),
      axis.title.x = element_text(face = "bold", size = base_size * 0.95, margin = margin(t = 8)),
      axis.title.y = element_text(face = "bold", size = base_size * 0.95, margin = margin(r = 8)),
      axis.text.y = element_text(face = "bold", size = base_size * 0.88, color = "gray15"),
      axis.text.x = element_text(size = base_size * 0.82),
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
# 1. Extraction Pipeline Across All RS Models
# -------------------------------------------------------------------------
extract_random_slopes_consensus <- function(vars) {
  all_slopes_list <- list()
  
  for (i in seq_len(nrow(MODEL_REGISTRY))) {
    row <- MODEL_REGISTRY[i, ]
    if (row$re != "rs") next
    
    sys_path <- file.path(cache_dir, row$systematic_file)
    leg_path <- file.path(cache_dir, row$legacy_file)
    target_path <- if (file.exists(sys_path)) sys_path else if (file.exists(leg_path)) leg_path else NULL
    
    if (is.null(target_path)) next
    
    m <- tryCatch(readRDS(target_path), error = function(e) NULL)
    if (is.null(m) || !inherits(m, "brmsfit")) next
    
    r_vars <- grep("^r_cuisine\\[", variables(m), value = TRUE)
    avail_terms <- unique(sub("^r_cuisine\\[[^,]+,(.+)\\]$", "\\1", r_vars))
    
    matched_vars <- intersect(vars, avail_terms)
    if (length(matched_vars) == 0) next
    
    cat(sprintf("Extracting from %s (%s, %s) for terms: %s\n", 
                row$systematic_file, row$domain_label, row$threshold_label, paste(matched_vars, collapse = ", ")))
    
    for (v in matched_vars) {
      cs_param <- paste0("bcs_", v)
      b_param <- paste0("b_", v)
      
      if (cs_param %in% variables(m) || paste0("bcs_", v, "[1]") %in% variables(m)) {
        cs_sym <- rlang::sym(cs_param)
        fe_draws <- m %>%
          spread_draws((!!cs_sym)[threshold]) %>%
          group_by(.chain, .iteration, .draw) %>%
          summarize(fe_val = mean(!!cs_sym), .groups = "drop")
        
        re_draws <- m %>%
          spread_draws(r_cuisine[cuisine, term]) %>%
          filter(term == v) %>%
          left_join(fe_draws, by = c(".chain", ".iteration", ".draw")) %>%
          mutate(
            model_key = row$systematic_file,
            domain = row$domain,
            domain_label = row$domain_label,
            threshold = row$threshold,
            threshold_label = row$threshold_label,
            model_label = sprintf("%s [%s]", row$domain_label, ifelse(row$threshold == "relaxed", "CS", "PO")),
            term = v,
            total_slope = fe_val + r_cuisine
          )
      } else if (b_param %in% variables(m)) {
        re_draws <- m %>%
          spread_draws(r_cuisine[cuisine, term], !!sym(b_param)) %>%
          filter(term == v) %>%
          mutate(
            model_key = row$systematic_file,
            domain = row$domain,
            domain_label = row$domain_label,
            threshold = row$threshold,
            threshold_label = row$threshold_label,
            model_label = sprintf("%s [%s]", row$domain_label, ifelse(row$threshold == "relaxed", "CS", "PO")),
            term = v,
            total_slope = !!sym(b_param) + r_cuisine
          )
      } else {
        next
      }
      
      all_slopes_list[[length(all_slopes_list) + 1]] <- re_draws %>%
        select(model_key, domain, domain_label, threshold, threshold_label, model_label, cuisine, term, total_slope, .chain, .iteration, .draw)
    }
  }
  bind_rows(all_slopes_list)
}

# -------------------------------------------------------------------------
# 2. Plotting Function for Consensus Random Slopes
# -------------------------------------------------------------------------
plot_consensus_cuisine_slopes <- function(slopes_df, vars, var_labels, title, subtitle, filename, order_by_var = vars[1], width = 11.5, height = 8.5) {
  df <- slopes_df %>%
    filter(term %in% vars) %>%
    mutate(
      cuisine_label = cuisine_labels_clean[as.character(cuisine)],
      predictor_label = factor(var_labels[term], levels = unname(var_labels[vars])),
      cuisine_group = factor(cuisine_groups_map[cuisine_label], levels = group_levels)
    )
  
  # Consensus summary across all models
  consensus_summary <- df %>%
    group_by(predictor_label, term, cuisine_group, cuisine_label) %>%
    summarize(
      k_models = n_distinct(model_key),
      consensus_mean = mean(total_slope),
      consensus_median = median(total_slope),
      q025 = quantile(total_slope, 0.025),
      q975 = quantile(total_slope, 0.975),
      p_pos = mean(total_slope > 0),
      p_neg = mean(total_slope < 0),
      cred_status = case_when(
        p_pos >= 0.95 ~ "Credibly Positive (≥ 95% Mass)",
        p_neg >= 0.95 ~ "Credibly Negative (≥ 95% Mass)",
        TRUE ~ "Spans Zero (< 95% Mass)"
      ),
      .groups = "drop"
    )
  
  # Individual model medians for specification stability tick marks
  model_medians <- df %>%
    group_by(predictor_label, term, cuisine_group, cuisine_label, model_key, model_label) %>%
    summarize(
      model_median = median(total_slope),
      .groups = "drop"
    )
  
  # Determine ordering of cuisines within facets
  if (!is.null(order_by_var) && order_by_var %in% names(var_labels)) {
    target_label <- var_labels[order_by_var]
    cuis_order <- consensus_summary %>%
      filter(predictor_label == target_label) %>%
      arrange(consensus_median) %>%
      pull(cuisine_label)
  } else {
    cuis_order <- consensus_summary %>%
      group_by(cuisine_label) %>%
      summarize(grand_med = mean(consensus_median), .groups = "drop") %>%
      arrange(grand_med) %>%
      pull(cuisine_label)
  }
  
  df <- df %>%
    left_join(consensus_summary %>% select(predictor_label, cuisine_label, cred_status), by = c("predictor_label", "cuisine_label")) %>%
    mutate(
      cuisine_label = factor(cuisine_label, levels = cuis_order),
      cred_status = factor(cred_status, levels = c("Credibly Positive (≥ 95% Mass)", "Credibly Negative (≥ 95% Mass)", "Spans Zero (< 95% Mass)"))
    )
  
  model_medians <- model_medians %>%
    mutate(cuisine_label = factor(cuisine_label, levels = cuis_order))
  
  k_mod_text <- n_distinct(df$model_key)
  caption_text <- sprintf(
    "Consensus posterior density slabs, medians, and 80%%/95%% CrIs synthesized across %d Bayesian random-slope model specifications.\nTick marks (+) represent individual model posterior medians demonstrating cross-specification stability.\nCuisines grouped into baseline consecration tiers and ordered by consensus median net slope within each tier.",
    k_mod_text
  )
  
  p <- ggplot(df, aes(x = total_slope, y = cuisine_label, fill = cred_status, color = cred_status)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.7) +
    stat_halfeye(
      point_interval = median_qi,
      .width = c(0.80, 0.95),
      point_size = 2.8,
      interval_size = 1.0,
      slab_alpha = 0.15,
      scale = 0.65
    ) +
    geom_point(
      data = model_medians,
      aes(x = model_median, y = cuisine_label),
      color = "gray15",
      size = 1.6,
      alpha = 0.55,
      shape = 3,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(values = color_credibility, name = "Consensus Credibility (≥ 95% Posterior Mass)") +
    scale_color_manual(values = color_credibility, name = "Consensus Credibility (≥ 95% Posterior Mass)") +
    facet_grid(cuisine_group ~ predictor_label, scales = "free", space = "free_y") +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Net Effect (Log-Odds Shift per 1 SD Increase)",
      y = NULL,
      caption = caption_text
    ) +
    theme_cuisine(base_size = 11) +
    theme(
      strip.text = element_text(face = "bold", size = 10, color = "gray15"),
      strip.background = element_rect(fill = "gray95", color = NA),
      axis.text.y = element_text(face = "bold", size = 9, color = "gray15"),
      panel.spacing = unit(0.9, "lines"),
      legend.position = "bottom"
    )
  
  if (!is.null(filename)) {
    out_path <- file.path(plot_dir, filename)
    ggsave(out_path, p, width = width, height = height, dpi = 300, bg = "white")
    cat(sprintf("Saved: %s\n", out_path))
  }
  return(list(plot = p, summary = consensus_summary, model_medians = model_medians))
}

# -------------------------------------------------------------------------
# 3. Extract All Random Slopes & Generate Section 8 Figures
# -------------------------------------------------------------------------
cat("========================================================================\n")
cat("Extracting Random Slopes Across All Model Specifications\n")
cat("========================================================================\n")

all_vars <- c(
  "social_c", "economic_c",
  "educ_c", "peduc_c", "arts_c",
  "highbrow_arts_c", "fancy_rest_c", "fast_food_c",
  "taste_authentic_c", "taste_familiar_c",
  "cosmo_global_c", "network_diversity_c"
)

all_slopes_df <- extract_random_slopes_consensus(all_vars)

# Figure 9: Political Ideology Slopes
cat("\nGenerating Figure 9: Political Ideology Slopes...\n")
res_ideo <- plot_consensus_cuisine_slopes(
  all_slopes_df,
  vars = c("social_c", "economic_c"),
  var_labels = c("social_c" = "Social Conservatism", "economic_c" = "Economic Conservatism"),
  title = "Cuisine-Specific Ideological Effects on Authenticity Orientation: Cross-Model Consensus",
  subtitle = "Consensus net slopes (Global Fixed Effect + Cuisine-Specific Random Deviation u1) across all random slope models\nGrouped by Baseline Consecration Tier; tick marks (+) indicate individual model posterior medians",
  filename = "rs_cuisine_slopes_ideology.png",
  order_by_var = "social_c",
  width = 11.5, height = 8.5
)

# Figure 10: Cultural Capital Slopes
cat("\nGenerating Figure 10: Cultural Capital Slopes...\n")
res_cult <- plot_consensus_cuisine_slopes(
  all_slopes_df,
  vars = c("educ_c", "peduc_c", "arts_c"),
  var_labels = c(
    "educ_c"  = "Educational Attainment",
    "peduc_c" = "Parental Education",
    "arts_c"  = "Childhood Arts Exposure"
  ),
  title = "Cuisine-Specific Cultural Capital Effects on Authenticity Orientation: Cross-Model Consensus",
  subtitle = "Consensus net slopes across all random slope models grouped by Consecration Tier\nOrdered by Educational Attainment effect; tick marks (+) indicate individual model posterior medians",
  filename = "rs_cuisine_slopes_cultural.png",
  order_by_var = "educ_c",
  width = 13.5, height = 8.5
)

# Figure 11: Dining Practices Slopes
cat("\nGenerating Figure 11: Dining Practices Slopes...\n")
res_prac <- plot_consensus_cuisine_slopes(
  all_slopes_df,
  vars = c("highbrow_arts_c", "fancy_rest_c", "fast_food_c"),
  var_labels = c(
    "highbrow_arts_c" = "Adult Highbrow Arts Attendance",
    "fancy_rest_c"    = "Fine Dining Frequency",
    "fast_food_c"     = "Fast Food Frequency"
  ),
  title = "Cuisine-Specific Dining & Cultural Practices Effects: Cross-Model Consensus",
  subtitle = "Consensus net slopes across all random slope models grouped by Consecration Tier\nOrdered by Fine Dining Frequency effect; tick marks (+) indicate individual model posterior medians",
  filename = "rs_cuisine_slopes_practices.png",
  order_by_var = "fancy_rest_c",
  width = 13.5, height = 8.5
)

# Figure 12: Taste Dispositions Slopes
cat("\nGenerating Figure 12: Taste Dispositions Slopes...\n")
res_disp <- plot_consensus_cuisine_slopes(
  all_slopes_df,
  vars = c("taste_authentic_c", "taste_familiar_c"),
  var_labels = c(
    "taste_authentic_c" = "Exotic & Authentic Taste",
    "taste_familiar_c"  = "Familiar & Conventional Taste"
  ),
  title = "Cuisine-Specific Taste Dispositions Effects: Cross-Model Consensus",
  subtitle = "Consensus net slopes across all random slope models grouped by Consecration Tier\nOrdered by Exotic & Authentic Taste effect; tick marks (+) indicate individual model posterior medians",
  filename = "rs_cuisine_slopes_dispositions.png",
  order_by_var = "taste_authentic_c",
  width = 11.5, height = 8.5
)

# Figure 13: Cosmopolitan Capital Slopes
cat("\nGenerating Figure 13: Cosmopolitan Capital Slopes...\n")
res_cosmo <- plot_consensus_cuisine_slopes(
  all_slopes_df,
  vars = c("cosmo_global_c", "network_diversity_c"),
  var_labels = c(
    "cosmo_global_c"      = "Global Citizen Identity",
    "network_diversity_c" = "Friendship Network Diversity"
  ),
  title = "Cuisine-Specific Cosmopolitan Capital & Networks Effects: Cross-Model Consensus",
  subtitle = "Consensus net slopes across all random slope models grouped by Consecration Tier\nOrdered by Friendship Network Diversity effect; tick marks (+) indicate individual model posterior medians",
  filename = "rs_cuisine_slopes_cosmopolitan.png",
  order_by_var = "network_diversity_c",
  width = 11.5, height = 8.5
)

# -------------------------------------------------------------------------
# 4. Save Comprehensive Random Slopes Stability Summary
# -------------------------------------------------------------------------
full_summary <- bind_rows(
  res_ideo$summary,
  res_cult$summary,
  res_prac$summary,
  res_disp$summary,
  res_cosmo$summary
)

saveRDS(full_summary, file.path(cache_dir, "random_slopes_stability_summary.rds"))
write_csv(full_summary, file.path(cache_dir, "random_slopes_stability_summary.csv"))
cat("\nSaved random_slopes_stability_summary.csv and .rds to cache/\n")
cat("All Section 8 consensus random slope plots generated successfully!\n")
