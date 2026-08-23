#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(tidyverse)
  library(brms)
  library(tidybayes)
  library(ggrepel)
  library(here)
})

cat("===========================================\n")
cat("Generating Plots: Cuisine Authenticity\n")
cat("===========================================\n")

plot_dir <- here("Plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

cfile <- function(m) here("cache", paste0("hier_", m, ".rds"))
pfile <- function(p) file.path(plot_dir, paste0(p, ".png"))

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

# 1. WAIC Comparison
cat("1. Plotting WAIC comparison...\n")
waic_list <- list()
models_to_check <- list(
  "1. Baseline Strict" = m1,
  "2. Relaxed CS" = m2,
  "3. Random Slopes Strict" = m3,
  "4. Variance Strict" = m4,
  "5. Variance + Random Slopes" = m5,
  "6. Relaxed CS + Random Slopes" = m6
)

for(n in names(models_to_check)) {
  m <- models_to_check[[n]]
  if(!is.null(m) && !is.null(m$criteria$waic)) {
    w <- m$criteria$waic$estimates["waic", ]
    waic_list[[n]] <- data.frame(Model = n, WAIC = w["Estimate"], SE = w["SE"])
  }
}
if(length(waic_list) > 0) {
  waic_df <- bind_rows(waic_list) |>
    mutate(Delta_WAIC = WAIC - min(WAIC)) |>
    arrange(WAIC)
  
  saveRDS(waic_df, here("cache", "cuisine_waic_comparison.rds"))
  
  p_waic <- ggplot(waic_df, aes(x = reorder(Model, -WAIC), y = WAIC)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = WAIC - SE, ymax = WAIC + SE), width = 0.2) +
    coord_flip() +
    labs(title = "WAIC Comparison: Cuisine Authenticity", x = "", y = "WAIC (Lower is Better)") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold"))
  ggsave(pfile("model_fit_comparison"), p_waic, width = 10, height = 5, bg = "white")
}

if(!is.null(m5)) {
  # 2. Demographic Fixed Effects (Location)
  cat("2. Plotting Fixed Effects (Location)...\n")
  available_vars <- variables(m5)
  vars_to_extract <- c("b_educ_c", "b_peduc_c", "b_social_c", "b_economic_c", "b_income_c", "b_arts_c",
                       "b_age_c", "b_gend.fWoman", "b_gend.fNonbinaryDOther", 
                       "b_race.fAsian", "b_race.fBlack", "b_race.fHispanic", 
                       "b_race.fMixedOther", "b_race.fMixedWhite")
  vars_to_extract <- intersect(vars_to_extract, available_vars)
  
  draws_fixed <- m5 |>
    gather_draws(!!!syms(vars_to_extract)) |>
    mutate(
      Predictor = case_when(
        .variable == "b_social_c" ~ "Social Conservatism",
        .variable == "b_economic_c" ~ "Economic Conservatism",
        .variable == "b_educ_c" ~ "Education",
        .variable == "b_peduc_c" ~ "Parental Education",
        .variable == "b_arts_c" ~ "Childhood Arts Exposure",
        .variable == "b_income_c" ~ "Income",
        .variable == "b_age_c" ~ "Age",
        .variable == "b_gend.fWoman" ~ "Gender: Woman",
        .variable == "b_gend.fNonbinaryDOther" ~ "Gender: Nonbinary/Other",
        .variable == "b_race.fAsian" ~ "Race: Asian",
        .variable == "b_race.fBlack" ~ "Race: Black",
        .variable == "b_race.fHispanic" ~ "Race: Hispanic",
        .variable == "b_race.fMixedOther" ~ "Race: Mixed/Other",
        .variable == "b_race.fMixedWhite" ~ "Race: Mixed White",
        TRUE ~ .variable
      ),
      Category = case_when(
        grepl("social|economic", .variable) ~ "Ideology (SD)",
        grepl("educ|peduc|income|arts", .variable) ~ "Capital & Socialization (SD)",
        grepl("age", .variable) ~ "Demographics (SD)",
        grepl("gend", .variable) ~ "Gender (vs. Man)",
        grepl("race", .variable) ~ "Race (vs. White)"
      )
    ) |>
    group_by(Predictor) |>
    mutate(med_val = median(.value)) |>
    ungroup() |>
    mutate(Predictor = fct_reorder(Predictor, med_val))

  p_fixed <- ggplot(draws_fixed, aes(x = .value, y = Predictor, fill = Category)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 1) +
    stat_halfeye(alpha = 0.7, .width = c(0.8, 0.95)) +
    scale_fill_viridis_d(option = "turbo", end = 0.9) +
    theme_minimal(base_size = 14) +
    labs(
      title = "Demographic Fixed Effects on Cuisine Ratings",
      subtitle = "Posterior distributions from best-fitting model (Variance + Random Slopes)\nPositive values push ratings toward 'Professional Chef' (7) | Negative toward 'Elder' (1)",
      x = "Effect Size (Log-Odds Shift)",
      y = NULL,
      fill = "Variable Type"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray30", margin = margin(b = 15)),
      legend.position = "bottom",
      panel.grid.major.y = element_blank()
    )
  ggsave(pfile("demographic_fixed_effects"), p_fixed, width = 11, height = 8, dpi = 300, bg = "white")

  # 3. Cuisine Random Effects
  cat("3. Plotting Cuisine Random Intercepts...\n")
  cuisine_re <- m5 |> 
    spread_draws(r_cuisine[cuisine, term]) |>
    filter(term == "Intercept") |>
    group_by(cuisine) |>
    mutate(med_val = median(r_cuisine)) |>
    ungroup() |>
    mutate(
      genre_label = str_to_title(str_replace_all(cuisine, "_", " ")),
      genre_label = fct_reorder(genre_label, med_val)
    )
  p_re <- ggplot(cuisine_re, aes(x = r_cuisine, y = genre_label)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 1) +
    stat_halfeye(fill = "#2c3e50", alpha = 0.7, .width = c(0.8, 0.95)) +
    theme_minimal(base_size = 14) +
    labs(
      title = "Baseline Cuisine Authenticity (Random Intercepts)",
      x = "Location Intercept (Log-Odds)",
      y = NULL
    ) +
    theme(plot.title = element_text(face = "bold"), panel.grid.major.y = element_blank())
  ggsave(pfile("cuisine_random_effects"), p_re, width = 10, height = 8, dpi = 300, bg = "white")

  # 4. 2D Consensus Plot
  cat("4. Plotting 2D Consensus...\n")
  draws_loc <- m5 |> 
    spread_draws(r_cuisine[cuisine, term]) |>
    filter(term == "Intercept") |>
    rename(loc_effect = r_cuisine)

  draws_disc <- m5 |> 
    spread_draws(r_cuisine__disc[cuisine, term]) |>
    filter(term == "Intercept") |>
    rename(disc_effect = r_cuisine__disc)

  summary_df <- draws_loc |>
    left_join(draws_disc, by = c(".chain", ".iteration", ".draw", "cuisine")) |>
    group_by(cuisine) |>
    summarize(
      loc_med = median(loc_effect),
      loc_lower = quantile(loc_effect, 0.025),
      loc_upper = quantile(loc_effect, 0.975),
      disc_med = median(disc_effect),
      disc_lower = quantile(disc_effect, 0.025),
      disc_upper = quantile(disc_effect, 0.975),
      .groups = "drop"
    ) |>
    mutate(genre_label = str_to_title(str_replace_all(cuisine, "_", " ")))

  p_2d <- ggplot(summary_df, aes(x = loc_med, y = disc_med)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray60", alpha = 0.7) +
    geom_errorbar(aes(ymin = disc_lower, ymax = disc_upper), width = 0, color = "gray40", alpha = 0.5) +
    geom_errorbar(aes(xmin = loc_lower, xmax = loc_upper), orientation = "y", width = 0, color = "gray40", alpha = 0.5) +
    geom_point(size = 3, color = "#2c3e50") +
    geom_label_repel(aes(label = genre_label), size = 4.5, box.padding = 0.5, point.padding = 0.3, max.overlaps = 20) +
    coord_cartesian(ylim = c(-1.5, 1.0)) +
    theme_minimal(base_size = 14) +
    labs(
      title = "Cuisine Authenticity: Consensus vs. Preparation Preference",
      subtitle = "Posterior medians and 95% CIs for Cuisine Random Effects",
      x = "← Traditional Elder          Location (Intercept)                Professional Chef →",
      y = "← High Disagreement          Consensus (Disc Parameter)          High Agreement →"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray30", margin = margin(b = 15)),
      axis.title.x = element_text(margin = margin(t = 15), face = "bold"),
      axis.title.y = element_text(margin = margin(r = 15), face = "bold")
    )
  ggsave(pfile("cuisine_2d_consensus"), p_2d, width = 10, height = 8, dpi = 300, bg = "white")

  # 5. Demographic Effects on Global Variance
  cat("5. Plotting Demographic Effects on Variance...\n")
  available_var_vars <- variables(m5)
  var_vars_to_extract <- c("b_disc_educ_c", "b_disc_peduc_c", "b_disc_arts_c", "b_disc_social_c", "b_disc_economic_c")
  var_vars_to_extract <- intersect(var_vars_to_extract, available_var_vars)
  
  draws_disc_fixed <- m5 |>
    gather_draws(!!!syms(var_vars_to_extract)) |>
    mutate(
      Predictor = case_when(
        .variable == "b_disc_social_c" ~ "Social Conservatism",
        .variable == "b_disc_economic_c" ~ "Economic Conservatism",
        .variable == "b_disc_educ_c" ~ "Education",
        .variable == "b_disc_peduc_c" ~ "Parental Education",
        .variable == "b_disc_arts_c" ~ "Childhood Arts Exposure",
        TRUE ~ .variable
      ),
      Category = case_when(
        grepl("social|economic", .variable) ~ "Ideology",
        TRUE ~ "Cultural Capital / Socialization"
      )
    ) |>
    group_by(Predictor) |>
    mutate(med_val = median(.value)) |>
    ungroup() |>
    mutate(Predictor = fct_reorder(Predictor, med_val))

  p_disc <- ggplot(draws_disc_fixed, aes(x = .value, y = Predictor, fill = Category)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 1) +
    stat_halfeye(alpha = 0.7, .width = c(0.8, 0.95)) +
    scale_fill_brewer(palette = "Set1") +
    theme_minimal(base_size = 14) +
    labs(
      title = "Demographic Predictors of Consensus (Discrimination)",
      subtitle = "Positive values = Increased Consensus / Agreement (Lower Variance)\nNegative values = Decreased Consensus / Disagreement (Higher Variance)",
      x = "Effect on Log-Discrimination Parameter",
      y = NULL,
      fill = "Predictor Domain"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray30", margin = margin(b = 15)),
      legend.position = "bottom",
      panel.grid.major.y = element_blank()
    )
  ggsave(pfile("demographic_variance_effects_forest"), p_disc, width = 10, height = 6, dpi = 300, bg = "white")
}

cat("All plots generated successfully!\n")
