#!/usr/bin/env Rscript
#' @title Bayesian Extension Models Full Taxonomy Suite
#' @description Fits the complete 2x2 factorial nesting grid for each theoretical extension domain:
#'   Domains: Practices (P), Dispositions (D), Cosmopolitan (C)
#'   Cells per domain:
#'     Cell 1: Strict Proportional Odds + Random Intercepts (RI)
#'     Cell 2: Relaxed Category-Specific (CS) + Random Intercepts (RI)
#'     Cell 3: Strict Proportional Odds + Full Random Slopes (RS)
#'     Cell 4: Relaxed Category-Specific (CS) + Full Random Slopes (RS) [handled by fit_ext_models_relaxed.R]
#'
#' Task ID Mapping (1-9):
#'   1: Practices - Strict RI
#'   2: Practices - Relaxed RI
#'   3: Practices - Strict RS
#'   4: Dispositions - Strict RI
#'   5: Dispositions - Relaxed RI
#'   6: Dispositions - Strict RS
#'   7: Cosmopolitan - Strict RI
#'   8: Cosmopolitan - Relaxed RI
#'   9: Cosmopolitan - Strict RS

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(readr)
  library(purrr)
  library(brms)
  library(cmdstanr)
  library(here)
})

# Dynamic HPC Core Allocation
n_slots <- as.numeric(Sys.getenv("NSLOTS", unset = "4"))
threads_per_chain <- max(1, floor(n_slots / 4))
cat(sprintf("Allocated NSLOTS: %d | Threads per chain: %d\n", n_slots, threads_per_chain))

# Check for task ID from environment or command line args
args <- commandArgs(trailingOnly = TRUE)
task_id <- as.numeric(Sys.getenv("SGE_TASK_ID", unset = ifelse(length(args) > 0, args[1], "0")))
cat(sprintf("Running Task ID: %d\n", task_id))

# Load prepared extended data
if (file.exists(here("data", "extended_cuisine_data.rds"))) {
  data_list <- readRDS(here("data", "extended_cuisine_data.rds"))
} else {
  source(here("scripts", "data_prep_extension.R"))
  data_list <- prepare_extended_data()
}

dat_long <- data_list$stacked

# Helper fitting function
run_brm <- function(formula, data, file_name, desc) {
  cat(sprintf("\n========================================================================\n"))
  cat(sprintf(">>> Fitting %s\n", desc))
  cat(sprintf(">>> Output: cache/%s.rds\n", file_name))
  cat(sprintf("========================================================================\n"))
  
  out_path <- here("cache", file_name)
  if (!grepl("\\.rds$", out_path)) out_path <- paste0(out_path, ".rds")
  
  fit <- brm(
    formula = formula,
    data = data,
    family = acat("logit"),
    prior = c(
      prior(normal(0, 1.5), class = "Intercept"),
      prior(normal(0, 1), class = "b")
    ),
    chains = 4,
    cores = min(4, n_slots),
    iter = 2000,
    warmup = 1000,
    seed = 1234,
    control = list(adapt_delta = 0.90),
    backend = "cmdstanr",
    threads = threading(threads_per_chain),
    file = here("cache", file_name)
  )
  
  # Ensure WAIC is computed and safely saved on cluster
  fit$file <- NULL
  if (is.null(fit$criteria$waic)) {
    cat(sprintf("\n>>> Computing WAIC on cluster (1 core, memory-safe)...\n"))
    fit <- add_criterion(fit, "waic", cores = 1, file = NULL)
    cat(sprintf(">>> Saving updated model with WAIC to: %s\n", out_path))
    saveRDS(fit, file = out_path)
  }
  
  return(fit)
}

# -------------------------------------------------------------
# Data Subsets
# -------------------------------------------------------------
dat_prac <- dat_long |>
  filter(
    !is.na(rating), !is.na(social_c), !is.na(economic_c), !is.na(educ_c),
    !is.na(peduc_c), !is.na(arts_c), !is.na(income_c), !is.na(age_c),
    !is.na(gend.f), !is.na(race.f), !is.na(fancy_rest_c), !is.na(fast_food_c), !is.na(highbrow_arts_c)
  )

dat_disp <- dat_long |>
  filter(
    !is.na(rating), !is.na(social_c), !is.na(economic_c), !is.na(educ_c),
    !is.na(peduc_c), !is.na(arts_c), !is.na(income_c), !is.na(age_c),
    !is.na(gend.f), !is.na(race.f), !is.na(taste_authentic_c), !is.na(taste_familiar_c),
    !is.na(taste_light_c), !is.na(taste_rich_c)
  )

dat_cosmo <- dat_long |>
  filter(
    !is.na(rating), !is.na(social_c), !is.na(economic_c), !is.na(educ_c),
    !is.na(peduc_c), !is.na(arts_c), !is.na(income_c), !is.na(age_c),
    !is.na(gend.f), !is.na(race.f), !is.na(cosmo_global_c), !is.na(network_diversity_c)
  )

# -------------------------------------------------------------
# Model Definitions
# -------------------------------------------------------------

# --- PRACTICES ---
# Task 1: Practices - Strict RI
f_prac_strict_ri <- bf(
  rating_ord ~ 1 + educ_c + peduc_c + arts_c + social_c + economic_c + 
    fancy_rest_c + fast_food_c + highbrow_arts_c + 
    income_c + age_c + gend.f + race.f + 
    (1 | respondent_id) + (1 | cuisine)
)

# Task 2: Practices - Relaxed RI
f_prac_relaxed_ri <- bf(
  rating_ord ~ 1 + cs(educ_c) + cs(peduc_c) + cs(arts_c) + cs(social_c) + cs(economic_c) + 
    cs(fancy_rest_c) + cs(fast_food_c) + cs(highbrow_arts_c) + 
    income_c + age_c + gend.f + race.f + 
    (1 | respondent_id) + (1 | cuisine)
)

# Task 3: Practices - Strict RS
f_prac_strict_rs <- bf(
  rating_ord ~ 1 + educ_c + peduc_c + arts_c + social_c + economic_c + 
    fancy_rest_c + fast_food_c + highbrow_arts_c + 
    income_c + age_c + gend.f + race.f + 
    (1 | respondent_id) + 
    (1 + educ_c + peduc_c + arts_c + social_c + economic_c + fancy_rest_c + fast_food_c + highbrow_arts_c | cuisine)
)

# --- DISPOSITIONS ---
# Task 4: Dispositions - Strict RI
f_disp_strict_ri <- bf(
  rating_ord ~ 1 + educ_c + peduc_c + arts_c + social_c + economic_c + 
    taste_authentic_c + taste_familiar_c + taste_light_c + taste_rich_c + 
    income_c + age_c + gend.f + race.f + 
    (1 | respondent_id) + (1 | cuisine)
)

# Task 5: Dispositions - Relaxed RI
f_disp_relaxed_ri <- bf(
  rating_ord ~ 1 + cs(educ_c) + cs(peduc_c) + cs(arts_c) + cs(social_c) + cs(economic_c) + 
    cs(taste_authentic_c) + cs(taste_familiar_c) + cs(taste_light_c) + cs(taste_rich_c) + 
    income_c + age_c + gend.f + race.f + 
    (1 | respondent_id) + (1 | cuisine)
)

# Task 6: Dispositions - Strict RS
f_disp_strict_rs <- bf(
  rating_ord ~ 1 + educ_c + peduc_c + arts_c + social_c + economic_c + 
    taste_authentic_c + taste_familiar_c + taste_light_c + taste_rich_c + 
    income_c + age_c + gend.f + race.f + 
    (1 | respondent_id) + 
    (1 + educ_c + peduc_c + arts_c + social_c + economic_c + taste_authentic_c + taste_familiar_c | cuisine)
)

# --- COSMOPOLITAN ---
# Task 7: Cosmopolitan - Strict RI
f_cosmo_strict_ri <- bf(
  rating_ord ~ 1 + educ_c + peduc_c + arts_c + social_c + economic_c + 
    cosmo_global_c + network_diversity_c + 
    income_c + age_c + gend.f + race.f + 
    (1 | respondent_id) + (1 | cuisine)
)

# Task 8: Cosmopolitan - Relaxed RI
f_cosmo_relaxed_ri <- bf(
  rating_ord ~ 1 + cs(educ_c) + cs(peduc_c) + cs(arts_c) + cs(social_c) + cs(economic_c) + 
    cs(cosmo_global_c) + cs(network_diversity_c) + 
    income_c + age_c + gend.f + race.f + 
    (1 | respondent_id) + (1 | cuisine)
)

# Task 9: Cosmopolitan - Strict RS
f_cosmo_strict_rs <- bf(
  rating_ord ~ 1 + educ_c + peduc_c + arts_c + social_c + economic_c + 
    cosmo_global_c + network_diversity_c + 
    income_c + age_c + gend.f + race.f + 
    (1 | respondent_id) + 
    (1 + educ_c + peduc_c + arts_c + social_c + economic_c + cosmo_global_c + network_diversity_c | cuisine)
)

# -------------------------------------------------------------
# Dispatch by Task ID
# -------------------------------------------------------------
if (task_id == 1) {
  run_brm(f_prac_strict_ri, dat_prac, "hier_ext_practices_strict_ri", "Practices (Strict Proportional Odds + Random Intercepts)")
} else if (task_id == 2) {
  run_brm(f_prac_relaxed_ri, dat_prac, "hier_ext_practices_relaxed_ri", "Practices (Relaxed CS + Random Intercepts)")
} else if (task_id == 3) {
  run_brm(f_prac_strict_rs, dat_prac, "hier_ext_practices_strict_rs", "Practices (Strict Proportional Odds + Full Random Slopes)")
} else if (task_id == 4) {
  run_brm(f_disp_strict_ri, dat_disp, "hier_ext_dispositions_strict_ri", "Dispositions (Strict Proportional Odds + Random Intercepts)")
} else if (task_id == 5) {
  run_brm(f_disp_relaxed_ri, dat_disp, "hier_ext_dispositions_relaxed_ri", "Dispositions (Relaxed CS + Random Intercepts)")
} else if (task_id == 6) {
  run_brm(f_disp_strict_rs, dat_disp, "hier_ext_dispositions_strict_rs", "Dispositions (Strict Proportional Odds + Full Random Slopes)")
} else if (task_id == 7) {
  run_brm(f_cosmo_strict_ri, dat_cosmo, "hier_ext_cosmopolitan_strict_ri", "Cosmopolitan (Strict Proportional Odds + Random Intercepts)")
} else if (task_id == 8) {
  run_brm(f_cosmo_relaxed_ri, dat_cosmo, "hier_ext_cosmopolitan_relaxed_ri", "Cosmopolitan (Relaxed CS + Random Intercepts)")
} else if (task_id == 9) {
  run_brm(f_cosmo_strict_rs, dat_cosmo, "hier_ext_cosmopolitan_strict_rs", "Cosmopolitan (Strict Proportional Odds + Full Random Slopes)")
} else {
  cat("No valid task ID specified (expected 1-9).\n")
}

cat("\n========================================================================\n")
cat("Taxonomy model task execution complete.\n")
cat("========================================================================\n")
