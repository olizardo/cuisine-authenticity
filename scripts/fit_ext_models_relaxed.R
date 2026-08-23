#!/usr/bin/env Rscript
#' @title Bayesian Relaxed Category-Specific Extension Models for Cuisine Authenticity
#' @description Fits relaxed category-specific (CS) Bayesian Adjacent Category Ordinal Regression models
#'   (brms / CmdStan) freeing the proportional odds assumption for BOTH the earlier baseline variables
#'   (educ, peduc, arts, social, economic) AND the new theoretical extension variables:
#'   1. Model EXT-Practices-Relaxed: cs(educ, peduc, arts, social, economic) + cs(fancy_rest, fast_food, highbrow_arts) + Cuisine Random Slopes
#'   2. Model EXT-Dispositions-Relaxed: cs(educ, peduc, arts, social, economic) + cs(taste_authentic, taste_familiar, taste_light, taste_rich) + Cuisine Random Slopes
#'   3. Model EXT-Cosmopolitan-Relaxed: cs(educ, peduc, arts, social, economic) + cs(cosmo_global, network_diversity) + Cuisine Random Slopes
#' @author Cuisine Authenticity Project

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

# Check for task ID if running under an SGE Array Job
task_id <- as.numeric(Sys.getenv("SGE_TASK_ID", unset = "0"))

# Load prepared extended data
if (file.exists(here("data", "extended_cuisine_data.rds"))) {
  data_list <- readRDS(here("data", "extended_cuisine_data.rds"))
} else {
  source(here("scripts", "data_prep_extension.R"))
  data_list <- prepare_extended_data()
}

dat_long <- data_list$stacked

# -------------------------------------------------------------
# 1. Model EXT-Practices-Relaxed
# -------------------------------------------------------------
fit_ext_practices_relaxed <- function() {
  cat("\n========================================================================\n")
  cat(">>> Fitting Model EXT-Practices-Relaxed (Category-Specific CS + Random Slopes)...\n")
  cat("========================================================================\n")
  
  dat_sub <- dat_long |>
    filter(
      !is.na(rating), !is.na(social_c), !is.na(economic_c), !is.na(educ_c),
      !is.na(peduc_c), !is.na(arts_c), !is.na(income_c), !is.na(age_c),
      !is.na(gend.f), !is.na(race.f), !is.na(fancy_rest_c), !is.na(fast_food_c), !is.na(highbrow_arts_c)
    )
  
  f_prac_relaxed <- bf(
    rating_ord ~ 1 + 
      # Earlier relaxed variables
      cs(educ_c) + cs(peduc_c) + cs(arts_c) + cs(social_c) + cs(economic_c) + 
      # New extension variables (also relaxed)
      cs(fancy_rest_c) + cs(fast_food_c) + cs(highbrow_arts_c) + 
      # Strictly linear demographic controls
      income_c + age_c + gend.f + race.f + 
      # Crossed random effects
      (1 | respondent_id) + 
      (1 + educ_c + peduc_c + arts_c + social_c + economic_c + fancy_rest_c + fast_food_c + highbrow_arts_c | cuisine)
  )
  
  fit <- brm(
    formula = f_prac_relaxed,
    data = dat_sub,
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
    file = here("cache", "hier_ext_practices_relaxed")
  )
  
  # Ensure WAIC is computed and safely saved on cluster
  fit$file <- NULL
  if (is.null(fit$criteria$waic)) {
    cat(sprintf("\n>>> Computing WAIC on cluster (1 core, memory-safe)...\n"))
    fit <- add_criterion(fit, "waic", cores = 1, file = NULL)
    out_path <- here("cache", "hier_ext_practices_relaxed.rds")
    cat(sprintf(">>> Saving updated model with WAIC to: %s\n", out_path))
    saveRDS(fit, file = out_path)
  }
  return(fit)
}

# -------------------------------------------------------------
# 2. Model EXT-Dispositions-Relaxed
# -------------------------------------------------------------
fit_ext_dispositions_relaxed <- function() {
  cat("\n========================================================================\n")
  cat(">>> Fitting Model EXT-Dispositions-Relaxed (Category-Specific CS + Random Slopes)...\n")
  cat("========================================================================\n")
  
  dat_sub <- dat_long |>
    filter(
      !is.na(rating), !is.na(social_c), !is.na(economic_c), !is.na(educ_c),
      !is.na(peduc_c), !is.na(arts_c), !is.na(income_c), !is.na(age_c),
      !is.na(gend.f), !is.na(race.f), !is.na(taste_authentic_c), !is.na(taste_familiar_c),
      !is.na(taste_light_c), !is.na(taste_rich_c)
    )
  
  f_disp_relaxed <- bf(
    rating_ord ~ 1 + 
      # Earlier relaxed variables
      cs(educ_c) + cs(peduc_c) + cs(arts_c) + cs(social_c) + cs(economic_c) + 
      # New extension variables (also relaxed)
      cs(taste_authentic_c) + cs(taste_familiar_c) + cs(taste_light_c) + cs(taste_rich_c) + 
      # Strictly linear demographic controls
      income_c + age_c + gend.f + race.f + 
      # Crossed random effects
      (1 | respondent_id) + 
      (1 + educ_c + peduc_c + arts_c + social_c + economic_c + taste_authentic_c + taste_familiar_c | cuisine)
  )
  
  fit <- brm(
    formula = f_disp_relaxed,
    data = dat_sub,
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
    file = here("cache", "hier_ext_dispositions_relaxed")
  )
  
  # Ensure WAIC is computed and safely saved on cluster
  fit$file <- NULL
  if (is.null(fit$criteria$waic)) {
    cat(sprintf("\n>>> Computing WAIC on cluster (1 core, memory-safe)...\n"))
    fit <- add_criterion(fit, "waic", cores = 1, file = NULL)
    out_path <- here("cache", "hier_ext_dispositions_relaxed.rds")
    cat(sprintf(">>> Saving updated model with WAIC to: %s\n", out_path))
    saveRDS(fit, file = out_path)
  }
  return(fit)
}

# -------------------------------------------------------------
# 3. Model EXT-Cosmopolitan-Relaxed
# -------------------------------------------------------------
fit_ext_cosmopolitan_relaxed <- function() {
  cat("\n========================================================================\n")
  cat(">>> Fitting Model EXT-Cosmopolitan-Relaxed (Category-Specific CS + Random Slopes)...\n")
  cat("========================================================================\n")
  
  dat_sub <- dat_long |>
    filter(
      !is.na(rating), !is.na(social_c), !is.na(economic_c), !is.na(educ_c),
      !is.na(peduc_c), !is.na(arts_c), !is.na(income_c), !is.na(age_c),
      !is.na(gend.f), !is.na(race.f), !is.na(cosmo_global_c), !is.na(network_diversity_c)
    )
  
  f_cosmo_relaxed <- bf(
    rating_ord ~ 1 + 
      # Earlier relaxed variables
      cs(educ_c) + cs(peduc_c) + cs(arts_c) + cs(social_c) + cs(economic_c) + 
      # New extension variables (also relaxed)
      cs(cosmo_global_c) + cs(network_diversity_c) + 
      # Strictly linear demographic controls
      income_c + age_c + gend.f + race.f + 
      # Crossed random effects
      (1 | respondent_id) + 
      (1 + educ_c + peduc_c + arts_c + social_c + economic_c + cosmo_global_c + network_diversity_c | cuisine)
  )
  
  fit <- brm(
    formula = f_cosmo_relaxed,
    data = dat_sub,
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
    file = here("cache", "hier_ext_cosmopolitan_relaxed")
  )
  
  # Ensure WAIC is computed and safely saved on cluster
  fit$file <- NULL
  if (is.null(fit$criteria$waic)) {
    cat(sprintf("\n>>> Computing WAIC on cluster (1 core, memory-safe)...\n"))
    fit <- add_criterion(fit, "waic", cores = 1, file = NULL)
    out_path <- here("cache", "hier_ext_cosmopolitan_relaxed.rds")
    cat(sprintf(">>> Saving updated model with WAIC to: %s\n", out_path))
    saveRDS(fit, file = out_path)
  }
  return(fit)
}

# -------------------------------------------------------------
# Dispatch by Array Task ID or sequential execution
# -------------------------------------------------------------
if (task_id == 1) {
  fit_ext_practices_relaxed()
} else if (task_id == 2) {
  fit_ext_dispositions_relaxed()
} else if (task_id == 3) {
  fit_ext_cosmopolitan_relaxed()
} else {
  # Run all sequentially
  fit_ext_practices_relaxed()
  fit_ext_dispositions_relaxed()
  fit_ext_cosmopolitan_relaxed()
}

cat("\n========================================================================\n")
cat("Relaxed extension model estimation process completed successfully.\n")
cat("========================================================================\n")
