#!/usr/bin/env Rscript
#' @title Full Omnibus Meta-Models for Cuisine Authenticity
#' @description Fits comprehensive omnibus meta-models combining all theoretical domains:
#'   1. Cultural Capital & Political Ideology (educ, peduc, arts, social, economic)
#'   2. Dining Practices & Highbrow Consumption (fancy_rest, fast_food, highbrow_arts)
#'   3. Taste Dispositions & Schemas (taste_authentic, taste_familiar, taste_light, taste_rich)
#'   4. Cosmopolitanism & Network Diversity (cosmo_global, network_diversity)
#'   5. Demographics (income, age, gender, race)
#'
#' Models:
#'   - Task 1: Meta Relaxed RI (Category-Specific CS on all 14 focal variables + Random Intercepts)
#'   - Task 2: Meta-Meta Relaxed RS (Category-Specific CS on all 14 focal variables + Full Crossed Cuisine Random Slopes)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(readr)
  library(brms)
  library(cmdstanr)
  library(here)
})

# Dynamic HPC Core Allocation
n_slots <- as.numeric(Sys.getenv("NSLOTS", unset = "4"))
threads_per_chain <- max(1, floor(n_slots / 4))
cat(sprintf("Allocated NSLOTS: %d | Threads per chain: %d\n", n_slots, threads_per_chain))

# Check for task ID from environment or command line
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

# Complete cases across all focal variables
focal_vars <- c(
  "rating_ord", "social_c", "economic_c", "educ_c", "peduc_c", "arts_c",
  "income_c", "age_c", "gend.f", "race.f",
  "fancy_rest_c", "fast_food_c", "highbrow_arts_c",
  "taste_authentic_c", "taste_familiar_c", "taste_light_c", "taste_rich_c",
  "cosmo_global_c", "network_diversity_c",
  "respondent_id", "cuisine"
)

dat_meta <- dat_long %>%
  filter(if_all(all_of(focal_vars), ~ !is.na(.)))

cat(sprintf("Dataset prepared: %d ratings across %d respondents and %d cuisines.\n\n",
            nrow(dat_meta), length(unique(dat_meta$respondent_id)), length(unique(dat_meta$cuisine))))

# Helper function to run brms, save safely, and compute WAIC on cluster
run_meta_model <- function(formula, data, file_name, desc) {
  cat(sprintf("\n========================================================================\n"))
  cat(sprintf(">>> Fitting %s\n", desc))
  cat(sprintf(">>> Output: cache/%s.rds\n", file_name))
  cat(sprintf("========================================================================\n"))
  
  out_path <- here("cache", paste0(file_name, ".rds"))
  
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
# 1. Model Definitions
# -------------------------------------------------------------

# Formula: Relaxed proportionality across all 14 focal variables + Random Intercepts
f_meta_relaxed_ri <- bf(
  rating_ord ~ 1 + 
    # Cultural Capital & Ideology (Relaxed CS)
    cs(educ_c) + cs(peduc_c) + cs(arts_c) + cs(social_c) + cs(economic_c) + 
    # Dining Practices & Highbrow Consumption (Relaxed CS)
    cs(fancy_rest_c) + cs(fast_food_c) + cs(highbrow_arts_c) + 
    # Taste Dispositions (Relaxed CS)
    cs(taste_authentic_c) + cs(taste_familiar_c) + cs(taste_light_c) + cs(taste_rich_c) + 
    # Cosmopolitanism & Networks (Relaxed CS)
    cs(cosmo_global_c) + cs(network_diversity_c) + 
    # Linear Demographic Controls
    income_c + age_c + gend.f + race.f + 
    # Random Intercepts Only
    (1 | respondent_id) + (1 | cuisine)
)

# Formula: Relaxed proportionality across all 14 focal variables + Full Random Slopes
f_meta_relaxed_rs <- bf(
  rating_ord ~ 1 + 
    # Cultural Capital & Ideology (Relaxed CS)
    cs(educ_c) + cs(peduc_c) + cs(arts_c) + cs(social_c) + cs(economic_c) + 
    # Dining Practices & Highbrow Consumption (Relaxed CS)
    cs(fancy_rest_c) + cs(fast_food_c) + cs(highbrow_arts_c) + 
    # Taste Dispositions (Relaxed CS)
    cs(taste_authentic_c) + cs(taste_familiar_c) + cs(taste_light_c) + cs(taste_rich_c) + 
    # Cosmopolitanism & Networks (Relaxed CS)
    cs(cosmo_global_c) + cs(network_diversity_c) + 
    # Linear Demographic Controls
    income_c + age_c + gend.f + race.f + 
    # Crossed Random Effects with Random Slopes on all 14 focal predictors
    (1 | respondent_id) + 
    (1 + educ_c + peduc_c + arts_c + social_c + economic_c + 
         fancy_rest_c + fast_food_c + highbrow_arts_c + 
         taste_authentic_c + taste_familiar_c + taste_light_c + taste_rich_c + 
         cosmo_global_c + network_diversity_c | cuisine)
)

# -------------------------------------------------------------
# 2. Dispatch by Task ID
# -------------------------------------------------------------
if (task_id == 1) {
  run_meta_model(
    f_meta_relaxed_ri, 
    dat_meta, 
    "hier_meta_relaxed_ri", 
    "Meta Model (Relaxed CS across all 14 variables + Random Intercepts)"
  )
} else if (task_id == 2) {
  run_meta_model(
    f_meta_relaxed_rs, 
    dat_meta, 
    "hier_meta_relaxed_rs", 
    "Meta-Meta Model (Relaxed CS across all 14 variables + Full Cuisine Random Slopes)"
  )
} else {
  cat("Sequential Execution: Running both Meta models...\n")
  run_meta_model(f_meta_relaxed_ri, dat_meta, "hier_meta_relaxed_ri", "Meta Model (Relaxed RI)")
  run_meta_model(f_meta_relaxed_rs, dat_meta, "hier_meta_relaxed_rs", "Meta-Meta Model (Relaxed RS)")
}

cat("\n========================================================================\n")
cat("Meta model task execution completed successfully.\n")
cat("========================================================================\n")
