#!/usr/bin/env Rscript
#' @title Bayesian Extension Models for Cuisine Authenticity & Taste
#' @description Fits novel Bayesian Adjacent Category Ordinal Regression models (brms / CmdStan)
#'   investigating questions from "Authenticity vs. Conventional Highbrow Restaurant Preferences":
#'   1. Model EXT-Practices: Dining Practices (Fancy Restaurants, Fast Food, Highbrow Arts) & Cultural Capital.
#'   2. Model EXT-Dispositions: Bourdieu Food Taste Dispositions (Exotic/Authentic, Conventional/Familiar, Caviar, Nuggets).
#'   3. Model EXT-Cosmopolitan: Cosmopolitan Identity & Inter-Ethnic Social Network Diversity.
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

# Load prepared extended data
if (file.exists(here("data", "extended_cuisine_data.rds"))) {
  data_list <- readRDS(here("data", "extended_cuisine_data.rds"))
} else {
  source(here("scripts", "data_prep_extension.R"))
  data_list <- prepare_extended_data()
}

dat_long <- data_list$stacked

# -------------------------------------------------------------
# 1. Model EXT-Practices: Dining Practices & Cultural Capital
# -------------------------------------------------------------
fit_ext_practices <- function() {
  cat("\n>>> Fitting Model EXT-Practices (Fine Dining, Fast Food, Highbrow Arts)...\n")
  dat_sub <- dat_long |>
    filter(
      !is.na(rating), !is.na(social_c), !is.na(economic_c), !is.na(educ_c),
      !is.na(peduc_c), !is.na(arts_c), !is.na(income_c), !is.na(age_c),
      !is.na(gend.f), !is.na(race.f), !is.na(fancy_rest_c), !is.na(fast_food_c), !is.na(highbrow_arts_c)
    )
  
  f_prac <- bf(
    rating_ord ~ 1 + educ_c + peduc_c + arts_c + fancy_rest_c + fast_food_c + highbrow_arts_c + 
      social_c + economic_c + income_c + age_c + gend.f + race.f + 
      (1 | respondent_id) + (1 + fancy_rest_c + fast_food_c | cuisine)
  )
  
  fit <- brm(
    formula = f_prac,
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
    file = here("cache", "hier_ext_practices")
  )
  return(fit)
}

# -------------------------------------------------------------
# 2. Model EXT-Dispositions: Bourdieu Food Tastes & Authenticity
# -------------------------------------------------------------
fit_ext_dispositions <- function() {
  cat("\n>>> Fitting Model EXT-Dispositions (Authentic/Exotic, Familiar, Light, Rich, Caviar, Nuggets)...\n")
  dat_sub <- dat_long |>
    filter(
      !is.na(rating), !is.na(social_c), !is.na(economic_c), !is.na(educ_c),
      !is.na(peduc_c), !is.na(arts_c), !is.na(income_c), !is.na(age_c),
      !is.na(gend.f), !is.na(race.f), !is.na(taste_authentic_c), !is.na(taste_familiar_c),
      !is.na(taste_light_c), !is.na(taste_rich_c)
    )
  
  f_disp <- bf(
    rating_ord ~ 1 + taste_authentic_c + taste_familiar_c + taste_light_c + taste_rich_c + 
      educ_c + peduc_c + arts_c + social_c + economic_c + income_c + age_c + gend.f + race.f + 
      (1 | respondent_id) + (1 + taste_authentic_c + taste_familiar_c | cuisine)
  )
  
  fit <- brm(
    formula = f_disp,
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
    file = here("cache", "hier_ext_dispositions")
  )
  return(fit)
}

# -------------------------------------------------------------
# 3. Model EXT-Cosmopolitan: Cosmopolitan Identity & Diverse Networks
# -------------------------------------------------------------
fit_ext_cosmopolitan <- function() {
  cat("\n>>> Fitting Model EXT-Cosmopolitan (Global Citizen Identity & Minority Tie Diversity)...\n")
  dat_sub <- dat_long |>
    filter(
      !is.na(rating), !is.na(social_c), !is.na(economic_c), !is.na(educ_c),
      !is.na(peduc_c), !is.na(arts_c), !is.na(income_c), !is.na(age_c),
      !is.na(gend.f), !is.na(race.f), !is.na(cosmo_global_c), !is.na(network_diversity_c)
    )
  
  f_cosmo <- bf(
    rating_ord ~ 1 + cosmo_global_c + network_diversity_c + 
      educ_c + peduc_c + arts_c + social_c + economic_c + income_c + age_c + gend.f + race.f + 
      (1 | respondent_id) + (1 + cosmo_global_c | cuisine)
  )
  
  fit <- brm(
    formula = f_cosmo,
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
    file = here("cache", "hier_ext_cosmopolitan")
  )
  return(fit)
}

# Execute novel extension models
fit_ext_practices()
fit_ext_dispositions()
fit_ext_cosmopolitan()

cat("\n========================================================================\n")
cat("Novel extension model estimation completed successfully.\n")
cat("========================================================================\n")
