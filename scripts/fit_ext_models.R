#!/usr/bin/env Rscript
#' @title Bayesian Extension Models for Cuisine Authenticity & Taste
#' @description Fits extended Bayesian Adjacent Category Ordinal Regression models (brms / CmdStan)
#'   investigating political ideology asymmetry (H1–H3), cuisine consecration hierarchies (H4),
#'   cultural capital disaggregation, and dispositional congruence.
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

dat_long <- data_list$stacked |>
  filter(
    !is.na(rating), !is.na(social_c), !is.na(economic_c), !is.na(educ_c),
    !is.na(peduc_c), !is.na(income_c), !is.na(age_c), !is.na(arts_c),
    !is.na(gend.f), !is.na(race.f)
  )

cat(sprintf("Data ready for estimation: N = %d ratings across J = %d respondents.\n", nrow(dat_long), n_distinct(dat_long$respondent_id)))

# Read model selection from args or env
args <- commandArgs(trailingOnly = TRUE)
model_type <- ifelse(length(args) > 0, args[1], Sys.getenv("MODEL_TYPE", "ext_all"))

fit_ext1_ideology <- function() {
  cat("\n>>> Fitting Model EXT-1: Political Ideology Asymmetry & Cultural Polarization (H1, H2, H3)...\n")
  f_ext1 <- bf(
    rating_ord ~ 1 + social_c + economic_c + educ_c + peduc_c + arts_c + income_c + age_c + gend.f + race.f + 
      (1 | respondent_id) + (1 | cuisine)
  )
  
  fit1 <- brm(
    formula = f_ext1,
    data = dat_long,
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
    file = here("cache", "hier_ext1_ideology")
  )
  return(fit1)
}

fit_ext2_consecration_slopes <- function() {
  cat("\n>>> Fitting Model EXT-2: Cuisine Consecration & Crossed Ideological Slopes (H4)...\n")
  f_ext2 <- bf(
    rating_ord ~ 1 + social_c + economic_c + educ_c + peduc_c + arts_c + income_c + age_c + gend.f + race.f + 
      (1 | respondent_id) + (1 + social_c + economic_c | cuisine)
  )
  
  fit2 <- brm(
    formula = f_ext2,
    data = dat_long,
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
    control = list(adapt_delta = 0.92),
    backend = "cmdstanr",
    threads = threading(threads_per_chain),
    file = here("cache", "hier_ext2_consecration")
  )
  return(fit2)
}

fit_ext3_cultural_capital <- function() {
  cat("\n>>> Fitting Model EXT-3: Cultural Capital Disaggregation (Childhood vs Education vs Dining Practices)...\n")
  f_ext3 <- bf(
    rating_ord ~ 1 + child_arts_c + educ_c + peduc_c + fancy_rest_c + fast_food_c + highbrow_arts_c + 
      social_c + economic_c + income_c + age_c + gend.f + race.f + 
      (1 | respondent_id) + (1 + fancy_rest_c + fast_food_c | cuisine)
  )
  
  # Ensure clean subset for dining practices
  dat_ext3 <- dat_long |>
    mutate(child_arts_c = arts_c) |>
    filter(!is.na(fancy_rest_c), !is.na(fast_food_c), !is.na(highbrow_arts_c))
  
  fit3 <- brm(
    formula = f_ext3,
    data = dat_ext3,
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
    file = here("cache", "hier_ext3_cultural_capital")
  )
  return(fit3)
}

# Run designated model(s)
if (model_type %in% c("ext1", "all", "ext_all")) fit_ext1_ideology()
if (model_type %in% c("ext2", "all", "ext_all")) fit_ext2_consecration_slopes()
if (model_type %in% c("ext3", "all", "ext_all")) fit_ext3_cultural_capital()

cat("\n========================================================================\n")
cat("Model estimation finished successfully.\n")
cat("========================================================================\n")
