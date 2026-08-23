#' @title Model Registry and Systematic Naming Architecture
#' @description Defines the systematic naming taxonomy for all Bayesian models in the Cuisine Authenticity project.
#'   The architecture spans a complete 4 (Domains) x 2 (Thresholds) x 2 (Random Effects) = 16-cell factorial design.
#'
#' Systematic Naming Schema:
#'   hier_<domain>_<threshold>_<random_effects>.rds
#'
#' Components:
#'   - <domain>: base | practices | dispositions | cosmopolitan
#'   - <threshold>: strict (Proportional Odds) | relaxed (Category-Specific CS)
#'   - <random_effects>: ri (Random Intercepts) | rs (Crossed Random Slopes)

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(here)
})

#' Complete 16-Cell Factorial Model Registry
MODEL_REGISTRY <- tribble(
  ~domain,          ~domain_label,      ~threshold, ~threshold_label,       ~re,   ~re_label,              ~cell, ~systematic_file,                 ~legacy_file,                 ~status,
  # Base Domain
  "base",          "Core Baseline",    "strict",   "Strict (Equal Slopes)", "ri",  "Random Intercepts",    1,     "hier_base_strict_ri.rds",        "hier_1_baseline.rds",        "completed",
  "base",          "Core Baseline",    "relaxed",  "Relaxed CS",            "ri",  "Random Intercepts",    2,     "hier_base_relaxed_ri.rds",       "hier_2_relaxed.rds",         "completed",
  "base",          "Core Baseline",    "strict",   "Strict (Equal Slopes)", "rs",  "Crossed Random Slopes", 3,     "hier_base_strict_rs.rds",        "hier_3_rs.rds",              "completed",
  "base",          "Core Baseline",    "relaxed",  "Relaxed CS",            "rs",  "Crossed Random Slopes", 4,     "hier_base_relaxed_rs.rds",       "hier_6_relaxed_rs.rds",      "completed",

  # Practices Domain
  "practices",     "Dining Practices", "strict",   "Strict (Equal Slopes)", "ri",  "Random Intercepts",    1,     "hier_practices_strict_ri.rds",   "hier_ext_practices_strict_ri.rds", "completed",
  "practices",     "Dining Practices", "relaxed",  "Relaxed CS",            "ri",  "Random Intercepts",    2,     "hier_practices_relaxed_ri.rds",  "hier_ext_practices_relaxed_ri.rds","completed",
  "practices",     "Dining Practices", "strict",   "Strict (Equal Slopes)", "rs",  "Crossed Random Slopes", 3,     "hier_practices_strict_rs.rds",   "hier_ext_practices.rds",     "completed",
  "practices",     "Dining Practices", "relaxed",  "Relaxed CS",            "rs",  "Crossed Random Slopes", 4,     "hier_practices_relaxed_rs.rds",  "hier_ext_practices_relaxed.rds",  "completed",

  # Dispositions Domain
  "dispositions",  "Food Taste Disp",  "strict",   "Strict (Equal Slopes)", "ri",  "Random Intercepts",    1,     "hier_dispositions_strict_ri.rds","hier_ext_dispositions_strict_ri.rds", "completed",
  "dispositions",  "Food Taste Disp",  "relaxed",  "Relaxed CS",            "ri",  "Random Intercepts",    2,     "hier_dispositions_relaxed_ri.rds","hier_ext_dispositions_relaxed_ri.rds","completed",
  "dispositions",  "Food Taste Disp",  "strict",   "Strict (Equal Slopes)", "rs",  "Crossed Random Slopes", 3,     "hier_dispositions_strict_rs.rds","hier_ext_dispositions.rds",  "completed",
  "dispositions",  "Food Taste Disp",  "relaxed",  "Relaxed CS",            "rs",  "Crossed Random Slopes", 4,     "hier_dispositions_relaxed_rs.rds","hier_ext_dispositions_relaxed.rds",  "completed",

  # Cosmopolitan Domain
  "cosmopolitan",  "Cosmo & Networks", "strict",   "Strict (Equal Slopes)", "ri",  "Random Intercepts",    1,     "hier_cosmopolitan_strict_ri.rds","hier_ext_cosmopolitan_strict_ri.rds", "completed",
  "cosmopolitan",  "Cosmo & Networks", "relaxed",  "Relaxed CS",            "ri",  "Random Intercepts",    2,     "hier_cosmopolitan_relaxed_ri.rds","hier_ext_cosmopolitan_relaxed_ri.rds","completed",
  "cosmopolitan",  "Cosmo & Networks", "strict",   "Strict (Equal Slopes)", "rs",  "Crossed Random Slopes", 3,     "hier_cosmopolitan_strict_rs.rds","hier_ext_cosmopolitan.rds",  "completed",
  "cosmopolitan",  "Cosmo & Networks", "relaxed",  "Relaxed CS",            "rs",  "Crossed Random Slopes", 4,     "hier_cosmopolitan_relaxed_rs.rds","hier_ext_cosmopolitan_relaxed.rds",  "completed",

  # Meta Omnibus Domain
  "meta",          "Omnibus Meta",     "relaxed",  "Relaxed CS (All Vars)", "ri",  "Random Intercepts",    2,     "hier_meta_relaxed_ri.rds",       "hier_meta_relaxed_ri.rds",   "completed",
  "meta",          "Omnibus Meta-Meta","relaxed",  "Relaxed CS (All Vars)", "rs",  "Crossed Random Slopes", 4,     "hier_meta_relaxed_rs.rds",       "hier_meta_relaxed_rs.rds",   "completed"
)

#' Resolve model file path transparently from systematic name, legacy name, or taxonomy coordinates
#' @param ... Either (domain, threshold, re) or a single string with systematic/legacy name
#' @return Absolute file path to the cached .rds file
get_model_path <- function(...) {
  args <- list(...)
  cache_dir <- here("cache")
  
  if (length(args) == 1 && is.character(args[[1]])) {
    query <- args[[1]]
    if (!grepl("\\.rds$", query)) query <- paste0(query, ".rds")
    
    # Check if direct file exists
    direct_path <- file.path(cache_dir, query)
    if (file.exists(direct_path)) return(direct_path)
    
    # Check registry matches
    matched <- MODEL_REGISTRY |>
      filter(systematic_file == query | legacy_file == query)
    
    if (nrow(matched) > 0) {
      sys_p <- file.path(cache_dir, matched$systematic_file[1])
      leg_p <- file.path(cache_dir, matched$legacy_file[1])
      if (file.exists(sys_p)) return(sys_p)
      if (file.exists(leg_p)) return(leg_p)
    }
    return(direct_path)
  } else if (length(args) >= 3) {
    d <- args[[1]]; t <- args[[2]]; r <- args[[3]]
    matched <- MODEL_REGISTRY |>
      filter(domain == d, threshold == t, re == r)
    if (nrow(matched) > 0) {
      sys_p <- file.path(cache_dir, matched$systematic_file[1])
      leg_p <- file.path(cache_dir, matched$legacy_file[1])
      if (file.exists(sys_p)) return(sys_p)
      if (file.exists(leg_p)) return(leg_p)
      return(sys_p)
    }
  }
  stop("Invalid model specification. Provide (domain, threshold, re) or a registered model name.")
}

#' Load model seamlessly from registry
#' @param ... Arguments passed to get_model_path
#' @return brmsfit object or NULL if not found
load_registered_model <- function(...) {
  path <- get_model_path(...)
  if (file.exists(path)) {
    return(readRDS(path))
  }
  warning(sprintf("Model file not found at: %s", path))
  return(NULL)
}
