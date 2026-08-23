#!/usr/bin/env Rscript
#' @title Compute WAIC for Full 16-Cell Model Taxonomy on Hoffman2
#' @description Iterates through all 16 models in the factorial taxonomy, computes WAIC sequentially
#'   using memory-safe 1-core execution, safely updates the .rds files, and compiles a comprehensive
#'   model comparison table across all completed specifications.

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(readr)
  library(brms)
  library(here)
})

source(here("scripts", "model_registry.R"))

cat("========================================================================\n")
cat("Starting Hoffman2 Sequential WAIC Computation for Full Model Taxonomy\n")
cat("Start Time:", as.character(Sys.time()), "\n")
cat("========================================================================\n\n")

cache_dir <- here("cache")

# Iterate over all models in the registry
for (i in seq_len(nrow(MODEL_REGISTRY))) {
  row <- MODEL_REGISTRY[i, ]
  sys_file <- row$systematic_file
  leg_file <- row$legacy_file
  
  sys_path <- file.path(cache_dir, sys_file)
  leg_path <- file.path(cache_dir, leg_file)
  
  target_path <- NULL
  if (file.exists(sys_path)) {
    target_path <- sys_path
  } else if (file.exists(leg_path)) {
    target_path <- leg_path
  }
  
  if (!is.null(target_path)) {
    cat(sprintf("[%s] Processing: %s (%s - %s - %s)...\n", 
                as.character(Sys.time()), basename(target_path), row$domain, row$threshold, row$re))
    
    m <- tryCatch(readRDS(target_path), error = function(e) {
      cat(sprintf("  Error reading %s: %s\n", target_path, e$message))
      NULL
    })
    
    if (!is.null(m) && inherits(m, "brmsfit")) {
      m$file <- NULL  # Clear cluster path metadata
      
      if (is.null(m$criteria$waic)) {
        cat(sprintf("  -> Computing WAIC for %s (1 core, memory-safe)...\n", basename(target_path)))
        t0 <- Sys.time()
        m <- tryCatch({
          add_criterion(m, criterion = "waic", cores = 1, file = NULL)
        }, error = function(e) {
          cat(sprintf("  Error computing WAIC: %s\n", e$message))
          NULL
        })
        t1 <- Sys.time()
        
        if (!is.null(m) && !is.null(m$criteria$waic)) {
          w <- m$criteria$waic$estimates["waic", ]
          cat(sprintf("  -> WAIC computed in %.1f mins: %.2f (SE = %.2f)\n", 
                      as.numeric(difftime(t1, t0, units = "mins")), w["Estimate"], w["SE"]))
          cat(sprintf("  -> Saving updated model to: %s\n", target_path))
          saveRDS(m, file = target_path)
          
          # Also sync to systematic path if target was legacy
          if (target_path == leg_path && !file.exists(sys_path)) {
            tryCatch(file.symlink(leg_file, sys_path), error = function(e) NULL)
          }
        }
      } else {
        w <- m$criteria$waic$estimates["waic", ]
        cat(sprintf("  -> Already contains WAIC = %.2f (SE = %.2f)\n", w["Estimate"], w["SE"]))
      }
    }
  } else {
    cat(sprintf("[%s] Not found yet: %s (%s - %s - %s)\n", 
                as.character(Sys.time()), sys_file, row$domain, row$threshold, row$re))
  }
}

# -------------------------------------------------------------
# Generate Consolidated Comparison Table Across All Models
# -------------------------------------------------------------
cat("\n========================================================================\n")
cat("Compiling Full Taxonomy Fit Comparison Table...\n")
cat("========================================================================\n")

fit_rows <- list()

for (i in seq_len(nrow(MODEL_REGISTRY))) {
  row <- MODEL_REGISTRY[i, ]
  sys_path <- file.path(cache_dir, row$systematic_file)
  leg_path <- file.path(cache_dir, row$legacy_file)
  
  target_path <- NULL
  if (file.exists(sys_path)) target_path <- sys_path
  else if (file.exists(leg_path)) target_path <- leg_path
  
  if (!is.null(target_path)) {
    m <- tryCatch(readRDS(target_path), error = function(e) NULL)
    if (!is.null(m) && !is.null(m$criteria$waic)) {
      w <- m$criteria$waic$estimates["waic", ]
      p_waic <- m$criteria$waic$estimates["p_waic", "Estimate"]
      
      fit_rows[[length(fit_rows) + 1]] <- tibble(
        Domain = row$domain,
        Domain_Label = row$domain_label,
        Threshold = row$threshold,
        Random_Effects = row$re,
        Cell = row$cell,
        Systematic_File = row$systematic_file,
        WAIC = w["Estimate"],
        SE = w["SE"],
        p_WAIC = p_waic,
        N_obs = nobs(m),
        N_resp = ngrps(m)$respondent_id,
        N_cuis = ngrps(m)$cuisine
      )
    }
  }
}

if (length(fit_rows) > 0) {
  full_fit_df <- bind_rows(fit_rows) %>%
    mutate(
      Delta_WAIC = WAIC - min(WAIC),
      Rank = rank(WAIC)
    ) %>%
    arrange(WAIC)
  
  out_rds <- file.path(cache_dir, "full_model_fit_comparison.rds")
  out_csv <- file.path(cache_dir, "full_model_fit_comparison.csv")
  saveRDS(full_fit_df, out_rds)
  write_csv(full_fit_df, out_csv)
  
  cat(sprintf("\nSaved fit comparison table (%d models) to:\n  - %s\n  - %s\n\n", 
              nrow(full_fit_df), out_rds, out_csv))
  print(full_fit_df)
} else {
  cat("No models with computed WAIC found.\n")
}

cat("\nHoffman2 WAIC Computation Process Complete!\n")
