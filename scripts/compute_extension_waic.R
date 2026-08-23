#!/usr/bin/env Rscript
#' @title Compute WAIC for Theoretical Extension Models in Background
#' @description Computes WAIC for Models EXT-Practices, EXT-Dispositions, and EXT-Cosmopolitan
#'   using parallel cores, updates the model cache files with add_criterion, and saves a
#'   consolidated fit table to cache/full_model_fit_comparison.rds.

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(brms)
  library(here)
})

cat("========================================================================\n")
cat("Starting Background WAIC Computation for Extension Models\n")
cat("Start Time:", as.character(Sys.time()), "\n")
cat("========================================================================\n\n")

ext_files <- c(
  "EXT-Practices"    = "hier_ext_practices.rds",
  "EXT-Dispositions" = "hier_ext_dispositions.rds",
  "EXT-Cosmopolitan" = "hier_ext_cosmopolitan.rds"
)

n_cores <- 1
cat(sprintf("Using %d core(s) for memory-safe sequential log-likelihood and WAIC extraction.\n\n", n_cores))

for (m_name in names(ext_files)) {
  f_name <- ext_files[m_name]
  path <- here("cache", f_name)
  cat(sprintf("[%s] Loading %s (%s)...\n", as.character(Sys.time()), m_name, f_name))
  m <- readRDS(path)
  m$file <- NULL  # Clear cluster-specific file metadata to prevent path errors
  
  if (is.null(m$criteria$waic)) {
    cat(sprintf("[%s] Computing WAIC for %s...\n", as.character(Sys.time()), m_name))
    m <- add_criterion(m, criterion = "waic", cores = n_cores, file = NULL)
    cat(sprintf("[%s] Saving updated model with WAIC to %s...\n", as.character(Sys.time()), path))
    saveRDS(m, file = path)
    w <- m$criteria$waic$estimates["waic", ]
    cat(sprintf("--> %s WAIC = %.2f (SE = %.2f)\n\n", m_name, w["Estimate"], w["SE"]))
  } else {
    w <- m$criteria$waic$estimates["waic", ]
    cat(sprintf("--> %s already has WAIC = %.2f (SE = %.2f)\n\n", m_name, w["Estimate"], w["SE"]))
  }
}

# -------------------------------------------------------------
# Generate Consolidated Comparison Table Across All Models
# -------------------------------------------------------------
cat("Building consolidated model fit table across all 7 models...\n")

all_models <- c(
  "Model 1: Baseline Strict"               = "hier_1_baseline.rds",
  "Model 2: Relaxed CS"                    = "hier_2_relaxed.rds",
  "Model 3: Random Slopes Strict"          = "hier_3_rs.rds",
  "Model 4: Relaxed CS + Random Slopes"    = "hier_6_relaxed_rs.rds",
  "Model 5: EXT-Practices (Dining & Arts)" = "hier_ext_practices.rds",
  "Model 6: EXT-Dispositions (Taste)"      = "hier_ext_dispositions.rds",
  "Model 7: EXT-Cosmopolitan (Networks)"   = "hier_ext_cosmopolitan.rds"
)

fit_rows <- list()
for (m_name in names(all_models)) {
  path <- here("cache", all_models[m_name])
  if (file.exists(path)) {
    m <- readRDS(path)
    if (!is.null(m$criteria$waic)) {
      w <- m$criteria$waic$estimates["waic", ]
      p_waic <- m$criteria$waic$estimates["p_waic", "Estimate"]
      fit_rows[[m_name]] <- tibble(
        Model = m_name,
        File = all_models[m_name],
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

full_fit_df <- bind_rows(fit_rows) %>%
  mutate(
    Delta_WAIC = WAIC - min(WAIC),
    Rank = rank(WAIC)
  ) %>%
  arrange(WAIC)

saveRDS(full_fit_df, here("cache", "full_model_fit_comparison.rds"))
write.csv(full_fit_df, here("cache", "full_model_fit_comparison.csv"), row.names = FALSE)

cat("\n========================================================================\n")
cat("All WAIC computations complete!\n")
cat("Consolidated Table Saved to cache/full_model_fit_comparison.rds and .csv\n")
cat("========================================================================\n")
print(full_fit_df)
