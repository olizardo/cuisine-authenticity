#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(brms)
  library(here)
})

cat("Starting model fitting...\n")

source(here::here("data", "recode.dat.R"))
dat <- recode.dat()
dat$respondent_id <- seq_len(nrow(dat))

cuisines_cols <- c("japanese", "french", "italian", "mexican", "moroccan", 
                   "korean", "peruvian", "native_american", "swedish", 
                   "pakistani", "ethiopian", "vietnamese", "nigerian", 
                   "jamaican", "lebanese")

dat_long <- dat |>
  select(respondent_id, all_of(cuisines_cols), gend.f, race.f, social, economic, educ, peduc, income, age, arts) |>
  pivot_longer(cols = all_of(cuisines_cols), names_to = "cuisine", values_to = "rating") |>
  mutate(income = ifelse(income == 13, NA, income)) |> 
  filter(
    !is.na(rating), !is.na(social), !is.na(economic), !is.na(educ), 
    !is.na(peduc), !is.na(income), !is.na(age), !is.na(arts),
    !is.na(gend.f), !is.na(race.f)
  ) |>
  mutate(
    rating_ord = factor(rating, levels = 1:7, ordered = TRUE),
    cuisine = as.factor(cuisine),
    respondent_id = as.factor(respondent_id)
  )

formula_mod <- bf(rating_ord ~ 1 + cs(educ_c) + cs(peduc_c) + cs(social_c) + cs(economic_c) + cs(arts_c) + income_c + age_c + gend.f + race.f + (1 | respondent_id) + (1 + educ_c + peduc_c + social_c + economic_c + arts_c | cuisine))

cat("Fitting Model 4: Relaxed CS + Random Slopes...\n")
fit <- brm(
  formula = formula_mod,
  data = dat_long,
  family = acat("logit"),
  prior = c(
    prior(normal(0, 1.5), class = "Intercept"),
    prior(normal(0, 1), class = "b")
  ),
  chains = 4,
  cores = 4,
  iter = 2000,
  warmup = 1000,
  seed = 1234,
  control = list(adapt_delta = 0.90),
  backend = "cmdstanr",
  threads = threading(4),
  save_pars = save_pars(all = FALSE)
)

cat("Saving...\n")
saveRDS(fit, file = here::here("cache", "hier_6_relaxed_rs.rds"))
gc()

cat("Computing WAIC...\n")
fit <- add_criterion(fit, "waic", ndraws = 1000)
saveRDS(fit, file = here::here("cache", "hier_6_relaxed_rs.rds"))
cat("Finished!\n")

