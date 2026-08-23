#' @title Data Preparation and Recoding Extension for Cuisine Authenticity
#' @description Prepares individual-level and stacked longitudinal datasets with extended
#'   variables including Bourdieu food tastes, cultural and dining practices, cosmopolitanism,
#'   social network diversity, and political ideology indicators.
#' @author Cuisine Authenticity Project

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(readr)
  library(purrr)
  library(haven)
  library(here)
})

prepare_extended_data <- function() {
  cat("Loading raw cultdat.dta...\n")
  raw_dat <- haven::read_dta(here("data", "dat", "cultdat.dta"))
  
  # Cuisines list
  cuisines_cols <- c("japanese", "french", "italian", "mexican", "moroccan", 
                     "korean", "peruvian", "native_american", "swedish", 
                     "pakistani", "ethiopian", "vietnamese", "nigerian", 
                     "jamaican", "lebanese")
  
  dat <- raw_dat |>
    mutate(respondent_id = seq_len(n())) |>
    # Race recoding
    mutate(race.f = case_when(
      race == "1" ~ "White",
      race == "2" ~ "Black",
      race == "3" ~ "White",
      race == "4" ~ "Asian",
      race == "5" ~ "Hispanic",
      race == "6" ~ "Asian",
      race == "7" ~ "Mixed Other",
      race == "8" ~ "Mixed Other",
      grepl("1,", race, fixed = TRUE) ~ "Mixed White",
      TRUE ~ "Mixed Other"
    )) |>
    # Age recoding
    mutate(age = age2) |>
    mutate(age.f = case_when(
      age >= 17 & age <= 21 ~ "Age (17-21)",
      age >= 22 & age <= 28 ~ "Age (22-28)",
      age >= 29 & age <= 35 ~ "Age (29-35)",
      age >= 36 & age <= 42 ~ "Age (36-42)",
      age >= 43 & age <= 49 ~ "Age (43-49)",
      age >= 50 & age <= 59 ~ "Age (50-59)",
      age >= 60 & age <= 69 ~ "Age (60-69)",
      age >= 70 ~ "Age (70+)"
    )) |>
    # Education recoding
    mutate(educ.f = case_when(
      educ %in% 1:3 ~ "High School or Less",
      educ %in% 4:5 ~ "Some College",
      educ == 6 ~ "College Degree",
      educ == 7 ~ "Prof./Graduate Degree"
    )) |>
    # Parental education recoding
    mutate(peduc = parented) |>
    mutate(peduc.f = case_when(
      peduc %in% 1:2 ~ "Less than High School",
      peduc == 3 ~ "High School",
      peduc %in% 4:5 ~ "Some College",
      peduc == 6 ~ "College Degree",
      peduc == 7 ~ "Prof./Graduate Degree"
    )) |>
    # Gender & Sexuality
    mutate(gend.f = factor(gender, labels = c("Woman", "Man", "Nonbinary/Other"))) |>
    mutate(sex.f = factor(sexuality, labels = c("Straight", "LGBTQ"))) |>
    # Urbanicity & Religion
    mutate(city = urban_rural, relig = attend_service, arts = child_arts) |>
    mutate(city.f = case_when(
      city < 3 ~ "Small Town",
      city >= 3 & city <= 5 ~ "Midsized Town",
      city > 5 ~ "Big City"
    )) |>
    mutate(relig.f = case_when(
      relig < 3 ~ "Not Religious",
      relig >= 3 & relig <= 5 ~ "Somewhat Religious",
      relig > 5 ~ "Very Religious"
    )) |>
    # Income recoding (13 = Prefer not to say / NA)
    mutate(income_clean = ifelse(income == 13, NA, income)) |>
    # Political Ideology
    mutate(poli = (social + economic) / 2) |>
    mutate(pol.f = case_when(
      poli < 2.5 ~ "Liberal",
      poli >= 2.5 & poli <= 5.5 ~ "Moderate",
      poli > 5.5 ~ "Conservative"
    )) |>
    mutate(spol.f = case_when(
      social < 3 ~ "Soc. Lib.",
      social >= 3 & social <= 5 ~ "Soc. Mod.",
      social > 5 ~ "Soc. Con."
    )) |>
    mutate(epol.f = case_when(
      economic < 3 ~ "Econ. Lib.",
      economic >= 3 & economic <= 5 ~ "Econ. Mod.",
      economic > 5 ~ "Econ. Con."
    )) |>
    # Dining Practices
    mutate(
      fancy_rest = fancy_restaurant,
      fast_food = fast_food,
      highbrow_arts_freq = (coalesce(museum, 1) + coalesce(art_gallery, 1) + 
                            coalesce(symphony_orchestra_opera, 1) + coalesce(play_or_musical, 1) + 
                            coalesce(dance_performance, 1)) / 5
    ) |>
    # Bourdieu Food Tastes
    mutate(
      taste_light = light_airy_fresh_delicate,
      taste_rich = rich_hearty_filling_savory,
      taste_authentic = exotic_authentic,
      taste_familiar = conventional_familiar,
      taste_caviar = caviar,
      taste_oysters = oysters,
      taste_nuggets = chicken_nuggets,
      taste_cheeseburger = cheeseburger,
      taste_sourdough = sourdough,
      taste_avocado = avocado
    ) |>
    # Cosmopolitan Identity Scale
    mutate(
      cosmo_global = global_citizen,
      cosmo_local = hood_city_town,
      cosmo_index = (global_citizen - hood_city_town)
    ) |>
    # Network Diversity Index (Count of distinct minority ties among close contacts)
    mutate(
      network_minority_ties = (
        (KnowAsianppl_strong > 1) + 
        (KnowHispanixppl_strong > 1) + 
        (KnowBlackppl_strong > 1) + 
        (KnowMENAppl_strong > 1) + 
        (KnowAmIndianppl_strong > 1)
      ),
      network_lib_ties = (KnowVeryLib_strong > 1),
      network_cons_ties = (KnowVeryCons_strong > 1)
    ) |>
    # Standardized Continuous Predictors
    mutate(
      social_c = as.numeric(scale(social)),
      economic_c = as.numeric(scale(economic)),
      poli_c = as.numeric(scale(poli)),
      educ_c = as.numeric(scale(educ)),
      peduc_c = as.numeric(scale(peduc)),
      income_c = as.numeric(scale(income_clean)),
      age_c = as.numeric(scale(age)),
      arts_c = as.numeric(scale(arts)),
      fancy_rest_c = as.numeric(scale(fancy_rest)),
      fast_food_c = as.numeric(scale(fast_food)),
      highbrow_arts_c = as.numeric(scale(highbrow_arts_freq)),
      taste_authentic_c = as.numeric(scale(taste_authentic)),
      taste_familiar_c = as.numeric(scale(taste_familiar)),
      taste_light_c = as.numeric(scale(taste_light)),
      taste_rich_c = as.numeric(scale(taste_rich)),
      cosmo_global_c = as.numeric(scale(cosmo_global)),
      network_diversity_c = as.numeric(scale(network_minority_ties))
    )
  
  # Relevel categorical factors
  dat$race.f <- relevel(factor(dat$race.f), "White")
  dat$gend.f <- relevel(factor(dat$gend.f), "Man")
  dat$pol.f <- relevel(factor(dat$pol.f, levels = c("Liberal", "Moderate", "Conservative")), "Moderate")
  
  cat("Stacking longitudinal panel across 15 cuisines...\n")
  dat_long <- dat |>
    select(
      respondent_id, all_of(cuisines_cols),
      gend.f, race.f, age, age_c, age.f,
      educ, educ_c, educ.f, peduc, peduc_c, peduc.f,
      income_clean, income_c,
      social, social_c, spol.f,
      economic, economic_c, epol.f,
      poli, poli_c, pol.f,
      arts, arts_c,
      fancy_rest, fancy_rest_c, fast_food, fast_food_c, highbrow_arts_c,
      taste_authentic_c, taste_familiar_c, taste_light_c, taste_rich_c,
      taste_caviar, taste_oysters, taste_nuggets, taste_cheeseburger,
      cosmo_global_c, cosmo_index, network_minority_ties, network_diversity_c
    ) |>
    pivot_longer(
      cols = all_of(cuisines_cols),
      names_to = "cuisine",
      values_to = "rating"
    ) |>
    mutate(
      rating_ord = factor(rating, levels = 1:7, ordered = TRUE),
      cuisine = as.factor(cuisine),
      respondent_id = as.factor(respondent_id),
      # Consecration Tier Classification (H4)
      consecration_tier = case_when(
        cuisine %in% c("french", "japanese", "italian", "swedish") ~ "Consecrated / Haute",
        cuisine %in% c("korean", "moroccan", "peruvian", "vietnamese") ~ "Intermediate / Emerging",
        TRUE ~ "Subaltern / Peripheral"
      ),
      consecration_tier = factor(consecration_tier, levels = c("Consecrated / Haute", "Intermediate / Emerging", "Subaltern / Peripheral"))
    )
  
  cat(sprintf("Successfully prepared stacked dataset: N = %d ratings across J = %d respondents.\n", nrow(dat_long), n_distinct(dat_long$respondent_id)))
  return(list(individual = dat, stacked = dat_long))
}

if (!interactive()) {
  data_list <- prepare_extended_data()
  saveRDS(data_list, here("data", "extended_cuisine_data.rds"))
  cat("Saved extended data to data/extended_cuisine_data.rds\n")
}
