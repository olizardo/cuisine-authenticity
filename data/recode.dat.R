recode.dat <- function() {
    dat <- read_dta(here("data", "dat", "cultdat.dta")) |> 
    mutate(race.f = 
        case_when(
            race == "1" ~ "White",
            race == "2" ~ "Black",
            race == "3" ~ "White",
            race == "4" ~ "Asian",
            race == "5" ~ "Hispanic",
            race == "6" ~ "Asian",
            race == "7" ~ "Mixed Other",
            race == "8" ~ "Mixed Other",
            grepl("1,", race, fixed = TRUE) ~ "Mixed White", TRUE ~ "Mixed Other"
            )
        ) |>     
    mutate(age.f = 
        case_when(
            age2 >= 17 & age2 <= 21 ~ "Age (17-21)",
            age2 >= 22 & age2 <= 28 ~ "Age (22-28)",
            age2 >= 29 & age2 <= 35 ~ "Age (29-35)",
            age2 >= 36 & age2 <= 42 ~ "Age (36-42)",
            age2 >= 43 & age2 <= 49 ~ "Age (43-49)",
            age2 >= 50 & age2 <= 59 ~ "Age (50-59)",
            age2 >= 60 & age2 <= 69 ~ "Age (60-69)",
            age2 >= 70 ~ "Age (70+)"
        )
    ) |> 
    mutate(educ.f = 
        case_when(
            educ == 1 ~ "High School or Less",
            educ == 2 ~ "High School or Less",
            educ == 3 ~ "High School or Less",
            educ == 4 ~ "Some College",
            educ == 5 ~ "Some College",
            educ == 6 ~ "College Degree",
            educ == 7 ~ "Prof./Graduate Degree"
        )
    ) |> 
    mutate(peduc.f  = 
        case_when(
            parented == 1 ~ "Less than High School",
            parented == 2 ~ "Less than High School",
            parented == 3 ~ "High School",
            parented == 4 ~ "Some College",
            parented == 5 ~ "Some College",
            parented == 6 ~ "College Degree",
            parented == 7 ~ "Prof./Graduate Degree"
        )
    ) |> 
    mutate(inc.f =
        case_when(
            income == 1 ~ "Less than 10K",
            income == 2 ~ "Between 10K and 19.9K",
            income == 3 ~ "Between 20K and 29.9K",
            income == 4 ~ "Between 30K and 39.9K",
            income == 5 ~ "Between 40K and 49.9K",
            income == 6 ~ "Between 50K and 59.9K",
            income == 7 ~ "Between 60K and 69.9K",
            income == 8 ~ "Between 70K and 79.9K",
            income == 9 ~ "Between 80K and 89.9K",
            income == 10 ~ "Between 90K and 99.9K",
            income == 11 ~ "Between 100K and 149.9K",
            income == 12 ~ "More than 150K",
            income == 13 ~ NA
        )
    ) |> 
    mutate(city.f =
        case_when(
            urban_rural < 3 ~ "Small Town",
            urban_rural >= 3 & urban_rural <= 5 ~ "Midsized Town",
            urban_rural > 5 ~ "Big City"
        )
    ) |> 
    mutate(arts.f =
        case_when(
            child_arts < 3 ~ "Low Exposure to Arts as Child",
            child_arts >= 3 & child_arts <= 5 ~ "Moderate Exposure to Arts as Child",
            child_arts > 5 ~ "High Exposure to Arts as Child"
        )
    ) |> 
    mutate(poli = (social + economic) / 2) |> 
    mutate(pol.f = 
        case_when(
            poli < 2.5 ~ "Liberal",
            poli >= 2.5 & poli <= 5.5 ~ "Moderate",
            poli > 5.5 ~ "Conservative"
        )
    ) |> 
    mutate(spol.f = 
        case_when(
            social < 3 ~ "Soc. Lib.",
            social >= 3 & social <= 5 ~ "Soc. Mod.",
            social > 5 ~ "Soc. Con."
        )
    ) |> 
    mutate(epol.f = 
        case_when(
            economic < 3 ~ "Econ. Lib.",
            economic >= 3 & economic <= 5 ~ "Econ. Mod.",
            economic > 5 ~ "Econ. Con."
        )
    ) |> 
    mutate(relig.f =
        case_when(
            attend_service < 3 ~ "Not Religious",
            attend_service >= 3 & attend_service <= 5 ~ "Somewhat Religious",
            attend_service > 5 ~ "Very Religious"
        )
    ) |> 
    mutate(
        across(
        .cols = japanese:lebanese, 
        .fns = ~as.numeric(.x),
        .names = "{.col}"         
        )
    ) |> 
    mutate(pol.f = factor(pol.f, levels = c("Liberal", "Moderate", "Conservative"))) |> 
    mutate(across(c(race.f, age.f, educ.f, peduc.f, city.f, arts.f, spol.f, 
        epol.f, relig.f, inc.f), factor)) |>
    mutate(gend.f = factor(gender, labels = c("Woman", "Man", "Nonbinary/Other"))) |> 
    mutate(sex.f = factor(sexuality, labels = c("Straight", "LGBTQ"))) |> 
    mutate(var.photo = apply(across(landscape:snake), 1, var, na.rm = TRUE)) |> 


    mutate(across(landscape:snake, ~factor(., levels = 1:4, labels = c("Be", "In", "Me", "Ug")))) |>
    mutate(workarts = if_else(field !=14 | is.na(field), 0, 1)) |> 
    rename(artpol_y = bestartpolitical, 
           artmoral_y = bestartmoral,
           artpol_n = bestartnotpolitical,
           artmoral_n = bestartnotmoral,
           avoidpol_y = polarized,
           cancel_y = cancelledartists,
           distinction_y = uniquetastes,
           aesthetic_y = enjoyabledifficult,
           ironic_y = ironic,
           serious_y = comedy,
           light = light_airy_fresh_delicate,
           rich = rich_hearty_filling_savory,
           authentic = exotic_authentic,
           familiar = conventional_familiar,
           big = too_large_portion,
           small = too_small_portion,
           nuggets = chicken_nuggets,
           age = age2, peduc = parented, arts = child_arts, 
           city = urban_rural, relig = attend_service
           ) |>  
    mutate(inq.f = ntile(income, 5)) |> 
    mutate(inq.f = factor(inq.f, labels = c("Bottom Income Quint.", "Second Income Quint.", 
    "Third Income Quint.", "Fourth Income Quint.", "Top Income Quint."))) |> 
    mutate(
      social_c = as.numeric(scale(social, center = TRUE, scale = TRUE)),
      economic_c = as.numeric(scale(economic, center = TRUE, scale = TRUE)),
      educ_c = as.numeric(scale(educ, center = TRUE, scale = TRUE)),
      peduc_c = as.numeric(scale(peduc, center = TRUE, scale = TRUE)),
      income_c = as.numeric(scale(income, center = TRUE, scale = TRUE)),
      age_c = as.numeric(scale(age, center = TRUE, scale = TRUE)),
      arts_c = as.numeric(scale(arts, center = TRUE, scale = TRUE))
    ) |>
    data.frame() 
    dat$race.f <- relevel(dat$race.f, "White") 
    dat$pol.f <- relevel(dat$pol.f, "Moderate") 
    dat$spol.f <- relevel(dat$spol.f, "Soc. Mod.") 
    dat$epol.f <- relevel(dat$epol.f, "Econ. Mod.") 
    dat$gend.f <- relevel(dat$gend.f, "Man") 
    dat$sex.f <- relevel(dat$sex.f, "Straight") 
    dat$city.f <- relevel(dat$city.f, "Midsized Town") 
    dat$relig.f <- relevel(dat$relig.f, "Somewhat Religious") 
    dat$arts.f <- relevel(dat$arts.f, "Moderate Exposure to Arts as Child") 
  return(dat)
}
