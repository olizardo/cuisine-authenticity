recode.qualtrics <- function() {
    library(haven)
    library(dplyr)
    library(here)
    
    dat <- read_dta(here("data", "dat", "qualtrics-original.dta")) |> 
    rename(
        economic = econ,
        swing = like_music_genres_21,
        bluegrass = like_music_genres_22,
        country = like_music_genres_23,
        randb = like_music_genres_24,
        musicals = like_music_genres_25,
        classical = like_music_genres_26,
        folk = like_music_genres_27,
        gospel = like_music_genres_28,
        jazz = like_music_genres_29,
        latin = like_music_genres_30,
        easy = like_music_genres_31,
        newage = like_music_genres_32,
        opera = like_music_genres_33,
        rap = like_music_genres_34,
        reggae = like_music_genres_35,
        pop = like_music_genres_36,
        rock = like_music_genres_37,
        oldies = like_music_genres_38,
        classrock = like_music_genres_39,
        metal = like_music_genres_40,
        tv_comedy = like_tv_genres_11,
        tv_drama = like_tv_genres_12,
        tv_reality = like_tv_genres_13,
        tv_game = like_tv_genres_14,
        tv_action = like_tv_genres_15,
        tv_scifi = like_tv_genres_16,
        tv_horror = like_tv_genres_17,
        tv_talk = like_tv_genres_18,
        tv_news = like_tv_genres_19,
        tv_sports = like_tv_genres_20,
        tv_life = like_tv_genres_21,
        tv_doc = like_tv_genres_22,
        tv_tech = like_tv_genres_23,
        mov_comedy = like_movies_genres_11,
        mov_drama = like_movies_genres_12,
        mov_doc = like_movies_genres_13,
        mov_animat = like_movies_genres_14,
        mov_action = like_movies_genres_15,
        mov_scifi = like_movies_genres_16,
        mov_horror = like_movies_genres_17,
        mov_crime = like_movies_genres_18,
        mov_musicl = like_movies_genres_19,
        mov_romanc = like_movies_genres_20,
        mov_thrill = like_movies_genres_21,
        mov_intl = like_movies_genres_22,
        mov_classc = like_movies_genres_23
    ) |>
    mutate(
        age2 = 2018 - as.numeric(age)
    ) |>
    mutate(race.f = 
        case_when(
            race == 1 ~ "White",
            race == 2 ~ "Black",
            race == 3 ~ "White", # Based on original logic
            race == 4 ~ "Asian",
            race == 5 ~ "Hispanic",
            race == 6 ~ "Asian",
            race == 7 ~ "Mixed Other",
            race == 8 ~ "Mixed Other",
            is.na(race) ~ NA_character_,
            TRUE ~ "Mixed Other"
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
            TRUE ~ NA_character_
        )
    ) |> 
    mutate(inq.f = ntile(income, 5)) |> 
    mutate(inq.f = factor(inq.f, labels = c("Bottom Income Quint.", "Second Income Quint.", 
    "Third Income Quint.", "Fourth Income Quint.", "Top Income Quint."))) |> 
    mutate(
        social_c = as.numeric(scale(social, center = TRUE, scale = TRUE)),
        economic_c = as.numeric(scale(economic, center = TRUE, scale = TRUE)),
        income_c = as.numeric(scale(income, center = TRUE, scale = TRUE)),
        educ_c = as.numeric(scale(educ, center = TRUE, scale = TRUE)),
        age_c = as.numeric(scale(age2, center = TRUE, scale = TRUE)),
        peduc = pmax(mom_educ, dad_educ, na.rm = TRUE),
        peduc_c = as.numeric(scale(peduc, center = TRUE, scale = TRUE)),
        arts_c = as.numeric(scale(child_arts, center = TRUE, scale = TRUE))
    ) |>
    mutate(gend.f = factor(sex, levels = c(1, 2, 3), labels = c("Woman", "Man", "Nonbinary/Other"))) |> 
    mutate(across(c(race.f, age.f, educ.f, inc.f), factor)) |>
    data.frame() 
    
    # Set Reference Levels
    if("White" %in% levels(dat$race.f)) dat$race.f <- relevel(dat$race.f, "White") 
    if("Man" %in% levels(dat$gend.f)) dat$gend.f <- relevel(dat$gend.f, "Man") 
    
  return(dat)
}