#' @title Generate Visualizations for Novel Extension Models
#' @description Produces publication-ready figures for the three extension models fit on Hoffman2:
#'   1. Model EXT-Practices: Dining Practices & Cultural Capital (Plots/ext_practices_forest.png)
#'   2. Model EXT-Dispositions: Bourdieu Food Taste Dispositions (Plots/ext_dispositions_forest.png)
#'   3. Model EXT-Cosmopolitan: Cosmopolitan Identity & Social Networks (Plots/ext_cosmopolitan_forest.png)
#' @author Cuisine Authenticity Project

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  library(brms)
  library(tidybayes)
  library(ggdist)
  library(here)
})

plot_dir <- here("Plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

theme_cuisine <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size * 1.15, margin = margin(b = 5)),
      plot.subtitle = element_text(size = base_size * 0.9, color = "gray25", margin = margin(b = 10)),
      plot.caption = element_text(size = base_size * 0.75, color = "gray40", hjust = 0, margin = margin(t = 10)),
      axis.title.x = element_text(face = "bold", size = base_size * 0.95, margin = margin(t = 8)),
      axis.title.y = element_text(face = "bold", size = base_size * 0.95, margin = margin(r = 8)),
      axis.text.y = element_text(face = "bold", size = base_size * 0.9, color = "gray15"),
      axis.text.x = element_text(size = base_size * 0.85),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray92", linewidth = 0.4),
      panel.grid.major.x = element_line(color = "gray92", linewidth = 0.4),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = base_size * 0.85),
      legend.text = element_text(size = base_size * 0.8),
      plot.margin = margin(t = 12, r = 16, b = 12, l = 12)
    )
}

color_map <- c(
  "Credibly Pro Chef (≥ 95% Mass)" = "#0072B2",
  "Credibly Domestic Elder (≥ 95% Mass)" = "#D55E00",
  "Spans Zero (< 95% Mass)" = "gray60"
)

# -------------------------------------------------------------
# 1. Figure: Dining Practices & Cultural Capital (Model EXT-Practices)
# -------------------------------------------------------------
m_prac <- readRDS(here("cache", "hier_ext_practices.rds"))
draws_prac <- as_draws_df(m_prac)

df_prac <- tibble(
  draw = seq_len(nrow(draws_prac)),
  `Highbrow Arts Frequency (Museum/Theater/Opera)` = draws_prac$b_highbrow_arts_c,
  `Fine Dining Frequency (Fancy Restaurants)` = draws_prac$b_fancy_rest_c,
  `Fast Food Frequency` = draws_prac$b_fast_food_c,
  `Educational Attainment` = draws_prac$b_educ_c,
  `Childhood Arts Socialization` = draws_prac$b_arts_c,
  `Social Conservatism` = draws_prac$b_social_c,
  `Economic Conservatism` = draws_prac$b_economic_c
) |>
  pivot_longer(cols = -draw, names_to = "parameter", values_to = "value") |>
  group_by(parameter) |>
  mutate(
    median_val = median(value),
    q025 = quantile(value, 0.025),
    q975 = quantile(value, 0.975),
    p_pos = mean(value > 0),
    p_neg = mean(value < 0),
    credibility = case_when(
      p_pos >= 0.95 ~ "Credibly Pro Chef (≥ 95% Mass)",
      p_neg >= 0.95 ~ "Credibly Domestic Elder (≥ 95% Mass)",
      TRUE ~ "Spans Zero (< 95% Mass)"
    ),
    label = sprintf("Median: %+.2f [%+.2f, %+.2f] | P(>0) = %.1f%%",
                    median_val, q025, q975, p_pos * 100)
  ) |>
  ungroup()

prac_order <- c(
  "Economic Conservatism",
  "Childhood Arts Socialization",
  "Fast Food Frequency",
  "Educational Attainment",
  "Fine Dining Frequency (Fancy Restaurants)",
  "Social Conservatism",
  "Highbrow Arts Frequency (Museum/Theater/Opera)"
)
df_prac$parameter <- factor(df_prac$parameter, levels = prac_order)
labels_prac <- df_prac |> distinct(parameter, label, credibility)

p_prac <- ggplot(df_prac, aes(x = value, y = parameter, fill = credibility)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
  stat_halfeye(
    point_interval = median_qi,
    .width = c(0.80, 0.95),
    point_size = 3.5,
    interval_size = 1.2,
    slab_alpha = 0.35,
    scale = 0.65
  ) +
  geom_text(
    data = labels_prac,
    aes(x = 0.28, y = parameter, label = label, color = credibility),
    hjust = 0,
    size = 3.2,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = color_map, name = "Credibility Status") +
  scale_color_manual(values = color_map, guide = "none") +
  scale_x_continuous(
    limits = c(-0.25, 0.65),
    breaks = seq(-0.2, 0.6, by = 0.2),
    labels = function(x) sprintf("%+.1f", x)
  ) +
  labs(
    title = "Dining Practices, Cultural Socialization, and Ideology (Model EXT-Practices)",
    subtitle = "Posterior distributions from Hierarchical ACAT Model with Crossed Random Slopes across 15 Cuisines",
    x = "Posterior Log-Odds Ratio (β) [Negative = Domestic Elder | Positive = Professional Chef]",
    y = "Predictor Variable",
    caption = "Directional credibility standard: ≥ 95% posterior probability mass on either side of zero.\nThick inner bar = 80% CrI; thin outer bar = 95% CrI; point = posterior median."
  ) +
  theme_cuisine(base_size = 12)

ggsave(file.path(plot_dir, "ext_practices_forest.png"), p_prac, width = 11.5, height = 6.5, dpi = 300, bg = "white")

# -------------------------------------------------------------
# 2. Figure: Bourdieu Taste Dispositions (Model EXT-Dispositions)
# -------------------------------------------------------------
m_disp <- readRDS(here("cache", "hier_ext_dispositions.rds"))
draws_disp <- as_draws_df(m_disp)

df_disp <- tibble(
  draw = seq_len(nrow(draws_disp)),
  `Taste: Exotic & Authentic Food` = draws_disp$b_taste_authentic_c,
  `Taste: Conventional & Familiar Food` = draws_disp$b_taste_familiar_c,
  `Taste: Light, Airy & Fresh Food` = draws_disp$b_taste_light_c,
  `Taste: Rich, Hearty & Savory Food` = draws_disp$b_taste_rich_c,
  `Social Conservatism` = draws_disp$b_social_c,
  `Educational Attainment` = draws_disp$b_educ_c
) |>
  pivot_longer(cols = -draw, names_to = "parameter", values_to = "value") |>
  group_by(parameter) |>
  mutate(
    median_val = median(value),
    q025 = quantile(value, 0.025),
    q975 = quantile(value, 0.975),
    p_pos = mean(value > 0),
    p_neg = mean(value < 0),
    credibility = case_when(
      p_pos >= 0.95 ~ "Credibly Pro Chef (≥ 95% Mass)",
      p_neg >= 0.95 ~ "Credibly Domestic Elder (≥ 95% Mass)",
      TRUE ~ "Spans Zero (< 95% Mass)"
    ),
    label = sprintf("Median: %+.2f [%+.2f, %+.2f] | P(>0) = %.1f%%",
                    median_val, q025, q975, p_pos * 100)
  ) |>
  ungroup()

disp_order <- c(
  "Taste: Exotic & Authentic Food",
  "Taste: Light, Airy & Fresh Food",
  "Taste: Rich, Hearty & Savory Food",
  "Taste: Conventional & Familiar Food",
  "Educational Attainment",
  "Social Conservatism"
)
df_disp$parameter <- factor(df_disp$parameter, levels = disp_order)
labels_disp <- df_disp |> distinct(parameter, label, credibility)

p_disp <- ggplot(df_disp, aes(x = value, y = parameter, fill = credibility)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
  stat_halfeye(
    point_interval = median_qi,
    .width = c(0.80, 0.95),
    point_size = 3.5,
    interval_size = 1.2,
    slab_alpha = 0.35,
    scale = 0.65
  ) +
  geom_text(
    data = labels_disp,
    aes(x = 0.28, y = parameter, label = label, color = credibility),
    hjust = 0,
    size = 3.2,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = color_map, name = "Credibility Status") +
  scale_color_manual(values = color_map, guide = "none") +
  scale_x_continuous(
    limits = c(-0.25, 0.65),
    breaks = seq(-0.2, 0.6, by = 0.2),
    labels = function(x) sprintf("%+.1f", x)
  ) +
  labs(
    title = "Bourdieu Taste Dispositions & Authenticity Orientation (Model EXT-Dispositions)",
    subtitle = "Posterior distributions from Hierarchical ACAT Model with Crossed Random Slopes across 15 Cuisines",
    x = "Posterior Log-Odds Ratio (β) [Negative = Domestic Elder | Positive = Professional Chef]",
    y = "Taste Disposition / Predictor",
    caption = "Directional credibility standard: ≥ 95% posterior probability mass on either side of zero.\nValidates whether explicit taste dispositions align with domestic vs professional evaluative schemas."
  ) +
  theme_cuisine(base_size = 12)

ggsave(file.path(plot_dir, "ext_dispositions_forest.png"), p_disp, width = 11.5, height = 6.0, dpi = 300, bg = "white")

# -------------------------------------------------------------
# 3. Figure: Cosmopolitan Capital & Social Networks (Model EXT-Cosmopolitan)
# -------------------------------------------------------------
m_cosmo <- readRDS(here("cache", "hier_ext_cosmopolitan.rds"))
draws_cosmo <- as_draws_df(m_cosmo)

df_cosmo <- tibble(
  draw = seq_len(nrow(draws_cosmo)),
  `Inter-Ethnic Friendship Network Diversity` = draws_cosmo$b_network_diversity_c,
  `Cosmopolitan Identity (Citizen of the World)` = draws_cosmo$b_cosmo_global_c,
  `Social Conservatism` = draws_cosmo$b_social_c,
  `Economic Conservatism` = draws_cosmo$b_economic_c,
  `Educational Attainment` = draws_cosmo$b_educ_c,
  `Childhood Arts Socialization` = draws_cosmo$b_arts_c
) |>
  pivot_longer(cols = -draw, names_to = "parameter", values_to = "value") |>
  group_by(parameter) |>
  mutate(
    median_val = median(value),
    q025 = quantile(value, 0.025),
    q975 = quantile(value, 0.975),
    p_pos = mean(value > 0),
    p_neg = mean(value < 0),
    credibility = case_when(
      p_pos >= 0.95 ~ "Credibly Pro Chef (≥ 95% Mass)",
      p_neg >= 0.95 ~ "Credibly Domestic Elder (≥ 95% Mass)",
      TRUE ~ "Spans Zero (< 95% Mass)"
    ),
    label = sprintf("Median: %+.2f [%+.2f, %+.2f] | P(>0) = %.1f%%",
                    median_val, q025, q975, p_pos * 100)
  ) |>
  ungroup()

cosmo_order <- c(
  "Childhood Arts Socialization",
  "Economic Conservatism",
  "Cosmopolitan Identity (Citizen of the World)",
  "Inter-Ethnic Friendship Network Diversity",
  "Educational Attainment",
  "Social Conservatism"
)
df_cosmo$parameter <- factor(df_cosmo$parameter, levels = cosmo_order)
labels_cosmo <- df_cosmo |> distinct(parameter, label, credibility)

p_cosmo <- ggplot(df_cosmo, aes(x = value, y = parameter, fill = credibility)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
  stat_halfeye(
    point_interval = median_qi,
    .width = c(0.80, 0.95),
    point_size = 3.5,
    interval_size = 1.2,
    slab_alpha = 0.35,
    scale = 0.65
  ) +
  geom_text(
    data = labels_cosmo,
    aes(x = 0.28, y = parameter, label = label, color = credibility),
    hjust = 0,
    size = 3.2,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = color_map, name = "Credibility Status") +
  scale_color_manual(values = color_map, guide = "none") +
  scale_x_continuous(
    limits = c(-0.25, 0.65),
    breaks = seq(-0.2, 0.6, by = 0.2),
    labels = function(x) sprintf("%+.1f", x)
  ) +
  labs(
    title = "Cosmopolitan Identity & Social Network Diversity (Model EXT-Cosmopolitan)",
    subtitle = "Posterior distributions from Hierarchical ACAT Model with Crossed Random Slopes across 15 Cuisines",
    x = "Posterior Log-Odds Ratio (β) [Negative = Domestic Elder | Positive = Professional Chef]",
    y = "Predictor Variable",
    caption = "Directional credibility standard: ≥ 95% posterior probability mass on either side of zero.\nTests the role of cosmopolitan world-citizen identification and inter-ethnic tie diversity."
  ) +
  theme_cuisine(base_size = 12)

ggsave(file.path(plot_dir, "ext_cosmopolitan_forest.png"), p_cosmo, width = 11.5, height = 6.0, dpi = 300, bg = "white")

# -------------------------------------------------------------
# 4. Cuisine-Specific Random Slopes for Extension Models
# -------------------------------------------------------------
color_credibility <- c(
  "Credibly Positive (≥ 95% Mass)" = "#0072B2",
  "Credibly Negative (≥ 95% Mass)" = "#D55E00",
  "Spans Zero (< 95% Mass)"        = "gray60"
)

generate_ext_cuisine_slope_plot <- function(model, vars, var_labels, title, subtitle, filename, order_by_var = vars[1], width = 11.5, height = 7.5) {
  tb_list <- list()
  for (v in vars) {
    b_var <- paste0("b_", v)
    re_draws <- model %>%
      spread_draws(r_cuisine[cuisine, term], !!sym(b_var)) %>%
      filter(term == v) %>%
      mutate(
        cuisine_label = stringr::str_to_title(stringr::str_replace_all(cuisine, "_", " ")),
        total_slope = !!sym(b_var) + r_cuisine,
        predictor_label = var_labels[v]
      )
    tb_list[[v]] <- re_draws
  }
  
  all_slopes <- bind_rows(tb_list)
  
  slope_summary <- all_slopes %>%
    group_by(predictor_label, cuisine_label) %>%
    summarize(
      median = median(total_slope),
      p_pos = mean(total_slope > 0),
      p_neg = mean(total_slope < 0),
      cred_status = case_when(
        p_pos >= 0.95 ~ "Credibly Positive (≥ 95% Mass)",
        p_neg >= 0.95 ~ "Credibly Negative (≥ 95% Mass)",
        TRUE ~ "Spans Zero (< 95% Mass)"
      ),
      .groups = "drop"
    )
  
  target_label <- var_labels[order_by_var]
  cuis_order <- slope_summary %>%
    filter(predictor_label == target_label) %>%
    arrange(median) %>%
    pull(cuisine_label)
  
  all_slopes <- all_slopes %>%
    left_join(slope_summary %>% select(predictor_label, cuisine_label, cred_status), by = c("predictor_label", "cuisine_label")) %>%
    mutate(
      cuisine_label = factor(cuisine_label, levels = cuis_order),
      cred_status = factor(cred_status, levels = c("Credibly Positive (≥ 95% Mass)", "Credibly Negative (≥ 95% Mass)", "Spans Zero (< 95% Mass)"))
    )
  
  p <- ggplot(all_slopes, aes(x = total_slope, y = cuisine_label, fill = cred_status, color = cred_status)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.7) +
    stat_halfeye(
      point_interval = median_qi,
      .width = c(0.80, 0.95),
      point_size = 3.0,
      interval_size = 1.0,
      slab_alpha = 0.35,
      scale = 0.65
    ) +
    scale_fill_manual(values = color_credibility, name = "Slope Credibility (≥ 95% Posterior Mass)") +
    scale_color_manual(values = color_credibility, name = "Slope Credibility (≥ 95% Posterior Mass)") +
    facet_wrap(~ predictor_label, scales = "free_x") +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Net Effect (Log-Odds Shift per 1 SD Increase)",
      y = NULL,
      caption = "Hierarchical ACAT model with cuisine-level crossed random slopes (4,000 MCMC draws).\nColor classified by ≥ 95% posterior probability mass on either side of zero."
    ) +
    theme_cuisine(base_size = 11) +
    theme(
      strip.text = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "gray95", color = NA),
      axis.text.y = element_text(face = "bold", size = 9.5, color = "gray15"),
      panel.spacing = unit(1.2, "lines"),
      legend.position = "bottom"
    )
  
  ggsave(file.path(plot_dir, filename), p, width = width, height = height, dpi = 300, bg = "white")
  return(p)
}

# 4A. Dining Practices Random Slopes
generate_ext_cuisine_slope_plot(
  m_prac,
  vars = c("fancy_rest_c", "fast_food_c"),
  var_labels = c("fancy_rest_c" = "Fine Dining Frequency", "fast_food_c" = "Fast Food Frequency"),
  title = "Cuisine-Specific Effects of Dining Practices (Model EXT-Practices)",
  subtitle = "Net slope estimates (Global Fixed Effect + Cuisine-Specific Random Deviation u1)\nOrdered by Fine Dining Frequency effect",
  filename = "rs_cuisine_slopes_practices.png",
  order_by_var = "fancy_rest_c",
  width = 11.5, height = 7.5
)

# 4B. Taste Dispositions Random Slopes
generate_ext_cuisine_slope_plot(
  m_disp,
  vars = c("taste_authentic_c", "taste_familiar_c"),
  var_labels = c("taste_authentic_c" = "Exotic & Authentic Taste", "taste_familiar_c" = "Familiar & Conventional Taste"),
  title = "Cuisine-Specific Effects of Taste Dispositions (Model EXT-Dispositions)",
  subtitle = "Net slope estimates (Global Fixed Effect + Cuisine-Specific Random Deviation u1)\nOrdered by Exotic & Authentic Taste effect",
  filename = "rs_cuisine_slopes_dispositions.png",
  order_by_var = "taste_authentic_c",
  width = 11.5, height = 7.5
)

# 4C. Cosmopolitanism Random Slopes
generate_ext_cuisine_slope_plot(
  m_cosmo,
  vars = c("cosmo_global_c"),
  var_labels = c("cosmo_global_c" = "Global Citizen Identity"),
  title = "Cuisine-Specific Effects of Cosmopolitan Identity (Model EXT-Cosmopolitan)",
  subtitle = "Net slope estimates (Global Fixed Effect + Cuisine-Specific Random Deviation u1)\nOrdered by Global Citizen Identity effect",
  filename = "rs_cuisine_slopes_cosmopolitan.png",
  order_by_var = "cosmo_global_c",
  width = 8.5, height = 7.5
)

cat("Novel extension plots and cuisine slope plots successfully created and saved to Plots/\n")
