# Global & Project Agent Guidelines

**Global Location:** `~/.config/agents/AGENTS.md`  
**Project:** Cuisine Authenticity and Taste (`cuisine-authenticity`)

---

## 1. Cuisine Authenticity Project Architecture & Key Findings

### Analytical Framework
* **Dependent Variable:** 7-point ordinal Likert scale evaluating whether the ideal preparation of a given cuisine is grounded in domestic tradition ($1 = \text{"traditional recipe prepared by an elder at home"}$) versus elite professional craftsmanship ($7 = \text{"developed recipe prepared by a professional chef at a high-end restaurant"}$), with $4$ as the neutral midpoint.
* **Data Structure:** Stacked longitudinal panel of $N = 18,180$ ratings across $1,212$ unique respondents evaluating 15 distinct national and regional cuisines.
* **Model Class:** Bayesian Adjacent Category Ordinal Regression (`family = acat("logit")`) with crossed random intercepts and random slopes for respondents and cuisines (`(1 | respondent_id) + (1 + ... | cuisine)`).

### Six Model Configurations & WAIC Performance
1. **Model 1: Baseline Strict** (`cache/hier_1_baseline.rds`): $\text{WAIC} = 55,310.59$ ($\Delta = +1,878.0$).
2. **Model 2: Relaxed CS** (`cache/hier_2_relaxed.rds`): Category-specific transition slopes. $\text{WAIC} = 55,177.51$.
3. **Model 3: Random Slopes Strict** (`cache/hier_3_rs.rds`): Cuisine random slopes on location. $\text{WAIC} = 55,171.37$.
4. **Model 4: Variance Strict** (`cache/hier_4_var.rds`): Location-scale model with discrimination submodel (`disc`). $\text{WAIC} = 53,653.40$.
5. **Model 5: Variance + Random Slopes** (`cache/hier_5_var_rs.rds`): Full location-scale model with random slopes on both location and discrimination. **Top-performing model overall ($\text{WAIC} = 53,432.63$)**.
6. **Model 6: Relaxed CS + Random Slopes** (`cache/hier_6_relaxed_rs.rds`): Category-specific threshold transitions + cuisine random slopes. **Top non-distributional model ($\text{WAIC} = 54,961.97$)**.

### Key Empirical Takeaways
* **Global Demographic Fixed Effects (Model 6)**:
  - *Credibly Pro-Chef*: Social Conservatism ($+0.19$, $P(\beta > 0) = 100\%$) and Education ($+0.12$, $P(\beta > 0) = 100\%$).
  - *Credibly Domestic Elder*: Mixed-Race White ($-0.28$, $P(\beta < 0) = 99.8\%$), Women ($-0.18$, $P(\beta < 0) = 99.9\%$), and older Age ($-0.06$, $P(\beta < 0) = 97.0\%$).
* **Baseline Cuisine Orientations (Model 5 Random Intercepts)**:
  - *Credibly Elder*: Native American ($-1.87$), Mexican ($-1.33$), Nigerian ($-1.12$), Jamaican ($-1.10$), Ethiopian ($-0.82$), Pakistani ($-0.77$), Italian ($-0.70$), Moroccan ($-0.56$), Lebanese ($-0.56$).
  - *Credibly Pro-Chef*: French ($+3.06$) and Japanese ($+0.72$).
* **2D Consensus vs. Orientation Space**:
  - *Elder Consensus (High Agreement)*: Ethiopian, Pakistani, Lebanese, Nigerian, Peruvian, Moroccan, Vietnamese, Jamaican.
  - *Elder Contested (High Disagreement)*: Native American, Mexican, Italian, Korean.
  - *Chef Contested (High Disagreement)*: French, Japanese, Swedish.
* **Consensus/Dispersion Predictors (Discrimination Submodel)**:
  - Social Conservatism ($-0.30$, $P < 0 = 100\%$) and Education ($-0.09$, $P < 0 = 99.6\%$) credibly decrease consensus (increase dispersion/contestation).
  - Economic Conservatism ($+0.16$, $P > 0 = 100\%$) credibly increases consensus (concentrates ratings toward center).

---

## 2. Directional Credibility Standard (≥ 95% Posterior Probability Mass)

In this project, Bayesian credibility is strictly evaluated based on the **directional posterior probability mass on either side of zero**:
$$\text{Credibly Positive: } P(\theta > 0 \mid \text{Data}) = \frac{1}{S}\sum_{s=1}^S \mathbb{I}(\theta^{(s)} > 0) \ge 0.95 \quad (\#0072B2 \text{ Okabe-Ito Blue})$$
$$\text{Credibly Negative: } P(\theta < 0 \mid \text{Data}) = \frac{1}{S}\sum_{s=1}^S \mathbb{I}(\theta^{(s)} < 0) \ge 0.95 \quad (\#D55E00 \text{ Okabe-Ito Vermillion})$$
$$\text{Spans Zero / Uncertain: } 0.05 < P(\theta > 0 \mid \text{Data}) < 0.95 \quad (\text{gray60})$$

* Always annotate forest/half-eye plots with directional probability percentages and median [95% CrI] intervals.

---

## 3. Visualization & Plotting Strategy (Childress & Lizardo Visual Guidelines)

All project visualizations are maintained in `scripts/generate_plots.R` and exported to `Plots/*.png`.

### Aesthetic Conventions
* **Theme**: Custom `theme_cuisine()` built on `theme_minimal(base_size = 12)`:
  - Bold titles (`face = "bold"`), muted subtitles (`gray25`), subtle major gridlines (`gray92`), stripped minor gridlines, bold y-axis category labels (`gray15`), and bottom-placed legends.
* **Distribution Density & Intervals (`tidybayes` + `ggdist`)**:
  - `stat_halfeye(point_interval = median_qi, .width = c(0.80, 0.95), point_size = 3.5, interval_size = 1.2, slab_alpha = 0.35, scale = 0.65)`
  - Thick inner bar = 80% CrI; thin outer bar = 95% CrI; point = posterior median.
* **2D Joint Space Plotting**:
  - Expand `coord_cartesian` to accommodate full 95% CrI bounds on both axes (`xlim = c(-3.3, 4.4), ylim = c(-1.6, 1.05)`).
  - Use `ggrepel::geom_label_repel` with `box.padding = 0.4`, `point.padding = 0.3`, and `seed = 42`.
* **Rendering Specifications**:
  - Standard resolution: `dpi = 300`, `bg = "white"`.
  - Dimensions: 9.5" to 11.5" width, 5.5" to 8.5" height.

---

## 4. Package Management & Fast Build Workflow (renv / Linux)

* **Repository Configuration**: Set Posit Public Package Manager (PPM) Debian Bookworm binary URL and Stan repository:
  ```r
  options(repos = c(
    STAN = "https://mc-stan.org/r-packages/",
    CRAN = "https://packagemanager.posit.co/cran/__linux__/bookworm/latest"
  ))
  options(Ncpus = parallel::detectCores())
  Sys.setenv(MAKEFLAGS = paste0("-j", parallel::detectCores()))
  ```
* **CmdStan Backend**: Rely on `cmdstanr` rather than heavy in-memory `rstan` builds.

---

## 5. Quarto Reporting & Academic Writing Standards

* Write in **full paragraphs**; avoid numbered lists or bullet points in narrative results sections.
* Avoid hyperbole (e.g., "massive", "gigantic"); always use qualified empirical prose (e.g., "suggest", "indicates").
* Avoid `size` on line layers in `ggplot2` (`geom_line`, `geom_vline`, `geom_errorbar`); always use `linewidth`.
* Re-render Quarto reports with `quarto render analysis.qmd` after updating figures or script outputs.

---

## 6. Supercomputing & HPC Integration (UCLA Hoffman2)

* Deploy heavy NUTS Bayesian jobs via Grid Engine (`qsub`).
* Use `h_rt=23:50:00` and `h_data=4G` per core.
* Preload `gcc/10.2.0` before `R` module in SGE submit scripts.
