# Global & Project Agent Guidelines

**Global Location:** `~/.config/agents/AGENTS.md`  
**Project:** Cuisine Authenticity and Taste (`cuisine-authenticity`)

---

## 1. Cuisine Authenticity Project Architecture & Core Empirical Findings

### Analytical Framework
* **Dependent Variable:** 7-point ordinal Likert scale evaluating whether the ideal preparation of a given cuisine is grounded in domestic tradition ($1 = \text{"traditional recipe prepared by an elder at home"}$) versus elite professional craftsmanship ($7 = \text{"developed recipe prepared by a professional chef at a high-end restaurant"}$), with $4$ as the neutral midpoint.
* **Data Structure:** Stacked longitudinal panel of $N = 18,180$ ratings across $1,212$ unique respondents evaluating 15 distinct national and regional cuisines.
* **Model Class:** Bayesian Adjacent Category Ordinal Regression (`family = acat("logit")`) with crossed random intercepts and random slopes for respondents and cuisines (`(1 | respondent_id) + (1 + ... | cuisine)`), estimated using `brms` with the `cmdstanr` backend.

### Primary Baseline Model Configurations & WAIC Performance
1. **Model 1: Baseline Strict** (`cache/hier_1_baseline.rds`): $\text{WAIC} = 55,310.59$ ($\Delta = +1,878.0$).
2. **Model 2: Relaxed CS** (`cache/hier_2_relaxed.rds`): Category-specific transition slopes. $\text{WAIC} = 55,177.51$.
3. **Model 3: Random Slopes Strict** (`cache/hier_3_rs.rds`): Cuisine random slopes on location. $\text{WAIC} = 55,171.37$.
4. **Model 4: Variance Strict** (`cache/hier_4_var.rds`): Location-scale model with discrimination submodel (`disc`). $\text{WAIC} = 53,653.40$.
5. **Model 5: Variance + Random Slopes** (`cache/hier_5_var_rs.rds`): Full location-scale model with random slopes on both location and discrimination. **Top-performing model overall ($\text{WAIC} = 53,432.63$)**.
6. **Model 6: Relaxed CS + Random Slopes** (`cache/hier_6_relaxed_rs.rds`): Category-specific threshold transitions + cuisine random slopes. **Top non-distributional model ($\text{WAIC} = 54,961.97$)**.

### Novel Extension Models (Hoffman2 HPC Deployments)
7. **Model EXT-Practices** (`cache/hier_ext_practices.rds`): Disaggregates dining frequencies (`fancy_restaurant`, `fast_food`) and highbrow arts participation (`highbrow_arts_freq`) with crossed random slopes across cuisines.
8. **Model EXT-Dispositions** (`cache/hier_ext_dispositions.rds`): Tests Bourdieu taste dispositions (`taste_authentic`, `taste_familiar`, `taste_light`, `taste_rich`) to validate construct congruence.
9. **Model EXT-Cosmopolitan** (`cache/hier_ext_cosmopolitan.rds`): Tests cosmopolitan world-citizen identity (`cosmo_global`) and inter-ethnic friendship network diversity (`network_diversity`).

---

## 2. Key Empirical Findings & Hypothesis Tests

### Core Theoretical Hypotheses (Childress & Lizardo)
* **H1 & H2: Ideological Sorting into Highbrow Modes**:
  - *Social Conservatism*: Credibly pro-chef ($+0.43$, $95\%\text{ CrI } [0.08, 0.84]$, $P(\beta > 0) = 99.1\%$ in Model 5; $+0.19$, $P = 100\%$ in Model 6).
  - *Progressive / Social Liberalism*: Credibly domestic elder authenticity.
* **H3: Social vs. Economic Ideological Asymmetry**:
  - *Economic Conservatism*: Attenuated and centered near zero ($+0.01$, $95\%\text{ CrI } [-0.34, 0.42]$, $P(\beta > 0) = 52.9\%$).
  - *Posterior Contrast Test*: $P(|\beta_{\text{social}}| > |\beta_{\text{economic}}| \mid \text{Data}) = 88.0\%$. Culinary distinction is fundamentally organized around symbolic and cultural boundaries rather than fiscal/market preferences.
* **H4: Cuisine Consecration Hierarchies & Ideological Slopes**:
  - *Consecrated / Haute Cuisines* (French $\mu = +3.06$, Japanese $\mu = +0.72$): Firmly chef-anchored at baseline.
  - *Subaltern / Peripheral Cuisines* (Native American $\mu = -1.87$, Mexican $\mu = -1.33$, Nigerian $\mu = -1.12$, Jamaican $\mu = -1.10$, Ethiopian $\mu = -0.82$, Pakistani $\mu = -0.77$, Italian $\mu = -0.70$): Strongly elder-anchored at baseline.
  - *Ideological Tension*: Social conservatism acts as a powerful countervailing force on subaltern cuisines ($\beta_{\text{slope}} \approx +0.47\text{ to }+0.53$), pulling peripheral traditions toward professionalization.

### Cultural Capital & Socialization Mechanisms
* **Cultural Capital Dual Mechanism (Model EXT-Practices)**:
  - *Adult Highbrow Arts Participation*: Credibly pro-chef ($+0.14$, $P > 0 = 99.98\%$).
  - *Fine Dining Frequency*: Credibly pro-chef ($+0.09$, $P > 0 = 98.2\%$).
  - *Childhood Arts Socialization*: Once adult cultural consumption is controlled, early childhood arts exposure credibly shifts ratings toward **domestic elder authenticity** ($-0.094$, $P(\beta < 0) = 99.92\%$). Early embodied socialization roots taste in heritage and tradition, whereas adult institutionalized consumption valorizes professional restaurant gastronomy.
  - *Educational Attainment*: Credibly pro-chef ($+0.43$, $P > 0 = 99.9\%$).
  - *Household Income*: Centered near zero ($-0.01$, $P = 42.1\%$), demonstrating detachment of cultural schemas from sheer economic wealth.

### Construct Validation & Network Diversity
* **Bourdieu Taste Dispositions (Model EXT-Dispositions)**:
  - Liking "Exotic and Authentic" food credibly predicts domestic elder authenticity ($-0.085$, $P(\beta < 0) = 99.43\%$), directly validating that authenticity seekers locate excellence in traditional domestic cooking.
* **Social Networks (Model EXT-Cosmopolitan)**:
  - Inter-ethnic close friendship network diversity credibly increases appreciation for professional chef execution ($+0.061$, $P > 0 = 97.85\%$).

### Consensus vs. Contestation (Discrimination Submodel)
* **2D Consensus vs. Orientation Space**:
  - *Elder Consensus (High Agreement)*: Ethiopian ($+0.54$), Pakistani ($+0.50$), Lebanese ($+0.47$), Nigerian ($+0.47$), Peruvian ($+0.38$), Moroccan ($+0.34$), Vietnamese ($+0.31$), Jamaican ($+0.29$).
  - *Elder Contested (High Disagreement)*: Native American ($-0.49$), Mexican ($-0.47$), Italian ($-1.10$), Korean ($-0.10$).
  - *Chef Contested (High Disagreement)*: French ($-0.98$), Japanese ($-0.58$), Swedish ($-0.22$).
* **Demographic Predictors of Consensus**:
  - Social Conservatism ($-0.30$, $P < 0 = 100\%$) and Education ($-0.09$, $P < 0 = 99.6\%$) credibly decrease consensus (increase evaluation contestation).
  - Economic Conservatism ($+0.16$, $P > 0 = 100\%$) credibly increases consensus (concentrates ratings toward center).

---

## 3. Directional Credibility Standard (≥ 95% Posterior Probability Mass)

In this project, Bayesian credibility is strictly evaluated based on the **directional posterior probability mass on either side of zero**:
$$\text{Credibly Positive: } P(\theta > 0 \mid \text{Data}) = \frac{1}{S}\sum_{s=1}^S \mathbb{I}(\theta^{(s)} > 0) \ge 0.95 \quad (\#0072B2 \text{ Okabe-Ito Blue})$$
$$\text{Credibly Negative: } P(\theta < 0 \mid \text{Data}) = \frac{1}{S}\sum_{s=1}^S \mathbb{I}(\theta^{(s)} < 0) \ge 0.95 \quad (\#D55E00 \text{ Okabe-Ito Vermillion})$$
$$\text{Spans Zero / Uncertain: } 0.05 < P(\theta > 0 \mid \text{Data}) < 0.95 \quad (\text{gray60})$$

* Always annotate forest/half-eye plots with directional probability percentages and median [95% CrI] intervals.

---

## 4. Visualization & Plotting Strategy (Childress & Lizardo Visual Guidelines)

All project visualizations are maintained in `scripts/generate_plots.R`, `scripts/generate_extension_plots.R`, and `scripts/generate_novel_extension_plots.R`, exported to `Plots/*.png`.

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

## 5. Quarto Reporting & Academic Writing Standards

* Write in **full paragraphs**; avoid numbered lists or bullet points in narrative results sections.
* Avoid hyperbole (e.g., "massive", "gigantic"); always use qualified empirical prose (e.g., "suggest", "indicates", "corroborates").
* Avoid `size` on line layers in `ggplot2` (`geom_line`, `geom_vline`, `geom_errorbar`); always use `linewidth`.
* Re-render Quarto reports with `quarto render analysis.qmd` after updating figures or script outputs.

---

## 6. Supercomputing & HPC Integration (UCLA Hoffman2)

* Deploy heavy NUTS Bayesian jobs via Grid Engine (`qsub`).
* Use `h_rt=23:50:00` and `h_data=4G` per core.
* Preload `gcc/10.2.0` before `R` module in SGE submit scripts.
* Prevent package conflicts by importing individual required packages (`dplyr`, `tidyr`, `tibble`, `readr`, `purrr`, `haven`, `brms`, `cmdstanr`) rather than the `tidyverse` bundle.
* Always use atomic serialization with `file = "..."` inside `brm()` to safeguard posterior draws before computing post-processing fit statistics.
