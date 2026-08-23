# Global & Project Agent Guidelines

**Project:** Cuisine Authenticity and Taste (`cuisine-authenticity`)

---

## 1. Project Architecture & Statistical Framework

### Analytical Framework
* **Dependent Variable:** 7-point ordinal Likert scale evaluating whether the ideal preparation of a given cuisine is grounded in domestic tradition ($1 = \text{"traditional recipe prepared by an elder at home"}$) versus elite professional craftsmanship ($7 = \text{"developed recipe prepared by a professional chef at a high-end restaurant"}$), with $4$ as the neutral midpoint.
* **Data Structure:** Stacked longitudinal panel of $N = 18,180$ ratings across $1,212$ unique respondents evaluating 15 distinct national and regional cuisines.
* **Model Class:** Bayesian Adjacent Category Ordinal Regression (`family = acat("logit")`) with crossed random intercepts and random slopes for respondents and cuisines (`(1 | respondent_id) + (1 + ... | cuisine)`), estimated using `brms` with the `cmdstanr` backend.

*(Note: All variance, location-scale, and cultural consensus modeling has been separated into the companion project: `/home/omarlizardo/projects/cuisine-authenticity-consensus`)*.

---

## 2. Complete Factorial Taxonomy, Meta Models & Model Status

Every model in the project is organized into a deterministic factorial taxonomy crossing 4 Substantive Domains $\times$ 2 Threshold Structures $\times$ 2 Random Effect Levels ($16$ cells), plus an **Omnibus Meta Domain** putting all mechanisms into simultaneous mutual adjustment.

### Systematic Naming Formula
$$\mathbf{\text{hier\_\{\text{domain}\}\_\{\text{threshold}\}\_\{\text{re}\}.rds}}$$

* **`{domain}`:** `base` (Cultural Capital) | `practices` | `dispositions` | `cosmopolitan` | `meta`
* **`{threshold}`:** `strict` (Proportional Odds $\beta_k = \beta$) | `relaxed` (Category-Specific Transitions `cs()`)
* **`{re}`:** `ri` (Random Intercepts Only) | `rs` (Crossed Random Slopes on Cuisines)

### Complete Factorial Registry & Real-Time Status

| Domain | Specification | Threshold Constraint | Random Effects | Status & Location | Systematic File |
|:---|:---|:---|:---|:---|:---|
| **Base (Cultural Capital)** | Cell 1 (Strict RI) | Strict Proportional Odds | Random Intercepts | **Completed (Local & Cluster)** | `hier_base_strict_ri.rds` |
| | Cell 2 (Relaxed RI) | Category-Specific (`cs`) | Random Intercepts | **Completed (Local & Cluster)** | `hier_base_relaxed_ri.rds` |
| | Cell 3 (Strict RS) | Strict Proportional Odds | Cuisine Random Slopes | **Completed (Local & Cluster)** | `hier_base_strict_rs.rds` |
| | Cell 4 (Relaxed RS) | Category-Specific (`cs`) | Cuisine Random Slopes | **Completed (Local & Cluster)** | `hier_base_relaxed_rs.rds` |
| **Practices** | Cell 1 (Strict RI) | Strict Proportional Odds | Random Intercepts | **Completed (Local & Cluster)** | `hier_practices_strict_ri.rds` |
| | Cell 2 (Relaxed RI) | Category-Specific (`cs`) | Random Intercepts | **Completed (Local & Cluster)** | `hier_practices_relaxed_ri.rds` |
| | Cell 3 (Strict RS) | Strict Proportional Odds | Cuisine Random Slopes | **Completed (Local & Cluster)** | `hier_practices_strict_rs.rds` |
| | Cell 4 (Relaxed RS) | Category-Specific (`cs`) | Cuisine Random Slopes | **Completed (Local & Cluster)** | `hier_practices_relaxed_rs.rds` |
| **Dispositions** | Cell 1 (Strict RI) | Strict Proportional Odds | Random Intercepts | **Completed (Local & Cluster)** | `hier_dispositions_strict_ri.rds` |
| | Cell 2 (Relaxed RI) | Category-Specific (`cs`) | Random Intercepts | **Completed (Local & Cluster)** | `hier_dispositions_relaxed_ri.rds` |
| | Cell 3 (Strict RS) | Strict Proportional Odds | Cuisine Random Slopes | **Completed (Local & Cluster)** | `hier_dispositions_strict_rs.rds` |
| | Cell 4 (Relaxed RS) | Category-Specific (`cs`) | Cuisine Random Slopes | **Completed (Local & Cluster)** | `hier_dispositions_relaxed_rs.rds` |
| **Cosmopolitan** | Cell 1 (Strict RI) | Strict Proportional Odds | Random Intercepts | **Completed (Local & Cluster)** | `hier_cosmopolitan_strict_ri.rds` |
| | Cell 2 (Relaxed RI) | Category-Specific (`cs`) | Random Intercepts | **Completed (Local & Cluster)** | `hier_cosmopolitan_relaxed_ri.rds` |
| | Cell 3 (Strict RS) | Strict Proportional Odds | Cuisine Random Slopes | **Completed (Local & Cluster)** | `hier_cosmopolitan_strict_rs.rds` |
| | Cell 4 (Relaxed RS) | Category-Specific (`cs`) | Cuisine Random Slopes | **Completed (Local & Cluster)** | `hier_cosmopolitan_relaxed_rs.rds` |
| **Omnibus Meta** | Meta Relaxed RI | Relaxed CS (All 14 Vars) | Random Intercepts | **MCMC Finishing (Chains 1, 3, 4 Done; Chain 2 at 90% Sampling)** | `hier_meta_relaxed_ri.rds` |
| | Meta-Meta Relaxed RS | Relaxed CS (All 14 Vars) | Full Cuisine Random Slopes | **Running (Hoffman2 Array 14512323 Task 2, 35% Warmup)** | `hier_meta_relaxed_rs.rds` |

### Consolidated WAIC Fit Table (All 16 Factorial Models, Ordered by Complexity)

| Domain | Model Specification | Threshold Constraint | Random Effects | Cache File | WAIC (SE) | $p_{\text{WAIC}}$ | $\Delta \text{WAIC}_{\text{vs. Baseline}}$ | Substantive Predictive Fit |
|:---|:---|:---:|:---:|:---|:---:|:---:|:---:|:---|
| **Core Baseline (Cultural Capital)** | **Model 1: Strict RI** | **Strict Proportional Odds** | **Random Intercepts** | `hier_base_strict_ri.rds` | **55,310.59 (241.69)** | **1,141.6** | **0.00** | ***Reference Baseline*** |
| | **Model 2: Relaxed RI** | Relaxed (`cs`) | Random Intercepts | `hier_base_relaxed_ri.rds` | 55,177.51 (243.10) | 1,173.3 | **-133.08** | Substantial Improvement |
| | **Model 3: Strict RS** | Strict Proportional Odds | Crossed Random Slopes | `hier_base_strict_rs.rds` | 55,171.37 (243.27) | 1,185.2 | **-139.22** | Substantial Improvement |
| | **Model 4: Relaxed RS** | Relaxed (`cs`) | Crossed Random Slopes | `hier_base_relaxed_rs.rds` | **54,961.97 (244.84)** | 1,217.9 | **-348.62** | Decisive Improvement |
| **Dining Practices** | **Practices Strict RI** | Strict Proportional Odds | Random Intercepts | `hier_practices_strict_ri.rds` | 55,312.28 (241.61) | 1,138.4 | **+1.69** | Indistinguishable from Baseline |
| | **Practices Relaxed RI** | Relaxed (`cs`) | Random Intercepts | `hier_practices_relaxed_ri.rds` | 54,818.51 (247.60) | 1,185.9 | **-492.08** | Decisive Improvement |
| | **Practices Strict RS** | Strict Proportional Odds | Crossed Random Slopes | `hier_practices_strict_rs.rds` | 55,176.81 (242.61) | 1,160.9 | **-133.78** | Substantial Improvement |
| | **Practices Relaxed RS** | Relaxed (`cs`) | Crossed Random Slopes | `hier_practices_relaxed_rs.rds` | **54,446.72 (251.27)** | 1,254.4 | **-863.87** | Decisive Improvement |
| **Taste Dispositions** | **Dispositions Strict RI** | Strict Proportional Odds | Random Intercepts | `hier_dispositions_strict_ri.rds` | 55,311.35 (242.25) | 1,143.1 | **+0.76** | Indistinguishable from Baseline |
| | **Dispositions Relaxed RI** | Relaxed (`cs`) | Random Intercepts | `hier_dispositions_relaxed_ri.rds` | 54,586.18 (245.60) | 1,187.5 | **-724.41** | Decisive Improvement |
| | **Dispositions Strict RS** | Strict Proportional Odds | Crossed Random Slopes | `hier_dispositions_strict_rs.rds` | 55,248.60 (242.35) | 1,162.9 | **-61.99** | Moderate Improvement |
| | **Dispositions Relaxed RS** | Relaxed (`cs`) | Crossed Random Slopes | `hier_dispositions_relaxed_rs.rds` | **54,348.26 (247.35)** | 1,246.1 | **-962.33** | Decisive Improvement (Top Model) |
| **Cosmopolitan Capital** | **Cosmopolitan Strict RI** | Strict Proportional Odds | Random Intercepts | `hier_cosmopolitan_strict_ri.rds` | 55,309.75 (241.93) | 1,141.8 | **-0.84** | Indistinguishable from Baseline |
| | **Cosmopolitan Relaxed RI** | Relaxed (`cs`) | Random Intercepts | `hier_cosmopolitan_relaxed_ri.rds` | 55,069.70 (243.43) | 1,176.9 | **-240.89** | Substantial Improvement |
| | **Cosmopolitan Strict RS** | Strict Proportional Odds | Crossed Random Slopes | `hier_cosmopolitan_strict_rs.rds` | 55,296.93 (242.10) | 1,151.7 | **-13.66** | Modest Improvement |
| | **Cosmopolitan Relaxed RS** | Relaxed (`cs`) | Crossed Random Slopes | `hier_cosmopolitan_relaxed_rs.rds` | **54,804.97 (245.00)** | 1,235.7 | **-505.62** | Decisive Improvement |

### WAIC Computation Protocol
* **Cluster-Side Computation:** Compute WAIC directly on Hoffman2 via `scripts/submit_hoffman_waic.sh` and `scripts/compute_taxonomy_waic.R` using sequential 1-core execution with 16GB memory.
* **Safety Protocol:** Models are automatically written to disk immediately after MCMC completion before WAIC extraction.
* **Cluster Path Clearance:** Clear `m$file <- NULL` before calling `add_criterion()` to prevent write attempts to UCLA Hoffman2 paths.

---

## 3. Key Empirical Findings & Hypothesis Tests

### Core Theoretical Hypotheses (Childress & Lizardo)
* **H1 & H2: Ideological Sorting into Highbrow Modes**:
  - *Social Conservatism*: Credibly pro-chef ($+0.19$, $95\%\text{ CrI } [0.09, 0.31]$, $P(\beta > 0) = 100\%$).
  - *Progressive / Social Liberalism*: Credibly domestic elder authenticity.
* **H3: Social vs. Economic Ideological Asymmetry**:
  - *Economic Conservatism*: Attenuated and centered near zero ($-0.04$, $95\%\text{ CrI } [-0.15, 0.06]$, $P(\beta > 0) = 20.0\%$).
  - *Posterior Contrast Test*: $P(\beta_{\text{social}} - \beta_{\text{economic}} > 0 \mid \text{Data}) = 99.3\%$. Culinary distinction is fundamentally organized around symbolic and cultural boundaries rather than fiscal/market preferences.
* **H4: Cuisine Consecration Hierarchies & Ideological Slopes**:
  - *Consecrated / Haute Cuisines* (French $\mu = +0.70$, Japanese $\mu = +0.29$, Swedish $\mu = +0.20$): Firmly chef-anchored at baseline.
  - *Subaltern / Peripheral Cuisines* (Native American $\mu = -0.29$, Nigerian $\mu = -0.25$, Jamaican $\mu = -0.23$, Ethiopian $\mu = -0.17$, Mexican $\mu = -0.16$, Pakistani $\mu = -0.15$): Strongly elder-anchored at baseline.
  - *Ideological Tension*: Social conservatism acts as a countervailing force across subaltern cuisines (Native American $+0.34$, Pakistani $+0.25$, Mexican $+0.24$, Jamaican $+0.24$), pulling peripheral traditions toward professionalization.

### Cultural Capital & Socialization Mechanisms
* **Cultural Capital Dual Mechanism (Model EXT-Practices)**:
  - *Adult Highbrow Arts Participation*: Credibly pro-chef ($+0.14$, $P > 0 = 99.98\%$).
  - *Fine Dining Frequency*: Credibly pro-chef ($+0.09$, $P > 0 = 98.2\%$), with strongest pulls on subaltern cuisines (Native American $+0.24$, Mexican $+0.20$, Italian $+0.18$).
  - *Childhood Arts Socialization*: Once adult cultural consumption is controlled, early childhood arts exposure credibly shifts ratings toward **domestic elder authenticity** ($-0.09$, $P(\beta < 0) = 99.92\%$). Early embodied socialization roots taste in heritage and tradition, whereas adult institutionalized consumption valorizes professional restaurant gastronomy.
  - *Educational Attainment*: Credibly pro-chef ($+0.12$, $P > 0 = 100\%$).
  - *Household Income*: Centered near zero ($+0.02$, $P = 74.8\%$), demonstrating detachment of cultural schemas from sheer economic wealth.

### Construct Validation & Network Diversity
* **Bourdieu Taste Dispositions (Model EXT-Dispositions)**:
  - Liking "Exotic and Authentic" food credibly predicts domestic elder authenticity ($-0.09$, $P(\beta < 0) = 99.43\%$), directly validating that authenticity seekers locate excellence in traditional domestic cooking.
  - *Cuisine Heterogeneity*: This effect holds across all subaltern cuisines (Nigerian $-0.14$, Peruvian $-0.13$, Moroccan $-0.12$, Ethiopian $-0.12$, Pakistani $-0.11$, Jamaican $-0.11$, Lebanese $-0.10$).
* **Social Networks & Cosmopolitan Capital (Model EXT-Cosmopolitan)**:
  - Inter-ethnic close friendship network diversity credibly increases appreciation for professional chef execution ($+0.06$, $P > 0 = 97.85\%$).
  - *Global Citizen Identity*: Elevates marginalized cuisines into fine dining legitimacy (Native American $+0.09$, Mexican $+0.06$) while de-centering Western haute culinary hegemony (French $-0.02$).

---

## 4. Directional Credibility Standard (≥ 95% Posterior Probability Mass)

Bayesian credibility is evaluated based on directional posterior probability mass on either side of zero:
$$\text{Credibly Positive: } P(\theta > 0 \mid \text{Data}) \ge 0.95 \quad (\#0072B2 \text{ Okabe-Ito Blue})$$
$$\text{Credibly Negative: } P(\theta < 0 \mid \text{Data}) \ge 0.95 \quad (\#D55E00 \text{ Okabe-Ito Vermillion})$$
$$\text{Spans Zero / Uncertain: } 0.05 < P(\theta > 0 \mid \text{Data}) < 0.95 \quad (\text{gray60})$$

---

## 5. Visual Standards, Ordering Rules & Quarto Pipeline

### Plot Conventions & Guidelines
* **No Redundant Text Labels:** Do not overlay text annotations with point estimates / CI bounds on forest plots when the visual halfeyes and interval bars already convey them.
* **Tightened Axis Framing:** Adjust `coord_cartesian()` and `scale_x_continuous()` tightly to empirical distribution spans (e.g. `[-0.65, +1.05]` for cuisine random intercepts; `[-0.52, +0.35]` for demographic fixed effects).
* **Single-Gridline Alignment for Multi-Model Envelopes:** In stability and consensus forest plots, align all model specifications for a given variable along the **exact same single horizontal gridline** using `position_identity()` and semi-transparent intervals (`alpha = 0.55`) so that the visual overlap cleanly conveys the dense consensus core and specification variance.
* **Category Grouping & Ordering:**
  - *Demographic Fixed Effects (Figure 3):* Order y-axis predictors by substantive domain from bottom to top: Ethnoracial Identification $\to$ Demographics $\to$ Cultural & Socioeconomic Capital $\to$ Dining & Arts Practices $\to$ Taste Dispositions $\to$ Cosmopolitan Capital $\to$ Political Ideology. Exclude sparse categories with unstable bounds (*Nonbinary / Other Gender*).
  - *Cuisine Random Slopes (Figures 6, 7, 12, 14, 16):* Order cuisines along the y-axis strictly by the target focal predictor's median net slope (e.g. Social Conservatism for Ideology; Educational Attainment for Cultural Capital; Fine Dining for Practices; Exotic & Authentic Taste for Dispositions; Global Citizen Identity for Cosmopolitanism).
  - *Midpoint Contrasts:* Prefer midpoint contrast plots (vs. Category 4) over raw threshold transition steps.

### Complete Figure Registry & Quarto Workflow
* `scripts/extract_fixed_effects_stability.R`:
  - Figure 3: `Plots/fixed_effects_stability_forest.png` (Cross-Specification Fixed Effects Stability Envelope)
* `scripts/generate_plots.R`:
  - Figure 1: `Plots/model_fit_comparison.png` (WAIC Model Fit Comparison)
  - Figure 2: `Plots/cuisine_random_effects.png` (Baseline Cuisine Random Intercepts)
  - (Legacy Single-Model Fixed Effects): `Plots/demographic_fixed_effects.png`
  - Figure 4: `Plots/ideology_cs_midpoint_effects.png` (Ideology Midpoint Contrast Shifts)
  - Figure 5: `Plots/cultural_cs_midpoint_effects.png` (Cultural Capital Midpoint Contrast Shifts)
  - Figure 6: `Plots/rs_cuisine_slopes_ideology.png` (Cuisine Random Slopes: Ideology, ordered by Social Conservatism)
  - Figure 7: `Plots/rs_cuisine_slopes_cultural.png` (Cuisine Random Slopes: Cultural Capital, ordered by Education)
* `scripts/generate_extension_plots.R`:
  - Figure 8: `Plots/ext_h1_h3_ideology_forest.png` (H1–H3 Ideological Asymmetry & Contrast Test)
  - Figure 9: `Plots/ext_h4_cuisine_consecration_slopes.png` (H4 Consecration Tiers & Slopes)
  - Figure 10: `Plots/ext_cultural_capital_mechanisms.png` (Cultural Capital Disaggregation)
* `scripts/generate_novel_extension_plots.R`:
  - Figure 11: `Plots/ext_practices_forest.png` (Practices Global Forest)
  - Figure 12: `Plots/practices_cs_midpoint_effects.png` (Practices Midpoint Contrasts)
  - Figure 13: `Plots/rs_cuisine_slopes_practices.png` (Practices Cuisine Slopes)
  - Figure 14: `Plots/ext_dispositions_forest.png` (Dispositions Global Forest)
  - Figure 15: `Plots/dispositions_cs_midpoint_effects.png` (Dispositions Midpoint Contrasts)
  - Figure 16: `Plots/rs_cuisine_slopes_dispositions.png` (Dispositions Cuisine Slopes)
  - Figure 17: `Plots/ext_cosmopolitan_forest.png` (Cosmopolitan Global Forest)
  - Figure 18: `Plots/cosmopolitan_cs_midpoint_effects.png` (Cosmopolitan Midpoint Contrasts)
  - Figure 19: `Plots/rs_cuisine_slopes_cosmopolitan.png` (Cosmopolitan Cuisine Slopes)
* `scripts/generate_novel_midpoint_contrasts.R`:
  - Figure 20: `Plots/consensus_credible_midpoint_contrasts.png` (Master Consensus Midpoint Contrasts for All Majority-Credible Mechanisms)
* Render pipeline: `quarto render analysis.qmd`.

---

## 6. Supercomputing & HPC Standards (UCLA Hoffman2)

* **Resource Footprint:** `#$ -pe shared 4`, `#$ -l h_data=4G` ($4 \text{ cores} \times 4\text{ GB} = 16\text{ GB}$ total memory request per job).
* **Walltime Bound:** `#$ -l h_rt=23:50:00` (always bound to just under 24 hours).
* **Module Loading:** Source `/u/local/Modules/default/init/bash`, load `gcc/10.2.0` before `R`.
* **Array Staggering:** Always stagger array task startups by at least 5 to 15 minutes (`sleep $(( (SGE_TASK_ID - 1) * 300 ))`) to prevent parallel compilation clashes.
* **Threaded MCMC:** Use `threads = threading(threads_per_chain)` in `brm()`.
