# Cuisine Authenticity & Taste: Multilevel Ordinal Modeling Pipeline

This repository contains the complete, self-contained replication package for the **Cuisine Authenticity** study. It evaluates how public perceptions of culinary authenticity (from traditional home preparation to high-end professional chef creation) are structured by social conservatism, cultural capital, and sociodemographic characteristics across 15 global cuisines.

---

## 📂 Repository Structure

```text
cuisine-authenticity/
├── README.md                      # Replication guide & documentation
├── analysis.qmd                   # Main Quarto analysis report source
├── analysis.html                  # Self-contained, rendered HTML report
├── .Rprofile                      # R environment bootstrap
├── renv.lock                      # Package dependencies lockfile
├── .gitignore                     # Git ignore rules for caches/artifacts
│
├── data/
│   ├── dat/                       # Survey datasets (Stata .dta and codebook PDFs)
│   │   ├── cultdat.dta            # Combined Qualtrics + Prolific analysis dataset
│   │   ├── prolific-original.dta  # Prolific sample raw dataset
│   │   ├── prolific.dta           # Prolific sample processed dataset
│   │   ├── prolific.pdf           # Prolific survey export
│   │   ├── qualtrics-original.dta # Qualtrics sample raw dataset
│   │   └── qualtrics.pdf          # Qualtrics survey export
│   ├── recode.dat.R               # Universal variable cleaning & recoding function
│   └── recode.qualtrics.R         # Qualtrics raw variable harmonization
│
├── docs/                          # Codebooks and survey instruments
│   ├── survey_instruments/
│   │   ├── Final_Survey.pdf       # Full survey questionnaire
│   │   └── Final_Survey_Text.txt  # Plain-text survey export
│   ├── codebook.qmd / .html       # Master variable codebook
│   ├── prolific-data-codebook.qmd / .html
│   ├── qualtrics-data-codebook.qmd / .html
│   └── variables_metadata.txt     # Variable labels and response coding definitions
│
├── scripts/                       # Model fitting & visualization scripts
│   ├── hier_1_baseline.R          # Model 1: Baseline Strict ACAT
│   ├── hier_2_relaxed.R           # Model 2: Category-Specific (CS) Thresholds
│   ├── hier_3_rs.R                # Model 3: Random Slopes Strict
│   ├── hier_4_var.R               # Model 4: Distributional (Location-Scale) Variance Strict
│   ├── hier_5_var_rs.R            # Model 5: Variance + Random Slopes (Best Model)
│   ├── hier_6_relaxed_rs.R        # Model 6: Relaxed CS + Random Slopes
│   └── generate_plots.R           # Generates all Bayesian density & forest plots
│
├── Plots/                         # Generated figures
│   ├── model_fit_comparison.png
│   ├── demographic_fixed_effects.png
│   ├── cuisine_random_effects.png
│   ├── cuisine_2d_consensus.png
│   ├── demographic_variance_effects_forest.png
│   ├── ideology_cs_effects.png
│   ├── ideology_cs_midpoint_effects.png
│   ├── cultural_cs_effects.png
│   ├── cultural_cs_midpoint_effects.png
│   ├── rs_cuisine_slopes_ideology.png
│   ├── rs_cuisine_slopes_cultural.png
│   ├── rs_variance_ideology.png
│   └── rs_variance_cultural.png
│
└── cache/                         # Storage directory for fitted brms .rds models
    └── cuisine_waic_comparison.rds
```

---

## 🎯 Theoretical & Modeling Strategy

The dependent variable is a 1-to-7 ordinal rating scale assessing where 15 cuisines are best prepared:
* **1** = *A Traditional Recipe by an Elder at Home*
* **4** = *Neutral / Midpoint*
* **7** = *A Developed Recipe by a Professional Chef at a High-End Restaurant*

### Bayesian Adjacent Category (ACAT) Models
Unlike cumulative ordinal models, ACAT models predict the probability of choosing category $k+1$ relative to category $k$, matching the sequential step-by-step nature of Likert choices. 

Six competing model specifications are fitted in `brms`:
1. **Baseline Strict**: Constant linear slopes across categories and cuisines.
2. **Relaxed CS**: Category-specific thresholds via `cs()` formulation.
3. **Random Slopes Strict**: Linear slopes varying by cuisine.
4. **Variance Strict**: Distributional modeling of location and scale/consensus (`disc`).
5. **Variance + Random Slopes** (*Optimal fit*, $\Delta$ WAIC > 1,800 over baseline): Location-scale formulation with cuisine random slopes.
6. **Relaxed CS + Random Slopes**: Combined category-specific thresholds with random slopes.

---

## 🚀 Reproduction Instructions

### 1. Restore the R Environment
Open R in the project root and restore dependencies:
```r
renv::restore()
```

### 2. Fit Bayesian Models (Optional if using cached models)
To fit any or all models using `brms` and `cmdstanr`:
```bash
Rscript scripts/hier_1_baseline.R
Rscript scripts/hier_2_relaxed.R
Rscript scripts/hier_3_rs.R
Rscript scripts/hier_4_var.R
Rscript scripts/hier_5_var_rs.R
Rscript scripts/hier_6_relaxed_rs.R
```

### 3. Generate Analysis Figures
To extract posterior draws and render all standard density plots and comparison charts into `Plots/`:
```bash
Rscript scripts/generate_plots.R
```

### 4. Render Quarto Report
Compile the standalone Quarto analysis report:
```bash
quarto render analysis.qmd
```
This produces `analysis.html` with all embedded figures and interpretations.

---

## 📊 Survey Documentation

* **Questionnaire**: `docs/survey_instruments/Final_Survey.pdf`
* **Variable Metadata**: `docs/variables_metadata.txt`
* **Interactive Codebooks**: Open `docs/codebook.html`, `docs/qualtrics-data-codebook.html`, or `docs/prolific-data-codebook.html` in any browser.
