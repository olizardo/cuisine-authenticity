# Cuisine Authenticity, Cultural Distinction, and Ideological Polarization

This repository analyzes how cultural capital, political ideology, Bourdieu taste dispositions, dining practices, and cosmopolitan social networks shape evaluations of cuisine authenticity across 15 national and regional cuisines.

## Statistical Framework
- **Model Class**: Bayesian Multilevel Adjacent Category Ordinal Regression (`family = acat("logit")`, estimated via `brms` and `cmdstanr`).
- **Data Structure**: Stacked panel of $N = 18,180$ ratings from $1,212$ respondents across 15 cuisines.

## Key Models
1. **Model 1 (Baseline Strict)**: Proportional odds ACAT model with crossed random intercepts.
2. **Model 2 (Relaxed CS)**: Category-specific threshold transition slopes (`cs()`).
3. **Model 3 (Random Slopes Strict)**: Cuisine-specific random slopes on location.
4. **Model 4 (Relaxed CS + Random Slopes)**: Category-specific threshold transitions with crossed random slopes.
5. **Extension Models**:
   - `EXT-Practices`: Fine dining, fast food, and highbrow arts participation.
   - `EXT-Dispositions`: Bourdieu food taste dispositions (authentic/exotic, familiar, light, rich).
   - `EXT-Cosmopolitan`: Global citizen identity and inter-ethnic friendship network diversity.

*(Note: All variance, location-scale, and cultural consensus modeling has been separated into the dedicated companion project at `/home/omarlizardo/projects/cuisine-authenticity-consensus`)*.

## Structure
- `data/`: Datasets, recoding scripts, and metadata.
- `scripts/`: Model fitting and publication-grade visualization scripts.
- `cache/`: Serialized Bayesian posterior model fits (`.rds`).
- `Plots/`: High-resolution figures exported at 300 DPI.
- `analysis.qmd`: Complete Quarto analytical report.
