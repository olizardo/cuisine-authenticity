#!/usr/bin/env Rscript
#' Generate Markdown Tables for Quarto and Google Drive Manuscript Synchronization
#' Cuisine Authenticity Project

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

cat("Generating markdown tables in cache/...\n")
dir.create(here("cache"), showWarnings = FALSE, recursive = TRUE)

# -------------------------------------------------------------
# 1. Hypotheses Table (Table 1 in Manuscript)
# -------------------------------------------------------------
hypotheses_md <- "| Number | Hypothesis Name | Summary |
|:---|:---|:---|
| **H1** | **Cuisine Consecration Hierarchy** | Consecrated Western and haute cuisines align with professional technique, whereas subaltern and peripheral cuisines align with domestic authenticity. |
| **H2** | **Ideological Polarization** | Social and cultural conservatism predicts orientation toward professional chef mastery, while progressive orientations predict domestic elder authenticity. |
| **H3** | **Ideological Asymmetry** | Ideological sorting across culinary schemas is driven primarily by social conservatism rather than economic conservatism. |
| **H4** | **Dual-Socialization Mechanism** | Adult institutionalized and embodied capital increases orientation toward professional chef mastery, while primary childhood socialization anchors taste in domestic moral authenticity. |
| **H5** | **Cosmopolitan Capital** | Cosmopolitan capital fosters appreciation for professional gastronomic mastery over domestic moral authenticity across diverse cuisines. |
| **H6** | **Economic Capital Decoupling** | Economic capital is decoupled from culinary authenticity evaluations, indicating these schemas reflect aesthetic socialization rather than monetary resources. |
| **H7** | **Gastronomic & Arts Participation** | Participation in formal gastronomic spaces and highbrow arts reinforces the professional chef schema, whereas lowbrow dining has no effect. |
| **H8** | **Aesthetic Validation** | Prioritizing exotic and authentic traditional foods predicts endorsement of the domestic elder anchor, while general comfort/texture preferences have minimal effect. |
"

writeLines(hypotheses_md, here("cache", "table_hypotheses.md"))

# -------------------------------------------------------------
# 2. Consolidated Model Fit Table (Table 2 in Manuscript / Table 1 in analysis.qmd)
# -------------------------------------------------------------
fit_comp_md <- "| Domain | Model Specification | Threshold Constraint | Random Effects | WAIC (SE) | pWAIC | ΔWAIC | Substantive Predictive Fit |
|:---|:---|:---:|:---:|:---:|:---:|:---:|:---|
| **Core Baseline (Cultural Capital)** | **Model 1: Strict RI** | **Strict Proportional Odds** | **Random Intercepts** | **55,310.59 (241.69)** | **1,141.6** | **0.00** | ***Reference Baseline*** |
| | **Model 2: Relaxed RI** | Relaxed (cs) | Random Intercepts | 55,177.51 (243.10) | 1,173.3 | **-133.08** | Substantial Improvement |
| | **Model 3: Strict RS** | Strict Proportional Odds | Crossed Random Slopes | 55,171.37 (243.27) | 1,185.2 | **-139.22** | Substantial Improvement |
| | **Model 4: Relaxed RS** | Relaxed (cs) | Crossed Random Slopes | **54,961.97 (244.84)** | 1,217.9 | **-348.62** | Substantial Improvement |
| **Dining Practices** | **Practices Strict RI** | Strict Proportional Odds | Random Intercepts | 55,312.28 (241.61) | 1,138.4 | **+1.69** | Indistinguishable from Baseline |
| | **Practices Relaxed RI** | Relaxed (cs) | Random Intercepts | 54,818.51 (247.60) | 1,185.9 | **-492.08** | Substantial Improvement |
| | **Practices Strict RS** | Strict Proportional Odds | Crossed Random Slopes | 55,176.81 (242.61) | 1,160.9 | **-133.78** | Substantial Improvement |
| | **Practices Relaxed RS** | Relaxed (cs) | Crossed Random Slopes | **54,447.28 (250.65)** | 1,254.3 | **-863.31** | Substantial Improvement |
| **Taste Dispositions** | **Dispositions Strict RI** | Strict Proportional Odds | Random Intercepts | 55,311.35 (242.25) | 1,143.1 | **+0.76** | Indistinguishable from Baseline |
| | **Dispositions Relaxed RI** | Relaxed (cs) | Random Intercepts | 54,586.18 (245.60) | 1,187.5 | **-724.41** | Substantial Improvement |
| | **Dispositions Strict RS** | Strict Proportional Odds | Crossed Random Slopes | 55,248.60 (242.35) | 1,162.9 | **-61.99** | Moderate Improvement |
| | **Dispositions Relaxed RS** | Relaxed (cs) | Crossed Random Slopes | **54,348.26 (247.35)** | 1,246.1 | **-962.33** | Substantial Improvement |
| **Cosmopolitan Capital** | **Cosmopolitan Strict RI** | Strict Proportional Odds | Random Intercepts | 55,309.75 (241.93) | 1,141.8 | **-0.84** | Indistinguishable from Baseline |
| | **Cosmopolitan Relaxed RI** | Relaxed (cs) | Random Intercepts | 55,069.70 (243.43) | 1,176.9 | **-240.89** | Substantial Improvement |
| | **Cosmopolitan Strict RS** | Strict Proportional Odds | Crossed Random Slopes | 55,296.93 (242.10) | 1,151.7 | **-13.66** | Modest Improvement |
| | **Cosmopolitan Relaxed RS** | Relaxed (cs) | Crossed Random Slopes | **54,804.97 (245.00)** | 1,235.7 | **-505.62** | Substantial Improvement |
| **Omnibus Meta** | **Omnibus Meta Relaxed RI (Model 17)** | Relaxed (cs, All 14 Vars) | Random Intercepts | 54,311.35 (249.03) | 1,213.3 | **-999.24** | Substantial Improvement |
| | **Omnibus Meta Relaxed RS (Model 18)** | Relaxed (cs, All 14 Vars) | Crossed Random Slopes | **53,934.59 (252.15)** | **1,322.6** | **-1,375.99** | **Optimal Overall Predictive Fit** |
"

writeLines(fit_comp_md, here("cache", "table_fit_comparison.md"))

# -------------------------------------------------------------
# 3. Cuisine Random Intercepts Hierarchy (Table 2 in analysis.qmd)
# -------------------------------------------------------------
cuisine_hierarchy_md <- "| Culinary Tradition | Models (k) | Mean Effect Size | Baseline Intercept Range [min, max] | 95% Credible Envelope | Directional Consensus Classification |
|:---|:---:|:---:|:---:|:---:|:---|
| **French** | 18 | +0.691 | [+0.676, +0.718] | [+0.514, +0.859] | **Credibly Pro-Chef (P > 0 = 100%)** |
| **Japanese** | 18 | +0.290 | [+0.280, +0.312] | [+0.122, +0.450] | **Credibly Pro-Chef (P > 0 = 100%)** |
| **Swedish** | 18 | +0.199 | [+0.191, +0.212] | [+0.034, +0.355] | **Credibly Pro-Chef (P > 0 ≥ 99.0%)** |
| **Korean** | 18 | +0.079 | [+0.072, +0.090] | [-0.083, +0.237] | **Spans Zero / Neutral** |
| **Italian** | 18 | +0.074 | [+0.070, +0.081] | [-0.090, +0.232] | **Spans Zero / Neutral** |
| **Vietnamese** | 18 | -0.041 | [-0.047, -0.033] | [-0.204, +0.116] | **Spans Zero / Neutral** |
| **Peruvian** | 18 | -0.061 | [-0.069, -0.052] | [-0.222, +0.099] | **Spans Zero / Neutral** |
| **Moroccan** | 18 | -0.076 | [-0.080, -0.066] | [-0.240, +0.081] | **Spans Zero / Neutral** |
| **Lebanese** | 18 | -0.080 | [-0.092, -0.071] | [-0.239, +0.079] | **Spans Zero / Neutral** |
| **Pakistani** | 18 | -0.141 | [-0.152, -0.132] | [-0.304, +0.022] | **Credibly Domestic Elder (P < 0 ≥ 96.0%)** |
| **Mexican** | 18 | -0.148 | [-0.162, -0.137] | [-0.306, +0.013] | **Credibly Domestic Elder (P < 0 ≥ 96.4%)** |
| **Ethiopian** | 18 | -0.165 | [-0.169, -0.157] | [-0.326, -0.004] | **Credibly Domestic Elder (P < 0 ≥ 97.6%)** |
| **Jamaican** | 18 | -0.223 | [-0.232, -0.214] | [-0.383, -0.060] | **Credibly Domestic Elder (P < 0 ≥ 99.5%)** |
| **Nigerian** | 18 | -0.246 | [-0.252, -0.237] | [-0.409, -0.086] | **Credibly Domestic Elder (P < 0 ≥ 99.6%)** |
| **Native American** | 18 | -0.271 | [-0.304, -0.250] | [-0.442, -0.097] | **Credibly Domestic Elder (P < 0 ≥ 99.8%)** |
"

writeLines(cuisine_hierarchy_md, here("cache", "table_cuisine_hierarchy.md"))

# -------------------------------------------------------------
# 4. Parameter Stability Envelope (Table 3 in analysis.qmd)
# -------------------------------------------------------------
param_stability_md <- "| Predictor Variable | Substantive Domain | Models (k) | Grand Mean | Posterior Median Range [min, max] | 95% Credible Envelope | Directional Consensus |
|:---|:---|:---:|:---:|:---:|:---:|:---|
| **Ethnoracial: Mixed White** | Demographics | 18 | -0.257 | [-0.291, -0.203] | [-0.499, +0.001] | **Credibly Domestic Elder (P < 0 ≥ 97.4%)** |
| **Ethnoracial: Mixed Other** | Demographics | 18 | -0.191 | [-0.215, -0.164] | [-0.556, +0.175] | **Spans Zero / Inconclusive** |
| **Gender: Woman** | Demographics | 18 | -0.175 | [-0.200, -0.155] | [-0.314, -0.043] | **Credibly Domestic Elder (P < 0 ≥ 99.6%)** |
| **Dispositions: Exotic / Authentic** | Taste Dispositions | 6 | -0.100 | [-0.126, -0.085] | [-0.191, -0.018] | **Credibly Domestic Elder (P < 0 ≥ 99.4%)** |
| **Ethnoracial: Asian** | Demographics | 18 | -0.081 | [-0.125, +0.005] | [-0.381, +0.271] | **Consistently Spans Zero** |
| **Childhood Arts Socialization** | Cultural Capital | 18 | -0.053 | [-0.094, -0.022] | [-0.156, +0.049] | **Credibly Domestic Elder (P < 0 ≥ 99.9%)** |
| **Age** | Demographics | 18 | -0.048 | [-0.068, -0.027] | [-0.128, +0.037] | **Credibly Domestic Elder (P < 0 ≥ 98.4%)** |
| **Dispositions: Light / Fresh** | Taste Dispositions | 6 | -0.031 | [-0.045, -0.020] | [-0.105, +0.040] | **Consistently Spans Zero** |
| **Parental Education** | Cultural Capital | 18 | -0.030 | [-0.044, -0.019] | [-0.113, +0.048] | **Consistently Spans Zero** |
| **Dispositions: Rich / Hearty** | Taste Dispositions | 6 | -0.029 | [-0.046, -0.013] | [-0.107, +0.050] | **Consistently Spans Zero** |
| **Economic Conservatism** | Political Ideology | 18 | -0.022 | [-0.043, +0.003] | [-0.151, +0.094] | **Consistently Spans Zero** |
| **Ethnoracial: Hispanic / Latino** | Demographics | 18 | +0.001 | [-0.023, +0.028] | [-0.263, +0.274] | **Consistently Spans Zero** |
| **Ethnoracial: Black** | Demographics | 18 | +0.013 | [-0.032, +0.066] | [-0.209, +0.248] | **Consistently Spans Zero** |
| **Household Income** | Cultural Capital | 18 | +0.020 | [+0.002, +0.033] | [-0.060, +0.093] | **Consistently Spans Zero** |
| **Dispositions: Familiar Comfort** | Taste Dispositions | 6 | +0.028 | [-0.008, +0.049] | [-0.066, +0.112] | **Consistently Spans Zero** |
| **Global Citizen Identity** | Cosmopolitan Capital | 6 | +0.034 | [+0.031, +0.038] | [-0.031, +0.101] | **Consistently Spans Zero** |
| **Fast Food Frequency** | Dining Practices | 6 | +0.038 | [+0.027, +0.046] | [-0.031, +0.105] | **Consistently Spans Zero** |
| **Friendship Network Diversity** | Cosmopolitan Capital | 6 | +0.045 | [+0.012, +0.061] | [-0.058, +0.126] | **Credibly Pro-Chef (P > 0 ≥ 96.6%)** |
| **Educational Attainment** | Cultural Capital | 18 | +0.094 | [+0.053, +0.123] | [-0.017, +0.194] | **Credibly Pro-Chef (P > 0 ≥ 93.3%)** |
| **Fine Dining Frequency** | Dining Practices | 6 | +0.111 | [+0.087, +0.125] | [+0.005, +0.205] | **Credibly Pro-Chef (P > 0 ≥ 98.2%)** |
| **Adult Highbrow Arts Attendance** | Dining Practices | 6 | +0.117 | [+0.102, +0.142] | [+0.010, +0.215] | **Credibly Pro-Chef (P > 0 ≥ 98.6%)** |
| **Social Conservatism** | Political Ideology | 18 | +0.148 | [+0.107, +0.193] | [+0.020, +0.306] | **Credibly Pro-Chef (P > 0 ≥ 99.1%)** |
"

writeLines(param_stability_md, here("cache", "table_parameter_stability.md"))

cat("All markdown tables generated successfully.\n")
