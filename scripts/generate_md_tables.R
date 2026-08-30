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
fit_comp_md <- "| Model | Specification / Architecture | WAIC | ΔWAIC |
|:---:|:---|:---:|:---:|
| **M1** | **Base Strict RI:** `(1|ID) + (1|Cuisine)` | 55,310.6 | **Ref** |
| **M2** | **Base Relaxed CS:** `(1|ID) + (1|Cuisine)` | 55,177.5 | **-133.1** |
| **M3** | **Base Strict RS:** `(1|ID) + (1+Base|Cuisine)` | 55,171.4 | **-139.2** |
| **M4** | **Base Relaxed RS:** `(1|ID) + (1+Base|Cuisine)` | 54,961.0 | **-348.6** |
| **M5** | **Practices Strict RI:** `(1|ID) + (1|Cuisine)` | 55,312.3 | **+1.7** |
| **M6** | **Practices Relaxed CS:** `(1|ID) + (1|Cuisine)` | 54,818.5 | **-492.1** |
| **M7** | **Practices Strict RS:** `(1|ID) + (1+Pract|Cuisine)` | 55,176.8 | **-133.8** |
| **M8** | **Practices Relaxed RS:** `(1|ID) + (1+Pract|Cuisine)` | 54,447.3 | **-863.3** |
| **M9** | **Dispositions Strict RI:** `(1|ID) + (1|Cuisine)` | 55,311.4 | **+0.8** |
| **M10** | **Dispositions Relaxed CS:** `(1|ID) + (1|Cuisine)` | 54,586.2 | **-724.4** |
| **M11** | **Dispositions Strict RS:** `(1|ID) + (1+Disp|Cuisine)` | 55,248.6 | **-62.0** |
| **M12** | **Dispositions Relaxed RS:** `(1|ID) + (1+Disp|Cuisine)` | 54,348.3 | **-962.3** |
| **M13** | **Cosmopolitan Strict RI:** `(1|ID) + (1|Cuisine)` | 55,309.8 | **-0.8** |
| **M14** | **Cosmopolitan Relaxed CS:** `(1|ID) + (1|Cuisine)` | 55,069.7 | **-240.9** |
| **M15** | **Cosmopolitan Strict RS:** `(1|ID) + (1+Cosmo|Cuisine)` | 55,296.9 | **-13.7** |
| **M16** | **Cosmopolitan Relaxed RS:** `(1|ID) + (1+Cosmo|Cuisine)` | 54,805.0 | **-505.6** |
| **M17** | **Omnibus Meta Relaxed RI:** `(1|ID) + (1|Cuisine)` | 54,311.4 | **-999.2** |
| **M18** | **Omnibus Meta Relaxed RS:** `(1|ID) + (1+All|Cuisine)` | **53,934.6** | **-1,376.0** |
"

writeLines(fit_comp_md, here("cache", "table_fit_comparison.md"))

# Remove legacy tables 3 & 4 from cache if present
if (file.exists(here("cache", "table_cuisine_hierarchy.md"))) {
  unlink(here("cache", "table_cuisine_hierarchy.md"))
}
if (file.exists(here("cache", "table_parameter_stability.md"))) {
  unlink(here("cache", "table_parameter_stability.md"))
}

cat("All markdown tables generated successfully.\n")
