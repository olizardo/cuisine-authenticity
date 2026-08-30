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
| **Omnibus Meta** | Meta Relaxed RI (Model 17) | Relaxed CS (All 14 Vars) | Random Intercepts | **Completed (Local & Cluster)** | `hier_meta_relaxed_ri.rds` |
| | Meta-Meta Relaxed RS (Model 18) | Relaxed CS (All 14 Vars) | Full Cuisine Random Slopes | **Completed (Local & Cluster)** | `hier_meta_relaxed_rs.rds` |

### Consolidated Model Fit Table (All 18 Completed Models, M1–M18)

| Model | Specification / Architecture | WAIC | ΔWAIC |
|:---:|:---|:---:|:---:|
| **M1** | **Base Strict RI:** `(1\|ID) + (1\|Cuisine)` | 55,310.6 | **Ref** |
| **M2** | **Base Relaxed CS:** `(1\|ID) + (1\|Cuisine)` | 55,177.5 | **-133.1** |
| **M3** | **Base Strict RS:** `(1\|ID) + (1+Base\|Cuisine)` | 55,171.4 | **-139.2** |
| **M4** | **Base Relaxed RS:** `(1\|ID) + (1+Base\|Cuisine)` | 54,961.0 | **-348.6** |
| **M5** | **Practices Strict RI:** `(1\|ID) + (1\|Cuisine)` | 55,312.3 | **+1.7** |
| **M6** | **Practices Relaxed CS:** `(1\|ID) + (1\|Cuisine)` | 54,818.5 | **-492.1** |
| **M7** | **Practices Strict RS:** `(1\|ID) + (1+Pract\|Cuisine)` | 55,176.8 | **-133.8** |
| **M8** | **Practices Relaxed RS:** `(1\|ID) + (1+Pract\|Cuisine)` | 54,447.3 | **-863.3** |
| **M9** | **Dispositions Strict RI:** `(1\|ID) + (1\|Cuisine)` | 55,311.4 | **+0.8** |
| **M10** | **Dispositions Relaxed CS:** `(1\|ID) + (1\|Cuisine)` | 54,586.2 | **-724.4** |
| **M11** | **Dispositions Strict RS:** `(1\|ID) + (1+Disp\|Cuisine)` | 55,248.6 | **-62.0** |
| **M12** | **Dispositions Relaxed RS:** `(1\|ID) + (1+Disp\|Cuisine)` | 54,348.3 | **-962.3** |
| **M13** | **Cosmopolitan Strict RI:** `(1\|ID) + (1\|Cuisine)` | 55,309.8 | **-0.8** |
| **M14** | **Cosmopolitan Relaxed CS:** `(1\|ID) + (1\|Cuisine)` | 55,069.7 | **-240.9** |
| **M15** | **Cosmopolitan Strict RS:** `(1\|ID) + (1+Cosmo\|Cuisine)` | 55,296.9 | **-13.7** |
| **M16** | **Cosmopolitan Relaxed RS:** `(1\|ID) + (1+Cosmo\|Cuisine)` | 54,805.0 | **-505.6** |
| **M17** | **Omnibus Meta Relaxed RI:** `(1\|ID) + (1\|Cuisine)` | 54,311.4 | **-999.2** |
| **M18** | **Omnibus Meta Relaxed RS:** `(1\|ID) + (1+All\|Cuisine)` | **53,934.6** | **-1,376.0** |

### WAIC Computation Protocol
* **Cluster-Side Computation:** Compute WAIC directly on Hoffman2 via `scripts/submit_hoffman_waic.sh` and `scripts/compute_taxonomy_waic.R` using sequential 1-core execution with 16GB memory.
* **Safety Protocol:** Models are automatically written to disk immediately after MCMC completion before WAIC extraction.
* **Cluster Path Clearance:** Clear `m$file <- NULL` before calling `add_criterion()` to prevent write attempts to UCLA Hoffman2 paths.

---

## 3. Key Empirical Findings & Theoretical Hypotheses

### Complete Hypotheses Registry
* **H1: Cuisine Consecration Hierarchy**:
  - *Consecrated Cuisines* (French $\mu = +0.691$, Japanese $\mu = +0.290$, Swedish $\mu = +0.199$): Firmly chef-anchored at baseline ($P > 0 \ge 99.0\%$).
  - *Intermediate Cuisines* (Korean $+0.079$, Italian $+0.074$, Vietnamese $-0.041$, Peruvian $-0.061$, Moroccan $-0.076$, Lebanese $-0.080$): Neutrally centered spanning zero.
  - *Subaltern Cuisines* (Native American $-0.271$, Nigerian $-0.246$, Jamaican $-0.223$, Ethiopian $-0.165$, Mexican $-0.148$, Pakistani $-0.141$): Strongly elder-anchored at baseline ($P < 0 \ge 96.0\%$).
* **H2 & H3: Ideological Polarization & Asymmetry**:
  - *Social Conservatism*: Credibly pro-chef across all 18 models ($\text{grand mean } \bar{\beta} = +0.148$, median range $[+0.107, +0.193]$, $P(\beta > 0) \ge 99.1\%$).
  - *Economic Conservatism*: Attenuated and centered near zero ($\text{grand mean } \bar{\beta} = -0.022$, median range $[-0.043, +0.003]$, $95\%\text{ CrI } [-0.151, +0.094]$).
  - *Posterior Contrast Test*: $P(\beta_{\text{social}} - \beta_{\text{economic}} > 0 \mid \text{Data}) = 99.3\%$. Culinary distinction is fundamentally organized around symbolic and cultural boundaries rather than fiscal/market preferences.
* **H4 & H6: Cultural Capital Dual Mechanism & Economic Wealth Decoupling**:
  - *Formal Educational Attainment*: Credibly pro-chef ($\bar{\beta} = +0.094$, $P > 0 = 93.3\%$).
  - *Childhood Arts Socialization*: Once adult cultural consumption is controlled, early childhood arts exposure credibly shifts ratings toward **domestic elder authenticity** ($\bar{\beta} = -0.053$, $P(\beta < 0) \ge 99.9\%$). Early embodied socialization roots taste in heritage and tradition, whereas adult institutionalized consumption valorizes professional restaurant gastronomy.
  - *Household Income*: Centered near zero ($\bar{\beta} = +0.020$, spans zero), demonstrating detachment of cultural schemas from sheer economic wealth.
* **H5: Cosmopolitan Capital & Social Networks**:
  - Inter-ethnic close friendship network diversity credibly increases appreciation for professional chef execution ($\bar{\beta} = +0.045$, $P > 0 \ge 96.6\%$).
  - *Global Citizen Identity*: Elevates marginalized cuisines into fine dining legitimacy (Native American $+0.15$ and $+0.08$, Italian $+0.13$, Mexican $+0.08$ and $+0.06$) while de-centering Western haute culinary hegemony (French $-0.02$).
* **H7: Behavioral Dining Practices**:
  - *Adult Highbrow Arts Participation*: Credibly pro-chef ($\bar{\beta} = +0.117$, $P > 0 = 98.6\%$).
  - *Fine Dining Frequency*: Credibly pro-chef ($\bar{\beta} = +0.111$, $P > 0 = 98.2\%$), with strongest pro-chef pulls on subaltern cuisines (Native American $+0.20$, Mexican $+0.18$, Italian $+0.16$).
  - *Fast Food Frequency*: Centered near zero ($\bar{\beta} = +0.038$, spans zero).
* **H8: Aesthetic Disposition Validation**:
  - Liking "Exotic and Authentic" food credibly predicts domestic elder authenticity ($\bar{\beta} = -0.100$, $P(\beta < 0) \ge 99.4\%$), directly validating that authenticity seekers locate excellence in traditional domestic cooking. Holds across all subaltern cuisines (Nigerian $-0.13$, Peruvian $-0.13$, Moroccan $-0.12$, Ethiopian $-0.12$, Pakistani $-0.11$, Jamaican $-0.10$, Lebanese $-0.10$).
* **H9: Ideological Countervailing Slopes**:
  - Social conservatism acts as a countervailing force across subaltern cuisines (Native American $+0.30$, Pakistani $+0.22$, Lebanese $+0.21$, Jamaican $+0.21$, Mexican $+0.21$, Nigerian $+0.18$), pulling peripheral traditions toward professionalization.
* **H10: Non-Proportional Threshold Dynamics**:
  - Category-specific adjacent category contrasts relative to Category 4 (neutral midpoint) confirm asymmetric threshold shifts across scale levels.

---

## 4. Directional Credibility Standard (≥ 95% Posterior Probability Mass)

Bayesian credibility is evaluated based on directional posterior probability mass on either side of zero:
$$\text{Credibly Positive: } P(\theta > 0 \mid \text{Data}) \ge 0.95 \quad (\#0072B2 \text{ Okabe-Ito Blue})$$
$$\text{Credibly Negative: } P(\theta < 0 \mid \text{Data}) \ge 0.95 \quad (\#D55E00 \text{ Okabe-Ito Vermillion})$$
$$\text{Spans Zero / Uncertain: } 0.05 < P(\theta > 0 \mid \text{Data}) < 0.95 \quad (\text{gray60})$$

---

## 5. Visual Standards, Ordering Rules & Asset Pipeline

### Plot Conventions & Guidelines
* **Half-Eye Posterior Distributions (`stat_halfeye`):** Master consensus forest plots (Figure 1: Cuisine Random Intercepts; Figure 2: Fixed Effects Stability Envelope; Figures 3–7: Midpoint Contrasts; Figures 8–12: Random Slopes) incorporate full posterior density slabs, posterior medians, 80% and 95% credible intervals, with specification stability tick marks (`+`) representing individual model posterior medians.
* **Dual-Interval Multi-Width Thickness (`interval_size_range`):** Always supply `.width = c(0.80, 0.95)` with `interval_size_range = c(0.75, 1.9)` to visually distinguish the thick inner 80% credible interval from the thinner outer 95% credible interval. Never pass a single fixed scalar like `interval_size = ...`.
* **Marker & Color Mapping (Grayscale Consistency Standard):**
  - *Credible Shift (≥ 95% Posterior Mass):* Solid marker (`shape = 16`), `#0072B2` (Deep Blue) for pro-chef/positive shifts, `#D55E00` (Vermillion) for domestic elder/negative shifts.
  - *Non-Credible / Spanning Zero (< 95% Mass):* Rendered consistently in grayscale (`"gray60"`) with solid markers.
  - *Sizing:* `point_size = 4.8` (or `3.2` on dense multi-facet/multi-panel plots).
  - *Density Slabs:* `slab_alpha = 0.15` with `scale = 0.65` (or `0.45` on crowded categorical dodge plots).
* **Responsive Legend Layout & Centering:** Enforce single-row guide strips (`nrow = 1`) and centered legend boxes via `theme(legend.box = "horizontal", legend.box.just = "center", legend.spacing.x = unit(0.4, "cm"), legend.margin = margin(t = 6, b = 2))`.
* **Strict Effect-Size Ordering:**
  - *Fixed Effects Stability Envelope (Figure 2):* Order predictors along the y-axis strictly from lowest / most negative effect size at the bottom (Ethnoracial Mixed White $-0.257$) to highest / most positive effect size at the top (Social Conservatism $+0.148$).
  - *Baseline Cuisine Hierarchy (Figure 1):* Order cuisines along the y-axis strictly from lowest / most domestic elder at the bottom (Native American $-0.271$) to highest / most pro-chef at the top (French $+0.691$).
  - *Cuisine Random Slopes (Figures 8–12):* Partition y-axis into three vertical consecration facet strips: `Consecrated`, `Intermediate`, `Subaltern` via `facet_grid(cuisine_group ~ predictor_label, scales = "free", space = "free_y")`. Order cuisines within each tier by the focal predictor's median net slope.
* **Tightened Axis Framing:** Adjust `coord_cartesian()` and `scale_x_continuous()` tightly to empirical distribution spans (e.g. `[-0.65, +1.05]` for cuisine random intercepts; `[-0.52, +0.38]` for demographic fixed effects).

### Complete Figure Registry (Figures 1–12 in Manuscript)
* Figure 1: `Plots/cuisine_random_effects.png` (Baseline Cuisine Authenticity Hierarchy: Cross-Specification Consensus)
* Figure 2: `Plots/fixed_effects_stability_forest.png` (Cross-Specification Parameter Stability Envelope Across All 18 Models)
* Figure 3: `Plots/ideology_cs_midpoint_effects.png` (Political Ideology Multi-Model Midpoint Contrasts)
* Figure 4: `Plots/cultural_cs_midpoint_effects.png` (Cultural Capital Multi-Model Midpoint Contrasts)
* Figure 5: `Plots/practices_cs_midpoint_effects.png` (Dining Practices Multi-Model Midpoint Contrasts)
* Figure 6: `Plots/dispositions_cs_midpoint_effects.png` (Bourdieu Taste Dispositions Multi-Model Midpoint Contrasts)
* Figure 7: `Plots/cosmopolitan_cs_midpoint_effects.png` (Cosmopolitan Capital Multi-Model Midpoint Contrasts)
* Figure 8: `Plots/rs_cuisine_slopes_ideology.png` (Cuisine Random Slopes: Ideology, partitioned by Consecration Tiers)
* Figure 9: `Plots/rs_cuisine_slopes_cultural.png` (Cuisine Random Slopes: Cultural Capital, partitioned by Consecration Tiers)
* Figure 10: `Plots/rs_cuisine_slopes_practices.png` (Cuisine Random Slopes: Dining Practices, partitioned by Consecration Tiers)
* Figure 11: `Plots/rs_cuisine_slopes_dispositions.png` (Cuisine Random Slopes: Taste Dispositions, partitioned by Consecration Tiers)
* Figure 12: `Plots/rs_cuisine_slopes_cosmopolitan.png` (Cuisine Random Slopes: Cosmopolitan Capital, partitioned by Consecration Tiers)

---

## 6. Supercomputing & HPC Standards (UCLA Hoffman2)

* **Resource Footprint:** `#$ -pe shared 4`, `#$ -l h_data=4G` ($4 \text{ cores} \times 4\text{ GB} = 16\text{ GB}$ total memory request per job).
* **Walltime Bound:** `#$ -l h_rt=23:50:00` (always bound to just under 24 hours).
* **Module Loading:** Source `/u/local/Modules/default/init/bash`, load `gcc/10.2.0` before `R`.
* **Array Staggering:** Always stagger array task startups by at least 5 to 15 minutes (`sleep $(( (SGE_TASK_ID - 1) * 300 ))`) to prevent parallel compilation clashes.
* **Threaded MCMC:** Use `threads = threading(threads_per_chain)` in `brm()`.

---

## 7. Google Drive & Manuscript Synchronization Standards

* **Google Doc ID:** `1qU0OoUbKx_jQ6t1BvkSJ2F2mdbqmJbhqfyRs3SNdrNY`
* **Google Doc URL:** `https://docs.google.com/document/d/1qU0OoUbKx_jQ6t1BvkSJ2F2mdbqmJbhqfyRs3SNdrNY/edit?usp=sharing`
* **Master Driver Script:** `scripts/sync_manuscript.R` (run via `Rscript scripts/sync_manuscript.R` or `source("scripts/sync_manuscript.R")`).
* **Table Pre-Computation:** `scripts/generate_md_tables.R` serializes markdown tables to `cache/` (`table_hypotheses.md`, `table_fit_comparison.md`).
* **In-Place OpenXML Engine (`scripts/sync_manuscript.py`):**
  - **Non-Destructive Live Document Text Preservation (Crucial Source of Truth):**
    - The sync engine parses the live document downloaded from Google Drive (`draft_live.docx`) and **preserves all user prose, edits, paragraphs, headings, citations, and custom table content verbatim**.
    - Under no circumstances should manuscript sections or paragraphs be reconstructed from hardcoded Python template strings.
    - Only project table elements (`Table 1`, `Table 2`) and figure drawing/caption/note blocks (`Figures 1–12`) are surgically updated/injected at their respective anchors.
  - **Typography Rules & "Alien Font" Trap Prevention:**
    - Universal base font is **Alegreya Sans** across `docDefaults`, standard paragraph styles (Headings 1–6, Normal, Title, Subtitle), tables, captions, and notes.
    - Software/package names (`brms`, `cmdstanr`, `CmdStan`, `loo`, `bayesplot`, `tidybayes`, `ggplot2`) are formatted in **Courier New** two font sizes smaller (10 pt / `sz=20` relative to 12 pt body text).
    - All alien/foreign font overrides (Cardo, Nova Mono, Lato, Aptos, Caudex) and direct run overrides (`<w:shd>`, `<w:highlight>`, `<w:color>`, `<w:sz>`, `<w:vertAlign w:val="baseline"/>`) are strictly stripped from all XML parts (`document.xml`, `styles.xml`, `theme1.xml`, `fontTable.xml`, `footer1.xml`) while preserving genuine semantic formatting (`<w:b>`, `<w:i>`, `<w:vertAlign w:val="subscript"/>`, `<w:vertAlign w:val="superscript"/>`).
  - **Universal APA 7th Table Styling:**
    - Total table width scales to full **6.5-inch portrait width** (`9360 dxa`).
    - Column 1 left-justified (`jc="left"`), numeric/statistic columns center-justified (`jc="center"`).
    - Anti-word-break protection (`<w:suppressAutoHyphens/>` on all cell paragraphs) and `<w:noWrap/>` on numeric cells.
    - Row protection (`<w:cantSplit/>` on all rows) and repeating headers (`<w:tblHeader/>` on header row).
    - APA horizontal borders: 1pt top border (`sz="8"`), 0.5pt header-bottom border (`sz="4"`), 1pt table-bottom border (`sz="8"`), vertical borders `w:val="none"`.
    - Cell margins: top/bottom `120 dxa` (6pt), left/right `160 dxa` (8pt).
    - Concise column naming (e.g. `Model`, `Specification / Architecture`, `WAIC`, `ΔWAIC`).
  - **Mathematical Equations & Formulas (OMML Standards):**
    - Standalone mathematical formulas (Equations 1–3) are formatted as centered formulas with **zero first-line indent** (`firstLine="0"`), **centered alignment** (`jc="center"`), and balanced vertical spacing (`before="140" after="140"`).
    - Formatted using native OMML (`http://schemas.openxmlformats.org/officeDocument/2006/math` with `<m:oMath>`, `<m:f>`, `<m:sSub>`, `<m:sSup>`, `<m:nary>`, `<m:d>`) so Google Docs and Microsoft Word render native equation objects.
    - Standalone equation paragraphs are explicitly distinguished from normal body prose containing inline math runs to preserve 0.5-inch indentation on prose.
  - **Document Paragraph & Indentation Normalization:**
    - **Normal Body Prose:** Inherits `Normal` style with **0.5-inch first-line indent** (`w:firstLine="720"` / 36 pt), `w:left="0"`, `w:right="0"`, justified alignment (`jc="both"`).
    - **Headings (H1, H2, H3):** Zero first-line indent (`firstLine="0"`), left-aligned (`jc="left"`), with clean style inheritance and direct run overrides stripped. Automated purge of phantom empty heading paragraphs.
    - **Table Captions (`Table X. ...`):** Bold (`<w:b/>`), left-aligned (`jc="left"`), zero first-line indent (`firstLine="0"`), tight spacing (`before="180" after="80"`).
    - **Figure Drawings:** Dual DrawingML extent synchronization to native 6.5 in width ($5,943,600\text{ EMUs}$) and exact aspect-ratio height; centered (`jc="center"`), zero first-line indent.
    - **Figure Captions (`Figure X. ...`):** Placed **below** the figure drawing, bold (`<w:b/>`), justified (`jc="both"`), zero first-line indent (`firstLine="0"`), tight spacing (`before="80" after="60"`).
    - **Table & Figure Notes (`Note: ...`):** Strictly styled as `Normal` (**never** `Heading4`), zero first-line indent (`firstLine="0"`), justified (`jc="both"`), tight spacing (`before="40" after="200"`).
  - **Inline Placement Architecture:**
    - Tables and figures are placed **inline** directly following their substantive text discussions rather than segregated into separate appendix sections at the end of the manuscript.
    - **Table 1** (*Summary of Theoretical Hypotheses*): Placed at the conclusion of the `HYPOTHESES` section, directly preceding `DATA AND METHODS`.
    - **Table 2** (*Model Comparison and Predictive Fit Progression*): Placed inline under `Results -> Bayesian Model Fit and Taxonomy Comparison`.
    - **Figure 1** (*Baseline Cuisine Authenticity Hierarchy*): Placed inline under `Results -> Baseline Authenticity by Cuisine: Cross-Specification Consensus`.
    - **Figure 2** (*Cross-Specification Parameter Stability Envelope*): Placed inline under `Results -> Cross-Specification Consensus and Fixed Effects Stability Envelope`.
    - **Figures 3–7** (*Midpoint Contrasts: Ideology, Cultural Capital, Dining Practices, Dispositions, Cosmopolitan Capital*): Placed inline under their respective contrast subsections.
    - **Figures 8–12** (*Cuisine Random Slopes: Ideology, Cultural Capital, Dining Practices, Dispositions, Cosmopolitan Capital*): Placed inline under their respective domain slope subsections.
    - **Figure Presentation Standard:** High-resolution centered plot drawing (6.5 in width), followed below by its bold APA caption (`Figure X. ...`), followed below by its italicized APA note (`Note: ...`).
  - **Workspace Hygiene:** Automated `on.exit()` in `scripts/sync_manuscript.R` and `cleanup_scratch_files()` in `scripts/sync_manuscript.py` to purge transient intermediate files (`draft_*.docx`, `draft_*.txt`, `*.tmp`).
