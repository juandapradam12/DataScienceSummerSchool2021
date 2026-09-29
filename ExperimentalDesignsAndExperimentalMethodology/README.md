# Experimental Designs & Experimental Methodology

Design and analysis of survey experiments, with a focus on **conjoint** designs.

## Techniques

- Conjoint experiment data structure and estimation
- Average Marginal Component Effects (AMCE-style reporting)
- Effect plots for factor-level treatments
- Robust variance estimation for experimental contrasts

## Key files

| Path | Role |
|---|---|
| `experiment/conjoint.R` | End-to-end conjoint analysis |
| `experiment/effect_plotter.R` | Reusable effect-plot helper |
| `experiment/finalData.csv` | Analysis data |
| `experiment/conj_effects_pooled.pdf` | Example output figure |
| `experiments_slides.pdf` | Lecture slides |

## Tools

R · `ggplot2` · `dplyr` · `lmtest` / `sandwich`

## Takeaway

Connects experimental design choices to transparent, publication-style effect visualizations.

## What I take away

Conjoint designs turn multi-attribute preferences into estimable contrasts; effect plots are how those contrasts get communicated without dumping a giant regression table.
