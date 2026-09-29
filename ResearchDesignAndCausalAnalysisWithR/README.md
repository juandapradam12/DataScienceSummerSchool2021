# Research Design & Causal Analysis with R

Causal inference intuition through simulation: confounding, d-separation, and bad controls.

## Techniques

- OLS with and without confounders (simulation)
- Directed separation (d-separation) examples
- Post-treatment bias / collider-style mistakes
- Translating research-design ideas into estimable regressions

## Key files

| File | Role |
|---|---|
| `Hertie_DS_SS_Research_Design.ipynb` | Main executable notebook |
| `Hertie_DS_SS_Research_Design.R` | R script companion |
| `Research_Design.R` | Compact simulation scripts |
| `lecture.pdf` | Course slides |

## Tools

R · Jupyter (R kernel) · base `lm` simulations

## Takeaway

Shows when adding covariates helps identification — and when it hurts — using transparent simulated DGPs rather than black-box software.

## What I take away

Before adding covariates, ask what the DAG implies. Simulations make confounding and post-treatment bias visceral — a habit I use before trusting any observational regression.
