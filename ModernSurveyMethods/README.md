# Modern Survey Methods

Small-area and subgroup opinion estimation with multilevel regression and poststratification.

## Techniques

- Classical survey estimation baselines
- **MrP** (Multilevel Regression and Poststratification)
- **MrsP** / automated MrP workflows (`autoMrP`)
- Combining survey microdata with census poststratification frames

## Key files

| Path | Role |
|---|---|
| `Lab/Family_P_Code.R` | Worked MrP / MrsP / autoMrP illustration |
| `Lab/Minaret_B.dta` | Survey microdata example |
| `Lab/Census.Rda` | Census / poststratification object |
| `survey-slides.pdf` | Lecture slides |

## Tools

R · `lme4` · `autoMrP` · `haven` · `arm`

## How to run

Open R in `Lab/` (so relative paths to `.dta` / `.Rda` resolve), then source `Family_P_Code.R`.

## What I take away

National margins are not enough for local questions. MrP-style models let survey microdata speak at finer geographies when paired with a good poststratification frame.
