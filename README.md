# Data Science Summer School 2021

**Portfolio of techniques from the CIVICA / Hertie School Data Science Summer School (DS³)**  
Juan David Prada · [GitHub](https://github.com/juandapradam12)

A curated learning portfolio: labs and notebooks across modern computational social science — research design, experiments, surveys, ML/DL, text-as-data, networks, spatial analysis, and web data collection.

**Stack:** R · Python · scikit-learn · quanteda · mlr3 · Keras · tidyverse

---

## Start here (≈15 minutes)

1. **[`showcase/`](showcase/)** — end-to-end text classification case study (TF–IDF → logistic regression → held-out metrics → coefficient plots). Macro-F1 ≈ 0.88.
2. **[`ResearchDesignAndCausalAnalysisWithR/`](ResearchDesignAndCausalAnalysisWithR/)** — when controls help vs. hurt (confounding & post-treatment bias by simulation).
3. **[`IntroToMachineLearning/`](IntroToMachineLearning/)** — logistic regression from scratch, then `mlr3`.

Then skim the capability map below and dive into the modules that match the role or project you care about.

---

## What I take away from DS³

| I can… | Evidence in this repo |
|---|---|
| Argue about identification before fitting a model | Causal simulations, conjoint & social-media experiment labs |
| Build honest supervised baselines for text | Showcase + NLP / Text2Data modules |
| Move from toy estimators to a real ML workflow | Logit-from-scratch → `mlr3` |
| Collect web data responsibly | APIs, HTML scraping, Selenium |
| Communicate results with clear graphics | Visualization labs + showcase figures |

---

## Why this repo

| Capability | What you will find |
|---|---|
| **Causal thinking** | Identification, DAG intuition, OLS with controls, post-treatment bias |
| **Experiments** | Conjoint designs, social-media experiments, randomization inference |
| **Survey methods** | Multilevel regression and poststratification (MrP / MrsP) |
| **Classical ML** | Logistic regression from scratch, `mlr3`, bias–variance, resampling |
| **Deep learning** | Neural nets on MNIST & tabular data; CNN transfer / retraining |
| **Text as data** | Corpora, classification, topic models, ideal-point style scaling |
| **NLP pipelines** | TF–IDF, supervised text classification, topic modeling on reviews |
| **Networks** | Structure, generative models, spreading processes |
| **Spatial analysis** | Geo-data workflows in R (linked course materials) |
| **Visualization** | Publication-ready graphs in base R / ggplot-oriented labs |
| **Data collection** | REST APIs, HTML scraping, Selenium automation |
| **Programming** | Python fundamentals for computational social science |

---

## Repository map

Each folder has a `README` with techniques, tools, key files, and a short personal takeaway.

### Showcase

| Module | Focus |
|---|---|
| [`showcase/`](showcase/) | Polished case study: predict wine country from review text |

### 1. Foundations

| Module | Focus |
|---|---|
| [`IntroductionToPython/`](IntroductionToPython/) | Types, control flow, functions, classes; speech-comparison mini-project |
| [`DataVisualizationWithR/`](DataVisualizationWithR/) | Exploratory and presentation graphics (Gapminder, census, maps) |
| [`ResearchDesignAndCausalAnalysisWithR/`](ResearchDesignAndCausalAnalysisWithR/) | Causal graphs, confounding, d-separation, simulation-based intuition |

### 2. Experiments & surveys

| Module | Focus |
|---|---|
| [`ExperimentalDesignsAndExperimentalMethodology/`](ExperimentalDesignsAndExperimentalMethodology/) | Conjoint experiments and effect plots |
| [`SocialMediaBasedExperiments/`](SocialMediaBasedExperiments/) | Design & analysis of social-media experiments; clustering |
| [`ModernSurveyMethods/`](ModernSurveyMethods/) | MrP / MrsP / autoMrP for small-area opinion estimation |

### 3. Machine learning & deep learning

| Module | Focus |
|---|---|
| [`IntroToMachineLearning/`](IntroToMachineLearning/) | Logit from scratch; `mlr3` workflow |
| [`IntroToDeepLearning/`](IntroToDeepLearning/) | Feed-forward nets, train/val/test, regression with Keras |
| [`ImageAsData/`](ImageAsData/) | CNN training / retraining for digit recognition |

### 4. Text, language & networks

| Module | Focus |
|---|---|
| [`Text2Data/`](Text2Data/) | quanteda corpus workflows: intro → classification → STM → scaling |
| [`NaturalLanguageProcessing/`](NaturalLanguageProcessing/) | Sklearn text classification & topic models on wine reviews |
| [`NetworkAnalysis/`](NetworkAnalysis/) | Network structure, models, and contagion *(slide reference)* |

### 5. Spatial data & collection

| Module | Focus |
|---|---|
| [`GeoDataAndSpatialDataAnalysisWithR/`](GeoDataAndSpatialDataAnalysisWithR/) | Spatial analysis pointer to the live course site *(external labs)* |
| [`WebScrapingWithR/`](WebScrapingWithR/) | APIs, open-web scraping, Selenium |

---

## Technique index

- **Causal identification & simulation** → [`ResearchDesignAndCausalAnalysisWithR/`](ResearchDesignAndCausalAnalysisWithR/)
- **Conjoint AMCE-style analysis** → [`ExperimentalDesignsAndExperimentalMethodology/experiment/`](ExperimentalDesignsAndExperimentalMethodology/experiment/)
- **Randomization & estimands in online experiments** → [`SocialMediaBasedExperiments/labs/`](SocialMediaBasedExperiments/labs/)
- **MrP / multilevel poststratification** → [`ModernSurveyMethods/Lab/`](ModernSurveyMethods/Lab/)
- **Logistic regression from scratch + mlr3** → [`IntroToMachineLearning/`](IntroToMachineLearning/)
- **Neural nets (MNIST, Boston housing)** → [`IntroToDeepLearning/`](IntroToDeepLearning/)
- **CNNs on images** → [`ImageAsData/RetrainingACNN.ipynb`](ImageAsData/RetrainingACNN.ipynb)
- **End-to-end text classification showcase** → [`showcase/`](showcase/)
- **Corpus construction, keyness, Naive Bayes, STM, scaling** → [`Text2Data/exercises/`](Text2Data/exercises/)
- **TF–IDF + supervised NLP; topic models** → [`NaturalLanguageProcessing/`](NaturalLanguageProcessing/)
- **Network metrics, models, epidemics on graphs** → [`NetworkAnalysis/`](NetworkAnalysis/)
- **Twitter / NYT APIs, CSS selectors, Selenium** → [`WebScrapingWithR/`](WebScrapingWithR/)
- **Bullet graphs, tableplots, joy-plot maps** → [`DataVisualizationWithR/code/`](DataVisualizationWithR/code/)

---

## Setup

Python notebooks (showcase, NLP, deep learning):

```bash
python -m venv .venv
source .venv/bin/activate   # Windows: .venv\Scripts\activate
pip install -r requirements.txt
```

R labs assume a recent R / RStudio with packages listed in each module README (`tidyverse`, `quanteda`, `mlr3`, `lme4`, etc.).

> Some lecture PDFs are large (especially Network Analysis). Prefer notebooks and `.R` labs for hands-on review.

---

## Suggested GitHub repo settings

To make the landing page match this portfolio, set on GitHub → Settings:

- **Description:** `CIVICA / Hertie DS³ 2021 portfolio — causal inference, experiments, ML/DL, text-as-data, and web data collection in R & Python`
- **Topics:** `data-science`, `computational-social-science`, `causal-inference`, `machine-learning`, `nlp`, `r`, `python`, `text-as-data`, `survey-methods`, `web-scraping`

---

## Attribution

Course content originates from DS³ instructors; see [`ATTRIBUTION.md`](ATTRIBUTION.md). Original licenses are preserved where present (e.g. web scraping materials).
