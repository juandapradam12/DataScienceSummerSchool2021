# Data Science Summer School 2021

**Portfolio of techniques from the CIVICA / Hertie School Data Science Summer School (DS³)**  
Collected and organized by [Juan David Prada](https://github.com/juandapradam12)

This repository is a curated learning portfolio: labs, notebooks, and notes covering the full stack of modern computational social science — from research design and causal inference to machine learning, text-as-data, networks, and web data collection.

---

## Why this repo

DS³ is an intensive program for social scientists who want to **collect, model, and communicate evidence with code**. The materials here show hands-on practice with the methods that matter in applied research and data work:

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
| **Spatial analysis** | Geo-data workflows in R |
| **Visualization** | Publication-ready graphs in base R / ggplot-oriented labs |
| **Data collection** | REST APIs, HTML scraping, Selenium automation |
| **Programming** | Python fundamentals for computational social science |

---

## Repository map

Materials are grouped by theme. Each folder has its own `README` with techniques, tools, and key files.

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
| [`NetworkAnalysis/`](NetworkAnalysis/) | Network structure, models, and contagion / spreading |

### 5. Spatial data & collection

| Module | Focus |
|---|---|
| [`GeoDataAndSpatialDataAnalysisWithR/`](GeoDataAndSpatialDataAnalysisWithR/) | Spatial data analysis course materials (external lab site) |
| [`WebScrapingWithR/`](WebScrapingWithR/) | APIs, open-web scraping, Selenium |

---

## Technique index

Jump straight to the method you care about:

- **Causal identification & simulation** → [`ResearchDesignAndCausalAnalysisWithR/`](ResearchDesignAndCausalAnalysisWithR/)
- **Conjoint AMCE-style analysis** → [`ExperimentalDesignsAndExperimentalMethodology/experiment/`](ExperimentalDesignsAndExperimentalMethodology/experiment/)
- **Randomization & estimands in online experiments** → [`SocialMediaBasedExperiments/labs/`](SocialMediaBasedExperiments/labs/)
- **MrP / multilevel poststratification** → [`ModernSurveyMethods/Lab/`](ModernSurveyMethods/Lab/)
- **Logistic regression from scratch + mlr3** → [`IntroToMachineLearning/`](IntroToMachineLearning/)
- **Neural nets (MNIST, Boston housing)** → [`IntroToDeepLearning/`](IntroToDeepLearning/)
- **CNNs on images** → [`ImageAsData/RetrainingACNN.ipynb`](ImageAsData/RetrainingACNN.ipynb)
- **Corpus construction, keyness, Naive Bayes, STM, Wordfish/Wordscores-style scaling** → [`Text2Data/exercises/`](Text2Data/exercises/)
- **TF–IDF + supervised NLP; topic models** → [`NaturalLanguageProcessing/`](NaturalLanguageProcessing/)
- **Network metrics, models, epidemics on graphs** → [`NetworkAnalysis/`](NetworkAnalysis/)
- **Twitter / NYT APIs, CSS selectors, Selenium** → [`WebScrapingWithR/`](WebScrapingWithR/)
- **Bullet graphs, tableplots, joy-plot maps** → [`DataVisualizationWithR/code/`](DataVisualizationWithR/code/)

---

## Stack

| Language | Typical libraries |
|---|---|
| **R** | `tidyverse`, `quanteda`, `stm`, `mlr3`, `lme4`, `estimatr`, `ri2`, `rvest` / API clients |
| **Python** | `numpy` / scientific stack, `scikit-learn`, `keras` / TensorFlow, Jupyter |

Notebooks marked for Colab can be opened in the browser; R labs are meant to be run from the module folder (relative paths preferred).

---

## How to browse

1. Start with this README to pick a theme.
2. Open the module `README` for learning outcomes and file pointers.
3. Prefer notebooks / `.R` labs over slide PDFs when you want executable practice; use PDFs for theory and lecture context.

> **Note:** Slide decks and some datasets are large. Clone with git LFS only if you later add it; otherwise a normal clone is enough.

---

## Attribution

Course content originates from the **CIVICA Data Science Summer School / Hertie School Data Lab (2021)** and the instructors who taught each module. Original course READMEs and licenses are preserved where present (e.g. web scraping materials). This portfolio reorganizes and documents those materials as a personal learning record.

---

## Author

**Juan David Prada** — data scientist.  
Portfolio focus: turning methods training into clear, reproducible analytical practice.
