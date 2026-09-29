# Text as Data

From raw documents to corpora, classifiers, topic models, and scaling models in R.

## Techniques

1. **Corpus construction** — `readtext`, document variables, keyness plots  
2. **Supervised classification** — train/test splits, `quanteda.textmodels`, ROC-style evaluation  
3. **Topic models** — Structural Topic Models (`stm`) on reshaped paragraphs  
4. **Text scaling** — Wordscores / Wordfish-style ideal-point models with labeled plots  

## Key files

| Path | Role |
|---|---|
| `exercises/1-intro/` | Manifestos → corpus & keyness |
| `exercises/2-classification/` | Movie-review classification |
| `exercises/3-topic-models/` | STM topic modeling |
| `exercises/4-scaling/` | Scaling / ideal points |
| `tada-slides.pdf` | Lecture slides |

## Tools

R · `quanteda` · `quanteda.textmodels` · `stm` · `tidyverse` · `ggrepel`

## How to browse

Each exercise folder contains an `.R` script (and often a rendered `.html`). Open the project folder first so relative data paths resolve.

## What I take away

A full text-as-data ladder in R: corpus → supervised classification → STM topics → scaling. I reach for this stack when the documents are political or legislative and the workflow should stay in quanteda.
