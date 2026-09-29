# Natural Language Processing

Practical NLP in Python: supervised text classification and unsupervised topic modeling.

## Techniques

### Text classification (`TextClassification/`)

- Text preprocessing checklist for labeled corpora
- TF–IDF vectorization (`TfidfVectorizer`)
- Feature selection and supervised learning for multi-class labels
- Held-out prediction and classification reports (wine country / origin style task)

### Topic modeling (`TopicModeling/`)

- Lemma-oriented cleaning patterns (spaCy-style)
- Topic model fitting and inspection on large review corpora
- Interpreting topics as latent themes in unstructured text

## Key files

| Path | Role |
|---|---|
| `TextClassification/Text_classification.ipynb` | Full classification pipeline |
| `TextClassification/wine_reviews_classification.xlsx` | Labeled review subset |
| `TopicModeling/Topic_Models.ipynb` | Topic modeling notebook |
| `TopicModeling/wine_reviews_topics.xlsx` | Topic-modeling corpus |
| `*.pdf` | Supporting lecture notes |

## Tools

Python · scikit-learn · pandas · Jupyter · (spaCy concepts for cleaning)
