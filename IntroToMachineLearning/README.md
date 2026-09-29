# Introduction to Machine Learning

Core supervised learning ideas implemented from scratch and with a modern R ML framework.

## Techniques

- Logistic regression derived and coded from scratch
- Likelihood, gradients, and iterative fitting intuition
- Bias–variance trade-off and resampling (e.g. LOOCV concepts)
- End-to-end modeling with **`mlr3`** (tasks, learners, resampling, evaluation)

## Key files

| File | Role |
|---|---|
| `01_ds3_ml_logit.ipynb` | In-class logit workbook |
| `01_ds3_ml_logit_complete.ipynb` | Completed logit-from-scratch solutions |
| `02_ds3_ml_mlr3.ipynb` | `mlr3` introduction |
| `00_ds3_ml_presentation.html` | Rendered lecture slides |
| `00_ds3_ml_presentation.Rmd` | Slide source |
| `bias-variance.png` / `LOOCV.gif` | Teaching visuals |

## Tools

R · Jupyter · `mlr3` ecosystem

## Original course

Marcel Neunhoeffer (LMU) & Christian Arnold (Cardiff). Upstream: [mneunhoe/ds3_ml](https://github.com/mneunhoe/ds3_ml).

## What I take away

Implementing logit from scratch builds intuition for loss and regularization; `mlr3` then shows how that estimator sits inside a repeatable train–resample–evaluate workflow.
