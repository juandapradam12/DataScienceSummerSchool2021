"""Build and execute the portfolio showcase notebook with embedded outputs."""

from __future__ import annotations

import json
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    ConfusionMatrixDisplay,
    classification_report,
    f1_score,
)
from sklearn.model_selection import train_test_split
from sklearn.pipeline import Pipeline

ROOT = Path(__file__).resolve().parent
DATA = (
    ROOT.parent
    / "NaturalLanguageProcessing"
    / "TextClassification"
    / "wine_reviews_classification.xlsx"
)
FIG = ROOT / "figures"
FIG.mkdir(exist_ok=True)


def run_analysis() -> dict:
    df = pd.read_excel(DATA)
    df = df.dropna(subset=["description_cleaned", "country"]).copy()
    # Keep the four-country task from the DS³ NLP lab
    df = df[df["country"].isin(["US", "France", "Italy", "Spain"])]

    X_train, X_test, y_train, y_test = train_test_split(
        df["description_cleaned"],
        df["country"],
        test_size=0.25,
        random_state=42,
        stratify=df["country"],
    )

    pipe = Pipeline(
        steps=[
            (
                "tfidf",
                TfidfVectorizer(
                    max_features=5000,
                    ngram_range=(1, 2),
                    min_df=5,
                ),
            ),
            (
                "clf",
                LogisticRegression(
                    max_iter=1000,
                    class_weight="balanced",
                    solver="lbfgs",
                ),
            ),
        ]
    )
    pipe.fit(X_train, y_train)
    pred = pipe.predict(X_test)

    macro_f1 = f1_score(y_test, pred, average="macro")
    report = classification_report(y_test, pred, digits=3)
    labels = sorted(y_test.unique())

    fig, ax = plt.subplots(figsize=(6.5, 5.5))
    ConfusionMatrixDisplay.from_predictions(
        y_test,
        pred,
        labels=labels,
        cmap="Blues",
        colorbar=False,
        ax=ax,
    )
    ax.set_title("Country predicted from wine-review text")
    fig.tight_layout()
    cm_path = FIG / "confusion_matrix.png"
    fig.savefig(cm_path, dpi=160)
    plt.close(fig)

    # Top n-grams associated with each country (coefficients)
    vec: TfidfVectorizer = pipe.named_steps["tfidf"]
    clf: LogisticRegression = pipe.named_steps["clf"]
    terms = np.array(vec.get_feature_names_out())
    fig, axes = plt.subplots(2, 2, figsize=(10, 8), sharex=False)
    for ax, class_idx, country in zip(axes.ravel(), range(len(clf.classes_)), clf.classes_):
        coefs = clf.coef_[class_idx]
        top_idx = np.argsort(coefs)[-8:]
        ax.barh(terms[top_idx], coefs[top_idx], color="#1f4e79")
        ax.set_title(country)
        ax.set_xlabel("Logistic coefficient")
    fig.suptitle("Strongest TF–IDF cues by country", y=1.02)
    fig.tight_layout()
    feats_path = FIG / "top_features.png"
    fig.savefig(feats_path, dpi=160, bbox_inches="tight")
    plt.close(fig)

    return {
        "n_rows": int(len(df)),
        "train_size": int(len(X_train)),
        "test_size": int(len(X_test)),
        "macro_f1": float(macro_f1),
        "report": report,
        "class_counts": df["country"].value_counts().to_dict(),
        "cm_path": str(cm_path.relative_to(ROOT)),
        "feats_path": str(feats_path.relative_to(ROOT)),
        "labels": labels,
    }


def md(source: str) -> dict:
    return {"cell_type": "markdown", "metadata": {}, "source": [line + "\n" for line in source.split("\n")]}


def code(source: str, outputs: list | None = None) -> dict:
    return {
        "cell_type": "code",
        "execution_count": None,
        "metadata": {},
        "outputs": outputs or [],
        "source": [line + "\n" for line in source.split("\n")],
    }


def stdout(text: str) -> dict:
    return {
        "name": "stdout",
        "output_type": "stream",
        "text": [text if text.endswith("\n") else text + "\n"],
    }


def display_png(path: Path) -> dict:
    import base64

    data = base64.b64encode(path.read_bytes()).decode("ascii")
    return {
        "output_type": "display_data",
        "metadata": {},
        "data": {"image/png": data, "text/plain": ["<Figure>"]},
    }


def build_notebook(stats: dict) -> dict:
    counts = ", ".join(f"{k}: {v}" for k, v in sorted(stats["class_counts"].items()))
    cells = [
        md(
            "# Showcase: Infer wine country from review text\n"
            "\n"
            "**Juan David Prada** · Data Science Summer School 2021 portfolio piece\n"
            "\n"
            "This notebook turns one DS³ NLP lab into a short, readable case study: "
            "given only the cleaned tasting note, can a linear model recover the country of origin?\n"
            "\n"
            "### Why this matters\n"
            "In applied work the same pattern shows up everywhere — support tickets, policy documents, "
            "open-ended survey answers. You need a disciplined path from raw text → features → "
            "held-out evaluation → interpretable signals.\n"
            "\n"
            "### Pipeline\n"
            "1. Load preprocessed wine reviews (US, France, Italy, Spain)\n"
            "2. Stratified train / test split\n"
            "3. TF–IDF (unigrams + bigrams) + balanced logistic regression\n"
            "4. Report macro-F1, confusion matrix, and top coefficients per class"
        ),
        md("## 1. Data"),
        code(
            "from pathlib import Path\n"
            "\n"
            "import matplotlib.pyplot as plt\n"
            "import numpy as np\n"
            "import pandas as pd\n"
            "from sklearn.feature_extraction.text import TfidfVectorizer\n"
            "from sklearn.linear_model import LogisticRegression\n"
            "from sklearn.metrics import (\n"
            "    ConfusionMatrixDisplay,\n"
            "    classification_report,\n"
            "    f1_score,\n"
            ")\n"
            "from sklearn.model_selection import train_test_split\n"
            "from sklearn.pipeline import Pipeline\n"
            "\n"
            "DATA = Path(\"../NaturalLanguageProcessing/TextClassification/wine_reviews_classification.xlsx\")\n"
            "df = pd.read_excel(DATA).dropna(subset=[\"description_cleaned\", \"country\"])\n"
            "df = df[df[\"country\"].isin([\"US\", \"France\", \"Italy\", \"Spain\"])]\n"
            "\n"
            "print(f\"Rows: {len(df):,}\")\n"
            "print(df[\"country\"].value_counts())\n"
            "df[[\"country\", \"description_cleaned\"]].head(3)",
            outputs=[
                stdout(
                    f"Rows: {stats['n_rows']:,}\n"
                    + "\n".join(
                        f"{k:>6}    {v}"
                        for k, v in sorted(
                            stats["class_counts"].items(),
                            key=lambda kv: -kv[1],
                        )
                    )
                    + f"\nName: country, dtype: int64\n"
                )
            ],
        ),
        md(
            f"Class balance in this extract — {counts}. "
            "Stratified splitting keeps those proportions in train and test."
        ),
        md("## 2. Model"),
        code(
            "X_train, X_test, y_train, y_test = train_test_split(\n"
            "    df[\"description_cleaned\"],\n"
            "    df[\"country\"],\n"
            "    test_size=0.25,\n"
            "    random_state=42,\n"
            "    stratify=df[\"country\"],\n"
            ")\n"
            "\n"
            "pipe = Pipeline(\n"
            "    steps=[\n"
            "        (\"tfidf\", TfidfVectorizer(max_features=5000, ngram_range=(1, 2), min_df=5)),\n"
            "        (\n"
            "            \"clf\",\n"
            "            LogisticRegression(\n"
            "                max_iter=1000,\n"
            "                class_weight=\"balanced\",\n"
            "                solver=\"lbfgs\",\n"
            "            ),\n"
            "        ),\n"
            "    ]\n"
            ")\n"
            "pipe.fit(X_train, y_train)\n"
            "pred = pipe.predict(X_test)\n"
            "print(f\"Train: {len(X_train):,} | Test: {len(X_test):,}\")\n"
            "print(f\"Macro-F1: {f1_score(y_test, pred, average='macro'):.3f}\")",
            outputs=[
                stdout(
                    f"Train: {stats['train_size']:,} | Test: {stats['test_size']:,}\n"
                    f"Macro-F1: {stats['macro_f1']:.3f}\n"
                )
            ],
        ),
        md("## 3. Held-out evaluation"),
        code(
            "print(classification_report(y_test, pred, digits=3))\n"
            "\n"
            "fig, ax = plt.subplots(figsize=(6.5, 5.5))\n"
            "ConfusionMatrixDisplay.from_predictions(\n"
            "    y_test, pred, labels=sorted(y_test.unique()), cmap=\"Blues\", colorbar=False, ax=ax\n"
            ")\n"
            "ax.set_title(\"Country predicted from wine-review text\")\n"
            "fig.tight_layout()\n"
            "fig.savefig(\"figures/confusion_matrix.png\", dpi=160)\n"
            "plt.show()",
            outputs=[
                stdout(stats["report"]),
                display_png(ROOT / stats["cm_path"]),
            ],
        ),
        md(
            "## 4. What the model relies on\n"
            "\n"
            "Coefficients on TF–IDF features are a blunt but useful interpretability check: "
            "do the top n-grams look like geography, grape, or tasting-culture cues?"
        ),
        code(
            "vec = pipe.named_steps[\"tfidf\"]\n"
            "clf = pipe.named_steps[\"clf\"]\n"
            "terms = np.array(vec.get_feature_names_out())\n"
            "\n"
            "fig, axes = plt.subplots(2, 2, figsize=(10, 8))\n"
            "for ax, class_idx, country in zip(axes.ravel(), range(len(clf.classes_)), clf.classes_):\n"
            "    coefs = clf.coef_[class_idx]\n"
            "    top_idx = np.argsort(coefs)[-8:]\n"
            "    ax.barh(terms[top_idx], coefs[top_idx], color=\"#1f4e79\")\n"
            "    ax.set_title(country)\n"
            "    ax.set_xlabel(\"Logistic coefficient\")\n"
            "fig.suptitle(\"Strongest TF–IDF cues by country\", y=1.02)\n"
            "fig.tight_layout()\n"
            "fig.savefig(\"figures/top_features.png\", dpi=160, bbox_inches=\"tight\")\n"
            "plt.show()",
            outputs=[display_png(ROOT / stats["feats_path"])],
        ),
        md(
            "## Takeaway\n"
            "\n"
            f"- **Macro-F1 ≈ {stats['macro_f1']:.2f}** on a stratified held-out set — strong enough to show "
            "that country signal lives in tasting language, not only in structured fields.\n"
            "- The reusable pattern from DS³: **preprocess once → vectorize with a frozen vocabulary → "
            "evaluate on data the model never saw → inspect coefficients before shipping predictions**.\n"
            "- Same workflow transfers to survey open-ends, news corpora, or moderation queues.\n"
            "\n"
            "Related course materials: "
            "[`NaturalLanguageProcessing/`](../NaturalLanguageProcessing/) · "
            "[`Text2Data/`](../Text2Data/)."
        ),
    ]

    for cell in cells:
        text = "".join(cell["source"])
        if not text.endswith("\n"):
            text += "\n"
        cell["source"] = text.splitlines(keepends=True)

    return {
        "nbformat": 4,
        "nbformat_minor": 5,
        "metadata": {
            "kernelspec": {
                "display_name": "Python 3",
                "language": "python",
                "name": "python3",
            },
            "language_info": {"name": "python", "pygments_lexer": "ipython3"},
        },
        "cells": cells,
    }


def main() -> None:
    stats = run_analysis()
    nb = build_notebook(stats)
    out = ROOT / "country_from_reviews.ipynb"
    out.write_text(json.dumps(nb, indent=1, ensure_ascii=False) + "\n")
    print(f"Wrote {out}")
    print(f"Macro-F1: {stats['macro_f1']:.3f}")
    print(f"Figures: {stats['cm_path']}, {stats['feats_path']}")


if __name__ == "__main__":
    main()
