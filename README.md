# DSI Studio: a Reusable Data Analysis and Modelling App (R Shiny)

A point-and-click R Shiny app that takes any CSV through a complete modelling workflow: exploratory analysis, missing-data and outlier handling, preprocessing, and comparing 30+ regression models, with every step reproducible and every stage's data downloadable.

Built for the University of Canterbury course *Data Science in Industry* (DATA423, grade A+) as one modular framework reused across three assignments on manufacturing, public-health and clinical data.

**[▶ Try the live app](https://williamhuichang.shinyapps.io/dsi-studio/)** (free hosting: the first load after a quiet spell takes about a minute)

## What it does

The sidebar follows the order of a real analysis. Each stage passes its cleaned data to the next.

| Stage | What you can do |
|---|---|
| **Config** | Assign column roles (outcome, ID, date, train/test split, weights, stratifier, sensitive, ignore); split the data 6 ways (train/test, train/validation/test, stratified bootstrap, leave-group-out, time series, diversity sampling) with fixed seeds; download the data at any stage |
| **EDA** (13 views) | Data table, summary, word cloud, missingness map and UpSet plot, rising-value plot, mosaic, tabplot, correlation heatmap, pairs plot, bar and box plots, interaction plots |
| **Missing data** (8 tools) | Detect disguised missing codes (e.g. `-99`, `--`), add shadow variables, treat "not applicable" values, drop excessively missing rows/columns, compare imputation methods (KNN, bagged trees, median/mode), check transformations, and model *why* data is missing (rpart tree, variable importance) |
| **Outliers** (11 tools) | Histogram, boxplot, bagplot, Mahalanobis distance, Cook's distance, local outlier factor, one-class SVM, random forest, isolation forest, a consensus summary across methods, and outcome (response) outliers |
| **Models** | Browse caret's model catalogue as a table or map, then train models from 7 families (null baseline, linear/regularised, trees, kernel methods, ensembles, neural networks, wildcards such as MARS and M5). Each model gets its own chain of up to 31 preprocessing steps (imputation, transforms, PCA/PLS, dummy coding, interactions and more) |
| **Selection & performance** | Compare cross-validated results across all trained models, pick one, and check it on the held-out test set |

## Try it with the bundled data

Three datasets are included (synthetic data provided for the DATA423 course), with their roles pre-set:

- **A3_clinical.csv** (opens by default): predict a patient response from reagent measurements and lifestyle factors.
- **A2_public_health.csv**: predict death rates from health and economic indicators; about 15% of values are missing.
- **A1_manufacturing.csv**: 30 sensor readings plus categorical process variables; good for EDA and outliers.

Or use **Upload CSV** in the header to load your own file (up to 10 MB).

**Tip for a quick look:** open *Methods*, pick a model such as `glmnet` and click **Load** to load a pre-trained demo model instead of training it, then go to *Model Selection* and *Performance*.

## How it's built

- **Modular:** 45 Shiny modules, one per view or step, found and loaded automatically from the `modules/` folder. Adding a view means adding one file.
- **Chained pipeline:** raw data → missing-value variants → shadow variables → "not applicable" handling → excessive missingness → response outliers → imputation → transformation. Each stage is a reactive step, so changing an early choice updates everything downstream.
- **Config-driven:** dataset-specific defaults (roles, seeds, preprocessing per model) live in `global.R`, so the same app serves any dataset.
- **Reproducible:** fixed seeds for splitting and model training; trained models are saved and can be reloaded.

## Run it locally

Requires R (4.3+) and the packages loaded in `global.R` and `modules/method/mod_meth_shared.R`.

```r
shiny::runApp(".")   # from this folder, or open ui.R in RStudio and click Run App
```

To deploy your own copy to shinyapps.io, see `deploy.R`.

## Repository contents

```
global.R, ui.R, server.R   app entry points and configuration
modules/                   config, eda, miss, out and method modules
data/                      the three bundled datasets
deploy.R                   one-command deployment to shinyapps.io
```

## Credits

- Course: DATA423 Data Science in Industry, University of Canterbury (lecturer: Phil Davies).
- Built with [shiny](https://shiny.posit.co/), [bs4Dash](https://rinterface.github.io/bs4Dash/), [caret](https://topepo.github.io/caret/) and [recipes](https://recipes.tidymodels.org/).
- The hosting changes (file upload, per-visitor model storage, single-core training when hosted) were made with AI-assisted coding.
