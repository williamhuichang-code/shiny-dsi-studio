# =================================================================================
# global.R
# =================================================================================

# ── LIBRARY ──────────────────────────────────────────────────────────────────

library(shiny)
library(bs4Dash)
library(dplyr)
library(waiter)
library(plotly)
library(ggrepel)
library(ggplot2)
library(DT)
library(recipes)
library(shinycssloaders) # busy spinner (compatibility with bs4Dash TBC)
library(cluster)
# caret, doParallel, cli, butcher, glmnet, pls, rpart, rpart.plot loaded in mod_meth_shared.R


# ── GLOBAL CONFIG ────────────────────────────────────────────────────────────

# file of interest
FILE_OF_INTEREST <- ""

# explicit data folder (bundled demo datasets live here)
DATA_WD <- "data"

# TRUE when running on a hosting service (shinyapps.io / Shiny Server) rather than
# locally. Used to keep each visitor's trained models private and to disable
# multi-core training, which the free hosting tier does not have.
IS_HOSTED <- nzchar(Sys.getenv("SHINY_PORT")) ||
  identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")

# largest CSV a visitor may upload (MB)
options(shiny.maxRequestSize = 10 * 1024^2)

# sets R to display numbers with 3 significant digits globally
DIGITS = 3


# ── TASK SPECIFIC DEFAULT ────────────────────────────────────────────────────

# Demo defaults: open the clinical dataset (Assignment 3) with its roles pre-set,
# so visitors can go straight to model selection. Comment out for general use.
FILE_OF_INTEREST <- "A3_clinical.csv"

general_initial <- c("impute_bag", 
                     "dow",
                     "other", 
                     "dummy", 
                     "interact", "lincomb",
                     "zv", "nzv", "center", "scale")

brnn_initial <- c("impute_bag", "dow", 
                  "other", 
                  "dummy", 
                  "interact", "lincomb",
                  "zv", "nzv", "center", "scale")

gaussprpoly_initial <- c("impute_bag", 
                         "dow",
                         "other", 
                         "dummy", 
                         "interact", "lincomb",
                         "zv", "nzv", "center", "scale")

svmpoly_initial <- c("impute_bag", 
                     "dow",
                     "other", 
                     "dummy", 
                     "interact", "lincomb",
                     "zv", "nzv", "center", "scale")

glmnet_initial <- c("impute_bag", "dow", 
                    "other", 
                    "YeoJohnson",  # need transformation
                    "dummy", 
                    "interact", "lincomb",
                    "zv", "nzv", "center", "scale")

svmrs_initial <- c("impute_bag", "dow", 
                   "other", 
                   "dummy", 
                   "interact", "lincomb",
                   "zv", "nzv", "center", "scale") # svmrs means svmRadialSigma

pls_initial <- c("impute_bag", "dow", 
                 "other", 
                 "dummy", 
                 "interact", "lincomb",
                 "zv", "nzv", "center", "scale")

# Column-name based: roles are applied only to columns that exist in the loaded
# file, so one list covers all three bundled datasets.
task_specific_roles <- list(
  # A3_clinical.csv
  Patient         = "obs_id",
  Response        = "outcome",
  ObservationDate = "date",
  # A2_public_health.csv
  CODE            = "obs_id",
  DEATH_RATE      = "outcome",
  OBS_TYPE        = "split",
  # A1_manufacturing.csv
  ID              = "obs_id",
  Y               = "outcome",
  Date            = "date"
)

SPLIT_SEED   <- 199
MODEL_SEED   <- 673
AUTO_SPLIT   <- TRUE
SPLIT_RATIO  <- 0.8

# Available Methods explorer — tags to exclude from the table/map by default,
# and tags to highlight as literature-informed choices.
# Fall back to character(0) (nothing pre-selected) when not defined.
av_exclude_tags <- c(
  "Two Class Only", "ROC Curves", "Text Mining", "String Kernel",
  "Self-Organising Maps", "Binary Predictors Only",
  "Categorical Predictors Only", "Cost Sensitive Learning",
  "Ordinal Outcomes"
)

av_highlight_tags <- c(
  "Polynomial Model", "Radial Basis Function", "Regularization", 
  "Partial Least Squares", "Generalized Linear Model", 
  "Multivariate Adaptive Regression Splines"
)

A3_omit_ids <- c(
  "tid-57748", "tid-57237", "tid-57537", "tid-57651",
  "tid-57689", "tid-57787", "tid-57761", "tid-57431",
  "tid-57479", "tid-57487", "tid-57600", "tid-57732",
  "tid-57739", "tid-57808", "tid-57845", "tid-57859",
  "tid-57921", "tid-57928", "tid-58050", "tid-58055",
  "tid-57470", "tid-57569", "tid-57580", "tid-57899"
)


# ── FILE LOADING LOGIC ───────────────────────────────────────────────────────

# all csv files as a list
csv_files <- list.files(DATA_WD, pattern = "\\.csv$", full.names = FALSE)

# add a (none) option to choices
file_choices_with_none    <- c("(none)", csv_files)

# prioritise on interested file and fallback at (none)
default_selected <- if (FILE_OF_INTEREST %in% csv_files) FILE_OF_INTEREST else "(none)"


# ── PREPROCESSING UTILITIES ──────────────────────────────────────────────────
# defined before module loading so category UIs can reference ppchoices at build time
# dynamicSteps() is defined in modules/mod_meth_tune.R (sourced into globalenv)

ppchoices <- c(
  "impute_knn", "impute_bag", "impute_median", "impute_mode",
  "YeoJohnson", "BoxCox", "log", "sqrt",
  "naomit",
  "pca", "pls", "ica",
  "center", "scale", "range", "spatialsign",
  "year", "quarter", "month", "week", "dow", "dateDecimal",
  "nzv", "zv", "other", "dummy",
  "poly", "interact", "lincomb",
  "indicate_na", "corr"
)

# ── MODULE LOADING LOGIC ─────────────────────────────────────────────────────

# look inside "modules" folder and its subs, load all files with .R according to their full paths
list.files("modules", pattern = "\\.R$", recursive = TRUE, full.names = TRUE) |>
  lapply(source)


# ── AESTHETIC LOGIC ──────────────────────────────────────────────────────────

# sets R to display numbers with n significant digits globally (set it in global config)
options(digits = DIGITS)




