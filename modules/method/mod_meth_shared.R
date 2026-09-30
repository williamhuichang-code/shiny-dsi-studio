# =================================================================================
# mod_meth_shared.R
# =================================================================================
# Source this file (NOT moduleServer'd).  Provides everything category modules
# and method modules need so they stay thin and focused.
#
# Provides
#   ─ Global utilities     startMode / stopMode / description
#                          saveToRds / loadRds / deleteRds / dynamicSteps
#   ─ UI factories         .section_hdr / .meth_subtabs_ui / .meth_sidebar_ui
#   ─ Render helpers       .recipe_html_output / .recipe_term_table
#   ─ Server helpers       .meth_get_cfg / .meth_build_recipe /
#                          .meth_build_tr_control / .meth_register_outputs
# =================================================================================

library(caret)
library(doParallel)
library(cli)
library(butcher)
library(glmnet)
library(pls)
library(rpart)
library(rpart.plot)
library(evtree)
library(kernlab)
library(randomForest)
library(Cubist)
library(ranger)
library(qrnn)
library(brnn)
library(earth)
library(RWeka)


# ── Null-coalescing operator ──────────────────────────────────────────────────

`%||%` <- function(a, b) if (!is.null(a)) a else b


# =================================================================================
# GLOBAL UTILITIES
# =================================================================================

startMode <- function(parallel = TRUE) {
  if (isTRUE(parallel)) {
    outfile <- tempfile(pattern = "output")
    unlink(outfile)
    clus <- makeCluster(min(c(3, detectCores(all.tests = FALSE, logical = TRUE))),
                        outfile = outfile)
    registerDoParallel(clus)
    list(cluster = clus, outfile = outfile)
  } else {
    registerDoSEQ()   # ensure any previously registered parallel backend is cleared
    NULL
  }
}

stopMode <- function(obj) {
  if (!is.null(obj)) {
    stopCluster(obj$cluster)
    lines <- readLines(con = obj$outfile)
    lapply(paste0(lines, "\n"), FUN = cat)
    unlink(obj$outfile)
    registerDoSEQ()
  }
}

description <- function(name) {
  regexName <- paste0("^", name, "$")
  mlist     <- caret::getModelInfo(model = regexName)[[name]]
  line0     <- paste0(mlist$label, " (", name, ")")
  line1     <- paste0("Method \"", name, "\" is able to do ",
                      paste(collapse = " and ", mlist$type), ".")
  line2     <- paste0("It uses parameters: ",
                      paste0(collapse = ", ", mlist$parameters$parameter), ".")
  line3     <- paste0("Its characteristics are: ",
                      paste0(collapse = ", ", mlist$tags))
  paste(sep = "\n", line0, "", line1, line2, line3)
}

# Where trained models are written. Locally: ./SavedModels (as before).
# When hosted: a private per-visitor folder, so visitors never overwrite each
# other's models or the bundled demo models in ./SavedModels.
modelDir <- function(session = NULL) {
  if (!isTRUE(IS_HOSTED) || is.null(session)) return(file.path(".", "SavedModels"))
  d <- file.path(tempdir(), "SavedModels", session$token)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

saveToRds <- function(model, name, session = NULL) {
  try(
    if (!is.null(model$finalModel$classifier))
      rJava::.jcache(model$finalModel$classifier),
    silent = TRUE
  )
  file   <- file.path(modelDir(session), paste0(name, ".rds"))
  model2 <- butcher::axe_env(model, verbose = TRUE)
  saveRDS(model2, file)
}

loadRds <- function(name, session) {
  # the visitor's own model first, then the bundled demo model
  rdsfile <- file.path(modelDir(session), paste0(name, ".rds"))
  if (!file.exists(rdsfile)) rdsfile <- file.path(".", "SavedModels", paste0(name, ".rds"))
  if (!file.exists(rdsfile)) {
    showNotification("Model needs to be trained first", session = session, duration = 3)
    return(NULL)
  }
  showNotification(paste("Loading", name, "from", rdsfile), session = session, duration = 3)
  readRDS(file = rdsfile)
}

deleteRds <- function(name, session = NULL) {
  rdsfile <- file.path(modelDir(session), paste0(name, ".rds"))
  if (file.exists(rdsfile)) unlink(rdsfile, force = TRUE) else TRUE
}

# Builds a recipes pipeline from an ordered character vector of step names.
# cfg: named list of hyperparameter overrides, e.g. list(pca_num_comp = 10).
dynamicSteps <- function(recipe, preprocess, cfg = list()) {
  if (is.null(preprocess)) stop("preprocess is NULL - check control identifier")

  # Capture original predictor types before dummy/date steps alter the column set.
  var_info     <- recipe$var_info
  orig_numeric <- var_info$variable[
    var_info$role == "predictor" &
      vapply(var_info$type, function(t) any(c("double", "integer") %in% t), logical(1))
  ]
  orig_nominal <- var_info$variable[
    var_info$role == "predictor" &
      vapply(var_info$type, function(t) "nominal" %in% t, logical(1))
  ]

  for (s in preprocess) {
    if (s == "impute_knn") {
      recipe <- step_impute_knn(recipe, all_numeric_predictors(), all_nominal_predictors(),
                                neighbors = cfg$impute_knn_neighbors %||% 2)

    } else if (s == "impute_bag") {
      recipe <- step_impute_bag(recipe, all_numeric_predictors(), all_nominal_predictors(),
                                trees = cfg$impute_bag_trees %||% 4)

    } else if (s == "impute_median") {
      recipe <- recipes::step_impute_median(recipe, all_numeric_predictors())

    } else if (s == "impute_mode") {
      recipe <- recipes::step_impute_mode(recipe, all_nominal_predictors())

    } else if (s == "YeoJohnson") {
      recipe <- recipes::step_YeoJohnson(recipe, all_numeric_predictors())

    } else if (s == "BoxCox") {
      recipe <- recipes::step_BoxCox(recipe, all_numeric_predictors())

    } else if (s == "log") {
      recipe <- recipes::step_log(recipe, all_numeric_predictors())

    } else if (s == "sqrt") {
      recipe <- recipes::step_sqrt(recipe, all_numeric_predictors())

    } else if (s == "naomit") {
      recipe <- recipes::step_naomit(recipe, all_predictors(), skip = TRUE)

    } else if (s == "pca") {
      recipe <- recipes::step_pca(recipe, all_numeric_predictors(),
                                  num_comp = cfg$pca_num_comp %||% 25)

    } else if (s == "pls") {
      recipe <- recipes::step_pls(recipe, all_numeric_predictors(), outcome = "Response",
                                  num_comp = cfg$pls_num_comp %||% 25)

    } else if (s == "ica") {
      recipe <- recipes::step_ica(recipe, all_numeric_predictors(),
                                  num_comp = cfg$ica_num_comp %||% 25)

    } else if (s == "center") {
      recipe <- recipes::step_center(recipe, all_numeric_predictors())

    } else if (s == "scale") {
      recipe <- recipes::step_scale(recipe, all_numeric_predictors())

    } else if (s == "range") {
      recipe <- recipes::step_range(recipe, all_numeric_predictors())

    } else if (s == "spatialsign") {
      recipe <- recipes::step_spatialsign(recipe, all_numeric_predictors())

    } else if (s == "year") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("year"),    ordinal = FALSE)

    } else if (s == "quarter") {
      if (isTRUE(cfg$cyclic_quarter)) {
        # Cyclic sin/cos encoding (period = 4). Quarter derived from month: ceiling(month/3).
        recipe <- recipes::step_mutate(recipe,
          dplyr::across(where(~inherits(., "Date")),
            list(quarter_sin = ~sin(2 * pi * ceiling(as.integer(format(., "%m")) / 3) / 4),
                 quarter_cos = ~cos(2 * pi * ceiling(as.integer(format(., "%m")) / 3) / 4)),
            .names = "{.col}_{.fn}"))
      } else {
        recipe <- recipes::step_date(recipe, recipes::has_type("date"), features = "quarter", ordinal = FALSE)
      }

    } else if (s == "month") {
      if (isTRUE(cfg$cyclic_month)) {
        # Cyclic sin/cos encoding (period = 12) applied directly to raw date columns.
        # The date column itself is removed by step_rm(has_type("date")) in .meth_build_recipe().
        recipe <- recipes::step_mutate(recipe,
          dplyr::across(where(~inherits(., "Date")),
            list(month_sin = ~sin(2 * pi * as.integer(format(., "%m")) / 12),
                 month_cos = ~cos(2 * pi * as.integer(format(., "%m")) / 12)),
            .names = "{.col}_{.fn}"))
      } else {
        recipe <- recipes::step_date(recipe, recipes::has_type("date"), features = "month", ordinal = FALSE)
      }

    } else if (s == "week") {
      if (isTRUE(cfg$cyclic_week)) {
        # Cyclic sin/cos encoding (period = 52, ISO week).
        recipe <- recipes::step_mutate(recipe,
          dplyr::across(where(~inherits(., "Date")),
            list(week_sin = ~sin(2 * pi * as.integer(format(., "%V")) / 52),
                 week_cos = ~cos(2 * pi * as.integer(format(., "%V")) / 52)),
            .names = "{.col}_{.fn}"))
      } else {
        recipe <- recipes::step_date(recipe, recipes::has_type("date"), features = "week", ordinal = FALSE)
      }

    } else if (s == "dow") {
      if (isTRUE(cfg$cyclic_dow)) {
        # Cyclic sin/cos encoding (period = 7, %u: Mon=1 … Sun=7 → remap to 0–6).
        recipe <- recipes::step_mutate(recipe,
          dplyr::across(where(~inherits(., "Date")),
            list(dow_sin = ~sin(2 * pi * (as.integer(format(., "%u")) - 1L) / 7),
                 dow_cos = ~cos(2 * pi * (as.integer(format(., "%u")) - 1L) / 7)),
            .names = "{.col}_{.fn}"))
      } else {
        recipe <- recipes::step_date(recipe, recipes::has_type("date"), features = "dow", ordinal = FALSE)
      }

    } else if (s == "dateDecimal") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("decimal"), ordinal = FALSE)

    } else if (s == "zv") {
      recipe <- recipes::step_zv(recipe, all_predictors())

    } else if (s == "nzv") {
      recipe <- recipes::step_nzv(recipe, all_predictors(),
                                  freq_cut   = cfg$nzv_freq_cut   %||% 95/5,
                                  unique_cut = cfg$nzv_unique_cut  %||% 10)

    } else if (s == "other") {
      recipe <- recipes::step_other(recipe, all_nominal_predictors(),
                                    threshold = cfg$other_threshold %||% 0.05,
                                    other     = cfg$other_label     %||% "other")

    } else if (s == "dummy") {
      recipe <- recipes::step_dummy(recipe, all_nominal_predictors(), one_hot = FALSE)

    } else if (s == "poly") {
      recipe <- recipes::step_poly(recipe, all_numeric_predictors(),
                                   degree = cfg$poly_degree %||% 2)

    } else if (s == "interact") {
      interact_type <- cfg$interact_type %||% "all"
      terms <- switch(interact_type,
        "all" = ~ all_numeric_predictors():all_numeric_predictors(),
        "nominal_numeric" = {
          pattern <- paste0("^(", paste(orig_nominal, collapse = "|"), ")_")
          as.formula(paste0("~ matches('", pattern, "'):all_numeric_predictors()"))
        },
        "nominal_nominal" = {
          pattern <- paste0("^(", paste(orig_nominal, collapse = "|"), ")_")
          as.formula(paste0("~ matches('", pattern, "'):matches('", pattern, "')"))
        },
        "numeric_numeric" = ~ all_numeric_predictors():all_numeric_predictors(),
        stop(paste("Unknown interact_type:", interact_type))
      )
      recipe <- recipes::step_interact(recipe, terms = terms)

    } else if (s == "lincomb") {
      recipe <- recipes::step_lincomb(recipe, all_numeric_predictors())

    } else if (s == "indicate_na") {
      recipe <- recipes::step_indicate_na(recipe, all_predictors())

    } else if (s == "corr") {
      recipe <- recipes::step_corr(recipe, all_numeric_predictors(),
                                   threshold = cfg$corr_threshold %||% 0.9)

    } else if (s == "rm") {
      # intentionally blank

    } else {
      stop(paste("Attempting to use an unknown recipe step:", s))
    }
  }
  recipe
}


# =================================================================================
# UI FACTORIES
# =================================================================================

# ── Section header chip ───────────────────────────────────────────────────────

.section_hdr <- function(label, bg, border_col) {
  div(
    style = paste0(
      "background:", bg, "; border-left:4px solid ", border_col, ";",
      "padding:6px 10px; border-radius:4px; margin:14px 0 8px;",
      "font-weight:700; font-size:13px; color:#343a40;"
    ),
    label
  )
}


# ── Per-method tab group (Summary / Tuning / Recipe / Model Output) ───────────
#
# Arguments
#   ns         — namespace function from the enclosing module UI
#   m          — method name string, e.g. "glmnet"
#   has_tuning — TRUE to include the Tuning tab (defaults to m != "null")

.meth_subtabs_ui <- function(ns, m, has_tuning = (m != "null"),
                             tuning_controls = NULL) {

  tabs <- list(
    tabPanel(
      title = tagList(icon("list"), " Summary"),
      style = "padding-top:14px;",
      verbatimTextOutput(ns(paste0(m, "_desc"))),
      uiOutput(ns(paste0(m, "_na_hint"))),
      uiOutput(ns(paste0(m, "_maxit_hint"))),
      hr(),
      tags$h6("Resampled performance",
              style = "font-weight:700; color:#343a40; margin-bottom:8px;"),
      tableOutput(ns(paste0(m, "_metrics")))
    )
  )

  if (has_tuning) {
    tabs <- c(tabs, list(
      tabPanel(
        title = tagList(icon("chart-line"), " Tuning"),
        style = "padding-top:14px;",
        tuning_controls,
        plotOutput(ns(paste0(m, "_tune_plot")), width = "100%", height = "640px")
      )
    ))
  }

  tabs <- c(tabs, list(
    tabPanel(
      title = tagList(icon("utensils"), " Recipe"),
      style = "padding-top:14px;",
      uiOutput(ns(paste0(m, "_recipe_html"))),
      hr(),
      tableOutput(ns(paste0(m, "_recipe_table")))
    ),
    tabPanel(
      title = tagList(icon("terminal"), " Model Output"),
      style = "padding-top:14px;",
      fluidRow(
        column(6,
               tags$h6("Training summary",
                       style = "font-weight:700; color:#343a40; margin-bottom:8px;"),
               verbatimTextOutput(ns(paste0(m, "_train_summary")))
        ),
        column(6,
               tags$h6("Coefficients",
                       style = "font-weight:700; color:#343a40; margin-bottom:8px;"),
               tableOutput(ns(paste0(m, "_coef")))
        )
      )
    )
  ))

  do.call(tabsetPanel, c(list(type = "tabs"), tabs))
}


# ── Shared sidebar (Training Setup + Preprocessing + optional Model Specific) ─
#
# Arguments
#   ns                  — namespace function from the enclosing module UI
#   model_seed          — numeric: initial seed value shown in the seed box
#   pp_choices          — named character vector of preprocessing choices
#   default_preprocess  — character vector: initially selected preprocessing steps
#   specific_panels     — optional tagList injected inside "Model Specific Configs"
#                         section (shown only when config_mode == "specific").
#                         Pass NULL to omit the section entirely.

.meth_sidebar_ui <- function(ns,
                             model_seed         = 2026,
                             pp_choices         = character(0),
                             default_preprocess = character(0),
                             specific_panels    = NULL) {
  div(
    style = "background-color:#e8f0fe; border-left:2px solid #a8c0fd;
             min-height:100vh; padding:15px; font-size:13px;",

    # ── Info box ──────────────────────────────────────────────────────────────
    div(
      style = "background:white; padding:10px; border-left:4px solid #0d6efd;
               border-radius:6px; margin-bottom:12px; font-size:13px; color:#343a40;
               box-shadow:0 1px 2px rgba(0,0,0,0.05);",
      icon("circle-info", style = "color:#0d6efd;"),
      HTML("&nbsp;<b>Training controls:</b><br><br>
            Choose preprocessing, tune settings, then train or load the selected model.")
    ),

    # ── Action buttons ────────────────────────────────────────────────────────
    actionButton(ns("train"), "Train", icon = icon("play"),        width = "100%",
                 style = "background-color:#185FA5; color:white; border:none; margin-bottom:6px;"),
    actionButton(ns("load"),  "Load",  icon = icon("folder-open"), width = "100%",
                 style = "margin-bottom:6px;"),
    actionButton(ns("delete"),"Delete",icon = icon("trash"),       width = "100%",
                 style = "background-color:#dc3545; color:white; border:none; margin-bottom:6px;"),
    uiOutput(ns("action_feedback")),

    hr(),

    # ── Training Setup ────────────────────────────────────────────────────────
    .section_hdr("Training Setup", "#e9ecef", "#6c757d"),

    tags$label("Train/resampling seed:",
               style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
    numericInput(ns("train_seed"), NULL,
                 value = model_seed %||% 1, min = 0, step = 1, width = "100%"),
    checkboxInput(ns("follow_global_seed"), "Follow global seed", value = FALSE),
    conditionalPanel(
      condition = sprintf("input['%s']", ns("follow_global_seed")),
      uiOutput(ns("global_seed_display"))
    ),

    hr(),

    checkboxInput(ns("parallel"), "Use parallel processing", value = !isTRUE(IS_HOSTED)),

    tags$label("Resampling method:",
               style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
    selectInput(ns("resample_method"), NULL,
                choices  = c("Bootstrap"                 = "boot",
                             "Cross-validation"          = "cv",
                             "Repeated cross-validation" = "repeatedcv"),
                selected = "repeatedcv", width = "100%"),

    conditionalPanel(
      condition = sprintf("input['%s'] === 'boot'", ns("resample_method")),
      tags$label("Bootstrap resamples:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("boot_n"), NULL, min = 5, max = 50, value = 25, step = 5, width = "100%")
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] === 'cv'", ns("resample_method")),
      tags$label("Number of folds:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("cv_folds"), NULL, min = 3, max = 20, value = 10, step = 1, width = "100%")
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] === 'repeatedcv'", ns("resample_method")),
      tags$label("Number of folds:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("rcv_folds"),   NULL, min = 2, max = 10, value = 4, step = 1, width = "100%"),
      tags$label("Repeats:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("rcv_repeats"), NULL, min = 1, max = 10, value = 6, step = 1, width = "100%")
    ),

    tags$label("Search type:",
               style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
    selectInput(ns("search_type"), NULL,
                choices  = c("Grid search"   = "grid",
                             "Random search" = "random"),
                selected = "grid", width = "100%"),

    tags$label("Selection function:",
               style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
    selectInput(ns("selection_fn"), NULL,
                choices  = c("Best (lowest RMSE)"              = "best",
                             "One SE (simplest within 1 SE)"   = "oneSE",
                             "Tolerance (simplest within X %)" = "tolerance"),
                selected = "best", width = "100%"),

    conditionalPanel(
      condition = sprintf("input['%s'] === 'tolerance'", ns("selection_fn")),
      tags$label("Tolerance (%):",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      numericInput(ns("tol_pct"), NULL, value = 2, min = 0.1, max = 20, step = 0.1, width = "100%")
    ),

    hr(),

    # ── Config Mode ───────────────────────────────────────────────────────────
    .section_hdr("Config Mode", "#fff3cd", "#ffc107"),

    radioButtons(ns("config_mode"), NULL,
                 choices  = c("General configs"        = "general",
                              "Model specific configs" = "specific"),
                 selected = "general"),

    hr(),

    # ── Preprocessing ─────────────────────────────────────────────────────────
    .section_hdr("Preprocessing", "#d1ecf1", "#17a2b8"),

    tags$label("Pre-processing:",
               style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
    selectizeInput(ns("preprocess"), NULL,
                   choices  = pp_choices,
                   selected = default_preprocess,
                   multiple = TRUE,
                   options  = list(plugins = list("remove_button"),
                                   placeholder = "Select preprocessing steps..."),
                   width = "100%"),

    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('impute_bag')", ns("preprocess")),
      tags$label("Bagged imputation trees:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("impute_bag_trees"), NULL, min = 2, max = 50, value = 4, step = 1, width = "100%")
    ),

    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('impute_knn')", ns("preprocess")),
      tags$label("KNN imputation neighbours (k):",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("impute_knn_neighbors"), NULL, min = 1, max = 20, value = 5, step = 1, width = "100%")
    ),

    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('nzv')", ns("preprocess")),
      tags$label("NZV frequency cut:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      numericInput(ns("nzv_freq_cut"), NULL, value = 19, min = 1, step = 1, width = "100%"),
      tags$label("NZV unique cut:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      numericInput(ns("nzv_unique_cut"), NULL, value = 10, min = 1, step = 1, width = "100%")
    ),

    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('other')", ns("preprocess")),
      tags$label("Rare level threshold:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("other_threshold"), NULL,
                  min = 0.01, max = 0.2, value = 0.05, step = 0.01, width = "100%")
    ),

    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('interact')", ns("preprocess")),
      tags$label("Interaction terms:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      selectInput(ns("interact_type"), NULL,
                  choices  = c("All encoded predictors" = "all",
                               "Numeric × Numeric"      = "numeric_numeric",
                               "Nominal × Numeric"      = "nominal_numeric",
                               "Nominal × Nominal"      = "nominal_nominal"),
                  selected = "all", width = "100%")
    ),

    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('pca')", ns("preprocess")),
      tags$label("PCA components:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("pca_num_comp"), NULL, min = 1, max = 100, value = 25, step = 1, width = "100%")
    ),

    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('pls')", ns("preprocess")),
      tags$label("PLS components:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("pls_num_comp"), NULL, min = 1, max = 100, value = 25, step = 1, width = "100%")
    ),

    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('ica')", ns("preprocess")),
      tags$label("ICA components:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("ica_num_comp"), NULL, min = 1, max = 100, value = 25, step = 1, width = "100%")
    ),

    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('poly')", ns("preprocess")),
      tags$label("Polynomial degree:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("poly_degree"), NULL, min = 2, max = 10, value = 2, step = 1, width = "100%")
    ),

    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('corr')", ns("preprocess")),
      tags$label("Correlation threshold:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("corr_threshold"), NULL, min = 0.1, max = 0.99, value = 0.9, step = 0.01, width = "100%")
    ),

    # ── Cyclic encoding toggles (appear when the matching date step is selected) ──
    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('quarter')", ns("preprocess")),
      checkboxInput(ns("cyclic_quarter"), "Cyclic quarter (sin/cos, period = 4)", value = FALSE)
    ),
    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('month')", ns("preprocess")),
      checkboxInput(ns("cyclic_month"), "Cyclic month (sin/cos, period = 12)", value = FALSE)
    ),
    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('week')", ns("preprocess")),
      checkboxInput(ns("cyclic_week"),  "Cyclic week (sin/cos, period = 52)",  value = FALSE)
    ),
    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('dow')", ns("preprocess")),
      checkboxInput(ns("cyclic_dow"),   "Cyclic dow (sin/cos, period = 7)",    value = FALSE)
    ),

    tags$label("Tune length:",
               style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
    sliderInput(ns("tune_length"), NULL, min = 1, max = 50, value = 5, step = 1, width = "100%"),

    hr(),

    # ── Model Specific Configs (conditional, injected per category) ───────────
    if (!is.null(specific_panels)) {
      conditionalPanel(
        condition = sprintf("input['%s'] === 'specific'", ns("config_mode")),
        .section_hdr("Model Specific Configs", "#f8d7da", "#dc3545"),
        specific_panels
      )
    }
  )
}


# =================================================================================
# RENDER HELPERS  (pure functions, no reactivity)
# =================================================================================

.recipe_html_output <- function(ns_id, mod) {
  req(mod)
  html <- mod$recipe |>
    print() |>
    cli::cli_fmt() |>
    cli::ansi_collapse(sep = "<br>", last = "<br>") |>
    cli::ansi_html(escape_reserved = FALSE) |>
    gsub(pattern = "──────", replacement = "─", x = _, fixed = TRUE)
  css <- paste(format(cli::ansi_html_style()), collapse = "\n")
  tagList(tags$head(tags$style(css)), tags$pre(HTML(html)))
}

.recipe_term_table <- function(mod) {
  req(mod)
  terms <- as.data.frame(mod$recipe$term_info)
  n     <- nrow(terms)
  types <- character(n)
  for (i in seq_len(n)) types[i] <- paste(unlist(terms$type[i]), collapse = " ")
  terms$type <- types
  terms |>
    dplyr::filter(role == "predictor") |>
    dplyr::select(type, source) |>
    dplyr::group_by(type, source) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop")
}


# =================================================================================
# SERVER HELPERS  (called inside moduleServer)
# =================================================================================

# ── Extract preprocessing hyperparams from input ──────────────────────────────
#
# Call inside a reactive() or observeEvent() that has access to `input`.
# Returns a plain list — pass as `cfg` to .meth_build_recipe / dynamicSteps.

.meth_get_cfg <- function(input) {
  list(
    impute_bag_trees     = input$impute_bag_trees,
    impute_knn_neighbors = input$impute_knn_neighbors,
    nzv_freq_cut         = input$nzv_freq_cut,
    nzv_unique_cut       = input$nzv_unique_cut,
    other_threshold      = input$other_threshold,
    interact_type        = input$interact_type,
    pca_num_comp         = input$pca_num_comp,
    pls_num_comp         = input$pls_num_comp,
    ica_num_comp         = input$ica_num_comp,
    poly_degree          = input$poly_degree,
    corr_threshold       = input$corr_threshold,
    cyclic_quarter       = input$cyclic_quarter,
    cyclic_month         = input$cyclic_month,
    cyclic_week          = input$cyclic_week,
    cyclic_dow           = input$cyclic_dow
  )
}


# ── Build a recipes pipeline ──────────────────────────────────────────────────
#
# Arguments
#   train_df         — the training data frame
#   preprocess_steps — character vector of step names (from input$preprocess)
#   cfg              — list from .meth_get_cfg(input)
#   roles            — named character vector: variable → role
#   is_null          — TRUE for the null model (skip all preprocessing steps)

.meth_build_recipe <- function(train_df, preprocess_steps, cfg, roles, is_null = FALSE) {
  outcome_col <- names(roles)[roles == "outcome"][1]
  form        <- as.formula(paste0("`", outcome_col, "` ~ ."))
  rec         <- recipes::recipe(form, data = train_df)

  if (is_null) return(rec)

  # Remove columns that are not predictors before preprocessing
  drop_roles <- c("split", "obs_id", "ignore", "sensitive", "stratifier", "weight")
  drop_cols  <- intersect(names(roles)[roles %in% drop_roles], names(train_df))
  if (length(drop_cols) > 0)
    rec <- recipes::step_rm(rec, tidyselect::any_of(drop_cols))

  rec <- dynamicSteps(rec, preprocess_steps, cfg = cfg)

  # Strip any remaining date columns the user did not convert via a date step
  rec <- recipes::step_rm(rec, recipes::has_type("date"))
  rec
}


# ── Build a trainControl object ───────────────────────────────────────────────
#
# Arguments
#   input   — Shiny input object (reads resample_method, boot_n, cv_folds, etc.)
#   eseed   — effective seed value (numeric, already resolved from follow_global_seed)
#   train_y — outcome vector for the training rows (used to build resampling indices)

.meth_build_tr_control <- function(input, eseed, train_y) {
  method <- input$resample_method %||% "boot"
  search <- input$search_type     %||% "grid"

  # Matches template exactly:
  #   1. set.seed(eseed)
  #   2. Generate a seeds list (this consumes RNG state before createResample,
  #      exactly as the template's getTrControl() does with runif burn-in)
  #   3. createResample / createFolds AFTER the seeds list is built
  #   4. Pass both index = idx and seeds = seeds to trainControl so that
  #      per-resample RNG is controlled identically across parallel workers.
  set.seed(eseed)

  # ╔══════════════════════════════════════════════════════════════════════════════╗
  # ║  !! DO NOT MODIFY THIS SEEDING BLOCK WITHOUT UNDERSTANDING THE CONSEQUENCES ║
  # ║                                                                              ║
  # ║  BURN-IN: runif(n = 55, ...) is called once per resample BEFORE             ║
  # ║  createFolds / createMultiFolds / createResample.                            ║
  # ║  This exact RNG consumption (55 × n_total + 1 draws) was calibrated to      ║
  # ║  match the assignment template's resampling infrastructure.                  ║
  # ║                                                                              ║
  # ║  Verified template matches (Bootstrap, seed = 673):                         ║
  # ║    null   — RMSE 932.732  MAE 730.013                                       ║
  # ║    glmnet — confirmed ✓                                                     ║
  # ║    pls    — confirmed ✓                                                     ║
  # ║    rpart  — cp 0.026  RMSE 752.397  MAE 579.971  RMSESD 32.570  ✓          ║
  # ║                                                                              ║
  # ║  Changing n=55, the order of runif vs createFolds, or the seed call         ║
  # ║  WILL shift fold/resample assignments and break all template comparisons.   ║
  # ║                                                                              ║
  # ║  .pad_seeds() extends each 55-element vector to 500 elements AFTER fold     ║
  # ║  creation so large custom tuning grids (>55 combos) don't hit caret's       ║
  # ║  "bad seeds" error. The padding is deterministic and does NOT consume RNG.  ║
  # ╚══════════════════════════════════════════════════════════════════════════════╝
  .pad_seeds <- function(seeds, n_total) {
    for (i in seq_len(n_total)) {
      sv  <- seeds[[i]]
      gap <- 500L - length(sv)
      if (gap > 0L)
        seeds[[i]] <- c(sv, as.integer((as.numeric(sv[[1L]]) * seq_len(gap) * 7919) %% 4000 + 1000))
    }
    seeds
  }

  if (method == "cv") {
    n       <- input$cv_folds %||% 10
    reps    <- NA
    n_total <- n
    seeds   <- vector(mode = "list", length = n_total + 1L)
    for (i in seq_len(n_total))
      seeds[[i]] <- as.integer(runif(n = 55, min = 1000, max = 5000))  # !! DO NOT CHANGE n=55
    seeds[[n_total + 1L]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    idx   <- caret::createFolds(y = train_y, k = n, returnTrain = TRUE)
    seeds <- .pad_seeds(seeds, n_total)

  } else if (method == "repeatedcv") {
    n       <- input$rcv_folds   %||% 4
    reps    <- input$rcv_repeats %||% 6
    n_total <- n * reps
    seeds   <- vector(mode = "list", length = n_total + 1L)
    for (i in seq_len(n_total))
      seeds[[i]] <- as.integer(runif(n = 55, min = 1000, max = 5000))  # !! DO NOT CHANGE n=55
    seeds[[n_total + 1L]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    idx   <- caret::createMultiFolds(y = train_y, k = n, times = reps)
    seeds <- .pad_seeds(seeds, n_total)

  } else {
    method  <- "boot"
    n       <- input$boot_n %||% 25
    reps    <- NA
    seeds   <- vector(mode = "list", length = n + 1L)
    for (i in seq_len(n))
      seeds[[i]] <- as.integer(runif(n = 55, min = 1000, max = 5000))  # !! DO NOT CHANGE n=55
    seeds[[n + 1L]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    idx   <- caret::createResample(y = train_y, times = n)
    seeds <- .pad_seeds(seeds, n)
  }

  sel_fn <- switch(input$selection_fn %||% "best",
    "oneSE"     = "oneSE",         # pass as string — caret handles dispatch & edge cases
    "tolerance" = function(x, metric, maximize) {
                    tolerance(x, metric,
                              tol      = input$tol_pct %||% 2,
                              maximize = maximize)
                  },
    "best"   # default
  )

  trainControl(
    method            = method,
    number            = n,
    repeats           = reps,
    allowParallel     = isTRUE(input$parallel) && !isTRUE(IS_HOSTED),
    search            = search,
    index             = idx,
    savePredictions   = "final",
    seeds             = seeds,
    trim              = TRUE,
    selectionFunction = sel_fn
  )
}


# ── Response-normalised training for nnet-family models ──────────────────────
#
# nnet (mlpWeightDecay, pcaNNet) and RSNNS (mlpML) are all sensitive to the
# scale of the response variable. When the response is in the thousands the
# optimizer diverges and predictions collapse to near-zero (R² ≈ 0).
# This helper standardises y to mean=0, sd=1 before training and back-scales
# RMSE/MAE metrics to the original scale afterwards.
#
# Arguments
#   rec         — unprepped recipe object from .meth_build_recipe()
#   train_df    — training data frame
#   outcome_col — name of the outcome column
#   method      — caret method string, e.g. "mlpWeightDecay"
#   tr_ctrl     — trainControl object from .meth_build_tr_control()
#   tune_length — integer tuneLength passed to caret::train()
#   ...         — additional args forwarded to caret::train()

# Returns a human-readable resampling label from a trained caret model,
# used as the y-axis title in all tuning plots.
.resample_label <- function(mod) {
  method <- tryCatch(mod$control$method, error = function(e) "boot") %||% "boot"
  switch(method,
    "boot"       = "Bootstrap",
    "boot632"    = "Bootstrap (632)",
    "cv"         = "Cross-Validation",
    "repeatedcv" = "Repeated CV",
    "LOOCV"      = "LOOCV",
    "timeslice"  = "Time Slice",
    method)
}

.meth_nnet_normalised_train <- function(rec, train_df, outcome_col, method,
                                         tr_ctrl, tune_length, ...) {
  # Prep and bake the recipe
  prep_rec <- recipes::prep(rec, training = train_df, retain = TRUE)
  baked    <- recipes::bake(prep_rec, new_data = NULL)

  # Separate predictors and outcome; keep only numeric predictors
  y <- baked[[outcome_col]]
  x <- baked[, setdiff(names(baked), outcome_col), drop = FALSE]
  x <- x[, vapply(x, is.numeric, logical(1)), drop = FALSE]

  # Remove any remaining NA rows
  ok <- complete.cases(x, y)
  x  <- as.matrix(x[ok, , drop = FALSE])
  y  <- y[ok]

  if (nrow(x) <= 5 || ncol(x) == 0)
    stop("Too few complete cases after preprocessing.")

  # Standardise response to mean = 0, sd = 1
  y_center <- mean(y, na.rm = TRUE)
  y_scale  <- stats::sd(y, na.rm = TRUE)
  if (!is.finite(y_center) || !is.finite(y_scale) || y_scale <= 0)
    stop("Response normalisation failed: check for constant or all-NA outcome.")
  y_train <- as.numeric((y - y_center) / y_scale)

  # Train on normalised response (x/y interface — no na.action needed)
  model <- caret::train(x = x, y = y_train, method = method,
                        metric = "RMSE", trControl = tr_ctrl,
                        tuneLength = tune_length, ...)

  # Back-scale RMSE/MAE metrics to the original response scale
  # (Rsquared is scale-invariant and does not need back-scaling)
  for (m in c("RMSE", "MAE", "RMSESD", "MAESD")) {
    if (m %in% names(model$results))
      model$results[[m]] <- model$results[[m]] * y_scale
    if (!is.null(model$resample) && m %in% names(model$resample))
      model$resample[[m]] <- model$resample[[m]] * y_scale
  }

  # Store normalisation params and prepped recipe for downstream use
  model$outcomeCenter <- y_center
  model$outcomeScale  <- y_scale
  model$recipe        <- prep_rec

  model
}


# ── Common server setup (seed / global_seed_display / get_train / get_test) ───
#
# Call once at the top of every category module's moduleServer.
# Returns a list:
#   $effective_seed — reactive: numeric seed to use for training
#   $get_train      — reactive: training data frame (rows where split == "Train")
#   $get_test       — reactive: test/val data frame  (rows where split == "Test")
#
# Arguments
#   input, output, session — standard Shiny objects
#   get_data   — reactive returning the full modelling data frame
#   roles      — reactive returning named role vector (variable → role)
#   seed       — reactive returning the global seed (from the data roles module)
#   model_seed — optional numeric override (e.g. MODEL_SEED = 673).
#                If NULL the seed box is initialised from seed() on startup,
#                so the fallback chain is: model_seed → data-roles seed → data-roles default.

.meth_common_server_setup <- function(input, output, session,
                                       get_data, roles, seed,
                                       model_seed = NULL) {

  # ── Initialise the seed numericInput once the session is ready ───────────
  # model_seed (explicit override) wins; otherwise use the data-roles seed.
  observe({
    init <- if (!is.null(model_seed)) model_seed else seed()
    req(init)
    updateNumericInput(session, "train_seed", value = init)
  }) |> bindEvent(seed(), once = TRUE, ignoreInit = FALSE)

  effective_seed <- reactive({
    if (isTRUE(input$follow_global_seed)) seed() else input$train_seed
  })

  output$global_seed_display <- renderUI({
    s <- seed()
    div(
      style = "font-size:12px; color:#0f6e56; background:#e1f5ee;
               border-left:3px solid #9fe1cb; padding:5px 10px;
               border-radius:4px; margin-top:2px; margin-bottom:4px;",
      icon("circle-check", style = "color:#0f6e56;"),
      HTML(paste0(" Using global seed: <b>", s, "</b>"))
    )
  })

  get_train <- reactive({
    df <- get_data(); req(df)
    r  <- roles();   req(r)
    sc <- names(r)[r == "split"]
    if (length(sc) == 0) return(df)
    df[as.character(df[[sc[1]]]) %in% c("Train", "train"), , drop = FALSE]
  })

  get_test <- reactive({
    df <- get_data(); req(df)
    r  <- roles();   req(r)
    sc <- names(r)[r == "split"]
    if (length(sc) == 0) return(NULL)
    df[as.character(df[[sc[1]]]) %in% c("Test", "test", "Validation", "val"), , drop = FALSE]
  })

  list(effective_seed = effective_seed, get_train = get_train, get_test = get_test)
}


# ── Unified Train / Load / Delete dispatcher ──────────────────────────────────
#
# Call once per category module.  Wires input$train / input$load / input$delete
# to the appropriate action for whatever method is currently active.
#
# Arguments
#   input, output, session — standard Shiny objects
#   models          — reactiveValues() keyed by method name
#   current_method  — reactive returning the active method name string
#   train_fns       — named list of zero-argument closures, one per method.
#                     Each closure captures its own data/config from the outer
#                     scope and, when called, runs caret::train() + returns the
#                     trained model (or throws on error).

.meth_action_dispatcher <- function(input, output, session,
                                    models, current_method, train_fns,
                                    roles = NULL) {

  # ── Train ─────────────────────────────────────────────────────────────────
  observeEvent(input$train, {
    method   <- current_method(); req(!is.null(method))
    train_fn <- train_fns[[method]]; req(!is.null(train_fn))

    # ── Guard: outcome role must be assigned ──────────────────────────────
    if (!is.null(roles)) {
      oc <- tryCatch(names(roles())[roles() == "outcome"], error = function(e) character(0))
      if (length(oc) == 0) {
        showNotification(
          ui       = tagList(icon("triangle-exclamation"),
                             HTML("&nbsp;<b>No outcome role assigned.</b><br>
                                  Go to <b>Config → Data Roles</b> and set an outcome column,
                                  or update the task-specific roles in the config file.")),
          type     = "error",
          duration = 10,
          session  = session
        )
        return()
      }
    }

    models[[method]] <- NULL
    output$action_feedback <- renderUI(NULL)
    showNotification(
      id       = method,
      ui       = tagList(
        tags$span(
          style = "display:flex; align-items:center; gap:10px; font-size:15px; font-weight:700; color:#dc3545;",
          icon("circle-notch", class = "fa-spin", style = "font-size:18px; color:#dc3545;"),
          tags$span(paste0("Training ", method, " — please wait…"))
        )
      ),
      type     = "warning",
      duration = NULL,
      session  = session
    )

    obj <- startMode(isTRUE(input$parallel) && !isTRUE(IS_HOSTED))
    t0  <- proc.time()

    tryCatch({
      model   <- train_fn()
      t_train <- round((proc.time() - t0)["elapsed"], 2)

      # Attach timing BEFORE saving so it is preserved in the RDS
      model$.wall_train <- t_train
      model$.wall_total <- t_train
      deleteRds(method, session)
      saveToRds(model, method, session)
      t_total <- round((proc.time() - t0)["elapsed"], 2)

      # Update session copy with accurate total (train + save)
      model$.wall_total <- t_total
      models[[method]]  <- model

      meth_snap   <- method
      train_snap  <- t_train
      total_snap  <- t_total
      output$action_feedback <- renderUI({
        div(
          style = "margin-top:8px; font-size:12px; color:#0f6e56; background:#e1f5ee;
                   border-left:3px solid #9fe1cb; padding:6px 10px; border-radius:6px;",
          icon("circle-check", style = "color:#0f6e56;"),
          HTML(paste0(" <b>", meth_snap, "</b> trained — ",
                      train_snap, "s fit | ", total_snap, "s total (incl. save)"))
        )
      })

    }, error = function(e) {
      msg    <- conditionMessage(e)
      is_na  <- grepl("\\bNA\\b|missing value|na\\.fail|na\\.action|NAs present", msg, ignore.case = TRUE)
      hint   <- if (is_na)
        "<br><br><b>Hint:</b> Your data may contain NAs that are not handled in the
         preprocessing pipeline. Add <code>naomit</code>, <code>impute_bag</code>, or
         <code>impute_knn</code> to your preprocessing steps and retrain."
      else ""
      output$action_feedback <- renderUI({
        div(
          style = "margin-top:8px; font-size:12px; color:#dc3545; background:#fff5f5;
                   border-left:3px solid #f5c2c7; padding:6px 10px; border-radius:6px;",
          icon("circle-xmark", style = "color:#dc3545;"),
          HTML(paste0(" <b>Error:</b> ", msg, hint))
        )
      })
    }, finally = {
      removeNotification(id = method)
      stopMode(obj)
    })
  })

  # ── Load ──────────────────────────────────────────────────────────────────
  observeEvent(input$load, {
    method <- current_method(); req(!is.null(method))
    model  <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
      meth_snap <- method
      output$action_feedback <- renderUI({
        div(
          style = "margin-top:8px; font-size:12px; color:#185FA5; background:#e8f0fe;
                   border-left:3px solid #a8c0fd; padding:6px 10px; border-radius:6px;",
          icon("folder-open", style = "color:#185FA5;"),
          HTML(paste0(" <b>", meth_snap, "</b> loaded from file."))
        )
      })
    }
  })

  # ── Delete ────────────────────────────────────────────────────────────────
  observeEvent(input$delete, {
    method <- current_method(); req(!is.null(method))
    models[[method]] <- NULL
    gc()
    meth_snap <- method
    output$action_feedback <- renderUI({
      div(
        style = "margin-top:8px; font-size:12px; color:#6c757d; background:#f8f9fa;
                 border-left:3px solid #ced4da; padding:6px 10px; border-radius:6px;",
        icon("trash", style = "color:#6c757d;"),
        HTML(paste0(" <b>", meth_snap, "</b> cleared from session."))
      )
    })
  })
}


# ── Register all standard output IDs for one method ──────────────────────────
#
# Call once per method inside a category module's moduleServer, after
# `models` reactiveValues and `ns` are available.
#
# Arguments
#   output  — Shiny output object
#   m       — method name string, e.g. "glmnet"
#   models  — reactiveValues() storing trained caret models keyed by method name
#   ns      — namespace function from session$ns

# ── Per-method NA action hints ────────────────────────────────────────────────
.na_action_info <- list(
  # na.pass — recipe is responsible
  null           = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  lm             = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  rlm            = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  glmnet         = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  pls            = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  svmLinear      = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  svmRadialSigma = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  svmRadial      = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  svmPoly        = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  krlsRadial     = list(action = "omitted",     severity = "warning",
                        note = "KRLS::krls() does not accept a na.action argument. Ensure NAs are handled in your preprocessing pipeline (naomit or imputation)."),
  krlsPoly       = list(action = "omitted",     severity = "warning",
                        note = "KRLS::krls() does not accept a na.action argument. Ensure NAs are handled in your preprocessing pipeline (naomit or imputation)."),
  gaussprRadial  = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  gaussprPoly    = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  gaussprLinear  = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  evtree         = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  cubist         = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  ppr            = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  M5             = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  qrnn           = list(action = "omitted",     severity = "warning",
                        note = "qrnn::qrnn.fit() passes data directly to nlm() which cannot handle NAs. Ensure NAs are handled in your preprocessing pipeline (naomit or imputation)."),
  brnn           = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  pcaNNet        = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  mlpWeightDecay = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  mlpML          = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  avNNet         = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  monmlp         = list(action = "omitted",     severity = "warning",
                        note = "monmlp::monmlp.fit() does not accept a na.action argument. Ensure NAs are handled in your preprocessing pipeline (naomit or imputation)."),
  # na.rpart — model handles NAs natively
  rpart          = list(action = "na.rpart",    severity = "success",
                        note = "rpart handles NAs natively via surrogate splits — no preprocessing required. However, explicit imputation is still recommended for consistency across models."),
  # na.roughfix — model imputes NAs
  rf             = list(action = "na.roughfix", severity = "success",
                        note = "randomForest applies rough-fix imputation (median/mode) to NAs automatically. Explicit imputation in preprocessing is still recommended for consistency."),
  # na.fail — NAs will cause an error
  earth          = list(action = "na.fail",     severity = "danger",
                        note = "earth requires zero NAs in the data. Add naomit or an imputation step to your preprocessing — earth will error on every resample if any NAs remain."),
  gcvEarth       = list(action = "na.fail",     severity = "danger",
                        note = "gcvEarth uses the earth package internally and shares the same restriction — zero NAs allowed. Add naomit or an imputation step to your preprocessing."),
  bagEarth       = list(action = "na.fail",     severity = "danger",
                        note = "bagEarth wraps earth models and shares the same restriction — zero NAs allowed. Add naomit or an imputation step to your preprocessing."),
  gam            = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  gamSpline      = list(action = "na.pass",     severity = "info",
                        note = "NAs must be handled in your preprocessing pipeline (naomit or imputation). The recipe processes data before the model sees it."),
  gamboost       = list(action = "na.omit",     severity = "warning",
                        note = "mboost cannot handle NAs internally — rows with missing values are dropped before fitting. Handle NAs in the recipe (naomit or imputation) to avoid losing data."),
  # omitted — ranger handles internally
  ranger         = list(action = "omitted",     severity = "warning",
                        note = "ranger handles NA action internally via string comparison and does not accept a function argument. Ensure NAs are handled in your preprocessing pipeline.")
)

# ── Per-method response-normalisation notes (nnet family) ────────────────────
# mlpWeightDecay, pcaNNet (nnet::nnet()) and mlpML (RSNNS::mlp()) are all
# sensitive to the scale of the response. When the response is in the thousands
# the optimizer diverges and predictions collapse to near-zero (R² ≈ 0).
# .meth_nnet_normalised_train() standardises y before training and back-scales
# metrics. This list drives the info box in each model's Summary tab.
.maxit_info <- list(
  mlpWeightDecay = paste0(
    "nnet's optimizer struggles when the response is on a large scale — gradients become enormous, ",
    "causing weight divergence and collapse to near-zero predictions (R² ≈ 0, RMSE far worse than null). ",
    "This app standardises the response to mean = 0, sd = 1 before training and back-transforms ",
    "RMSE/MAE metrics to the original scale. outcomeCenter and outcomeScale are stored in the model."
  ),
  pcaNNet = paste0(
    "pcaNNet's internal PCA normalises inputs to unit scale, making the mismatch with a large-scale ",
    "response even more severe — predictions collapse to ≈ 0 for all observations. ",
    "This app standardises the response to mean = 0, sd = 1 before training and back-transforms ",
    "RMSE/MAE metrics to the original scale. outcomeCenter and outcomeScale are stored in the model."
  ),
  mlpML = paste0(
    "RSNNS::mlp() shares the same sensitivity to response scale as nnet — when the response is in the ",
    "thousands the optimizer diverges and predictions collapse to near-zero. ",
    "This app standardises the response to mean = 0, sd = 1 before training and back-transforms ",
    "RMSE/MAE metrics to the original scale. outcomeCenter and outcomeScale are stored in the model."
  ),
  avNNet = paste0(
    "avNNet averages multiple nnet models and shares the same response-scale sensitivity — ",
    "when the response is in the thousands the optimizer diverges across all ensemble members. ",
    "This app standardises the response to mean = 0, sd = 1 before training and back-transforms ",
    "RMSE/MAE metrics to the original scale. outcomeCenter and outcomeScale are stored in the model."
  )
)

.meth_register_outputs <- function(output, m, models, ns) {
  local({
    meth <- m

    # Method description
    output[[paste0(meth, "_desc")]] <- renderText({
      tryCatch(description(meth), error = function(e) paste("Method:", meth))
    })

    # NA action hint box
    output[[paste0(meth, "_na_hint")]] <- renderUI({
      info <- .na_action_info[[meth]]
      if (is.null(info)) return(NULL)
      cfg <- switch(info$severity,
        "danger"  = list(bg = "#fff5f5", border = "#dc3545", icon_col = "#dc3545",
                         icon = "circle-xmark",    label = "NA handling: REQUIRED in preprocessing"),
        "warning" = list(bg = "#fff9e6", border = "#ffc107", icon_col = "#856404",
                         icon = "triangle-exclamation", label = "NA handling: handle in preprocessing"),
        "success" = list(bg = "#f0fff4", border = "#198754", icon_col = "#198754",
                         icon = "circle-check",    label = "NA handling: model tolerates NAs"),
        # info (default)
                    list(bg = "#e8f0fe", border = "#0d6efd", icon_col = "#0d6efd",
                         icon = "circle-info",     label = "NA handling: recipe is responsible")
      )
      div(
        style = paste0(
          "background:", cfg$bg, "; border-left:4px solid ", cfg$border, ";",
          "border-radius:6px; padding:8px 12px; margin:8px 0 4px;",
          "font-size:12px; color:#343a40;"
        ),
        icon(cfg$icon, style = paste0("color:", cfg$icon_col, ";")),
        HTML(paste0(
          " <b>", cfg$label, "</b> &nbsp;|&nbsp; <code>na.action = ", info$action, "</code>",
          "<br><span style='color:#6c757d;'>", info$note, "</span>"
        ))
      )
    })

    # Response normalisation hint (mlpWeightDecay / pcaNNet / mlpML)
    output[[paste0(meth, "_maxit_hint")]] <- renderUI({
      note <- .maxit_info[[meth]]
      if (is.null(note)) return(NULL)
      div(
        style = paste0(
          "background:#e8f0fe; border-left:4px solid #0d6efd;",
          "border-radius:6px; padding:8px 12px; margin:4px 0 4px;",
          "font-size:12px; color:#343a40;"
        ),
        icon("circle-info", style = "color:#0d6efd;"),
        HTML(paste0(
          " <b>Response normalisation</b> &nbsp;|&nbsp;",
          " <code>y → (y − μ) / σ</code>",
          "<br><span style='color:#6c757d;'>", note, "</span>"
        ))
      )
    })

    # Resampled metrics — best-RMSE row + three timing columns appended
    output[[paste0(meth, "_metrics")]] <- renderTable({
      mod  <- models[[meth]]; req(mod)
      best <- mod$results[which.min(mod$results[["RMSE"]]), ]

      # AvgFoldSecs: prefer caret's built-in, fall back to wall_train / n_resamples
      if ("TimedFitStepSeconds" %in% names(best)) {
        names(best)[names(best) == "TimedFitStepSeconds"] <- "AvgFoldSecs"
      } else {
        n_folds <- if (!is.null(mod$resample)) nrow(mod$resample) else NA
        if (!is.null(mod$.wall_train) && !is.na(n_folds) && n_folds > 0)
          best$AvgFoldSecs <- round(mod$.wall_train / n_folds, 3)
      }

      # Append custom timers (train-only, then train+save)
      if (!is.null(mod$.wall_train)) best$FinalFitSecs  <- mod$.wall_train
      if (!is.null(mod$.wall_total)) best$WallClockSecs <- mod$.wall_total

      # Enforce column order: metrics first, then FinalFitSecs → WallClockSecs → AvgFoldSecs
      time_cols <- intersect(c("FinalFitSecs", "WallClockSecs", "AvgFoldSecs"), names(best))
      non_time  <- setdiff(names(best), time_cols)
      best[, c(non_time, time_cols), drop = FALSE]
    }, digits = 3)

    # Tuning plot — null model gets a "no parameters" placeholder
    output[[paste0(meth, "_tune_plot")]] <- renderPlot({
      mod <- models[[meth]]; req(mod)
      if (meth == "null") {
        plot.new()
        text(0.5, 0.5,
             "The null model has no tuning parameters.",
             cex = 1.3, col = "#6c757d", adj = c(0.5, 0.5))
      } else {
        plot(mod)
      }
    })

    # Recipe HTML printout
    output[[paste0(meth, "_recipe_html")]] <- renderUI({
      mod <- models[[meth]]; req(mod)
      tryCatch(.recipe_html_output(ns, mod),
               error = function(e) tags$code(conditionMessage(e)))
    })

    # Recipe predictor-type summary table
    output[[paste0(meth, "_recipe_table")]] <- renderTable({
      mod <- models[[meth]]; req(mod)
      tryCatch(.recipe_term_table(mod), error = function(e) NULL)
    })

    # Full caret print() + wall-clock header
    output[[paste0(meth, "_train_summary")]] <- renderPrint({
      mod <- models[[meth]]; req(mod)
      if (!is.null(mod$.wall_total))
        cat("Total wall-clock time:", mod$.wall_total, "s\n",
            "(fit + save; TimedFitStepSeconds is per-resample only)\n\n")
      print(mod)
    })

    # Coefficients / variable importance (method-specific)
    output[[paste0(meth, "_coef")]] <- renderTable({
      mod <- models[[meth]]; req(mod)
      tryCatch({
        if (meth == "glmnet") {
          co <- as.matrix(coef(mod$finalModel, s = mod$bestTune$lambda))
          df <- data.frame(Coefficient = round(co[, 1], 6), row.names = rownames(co))
          df <- df[df$Coefficient != 0, , drop = FALSE]
          # Sort: (Intercept) pinned top, rest by |Coefficient| descending
          has_int  <- "(Intercept)" %in% rownames(df)
          int_row  <- if (has_int) df["(Intercept)", , drop = FALSE] else NULL
          rest     <- df[rownames(df) != "(Intercept)", , drop = FALSE]
          rest     <- rest[order(abs(rest$Coefficient), decreasing = TRUE), , drop = FALSE]
          if (!is.null(int_row)) rbind(int_row, rest) else rest

        } else if (meth == "pls") {
          co  <- coef(mod$finalModel)
          df  <- as.data.frame(co, row.names = rownames(co))
          # Sort all rows by |first coefficient column| descending
          df[order(abs(df[[1]]), decreasing = TRUE), , drop = FALSE]

        } else if (meth %in% c("lm", "rlm")) {
          co  <- coef(mod$finalModel)
          df  <- data.frame(Variable = names(co), Coefficient = round(co, 6),
                            stringsAsFactors = FALSE)
          # (Intercept) pinned top, rest by |Coefficient| descending
          int_idx  <- which(df$Variable == "(Intercept)")
          rest_idx <- setdiff(order(abs(df$Coefficient), decreasing = TRUE), int_idx)
          df[c(int_idx, rest_idx), , drop = FALSE]

        } else if (meth == "rpart") {
          vi <- mod$finalModel$variable.importance
          if (length(vi) == 0)
            return(data.frame(Note = "No splits — tree is a single root node."))
          data.frame(
            Variable   = names(vi),
            Importance = round(vi, 4)
          )

        } else if (meth == "evtree") {
          data.frame(Note = "EVTree has no linear coefficients. See Model Output tab for the full tree structure.")

        } else if (meth == "rf") {
          imp <- mod$finalModel$importance
          if (is.null(imp) || nrow(imp) == 0)
            return(data.frame(Note = "No variable importance available."))
          # randomForest importance: columns differ by type (regression → %IncMSE + IncNodePurity)
          imp_df <- as.data.frame(imp)
          imp_df <- imp_df[order(imp_df[[ncol(imp_df)]], decreasing = TRUE), , drop = FALSE]
          imp_df$Variable <- rownames(imp_df)
          rownames(imp_df) <- NULL
          imp_df[, c("Variable", names(imp_df)[names(imp_df) != "Variable"]), drop = FALSE]

        } else if (meth == "cubist") {
          usg <- mod$finalModel$usage
          if (is.null(usg) || nrow(usg) == 0)
            return(data.frame(Note = "No variable usage statistics available."))
          # Cubist usage: columns Variable, Conditions, Model (% appearances)
          usg[order(usg$Model + usg$Conditions, decreasing = TRUE), , drop = FALSE]

        } else if (meth == "svmLinear") {
          fm <- mod$finalModel
          data.frame(
            Metric = c("Support vectors", "Best C"),
            Value  = c(fm@nSV, signif(mod$bestTune$C, 4))
          )

        } else if (meth == "svmRadialSigma") {
          fm <- mod$finalModel
          data.frame(
            Metric = c("Support vectors", "Best C", "Best sigma"),
            Value  = c(fm@nSV, signif(mod$bestTune$C, 4), signif(mod$bestTune$sigma, 4))
          )

        } else if (meth == "svmRadial") {
          fm <- mod$finalModel
          data.frame(
            Metric = c("Support vectors", "Best C", "Estimated sigma"),
            Value  = c(fm@nSV,
                       signif(mod$bestTune$C, 4),
                       signif(mod$finalModel@kernelf@kpar$sigma, 4))
          )

        } else if (meth == "svmPoly") {
          fm <- mod$finalModel
          data.frame(
            Metric = c("Support vectors", "Best degree", "Best scale", "Best C"),
            Value  = c(fm@nSV,
                       as.character(mod$bestTune$degree),
                       signif(mod$bestTune$scale, 4),
                       signif(mod$bestTune$C, 4))
          )

        } else if (meth == "krlsPoly") {
          data.frame(
            Metric = c("Best degree", "Best lambda"),
            Value  = c(as.character(mod$bestTune$degree),
                       signif(mod$bestTune$lambda, 4))
          )

        } else if (meth == "krlsRadial") {
          data.frame(
            Metric = c("Best sigma", "Best lambda"),
            Value  = c(signif(mod$bestTune$sigma,  4),
                       signif(mod$bestTune$lambda, 4))
          )

        } else if (meth == "gaussprRadial") {
          data.frame(
            Metric = "Best sigma",
            Value  = signif(mod$bestTune$sigma, 4)
          )

        } else if (meth == "gaussprPoly") {
          data.frame(
            Metric = "Best degree",
            Value  = as.character(mod$bestTune$degree)
          )

        } else if (meth == "gaussprLinear") {
          data.frame(Note = "Gaussian Process Linear has no tuning parameters.")

        } else if (meth == "pcaNNet") {
          data.frame(
            Metric = c("Best size (hidden units)", "Best decay"),
            Value  = c(as.character(mod$bestTune$size),
                       signif(mod$bestTune$decay, 4))
          )

        } else if (meth == "mlpWeightDecay") {
          data.frame(
            Metric = c("Best size (hidden units)", "Best decay"),
            Value  = c(as.character(mod$bestTune$size),
                       signif(mod$bestTune$decay, 4))
          )

        } else if (meth == "mlpML") {
          data.frame(
            Metric = c("Best layer1", "Best layer2", "Best layer3"),
            Value  = c(as.character(mod$bestTune$layer1),
                       as.character(mod$bestTune$layer2),
                       as.character(mod$bestTune$layer3))
          )

        } else if (meth == "monmlp") {
          data.frame(
            Metric = c("Best hidden1", "Best n.ensemble"),
            Value  = c(as.character(mod$bestTune$hidden1),
                       as.character(mod$bestTune$n.ensemble))
          )

        } else if (meth == "ranger") {
          imp <- mod$finalModel$variable.importance
          if (is.null(imp) || length(imp) == 0)
            return(data.frame(Note = "No variable importance available (set importance = 'impurity' or 'permutation' in train)."))
          imp_df <- data.frame(
            Variable   = names(imp),
            Importance = round(imp, 4)
          )
          imp_df[order(imp_df$Importance, decreasing = TRUE), , drop = FALSE]

        } else if (meth == "qrnn") {
          data.frame(
            Metric = c("Best n.hidden", "Best penalty", "Best bag"),
            Value  = c(as.character(mod$bestTune$n.hidden),
                       signif(mod$bestTune$penalty, 4),
                       as.character(mod$bestTune$bag))
          )

        } else if (meth == "brnn") {
          data.frame(
            Metric = "Best neurons",
            Value  = as.character(mod$bestTune$neurons)
          )

        } else if (meth %in% c("earth", "gcvEarth", "bagEarth")) {
          co <- tryCatch(mod$finalModel$coefficients, error = function(e) NULL)
          if (is.null(co) || length(co) == 0)
            return(data.frame(Note = "No MARS coefficients available."))
          df  <- data.frame(Term = names(co), Coefficient = round(co, 6),
                            stringsAsFactors = FALSE)
          # (Intercept) pinned top, rest by |Coefficient| descending
          int_idx  <- which(df$Term == "(Intercept)")
          rest_idx <- setdiff(order(abs(df$Coefficient), decreasing = TRUE), int_idx)
          df[c(int_idx, rest_idx), , drop = FALSE]

        } else if (meth %in% c("gam", "gamboost")) {
          data.frame(Note = "GAM/gamboost has no simple linear coefficients. See Model Output tab for the fitted smooth terms.")

        } else if (meth == "gamSpline") {
          data.frame(
            Metric = "Best df",
            Value  = as.character(mod$bestTune$df)
          )

        } else if (meth == "M5") {
          data.frame(
            Metric = c("Best pruned", "Best smoothed", "Best rules"),
            Value  = c(as.character(mod$bestTune$pruned),
                       as.character(mod$bestTune$smoothed),
                       as.character(mod$bestTune$rules))
          )

        } else {
          data.frame(Note = "No coefficients available for this method.")
        }
      }, error = function(e) data.frame(Note = conditionMessage(e)))
    }, rownames = (meth %in% c("glmnet", "pls")), striped = TRUE, hover = TRUE, bordered = TRUE)
  })
}
