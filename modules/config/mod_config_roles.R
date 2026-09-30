# =================================================================================
# mod_config_roles.R
# =================================================================================
# Requires: library(caret) and library(cluster) in the main app

`%||%` <- function(a, b) if (!is.null(a)) a else b


# ── UI ───────────────────────────────────────────────────────────────────────

data_roles_ui <- function(id, default_seed = NULL, split_ratio = NULL) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 3,
      style = "background-color: #e8f0fe; border-left: 2px solid #a8c0fd; min-height: 100vh; padding-left: 20px;",

      div(
        style = "font-size: 13px; color: #343a40; background-color: white;
             padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
             margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; Tab Note: <br><br>
             Assign variable roles by dragging pills between buckets. <br><br>
             <b>Shield</b> icons mark domain-important variables — protected
             from automatic exclusion during missingness and feature selection steps.")
      ),
      hr(),

      # ── 1. Seed ──────────────────────────────────────────────────────────
      tags$label("Global Seed:", style = "font-weight:600; font-size:13px; color:#343a40;"),
      numericInput(ns("seed"), label = NULL,
                   value = default_seed %||% as.integer(format(Sys.Date(), "%Y")),
                   min = 0, step = 1, width = "100%"),
      helpText("Affects splitting and all stochastic steps."),
      hr(),

      # ── 2. Split method ───────────────────────────────────────────────────
      tags$label("Split Method:", style = "font-weight:600; font-size:13px; color:#343a40;"),
      selectInput(ns("split_method"), label = NULL, width = "100%",
                  choices = c(
                    "2-Partition (Train / Test)"       = "two_partition",
                    "3-Partition (Train / Val / Test)" = "three_partition",
                    "Stratified Bootstrap"             = "boot_stratified",
                    "Leave Group Out"                  = "leave_group_out",
                    "Time Series"                      = "time_series",
                    "Diversity Down-Sampling"          = "diversity"
                  ),
                  selected = "two_partition"),

      # ── 2a. 2-Partition ───────────────────────────────────────────────────
      conditionalPanel(
        condition = sprintf("input['%s'] === 'two_partition'", ns("split_method")),
        sliderInput(ns("p2_train"), "Train proportion",
                    min = 0.05, max = 0.95, value = split_ratio %||% 0.8, step = 0.05, width = "100%"),
        tags$label("Stratify by (y =):", style = "font-weight:600; font-size:13px; color:#343a40;"),
        selectInput(ns("p2_stratify"), label = NULL, choices = NULL, width = "100%"),
        uiOutput(ns("p2_stratify_warn")),
        div(style = "margin:6px 0;",
            span("Independent observations",
                 style = "background:#e8eaf6;color:#3949ab;padding:3px 8px;border-radius:12px;font-size:11px;"),
            br(), br(),
            tags$small(style = "color:#888;font-family:monospace;",
                       "Uses ", tags$code("base::sample()"), " and ",
                       tags$code("caret::createDataPartition()")))
      ),

      # ── 2b. 3-Partition ───────────────────────────────────────────────────
      conditionalPanel(
        condition = sprintf("input['%s'] === 'three_partition'", ns("split_method")),
        sliderInput(ns("p3_train"), "Train proportion",
                    min = 0.4, max = 0.8, value = 0.6, step = 0.05, width = "100%"),
        sliderInput(ns("p3_val"), "Validation proportion (of remainder)",
                    min = 0.1, max = 0.9, value = 0.5, step = 0.05, width = "100%"),
        tags$label("Stratify by (y =):", style = "font-weight:600; font-size:13px; color:#343a40;"),
        selectInput(ns("p3_stratify"), label = NULL, choices = NULL, width = "100%"),
        uiOutput(ns("p3_stratify_warn")),
        div(style = "margin:6px 0;",
            span("Independent observations",
                 style = "background:#e8eaf6;color:#3949ab;padding:3px 8px;border-radius:12px;font-size:11px;"),
            br(), br(),
            tags$small(style = "color:#888;font-family:monospace;",
                       "Uses ", tags$code("caret::createDataPartition()"), " twice"))
      ),

      # ── 2c. Stratified Bootstrap ──────────────────────────────────────────
      conditionalPanel(
        condition = sprintf("input['%s'] === 'boot_stratified'", ns("split_method")),
        sliderInput(ns("boot_n"), "Number of resamples",
                    min = 5, max = 100, value = 25, step = 5, width = "100%"),
        tags$label("Stratify by (y =):", style = "font-weight:600; font-size:13px; color:#343a40;"),
        selectInput(ns("boot_stratify"), label = NULL, choices = NULL, width = "100%"),
        uiOutput(ns("boot_stratify_warn")),
        div(style = "margin:6px 0;",
            span("Independent observations",
                 style = "background:#e8eaf6;color:#3949ab;padding:3px 8px;border-radius:12px;font-size:11px;"),
            br(), br(),
            tags$small(style = "color:#888;font-family:monospace;",
                       "Uses ", tags$code("caret::createResample()")))
      ),

      # ── 2d. Leave Group Out ───────────────────────────────────────────────
      conditionalPanel(
        condition = sprintf("input['%s'] === 'leave_group_out'", ns("split_method")),
        tags$label("Group variable:", style = "font-weight:600; font-size:13px; color:#343a40;"),
        selectInput(ns("lgo_group"), label = NULL, choices = NULL, width = "100%"),
        sliderInput(ns("lgo_k"), "Number of folds (k)",
                    min = 2, max = 10, value = 5, step = 1, width = "100%"),
        div(style = "margin:6px 0;",
            span("Group dependent observations",
                 style = "background:#e8f5e9;color:#2e7d32;padding:3px 8px;border-radius:12px;font-size:11px;"),
            br(), br(),
            tags$small(style = "color:#888;font-family:monospace;",
                       "Uses ", tags$code("caret::groupKFold()")))
      ),

      # ── 2e. Time Series ───────────────────────────────────────────────────
      conditionalPanel(
        condition = sprintf("input['%s'] === 'time_series'", ns("split_method")),
        tags$label("Time variable (sort order):", style = "font-weight:600; font-size:13px; color:#343a40;"),
        selectInput(ns("ts_var"), label = NULL, choices = NULL, width = "100%"),
        sliderInput(ns("ts_init"), "Initial window",
                    min = 2, max = 50, value = 5, step = 1, width = "100%"),
        sliderInput(ns("ts_horizon"), "Horizon",
                    min = 1, max = 20, value = 3, step = 1, width = "100%"),
        selectInput(ns("ts_fixed"), "Fixed window",
                    choices = c("FALSE (expanding)" = "FALSE", "TRUE (fixed)" = "TRUE"),
                    selected = "FALSE", width = "100%"),
        sliderInput(ns("ts_skip"), "Skip (slices between each window)",
                    min = 0, max = 10, value = 0, step = 1, width = "100%"),
        div(style = "margin:6px 0;",
            span("Time dependent observations",
                 style = "background:#fff8e1;color:#f57f17;padding:3px 8px;border-radius:12px;font-size:11px;"),
            br(), br(),
            tags$small(style = "color:#888;font-family:monospace;",
                       "Uses ", tags$code("caret::createTimeSlices()")))
      ),

      # ── 2f. Diversity Down-Sampling ───────────────────────────────────────
      conditionalPanel(
        condition = sprintf("input['%s'] === 'diversity'", ns("split_method")),
        sliderInput(ns("div_k"), "Number of clusters (k)",
                    min = 10, max = 300, value = 80, step = 10, width = "100%"),
        tags$label("Clustering features:", style = "font-weight:600; font-size:13px; color:#343a40;"),
        uiOutput(ns("div_features_ui")),
        tags$label("X axis variable (plot only):", style = "font-weight:600; font-size:13px; color:#343a40;"),
        selectInput(ns("div_x"), label = NULL, choices = NULL, width = "100%"),
        tags$label("Y axis variable (plot only):", style = "font-weight:600; font-size:13px; color:#343a40;"),
        selectInput(ns("div_y"), label = NULL, choices = NULL, width = "100%"),
        selectInput(ns("div_metric"), "Distance metric",
                    choices = c("euclidean", "manhattan"), selected = "euclidean", width = "100%"),
        selectInput(ns("div_stand"), "Standardise features",
                    choices = c("TRUE", "FALSE"), selected = "TRUE", width = "100%"),
        div(style = "margin:6px 0;",
            span("Diversity down-sampling",
                 style = "background:#fbe9e7;color:#bf360c;padding:3px 8px;border-radius:12px;font-size:11px;"),
            br(), br(),
            tags$small(style = "color:#888;font-family:monospace;",
                       "Uses ", tags$code("cluster::pam()")))
      ),

      br(),
      uiOutput(ns("apply_btn_ui")),
      uiOutput(ns("split_counts_ui")),
      hr(),

      # ── 3. Important vars ─────────────────────────────────────────────────
      tags$label("Domain Important Variables:", style = "font-weight:600; font-size:13px; color:#343a40;"),
      helpText("These will show a shield icon in the role buckets."),
      selectizeInput(ns("important_vars"), label = NULL,
                     choices = NULL, multiple = TRUE,
                     options = list(placeholder = "Select important variables")),
      hr(),

      # ── 4. Custom title ───────────────────────────────────────────────────
      tags$label("Custom panel title:", style = "font-weight:600; font-size:13px; color:#343a40;"),
      textInput(ns("custom_title"), label = NULL, placeholder = "Auto-generated if empty"),
      hr(),

      # ── 5. Reset ──────────────────────────────────────────────────────────
      actionButton(ns("reset"), label = "Reset All", icon = icon("rotate-left"), width = "100%")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("main_tabs"),
        tabPanel("Roles",         uiOutput(ns("roles_ui"))),
        tabPanel("Split Results", uiOutput(ns("split_results_ui")))
      )
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

data_roles_server <- function(id, get_raw, default_roles = NULL,
                              auto_split = FALSE, split_ratio = NULL) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Resolve split_ratio default here so every downstream use is consistent
    split_ratio <- split_ratio %||% 0.8

    # ── Role definitions ─────────────────────────────────────────────────────

    ROLES <- list(
      list(id = "predictor",  label = "Predictor",       hdr = "#185FA5", bg = "#E6F1FB", txt = "#0C447C", bdr = "#B5D4F4"),
      list(id = "outcome",    label = "Outcome",          hdr = "#0F6E56", bg = "#E1F5EE", txt = "#085041", bdr = "#9FE1CB"),
      list(id = "obs_id",     label = "Observation ID",   hdr = "#534AB7", bg = "#EEEDFE", txt = "#3C3489", bdr = "#CECBF6"),
      list(id = "split",      label = "Train-Test Split", hdr = "#BA7517", bg = "#FAEEDA", txt = "#633806", bdr = "#FAC775"),
      list(id = "weight",     label = "Case Weight",      hdr = "#3B6D11", bg = "#EAF3DE", txt = "#27500A", bdr = "#C0DD97"),
      list(id = "stratifier", label = "Stratifier",       hdr = "#3C3489", bg = "#EEEDFE", txt = "#26215C", bdr = "#AFA9EC"),
      list(id = "sensitive",  label = "Sensitive",        hdr = "#993556", bg = "#FBEAF0", txt = "#72243E", bdr = "#F4C0D1"),
      list(id = "ignore",     label = "Ignore",           hdr = "#5F5E5A", bg = "#F1EFE8", txt = "#444441", bdr = "#D3D1C7"),
      list(id = "date",       label = "Date",             hdr = "#8B4513", bg = "#FDF3E7", txt = "#5C2E00", bdr = "#F0C080")
    )

    role_ids <- sapply(ROLES, `[[`, "id")

    # ── Split state ──────────────────────────────────────────────────────────
    # list(name, data, method, extra) or NULL
    split_col <- reactiveVal(NULL)

    # reset split when dataset changes to prevent row-count mismatch crash
    observeEvent(get_raw(), {
      split_col(NULL)
    }, ignoreInit = TRUE)

    # auto-apply 2-partition split on first data load when task-specific var is set
    observeEvent(get_raw(), {
      if (!isTRUE(auto_split)) return()
      df <- get_raw(); req(df)
      n  <- nrow(df)
      # use outcome col from default_roles for stratification, fallback to first col
      outcome_col <- if (!is.null(default_roles)) {
        names(default_roles)[unlist(default_roles) == "outcome"]
      } else character(0)
      outcome_col <- intersect(outcome_col, names(df))
      y <- if (length(outcome_col) > 0) {
        df[[outcome_col[1]]]
      } else {
        df[[1]]
      }
      set.seed(input$seed %||% 42)
      idx_str <- caret::createDataPartition(y = y, p = split_ratio, list = FALSE)
      idx_srs <- base::sample(x = n, size = floor(split_ratio * n), replace = FALSE)
      out     <- rep("Test", n)
      out[idx_str] <- "Train"
      split_col(list(
        name   = "split_TrainTest",
        data   = factor(out, levels = c("Train", "Test")),
        method = "two_partition",
        extra  = list(idx_srs = idx_srs, idx_str = idx_str, y = y)
      ))
      # build full assignments so default_roles + split role are both applied
      # (can't rely on init_assignments because it skips default_roles when cur is non-empty)
      vars    <- c(names(df), "split_TrainTest")
      new_out <- setNames(rep("predictor", length(vars)), vars)
      if (!is.null(default_roles)) {
        for (v in intersect(names(default_roles), vars))
          new_out[[v]] <- default_roles[[v]]
      }
      new_out[["split_TrainTest"]] <- "split"
      assignments(new_out)
    }, ignoreInit = FALSE, once = TRUE)

    # ── Internal data ────────────────────────────────────────────────────────

    get_data <- reactive({
      df <- get_raw(); req(df)

      # inject applied split column; guard against stale split from a previous dataset
      sc <- split_col()
      if (!is.null(sc)) {
        if (length(sc$data) == nrow(df)) {
          df[[sc$name]] <- sc$data
        } else {
          split_col(NULL)  # self-heal: discard split that no longer matches
        }
      }

      # convert date-role columns to Date type so has_type("date") works in recipes
      cur <- isolate(assignments())
      date_cols <- names(cur)[cur == "date"]
      for (col in date_cols) {
        if (col %in% names(df)) {
          converted <- suppressWarnings(as.Date(as.character(df[[col]]), tryFormats = c(
            "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%Y/%m/%d", "%d-%m-%Y"
          )))
          if (!all(is.na(converted))) df[[col]] <- converted
        }
      }
      df
    })

    # ── Dropdown population ──────────────────────────────────────────────────

    observe({
      df <- get_raw(); req(df)
      all_cols <- names(df)
      num_cols <- names(df)[sapply(df, is.numeric)]

      # auto-select outcome variable for stratify dropdowns; preserve user's choice if already set
      cur          <- assignments()
      outcome_vars <- names(cur)[cur == "outcome"]
      default_strat <- if (length(outcome_vars) > 0) outcome_vars[1] else all_cols[1]

      for (id in c("p2_stratify", "p3_stratify", "boot_stratify")) {
        cur_sel  <- isolate(input[[id]])
        selected <- if (!is.null(cur_sel) && nzchar(cur_sel) && cur_sel %in% all_cols) cur_sel else default_strat
        updateSelectInput(session, id, choices = all_cols, selected = selected)
      }
      updateSelectInput(session, "lgo_group",
                        choices = all_cols,
                        selected = isolate(input$lgo_group) %||% all_cols[1])
      date_vars    <- names(cur)[cur == "date"]
      default_time <- if (length(date_vars) > 0) date_vars[1] else all_cols[1]
      ts_sel       <- isolate(input$ts_var)
      updateSelectInput(session, "ts_var",
                        choices  = all_cols,
                        selected = if (!is.null(ts_sel) && nzchar(ts_sel) && ts_sel %in% all_cols) ts_sel else default_time)
      updateSelectInput(session, "div_x",
                        choices = num_cols,
                        selected = isolate(input$div_x) %||% num_cols[1])
      updateSelectInput(session, "div_y",
                        choices = num_cols,
                        selected = isolate(input$div_y) %||%
                          (if (length(num_cols) > 1) num_cols[2] else num_cols[1]))
    })

    # ── Stratify mismatch warnings ────────────────────────────────────────────
    .stratify_warn <- function(sel_id) {
      renderUI({
        cur          <- assignments()
        outcome_vars <- names(cur)[cur == "outcome"]
        sel          <- input[[sel_id]]
        if (length(outcome_vars) == 0 || is.null(sel) || sel %in% outcome_vars) return(NULL)
        div(style = "margin:-4px 0 6px 0; padding:5px 8px; background:#fff3cd;
                     border-left:3px solid #fd7e14; border-radius:3px; font-size:12px; color:#856404;",
            icon("triangle-exclamation"), " Not stratifying by the outcome column (",
            tags$b(outcome_vars[1]), ").")
      })
    }
    output$p2_stratify_warn   <- .stratify_warn("p2_stratify")
    output$p3_stratify_warn   <- .stratify_warn("p3_stratify")
    output$boot_stratify_warn <- .stratify_warn("boot_stratify")

    # diversity clustering features selector
    output$div_features_ui <- renderUI({
      df <- get_raw(); req(df)
      num_cols <- names(df)[sapply(df, is.numeric)]
      selectizeInput(ns("div_features"), label = NULL,
                     choices = num_cols, selected = num_cols,
                     multiple = TRUE, width = "100%",
                     options = list(placeholder = "Select numeric features"))
    })

    # ── Apply button (colour per method) ─────────────────────────────────────

    output$apply_btn_ui <- renderUI({
      method <- input$split_method %||% "two_partition"
      btn_color <- switch(method,
        two_partition   = "#4f46e5",
        three_partition = "#4f46e5",
        boot_stratified = "#4f46e5",
        leave_group_out = "#15803d",
        time_series     = "#b45309",
        diversity       = "#9a3412",
        "#4f46e5"
      )
      tagList(
        tags$style(HTML(sprintf(
          "#%s { background-color: %s !important; border-color: %s !important;
                 color: white !important; width: 100%%; font-weight: 500; }
           #%s:hover { filter: brightness(0.88); }",
          ns("apply_split"), btn_color, btn_color, ns("apply_split")
        ))),
        actionButton(ns("apply_split"),
                     label = tagList(icon("check"), " Apply this split"),
                     width = "100%")
      )
    })

    # ── Split counts hint ─────────────────────────────────────────────────────

    output$split_counts_ui <- renderUI({
      sc <- split_col(); req(sc)
      tbl   <- table(sc$data)
      parts <- paste(names(tbl), paste0("<b>", as.integer(tbl), "</b>"), sep = ": ", collapse = "&nbsp;&nbsp;|&nbsp;&nbsp;")
      div(
        style = "margin-top:10px; padding:10px 12px; border-radius:8px;
                 background:#d1e7dd; border:1.5px solid #0f5132; color:#0f5132;",
        div(style = "font-size:13px; font-weight:600; margin-bottom:4px;",
            icon("circle-check"), HTML("&nbsp; Split applied successfully")),
        div(style = "font-size:13px;", HTML(parts)),
        div(style = "font-size:11px; margin-top:6px; color:#155724; font-style:italic;",
            icon("arrow-right"), " Check the ", tags$b("Split Results"), " tab for details.")
      )
    })

    # ── Apply split ───────────────────────────────────────────────────────────

    observeEvent(input$apply_split, {
      df  <- get_raw(); req(df)
      n   <- nrow(df)
      set.seed(as.integer(input$seed %||% 42))

      method   <- input$split_method
      col_name <- switch(method,
        two_partition   = "split_TrainTest",
        three_partition = "split_TrainValTest",
        boot_stratified = "split_Bootstrap",
        leave_group_out = "split_GroupFold",
        time_series     = "split_TimeSeries",
        diversity       = "split_Diversity"
      )

      result <- tryCatch({

        if (method == "two_partition") {
          y <- df[[input$p2_stratify]]
          p <- input$p2_train
          # tutorial: caret::createDataPartition for stratified (immediately after set.seed)
          idx_str <- caret::createDataPartition(y = y, p = p, list = FALSE)
          # tutorial: base::sample for simple random (after, so seed state matches classmates)
          idx_srs <- base::sample(x = n, size = floor(p * n), replace = FALSE)
          # use stratified as the actual split column
          out <- rep("Test", n)
          out[idx_str] <- "Train"
          list(
            data  = factor(out, levels = c("Train", "Test")),
            extra = list(idx_srs = idx_srs, idx_str = idx_str, y = y)
          )

        } else if (method == "three_partition") {
          y <- df[[input$p3_stratify]]
          # tutorial: first createDataPartition for train
          idx1 <- caret::createDataPartition(y = y, p = input$p3_train, list = FALSE)
          rem  <- setdiff(seq_len(n), idx1)
          # tutorial: second createDataPartition on remainder
          # p = 1 - val_prop gives test proportion; subIndex → test, -subIndex → val
          idx2     <- caret::createDataPartition(y = y[rem], p = 1 - input$p3_val, list = FALSE)
          test_idx <- rem[idx2]
          val_idx  <- rem[-idx2]
          out <- rep("Val", n)
          out[idx1]    <- "Train"
          out[test_idx] <- "Test"
          list(
            data  = factor(out, levels = c("Train", "Val", "Test")),
            extra = list()
          )

        } else if (method == "boot_stratified") {
          y         <- df[[input$boot_stratify]]
          # tutorial: caret::createResample
          resamples <- caret::createResample(y = y, times = input$boot_n, list = TRUE)
          # split column: included/excluded in first resample
          out <- rep("Excluded", n)
          out[unique(resamples[[1]])] <- "Included"
          # tutorial: calcPerc — unique obs % per resample
          calcPerc  <- function(x) round(length(unique(x)) / length(x) * 100)
          uniq_pct  <- sapply(resamples, calcPerc)
          list(
            data  = factor(out, levels = c("Included", "Excluded")),
            extra = list(uniq_pct = uniq_pct)
          )

        } else if (method == "leave_group_out") {
          grp   <- df[[input$lgo_group]]
          # tutorial: caret::groupKFold
          folds <- caret::groupKFold(group = grp, k = input$lgo_k)
          # assign each obs to the fold where it is held out (not in training)
          out <- rep(NA_character_, n)
          for (i in seq_along(folds)) {
            hold_out <- setdiff(seq_len(n), folds[[i]])
            out[hold_out] <- names(folds)[i]
          }
          list(
            data  = factor(out),
            extra = list(folds = folds, grp = grp)
          )

        } else if (method == "time_series") {
          ord <- order(df[[input$ts_var]])
          # tutorial: caret::createTimeSlices
          slices <- caret::createTimeSlices(
            y             = seq_len(n),
            initialWindow = input$ts_init,
            horizon       = input$ts_horizon,
            fixedWindow   = as.logical(input$ts_fixed),
            skip          = input$ts_skip
          )
          # use last window as the definitive split (tutorial pattern)
          last      <- length(slices$train)
          train_idx <- ord[slices$train[[last]]]
          test_idx  <- ord[slices$test[[last]]]
          out       <- rep("Unused", n)
          out[train_idx] <- "Train"
          out[test_idx]  <- "Test"
          list(
            data  = factor(out, levels = c("Train", "Test", "Unused")),
            extra = list(n_slices = last)
          )

        } else if (method == "diversity") {
          feats  <- input$div_features
          req(length(feats) > 0)
          num_df <- df[, intersect(feats, names(df)[sapply(df, is.numeric)]), drop = FALSE]
          req(ncol(num_df) > 0)
          # tutorial: cluster::pam
          cl <- cluster::pam(num_df, k = input$div_k,
                             metric = input$div_metric,
                             stand  = as.logical(input$div_stand))
          out <- rep("Not Sampled", n)
          out[cl$id.med] <- "Sampled"
          list(
            data  = factor(out, levels = c("Sampled", "Not Sampled")),
            extra = list(x_var = input$div_x, y_var = input$div_y)
          )
        }

      }, error = function(e) {
        showNotification(paste("Split error:", e$message), type = "error", session = session)
        NULL
      })

      req(!is.null(result))

      # remove old split col assignment, add new
      cur <- assignments()
      for (old in names(cur)[cur == "split"]) cur[[old]] <- "predictor"
      cur[[col_name]] <- "split"
      assignments(cur)

      tbl   <- table(result$data)
      parts <- paste(names(tbl), as.integer(tbl), sep = ": ", collapse = " | ")
      showNotification(
        tagList(
          tags$b(icon("circle-check"), " Split applied"),
          tags$br(),
          tags$span(parts, style = "font-family:monospace;"),
          tags$br(),
          tags$small(icon("arrow-right"), " Check the Split Results tab for details.")
        ),
        type = "message", duration = 6, session = session
      )

      split_col(list(name = col_name, data = result$data,
                     method = method, extra = result$extra))
    })

    # ── Split results UI ──────────────────────────────────────────────────────

    output$split_results_ui <- renderUI({
      sc <- split_col()

      if (is.null(sc)) {
        return(div(
          style = "padding:60px; text-align:center; color:#6c757d;",
          icon("circle-info", style = "font-size:32px; margin-bottom:12px; display:block;"),
          tags$p("No split applied yet.", style = "font-size:15px; font-weight:500;"),
          tags$p("Configure and click 'Apply this split' in the sidebar.",
                 style = "font-size:13px;")
        ))
      }

      # partition count table
      tbl <- as.data.frame(table(Partition = sc$data))
      tbl$Proportion <- paste0(round(tbl$Freq / sum(tbl$Freq) * 100, 1), "%")
      names(tbl) <- c("Partition", "Count", "Proportion")

      html_table <- function(df) {
        tags$table(
          style = "border-collapse:collapse; margin-bottom:16px;",
          tags$thead(tags$tr(style = "background:#f8f9fa; font-weight:600;",
                             lapply(names(df), function(h)
                               tags$th(h, style = "padding:8px 16px; border:1px solid #dee2e6;")))),
          tags$tbody(lapply(seq_len(nrow(df)), function(i)
            tags$tr(lapply(df[i, ], function(cell)
              tags$td(as.character(cell), style = "padding:8px 16px; border:1px solid #dee2e6;")))))
        )
      }

      method_label <- switch(sc$method,
        two_partition   = "2-Partition (Train / Test)",
        three_partition = "3-Partition (Train / Val / Test)",
        boot_stratified = "Stratified Bootstrap",
        leave_group_out = "Leave Group Out",
        time_series     = "Time Series",
        diversity       = "Diversity Down-Sampling"
      )

      # method-specific extra content
      extra_content <- if (sc$method == "two_partition") {
        # tutorial: compare table(iris[trainIndex, "Species"]) for SRS vs stratified
        y       <- sc$extra$y
        idx_srs <- sc$extra$idx_srs
        idx_str <- sc$extra$idx_str
        srs_tbl <- as.data.frame(table(y[idx_srs]))
        str_tbl <- as.data.frame(table(y[idx_str]))
        names(srs_tbl) <- c("Value", "Simple Random")
        names(str_tbl) <- c("Value", "Stratified")
        cmp <- merge(srs_tbl, str_tbl, by = "Value", all = TRUE)
        tagList(
          tags$h6("Train partition — Simple Random vs Stratified (y distribution):",
                  style = "font-weight:600; margin-top:16px;"),
          html_table(cmp)
        )

      } else if (sc$method == "boot_stratified") {
        # tutorial: calcPerc — show unique % per resample
        pct <- sc$extra$uniq_pct
        tagList(
          tags$h6("Unique observations per resample (tutorial: calcPerc):",
                  style = "font-weight:600; margin-top:16px;"),
          tags$p(paste(paste0(seq_along(pct), ": ", pct, "%"), collapse = "  |  "),
                 style = "font-size:12px; font-family:monospace; color:#495057; line-height:1.8;"),
          tags$p(paste0("Mean uniqueness: ", round(mean(pct), 1), "%"),
                 style = "font-size:13px; font-weight:500; margin-top:8px;")
        )

      } else if (sc$method == "leave_group_out") {
        # tutorial: show distinct groups in train and hold-out per fold
        folds <- sc$extra$folds
        grp   <- sc$extra$grp
        fold_summary <- do.call(rbind, lapply(seq_along(folds), function(i) {
          train_grps   <- length(unique(grp[folds[[i]]]))
          holdout_grps <- length(unique(grp[setdiff(seq_along(grp), folds[[i]])]))
          data.frame(Fold = names(folds)[i],
                     `Train groups`   = train_grps,
                     `Hold-out groups` = holdout_grps,
                     check.names = FALSE)
        }))
        tagList(
          tags$h6("Distinct groups per fold (tutorial: inGroups / outGroups):",
                  style = "font-weight:600; margin-top:16px;"),
          html_table(fold_summary)
        )

      } else if (sc$method == "time_series") {
        # tutorial: str(tsSamples$train), str(tsSamples$test)
        tagList(
          tags$h6(paste("Total time slices created:", sc$extra$n_slices),
                  style = "font-weight:600; margin-top:16px;"),
          tags$p("Split column reflects the last window only.",
                 style = "font-size:12px; color:#6c757d;")
        )

      } else if (sc$method == "diversity") {
        # tutorial: ggplot scatter coloured by Type
        plotOutput(ns("div_scatter"), height = "350px")

      } else {
        NULL
      }

      tagList(
        tags$h5(method_label, style = "font-weight:600; margin-bottom:4px;"),
        tags$p(paste("Column injected:", sc$name),
               style = "color:#6c757d; font-size:13px; margin-bottom:12px;"),
        html_table(tbl),
        extra_content
      )
    })

    # diversity scatter plot (tutorial: ggplot coloured by Type)
    output$div_scatter <- renderPlot({
      sc <- split_col()
      req(!is.null(sc), sc$method == "diversity")
      df    <- get_raw(); req(df)
      x_var <- sc$extra$x_var
      y_var <- sc$extra$y_var
      req(x_var %in% names(df), y_var %in% names(df))
      plot_df <- data.frame(x = df[[x_var]], y = df[[y_var]], Type = sc$data)
      ggplot2::ggplot(plot_df, ggplot2::aes(x = x, y = y, color = Type)) +
        ggplot2::geom_point(alpha = 0.7) +
        ggplot2::scale_color_manual(
          values = c("Sampled" = "#e63946", "Not Sampled" = "#adb5bd")) +
        ggplot2::labs(title = "Diversity Down-Sampling", x = x_var, y = y_var) +
        ggplot2::theme_minimal()
    })

    # ── Important vars selector ───────────────────────────────────────────────

    observe({
      df <- get_data(); req(df)
      updateSelectizeInput(session, "important_vars",
                           choices  = names(df),
                           selected = isolate(input$important_vars),
                           server   = TRUE)
    })

    # ── Assignments state ─────────────────────────────────────────────────────

    assignments <- reactiveVal(list())

    init_assignments <- function(df) {
      vars    <- names(df)
      cur     <- assignments()
      new_out <- setNames(rep("predictor", length(vars)), vars)

      # apply task-specific defaults to columns seen for the first time
      # (first load, or a newly selected / uploaded dataset)
      if (!is.null(default_roles)) {
        for (v in intersect(names(default_roles), setdiff(vars, names(cur)))) {
          new_out[[v]] <- default_roles[[v]]
        }
      }

      # preserve existing assignments for variables still present
      shared <- intersect(names(new_out), names(cur))
      new_out[shared] <- cur[shared]

      assignments(new_out)
    }

    observeEvent(get_data(), {
      req(get_data())
      init_assignments(get_data())
    })

    observeEvent(input$reset, {
      req(get_raw())
      new_out <- setNames(rep("predictor", ncol(get_raw())), names(get_raw()))
      assignments(new_out)
      split_col(NULL)
      updateNumericInput(session,    "seed",          value = as.integer(format(Sys.Date(), "%Y")))
      updateSelectizeInput(session,  "important_vars", selected = character(0))
      updateTextInput(session,       "custom_title",  value = "")
    })

    observeEvent(input$dropped, {
      req(input$dropped)
      info <- input$dropped
      cur  <- assignments()
      cur[[info$var]] <- info$to
      assignments(cur)
    })

    # ── Roles UI ──────────────────────────────────────────────────────────────

    output$roles_ui <- renderUI({
      req(get_data(), length(assignments()) > 0)
      cur       <- assignments()
      vars      <- names(cur)
      important <- input$important_vars %||% character(0)

      pill <- function(v, role_id) {
        r   <- if (role_id == "unassigned") NULL else ROLES[[which(role_ids == role_id)]]
        bg  <- if (is.null(r)) "#f1f3f5" else r$bg
        txt <- if (is.null(r)) "#495057" else r$txt
        bdr <- if (is.null(r)) "#dee2e6" else r$bdr
        icon_html <- if (v %in% important)
          "<i class='fas fa-shield-alt' style='color:#993556;font-size:14px;margin-right:3px;'></i>"
        else ""
        tags$div(
          class = "rv-pill", draggable = "true",
          `data-var` = v, `data-zone` = role_id,
          style = paste0(
            "display:inline-flex;align-items:center;gap:5px;",
            "padding:4px 10px;border-radius:999px;font-size:12px;font-weight:500;",
            "cursor:grab;user-select:none;border:0.5px solid ", bdr, ";",
            "background:", bg, ";color:", txt, ";margin:2px;"
          ),
          HTML(paste0(icon_html, v))
        )
      }

      unassigned_vars <- vars[cur == "unassigned"]
      unassigned_pool <- tags$div(
        tags$p("Unassigned",
               style = "font-size:12px;color:#6c757d;margin-bottom:6px;font-weight:500;"),
        tags$div(
          id = "zone-unassigned", class = "rv-zone",
          style = paste0(
            "display:flex;flex-wrap:wrap;gap:4px;padding:8px;min-height:44px;",
            "background:#f8f9fa;border-radius:8px;border:0.5px solid #dee2e6;",
            "margin-bottom:16px;"
          ),
          if (length(unassigned_vars) > 0)
            lapply(unassigned_vars, pill, role_id = "unassigned")
          else
            tags$span("(all assigned)", style = "font-size:12px;color:#adb5bd;")
        )
      )

      title_text <- if (nzchar(trimws(input$custom_title))) input$custom_title else "Assigned Variable Roles"
      role_grid <- tagList(
        tags$h4(title_text,
                style = "font-weight:600; margin-bottom:20px; color:#343a40; text-align:center;"),
        tags$div(
          style = "display:grid;grid-template-columns:1fr 1fr;gap:12px;",
          lapply(ROLES, function(r) {
            zone_vars <- vars[cur == r$id]
            tags$div(
              style = "border-radius:10px;border:0.5px solid #dee2e6;overflow:hidden;",
              tags$div(r$label,
                       style = paste0("padding:8px 14px;font-size:13px;font-weight:500;",
                                      "background:", r$hdr, ";color:white;")),
              tags$div(
                id = paste0("zone-", r$id), class = "rv-zone",
                style = paste0(
                  "display:flex;flex-wrap:wrap;gap:4px;padding:8px;min-height:52px;",
                  "background:white;border-top:0.5px solid #dee2e6;"
                ),
                if (length(zone_vars) > 0)
                  lapply(zone_vars, pill, role_id = r$id)
                else
                  tags$span("Drop here", style = "font-size:12px;color:#adb5bd;")
              )
            )
          })
        )
      )

      drag_js <- tags$script(HTML(sprintf("
        (function() {
          var dragging = null, fromZone = null;
          function attach() {
            document.querySelectorAll('.rv-pill').forEach(function(pill) {
              pill.addEventListener('dragstart', function(e) {
                dragging = pill.dataset.var;
                fromZone = pill.dataset.zone;
                e.dataTransfer.effectAllowed = 'move';
              });
              pill.addEventListener('dragend', function() {
                dragging = null; fromZone = null;
              });
            });
            document.querySelectorAll('.rv-zone').forEach(function(zone) {
              zone.addEventListener('dragover', function(e) {
                e.preventDefault();
                e.stopPropagation();
                zone.style.outline = '2px dashed #4a90d9';
              });
              zone.addEventListener('dragleave', function(e) {
                if (!zone.contains(e.relatedTarget)) zone.style.outline = '';
              });
              zone.addEventListener('drop', function(e) {
                e.preventDefault();
                e.stopPropagation();
                zone.style.outline = '';
                var toZone = zone.id.replace('zone-', '');
                if (dragging && fromZone !== toZone) {
                  Shiny.setInputValue('%s', {var: dragging, to: toZone}, {priority: 'event'});
                }
                dragging = null; fromZone = null;
              });
            });
          }
          $(document).on('shiny:value', function(e) {
            if (e.name === '%s') setTimeout(attach, 80);
          });
          setTimeout(attach, 200);
        })();
      ", ns("dropped"), ns("roles_ui"))))

      tagList(unassigned_pool, role_grid, drag_js)
    })

    # ── Return ────────────────────────────────────────────────────────────────

    return(list(
      data           = get_data,
      roles          = reactive(assignments()),
      important_vars = reactive(input$important_vars %||% character(0)),
      seed           = reactive(as.integer(input$seed %||% 42))
    ))

  })
}
