# =================================================================================
# mod_miss_impute.R
# =================================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

miss_impute_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 3,
      style = "background-color: #e8f0fe; border-left: 2px solid #a8c0fd;
               min-height: 100vh; padding-left: 20px;",
      
      # ── Tab note ──────────────────────────────────────────────────────────
      div(
        style = "font-size: 13px; color: #343a40; background-color: white;
                 padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
                 margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; <b>Imputation Strategy</b><br><br>
              Imputer is <b>fitted on train only</b> and applied to both
              train and test splits. This prevents data leakage while
              ensuring no NAs reach the model.")
      ),
      hr(),
      
      # ── Response variable ─────────────────────────────────────────────────
      tags$div(
        style = "margin-bottom: 10px;",
        tags$label("Response (Y) variable:",
                   style = "font-weight:600; font-size:13px; color:#343a40;"),
        selectInput(ns("y_var"), label = NULL, choices = c("(none)"), width = "100%")
      ),
      
      # ── Split variable ────────────────────────────────────────────────────
      tags$div(
        style = "margin-bottom: 4px;",
        tags$label("Split variable:",
                   style = "font-weight:600; font-size:13px; color:#343a40;"),
        selectInput(ns("split_var"), label = NULL, choices = c("(none)"), width = "100%")
      ),
      
      # Train / Test level selectors
      conditionalPanel(
        condition = sprintf("input['%s'] != '(none)'", ns("split_var")),
        fluidRow(
          column(6,
                 tags$label("Train level:", style = "font-size:12px; font-weight:600; color:#343a40;"),
                 selectInput(ns("train_level"), label = NULL, choices = c("(none)"), width = "100%")
          ),
          column(6,
                 tags$label("Test level:", style = "font-size:12px; font-weight:600; color:#343a40;"),
                 selectInput(ns("test_level"), label = NULL, choices = c("(none)"), width = "100%")
          )
        )
      ),
      
      # ── Ignore variables ──────────────────────────────────────────────────
      tags$div(
        style = "margin-top: 10px; margin-bottom: 10px;",
        tags$label("Ignore variables:",
                   style = "font-weight:600; font-size:13px; color:#343a40;"),
        div(style = "font-size:11px; color:#6c757d; margin-bottom:4px;",
            "Auto-filled from roles: Sensitive, Case Weight, Stratifier, Ignore"),
        selectizeInput(ns("ignore_vars"), label = NULL,
                       choices  = NULL,
                       multiple = TRUE,
                       options  = list(placeholder = "Select vars to exclude from imputation..."),
                       width    = "100%")
      ),
      hr(),
      
      # ── Algorithm choice ──────────────────────────────────────────────────
      tags$label("Imputation algorithm:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      radioButtons(
        ns("algorithm"),
        label    = NULL,
        choices  = c(
          "KNN (step_impute_knn)"         = "knn",
          "Bagged Tree (step_impute_bag)" = "bag",
          "Mean / Median / Mode"          = "mmm"
        ),
        selected = "bag"
      ),
      hr(),
      
      # ── KNN parameters ────────────────────────────────────────────────────
      conditionalPanel(
        condition = sprintf("input['%s'] == 'knn'", ns("algorithm")),
        tags$label("KNN parameters:", style = "font-weight:600; font-size:13px; color:#343a40;"),
        sliderInput(ns("knn_neighbors"), "Neighbours (k):",
                    min = 1, max = 25, value = 2, step = 1, width = "100%")
      ),
      
      # ── Bag parameters ────────────────────────────────────────────────────
      conditionalPanel(
        condition = sprintf("input['%s'] == 'bag'", ns("algorithm")),
        tags$label("Bag parameters:", style = "font-weight:600; font-size:13px; color:#343a40;"),
        sliderInput(ns("bag_trees"), "Number of trees:",
                    min = 2, max = 50, value = 4, step = 1, width = "100%"),
      ),
      
      # ── MMM parameters ────────────────────────────────────────────────────
      conditionalPanel(
        condition = sprintf("input['%s'] == 'mmm'", ns("algorithm")),
        tags$label("MMM parameters:",
                   style = "font-weight:600; font-size:13px; color:#343a40;"),
        div(style = "font-size:12px; color:#6c757d; margin-bottom:8px;",
            "Categorical columns are always imputed with mode."),
        tags$label("Impute with Mean:",
                   style = "font-size:12px; font-weight:600; color:#343a40; display:block; margin-bottom:2px;"),
        selectizeInput(ns("mmm_mean_cols"), label = NULL,
                       choices = NULL, multiple = TRUE,
                       options = list(placeholder = "Select numeric columns..."),
                       width = "100%"),
        tags$label("Impute with Median:",
                   style = "font-size:12px; font-weight:600; color:#343a40; display:block; margin-top:6px; margin-bottom:2px;"),
        selectizeInput(ns("mmm_median_cols"), label = NULL,
                       choices = NULL, multiple = TRUE,
                       options = list(placeholder = "Select numeric columns..."),
                       width = "100%")
      ),
      hr(),
      
      # ── Bag warning ───────────────────────────────────────────────────────
      conditionalPanel(
        condition = sprintf("input['%s'] == 'bag'", ns("algorithm")),
        div(
          style = "background:#fff3cd; border:1px solid #ffc107; border-radius:6px;
                   padding:8px 10px; font-size:12px; color:#856404; margin-bottom:10px;",
          icon("triangle-exclamation", style = "color:#ffc107;"),
          HTML("&nbsp; Bagged tree imputation can be slow on large datasets.
                Click <b>Run Imputation</b> when ready.")
        )
      ),
      
      # ── Run button ────────────────────────────────────────────────────────
      actionButton(
        ns("run"),
        label = "Run Imputation",
        icon  = icon("play"),
        width = "100%",
        style = "background-color:#185FA5; color:white; border:none; margin-bottom:8px;"
      ),
      
      # ── Reset current run ─────────────────────────────────────────────────
      actionButton(
        ns("reset"),
        label = "Reset",
        icon  = icon("rotate-left"),
        width = "100%",
        style = "margin-bottom:4px;"
      ),
      
      # ── Clear comparison ──────────────────────────────────────────────────
      actionButton(
        ns("reset_history"),
        label = "Clear Comparison",
        icon  = icon("trash-can"),
        width = "100%",
        style = "background-color:#6c757d; color:white; border:none;"
      ),
    ),
    
    # ── Main panel ────────────────────────────────────────────────────────────
    mainPanel(
      width = 9,
      tabsetPanel(
        id   = ns("main_tabs"),
        type = "tabs",
        
        # ── Tab 1: Current Run ──────────────────────────────────────────────
        tabPanel(
          title = tagList(icon("fill-drip"), " Current Run"),
          style = "padding-top:16px;",
          uiOutput(ns("current_run_ui"))
        ),
        
        # ── Tab 2: Comparison ───────────────────────────────────────────────
        tabPanel(
          title = tagList(icon("chart-area"), " Comparison"),
          style = "padding-top:16px;",
          uiOutput(ns("comparison_ui")),
          # ── Custom plot title ─────────────────────────────────────────────
          div(
            style = "background:#f8f9fa; border:1px solid #dee2e6; border-radius:10px;
                     padding:12px 20px; margin-bottom:12px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$label("Custom plot title:",
                       style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:6px;"),
            textInput(ns("custom_title"), label = NULL,
                      placeholder = "Auto-generated if empty", width = "100%")
          ),
          # ── View Mode controls — static div so selections never reset ─────
          div(
            style = "background:#f8f9fa; border:1px solid #dee2e6; border-radius:10px;
                     padding:16px 20px; margin-bottom:12px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$label("View Mode",
                       style = "font-weight:700; font-size:13px; color:#343a40; display:block; margin-bottom:10px;"),
            fluidRow(
              column(3,
                     radioButtons(ns("kde_mode"), label = NULL,
                                  choices  = c("Single"   = "single",
                                               "2x2 Grid" = "grid2",
                                               "3x3 Grid" = "grid3",
                                               "4x4 Grid" = "grid4"),
                                  selected = "single", inline = TRUE)
              ),
              column(9,
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'single'", ns("kde_mode")),
                       fluidRow(column(4, selectInput(ns("kde_col"), "Column:", choices = NULL, width = "100%")))
                     ),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'grid2'", ns("kde_mode")),
                       fluidRow(
                         column(6, selectInput(ns("kde_col1"), "Col 1:", choices = NULL, width = "100%")),
                         column(6, selectInput(ns("kde_col2"), "Col 2:", choices = NULL, width = "100%"))
                       ),
                       fluidRow(
                         column(6, selectInput(ns("kde_col3"), "Col 3:", choices = NULL, width = "100%")),
                         column(6, selectInput(ns("kde_col4"), "Col 4:", choices = NULL, width = "100%"))
                       )
                     ),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'grid3'", ns("kde_mode")),
                       fluidRow(
                         column(4, selectInput(ns("kde_col5"),  "Col 1:", choices = NULL, width = "100%")),
                         column(4, selectInput(ns("kde_col6"),  "Col 2:", choices = NULL, width = "100%")),
                         column(4, selectInput(ns("kde_col7"),  "Col 3:", choices = NULL, width = "100%"))
                       ),
                       fluidRow(
                         column(4, selectInput(ns("kde_col8"),  "Col 4:", choices = NULL, width = "100%")),
                         column(4, selectInput(ns("kde_col9"),  "Col 5:", choices = NULL, width = "100%")),
                         column(4, selectInput(ns("kde_col10"), "Col 6:", choices = NULL, width = "100%"))
                       ),
                       fluidRow(
                         column(4, selectInput(ns("kde_col11"), "Col 7:", choices = NULL, width = "100%")),
                         column(4, selectInput(ns("kde_col12"), "Col 8:", choices = NULL, width = "100%")),
                         column(4, selectInput(ns("kde_col13"), "Col 9:", choices = NULL, width = "100%"))
                       )
                     ),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'grid4'", ns("kde_mode")),
                       fluidRow(
                         column(3, selectInput(ns("kde_col14"), "Col 1:",  choices = NULL, width = "100%")),
                         column(3, selectInput(ns("kde_col15"), "Col 2:",  choices = NULL, width = "100%")),
                         column(3, selectInput(ns("kde_col16"), "Col 3:",  choices = NULL, width = "100%")),
                         column(3, selectInput(ns("kde_col17"), "Col 4:",  choices = NULL, width = "100%"))
                       ),
                       fluidRow(
                         column(3, selectInput(ns("kde_col18"), "Col 5:",  choices = NULL, width = "100%")),
                         column(3, selectInput(ns("kde_col19"), "Col 6:",  choices = NULL, width = "100%")),
                         column(3, selectInput(ns("kde_col20"), "Col 7:",  choices = NULL, width = "100%")),
                         column(3, selectInput(ns("kde_col21"), "Col 8:",  choices = NULL, width = "100%"))
                       ),
                       fluidRow(
                         column(3, selectInput(ns("kde_col22"), "Col 9:",  choices = NULL, width = "100%")),
                         column(3, selectInput(ns("kde_col23"), "Col 10:", choices = NULL, width = "100%")),
                         column(3, selectInput(ns("kde_col24"), "Col 11:", choices = NULL, width = "100%")),
                         column(3, selectInput(ns("kde_col25"), "Col 12:", choices = NULL, width = "100%"))
                       ),
                       fluidRow(
                         column(3, selectInput(ns("kde_col26"), "Col 13:", choices = NULL, width = "100%")),
                         column(3, selectInput(ns("kde_col27"), "Col 14:", choices = NULL, width = "100%")),
                         column(3, selectInput(ns("kde_col28"), "Col 15:", choices = NULL, width = "100%")),
                         column(3, selectInput(ns("kde_col29"), "Col 16:", choices = NULL, width = "100%"))
                       )
                     )
              )
            )
          )
        )
      )
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

miss_impute_server <- function(id, get_data, roles, seed = reactive(42)) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── Helpers ──────────────────────────────────────────────────────────────
    
    make_card <- function(..., bg = "white", border_color = "#dee2e6") {
      div(style = paste0(
        "background:", bg, "; border:1px solid ", border_color, ";",
        "border-radius:10px; padding:16px 20px; margin-bottom:12px;",
        "box-shadow:0 1px 3px rgba(0,0,0,0.06);"
      ), ...)
    }
    
    stat_box <- function(label, value, icon_name, color = "#0d6efd") {
      div(
        style = paste0(
          "display:flex; align-items:center; gap:14px;",
          "background:white; border:1px solid #dee2e6; border-radius:10px;",
          "padding:14px 18px; flex:1; min-width:160px;",
          "box-shadow:0 1px 2px rgba(0,0,0,0.05);"
        ),
        div(
          style = paste0(
            "width:42px; height:42px; border-radius:50%;",
            "background:", color, "22;",
            "display:flex; align-items:center; justify-content:center;",
            "flex-shrink:0;"
          ),
          icon(icon_name, style = paste0("color:", color, "; font-size:18px;"))
        ),
        div(
          div(style = "font-size:22px; font-weight:700; color:#212529; line-height:1.1;", value),
          div(style = "font-size:12px; color:#6c757d; margin-top:2px;", label)
        )
      )
    }
    
    # ── Populate selectors from roles ────────────────────────────────────────
    
    observe({
      req(get_data(), roles())
      r    <- roles()
      vars <- names(get_data())
      updateSelectInput(session, "y_var",
                        choices  = c("(none)", vars),
                        selected = { v <- names(r)[r == "outcome"]; if (length(v)) v[1] else "(none)" })
      updateSelectInput(session, "split_var",
                        choices  = c("(none)", vars),
                        selected = { v <- names(r)[r == "split"]; if (length(v)) v[1] else "(none)" })
    })
    
    # ── Train / Test level selectors ─────────────────────────────────────────
    
    observe({
      req(input$split_var, input$split_var != "(none)")
      df   <- get_data(); req(df)
      lvls <- as.character(unique(df[[input$split_var]]))
      lvls <- lvls[!is.na(lvls)]
      train_sel <- if ("Train" %in% lvls) "Train" else if ("train" %in% lvls) "train" else lvls[1]
      test_sel  <- if ("Test"  %in% lvls) "Test"  else if ("test"  %in% lvls) "test"  else
        if (length(lvls) >= 2) lvls[2] else lvls[1]
      updateSelectInput(session, "train_level", choices = c("(none)", lvls), selected = train_sel)
      updateSelectInput(session, "test_level",  choices = c("(none)", lvls), selected = test_sel)
    })
    
    # ── Auto-populate ignore vars from roles ─────────────────────────────────
    
    observeEvent(list(roles(), get_data()), {
      req(get_data())
      r    <- roles()
      vars <- names(get_data())
      ignore_role_names <- c("sensitive", "weight", "stratifier", "ignore")
      auto_ignored      <- if (!is.null(r))
        names(r)[tolower(as.character(r)) %in% ignore_role_names]
      else character(0)
      updateSelectizeInput(session, "ignore_vars",
                           choices  = vars,
                           selected = auto_ignored,
                           server   = TRUE)
    }, ignoreNULL = FALSE, ignoreInit = FALSE)
    
    # ── Populate MMM selectize boxes ─────────────────────────────────────────
    
    observe({
      req(get_data())
      df       <- get_data()
      excl     <- unique(c(input$y_var, input$split_var, input$ignore_vars))
      num_cols <- setdiff(names(df)[sapply(df, is.numeric)], excl)
      updateSelectizeInput(session, "mmm_mean_cols",   choices = num_cols, selected = num_cols, server = TRUE)
      updateSelectizeInput(session, "mmm_median_cols", choices = num_cols, selected = NULL,     server = TRUE)
    }) |> bindEvent(get_data(), input$y_var, input$split_var, input$ignore_vars)
    
    # ── State ─────────────────────────────────────────────────────────────────
    
    impute_result <- reactiveVal(NULL)
    run_history   <- reactiveVal(list())
    
    # ── Run ───────────────────────────────────────────────────────────────────
    
    observeEvent(input$run, {
      df   <- get_data(); req(df)
      algo <- input$algorithm
      start_time <- proc.time()[["elapsed"]]
      
      msg <- switch(algo,
                    "bag" = paste0("Running Bag imputation (", input$bag_trees, " trees) — this may take a while..."),
                    "knn" = paste0("Running KNN imputation (k=", input$knn_neighbors, ")..."),
                    "mmm" = "Running Mean/Median/Mode imputation..."
      )
      
      withProgress(message = msg, value = 0.1, {
        
        tryCatch({
          
          role_vals <- if (!is.null(roles)) roles() else NULL
          id_cols   <- if (!is.null(role_vals)) names(role_vals[role_vals == "obs_id"]) else character(0)
          
          excl <- unique(c(
            if (!is.null(input$y_var)     && input$y_var     != "(none)") input$y_var,
            if (!is.null(input$split_var) && input$split_var != "(none)") input$split_var,
            id_cols,
            if (length(input$ignore_vars) > 0) input$ignore_vars
          ))
          pred_cols <- setdiff(names(df), excl)
          
          sv        <- input$split_var
          tl        <- input$train_level
          ts        <- input$test_level
          splits_ok <- !is.null(sv) && sv != "(none)" &&
            !is.null(tl) && tl != "(none)" &&
            !is.null(ts) && ts != "(none)"
          
          train_df  <- if (splits_ok) df[as.character(df[[sv]]) == tl, , drop = FALSE] else df
          test_df   <- if (splits_ok) df[as.character(df[[sv]]) == ts, , drop = FALSE] else NULL
          train_pred <- train_df[, pred_cols, drop = FALSE]
          test_pred  <- if (!is.null(test_df)) test_df[, pred_cols, drop = FALSE] else NULL
          
          if (algo == "mmm") {
            library(recipes)
            num_cols    <- names(train_pred)[sapply(train_pred, is.numeric)]
            fac_cols    <- setdiff(pred_cols, num_cols)
            mean_cols   <- intersect(input$mmm_mean_cols,   num_cols)
            median_cols <- intersect(input$mmm_median_cols, num_cols)
            rec <- recipe(~ ., data = train_pred)
            if (length(mean_cols)   > 0) rec <- rec |> step_impute_mean(all_of(mean_cols))
            if (length(median_cols) > 0) rec <- rec |> step_impute_median(all_of(median_cols))
            if (length(fac_cols)    > 0) rec <- rec |> step_impute_mode(all_of(fac_cols))
            set.seed(seed())
            trained     <- prep(rec, training = train_pred, verbose = FALSE)
            train_baked <- bake(trained, new_data = NULL)
            test_baked  <- if (!is.null(test_pred)) bake(trained, new_data = test_pred) else NULL
            
          } else if (algo == "knn") {
            library(recipes)
            y_var <- input$y_var
            if (!is.null(y_var) && y_var != "(none)" && y_var %in% names(train_df)) {
              train_with_y <- train_df[, c(pred_cols, y_var), drop = FALSE]
              test_with_y  <- if (!is.null(test_df)) test_df[, c(pred_cols, y_var), drop = FALSE] else NULL
              fml <- as.formula(paste(y_var, "~ ."))
            } else {
              train_with_y <- train_pred; test_with_y <- test_pred; fml <- ~ .
            }
            rec <- recipe(fml, data = train_with_y) |>
              step_impute_knn(all_predictors(), -has_type("date"), neighbors = input$knn_neighbors)
            set.seed(seed())
            trained     <- prep(rec, training = train_with_y, verbose = FALSE)
            train_baked <- bake(trained, new_data = NULL)
            test_baked  <- if (!is.null(test_with_y)) bake(trained, new_data = test_with_y) else NULL
            train_baked <- train_baked[, intersect(pred_cols, names(train_baked)), drop = FALSE]
            if (!is.null(test_baked))
              test_baked <- test_baked[, intersect(pred_cols, names(test_baked)), drop = FALSE]
            
          } else if (algo == "bag") {
            library(recipes)
            y_var <- input$y_var
            if (!is.null(y_var) && y_var != "(none)" && y_var %in% names(train_df)) {
              train_with_y <- train_df[, c(pred_cols, y_var), drop = FALSE]
              test_with_y  <- if (!is.null(test_df)) test_df[, c(pred_cols, y_var), drop = FALSE] else NULL
              fml <- as.formula(paste(y_var, "~ ."))
            } else {
              train_with_y <- train_pred; test_with_y <- test_pred; fml <- ~ .
            }
            rec <- recipe(fml, data = train_with_y) |>
              step_impute_bag(all_predictors(), -has_type("date"), trees = input$bag_trees)
            set.seed(seed())
            withProgress(message = paste0("Fitting Bag (", input$bag_trees, " trees)..."), value = 0.2, {
              trained <- prep(rec, training = train_with_y, verbose = FALSE)
              setProgress(0.8, message = "Applying to train/test...")
              train_baked <- bake(trained, new_data = NULL)
              test_baked  <- if (!is.null(test_with_y)) bake(trained, new_data = test_with_y) else NULL
              setProgress(1.0)
            })
            train_baked <- train_baked[, intersect(pred_cols, names(train_baked)), drop = FALSE]
            if (!is.null(test_baked))
              test_baked <- test_baked[, intersect(pred_cols, names(test_baked)), drop = FALSE]
          }
          
          rebuild <- function(original, baked, p_cols) {
            out <- original
            for (col in intersect(p_cols, names(baked))) out[[col]] <- baked[[col]]
            out
          }
          train_out <- rebuild(train_df, train_baked, pred_cols)
          test_out  <- if (!is.null(test_baked)) rebuild(test_df, test_baked, pred_cols) else NULL
          incProgress(0.7, message = "Finalising results...")
          elapsed   <- round(proc.time()[["elapsed"]] - start_time, 2)
          
          # KDE snapshots
          imputed_vals <- lapply(pred_cols, function(col) {
            was_na <- is.na(train_pred[[col]])
            if (sum(was_na) == 0 || !is.numeric(train_out[[col]])) return(NULL)
            train_out[[col]][was_na]
          })
          names(imputed_vals) <- pred_cols
          
          observed_vals <- lapply(pred_cols, function(col) {
            if (!is.numeric(train_pred[[col]])) return(NULL)
            train_pred[[col]][!is.na(train_pred[[col]])]
          })
          names(observed_vals) <- pred_cols
          
          res_obj <- list(
            train_out    = train_out, test_out = test_out,
            algo         = algo, elapsed = elapsed,
            pred_cols    = pred_cols, excl = excl,
            na_before    = list(train = sum(is.na(train_pred)),
                                test  = if (!is.null(test_pred)) sum(is.na(test_pred)) else NA),
            na_after     = list(train = sum(is.na(train_out[, pred_cols, drop = FALSE])),
                                test  = if (!is.null(test_out))
                                  sum(is.na(test_out[, pred_cols, drop = FALSE])) else NA),
            col_na_before = colSums(is.na(train_pred)),
            col_na_after  = colSums(is.na(train_out[, pred_cols, drop = FALSE])),
            params        = list(knn_neighbors = input$knn_neighbors,
                                 bag_trees     = input$bag_trees),
            imputed_vals  = imputed_vals,
            observed_vals = observed_vals
          )
          impute_result(res_obj)
          
          hist  <- run_history()
          run_n <- length(hist) + 1
          algo_label_h <- switch(algo,
                                 "knn" = paste0("KNN (k=", input$knn_neighbors, ")"),
                                 "bag" = paste0("Bag (", input$bag_trees, " trees)"),
                                 "mmm" = "MMM"
          )
          hist[[run_n]] <- list(
            run           = run_n, label = algo_label_h, algo = algo,
            imputed_vals  = imputed_vals, observed_vals = observed_vals,
            total_imputed = sum(is.na(train_pred)) - sum(is.na(train_out[, pred_cols, drop = FALSE])),
            elapsed       = elapsed
          )
          run_history(hist)
          
        }, error = function(e) {
          impute_result(list(error = conditionMessage(e)))
        })
        
        setProgress(1)
      }) # end withProgress
    })
    
    # ── Reset current run ─────────────────────────────────────────────────────
    
    observeEvent(input$reset, {
      impute_result(NULL)
      updateRadioButtons(session, "algorithm",    selected = "knn")
      updateSliderInput(session, "knn_neighbors",  value = 2)
      updateSliderInput(session, "bag_trees",      value = 4)
      r                 <- roles()
      vars              <- names(get_data())
      ignore_role_names <- c("sensitive", "weight", "stratifier", "ignore")
      auto_ignored      <- names(r)[tolower(as.character(r)) %in% ignore_role_names]
      updateSelectizeInput(session, "ignore_vars", choices = vars, selected = auto_ignored, server = TRUE)
    })
    
    # ── Clear comparison ──────────────────────────────────────────────────────
    
    observeEvent(input$reset_history, {
      run_history(list())
    })
    
    # ── Current Run UI ────────────────────────────────────────────────────────
    
    output$current_run_ui <- renderUI({
      res <- impute_result()
      
      if (is.null(res)) {
        return(make_card(
          div(style = "text-align:center; color:#6c757d; padding:30px 0;",
              icon("circle-info", style = "font-size:32px; margin-bottom:10px; color:#adb5bd;"),
              br(),
              tags$span("No imputation run yet.", style = "font-size:15px;"),
              br(),
              tags$span("Configure settings and click Run Imputation.",
                        style = "font-size:12px; color:#adb5bd;")
          )
        ))
      }
      
      if (!is.null(res$error)) {
        return(make_card(
          bg = "#fff5f5", border_color = "#f5c2c7",
          div(
            icon("circle-xmark", style = "color:#dc3545; font-size:20px;"),
            tags$span(" Imputation error", style = "font-weight:600; color:#dc3545;"),
            br(), br(),
            tags$code(res$error, style = "font-size:12px;")
          )
        ))
      }
      
      algo_label <- switch(res$algo,
                           "knn" = paste0("KNN (k = ", res$params$knn_neighbors, ")"),
                           "bag" = paste0("Bagged Trees (", res$params$bag_trees, " trees)"),
                           "mmm" = "Mean / Median / Mode"
      )
      total_imputed <- (res$na_before$train - res$na_after$train) +
        ifelse(!is.na(res$na_before$test), res$na_before$test - res$na_after$test, 0)
      cols_affected <- sum(res$col_na_before > 0)
      
      tagList(
        div(
          style = "display:flex; gap:12px; flex-wrap:wrap; margin-bottom:14px;",
          stat_box("Cells Imputed",   format(total_imputed, big.mark = ","), "fill-drip",     "#0d6efd"),
          stat_box("Cols Affected",   cols_affected,                         "table-columns", "#6610f2"),
          stat_box("Train NAs After", res$na_after$train,                    "check-circle",  "#198754"),
          stat_box("Test NAs After",
                   if (is.na(res$na_after$test)) "—" else res$na_after$test, "check-circle",  "#198754"),
          stat_box("Time (s)",        res$elapsed,                           "stopwatch",     "#fd7e14")
        ),
        make_card(
          tags$h6("Imputation Summary", style = "font-weight:600; margin-bottom:12px; color:#343a40;"),
          tags$table(
            class = "table table-sm table-bordered",
            style = "font-size:13px; margin-bottom:0;",
            tags$thead(tags$tr(
              tags$th(""), tags$th("NAs Before"), tags$th("NAs After"), tags$th("Imputed")
            )),
            tags$tbody(
              tags$tr(
                tags$td(tags$b("Train")),
                tags$td(format(res$na_before$train, big.mark = ",")),
                tags$td(res$na_after$train),
                tags$td(style = "color:#198754; font-weight:600;",
                        format(res$na_before$train - res$na_after$train, big.mark = ","))
              ),
              tags$tr(
                tags$td(tags$b("Test")),
                tags$td(if (is.na(res$na_before$test)) "—"
                        else format(res$na_before$test, big.mark = ",")),
                tags$td(if (is.na(res$na_after$test)) "—" else res$na_after$test),
                tags$td(style = "color:#198754; font-weight:600;",
                        if (is.na(res$na_before$test)) "—"
                        else format(res$na_before$test - res$na_after$test, big.mark = ","))
              )
            )
          )
        ),
        make_card(
          bg = "#f8f9fa",
          tags$h6("Settings Used", style = "font-weight:600; margin-bottom:10px; color:#343a40;"),
          div(style = "display:flex; gap:8px; flex-wrap:wrap; font-size:12px;",
              span(style = "background:#e7f0ff; color:#0d6efd; padding:3px 10px; border-radius:999px;",
                   icon("wand-magic-sparkles"), paste0(" ", algo_label)),
              span(style = "background:#e8f5e9; color:#198754; padding:3px 10px; border-radius:999px;",
                   icon("circle-check"), " Fitted on train only"),
              if (length(res$excl) > 0)
                span(style = "background:#fff3e0; color:#e65100; padding:3px 10px; border-radius:999px;",
                     icon("eye-slash"),
                     paste0(" Excluded: ", paste(res$excl, collapse = ", ")))
          )
        ),
        make_card(
          tags$h6(icon("table", style = "color:#185FA5; margin-right:6px;"),
                  "Per-Column Imputation Detail",
                  style = "font-weight:600; margin-bottom:12px; color:#343a40;"),
          DT::dataTableOutput(ns("col_tbl"))
        )
      )
    })
    
    # ── Comparison UI ─────────────────────────────────────────────────────────
    
    output$comparison_ui <- renderUI({
      hist <- run_history()
      if (length(hist) == 0) {
        return(make_card(
          div(style = "text-align:center; color:#6c757d; padding:30px 0;",
              icon("chart-area", style = "font-size:32px; margin-bottom:10px; color:#adb5bd;"),
              br(),
              tags$span("No runs recorded yet.", style = "font-size:15px;"),
              br(),
              tags$span("Run imputation at least once to compare.",
                        style = "font-size:12px; color:#adb5bd;")
          )
        ))
      }
      tagList(
        make_card(
          uiOutput(ns("kde_card_title")),
          div(style = "font-size:12px; color:#6c757d; margin-bottom:10px;",
              "Observed (grey) = non-missing train values. Coloured lines = imputed values per run. 
              Limitation: imputation may not recover the true distribution well when missingness is high (>30%)."),
          plotly::plotlyOutput(ns("kde_plot"), height = "680px")
        ),
        make_card(
          tags$h6(icon("clock-rotate-left", style = "color:#185FA5; margin-right:6px;"),
                  "Run History",
                  style = "font-weight:600; margin-bottom:12px; color:#343a40;"),
          DT::dataTableOutput(ns("history_tbl"))
        )
      )
    })
    
    # ── KDE column selectors observer ─────────────────────────────────────────
    
    observe({
      hist <- run_history()
      req(length(hist) > 0)
      all_cols <- unique(unlist(lapply(hist, function(h) {
        names(Filter(function(v) !is.null(v) && length(v) > 0, h$imputed_vals))
      })))
      all_cols <- all_cols[nzchar(all_cols)]
      if (length(all_cols) > 0) {
        latest <- hist[[length(hist)]]
        counts <- vapply(all_cols, function(col) {
          v <- latest$imputed_vals[[col]]; if (is.null(v)) 0L else length(v)
        }, integer(1))
        all_cols <- all_cols[order(-counts)]
      }
      gc  <- c("(none)", all_cols)
      sel <- function(i) if (length(all_cols) >= i) all_cols[i] else "(none)"
      updateSelectInput(session, "kde_col",
                        choices  = if (length(all_cols) > 0) all_cols else "(none)",
                        selected = sel(1))
      for (i in 1:4)  updateSelectInput(session, paste0("kde_col", i),  choices = gc, selected = sel(i))
      for (i in 5:13) updateSelectInput(session, paste0("kde_col", i),  choices = gc, selected = sel(i - 4))
      for (i in 14:29) updateSelectInput(session, paste0("kde_col", i), choices = gc, selected = sel(i - 13))
    })
    
    # ── KDE plot helper ───────────────────────────────────────────────────────
    
    make_kde_subplot <- function(hist, col, run_colours, show_legend = TRUE) {
      fig <- plotly::plot_ly()
      for (i in rev(seq_along(hist))) {
        obs <- hist[[i]]$observed_vals[[col]]
        if (!is.null(obs) && length(obs) >= 2) {
          d <- density(obs, na.rm = TRUE)
          fig <- plotly::add_trace(fig,
                                   x = d$x, y = d$y, type = "scatter", mode = "lines",
                                   fill = "tozeroy", fillcolor = "rgba(180,180,180,0.25)",
                                   line = list(color = "rgba(120,120,120,0.8)", width = 2),
                                   name = "<b>Observed</b>", legendgroup = "obs", showlegend = show_legend)
          break
        }
      }
      for (i in seq_along(hist)) {
        h   <- hist[[i]]
        imp <- h$imputed_vals[[col]]
        if (is.null(imp) || length(imp) < 2) next
        d   <- density(imp, na.rm = TRUE)
        clr <- run_colours[((i - 1) %% length(run_colours)) + 1]
        rgb_vals <- paste(col2rgb(clr), collapse = ",")
        fig <- plotly::add_trace(fig,
                                 x = d$x, y = d$y, type = "scatter", mode = "lines",
                                 fill = "tozeroy",
                                 fillcolor = paste0("rgba(", rgb_vals, ",0.15)"),
                                 line = list(color = clr, width = 2),
                                 name = paste0("<b>Run ", h$run, ": ", h$label, "</b>"),
                                 legendgroup = paste0("run", i), showlegend = show_legend)
      }
      plotly::layout(fig,
                     xaxis  = list(title = paste0('<b>', col, '</b>'), titlefont = list(size = 11)),
                     yaxis  = list(title = "Density", titlefont = list(size = 11)),
                     margin = list(t = 30, b = 40))
    }
    
    # ── KDE plot ──────────────────────────────────────────────────────────────
    
    output$kde_plot <- plotly::renderPlotly({
      hist <- run_history()
      req(length(hist) > 0)
      
      run_colours <- c("#1f77b4","#ff7f0e","#2ca02c","#d62728",
                       "#9467bd","#8c564b","#e377c2","#17becf")
      
      kde_mode <- if (is.null(input$kde_mode) || !nzchar(input$kde_mode)) "single" else input$kde_mode
      
      make_grid <- function(cols_input, n_cols) {
        cols <- cols_input[!is.null(cols_input) & cols_input != "(none)" & nzchar(cols_input)]
        req(length(cols) > 0)
        n_rows <- ceiling(length(cols) / n_cols)
        subplots <- lapply(seq_along(cols), function(i)
          make_kde_subplot(hist, cols[i], run_colours, show_legend = (i == 1))
        )
        n <- length(subplots)
        if (n == 1) return(
          plotly::layout(subplots[[1]],
                         legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.02, 
                                       yanchor = "bottom", font = list(size = 12)), hovermode = "x unified")
        )
        p <- do.call(plotly::subplot, c(subplots,
                                        list(nrows = n_rows, shareY = FALSE, titleX = TRUE, titleY = TRUE,
                                             margin = 0.06)))
        plotly::layout(p,
                       legend    = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.02, 
                                        yanchor = "bottom", font = list(size = 12)),
                       hovermode = "closest",
                       margin    = list(t = 20, b = 80)
        )
      }
      
      if (kde_mode == "single") {
        col <- input$kde_col
        req(nzchar(col), col != "(none)")
        make_kde_subplot(hist, col, run_colours, show_legend = TRUE) |>
          plotly::layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.02, 
                                       yanchor = "bottom", font = list(size = 12)), hovermode = "x unified")
        
      } else if (kde_mode == "grid2") {
        make_grid(c(input$kde_col1, input$kde_col2,
                    input$kde_col3, input$kde_col4), n_cols = 2)
        
      } else if (kde_mode == "grid3") {
        make_grid(c(input$kde_col5,  input$kde_col6,  input$kde_col7,
                    input$kde_col8,  input$kde_col9,  input$kde_col10,
                    input$kde_col11, input$kde_col12, input$kde_col13), n_cols = 3)
        
      } else {
        make_grid(c(input$kde_col14, input$kde_col15, input$kde_col16, input$kde_col17,
                    input$kde_col18, input$kde_col19, input$kde_col20, input$kde_col21,
                    input$kde_col22, input$kde_col23, input$kde_col24, input$kde_col25,
                    input$kde_col26, input$kde_col27, input$kde_col28, input$kde_col29), n_cols = 4)
      }
    })
    
    # ── Run history DT ────────────────────────────────────────────────────────
    
    output$history_tbl <- DT::renderDataTable({
      hist <- run_history()
      req(length(hist) > 0)
      tbl <- do.call(rbind, lapply(hist, function(h) {
        data.frame(Run = h$run, Algorithm = h$label,
                   Cells_Imputed = h$total_imputed, Time_s = h$elapsed,
                   stringsAsFactors = FALSE)
      }))
      DT::datatable(tbl, options = list(pageLength = 10, dom = "tip"), rownames = FALSE) |>
        DT::formatStyle("Run", fontWeight = "bold")
    })
    
    # ── Per-column DT ─────────────────────────────────────────────────────────
    
    output$col_tbl <- DT::renderDataTable({
      res <- impute_result()
      req(res, is.null(res$error))
      tbl <- data.frame(
        Column         = names(res$col_na_before),
        Type           = sapply(get_data()[, names(res$col_na_before), drop = FALSE],
                                function(x) class(x)[1]),
        Missing_Before = as.integer(res$col_na_before),
        Missing_After  = as.integer(res$col_na_after),
        Imputed        = as.integer(res$col_na_before - res$col_na_after),
        stringsAsFactors = FALSE
      )
      tbl <- tbl[order(-tbl$Imputed), ]
      DT::datatable(tbl, options = list(pageLength = 15, dom = "tip"), rownames = FALSE) |>
        DT::formatStyle("Imputed",
                        color      = DT::styleInterval(0, c("#adb5bd", "#185FA5")),
                        fontWeight = "bold")
    })
    
    # ── KDE card title ────────────────────────────────────────────────────────
    
    output$kde_card_title <- renderUI({
      hist <- run_history()
      
      # build default title from latest run config
      default_title <- if (length(hist) == 0) {
        "Imputed Value Distribution (KDE)"
      } else {
        latest <- hist[[length(hist)]]
        algo_str <- switch(latest$algo,
                           "knn" = paste0("KNN (k=", latest$label, ")"),
                           "bag" = paste0("Bag (", latest$label, ")"),
                           "mmm" = "Mean/Median/Mode",
                           latest$label
        )
        paste0("KDE — ", length(hist), " run", if (length(hist) > 1) "s" else "",
               " | Latest: ", latest$label)
      }
      
      title_text <- if (nzchar(trimws(input$custom_title))) input$custom_title else default_title
      
      tags$h6(
        icon("chart-area", style = "color:#185FA5; margin-right:6px;"),
        title_text,
        style = "font-weight:600; margin-bottom:12px; color:#343a40;"
      )
    })
    
    # ── outputOptions (must come after all output definitions) ───────────────
    
    outputOptions(output, "kde_plot",        suspendWhenHidden = FALSE)
    outputOptions(output, "comparison_ui",   suspendWhenHidden = FALSE)
    outputOptions(output, "history_tbl",     suspendWhenHidden = FALSE)
    outputOptions(output, "kde_card_title",  suspendWhenHidden = FALSE)
    
    # ── Return ────────────────────────────────────────────────────────────────
    
    return(list(
      data = reactive({
        res <- impute_result()
        if (is.null(res) || !is.null(res$error)) return(get_data())
        df  <- get_data()
        sv  <- input$split_var
        tl  <- input$train_level
        ts  <- input$test_level
        out <- df
        if (!is.null(sv) && sv != "(none)" && !is.null(tl) && tl != "(none)") {
          train_rows <- which(as.character(df[[sv]]) == tl)
          for (col in intersect(names(res$train_out), names(out)))
            if (length(train_rows) == nrow(res$train_out))
              out[train_rows, col] <- res$train_out[[col]]
        }
        if (!is.null(res$test_out) && !is.null(ts) && ts != "(none)") {
          test_rows <- which(as.character(df[[sv]]) == ts)
          for (col in intersect(names(res$test_out), names(out)))
            if (length(test_rows) == nrow(res$test_out))
              out[test_rows, col] <- res$test_out[[col]]
        }
        out
      })
    ))
    
  })
}