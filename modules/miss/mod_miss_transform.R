# =================================================================================
# mod_miss_transform.R
# =================================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

miss_transform_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 3,
      style = "background-color: #e8f0fe; border-left: 2px solid #a8c0fd;
               min-height: 100vh; padding-left: 20px;",
      
      div(
        style = "font-size: 13px; color: #343a40; background-color: white;
                 padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
                 margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; <b>Transformation</b><br><br>
              Statistics are <b>fitted on train only</b> and applied to both
              train and test splits. This prevents data leakage.")
      ),
      hr(),
      
      # ── Split variable ────────────────────────────────────────────────────
      tags$div(
        style = "margin-bottom: 4px;",
        tags$label("Split variable:",
                   style = "font-weight:600; font-size:13px; color:#343a40;"),
        selectInput(ns("split_var"), label = NULL, choices = c("(none)"), width = "100%")
      ),
      
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
      hr(),
      
      # ── Column selector ───────────────────────────────────────────────────
      tags$div(
        style = "margin-bottom: 10px;",
        tags$label("Columns to transform:",
                   style = "font-weight:600; font-size:13px; color:#343a40;"),
        div(style = "font-size:11px; color:#6c757d; margin-bottom:4px;",
            "Auto-filled: all numeric predictors. Outcome/split/ignore excluded."),
        selectizeInput(ns("transform_cols"), label = NULL,
                       choices  = NULL,
                       multiple = TRUE,
                       options  = list(placeholder = "Select numeric columns..."),
                       width    = "100%")
      ),
      hr(),
      
      # ── Method ────────────────────────────────────────────────────────────
      tags$label("Method:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      radioButtons(
        ns("method"),
        label    = NULL,
        choices  = c(
          "(none)"                                = "none",
          "Standardise — (x - mean) / sd"        = "standardise",
          "Normalise — (x - min) / (max - min)"  = "normalise",
          "Centre only — x - mean"               = "centre",
          "Scale only — x / sd"                  = "scale"
        ),
        selected = "none"
      ),
      hr(),
      
      # ── Shift threshold ───────────────────────────────────────────────────
      tags$label("Shift warning threshold:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      div(style = "font-size:11px; color:#6c757d; margin-bottom:4px;",
          "Flag columns where test mean or SD deviates by more than this % from train."),
      sliderInput(ns("shift_thresh"), label = NULL,
                  min = 5, max = 100, value = 20, step = 5,
                  post = "%", width = "100%"),
      hr(),
      
      actionButton(ns("reset"), label = "Reset", icon = icon("rotate-left"), width = "100%")
    ),
    
    mainPanel(
      width = 9,
      uiOutput(ns("main_ui"))
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

miss_transform_server <- function(id, get_data, roles, seed = reactive(42)) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── Helpers ──────────────────────────────────────────────────────────────
    
    make_card <- function(label, value, color) {
      tags$div(
        style = paste0(
          "flex:1; min-width:130px; background:white; border-radius:10px;",
          "border:0.5px solid #dee2e6; padding:14px 18px;",
          "box-shadow:0 1px 3px rgba(0,0,0,0.06);"
        ),
        tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                          text-transform:uppercase; letter-spacing:.5px;", label),
        tags$div(style = paste0("font-size:28px; font-weight:700; color:", color, ";"), value)
      )
    }
    
    # ── Populate split selector from roles ────────────────────────────────────
    
    observe({
      req(get_data(), roles())
      r    <- roles()
      vars <- names(get_data())
      updateSelectInput(session, "split_var",
                        choices  = c("(none)", vars),
                        selected = { v <- names(r)[r == "split"]; if (length(v)) v[1] else "(none)" })
    })
    
    # ── Train / Test level selectors ──────────────────────────────────────────
    
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
    
    # ── Auto-populate columns from roles ──────────────────────────────────────
    
    observe({
      req(get_data())
      df <- get_data()
      r  <- if (!is.null(roles)) roles() else NULL
      excl <- character(0)
      if (!is.null(r))
        excl <- names(r)[tolower(as.character(r)) %in%
                           c("outcome", "split", "obs_id", "sensitive",
                             "weight", "stratifier", "ignore")]
      num_cols <- setdiff(names(df)[sapply(df, is.numeric)], excl)
      updateSelectizeInput(session, "transform_cols",
                           choices  = num_cols,
                           selected = num_cols,
                           server   = TRUE)
    })
    
    # ── Reset ─────────────────────────────────────────────────────────────────
    
    observeEvent(input$reset, {
      updateRadioButtons(session, "method", selected = "none")
      updateSelectInput(session, "split_var", selected = "(none)")
      df <- get_data(); req(df)
      r  <- if (!is.null(roles)) roles() else NULL
      excl <- character(0)
      if (!is.null(r))
        excl <- names(r)[tolower(as.character(r)) %in%
                           c("outcome", "split", "obs_id", "sensitive",
                             "weight", "stratifier", "ignore")]
      num_cols <- setdiff(names(df)[sapply(df, is.numeric)], excl)
      updateSelectizeInput(session, "transform_cols",
                           choices  = num_cols,
                           selected = num_cols,
                           server   = TRUE)
    })
    
    # ── Core transform reactive ───────────────────────────────────────────────
    
    result <- reactive({
      df     <- get_data(); req(df)
      method <- input$method
      
      # passthrough when no method selected
      if (is.null(method) || method == "none")
        return(list(out_df  = df,
                    col_stats = data.frame(),
                    cols    = character(0),
                    method  = "none",
                    n_train = nrow(df),
                    n_test  = NA,
                    n_shift = 0))
      
      cols <- input$transform_cols
      req(length(cols) > 0)
      
      sv        <- input$split_var
      tl        <- input$train_level
      ts        <- input$test_level
      splits_ok <- !is.null(sv) && sv != "(none)" &&
        !is.null(tl) && tl != "(none)" &&
        !is.null(ts) && ts != "(none)"
      
      train_df <- if (splits_ok) df[as.character(df[[sv]]) == tl, , drop = FALSE] else df
      test_df  <- if (splits_ok) df[as.character(df[[sv]]) == ts, , drop = FALSE] else NULL
      
      cols <- intersect(cols, names(train_df)[sapply(train_df, is.numeric)])
      req(length(cols) > 0)
      
      library(recipes)
      rec <- recipe(~ ., data = train_df[, cols, drop = FALSE])
      rec <- switch(method,
                    "standardise" = rec |> step_normalize(all_of(cols)),
                    "normalise"   = rec |> step_range(all_of(cols)),
                    "centre"      = rec |> step_center(all_of(cols)),
                    "scale"       = rec |> step_scale(all_of(cols))
      )
      
      set.seed(seed())
      trained     <- prep(rec, training = train_df[, cols, drop = FALSE], verbose = FALSE)
      train_baked <- bake(trained, new_data = NULL)
      test_baked  <- if (!is.null(test_df))
        bake(trained, new_data = test_df[, cols, drop = FALSE]) else NULL
      
      train_out <- train_df
      for (col in cols) train_out[[col]] <- train_baked[[col]]
      
      test_out <- test_df
      if (!is.null(test_df) && !is.null(test_baked))
        for (col in cols) test_out[[col]] <- test_baked[[col]]
      
      col_stats <- do.call(rbind, lapply(cols, function(col) {
        tb  <- train_df[[col]]
        ta  <- train_out[[col]]
        tea <- if (!is.null(test_out)) test_out[[col]] else NULL
        data.frame(
          Column            = col,
          Train_Mean_Before = round(mean(tb, na.rm = TRUE), 4),
          Train_Mean_After  = round(mean(ta, na.rm = TRUE), 4),
          Train_SD_Before   = round(sd(tb,   na.rm = TRUE), 4),
          Train_SD_After    = round(sd(ta,   na.rm = TRUE), 4),
          Train_Min_Before  = round(min(tb,  na.rm = TRUE), 4),
          Train_Min_After   = round(min(ta,  na.rm = TRUE), 4),
          Train_Max_Before  = round(max(tb,  na.rm = TRUE), 4),
          Train_Max_After   = round(max(ta,  na.rm = TRUE), 4),
          Test_Mean_After   = if (!is.null(tea)) round(mean(tea, na.rm = TRUE), 4) else NA,
          Test_SD_After     = if (!is.null(tea)) round(sd(tea,   na.rm = TRUE), 4) else NA,
          stringsAsFactors  = FALSE
        )
      }))
      
      col_stats$Shift_Warning <- FALSE
      if (!is.null(test_df)) {
        mean_dev <- abs(col_stats$Test_Mean_After - col_stats$Train_Mean_After)
        sd_dev   <- abs(col_stats$Test_SD_After   - col_stats$Train_SD_After)
        rel_mean <- ifelse(abs(col_stats$Train_Mean_After) > 1e-6,
                           mean_dev / abs(col_stats$Train_Mean_After), 0)
        rel_sd   <- ifelse(abs(col_stats$Train_SD_After) > 1e-6,
                           sd_dev   / abs(col_stats$Train_SD_After), 0)
        col_stats$Shift_Warning <- (rel_mean > input$shift_thresh / 100 |
                                      rel_sd   > input$shift_thresh / 100)
      }
      
      out_df <- df
      if (splits_ok) {
        train_rows <- which(as.character(df[[sv]]) == tl)
        test_rows  <- which(as.character(df[[sv]]) == ts)
        for (col in cols) {
          if (length(train_rows) == nrow(train_out))
            out_df[train_rows, col] <- train_out[[col]]
          if (!is.null(test_out) && length(test_rows) == nrow(test_out))
            out_df[test_rows, col] <- test_out[[col]]
        }
      } else {
        for (col in cols) out_df[[col]] <- train_out[[col]]
      }
      
      list(
        out_df    = out_df,
        col_stats = col_stats,
        cols      = cols,
        method    = method,
        n_train   = nrow(train_df),
        n_test    = if (!is.null(test_df)) nrow(test_df) else NA,
        n_shift   = sum(col_stats$Shift_Warning)
      )
    })
    
    # ── Main UI ───────────────────────────────────────────────────────────────
    
    output$main_ui <- renderUI({
      res <- result()
      
      # no method selected — show placeholder
      if (is.null(res) || res$method == "none") {
        return(div(
          style = "text-align:center; color:#6c757d; padding:60px 0;",
          icon("circle-info", style = "font-size:32px; color:#adb5bd; margin-bottom:10px;"),
          br(),
          tags$span("Select a transformation method on the right to begin.",
                    style = "font-size:15px;")
        ))
      }
      
      method_label <- switch(res$method,
                             "standardise" = "Standardise \u2014 (x \u2212 mean) / sd",
                             "normalise"   = "Normalise \u2014 (x \u2212 min) / (max \u2212 min)",
                             "centre"      = "Centre only \u2014 x \u2212 mean",
                             "scale"       = "Scale only \u2014 x / sd"
      )
      
      cards <- tags$div(
        style = "display:flex; gap:12px; margin-bottom:20px; flex-wrap:wrap;",
        make_card("Cols Transformed", length(res$cols),  "#185FA5"),
        make_card("Train Rows",       res$n_train,       "#0F6E56"),
        make_card("Test Rows",        if (is.na(res$n_test)) "\u2014" else res$n_test, "#534AB7"),
        make_card("Shift Warnings",   res$n_shift,       if (res$n_shift > 0) "#C41E3A" else "#adb5bd")
      )
      
      method_badge <- tags$div(
        style = "background:#f8f9fa; border:1px solid #dee2e6; border-radius:10px;
                 padding:12px 18px; margin-bottom:16px; font-size:13px;",
        icon("wand-magic-sparkles", style = "color:#185FA5;"),
        tags$span(style = "font-weight:600; color:#185FA5; margin-left:6px;", method_label),
        tags$span(style = "color:#6c757d; margin-left:10px;",
                  "\u2014 fitted on train only, applied to train and test")
      )
      
      shift_banner <- if (res$n_shift > 0) {
        flagged <- res$col_stats$Column[res$col_stats$Shift_Warning]
        tags$div(
          style = "background:#fff3cd; border:1px solid #ffc107; border-radius:8px;
                   padding:10px 14px; margin-bottom:16px; font-size:13px; color:#856404;",
          icon("triangle-exclamation", style = "color:#ffc107;"),
          HTML(paste0(
            " <strong>Distribution shift detected</strong> in ",
            res$n_shift, " column(s): <strong>",
            paste(flagged, collapse = ", "),
            "</strong>. Test mean or SD deviates more than threshold from train."
          ))
        )
      } else NULL
      
      tbl_section <- tags$div(
        style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                 padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
        tags$h5(icon("table", style = "color:#185FA5; margin-right:6px;"),
                "Per-Column Statistics",
                style = "font-weight:600; color:#343a40; margin-bottom:4px;"),
        tags$p("Train stats are the learned values used for transformation. Test stats show how well they generalise.",
               style = "font-size:12px; color:#6c757d; margin-bottom:12px;"),
        DT::dataTableOutput(ns("col_stats_tbl"))
      )
      
      tagList(cards, method_badge, shift_banner, tbl_section)
    })
    
    # ── Per-column DT ─────────────────────────────────────────────────────────
    
    output$col_stats_tbl <- DT::renderDataTable({
      res <- result()
      req(res, res$method != "none", nrow(res$col_stats) > 0)
      tbl <- res$col_stats
      
      display_tbl <- data.frame(
        Column           = tbl$Column,
        `Train Mean (B)` = tbl$Train_Mean_Before,
        `Train Mean (A)` = tbl$Train_Mean_After,
        `Train SD (B)`   = tbl$Train_SD_Before,
        `Train SD (A)`   = tbl$Train_SD_After,
        `Train Min (B)`  = tbl$Train_Min_Before,
        `Train Min (A)`  = tbl$Train_Min_After,
        `Train Max (B)`  = tbl$Train_Max_Before,
        `Train Max (A)`  = tbl$Train_Max_After,
        `Test Mean (A)`  = ifelse(is.na(tbl$Test_Mean_After), "\u2014", as.character(tbl$Test_Mean_After)),
        `Test SD (A)`    = ifelse(is.na(tbl$Test_SD_After),   "\u2014", as.character(tbl$Test_SD_After)),
        `Shift?`         = ifelse(tbl$Shift_Warning, "\u26a0 Yes", ""),
        stringsAsFactors = FALSE,
        check.names      = FALSE
      )
      
      DT::datatable(display_tbl,
                    options  = list(pageLength = 15, scrollX = TRUE, dom = "tip"),
                    rownames = FALSE) |>
        DT::formatStyle("Shift?",
                        color      = DT::styleEqual(c("\u26a0 Yes", ""), c("#C41E3A", "#adb5bd")),
                        fontWeight = "bold") |>
        DT::formatStyle("Train Mean (A)", color = "#185FA5", fontWeight = "bold") |>
        DT::formatStyle("Train SD (A)",   color = "#185FA5", fontWeight = "bold")
    })
    
    # ── Return ────────────────────────────────────────────────────────────────
    
    return(list(
      data = reactive({
        res <- result()
        if (is.null(res)) return(get_data())
        res$out_df
      })
    ))
    
  })
}