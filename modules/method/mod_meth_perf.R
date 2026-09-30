# =================================================================================
# mod_meth_perf.R
# =================================================================================
# Receives:  get_data   — reactive returning the full modelling dataset
#            roles      — reactive returning named role vector
#            get_models — reactive returning named list of all trained caret models
#            choice     — reactive returning the selected model name (from meth_select)
# Returns:   nothing (display only)


# ── UI ───────────────────────────────────────────────────────────────────────

meth_perf_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 3,
      style = "background-color:#e8f0fe; border-left:2px solid #a8c0fd;
               min-height:100vh; padding:15px; font-size:13px;",

      div(
        style = "background:white; padding:10px; border-left:4px solid #0d6efd;
                 border-radius:6px; margin-bottom:12px; font-size:13px; color:#343a40;
                 box-shadow:0 1px 2px rgba(0,0,0,0.05);",
        icon("circle-info", style = "color:#0d6efd;"),
        HTML("&nbsp;<b>Performance</b><br><br>
              Evaluates the chosen model on <b>unseen test data</b>.<br><br>
              Metrics use <code>caret::defaultSummary()</code> — RMSE, Rsquared, MAE.<br><br>
              Residual boxplots share the same y-axis range so train and test
              are directly comparable.")
      ),

      # ── Selected model ────────────────────────────────────────────────────
      .section_hdr("Selected Model", "#d1ecf1", "#17a2b8"),
      uiOutput(ns("model_badge")),

      hr(),

      # ── Residual plot controls ────────────────────────────────────────────
      .section_hdr("Residual Plot", "#e9ecef", "#6c757d"),

      tags$label("Outlier label column:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      selectInput(ns("label_var"), NULL, choices = NULL, width = "100%"),
      helpText("Variable whose values are shown as labels on outlier points.
               Choose your patient / ID column."),

      hr(),

      tags$label("IQR multiplier:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("iqr_mult"), NULL,
                  min = 0, max = 5, value = 2.3, step = 0.1, width = "100%"),
      helpText("Controls which residuals are flagged as outliers.
               1.5 = standard Tukey rule."),

      hr(),

      tags$label("Outlier label size:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("label_size"), NULL,
                  min = 2, max = 8, value = 3.5, step = 0.5, width = "100%")
    ),

    mainPanel(
      width = 9,
      uiOutput(ns("status_ui")),

      # ── Test metrics ──────────────────────────────────────────────────────
      uiOutput(ns("title_ui")),
      verbatimTextOutput(ns("test_summary")),
      hr(),

      # ── Plots ─────────────────────────────────────────────────────────────
      fluidRow(
        column(5,
               tags$h6("Predicted vs Observed (test)",
                        style = "font-weight:700; color:#343a40; margin-bottom:6px;"),
               plotOutput(ns("test_plot"), height = "500px")
        ),
        column(3,
               tags$h6("Test residuals",
                        style = "font-weight:700; color:#343a40; margin-bottom:6px;"),
               plotOutput(ns("test_residuals"), height = "500px")
        ),
        column(3,
               tags$h6("Train residuals",
                        style = "font-weight:700; color:#343a40; margin-bottom:6px;"),
               plotOutput(ns("train_residuals"), height = "500px")
        )
      )
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

meth_perf_server <- function(id, get_data, roles, get_models, choice) {
  moduleServer(id, function(input, output, session) {

    # ── Populate label_var selector ───────────────────────────────────────────

    observe({
      df <- get_data(); req(df)
      r  <- roles()
      id_col <- names(r)[r == "obs_id"]
      updateSelectInput(session, "label_var",
                        choices  = names(df),
                        selected = if (length(id_col) > 0) id_col[1] else names(df)[1])
    })

    # ── Split helpers ─────────────────────────────────────────────────────────

    get_train <- reactive({
      df <- get_data(); req(df)
      r  <- roles()
      sc <- names(r)[r == "split"]
      if (length(sc) == 0) return(df)
      df[as.character(df[[sc[1]]]) %in% c("Train", "train"), , drop = FALSE]
    })

    get_test <- reactive({
      df <- get_data(); req(df)
      r  <- roles()
      sc <- names(r)[r == "split"]
      if (length(sc) == 0) return(df)
      df[as.character(df[[sc[1]]]) %in% c("Test", "test", "Validation", "val"), , drop = FALSE]
    })

    outcome_col <- reactive({
      r  <- roles()
      oc <- names(r)[r == "outcome"]
      if (length(oc) == 0) return(NULL)
      oc[1]
    })

    # ── Get selected model ────────────────────────────────────────────────────

    get_model <- reactive({
      mod_name <- choice(); req(mod_name, nchar(mod_name) > 0)
      get_models()[[mod_name]]
    })

    # ── Prediction results ────────────────────────────────────────────────────

    get_test_results <- reactive({
      dat <- get_test();  req(dat, nrow(dat) > 0)
      mod <- get_model(); req(mod)
      oc  <- outcome_col(); req(oc)
      preds <- tryCatch(as.numeric(predict(mod, newdata = dat)), error = function(e) e)
      validate(need(!inherits(preds, "error"),
                    paste("Could not predict with this model on the current data:",
                          if (inherits(preds, "error")) conditionMessage(preds) else "")))
      data.frame(obs = dat[[oc]], pred = preds, row.names = rownames(dat))
    })

    get_train_results <- reactive({
      dat <- get_train(); req(dat, nrow(dat) > 0)
      mod <- get_model(); req(mod)
      oc  <- outcome_col(); req(oc)
      preds <- tryCatch(as.numeric(predict(mod, newdata = dat)), error = function(e) e)
      validate(need(!inherits(preds, "error"),
                    paste("Could not predict with this model on the current data:",
                          if (inherits(preds, "error")) conditionMessage(preds) else "")))
      data.frame(obs = dat[[oc]], pred = preds, row.names = rownames(dat))
    })

    # ── Shared residual range ─────────────────────────────────────────────────

    get_residual_range <- reactive({
      r1 <- get_train_results(); r1$res <- r1$obs - r1$pred
      r2 <- get_test_results();  r2$res <- r2$obs - r2$pred
      range(c(r1$res, r2$res), na.rm = TRUE)
    })

    # ── Status ────────────────────────────────────────────────────────────────

    output$status_ui <- renderUI({
      mod_name <- choice()
      if (is.null(mod_name) || nchar(mod_name) == 0) {
        div(
          style = "background:#fff3e0; border-left:4px solid #ffc107;
                   border-radius:6px; padding:12px 16px; margin-bottom:12px;
                   font-size:13px; color:#856404;",
          icon("triangle-exclamation", style = "color:#ffc107;"),
          HTML("&nbsp; No model selected. Go to <b>Model Selection</b> and choose a model.")
        )
      } else NULL
    })

    # ── Title ─────────────────────────────────────────────────────────────────

    output$title_ui <- renderUI({
      mod_name <- choice(); req(mod_name, nchar(mod_name) > 0)
      tags$h5(paste("Unseen data results for chosen model:", mod_name),
              style = "font-weight:700; color:#343a40; margin-bottom:8px;")
    })

    # ── Model badge ───────────────────────────────────────────────────────────

    output$model_badge <- renderUI({
      mod_name <- choice()
      if (is.null(mod_name) || nchar(mod_name) == 0) {
        tags$span(style = "color:#adb5bd; font-size:12px;", "None selected")
      } else {
        div(
          style = "background:#e7f0ff; color:#0d6efd; padding:5px 12px;
                   border-radius:999px; font-size:13px; font-weight:600;
                   display:inline-block; margin-top:2px;",
          icon("trophy", style = "color:#0d6efd;"),
          paste0(" ", mod_name)
        )
      }
    })

    # ── Test summary ──────────────────────────────────────────────────────────

    output$test_summary <- renderPrint({
      caret::defaultSummary(get_test_results())
    })

    # ── Predicted vs Observed (template style: base R par + abline) ───────────

    output$test_plot <- renderPlot({
      d   <- get_test_results()
      rng <- range(c(d$obs, d$pred), na.rm = TRUE)
      par(pty = "s",
          cex.axis = 1.3,   # larger axis tick labels
          cex.lab  = 1.3,   # larger axis titles
          font.lab = 2)     # bold axis titles
      plot(d$obs, d$pred,
           xlim = rng, ylim = rng,
           xlab = "Observed", ylab = "Predicted",
           main = "Predicted versus Observed for test data",
           pch  = 16, col = "#333333")
      abline(a = 0, b = 1, col = "blue", lty = 2, lwd = 3)
    })

    # ── Residual boxplot helper (template style + label var + bold + larger text)

    .residual_boxplot <- function(results_df, raw_df, title) {
      d           <- results_df
      d$residuals <- d$obs - d$pred
      coef        <- input$iqr_mult
      y_range     <- get_residual_range()
      limits      <- boxplot.stats(x = d$residuals, coef = coef)$stats

      # build labels from chosen column, aligned by rownames
      lv <- input$label_var
      if (!is.null(lv) && lv %in% names(raw_df)) {
        lv_vals <- as.character(raw_df[[lv]])[match(rownames(d), rownames(raw_df))]
      } else {
        lv_vals <- rownames(d)
      }
      label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], lv_vals, NA)

      ggplot2::ggplot(d, ggplot2::aes(y = residuals, x = 0)) +
        ggplot2::ylim(y_range[1], y_range[2]) +
        ggplot2::geom_boxplot(coef = coef, orientation = "vertical") +
        ggrepel::geom_text_repel(ggplot2::aes(label = label),
                                 fontface     = "bold",
                                 max.overlaps = 50,
                                 size         = input$label_size,
                                 na.rm        = TRUE) +
        ggplot2::labs(title    = title,
                      subtitle = paste(coef, "IQR Multiplier")) +
        ggplot2::theme(
          axis.title.x  = ggplot2::element_blank(),
          axis.text.x   = ggplot2::element_blank(),
          axis.ticks.x  = ggplot2::element_blank(),
          axis.text.y   = ggplot2::element_text(size = 13),
          axis.title.y  = ggplot2::element_text(size = 13, face = "bold"),
          plot.title    = ggplot2::element_text(size = 14, face = "bold"),
          plot.subtitle = ggplot2::element_text(size = 11, colour = "#6c757d")
        )
    }

    # ── Test residuals ────────────────────────────────────────────────────────

    output$test_residuals <- renderPlot({
      .residual_boxplot(get_test_results(), get_test(), "Test-Residual Boxplot")
    })

    # ── Train residuals ───────────────────────────────────────────────────────

    output$train_residuals <- renderPlot({
      .residual_boxplot(get_train_results(), get_train(), "Train-Residual Boxplot")
    })

  })
}
