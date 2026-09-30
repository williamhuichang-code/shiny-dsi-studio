# =================================================================================
# mod_out_boxplot.R
# =================================================================================

out_boxplot_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 3,
      style = "background-color:#e8f0fe; border-left:2px solid #a8c0fd;
               min-height:100vh; padding-left:20px;",
      div(
        style = "font-size:13px; color:#343a40; background:white; padding:10px;
                 border-left:4px solid #0d6efd; border-radius:6px; margin-bottom:12px;
                 box-shadow:0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; <b>Box Plot</b><br><br>
              Univariate outlier detection using IQR multiplier.
              Points beyond the whiskers are flagged as outliers.")
      ),
      hr(),
      selectInput(ns("var"), "Variable:", choices = NULL),
      hr(),
      selectInput(ns("label_col"), "Label outliers by:", choices = NULL),
      hr(),
      numericInput(ns("iqr_k"), "IQR multiplier (k):",
                   value = 1.5, min = 0.1, step = 0.5),
      hr(),
      checkboxInput(ns("tolerate_na"),
                    "Tolerate NAs (exclude NA rows for this variable only)",
                    value = TRUE),
      hr(),
      radioButtons(ns("transform"), "Power transform (visual only):",
                   choices  = c("None"        = "none",
                                "Box-Cox"     = "boxcox",
                                "Yeo-Johnson" = "yeojohnson"),
                   selected = "none"),
      conditionalPanel(
        condition = sprintf("input['%s'] != 'none'", ns("transform")),
        div(
          style = "font-size:12px; color:#856404; background:#fff3cd;
                   border-left:3px solid #ffc107; padding:6px 10px;
                   border-radius:4px; margin-top:4px;",
          icon("triangle-exclamation", style = "color:#ffc107;"),
          HTML(" Transform applied for display only — the dataset is not changed.<br><br>
                Box-Cox requires all values > 0.")
        )
      )
    ),
    mainPanel(
      width = 9,
      plotOutput(ns("plot"), height = "40vh"),
      hr(),
      uiOutput(ns("hint_ui")),
      hr(),
      h5("Outlier Rows"),
      DT::dataTableOutput(ns("table"))
    )
  )
}

out_boxplot_server <- function(id, get_data, get_raw, roles) {
  moduleServer(id, function(input, output, session) {
    
    # ── helpers ───────────────────────────────────────────────────────────────
    apply_transform <- function(x, method) {
      if (method == "none") return(x)
      tryCatch({
        if (method == "boxcox") {
          if (any(x <= 0, na.rm = TRUE)) stop("Box-Cox requires all values > 0")
          est <- MASS::boxcox(x ~ 1, plotit = FALSE)
          lam <- est$x[which.max(est$y)]
          if (abs(lam) < 1e-6) log(x) else (x^lam - 1) / lam
        } else {
          obj <- bestNormalize::yeojohnson(x, standardize = FALSE)
          predict(obj)
        }
      }, error = function(e) { warning("Transform failed: ", conditionMessage(e)); x })
    }
    
    get_df_filtered <- reactive({
      req(input$var)
      df <- get_data()
      if (isTRUE(input$tolerate_na))
        df <- df[!is.na(df[[input$var]]), , drop = FALSE]
      df
    })
    
    # ── populate selectors ────────────────────────────────────────────────────
    observe({
      df       <- get_data(); req(df)
      r        <- roles()
      id_col   <- names(r)[r == "obs_id"]
      num_cols <- names(df)[sapply(df, is.numeric)]
      updateSelectInput(session, "var", choices = num_cols,
                        selected = if (length(num_cols) > 0) num_cols[1] else NULL)
      updateSelectInput(session, "label_col",
                        choices  = names(df),
                        selected = if (length(id_col) > 0) id_col[1] else names(df)[1])
    })
    
    # ── plot ──────────────────────────────────────────────────────────────────
    output$plot <- renderPlot({
      req(input$var, input$label_col, input$iqr_k)
      df  <- get_df_filtered()
      var <- input$var
      
      x_plot <- apply_transform(df[[var]], input$transform)
      var_label <- if (input$transform != "none")
        paste0(var, " [", input$transform, "]") else var
      
      df_plot        <- df
      df_plot[[var]] <- x_plot
      names(df_plot)[names(df_plot) == var] <- var_label
      
      k      <- input$iqr_k
      x      <- df_plot[[var_label]]
      limits <- boxplot.stats(x = x, coef = k)$stats
      df_plot$label <- ifelse(
        x < limits[1] | x > limits[5],
        as.character(df_plot[[input$label_col]]),
        NA_character_
      )
      
      ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[var_label]], y = 1)) +
        ggplot2::geom_boxplot(coef = k, outlier.colour = "#C41E3A") +
        ggrepel::geom_text_repel(ggplot2::aes(label = label),
                                 max.overlaps = 20, na.rm = TRUE) +
        ggplot2::labs(title = paste("Boxplot of", var_label,
                                    "at IQR multiplier k =", k),
                      x = var_label) +
        ggplot2::theme_minimal(base_size = 16) +
        ggplot2::theme(
          plot.title   = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
          axis.title.y = ggplot2::element_blank(),
          axis.text.y  = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank())
    })
    
    # ── interpretation hint ───────────────────────────────────────────────────
    output$hint_ui <- renderUI({
      req(input$var, input$iqr_k)
      df <- get_df_filtered()
      x  <- df[[input$var]]
      n  <- length(x)
      if (n < 3 || anyNA(x)) return(NULL)
      
      sk  <- moments::skewness(x)
      kur <- moments::kurtosis(x) - 3
      sw  <- if (n <= 5000) shapiro.test(x)$p.value else NA_real_
      
      dist_msg <- if (abs(sk) < 0.5 && abs(kur) < 1 && (is.na(sw) || sw > 0.05)) {
        tags$span(style = "color:#2a9d8f; font-weight:600;",
                  icon("circle-check"),
                  " Distribution looks approximately normal.",
                  " IQR-based outlier detection is reliable here.")
      } else {
        reasons <- c(
          if (abs(sk) >= 0.5) sprintf("skewness = %.2f (%s)", sk,
                                      if (sk > 0) "right-skewed" else "left-skewed"),
          if (abs(kur) >= 1)  sprintf("excess kurtosis = %.2f (%s)", kur,
                                      if (kur > 0) "heavy tails" else "light tails"),
          if (!is.na(sw) && sw <= 0.05) sprintf("Shapiro-Wilk p = %.3f (non-normal)", sw)
        )
        tags$span(style = "color:#e07b39; font-weight:600;",
                  icon("triangle-exclamation"),
                  HTML(paste0(
                    " Non-normal distribution detected (",
                    paste(reasons, collapse = "; "),
                    "). IQR outlier boundaries may flag",
                    if (sk > 0.5) " extra points on the right tail —"
                    else if (sk < -0.5) " extra points on the left tail —"
                    else " more points than expected —",
                    " interpret with caution."
                  )))
      }
      
      k         <- input$iqr_k
      limits    <- boxplot.stats(x, coef = k)$stats
      n_out     <- sum(x < limits[1] | x > limits[5], na.rm = TRUE)
      z_equiv   <- k * 1.35
      p_out     <- 2 * pnorm(-z_equiv)
      expected  <- round(n * p_out)
      threshold <- max(expected * 2, expected + 3)
      
      out_msg <- if (n_out == 0) {
        tags$span(style = "color:#2a9d8f;",
                  icon("circle-check"),
                  sprintf(" No outliers detected at k = %.1f.", k))
      } else if (n_out <= threshold) {
        tags$span(style = "color:#495057;",
                  icon("circle-info"),
                  HTML(sprintf(
                    " With <b>n = %d</b> and <b>k = %.1f</b>, roughly <b>%d</b>
                    outlier(s) expected by chance under normality.
                    We have <b>%d</b> — within expected range.",
                    n, k, expected, n_out)))
      } else {
        tags$span(style = "color:#C41E3A; font-weight:600;",
                  icon("circle-exclamation"),
                  HTML(sprintf(
                    " With <b>n = %d</b> and <b>k = %.1f</b>, only ~<b>%d</b>
                    outlier(s) expected by chance. We have <b>%d</b> —
                    notably more than expected. Worth investigating.",
                    n, k, expected, n_out)))
      }
      
      div(
        style = "background:#f8f9fa; border-left:4px solid #0d6efd;
                 border-radius:6px; padding:12px 16px;
                 font-size:13px; line-height:1.7;",
        tags$b("Interpretation Hints"), tags$br(),
        dist_msg, tags$br(), tags$br(),
        out_msg
      )
    })
    
    # ── outlier table ─────────────────────────────────────────────────────────
    output$table <- DT::renderDataTable({
      req(input$var, input$iqr_k)
      df  <- get_df_filtered()
      x   <- df[[input$var]]
      k   <- input$iqr_k
      if (anyNA(x)) return(data.frame(Message = "NAs present — filter first."))
      limits  <- boxplot.stats(x, coef = k)$stats
      out_df  <- df[x < limits[1] | x > limits[5], , drop = FALSE]
      raw_df  <- get_raw()
      if (nrow(out_df) == 0) return(data.frame(Message = "No outliers flagged."))
      # show raw data for flagged rows
      idx <- as.integer(rownames(out_df))
      idx <- idx[!is.na(idx) & idx <= nrow(raw_df)]
      if (length(idx) > 0) raw_df[idx, , drop = FALSE] |>
        DT::datatable(options = list(pageLength = 10, dom = "tip", scrollX = TRUE),
                      rownames = FALSE)
      else
        DT::datatable(out_df,
                      options = list(pageLength = 10, dom = "tip", scrollX = TRUE),
                      rownames = FALSE)
    })
  })
}