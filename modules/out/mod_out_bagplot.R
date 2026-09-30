# =================================================================================
# mod_out_bagplot.R
# =================================================================================

out_bagplot_ui <- function(id) {
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
        HTML("&nbsp; <b>Bag Plot</b><br><br>
              Bivariate outlier detection. The bag contains the 50% most central
              observations; the fence is the bag inflated by factor k.
              Points outside the fence are flagged as outliers.<br><br>
              Uses <code>aplpack::bagplot()</code>.")
      ),
      hr(),
      selectInput(ns("x_var"), "X variable:", choices = NULL),
      selectInput(ns("y_var"), "Y variable:", choices = NULL),
      hr(),
      selectInput(ns("label_col"), "Label outliers by:", choices = NULL),
      hr(),
      numericInput(ns("bag_k"), "Bag factor (k):",
                   value = 3, min = 1, step = 0.5),
      helpText("Larger k = more permissive fence, fewer outliers."),
      hr(),
      checkboxInput(ns("tolerate_na"),
                    "Tolerate NAs (exclude NA rows for these variables only)",
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
      plotOutput(ns("plot"), height = "60vh"),
      hr(),
      h5("Outlier Rows"),
      DT::dataTableOutput(ns("table"))
    )
  )
}

out_bagplot_server <- function(id, get_data, get_raw, roles) {
  moduleServer(id, function(input, output, session) {
    
    # ── helper ────────────────────────────────────────────────────────────────
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
    
    # ── populate selectors ────────────────────────────────────────────────────
    observe({
      df       <- get_data(); req(df)
      r        <- roles()
      id_col   <- names(r)[r == "obs_id"]
      num_cols <- names(df)[sapply(df, is.numeric)]
      updateSelectInput(session, "x_var", choices = num_cols,
                        selected = if (length(num_cols) >= 1) num_cols[1] else NULL)
      updateSelectInput(session, "y_var", choices = num_cols,
                        selected = if (length(num_cols) >= 2) num_cols[2] else num_cols[1])
      updateSelectInput(session, "label_col",
                        choices  = names(df),
                        selected = if (length(id_col) > 0) id_col[1] else names(df)[1])
    })
    
    get_df_filtered <- reactive({
      req(input$x_var, input$y_var)
      df <- get_data()
      if (isTRUE(input$tolerate_na))
        df <- df[!is.na(df[[input$x_var]]) & !is.na(df[[input$y_var]]), , drop = FALSE]
      df
    })
    
    # ── plot ──────────────────────────────────────────────────────────────────
    output$plot <- renderPlot({
      req(input$x_var, input$y_var, input$label_col, input$bag_k)
      df <- get_df_filtered()
      
      x_vals <- apply_transform(df[[input$x_var]], input$transform)
      y_vals <- apply_transform(df[[input$y_var]], input$transform)
      x_label <- if (input$transform != "none")
        paste0(input$x_var, " [", input$transform, "]") else input$x_var
      y_label <- if (input$transform != "none")
        paste0(input$y_var, " [", input$transform, "]") else input$y_var
      
      n_missing <- sum(is.na(x_vals) | is.na(y_vals))
      if (n_missing > 0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = paste0("Cannot draw bagplot: ", n_missing,
                                           " rows have NA after transform.\n",
                                           "Apply a missingness strategy first."),
                            colour = "#C41E3A", size = 5) +
          ggplot2::theme_void()
        return()
      }
      
      tryCatch({
        # size and bold
        par(
          cex.main = 1.3,   # title size
          font.main = 2,    # title bold
          cex.lab  = 1.2,   # axis label size
          font.lab = 2,     # axis label bold
          cex.axis = 1.2    # axis number size
        )
        # draw bagplot natively (base R)
        aplpack::bagplot(x = x_vals, y = y_vals,
                         factor = input$bag_k,
                         show.bagpoints = FALSE,
                         main  = paste("Bagplot of", y_label, "vs", x_label,
                                       "at factor k =", input$bag_k),
                         xlab  = x_label,
                         ylab  = y_label)
        
        # overlay outlier labels
        bag <- aplpack::bagplot(x = x_vals, y = y_vals,
                                factor = input$bag_k,
                                show.bagpoints = FALSE,
                                create.plot = FALSE)
        
        if (!is.null(bag$pxy.outlier) && nrow(bag$pxy.outlier) > 0) {
          values       <- as.data.frame(bag$pxy.outlier)
          outlier_mask <- x_vals %in% values$x & y_vals %in% values$y
          outlier_df   <- df[outlier_mask, ]
          text(x      = x_vals[outlier_mask],
               y      = y_vals[outlier_mask],
               labels = as.character(outlier_df[[input$label_col]]),
               pos    = 3, cex = 1.3, col = "#C41E3A", font = 3)
        }
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = paste0("Bagplot error: ", conditionMessage(e),
                                           "\nEnsure aplpack is installed."),
                            colour = "#C41E3A", size = 4.5) +
          ggplot2::theme_void()
      })
    })
    
    # ── outlier table ─────────────────────────────────────────────────────────
    output$table <- DT::renderDataTable({
      req(input$x_var, input$y_var, input$bag_k)
      df <- get_df_filtered()
      
      x_vals <- apply_transform(df[[input$x_var]], input$transform)
      y_vals <- apply_transform(df[[input$y_var]], input$transform)
      
      if (anyNA(x_vals) || anyNA(y_vals))
        return(data.frame(Message = "NAs present after transform — filter first."))
      
      tryCatch({
        bag    <- aplpack::bagplot(x = x_vals, y = y_vals,
                                   factor = input$bag_k,
                                   show.bagpoints = FALSE,
                                   create.plot = FALSE)
        if (is.null(bag$pxy.outlier) || nrow(bag$pxy.outlier) == 0)
          return(data.frame(Message = "No outliers flagged."))
        
        values       <- as.data.frame(bag$pxy.outlier)
        outlier_mask <- x_vals %in% values$x & y_vals %in% values$y
        out_df       <- df[outlier_mask, , drop = FALSE]
        
        raw_df <- get_raw()
        idx    <- suppressWarnings(as.integer(rownames(out_df)))
        idx    <- idx[!is.na(idx) & idx <= nrow(raw_df)]
        
        tbl <- if (length(idx) > 0) raw_df[idx, , drop = FALSE] else out_df
        DT::datatable(tbl,
                      options  = list(pageLength = 10, dom = "tip", scrollX = TRUE),
                      rownames = FALSE)
      }, error = function(e) {
        data.frame(Message = paste0("Error: ", conditionMessage(e)))
      })
    })
  })
}