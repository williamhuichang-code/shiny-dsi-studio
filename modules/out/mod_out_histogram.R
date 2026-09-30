# =================================================================================
# mod_out_histogram.R
# =================================================================================

out_histogram_ui <- function(id) {
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
        HTML("&nbsp; <b>Histogram</b><br><br>
              Inspect the shape of a numeric variable before using boxplots.
              Heavy skew or multimodality means the default IQR multiplier (1.5)
              may flag false outliers.")
      ),
      hr(),
      selectInput(ns("var"), "Variable:", choices = NULL),
      hr(),
      sliderInput(ns("bins"), "Number of bins:",
                  min = 5, max = 200, value = 20, step = 5, width = "100%"),
      hr(),
      checkboxInput(ns("tolerate_na"), "Ignore NAs", value = TRUE),
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
      plotOutput(ns("plot"), height = "60vh")
    )
  )
}

out_histogram_server <- function(id, get_data, roles) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      df       <- get_data(); req(df)
      num_cols <- names(df)[sapply(df, is.numeric)]
      updateSelectInput(session, "var", choices = num_cols,
                        selected = if (length(num_cols) > 0) num_cols[1] else NULL)
    })
    
    output$plot <- renderPlot({
      req(input$var)
      df <- get_data()
      if (isTRUE(input$tolerate_na))
        df <- df[!is.na(df[[input$var]]), , drop = FALSE]
      
      x <- df[[input$var]]
      x_plot <- if (input$transform == "none") {
        x
      } else {
        tryCatch({
          if (input$transform == "boxcox") {
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
      
      x_label <- if (input$transform == "none") input$var
      else paste0(input$var, " [", input$transform, "]")
      
      ggplot2::ggplot(data.frame(x = x_plot), ggplot2::aes(x = x)) +
        ggplot2::geom_histogram(bins = input$bins, fill = "#4a80d4", colour = "white") +
        ggplot2::labs(
          title    = paste0("Histogram — ", x_label, "   |   Bins: ", input$bins),
          x        = x_label,
          y        = "Count"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          plot.title   = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
          axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
          axis.title.y = ggplot2::element_text(size = 14, face = "bold"),
          axis.text.x  = ggplot2::element_text(size = 14),
          axis.text.y  = ggplot2::element_text(size = 14)
        )
    })
  })
}