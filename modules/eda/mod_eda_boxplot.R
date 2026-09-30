# =================================================================================
# mod_eda_boxplot.R
# =================================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

eda_boxplot_ui <- function(id) {
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
        HTML("&nbsp; Tab Note: <br><br> Boxplots of all numeric variables. Adjust the IQR multiplier to control outlier visibility.")
      ),
      hr(),
      
      sliderInput(
        inputId = ns("multiplier"),
        label   = "IQR Multiplier",
        min = 0, max = 10, step = 0.1, value = 1.5
      ),
      checkboxInput(
        inputId = ns("normalise"),
        label   = "Standardise chart",
        value   = TRUE
      ),
      hr(),
      
      checkboxInput(
        inputId = ns("show_labels"),
        label   = "Show outlier labels",
        value   = TRUE
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == true", ns("show_labels")),
        selectInput(
          inputId = ns("label_col"),
          label   = "Label outliers by:",
          choices = NULL
        ),
        sliderInput(
          inputId = ns("label_size"),
          label   = "Label font size:",
          min = 1, max = 6, step = 0.5, value = 4
        )
      ),
      hr(),
      
      selectizeInput(
        inputId = ns("vars"),
        label   = "Variables to plot:",
        choices = NULL,
        multiple = TRUE,
        options  = list(placeholder = "Default: all numeric variables")
      ),
      
      textInput(
        inputId     = ns("custom_title"),
        label       = "Chart title (leave blank for default):",
        placeholder = "e.g. Numeric Variable Overview"
      )
    ),
    mainPanel(
      width = 9,
      plotOutput(ns("boxplot"), height = "80vh")
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

eda_boxplot_server <- function(id, get_data) {
  moduleServer(id, function(input, output, session) {
    
    # # populate both selectors, label column selector — rowname first, then all columns
    observe({
      d <- get_data()
      req(d)
      num_cols <- names(d)[sapply(d, is.numeric)]
      all_cols <- c("(rowname)" = ".rowname", setNames(names(d), names(d)))
      updateSelectInput(session, "label_col",
                        choices  = all_cols,
                        selected = ".rowname")
      updateSelectizeInput(session, "vars",
                           choices  = num_cols,
                           selected = num_cols)
    })
    
    output$boxplot <- renderPlot({
      d <- get_data()
      
      # attach rownames as a selectable label column
      d$.rowname <- rownames(d)
      
      numeric <- sapply(d, FUN = is.numeric)
      req(d, input$multiplier, sum(numeric) > 0)
      
      d_num <- d[, numeric, drop = FALSE]
      
      # filter to selected variables (empty selection = all)
      selected_vars <- input$vars
      if (length(selected_vars) > 0) {
        selected_vars <- intersect(selected_vars, names(d_num))
        if (length(selected_vars) > 0)
          d_num <- d_num[, selected_vars, drop = FALSE]
      }
      
      # standardise if requested
      if (input$normalise) {
        d_num <- as.data.frame(scale(d_num, center = TRUE, scale = TRUE))
      }
      
      # attach label column for outlier identification
      req(input$label_col)
      d_num$.label <- as.character(d[[input$label_col]])
      
      # reshape to long format
      d_long <- tidyr::pivot_longer(
        d_num,
        cols      = -".label",
        names_to  = "variable",
        values_to = "value"
      )
      
      # compute outlier flags per variable
      k <- input$multiplier
      d_long <- d_long |>
        dplyr::group_by(variable) |>
        dplyr::mutate(
          limits_lo     = boxplot.stats(value, coef = k)$stats[1],
          limits_hi     = boxplot.stats(value, coef = k)$stats[5],
          is_outlier    = value < limits_lo | value > limits_hi,
          outlier_label = ifelse(is_outlier & isTRUE(input$show_labels),
                                 .label, NA_character_)
        ) |>
        dplyr::ungroup()
      
      # use custom title if provided, otherwise default
      chart_title <- if (nzchar(trimws(input$custom_title))) {
        trimws(input$custom_title)
      } else {
        paste("Boxplots using IQR multiplier of", k)
      }
      
      ggplot2::ggplot(d_long, ggplot2::aes(x = variable, y = value)) +
        ggplot2::geom_boxplot(
          coef           = k,
          outlier.colour = "#C41E3A",
          outlier.size   = 1.5
        ) +
        ggrepel::geom_text_repel(
          ggplot2::aes(label = outlier_label),
          size         = input$label_size,
          max.overlaps = 10,
          na.rm        = TRUE,
          colour       = "#C41E3A",
          fontface     = "bold"
        ) +
        ggplot2::labs(
          title = chart_title,
          x     = NULL,
          y     = if (input$normalise) "Standardised value" else "Value"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1),
          plot.title       = ggplot2::element_text(face = "bold", hjust = 0.5),
          panel.grid.major = ggplot2::element_line(colour = "grey90")
        )
    })
    
  })
}