# =================================================================================
# mod_eda_interaction.R
# =================================================================================
#
# PURPOSE
#   Scatterplot coloured by a third variable to spot interactions.
#   - X axis  : a numeric predictor
#   - Y axis  : a numeric outcome (or any numeric)
#   - Colour  : a numeric OR factor "modifier" variable
#   If colour groups show DIFFERENT slopes/patterns → interaction likely exists.
#
# REGISTRATION (add to global.R / app.R)
#   UI     : tabItem(tabName = "eda_interaction", eda_interaction_ui("eda_interaction"))
#   Sidebar: menuSubItem("Interaction", tabName = "eda_interaction")
#   Server : eda_interaction_server("eda_interaction", get_diagnose_data, roles)
#
# =================================================================================


# ── UI ───────────────────────────────────────────────────────────────────────

eda_interaction_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      
      # layout
      width = 3,
      style = "background-color: #e8f0fe; border-left: 2px solid #a8c0fd; min-height: 100vh; padding-left: 20px;",
      
      # note
      div(
        style = "font-size: 13px; color: #343a40; background-color: white;
             padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
             margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; <b>Tab Note:</b><br><br>
             Plot a scatterplot of <b>X vs Y</b>, coloured by a third
             <b>modifier</b> variable.<br><br>
             <b>How to read it:</b><br>
             &bull; All colour groups follow the <em>same</em> trend &rarr; no interaction.<br>
             &bull; Colour groups follow <em>different</em> slopes or patterns &rarr; interaction likely exists.<br><br>
             Numeric modifiers are binned into quantile groups; factor modifiers use their levels.")
      ),
      hr(),
      
      # variable selectors
      selectInput(ns("int_x"),    "X variable (predictor):",  choices = NULL),
      selectInput(ns("int_y"),    "Y variable (outcome):",    choices = NULL),
      selectInput(ns("int_col"),  "Colour / modifier variable:", choices = NULL),
      hr(),
      
      # numeric modifier: number of quantile bins
      conditionalPanel(
        condition = "output.int_col_is_numeric",
        ns = ns,
        sliderInput(ns("int_bins"), "Quantile bins for numeric modifier:",
                    min = 2, max = 6, value = 3, step = 1, width = "100%"),
        hr()
      ),
      
      # smoother
      checkboxInput(ns("int_smooth"), "Add trend lines (per group)", value = TRUE),
      conditionalPanel(
        condition = "input.int_smooth == true",
        ns = ns,
        radioButtons(ns("int_method"), "Smoother method:",
                     choices  = c("lm", "loess"),
                     selected = "lm",
                     inline   = TRUE),
        checkboxInput(ns("int_se"), "Show confidence band", value = FALSE)
      ),
      hr(),
      
      # alpha / size
      sliderInput(ns("int_alpha"), "Point opacity:",
                  min = 0.1, max = 1.0, value = 0.6, step = 0.05, width = "100%"),
      sliderInput(ns("int_size"),  "Point size:",
                  min = 0.5, max = 5.0, value = 2.0, step = 0.25, width = "100%"),
      hr(),
      
      # custom title
      textInput(ns("int_title"), "Custom plot title:", placeholder = "Auto-generated if empty")
    ),
    
    mainPanel(
      width = 9,
      plotlyOutput(ns("int_output"), height = "80vh")
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

eda_interaction_server <- function(id, get_data, roles = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # ── helpers ──────────────────────────────────────────────────────────────
    
    # tells the UI whether the chosen colour variable is numeric
    output$int_col_is_numeric <- reactive({
      df  <- get_data(); req(df, input$int_col, input$int_col %in% names(df))
      is.numeric(df[[input$int_col]])
    })
    outputOptions(output, "int_col_is_numeric", suspendWhenHidden = FALSE)
    
    
    # bin a numeric modifier into n quantile-based groups
    bin_modifier <- function(v, n_bins) {
      breaks <- unique(quantile(v, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE))
      if (length(breaks) < 2) return(factor(rep("all", length(v))))
      cut(v, breaks = breaks, include.lowest = TRUE, dig.lab = 3)
    }
    
    
    # auto title
    auto_title <- function(x, y, col) {
      paste0("Interaction: ", x, " vs ", y, "  |  coloured by ", col)
    }
    
    
    # ── variable population ───────────────────────────────────────────────────
    
    observe({
      df <- get_data(); req(df)
      
      num_vars <- names(df)[sapply(df, is.numeric)]
      all_vars <- names(df)
      
      # use role assignments to pick sensible defaults when available
      default_x <- default_y <- default_col <- NULL
      
      if (!is.null(roles)) {
        rv <- roles()
        outcome_vars   <- names(rv[rv == "outcome"])
        predictor_vars <- names(rv[rv == "predictor"])
        
        default_y   <- if (length(outcome_vars)   > 0) intersect(outcome_vars,   num_vars)[1] else NULL
        default_x   <- if (length(predictor_vars) > 0) intersect(predictor_vars, num_vars)[1] else NULL
        default_col <- if (length(predictor_vars) > 1) intersect(predictor_vars, num_vars)[2] else NULL
      }
      
      if (is.null(default_x))   default_x   <- num_vars[1]
      if (is.null(default_y))   default_y   <- num_vars[min(2, length(num_vars))]
      if (is.null(default_col)) default_col <- all_vars[1]
      
      updateSelectInput(session, "int_x",   choices = num_vars, selected = default_x)
      updateSelectInput(session, "int_y",   choices = num_vars, selected = default_y)
      updateSelectInput(session, "int_col", choices = all_vars, selected = default_col)
    })
    
    
    # ── main plot ─────────────────────────────────────────────────────────────
    
    output$int_output <- renderPlotly({
      
      req(get_data(), input$int_x, input$int_y, input$int_col)
      
      df    <- get_data()
      x_var <- input$int_x
      y_var <- input$int_y
      c_var <- input$int_col
      
      req(x_var %in% names(df), y_var %in% names(df), c_var %in% names(df))
      
      # build a clean working frame
      plot_df <- df[, c(x_var, y_var, c_var), drop = FALSE]
      names(plot_df) <- c("x_val", "y_val", "c_raw")
      plot_df <- plot_df[complete.cases(plot_df), ]
      
      validate(need(nrow(plot_df) >= 3, "Not enough complete cases to plot."))
      
      # create colour grouping
      if (is.numeric(plot_df$c_raw)) {
        n_bins          <- input$int_bins
        plot_df$colour  <- bin_modifier(plot_df$c_raw, n_bins)
        colour_label    <- paste0(c_var, "\n(quantile bins)")
      } else {
        plot_df$colour  <- factor(plot_df$c_raw)
        colour_label    <- c_var
      }
      
      # tooltip text
      plot_df$tip <- paste0(
        "<b>", x_var, ":</b> ", round(plot_df$x_val, 3),
        "<br><b>", y_var, ":</b> ", round(plot_df$y_val, 3),
        "<br><b>", c_var, ":</b> ", plot_df$c_raw
      )
      
      # base scatterplot
      p <- ggplot(plot_df, aes(x = x_val, y = y_val, colour = colour, group = colour)) +
        geom_point(aes(text = tip), alpha = input$int_alpha, size = input$int_size) +
        scale_colour_viridis_d(option = "turbo", name = colour_label) +
        labs(x = x_var, y = y_var) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "right")
      
      # optional smoother
      if (isTRUE(input$int_smooth)) {
        p <- p + geom_smooth(
          method  = input$int_method,
          formula = y ~ x,
          se      = isTRUE(input$int_se),
          linewidth = 0.9
        )
      }
      
      # title
      plot_title <- if (nzchar(trimws(input$int_title))) {
        input$int_title
      } else {
        auto_title(x_var, y_var, c_var)
      }
      
      # convert to plotly
      ggplotly(p, tooltip = "text") |>
        layout(
          title  = list(
            text = paste0("<b>", plot_title, "</b>"),
            font = list(size = 22, color = "black"),
            x = 0.5, y = 0.96
          ),
          margin = list(t = 80),
          legend = list(
            x = 1.02, y = 0.5, yanchor = "middle",
            font  = list(size = 14),
            title = list(font = list(size = 14, color = "black"),
                         text = paste0("<b>", colour_label, "</b>"))
          ),
          xaxis = list(
            title = list(text = paste0("<b>", x_var, "</b>"), font = list(size = 18)),
            tickfont = list(size = 14)
          ),
          yaxis = list(
            title = list(text = paste0("<b>", y_var, "</b>"), font = list(size = 18)),
            tickfont = list(size = 14)
          )
        )
    })
    
  })
}