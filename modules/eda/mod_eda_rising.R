# =================================================================================
# mod_eda_rising.R
# =================================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

eda_rising_ui <- function(id) {
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
        HTML("&nbsp; Tab Note: <br><br> Placeholder for now.")
      ),
      hr(),
      
      # controls
      
      selectizeInput(ns("rv_vars"), "Numeric variables to plot:", choices = NULL, multiple = TRUE),
      hr(),
      checkboxInput(ns("rv_omit_na"), "Ignore NAs", value = FALSE),
      hr(),
      radioButtons(ns("rv_transform"), "Transform:",
                   choices  = c("None" = "none", "Centre" = "center", "Standardise" = "standardise", "Normalise" = "normalise"),
                   selected = "normalise"
      ),
      hr(),
      textInput(ns("rv_title"), "Custom plot title:", placeholder = "Auto-generated if empty"),
      hr(),
      sliderInput(ns("rv_lwd"), "Line width:", min = 0.2, max = 5, value = 1.6, step = 0.1, width = "100%"),
      hr(),
      radioButtons(ns("rv_lty"), "Line type:",
                   choices  = c("Solid" = "solid", "Dashed" = "dashed", "Dotted" = "dotted", "Dotdash" = "dotdash", "Longdash" = "longdash"),
                   selected = "dotdash"
      )
    ),
    mainPanel(
      width = 9,
      plotlyOutput(ns("rv_output"), height = "80vh")
    )
  )
}

# ── SERVER ───────────────────────────────────────────────────────────────────

eda_rising_server <- function(id, get_data, roles = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # helpers
    
    apply_transform <- function(y, method) {
      switch(method,
             "center"      = y - mean(y, na.rm = TRUE),
             "standardise" = as.numeric(scale(y, center = TRUE, scale = TRUE)),
             "normalise"   = (y - min(y, na.rm = TRUE)) / (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)),
             y
      )
    }
    
    to_rising <- function(v, df, omit_na, transform) {
      y <- df[[v]]
      if (omit_na) y <- na.omit(y)
      if (length(y) == 0) return(NULL)
      y   <- sort(apply_transform(y, transform), na.last = TRUE)
      pct <- seq_along(y) / length(y) * 100
      data.frame(Percentile = pct, Value = y, Variable = v)
    }
    
    auto_title <- function(omit_na, transform) {
      paste0(
        "Rising Value Graph for Continuity",
        if (omit_na)          " | NAs Omitted"                                  else "",
        if (transform != "none") paste0(" | ", tools::toTitleCase(transform))   else ""
      )
    }
    
    # variable selector
    
    observe({
      df <- get_data(); req(df)
      num_vars <- names(df)[sapply(df, is.numeric)]
      
      default_vars <- if (!is.null(roles)) {
        role_vals <- roles()
        pred_vars <- names(role_vals[role_vals %in% c("predictor", "outcome")])
        intersect(pred_vars, num_vars)
      } else {
        num_vars
      }
      
      updateSelectizeInput(session, "rv_vars",
                           choices  = num_vars,
                           selected = default_vars)
    })
    
    # plot
    
    output$rv_output <- renderPlotly({
      req(get_data(), input$rv_vars, length(input$rv_vars) > 0)
      df <- get_data()
      
      plot_df <- do.call(rbind, Filter(Negate(is.null),
                                       lapply(input$rv_vars, to_rising, df = df, omit_na = input$rv_omit_na, transform = input$rv_transform)
      ))
      
      validate(need(!is.null(plot_df) && nrow(plot_df) > 0, "No data to plot."))
      
      p <- ggplot(plot_df, aes(x = Percentile, y = Value, colour = Variable, group = Variable,
                               text = paste0(Variable,
                                             "<br>Percentile: ", round(Percentile, 1),
                                             "<br>Value: ",      round(Value, 3)))) +
        geom_line(linewidth = input$rv_lwd / 3, linetype = input$rv_lty) +
        scale_colour_viridis_d(option = "turbo") +
        theme_minimal(base_size = 11) +
        theme(legend.position = "right")
      
      plot_title <- if (nzchar(input$rv_title)) input$rv_title else auto_title(input$rv_omit_na, input$rv_transform)
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          title  = list(text = paste0("<b>", plot_title, "</b>"), font = list(size = 28, color = "black"), x = 0.5, y = 0.95),
          margin = list(t = 90),
          legend = list(x = 1.02, y = 0.5, yanchor = "middle",
                        font  = list(size = 16),
                        title = list(font = list(size = 16, color = "black"), text = "<b>Variable</b>")),
          xaxis  = list(title = list(text = "<b>Percentile</b>", font = list(size = 20)), tickfont = list(size = 18)),
          yaxis  = list(title = list(text = "<b>Value</b>",      font = list(size = 20)), tickfont = list(size = 18))
        )
    })
    
  })
}

