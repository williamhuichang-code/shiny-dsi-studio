# =================================================================================
# mod_eda_upset.R
# =================================================================================

# ── LIBRARY ──────────────────────────────────────────────────────────────────

library(plotly)
library(naniar)


# ── UI ───────────────────────────────────────────────────────────────────────

eda_upset_ui <- function(id) {
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
             Visualise patterns of missingness across variables.
             Each bar represents a unique combination of missing variables,
             sorted by frequency. Reveals whether variables tend to be
             missing together.")
      ),
      hr(),
      
      selectizeInput(ns("mu_vars"), "Variables to include:",
                     choices  = NULL,
                     multiple = TRUE),
      hr(),
      
      numericInput(ns("mu_nsets"), "Max variable sets (nsets):",
                   value = 6, min = 2, max = 20, step = 1),
      hr(),
      
      textInput(ns("mu_title"), "Custom plot title:", placeholder = "Auto-generated if empty")
    ),
    mainPanel(
      width = 9,
      # use uiOutput so we can stack a title div above the plot
      uiOutput(ns("mu_ui"))
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

eda_upset_server <- function(id, get_data, roles = NULL) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      df <- get_data(); req(df)
      updateSelectizeInput(session, "mu_vars",
                           choices  = names(df),
                           selected = names(df),
                           server   = TRUE)
    })
    
    # render the title + plot as stacked HTML
    output$mu_ui <- renderUI({
      title_str <- if (nzchar(input$mu_title)) input$mu_title else "Missing Value Intersection (UpSet)"
      tagList(
        div(
          style = "text-align: center; font-size: 22px; font-weight: bold;
                   margin-top: 10px; margin-bottom: 4px;",
          title_str
        ),
        plotOutput(session$ns("mu_output"), height = "80vh")
      )
    })
    
    output$mu_output <- renderPlot({
      req(get_data(), input$mu_vars)
      df <- get_data()
      df <- df[, intersect(input$mu_vars, names(df)), drop = FALSE]
      
      has_miss <- sapply(df, anyNA)
      if (!any(has_miss)) {
        plot.new()
        text(0.5, 0.5, "No missing values in selected variables.",
             cex = 1.2, col = "#C41E3A", adj = 0.5)
        return()
      }
      
      naniar::gg_miss_upset(data = df, nsets = input$mu_nsets, text.scale = 2)
    })
    
  })
}