# =================================================================================
# mod_eda_summary.R
# =================================================================================

# ── LIBRARY ──────────────────────────────────────────────────────────────────

library(plotly)
library(summarytools)


# ── UI ───────────────────────────────────────────────────────────────────────

eda_summary_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      
      # layout
      
      width = 3,
      style = "background-color: #e8f0fe; border-left: 2px solid #a8c0fd; min-height: 100vh; padding-left: 20px;",
      
      # tab note
      
      div(
        style = "font-size: 13px; color: #343a40; background-color: white;
             padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
             margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; Tab Note: <br><br> Placeholder for now.")
      ),
      hr(),
      
      # controls
      
      radioButtons(
        inputId  = ns("summary_type"),
        label    = "Summary Type",
        choices  = c(
          "Base R"    = "base",
          "Glimpse"   = "glimpse",
          "dfSummary" = "dfsummary"
        ),
        selected = "glimpse"
      )
    ),
    mainPanel(
      width = 9,
      uiOutput(ns("summary_output"))
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

eda_summary_server <- function(id, get_data) {
  moduleServer(id, function(input, output, session) {
    
    output$summary_output <- renderUI({
      req(get_data())
      type <- input$summary_type
      
      if (type == "base") {
        verbatimTextOutput(session$ns("base_out"))
      } else if (type == "glimpse") {
        verbatimTextOutput(session$ns("glimpse_out"))
      } else if (type == "dfsummary") {
        htmlOutput(session$ns("dfsummary_out"))
      }
    })
    
    output$base_out <- renderPrint({
      req(get_data(), input$summary_type == "base")
      summary(get_data())
    })
    
    output$glimpse_out <- renderPrint({
      req(get_data(), input$summary_type == "glimpse")
      dplyr::glimpse(get_data())
    })
    
    output$dfsummary_out <- renderPrint({
      req(get_data(), input$summary_type == "dfsummary")
      summarytools::dfSummary(get_data()) |>
        summarytools::view(method = "render")
    })
    
  })
}