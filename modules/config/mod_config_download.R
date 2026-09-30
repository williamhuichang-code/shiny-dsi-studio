# =================================================================================
# mod_config_download.R
# =================================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

data_download_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    # ── top bar: stage selector + download button ──────────────────────────
    tags$div(
      style = "display:flex; align-items:flex-end; gap:16px; margin-bottom:20px; flex-wrap:wrap;",
      
      tags$div(
        style = "min-width:220px;",
        tags$label("Stage to download:",
                   style = "font-size:13px; font-weight:600; color:#343a40; display:block; margin-bottom:6px;"),
        selectInput(ns("stage"), label = NULL, choices = NULL, width = "100%")
      ),
      
      tags$div(
        style = "min-width:180px;",
        tags$label("Filename:",
                   style = "font-size:13px; font-weight:600; color:#343a40; display:block; margin-bottom:6px;"),
        textInput(ns("filename"), label = NULL,
                  value = "data_processed", placeholder = "no extension needed")
      ),
      
      tags$div(
        style = "padding-bottom:1px;",
        downloadButton(ns("download_btn"), label = "Download CSV",
                       style = "background-color:#185FA5; color:white; border:none;
                                border-radius:6px; padding:7px 18px; font-size:13px;
                                font-weight:600; cursor:pointer;")
      )
    ),
    
    # ── summary banner ─────────────────────────────────────────────────────
    tags$div(
      style = "display:flex; gap:12px; margin-bottom:20px; flex-wrap:wrap;",
      
      tags$div(
        style = "flex:1; min-width:140px; background:white; border-radius:10px;
                 border:0.5px solid #dee2e6; padding:14px 18px;
                 box-shadow:0 1px 3px rgba(0,0,0,0.06);",
        tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                          text-transform:uppercase; letter-spacing:.5px;", "Rows"),
        tags$div(style = "font-size:28px; font-weight:700; color:#185FA5;",
                 textOutput(ns("n_rows"), inline = TRUE))
      ),
      
      tags$div(
        style = "flex:1; min-width:140px; background:white; border-radius:10px;
                 border:0.5px solid #dee2e6; padding:14px 18px;
                 box-shadow:0 1px 3px rgba(0,0,0,0.06);",
        tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                          text-transform:uppercase; letter-spacing:.5px;", "Columns"),
        tags$div(style = "font-size:28px; font-weight:700; color:#0F6E56;",
                 textOutput(ns("n_cols"), inline = TRUE))
      ),
      
      tags$div(
        style = "flex:1; min-width:140px; background:white; border-radius:10px;
                 border:0.5px solid #dee2e6; padding:14px 18px;
                 box-shadow:0 1px 3px rgba(0,0,0,0.06);",
        tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                          text-transform:uppercase; letter-spacing:.5px;", "Remaining NAs"),
        tags$div(style = "font-size:28px; font-weight:700; color:#534AB7;",
                 textOutput(ns("n_na"), inline = TRUE))
      ),
      
      tags$div(
        style = "flex:1; min-width:140px; background:white; border-radius:10px;
                 border:0.5px solid #dee2e6; padding:14px 18px;
                 box-shadow:0 1px 3px rgba(0,0,0,0.06);",
        tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                          text-transform:uppercase; letter-spacing:.5px;", "NA %"),
        tags$div(style = "font-size:28px; font-weight:700; color:#C04828;",
                 textOutput(ns("pct_na"), inline = TRUE))
      )
    ),
    
    # ── data preview ───────────────────────────────────────────────────────
    tags$div(
      style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
               padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
      tags$h5("Data Preview", style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
      DT::dataTableOutput(ns("preview_tbl"))
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

# stages: a named list of reactives, e.g.
#   list("Raw" = get_raw, "Processed" = get_data)
# The names appear in the stage dropdown.

data_download_server <- function(id, stages) {
  moduleServer(id, function(input, output, session) {
    
    # populate stage selector from names of stages list
    observe({
      updateSelectInput(session, "stage",
                        choices  = names(stages),
                        selected = names(stages)[length(names(stages))])
    })
    
    # currently selected reactive
    current_data <- reactive({
      req(input$stage)
      stages[[input$stage]]()
    })
    
    # summary outputs
    output$n_rows <- renderText({ req(current_data()); nrow(current_data()) })
    output$n_cols <- renderText({ req(current_data()); ncol(current_data()) })
    output$n_na   <- renderText({
      req(current_data())
      sum(is.na(current_data()))
    })
    output$pct_na <- renderText({
      req(current_data())
      df   <- current_data()
      total <- nrow(df) * ncol(df)
      if (total == 0) return("0%")
      paste0(round(sum(is.na(df)) / total * 100, 1), "%")
    })
    
    # preview table
    output$preview_tbl <- DT::renderDataTable({
      req(current_data())
      DT::datatable(
        current_data(),
        options = list(pageLength = 15, scrollX = TRUE, dom = "tip"),
        rownames = TRUE
      )
    })
    
    # download handler
    output$download_btn <- downloadHandler(
      filename = function() {
        nm <- trimws(input$filename)
        if (!nzchar(nm)) nm <- "data_processed"
        paste0(nm, ".csv")
      },
      content = function(file) {
        write.csv(current_data(), file, row.names = FALSE)
      }
    )
    
  })
}