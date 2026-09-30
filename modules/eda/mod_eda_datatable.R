# =================================================================================
# mod_eda_datatable.R
# =================================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

eda_datatable_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 3,
      style = "background-color: #e8f0fe; border-left: 2px solid #a8c0fd;
               min-height: 100vh; padding-left: 20px; padding-right: 20px;
               overflow-x: hidden;",
      
      div(
        style = "font-size: 13px; color: #343a40; background-color: white;
                 padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
                 margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; <b>Data Table</b><br><br>
              Explore the dataset interactively. Use filters and column
              selection to focus on variables of interest.")
      ),
      hr(),
      
      # ── Raw data toggle ────────────────────────────────────────────────
      checkboxInput(ns("use_raw"), "Show raw data (before pipeline)", value = FALSE),
      hr(),

      # ── NA filter ─────────────────────────────────────────────────────
      checkboxInput(ns("only_na_rows"), "Show only rows with at least one NA", value = FALSE),
      uiOutput(ns("na_row_count")),
      hr(),
      
      # ── Column selection ───────────────────────────────────────────────
      tags$label("Columns to show:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      selectizeInput(ns("cols"), label = NULL,
                     choices = NULL, multiple = TRUE,
                     options = list(placeholder = "Defaults to all columns")),
      hr(),
      
      # ── Page length ────────────────────────────────────────────────────
      tags$label("Rows per page:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      selectInput(ns("page_length"), label = NULL,
                  choices  = c(5, 10, 15, 25, 50, 100),
                  selected = 15),
      hr(),
      
      # ── Filter ─────────────────────────────────────────────────────────
      tags$label("Column filters:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      selectInput(ns("filter"), label = NULL,
                  choices  = c("None" = "none", "Top" = "top", "Bottom" = "bottom"),
                  selected = "top"),
      hr(),
      
      # ── Selection ──────────────────────────────────────────────────────
      tags$label("Row selection:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      selectInput(ns("selection"), label = NULL,
                  choices  = c("None" = "none", "Single" = "single", "Multiple" = "multiple"),
                  selected = "none"),
      hr(),
      
      # ── DOM ────────────────────────────────────────────────────────────
      tags$label("DOM elements:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      helpText("Controls which table UI elements are shown."),
      checkboxGroupInput(ns("dom"), label = NULL,
                         choices  = c("Page length (l)" = "l",
                                      "Search box (f)"  = "f",
                                      "Table (t)"       = "t",
                                      "Info (i)"        = "i",
                                      "Pagination (p)"  = "p"),
                         selected = c("l", "f", "t", "i", "p")),
      hr(),
      
      # ── Misc ───────────────────────────────────────────────────────────
      checkboxInput(ns("rownames"),   "Show row names",    value = TRUE),
      checkboxInput(ns("ordering"),   "Column ordering",   value = TRUE),
      checkboxInput(ns("responsive"), "Responsive layout", value = FALSE),
      hr(),
      actionButton(
        ns("reset"),
        label = "Reset",
        icon = icon("rotate-left"),
        width = "100%"
      )
    ),
    
    mainPanel(
      width = 9,
      style = "overflow-x: auto;",
      DT::dataTableOutput(ns("table")),
      hr(),
      h5("Selected Rows"),
      DT::dataTableOutput(ns("selected_tbl"))
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

eda_datatable_server <- function(id, get_data, get_raw) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    table_reset_nonce <- reactiveVal(0)
    
    # ── Active data source ────────────────────────────────────────────────
    active_data <- reactive({
      if (isTRUE(input$use_raw)) get_raw() else get_data()
    })
    
    # ── Populate column selector ──────────────────────────────────────────
    observe({
      df <- active_data(); req(df)
      updateSelectizeInput(session, "cols",
                           choices  = names(df),
                           selected = names(df),
                           server   = TRUE)
    }) |> bindEvent(active_data(), input$use_raw, ignoreNULL = TRUE)
    
    # ── Reset table controls and client-side table state ─────────────────
    observeEvent(input$reset, {
      updateCheckboxInput(session, "use_raw",       value = FALSE)
      updateCheckboxInput(session, "only_na_rows",  value = FALSE)

      df <- get_data(); req(df)
      updateSelectizeInput(session, "cols",
                           choices  = names(df),
                           selected = names(df),
                           server   = TRUE)
      
      updateSelectInput(session, "page_length", selected = 15)
      updateSelectInput(session, "filter", selected = "top")
      updateSelectInput(session, "selection", selected = "none")
      updateCheckboxGroupInput(session, "dom", selected = c("l", "f", "t", "i", "p"))
      updateCheckboxInput(session, "rownames", value = TRUE)
      updateCheckboxInput(session, "ordering", value = TRUE)
      updateCheckboxInput(session, "responsive", value = FALSE)
      
      table_reset_nonce(isolate(table_reset_nonce()) + 1)
    })
    
    # ── NA row count badge ────────────────────────────────────────────────
    output$na_row_count <- renderUI({
      req(isTRUE(input$only_na_rows))
      df <- active_data(); req(df)
      n  <- sum(apply(df, 1, anyNA))
      div(
        style = "font-size:12px; color:#856404; background:#fff9e6;
                 border-left:3px solid #ffc107; padding:4px 8px;
                 border-radius:4px; margin-top:4px;",
        icon("triangle-exclamation", style = "color:#856404;"),
        HTML(paste0(" <b>", n, "</b> row", if (n != 1) "s" else "",
                    " with at least one NA"))
      )
    })

    # ── Filtered data ─────────────────────────────────────────────────────
    get_display_df <- reactive({
      df   <- active_data(); req(df)

      # NA row filter applied before column selection so anyNA scans all cols
      if (isTRUE(input$only_na_rows))
        df <- df[apply(df, 1, anyNA), , drop = FALSE]

      cols <- intersect(input$cols, names(df))
      if (length(cols) == 0) return(df)
      df[, cols, drop = FALSE]
    })
    
    # ── Main table ────────────────────────────────────────────────────────
    output$table <- DT::renderDataTable({
      table_reset_nonce()
      df <- get_display_df(); req(df)
      
      ext <- if (isTRUE(input$responsive)) list(Responsive = TRUE) else list()
      
      DT::datatable(
        df,
        rownames   = input$rownames,
        selection  = input$selection,
        filter     = list(position = input$filter),
        extensions = ext,
        options    = list(
          searching  = TRUE,
          pageLength = as.integer(input$page_length),
          lengthMenu = c(5, 10, 15, 25, 50, 100),
          dom        = paste(input$dom, collapse = ""),
          ordering   = input$ordering,
          scrollX    = TRUE
        )
      )
    })
    
    # ── Selected rows ─────────────────────────────────────────────────────
    output$selected_tbl <- DT::renderDataTable({
      table_reset_nonce()
      req(input$table_rows_selected)
      df <- get_display_df()
      DT::datatable(
        df[input$table_rows_selected, , drop = FALSE],
        options  = list(pageLength = 10, scrollX = TRUE, dom = "tip"),
        rownames = TRUE
      )
    })
    
  })
}
