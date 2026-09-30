# =================================================================================
# mod_miss_variants.R
# =================================================================================

# ── HELPER FUNCTIONS ─────────────────────────────────────────────────────────

parse_vals <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) return(character(0))
  trimws(unlist(strsplit(txt, ",")))
}

apply_missingness_strategy <- function(df, global_vals) {
  if (length(global_vals) == 0) return(df)
  df[] <- lapply(names(df), function(nm) {
    col      <- df[[nm]]
    col_char <- as.character(col)
    col_char[col_char %in% global_vals] <- NA
    # restore original type
    if (is.factor(col))  return(factor(col_char, levels = levels(col)))
    if (is.integer(col)) return(suppressWarnings(as.integer(col_char)))
    if (is.numeric(col)) return(suppressWarnings(as.numeric(col_char)))
    col_char
  })
  df
}

apply_col_missingness_strategy <- function(df, col_rules) {
  if (length(col_rules) == 0) return(df)
  for (rule in col_rules) {
    col  <- rule$col
    vals <- rule$vals
    if (!col %in% names(df) || length(vals) == 0) next
    orig     <- df[[col]]
    col_char <- as.character(orig)
    col_char[col_char %in% vals] <- NA
    # restore original type
    df[[col]] <- if (is.factor(orig))   factor(col_char, levels = levels(orig))
    else if (is.integer(orig)) suppressWarnings(as.integer(col_char))
    else if (is.numeric(orig)) suppressWarnings(as.numeric(col_char))
    else col_char
  }
  df
}


# ── UI ───────────────────────────────────────────────────────────────────────

miss_variants_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      
      # layout
      
      width = 3,
      style = "background-color: #e8f0fe; border-left: 2px solid #a8c0fd; min-height: 100vh; padding-left: 20px;",
      
      # tab notes
      
      div(
        style = "font-size: 13px; color: #343a40; background-color: white;
             padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
             margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; Tab Note: <br><br> Placeholder for now.")
      ),
      hr(),
      
      # controls
      
      tags$div(
        style = "margin-bottom: 16px;",
        tags$label(
          HTML("<strong>Global &mdash; Treat as NA</strong> (e.g. -99, --):"),
          style = "font-size: 13px; color: #343a40; display:block; margin-bottom:6px;"
        ),
        textInput(ns("global_na_vals"), label = NULL,
                  placeholder = "comma separated e.g. -99, --, N/A, ?")
      ),
      
      hr(),
      
      tags$label("Column-Specific NA Rules",
                 style = "font-size: 13px; font-weight: 600; color: #343a40; display:block; margin-bottom:10px;"),
      
      uiOutput(ns("col_rules_ui")),
      
      actionButton(ns("add_rule"), label = "+ Add Rule", icon = icon("plus"),
                   width = "100%", style = "margin-top:8px; margin-bottom:8px;"),
      
      hr(),
      
      actionButton(ns("reset"), label = "Reset All", icon = icon("rotate-left"), width = "100%")
    ),
    
    mainPanel(
      width = 9,
      style = "overflow-x: auto;",
      uiOutput(ns("main_ui"))
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

miss_variants_server <- function(id, get_raw) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # state
    
    n_rules <- reactiveVal(2)
    
    observeEvent(input$add_rule, { n_rules(n_rules() + 1) })
    
    observeEvent(input$reset, {
      n_rules(2)
      updateTextInput(session, "global_na_vals", value = "")
    })
    
    # column rule rows
    
    output$col_rules_ui <- renderUI({
      req(get_raw())
      cols <- c("(none)", names(get_raw()))
      lapply(seq_len(n_rules()), function(i) {
        tags$div(
          style = "margin-bottom: 14px;",
          tags$div(
            style = "display:flex; gap:10px; align-items:flex-start;",
            tags$div(
              style = "flex:1;",
              tags$label("Column:", style = "font-weight:600; font-size:12px; color:#343a40;"),
              selectInput(ns(paste0("col_rule_col_", i)), label = NULL,
                          choices = cols, selected = "(none)")
            ),
            tags$div(
              style = "flex:1;",
              tags$label("Treat as NA:", style = "font-weight:600; font-size:12px; color:#343a40;"),
              textInput(ns(paste0("col_rule_val_", i)), label = NULL,
                        placeholder = if (i == 1) "e.g. 14" else "e.g. -1")
            )
          )
        )
      })
    })
    
    # settings reactives
    
    r_global_vals <- reactive({ parse_vals(input$global_na_vals) })
    
    r_col_rules <- reactive({
      rules <- list()
      for (i in seq_len(n_rules())) {
        col_nm <- input[[paste0("col_rule_col_", i)]]
        vals   <- parse_vals(input[[paste0("col_rule_val_", i)]])
        if (!is.null(col_nm) && col_nm != "(none)" && length(vals) > 0)
          rules[[length(rules) + 1]] <- list(col = col_nm, vals = vals)
      }
      rules
    })
    
    # pipeline step
    
    processed <- reactive({
      req(get_raw())
      get_raw() |>
        apply_missingness_strategy(r_global_vals()) |>
        apply_col_missingness_strategy(r_col_rules())
    })
    
    # change log
    
    change_log <- reactive({
      req(get_raw(), processed())
      df_orig <- get_raw()
      df_new  <- processed()
      
      changed_rows <- list()
      for (col in names(df_orig)) {
        if (!col %in% names(df_new)) next
        before_char <- as.character(df_orig[[col]])
        after_char  <- as.character(df_new[[col]])
        # find positions that became NA in processed but weren't NA in raw
        new_na <- which(is.na(df_new[[col]]) & !is.na(df_orig[[col]]))
        if (length(new_na) > 0) {
          changed_rows[[length(changed_rows) + 1]] <- data.frame(
            Row    = new_na,
            Column = col,
            Before = before_char[new_na],
            After  = "NA",
            stringsAsFactors = FALSE
          )
        }
      }
      
      if (length(changed_rows) == 0) return(data.frame())
      log_df <- do.call(rbind, changed_rows)
      log_df[order(log_df$Row, log_df$Column), ]
    })
    
    # main UI
    
    output$main_ui <- renderUI({
      req(get_raw())
      log_df    <- change_log()
      n_changed <- nrow(log_df)
      
      tagList(
        
        # summary banner
        tags$div(
          style = "display:flex; gap:12px; margin-bottom:20px; flex-wrap:wrap;",
          
          tags$div(
            style = "flex:1; min-width:160px; background:white; border-radius:10px;
                     border:0.5px solid #dee2e6; padding:14px 18px;
                     box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                              text-transform:uppercase; letter-spacing:.5px;", "Cells Recoded"),
            tags$div(style = "font-size:28px; font-weight:700; color:#185FA5;", n_changed)
          ),
          
          tags$div(
            style = "flex:1; min-width:160px; background:white; border-radius:10px;
                     border:0.5px solid #dee2e6; padding:14px 18px;
                     box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                              text-transform:uppercase; letter-spacing:.5px;", "Rows Affected"),
            tags$div(style = "font-size:28px; font-weight:700; color:#0F6E56;",
                     if (n_changed == 0) 0 else length(unique(log_df$Row)))
          ),
          
          tags$div(
            style = "flex:1; min-width:160px; background:white; border-radius:10px;
                     border:0.5px solid #dee2e6; padding:14px 18px;
                     box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                              text-transform:uppercase; letter-spacing:.5px;", "Columns Affected"),
            tags$div(style = "font-size:28px; font-weight:700; color:#534AB7;",
                     if (n_changed == 0) 0 else length(unique(log_df$Column)))
          )
        ),
        
        # change log
        tags$div(
          style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                   padding:16px; margin-bottom:20px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
          tags$h5("Change Log", style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
          if (n_changed == 0)
            tags$p("No values recoded yet — configure rules on the right.",
                   style = "color:#adb5bd; font-size:13px;")
          else
            DT::dataTableOutput(ns("change_log_tbl"))
        ),
        
        # before / after
        tags$div(
          style = "display:grid; grid-template-columns:1fr; gap:16px;",
          
          tags$div(
            style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                     padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$h5(icon("table", style = "color:#6c757d; margin-right:6px;"), "Before",
                    style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
            if (n_changed == 0)
              tags$p("No changes yet.", style = "color:#adb5bd; font-size:13px;")
            else
              DT::dataTableOutput(ns("before_tbl"))
          ),
          
          tags$div(
            style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                     padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$h5(icon("table", style = "color:#185FA5; margin-right:6px;"), "After",
                    style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
            if (n_changed == 0)
              tags$p("No changes yet.", style = "color:#adb5bd; font-size:13px;")
            else
              DT::dataTableOutput(ns("after_tbl"))
          )
        )
      )
    })
    
    # table renders
    
    dt_opts <- list(pageLength = 10, scrollX = TRUE, dom = "tip")
    
    output$change_log_tbl <- DT::renderDataTable({
      req(nrow(change_log()) > 0)
      DT::datatable(change_log(), options = dt_opts, rownames = FALSE) |>
        DT::formatStyle("After", color = "#185FA5", fontWeight = "bold")
    })
    
    output$before_tbl <- DT::renderDataTable({
      req(nrow(change_log()) > 0)
      rows <- unique(change_log()$Row)
      DT::datatable(get_raw()[rows, , drop = FALSE], options = dt_opts, rownames = TRUE)
    })
    
    output$after_tbl <- DT::renderDataTable({
      req(nrow(change_log()) > 0)
      rows <- unique(change_log()$Row)
      DT::datatable(processed()[rows, , drop = FALSE], options = dt_opts, rownames = TRUE)
    })
    
    # return
    
    return(list(data = processed))
    
  })
}