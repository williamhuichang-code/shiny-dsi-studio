# =================================================================================
# mod_miss_shadow.R
# =================================================================================

# ── HELPER FUNCTIONS ─────────────────────────────────────────────────────────

apply_shadow_vars <- function(df, shadow_rules) {
  if (length(shadow_rules) == 0) return(df)
  for (rule in shadow_rules) {
    col     <- rule$col
    new_nm  <- rule$name
    if (col %in% names(df) && nzchar(new_nm)) {
      df[[new_nm]] <- as.integer(is.na(df[[col]]))
    }
  }
  df
}

default_shadow_name <- function(col) {
  if (is.null(col) || col == "(none)") return("")
  paste0(col, "_shadow")
}


# ── UI ───────────────────────────────────────────────────────────────────────

miss_shadow_ui <- function(id) {
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
        HTML("&nbsp; Tab Note: <br><br> Placeholder for now.")
      ),
      hr(),
      
      tags$label("Shadow Variable Rules",
                 style = "font-size: 13px; font-weight: 600; color: #343a40; display:block; margin-bottom:10px;"),
      
      tags$p(
        "Each rule creates a new integer column: 1 = missing, 0 = not missing.",
        style = "font-size: 12px; color: #6c757d; margin-bottom: 10px;"
      ),
      
      uiOutput(ns("shadow_rules_ui")),
      
      actionButton(ns("add_rule"), label = "+ Add Shadow Var", icon = icon("plus"),
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

miss_shadow_server <- function(id, get_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── STATE ──────────────────────────────────────────────────────────────
    # Source of truth lives here, not in raw inputs.
    # Each element: list(col = "colname", name = "colname_shadow")
    
    n_rules          <- reactiveVal(1)
    shadow_rules_state <- reactiveVal(list())
    
    # ── ADD / RESET ────────────────────────────────────────────────────────
    
    observeEvent(input$add_rule, { n_rules(n_rules() + 1) })
    
    observeEvent(input$reset, {
      n_rules(1)
      shadow_rules_state(list())
      updateSelectInput(session, "shadow_col_1",  selected = "(none)")
      updateTextInput(session,   "shadow_name_1", value    = "")
    })
    
    # ── WRITE STATE from inputs ────────────────────────────────────────────
    # Runs whenever any relevant input changes; persists rules into reactiveVal.
    
    observe({
      rules <- list()
      for (i in seq_len(n_rules())) {
        col_nm <- input[[paste0("shadow_col_",  i)]]
        new_nm <- trimws(input[[paste0("shadow_name_", i)]])
        if (!is.null(col_nm) && col_nm != "(none)" && nzchar(new_nm))
          rules[[length(rules) + 1]] <- list(col = col_nm, name = new_nm)
      }
      shadow_rules_state(rules)
    })
    
    # ── AUTO-POPULATE shadow name when column selection changes ────────────
    
    lapply(1:20, function(i) {
      col_id  <- paste0("shadow_col_",  i)
      name_id <- paste0("shadow_name_", i)
      observeEvent(input[[col_id]], {
        suggested <- default_shadow_name(input[[col_id]])
        updateTextInput(session, name_id, value = suggested)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    })
    
    # ── SHADOW RULE ROWS ───────────────────────────────────────────────────
    # UI reads from get_data() for column choices.
    # After re-render, restore previously saved state via updateXxxInput.
    
    output$shadow_rules_ui <- renderUI({
      req(get_data())
      cols <- c("(none)", names(get_data()))
      
      ui_rows <- lapply(seq_len(n_rules()), function(i) {
        col_id  <- ns(paste0("shadow_col_",  i))
        name_id <- ns(paste0("shadow_name_", i))
        
        tags$div(
          style = "margin-bottom: 14px; padding: 10px; background: white;
                   border-radius: 8px; border: 0.5px solid #dee2e6;
                   box-shadow: 0 1px 2px rgba(0,0,0,0.04);",
          tags$div(
            style = "font-size: 11px; font-weight: 700; color: #6c757d;
                     text-transform: uppercase; letter-spacing: .5px; margin-bottom: 8px;",
            paste("Rule", i)
          ),
          tags$div(
            style = "display:flex; gap:10px; align-items:flex-start;",
            tags$div(
              style = "flex:1;",
              tags$label("Source Column:", style = "font-weight:600; font-size:12px; color:#343a40;"),
              selectInput(col_id, label = NULL, choices = cols, selected = "(none)")
            ),
            tags$div(
              style = "flex:1;",
              tags$label("New Column Name:", style = "font-weight:600; font-size:12px; color:#343a40;"),
              textInput(name_id, label = NULL, placeholder = "e.g. age_miss")
            )
          )
        )
      })
      
      # Restore saved state after renderUI replaces the DOM.
      # Uses session$onFlushed so inputs exist before we try to update them.
      saved <- isolate(shadow_rules_state())
      session$onFlushed(function() {
        for (i in seq_along(saved)) {
          prev_col  <- saved[[i]]$col
          prev_name <- saved[[i]]$name
          col_choices <- c("(none)", names(isolate(get_data())))
          safe_col <- if (!is.null(prev_col) && prev_col %in% col_choices) prev_col else "(none)"
          updateSelectInput(session, paste0("shadow_col_",  i), selected = safe_col)
          updateTextInput(session,   paste0("shadow_name_", i), value    = prev_name)
        }
      }, once = TRUE)
      
      ui_rows
    })
    
    # ── READ STATE ─────────────────────────────────────────────────────────
    # r_shadow_rules is now just a thin wrapper around the reactiveVal.
    
    r_shadow_rules <- reactive({ shadow_rules_state() })
    
    # ── PIPELINE STEP ──────────────────────────────────────────────────────
    
    processed <- reactive({
      req(get_data())
      apply_shadow_vars(get_data(), r_shadow_rules())
    })
    
    # ── CHANGE LOG ─────────────────────────────────────────────────────────
    
    change_log <- reactive({
      req(get_data(), processed())
      rules <- r_shadow_rules()
      if (length(rules) == 0) return(data.frame())
      
      do.call(rbind, lapply(rules, function(rule) {
        col    <- rule$col
        new_nm <- rule$name
        n_miss <- sum(is.na(get_data()[[col]]))
        n_obs  <- sum(!is.na(get_data()[[col]]))
        data.frame(
          `Source Column`   = col,
          `New Column`      = new_nm,
          `Type`            = "integer",
          `1 (missing)`     = n_miss,
          `0 (not missing)` = n_obs,
          stringsAsFactors  = FALSE,
          check.names       = FALSE
        )
      }))
    })
    
    # ── MAIN UI ────────────────────────────────────────────────────────────
    
    output$main_ui <- renderUI({
      req(get_data())
      log_df         <- change_log()
      n_rules_active <- nrow(log_df)
      
      tagList(
        
        tags$div(
          style = "display:flex; gap:12px; margin-bottom:20px; flex-wrap:wrap;",
          
          tags$div(
            style = "flex:1; min-width:160px; background:white; border-radius:10px;
                     border:0.5px solid #dee2e6; padding:14px 18px;
                     box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                              text-transform:uppercase; letter-spacing:.5px;", "Shadow Vars Created"),
            tags$div(style = "font-size:28px; font-weight:700; color:#185FA5;", n_rules_active)
          ),
          
          tags$div(
            style = "flex:1; min-width:160px; background:white; border-radius:10px;
                     border:0.5px solid #dee2e6; padding:14px 18px;
                     box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                              text-transform:uppercase; letter-spacing:.5px;", "Cols Before"),
            tags$div(style = "font-size:28px; font-weight:700; color:#0F6E56;",
                     ncol(get_data()))
          ),
          
          tags$div(
            style = "flex:1; min-width:160px; background:white; border-radius:10px;
                     border:0.5px solid #dee2e6; padding:14px 18px;
                     box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                              text-transform:uppercase; letter-spacing:.5px;", "Cols After"),
            tags$div(style = "font-size:28px; font-weight:700; color:#534AB7;",
                     ncol(processed()))
          )
        ),
        
        tags$div(
          style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                   padding:16px; margin-bottom:20px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
          tags$h5("Shadow Variable Summary", style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
          if (n_rules_active == 0)
            tags$p("No shadow variables configured yet — add rules on the right.",
                   style = "color:#adb5bd; font-size:13px;")
          else
            DT::dataTableOutput(ns("change_log_tbl"))
        ),
        
        tags$div(
          style = "display:grid; grid-template-columns:1fr; gap:16px;",
          
          tags$div(
            style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                     padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$h5(icon("table", style = "color:#6c757d; margin-right:6px;"), "Before",
                    style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
            if (n_rules_active == 0)
              tags$p("No changes yet.", style = "color:#adb5bd; font-size:13px;")
            else
              DT::dataTableOutput(ns("before_tbl"))
          ),
          
          tags$div(
            style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                     padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$h5(icon("table", style = "color:#185FA5; margin-right:6px;"), "After",
                    style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
            if (n_rules_active == 0)
              tags$p("No changes yet.", style = "color:#adb5bd; font-size:13px;")
            else
              DT::dataTableOutput(ns("after_tbl"))
          )
        )
      )
    })
    
    # ── TABLE RENDERS ──────────────────────────────────────────────────────
    
    dt_opts <- list(pageLength = 10, scrollX = TRUE, dom = "tip")
    
    output$change_log_tbl <- DT::renderDataTable({
      req(nrow(change_log()) > 0)
      DT::datatable(change_log(), options = dt_opts, rownames = FALSE) |>
        DT::formatStyle("New Column", color = "#185FA5", fontWeight = "bold") |>
        DT::formatStyle("Type",       color = "#534AB7", fontWeight = "bold")
    })
    
    output$before_tbl <- DT::renderDataTable({
      req(nrow(change_log()) > 0)
      src_cols <- vapply(r_shadow_rules(), `[[`, character(1), "col")
      DT::datatable(get_data()[, src_cols, drop = FALSE], options = dt_opts, rownames = TRUE)
    })
    
    output$after_tbl <- DT::renderDataTable({
      req(nrow(change_log()) > 0)
      src_cols    <- vapply(r_shadow_rules(), `[[`, character(1), "col")
      shadow_cols <- vapply(r_shadow_rules(), `[[`, character(1), "name")
      show_cols   <- c(src_cols, shadow_cols)
      DT::datatable(processed()[, show_cols, drop = FALSE], options = dt_opts, rownames = TRUE) |>
        DT::formatStyle(shadow_cols, color = "#185FA5", fontWeight = "bold")
    })
    
    # ── RETURN ─────────────────────────────────────────────────────────────
    
    return(list(data = processed))
    
  })
}