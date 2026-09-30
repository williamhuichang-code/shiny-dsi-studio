# =================================================================================
# mod_miss_napp.R
# =================================================================================
#
# Logic: for each rule —
#   in `target_col`, where `when_col` == `equals_val` (or is NA), replace NA with `impute_val`
#
# "Not Applicable" impute method = rule is inactive, skip it.
# Custom impute = free-text value coerced to numeric if possible, else character.
# =================================================================================


# ── HELPER FUNCTIONS ─────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (!is.null(a)) a else b

apply_napp_rules <- function(df, rules) {
  if (length(rules) == 0) return(df)
  for (rule in rules) {
    target  <- rule$target
    when    <- rule$when
    eq_val  <- rule$equals
    imp_val <- rule$impute
    
    if (!target %in% names(df)) next
    if (!when   %in% names(df)) next
    if (is.null(imp_val) || imp_val == "Not Applicable") next
    
    # condition: when_col is NA, or when_col matches equals_val
    if (eq_val == "(NA)") {
      mask <- is.na(df[[when]]) & is.na(df[[target]])
    } else {
      mask <- !is.na(df[[when]]) & as.character(df[[when]]) == eq_val & is.na(df[[target]])
    }
    if (!any(mask)) next
    
    # resolve statistical methods against target column
    fill <- if (imp_val == "Mean") {
      mean(df[[target]], na.rm = TRUE)
    } else if (imp_val == "Median") {
      median(df[[target]], na.rm = TRUE)
    } else if (imp_val == "Mode") {
      tbl <- sort(table(df[[target]]), decreasing = TRUE)
      if (length(tbl) == 0) NA else type.convert(names(tbl)[1], as.is = TRUE)
    } else {
      num_try <- suppressWarnings(as.numeric(imp_val))
      if (!is.na(num_try)) num_try else imp_val
    }
    
    df[[target]][mask] <- fill
  }
  df
}

# unique non-NA levels of when_col, with "(NA)" prepended as an extra option
get_equals_choices <- function(df, col) {
  if (is.null(col) || col == "(none)" || !col %in% names(df)) return(character(0))
  vals <- sort(unique(as.character(df[[col]][!is.na(df[[col]])])))
  c("(NA)", vals)
}

impute_choices <- c("Not Applicable", "0", "Mean", "Median", "Mode", "Custom")


# ── UI ───────────────────────────────────────────────────────────────────────

miss_napp_ui <- function(id) {
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
          For each rule: pick a <strong>target column</strong> that has NAs,
          specify a <strong>condition</strong> on another column (including when
          that column itself is NA), then choose how to impute.")
      ),
      hr(),
      
      tags$label("Imputation Rules",
                 style = "font-size: 13px; font-weight: 600; color: #343a40; display:block; margin-bottom:10px;"),
      
      uiOutput(ns("napp_rules_ui")),
      
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

miss_napp_server <- function(id, get_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── STATE ──────────────────────────────────────────────────────────────
    
    n_rules     <- reactiveVal(1)
    rules_state <- reactiveVal(list())
    
    # ── ADD / RESET ────────────────────────────────────────────────────────
    
    observeEvent(input$add_rule, { n_rules(n_rules() + 1) })
    
    observeEvent(input$reset, {
      n_rules(1)
      rules_state(list())
      updateSelectInput(session, "napp_target_1", selected = "(none)")
      updateSelectInput(session, "napp_when_1",   selected = "(none)")
      updateSelectInput(session, "napp_equals_1", choices  = character(0))
      updateSelectInput(session, "napp_impute_1", selected = "Not Applicable")
      updateTextInput(session,   "napp_custom_1", value    = "")
    })
    
    # ── WRITE STATE from inputs ────────────────────────────────────────────
    
    observe({
      rules <- list()
      for (i in seq_len(n_rules())) {
        target <- input[[paste0("napp_target_", i)]]
        when   <- input[[paste0("napp_when_",   i)]]
        equals <- input[[paste0("napp_equals_", i)]]
        impute <- input[[paste0("napp_impute_", i)]]
        custom <- trimws(input[[paste0("napp_custom_", i)]])
        
        imp_val <- if (is.null(impute) || impute == "Not Applicable") {
          "Not Applicable"
        } else if (impute == "Custom") {
          if (nzchar(custom)) custom else "Not Applicable"
        } else {
          impute  # "0", "Mean", "Median", "Mode"
        }
        
        if (!is.null(target) && target != "(none)" &&
            !is.null(when)   && when   != "(none)" &&
            !is.null(equals) && nzchar(equals)) {
          rules[[length(rules) + 1]] <- list(
            target        = target,
            when          = when,
            equals        = equals,
            impute        = imp_val,
            impute_method = impute %||% "Not Applicable"
          )
        }
      }
      rules_state(rules)
    })
    
    # ── UPDATE equals choices when when_col changes ────────────────────────
    # Prepends "(NA)" to the column's unique levels.
    
    lapply(1:20, function(i) {
      when_id   <- paste0("napp_when_",   i)
      equals_id <- paste0("napp_equals_", i)
      observeEvent(input[[when_id]], {
        req(get_data())
        choices <- get_equals_choices(get_data(), input[[when_id]])
        updateSelectInput(session, equals_id,
                          choices  = choices,
                          selected = if (length(choices) > 0) choices[1] else character(0))
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    })
    
    # ── RULE ROWS UI ───────────────────────────────────────────────────────
    
    output$napp_rules_ui <- renderUI({
      req(get_data())
      cols <- c("(none)", names(get_data()))
      
      ui_rows <- lapply(seq_len(n_rules()), function(i) {
        
        target_id <- ns(paste0("napp_target_", i))
        when_id   <- ns(paste0("napp_when_",   i))
        equals_id <- ns(paste0("napp_equals_", i))
        impute_id <- ns(paste0("napp_impute_", i))
        custom_id <- ns(paste0("napp_custom_", i))
        
        cur_target <- isolate(input[[paste0("napp_target_", i)]] %||% "(none)")
        cur_when   <- isolate(input[[paste0("napp_when_",   i)]] %||% "(none)")
        cur_equals <- isolate(input[[paste0("napp_equals_", i)]])
        cur_impute <- isolate(input[[paste0("napp_impute_", i)]] %||% "Not Applicable")
        cur_custom <- isolate(input[[paste0("napp_custom_", i)]] %||% "")
        
        eq_choices  <- if (!is.null(cur_when) && cur_when != "(none)") {
          get_equals_choices(get_data(), cur_when)
        } else character(0)
        
        safe_equals <- if (!is.null(cur_equals) && cur_equals %in% eq_choices) cur_equals else
          if (length(eq_choices) > 0) eq_choices[1] else character(0)
        
        safe_target <- if (!is.null(cur_target) && cur_target %in% cols) cur_target else "(none)"
        safe_when   <- if (!is.null(cur_when)   && cur_when   %in% cols) cur_when   else "(none)"
        
        tags$div(
          style = "margin-bottom: 14px; padding: 10px; background: white;
                   border-radius: 8px; border: 0.5px solid #dee2e6;
                   box-shadow: 0 1px 2px rgba(0,0,0,0.04);",
          
          tags$div(
            style = "font-size: 11px; font-weight: 700; color: #6c757d;
                     text-transform: uppercase; letter-spacing: .5px; margin-bottom: 8px;",
            paste("Rule", i)
          ),
          
          # Target
          tags$div(
            style = "margin-bottom: 8px;",
            tags$label("Target column (has NAs):",
                       style = "font-weight:600; font-size:12px; color:#343a40;"),
            selectInput(target_id, label = NULL, choices = cols, selected = safe_target)
          ),
          
          # When + Equals
          tags$div(
            style = "display:flex; gap:10px; align-items:flex-start; margin-bottom:8px;",
            tags$div(
              style = "flex:1;",
              tags$label("When column:", style = "font-weight:600; font-size:12px; color:#343a40;"),
              selectInput(when_id, label = NULL, choices = cols, selected = safe_when)
            ),
            tags$div(
              style = "flex:1;",
              tags$label("Equals:", style = "font-weight:600; font-size:12px; color:#343a40;"),
              selectInput(equals_id, label = NULL,
                          choices  = eq_choices,
                          selected = safe_equals)
            )
          ),
          
          # Impute method + custom value
          tags$div(
            style = "display:flex; gap:10px; align-items:flex-start;",
            tags$div(
              style = "flex:1;",
              tags$label("Impute with:", style = "font-weight:600; font-size:12px; color:#343a40;"),
              selectInput(impute_id, label = NULL,
                          choices  = impute_choices,
                          selected = cur_impute)
            ),
            tags$div(
              style = "flex:1;",
              tags$label("or, any custom value:", style = "font-weight:600; font-size:12px; color:#343a40;"),
              textInput(custom_id, label = NULL,
                        value       = cur_custom,
                        placeholder = "e.g. 0 or Unknown")
            )
          )
        )
      })
      
      # Restore saved state after renderUI replaces the DOM
      saved <- isolate(rules_state())
      session$onFlushed(function() {
        col_names <- names(isolate(get_data()))
        all_cols  <- c("(none)", col_names)
        for (i in seq_along(saved)) {
          r <- saved[[i]]
          
          safe_target <- if (!is.null(r$target) && r$target %in% all_cols) r$target else "(none)"
          safe_when   <- if (!is.null(r$when)   && r$when   %in% all_cols) r$when   else "(none)"
          eq_ch       <- if (nzchar(safe_when) && safe_when != "(none)") {
            get_equals_choices(isolate(get_data()), safe_when)
          } else character(0)
          safe_equals <- if (!is.null(r$equals) && r$equals %in% eq_ch) r$equals else
            if (length(eq_ch) > 0) eq_ch[1] else character(0)
          
          updateSelectInput(session, paste0("napp_target_", i), selected = safe_target)
          updateSelectInput(session, paste0("napp_when_",   i), selected = safe_when)
          updateSelectInput(session, paste0("napp_equals_", i),
                            choices = eq_ch, selected = safe_equals)
          updateSelectInput(session, paste0("napp_impute_", i), selected = r$impute_method)
          if (!is.null(r$impute_method) && r$impute_method == "Custom")
            updateTextInput(session, paste0("napp_custom_", i), value = r$impute)
        }
      }, once = TRUE)
      
      ui_rows
    })
    
    # ── READ STATE ─────────────────────────────────────────────────────────
    
    r_napp_rules <- reactive({ rules_state() })
    
    # ── PIPELINE STEP ──────────────────────────────────────────────────────
    
    processed <- reactive({
      req(get_data())
      apply_napp_rules(get_data(), r_napp_rules())
    })
    
    # ── CHANGE LOG ─────────────────────────────────────────────────────────
    
    change_log <- reactive({
      req(get_data(), processed())
      rules <- r_napp_rules()
      if (length(rules) == 0) return(data.frame())
      
      rows <- lapply(rules, function(rule) {
        if (rule$impute == "Not Applicable") return(NULL)
        target <- rule$target
        when   <- rule$when
        eq_val <- rule$equals
        if (!target %in% names(get_data())) return(NULL)
        if (!when   %in% names(get_data())) return(NULL)
        
        if (eq_val == "(NA)") {
          mask <- is.na(get_data()[[when]]) & is.na(get_data()[[target]])
        } else {
          mask <- !is.na(get_data()[[when]]) &
            as.character(get_data()[[when]]) == eq_val &
            is.na(get_data()[[target]])
        }
        
        n_imputed  <- sum(mask)
        fill_shown <- if (n_imputed > 0) as.character(processed()[[target]][which(mask)[1]]) else "—"
        
        data.frame(
          `Target Column` = target,
          `When Column`   = when,
          `Equals`        = eq_val,
          `Impute Method` = rule$impute_method,
          `Imputed Value` = fill_shown,
          `Rows Imputed`  = n_imputed,
          stringsAsFactors = FALSE,
          check.names      = FALSE
        )
      })
      
      rows <- Filter(Negate(is.null), rows)
      if (length(rows) == 0) return(data.frame())
      do.call(rbind, rows)
    })
    
    # rows affected across all rules (for before/after tables)
    affected_rows <- reactive({
      req(nrow(change_log()) > 0)
      rules <- r_napp_rules()
      idx   <- c()
      for (rule in rules) {
        if (rule$impute == "Not Applicable") next
        target <- rule$target
        when   <- rule$when
        eq_val <- rule$equals
        if (!target %in% names(get_data())) next
        if (!when   %in% names(get_data())) next
        if (eq_val == "(NA)") {
          mask <- is.na(get_data()[[when]]) & is.na(get_data()[[target]])
        } else {
          mask <- !is.na(get_data()[[when]]) &
            as.character(get_data()[[when]]) == eq_val &
            is.na(get_data()[[target]])
        }
        idx <- union(idx, which(mask))
      }
      sort(idx)
    })
    
    # ── MAIN UI ────────────────────────────────────────────────────────────
    
    output$main_ui <- renderUI({
      req(get_data())
      log_df        <- change_log()
      n_active      <- nrow(log_df)
      total_imputed <- if (n_active > 0) sum(log_df$`Rows Imputed`) else 0
      cols_affected <- if (n_active > 0) length(unique(log_df$`Target Column`)) else 0
      
      tagList(
        
        # summary banner
        tags$div(
          style = "display:flex; gap:12px; margin-bottom:20px; flex-wrap:wrap;",
          
          tags$div(
            style = "flex:1; min-width:160px; background:white; border-radius:10px;
                     border:0.5px solid #dee2e6; padding:14px 18px;
                     box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                              text-transform:uppercase; letter-spacing:.5px;", "Rules Active"),
            tags$div(style = "font-size:28px; font-weight:700; color:#185FA5;", n_active)
          ),
          
          tags$div(
            style = "flex:1; min-width:160px; background:white; border-radius:10px;
                     border:0.5px solid #dee2e6; padding:14px 18px;
                     box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                              text-transform:uppercase; letter-spacing:.5px;", "Cells Imputed"),
            tags$div(style = "font-size:28px; font-weight:700; color:#0F6E56;", total_imputed)
          ),
          
          tags$div(
            style = "flex:1; min-width:160px; background:white; border-radius:10px;
                     border:0.5px solid #dee2e6; padding:14px 18px;
                     box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                              text-transform:uppercase; letter-spacing:.5px;", "Columns Affected"),
            tags$div(style = "font-size:28px; font-weight:700; color:#534AB7;", cols_affected)
          )
        ),
        
        # imputation summary table
        tags$div(
          style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                   padding:16px; margin-bottom:20px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
          tags$h5("Imputation Summary", style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
          if (n_active == 0)
            tags$p("No active rules yet — configure on the right.",
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
            if (n_active == 0)
              tags$p("No changes yet.", style = "color:#adb5bd; font-size:13px;")
            else
              DT::dataTableOutput(ns("before_tbl"))
          ),
          
          tags$div(
            style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                     padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$h5(icon("table", style = "color:#185FA5; margin-right:6px;"), "After",
                    style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
            if (n_active == 0)
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
        DT::formatStyle("Target Column", fontWeight = "bold") |>
        DT::formatStyle("Imputed Value", color = "#185FA5", fontWeight = "bold") |>
        DT::formatStyle("Rows Imputed",  color = "#0F6E56", fontWeight = "bold")
    })
    
    output$before_tbl <- DT::renderDataTable({
      rows     <- affected_rows()
      req(length(rows) > 0)
      tgt_cols <- unique(vapply(r_napp_rules(), `[[`, character(1), "target"))
      tgt_cols <- tgt_cols[tgt_cols %in% names(get_data())]
      DT::datatable(get_data()[rows, tgt_cols, drop = FALSE],
                    options = dt_opts, rownames = TRUE)
    })
    
    output$after_tbl <- DT::renderDataTable({
      rows     <- affected_rows()
      req(length(rows) > 0)
      tgt_cols <- unique(vapply(r_napp_rules(), `[[`, character(1), "target"))
      tgt_cols <- tgt_cols[tgt_cols %in% names(processed())]
      DT::datatable(processed()[rows, tgt_cols, drop = FALSE],
                    options = dt_opts, rownames = TRUE) |>
        DT::formatStyle(tgt_cols, color = "#185FA5")
    })
    
    # ── RETURN ─────────────────────────────────────────────────────────────
    
    return(list(data = processed))
    
  })
}