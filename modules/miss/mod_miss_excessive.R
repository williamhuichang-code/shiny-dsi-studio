# =================================================================================
# mod_miss_excessive.R
# =================================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

miss_excessive_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 3,
      style = "background-color: #e8f0fe; border-left: 2px solid #a8c0fd;
               min-height: 100vh; padding-left: 20px;",
      
      div(
        style = "font-size: 13px; color: #343a40; background-color: white;
                 padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
                 margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; Tab Note: <br><br>Placeholder for now.")
      ),
      hr(),
      
      tags$p("Step 1 — Variable Threshold",
             style = "font-size:12px; color:#6c757d; margin-bottom:4px;"),
      tags$label("Max missingness per variable:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      sliderInput(ns("var_thresh"), label = NULL,
                  min = 0, max = 1, value = 1, step = 0.05, width = "100%"),
      hr(),
      
      tags$p("Step 2 — Observation Threshold",
             style = "font-size:12px; color:#6c757d; margin-bottom:4px;"),
      tags$label("Max missingness per observation:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      sliderInput(ns("obs_thresh"), label = NULL,
                  min = 0, max = 1, value = 1, step = 0.01, width = "100%"),
      hr(),
      
      hr(),
      
      tags$p("Step 2 Table \u2014 Extra Columns",
             style = "font-size:12px; color:#6c757d; margin-bottom:4px;"),
      tags$label("Show columns alongside excluded rows:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      div(style = "font-size:11px; color:#6c757d; margin-bottom:6px;",
          "e.g. select your ID column to identify excluded observations."),
      selectizeInput(ns("extra_cols"), label = NULL,
                     choices  = NULL,
                     multiple = TRUE,
                     options  = list(placeholder = "Select columns to display..."),
                     width    = "100%"),
      hr(),
      
      actionButton(ns("reset"), label = "Reset", icon = icon("rotate-left"), width = "100%")
    ),
    
    mainPanel(
      width = 9,
      uiOutput(ns("main_ui"))
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

miss_excessive_server <- function(id, get_data, roles = NULL) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    observeEvent(input$reset, {
      updateSliderInput(session, "var_thresh", value = 1)
      updateSliderInput(session, "obs_thresh", value = 1)
      updateSelectizeInput(session, "extra_cols", selected = character(0))
    })

    # ── populate extra_cols selector ─────────────────────────────────────
    observe({
      df <- get_data(); req(df)
      updateSelectizeInput(session, "extra_cols",
                           choices  = names(df),
                           selected = character(0),
                           server   = TRUE)
    }) |> bindEvent(get_data(), once = TRUE)


    # ── compute exclusions ────────────────────────────────────────────────

    result <- reactive({
      df         <- get_data()
      req(df)
      r          <- if (!is.null(roles)) roles() else character(0)
      # Protect outcome variable(s) from variable-level exclusion
      imp_vars   <- names(r)[r %in% c("outcome")]
      imp_vars   <- imp_vars %||% character(0)
      # Identify the observation ID column (role == "obs_id") for labelling excluded rows
      obs_id_col <- { col <- names(r)[r == "obs_id"]; if (length(col) > 0) col[1] else NULL }
      
      # identify shadow columns — exclude from all missingness calculations
      # shadow cols are always 0/1 and never NA, so including them dilutes miss %
      shadow_cols <- grep("_shadow$", names(df), value = TRUE)
      df_no_shadow <- df[, !names(df) %in% shadow_cols, drop = FALSE]
      
      # step 1: variable missingness (shadow cols excluded)
      var_miss   <- colMeans(is.na(df_no_shadow))
      excl_vars  <- names(var_miss[var_miss >= input$var_thresh])
      prot_vars  <- intersect(excl_vars, imp_vars)        # would be excluded but protected
      excl_vars  <- setdiff(excl_vars, imp_vars)          # actually excluded
      
      # remove excluded vars from FULL df (including shadow cols) for downstream
      df_after_vars <- df[, !names(df) %in% excl_vars, drop = FALSE]
      
      # step 2: observation missingness (shadow cols excluded from denominator)
      df_after_vars_no_shadow <- df_after_vars[, !names(df_after_vars) %in% shadow_cols, drop = FALSE]
      obs_miss   <- rowMeans(is.na(df_after_vars_no_shadow))
      excl_rows  <- which(obs_miss >= input$obs_thresh)
      
      df_final   <- if (length(excl_rows) > 0) df_after_vars[-excl_rows, , drop = FALSE] else df_after_vars
      
      step <- 0.01
      # Rows that would be additionally excluded if threshold tightens by one step
      marginal_tighter <- which(obs_miss >= max(0, input$obs_thresh - step) &
                                obs_miss <  input$obs_thresh)
      # Rows that would be recovered if threshold loosens by one step
      marginal_looser  <- which(obs_miss >= input$obs_thresh &
                                obs_miss <  min(1, input$obs_thresh + step))

      list(
        df_final         = df_final,
        var_miss         = var_miss,
        excl_vars        = excl_vars,
        prot_vars        = prot_vars,
        obs_miss         = obs_miss,
        excl_rows        = excl_rows,
        obs_id_col       = obs_id_col,
        marginal_tighter = marginal_tighter,
        marginal_looser  = marginal_looser
      )
    })
    
    # ── main UI ───────────────────────────────────────────────────────────
    
    output$main_ui <- renderUI({
      req(result())
      res <- result()
      df  <- get_data()
      
      n_excl_vars <- length(res$excl_vars)
      n_prot_vars <- length(res$prot_vars)
      n_excl_rows <- length(res$excl_rows)
      n_remain    <- nrow(res$df_final)
      n_cols      <- ncol(res$df_final)
      
      # ── summary cards ────────────────────────────────────────────────────
      
      card <- function(label, value, color) {
        tags$div(
          style = paste0(
            "flex:1; min-width:130px; background:white; border-radius:10px;",
            "border:0.5px solid #dee2e6; padding:14px 18px;",
            "box-shadow:0 1px 3px rgba(0,0,0,0.06);"
          ),
          tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                            text-transform:uppercase; letter-spacing:.5px;", label),
          tags$div(style = paste0("font-size:28px; font-weight:700; color:", color, ";"), value)
        )
      }
      
      cards <- tags$div(
        style = "display:flex; gap:12px; margin-bottom:20px; flex-wrap:wrap;",
        card("Vars Excluded",   n_excl_vars, "#C41E3A"),
        card("Vars Protected",  n_prot_vars, "#BA7517"),
        card("Rows Excluded",   n_excl_rows, "#993556"),
        card("Rows Remaining",  n_remain,    "#0F6E56"),
        card("Cols Remaining",  n_cols,      "#185FA5")
      )
      
      # ── protected vars warning ────────────────────────────────────────────
      
      prot_warn <- if (n_prot_vars > 0) {
        tags$div(
          style = "background:#FAEEDA; border:0.5px solid #FAC775; border-radius:8px;
                   padding:10px 14px; margin-bottom:16px; font-size:13px; color:#633806;",
          icon("shield-halved", style = "color:#BA7517;"),
          HTML(paste0(
            " <strong>", paste(res$prot_vars, collapse = ", "), "</strong>",
            " exceed the variable threshold but are <strong>protected</strong> as domain important variables and will not be excluded."
          ))
        )
      } else NULL
      
      # ── no exclusions note ────────────────────────────────────────────────
      
      no_excl_note <- if (n_excl_vars == 0 && n_excl_rows == 0) {
        tags$div(
          style = "background:#E1F5EE; border:0.5px solid #9FE1CB; border-radius:8px;
                   padding:10px 14px; margin-bottom:16px; font-size:13px; color:#085041;",
          icon("circle-check", style = "color:#0F6E56;"),
          " No variables or observations exceed the current thresholds."
        )
      } else NULL
      
      # ── excluded variables table ──────────────────────────────────────────
      
      var_section <- tags$div(
        style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
           padding:16px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
        tags$h5(icon("table-columns", style = "color:#C41E3A; margin-right:6px;"),
                "Step 1 — Excluded Variables",
                style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
        if (n_excl_vars == 0)
          tags$p("No variables excluded at current threshold.",
                 style = "color:#adb5bd; font-size:13px;")
        else
          tagList(
            tags$div(
              style = "font-size:12px; color:#0C447C; background:#E6F1FB;
                 border-left:4px solid #185FA5; border-radius:4px;
                 padding:8px 12px; margin-bottom:12px;",
              icon("user-doctor", style = "color:#185FA5;"),
              " Please consult a domain expert before removing these variables — 
              they might be important variables that should stay no matter what."
            ),
            DT::dataTableOutput(ns("excl_vars_tbl"))
          )
      )
      
      # ── excluded observations table ───────────────────────────────────────
      
      obs_section <- tags$div(
        style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                 padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
        tags$h5(icon("list", style = "color:#993556; margin-right:6px;"),
                "Step 2 — Excluded Observations",
                style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
        if (n_excl_rows == 0)
          tags$p("No observations excluded at current threshold.",
                 style = "color:#adb5bd; font-size:13px;")
        else
          DT::dataTableOutput(ns("excl_obs_tbl"))
      )
      
      # ── marginal observations ─────────────────────────────────────────────
      fmt_ids <- function(rows) {
        if (length(rows) == 0) return(tags$span("None", style = "color:#adb5bd;"))
        ids <- if (!is.null(res$obs_id_col) && res$obs_id_col %in% names(df))
          as.character(df[[res$obs_id_col]][rows])
        else
          as.character(rows)
        tags$span(paste(ids, collapse = ", "),
                  style = "font-family:monospace; font-size:12px; color:#495057;")
      }

      n_tighter <- length(res$marginal_tighter)
      n_looser  <- length(res$marginal_looser)

      marginal_section <- tags$div(
        style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                 padding:16px; margin-top:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
        tags$h5(
          icon("arrows-up-down", style = "color:#6c757d; margin-right:6px;"),
          paste0("Marginal Observations (±1 step = ±0.01)"),
          style = "font-weight:600; color:#343a40; margin-bottom:4px;"
        ),
        tags$p(
          paste0("Current threshold: ", input$obs_thresh),
          style = "font-size:12px; color:#6c757d; margin-bottom:12px;"
        ),
        # Tighter: would be additionally excluded
        tags$div(
          style = "background:#fff5f5; border-left:4px solid #993556; border-radius:6px;
                   padding:10px 14px; margin-bottom:10px; font-size:13px; color:#343a40;",
          icon("arrow-down", style = "color:#993556;"),
          HTML(paste0(
            " <b>Tighten by 0.01 (→ ", round(input$obs_thresh - 0.01, 2), ")</b>",
            " &nbsp;—&nbsp; <b style='color:#993556;'>", n_tighter, "</b> more row",
            if (n_tighter != 1) "s" else "", " would be excluded"
          )),
          if (n_tighter > 0)
            tags$div(style = "margin-top:6px;", fmt_ids(res$marginal_tighter))
        ),
        # Looser: would be recovered
        tags$div(
          style = "background:#f0fff4; border-left:4px solid #0F6E56; border-radius:6px;
                   padding:10px 14px; font-size:13px; color:#343a40;",
          icon("arrow-up", style = "color:#0F6E56;"),
          HTML(paste0(
            " <b>Loosen by 0.01 (→ ", round(input$obs_thresh + 0.01, 2), ")</b>",
            " &nbsp;—&nbsp; <b style='color:#0F6E56;'>", n_looser, "</b> row",
            if (n_looser != 1) "s" else "", " would be recovered"
          )),
          if (n_looser > 0)
            tags$div(style = "margin-top:6px;", fmt_ids(res$marginal_looser))
        )
      )

      tagList(cards, prot_warn, no_excl_note, var_section, obs_section, marginal_section)
    })
    
    # ── excluded vars table ───────────────────────────────────────────────
    
    output$excl_vars_tbl <- DT::renderDataTable({
      req(result())
      res <- result()
      req(length(res$excl_vars) > 0)
      
      tbl <- data.frame(
        Variable          = res$excl_vars,
        Missing_Pct       = paste0(round(res$var_miss[res$excl_vars] * 100, 1), "%"),
        stringsAsFactors  = FALSE
      )
      tbl <- tbl[order(-res$var_miss[res$excl_vars]), ]
      
      DT::datatable(tbl, options = list(pageLength = 10, dom = "tip"),
                    rownames = FALSE) |>
        DT::formatStyle("Missing_Pct", color = "#C41E3A", fontWeight = "bold")
    })
    
    # ── excluded obs table ────────────────────────────────────────────────
    
    output$excl_obs_tbl <- DT::renderDataTable({
      req(result())
      res <- result()
      req(length(res$excl_rows) > 0)
      df  <- get_data(); req(df)

      # Base columns: row index + missingness %
      tbl <- data.frame(
        Row         = res$excl_rows,
        Missing_Pct = paste0(round(res$obs_miss[res$excl_rows] * 100, 1), "%"),
        stringsAsFactors = FALSE
      )

      # Prepend obs_id column (from roles) as the first column so excluded
      # observations are identified by their meaningful ID, not just row numbers
      obs_id_col <- res$obs_id_col
      if (!is.null(obs_id_col) && obs_id_col %in% names(df)) {
        id_vals <- df[res$excl_rows, obs_id_col, drop = FALSE]
        tbl     <- cbind(id_vals, tbl)
      }

      # Append any user-selected extra columns (skip obs_id if already shown)
      extra <- setdiff(intersect(input$extra_cols, names(df)),
                       if (!is.null(obs_id_col)) obs_id_col else character(0))
      if (length(extra) > 0) {
        extra_df <- df[res$excl_rows, extra, drop = FALSE]
        tbl      <- cbind(tbl, extra_df)
      }

      tbl <- tbl[order(-res$obs_miss[res$excl_rows]), ]

      DT::datatable(tbl, options = list(pageLength = 10, dom = "tip", scrollX = TRUE),
                    rownames = FALSE) |>
        DT::formatStyle("Missing_Pct", color = "#993556", fontWeight = "bold")
    })
    
    # ── return ────────────────────────────────────────────────────────────
    
    return(list(data = reactive(result()$df_final)))
    
  })
}