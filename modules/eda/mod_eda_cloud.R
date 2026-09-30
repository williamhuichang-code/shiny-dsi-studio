# =================================================================================
# mod_eda_cloud.R
# =================================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

eda_cloud_ui <- function(id) {
  ns <- NS(id)
  
  label_fix <- tags$style(HTML("
    .shiny-input-container label,
    .radio label,
    .checkbox label {
      font-weight: 400 !important;
      font-size: 13px;
    }
  "))
  
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 3,
      style = "background-color: #f4f6fb; border-left: 3px solid #6a9fd8;
               min-height: 100vh; padding: 16px 14px;",
      
      label_fix,
      
      div(
        style = "font-size: 13px; color: #343a40; background-color: white;
                 padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
                 margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; <b>Word Cloud</b><br><br>
              Useful for spotting inconsistencies in variable names or values.
              Use <b>Check all values</b> mode to detect disguised NAs like
              <code>-99</code>, <code>--</code>, <code>?</code> appearing suspiciously often.")
      ),
      hr(),
      
      # ── Mode ──────────────────────────────────────────────────────────────
      tags$label("Mode:", style = "font-weight:600; font-size:13px; color:#343a40;"),
      radioButtons(ns("mode"), label = NULL,
                   choices = c(
                     "Categorical variable values" = "catvar",
                     "Check variable names"        = "varnames",
                     "Quick check for all values (character)" = "allvals"
                   ),
                   selected = "catvar"),
      hr(),
      
      # ── Variable selector (catvar only) ───────────────────────────────────
      conditionalPanel(
        condition = paste0("input['", ns("mode"), "'] == 'catvar'"),
        checkboxInput(ns("include_numeric"),
                      "Switch to numeric variables (converted to text, not recommended)",
                      value = FALSE),
        tags$label("Variables:", style = "font-weight:600; font-size:13px; color:#343a40;"),
        selectizeInput(ns("vars"), label = NULL,
                       choices = NULL, multiple = TRUE),
        hr()
      ),
      
      # ── Case sensitivity ──────────────────────────────────────────────────
      checkboxInput(ns("case"), "Case sensitive", value = TRUE),
      hr(),
      
      # ── Token split mode ──────────────────────────────────────────────────
      tags$label("Token split mode:", style = "font-weight:600; font-size:13px; color:#343a40;"),
      radioButtons(ns("split_mode"), label = NULL,
                   choices = c(
                     "None (whole value)"    = "none",
                     "Individual characters" = "chars",
                     "Alpha / numeric runs"  = "alphanum"
                   ),
                   selected = "none"),
      hr(),
      
      # ── Frequency controls ────────────────────────────────────────────────
      tags$label("Max words to show:", style = "font-weight:600; font-size:13px; color:#343a40;"),
      sliderInput(ns("max_words"), label = NULL,
                  min = 10, max = 500, value = 200, step = 10, width = "100%"),
      
      tags$label("Min frequency:", style = "font-weight:600; font-size:13px; color:#343a40;"),
      sliderInput(ns("min_freq"), label = NULL,
                  min = 1, max = 50, value = 1, step = 1, width = "100%"),
      helpText("Font size is proportional to frequency."),
      hr(),
      
      # ── Plot scale ────────────────────────────────────────────────────────
      tags$label("Plot scale:", style = "font-weight:600; font-size:13px; color:#343a40;"),
      sliderInput(ns("scale"), label = NULL,
                  min = 0.3, max = 3.0, value = 1.0, step = 0.1, width = "100%"),
      hr(),
      
      # ── Colour palette ────────────────────────────────────────────────────
      tags$label("Colour palette:", style = "font-weight:600; font-size:13px; color:#343a40;"),
      selectInput(ns("palette"), label = NULL,
                  choices  = c("Dark2", "Set1", "Set2", "Set3",
                               "Paired", "Accent", "Spectral"),
                  selected = "Dark2")
    ),
    
    mainPanel(
      width = 9,
      style = "overflow-x: auto;",
      plotOutput(ns("plot"), height = "80vh")
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

eda_cloud_server <- function(id, get_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── Populate variable selector ────────────────────────────────────────
    observe({
      df       <- get_data(); req(df)
      cat_cols <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
      num_cols <- names(df)[sapply(df, is.numeric)]
      choices  <- if (isTRUE(input$include_numeric)) num_cols else cat_cols
      updateSelectizeInput(session, "vars",
                           choices  = choices,
                           selected = if (length(choices) > 0) choices[1] else NULL,
                           server   = TRUE)
    })
    
    # ── Plot ──────────────────────────────────────────────────────────────
    output$plot <- renderPlot({
      req(input$mode)
      if (input$mode == "catvar") req(length(input$vars) > 0)
      
      df  <- get_data(); req(df)
      mode <- input$mode
      
      tryCatch({
        # collect tokens
        val <- switch(mode,
                      "varnames" = names(df),
                      "allvals"  = unlist(lapply(names(df), function(col) as.character(df[[col]]))),
                      "catvar"   = unlist(lapply(input$vars, function(v) as.character(df[[v]])))
        )
        
        val <- val[!is.na(val) & nchar(trimws(val)) > 0]
        if (!isTRUE(input$case)) val <- tolower(val)
        if (length(val) == 0) stop("No non-missing values to display.")
        
        tokens <- switch(input$split_mode,
                         "none"     = val,
                         "chars"    = { t <- unlist(strsplit(val, "")); t[!grepl("\\s", t)] },
                         "alphanum" = {
                           t <- unlist(lapply(val, function(tok) {
                             m <- gregexpr("[A-Za-z]+|[0-9]+", tok, perl = TRUE)
                             regmatches(tok, m)[[1]]
                           }))
                           t[nchar(t) > 0]
                         }
        )
        
        freq_table <- sort(table(tokens), decreasing = TRUE)
        freq_table <- freq_table[freq_table >= input$min_freq]
        if (length(freq_table) == 0)
          stop(paste0("No tokens appear at least ", input$min_freq, " times."))
        
        freq_table <- head(freq_table, input$max_words)
        words      <- names(freq_table)
        freqs      <- as.integer(freq_table)
        n          <- length(words)
        
        pal    <- RColorBrewer::brewer.pal(max(3, min(8, n)), input$palette)
        colors <- colorRampPalette(pal)(n)[rank(-freqs, ties.method = "first")]
        
        freq_ratio <- max(freqs) / max(median(freqs), 1)
        if (freq_ratio >= 3) {
          freqs_norm <- as.integer(20 + (sqrt(freqs) - sqrt(min(freqs))) /
                                     max(sqrt(max(freqs)) - sqrt(min(freqs)), 1) * 80)
        } else {
          freq_ranks <- rank(-freqs, ties.method = "first")
          freqs_norm <- as.integer(100 - (freq_ranks - 1) / max(freq_ranks - 1, 1) * 80)
        }
        
        n_eff      <- min(n, 30)
        base_scale <- max(3, min(9, 40 / sqrt(n_eff)))
        max_scale  <- min(12, base_scale * (1 + log10(max(freq_ratio, 1)))) * input$scale
        min_scale  <- max(0.3, max_scale * 0.15)
        
        par(mar = c(0, 0, 0, 0), bg = "white")
        wordcloud::wordcloud(
          words        = words,
          freq         = freqs_norm,
          max.words    = n,
          min.freq     = 1,
          random.order = FALSE,
          rot.per      = 0.15,
          colors       = colors,
          scale        = c(max_scale, min_scale),
          use.r.layout = FALSE
        )
        
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, conditionMessage(e), cex = 1.2, col = "#C41E3A", adj = 0.5)
      })
    })
    
  })
}