# =================================================================================
# mod_miss_importance.R
# =================================================================================

# ── HELPER ───────────────────────────────────────────────────────────────────

# plot missingness variable importance
plot_miss_importance <- function(df, id_col, response_col) {
  
  df <- df[order(df[[id_col]]), ]
  df[[id_col]] <- seq_len(nrow(df))
  df$missingness <- apply(X = is.na(df), MARGIN = 1, FUN = sum)
  
  tree <- caret::train(
    missingness ~ .,
    data      = df,
    method    = "rpart",
    na.action = na.rpart
  )
  
  imp <- caret::varImp(tree)$importance
  imp$variables  <- rownames(imp)
  imp$importance <- imp$Overall / sum(imp$Overall)
  imp <- imp[imp$importance > 0, ]
  imp <- imp[order(-imp$importance), ]
  
  interesting_vars <- c(id_col)
  if (!is.null(response_col) && response_col != "(none)") interesting_vars <- c(interesting_vars, response_col)
  imp$interesting  <- imp$variables %in% interesting_vars
  imp$variables    <- factor(imp$variables, levels = imp$variables)
  
  ggplot(imp, aes(x = variables, y = importance, fill = interesting)) +
    geom_col() +
    scale_fill_manual(values = c("FALSE" = "#e05c4b", "TRUE" = "#36b5a2")) +
    labs(
      title = "Variable Importance for Predicting Missing Variables Per Observation",
      x     = "Variables",
      y     = "Relative Importance",
      fill  = "Interesting"
    ) +
    theme_minimal() +
    theme(
      plot.title       = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.text.x      = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
      axis.text.y      = element_text(size = 11),
      axis.title.x     = element_text(size = 13, face = "bold"),
      axis.title.y     = element_text(size = 13, face = "bold"),
      legend.title     = element_text(size = 12, face = "bold"),
      legend.text      = element_text(size = 11, face = "bold")
    )
}



# ── UI ───────────────────────────────────────────────────────────────────────

miss_importance_ui <- function(id) {
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
        HTML("&nbsp; <b>Miss Importance</b><br><br>
              Variable importance for predicting the number of missing values per
              observation.<br><br>
              If <b>any bars appear</b>, MCAR is ruled out — missingness is likely
              MAR or MNAR.<br><br>
              <b style='color:#36b5a2;'>Teal</b> bars are interesting:<br><br>
              &nbsp;&bull; <b>Response variable (y)</b> appearing suggests
              <b>informative missingness</b> — the fact that values are missing is
              predictive of y. Create shadow variables before imputing.<br><br>
              &nbsp;&bull; <b>Row index</b> appearing suggests
              <b>sequential missingness</b> — needs domain knowledge to explain.")
      ),
      div(
        style = "font-size: 12px; color: #495057; background: white;
                 padding: 8px 10px; border-left: 4px solid #ffc107;
                 border-radius: 6px; margin-bottom: 12px;",
        icon("triangle-exclamation", style = "color:#ffc107;"),
        HTML(" rpart handles missing predictors intrinsically —
              no cleaning is applied before this model.")
      ),
      hr(),
      
      tags$label("ID column (converted to numeric order):",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      selectInput(ns("id_col"), label = NULL, choices = NULL),
      hr(),
      
      tags$label("Response variable (y):",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      selectInput(ns("response_col"), label = NULL, choices = NULL)
    ),
    
    mainPanel(
      width = 9,
      style = "overflow-x: auto;",
      plotOutput(ns("plot"), height = "85vh")
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

miss_importance_server <- function(id, get_data, roles = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── Populate selectors ────────────────────────────────────────────────
    observe({
      df     <- get_data(); req(df)
      id_col   <- if (!is.null(roles)) { r <- roles(); names(r)[r == "obs_id"]  } else character(0)
      resp_col <- if (!is.null(roles)) { r <- roles(); names(r)[r == "outcome"] } else character(0)
      
      updateSelectInput(session, "id_col",
                        choices  = names(df),
                        selected = if (length(id_col)   > 0) id_col[1]   else names(df)[1])
      updateSelectInput(session, "response_col",
                        choices  = c("(none)", names(df)),
                        selected = if (length(resp_col) > 0) resp_col[1] else "(none)")
    })
    
    # ── Plot ──────────────────────────────────────────────────────────────
    output$plot <- renderPlot({
      req(input$id_col)
      df <- get_data(); req(df)
      
      has_miss <- sapply(df, anyNA)
      if (!any(has_miss)) {
        plot.new()
        text(0.5, 0.5, "No missing values in dataset — importance not applicable.",
             cex = 1.2, col = "#C41E3A", adj = 0.5)
        return()
      }
      
      plot_miss_importance(df, input$id_col, input$response_col)
    }, res = 96, height = 700)
    
  })
}