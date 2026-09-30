# =================================================================================
# mod_miss_rpart.R
# =================================================================================

library(rpart)

# ── UI ───────────────────────────────────────────────────────────────────────

miss_rpart_ui <- function(id) {
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
        HTML("&nbsp; <b>Miss Tree</b><br><br>
              Predicts the number of missing values per observation using an rpart tree.
              If missingness can be predicted from other variables, it is NOT missing
              completely at random (MCAR). <br><br>
              A single-node tree suggests MCAR cannot be ruled out. <br><br>
              Multiple nodes rule out MCAR, suggesting MAR or MNAR is likely. <br><br>
              When nodes include the response variable, it suggests
              <b>informative missingness</b> — create shadow variables. <br><br>
              When nodes include row index, it suggests
              <b>sequential missingness</b> — needs domain explanation.")
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
      
      # ── ID column ─────────────────────────────────────────────────────────
      tags$label("ID column (converted to numeric order):",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      selectInput(ns("id_col"), label = NULL, choices = NULL),
      hr(),
      
      # ── Variables ─────────────────────────────────────────────────────────
      tags$label("Variables to include as predictors:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      selectizeInput(ns("vars"), label = NULL,
                     choices = NULL, multiple = TRUE,
                     options = list(placeholder = "All variables included by default"))
    ),
    
    mainPanel(
      width = 9,
      style = "overflow-x: auto;",
      plotOutput(ns("plot"), height = "85vh")
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

miss_rpart_server <- function(id, get_data, roles = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── Populate selectors ────────────────────────────────────────────────
    observe({
      df     <- get_data(); req(df)
      id_col <- if (!is.null(roles)) {
        r <- roles()
        names(r)[r == "obs_id"]
      } else character(0)
      
      updateSelectInput(session, "id_col",
                        choices  = names(df),
                        selected = if (length(id_col) > 0) id_col[1] else names(df)[1])
      updateSelectizeInput(session, "vars",
                           choices  = names(df),
                           selected = names(df),
                           server   = TRUE)
    })
    
    # ── Plot ──────────────────────────────────────────────────────────────
    output$plot <- renderPlot({
      req(input$id_col)
      df <- get_data(); req(df)
      
      has_miss <- sapply(df, anyNA)
      if (!any(has_miss)) {
        plot.new()
        text(0.5, 0.5, "No missing values in dataset — tree not applicable.",
             cex = 1.2, col = "#C41E3A", adj = 0.5)
        return()
      }
      
      df <- df[order(df[[input$id_col]]), ]
      df[[input$id_col]] <- seq_len(nrow(df))
      df$missingness <- apply(X = is.na(df), MARGIN = 1, FUN = sum)
      
      if (!is.null(input$vars) && length(input$vars) > 0) {
        keep <- union(input$vars, c(input$id_col, "missingness"))
        df   <- df[, intersect(keep, names(df)), drop = FALSE]
      }
      
      tree <- caret::train(
        missingness ~ .,
        data      = df,
        method    = "rpart",
        na.action = na.rpart
      )
      
      rpart.plot::rpart.plot(
        tree$finalModel,
        main        = "Predicting the number of missing variables in an observation",
        sub         = "Check whether the outcome variable is an important variable",
        roundint    = TRUE,
        clip.facs   = TRUE,
        cex         = 1.2,
        branch      = 0.5,
        box.palette = "Blues"
      )
    }, res = 96, height = 700)
    
  })
}