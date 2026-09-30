# =================================================================================
# mod_eda_ggpairs.R
# =================================================================================

# ── HELPER ───────────────────────────────────────────────────────────────────

plot_ggpairs <- function(df, vars, group_on, group_var, group_levels, fontsize = 14,
                         cardinality_threshold = 15, custom_title = NULL) {
  sel <- intersect(vars, names(df))
  
  sel <- sel[sapply(sel, function(v) {
    x <- df[[v]]
    if (is.factor(x) || is.character(x)) length(unique(na.omit(x))) <= cardinality_threshold
    else TRUE
  })]
  
  if (length(sel) < 2) {
    ggplot() +
      annotate("text", x = 0.5, y = 0.5,
               label = "Not enough plottable variables.\nHigh-cardinality factors were excluded.",
               colour = "#C41E3A", size = 5) +
      theme_void()
    return()
  }
  
  if (group_on && !is.null(group_var) && group_var %in% names(df)) {
    plot_df <- df[, c(sel, group_var), drop = FALSE]
    plot_df <- plot_df[!is.na(plot_df[[group_var]]), ]
    if (!is.null(group_levels) && length(group_levels) > 0) {
      plot_df <- plot_df[as.character(plot_df[[group_var]]) %in% group_levels, ]
      plot_df[[group_var]] <- droplevels(as.factor(plot_df[[group_var]]))
    }
    col_idx    <- seq_along(sel)
    auto_title <- paste0("GGPairs | Grouped by ", group_var)
    GGally::ggpairs(
      plot_df, progress = FALSE, columns = col_idx,
      mapping = aes(colour = .data[[group_var]], alpha = 0.6),
      upper   = list(continuous = GGally::wrap("cor", size = 4),
                     combo      = GGally::wrap("box_no_facet", alpha = 0.5),
                     discrete   = GGally::wrap("facetbar", alpha = 0.5)),
      lower   = list(continuous = GGally::wrap("points", alpha = 0.3, size = 0.8),
                     combo      = GGally::wrap("facethist", bins = 20, alpha = 0.5),
                     discrete   = GGally::wrap("facetbar", alpha = 0.5)),
      diag    = list(continuous = GGally::wrap("densityDiag", alpha = 0.5),
                     discrete   = GGally::wrap("barDiag", alpha = 0.5)),
      legend  = 1
    ) +
      theme_minimal(base_size = fontsize) +
      theme(plot.title   = element_text(size = fontsize + 6, face = "bold", hjust = 0.5),
            strip.text.x = element_text(size = fontsize, face = "bold"),
            strip.text.y = element_text(size = fontsize, face = "bold", angle = 0),
            axis.text.x  = element_text(size = fontsize, angle = 90, vjust = 0.5, hjust = 1),
            axis.text.y  = element_text(size = fontsize),
            legend.title = element_text(face = "bold"),
            legend.text  = element_text(size = fontsize - 2)) +
      labs(title = if (!is.null(custom_title) && nzchar(custom_title)) custom_title else auto_title)
    
  } else {
    GGally::ggpairs(
      df[, sel, drop = FALSE], progress = FALSE,
      upper = list(continuous = GGally::wrap("cor", size = 4)),
      lower = list(continuous = GGally::wrap("points", alpha = 0.4, size = 0.8)),
      diag  = list(continuous = GGally::wrap("densityDiag"))
    ) +
      theme_minimal(base_size = fontsize) +
      theme(plot.title   = element_text(size = fontsize + 6, face = "bold", hjust = 0.5),
            strip.text.x = element_text(size = fontsize, face = "bold"),
            strip.text.y = element_text(size = fontsize, face = "bold", angle = 0),
            axis.text.x  = element_text(size = fontsize, angle = 90, vjust = 0.5, hjust = 1),
            axis.text.y  = element_text(size = fontsize),
            legend.title = element_text(face = "bold"),
            legend.text  = element_text(size = fontsize - 2)) +
      labs(title = if (!is.null(custom_title) && nzchar(custom_title)) custom_title else "GGPairs")
  }
}


# ── UI ───────────────────────────────────────────────────────────────────────

eda_ggpairs_ui <- function(id) {
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
             Pairwise relationships, correlations, and marginal distributions
             across multiple variables. Useful for detecting suspicious
             relationships and structural irregularities early. <br><br>
             <b>Tip:</b> Assign the Outcome role in the Config tab first —
             it will auto-select as the grouping variable here.")
      ),
      hr(),
      
      selectizeInput(ns("gg_vars"), "Variables to plot:",
                     choices  = NULL,
                     multiple = TRUE),
      hr(),
      
      checkboxInput(ns("gg_group_on"), "Group by categorical variable", value = FALSE),
      conditionalPanel(
        condition = paste0("input['", ns("gg_group_on"), "'] == true"),
        selectInput(ns("gg_group_var"), "Grouping variable:", choices = NULL),
        selectizeInput(ns("gg_group_levels"), "Levels to include:",
                       choices  = NULL,
                       multiple = TRUE,
                       options  = list(placeholder = "All levels included by default"))
      ),
      hr(),
      
      sliderInput(ns("gg_fontsize"), "Label font size:",
                  min = 6, max = 24, value = 10, step = 1, width = "100%"),
      hr(),
      
      textInput(ns("gg_title"), "Custom plot title:", placeholder = "Auto-generated if empty"),
      hr(),
      
      actionButton(ns("gg_run"), "Plot", icon = icon("play"), width = "100%"),
      helpText("Large selections may be slow.")
    ),
    mainPanel(
      width = 9,
      plotOutput(ns("gg_output"), height = "80vh")
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

eda_ggpairs_server <- function(id, get_data, roles = NULL) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      df <- get_data(); req(df)
      
      role_cols <- if (!is.null(roles)) {
        role_vals <- roles()
        intersect(names(role_vals[role_vals %in% c("predictor", "outcome",
                                                   "Predictor", "Outcome")]),
                  names(df))
      } else {
        names(df)
      }
      
      outcome_col <- if (!is.null(roles)) {
        role_vals <- roles()
        names(role_vals[role_vals %in% c("outcome", "Outcome")])
      } else {
        character(0)
      }
      
      cat_cols <- names(df)[sapply(df, is.factor)]
      
      updateSelectizeInput(session, "gg_vars",
                           choices  = role_cols,
                           selected = role_cols,
                           server   = TRUE)
      
      updateSelectInput(session, "gg_group_var",
                        choices  = cat_cols,
                        selected = if (length(outcome_col) > 0 && outcome_col[1] %in% cat_cols)
                          outcome_col[1] else if (length(cat_cols) > 0) cat_cols[1] else NULL)
    })
    
    observeEvent(input$gg_group_var, {
      req(input$gg_group_var)
      df <- get_data(); req(df)
      if (!input$gg_group_var %in% names(df)) return()
      lvls <- levels(df[[input$gg_group_var]])
      updateSelectizeInput(session, "gg_group_levels",
                           choices  = lvls,
                           selected = lvls)
    })
    
    output$gg_output <- renderPlot({
      input$gg_run
      isolate({
        req(get_data(), input$gg_vars)
        validate(need(length(input$gg_vars) >= 2, "Select at least 2 variables."))
        plot_ggpairs(
          df           = get_data(),
          vars         = input$gg_vars,
          group_on     = isTRUE(input$gg_group_on),
          group_var    = input$gg_group_var,
          group_levels = input$gg_group_levels,
          fontsize     = input$gg_fontsize,
          custom_title = if (nzchar(input$gg_title)) input$gg_title else NULL
        )
      })
    })
    
  })
}