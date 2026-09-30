# =================================================================================
# mod_eda_tabplot.R
# =================================================================================

# ── HELPER ───────────────────────────────────────────────────────────────────

plot_tabplot <- function(df, vars, sort_on, sort_var, sort_desc, transform, nbin,
                         custom_title = NULL) {
  sel <- intersect(vars, names(df))
  if (length(sel) == 0) return(invisible(NULL))
  
  df_plot <- df[, sel, drop = FALSE]
  
  if (transform != "none") {
    num_cols <- names(df_plot)[sapply(df_plot, is.numeric)]
    df_plot[num_cols] <- lapply(df_plot[num_cols], function(y) {
      switch(transform,
             "center"      = y - mean(y, na.rm = TRUE),
             "standardise" = as.numeric(scale(y, center = TRUE, scale = TRUE)),
             "normalise"   = (y - min(y, na.rm = TRUE)) /
               (max(y, na.rm = TRUE) - min(y, na.rm = TRUE))
      )
    })
  }
  
  using_index <- !isTRUE(sort_on)
  
  if (using_index) {
    df_plot$.row_index <- seq_len(nrow(df_plot))
    sort_col <- ".row_index"
    df_plot  <- df_plot[, c(".row_index", setdiff(names(df_plot), ".row_index")), drop = FALSE]
  } else {
    sort_col <- sort_var
    if (!sort_col %in% names(df_plot)) df_plot[[sort_col]] <- df[[sort_col]]
    if (inherits(df_plot[[sort_col]], "Date")) df_plot[[sort_col]] <- as.numeric(df_plot[[sort_col]])
    df_plot <- df_plot[, c(sort_col, setdiff(names(df_plot), sort_col)), drop = FALSE]
  }
  
  auto_title <- paste0(
    "Tabplot | Sorted by ", if (using_index) "Row Order" else sort_var,
    if (transform != "none") paste0(" | ", tools::toTitleCase(transform)) else ""
  )
  
  tabplot::tableplot(
    df_plot,
    sortCol        = sort_col,
    decreasing     = if (using_index) FALSE else isTRUE(sort_desc),
    nBins          = nbin,
    title          = if (!is.null(custom_title) && nzchar(custom_title)) custom_title else auto_title,
    fontsize       = 14,
    fontsize.title = 22
  )
}


# ── UI ───────────────────────────────────────────────────────────────────────

eda_tabplot_ui <- function(id) {
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
             Visualises distributions of multiple variables simultaneously,
             sorted by a target variable. Useful for spotting patterns
             and how variables relate to the outcome.")
      ),
      hr(),
      
      selectizeInput(ns("tp_vars"), "Variables to plot:",
                     choices  = NULL,
                     multiple = TRUE),
      hr(),
      
      checkboxInput(ns("tp_sort_on"), "Sort by variable", value = TRUE),
      conditionalPanel(
        condition = paste0("input['", ns("tp_sort_on"), "'] == true"),
        selectInput(ns("tp_sortvar"), "Sort by variable:", choices = NULL),
        checkboxInput(ns("tp_decreasing"), "Sort descending", value = FALSE)
      ),
      hr(),
      
      radioButtons(ns("tp_transform"), "Transform numeric columns:",
                   choices  = c("None"        = "none",
                                "Centre"      = "center",
                                "Standardise" = "standardise",
                                "Normalise"   = "normalise"),
                   selected = "normalise"),
      hr(),
      
      sliderInput(ns("tp_nbin"), "Number of bins:",
                  min = 10, max = 500, value = 60, step = 10, width = "100%"),
      hr(),
      
      textInput(ns("tp_title"), "Custom plot title:", placeholder = "Auto-generated if empty")
    ),
    mainPanel(
      width = 9,
      plotOutput(ns("tp_output"), height = "80vh")
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

eda_tabplot_server <- function(id, get_data, roles = NULL) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      df        <- get_data(); req(df)
      all_cols  <- names(df)
      num_cols  <- names(df)[sapply(df, is.numeric)]
      date_cols <- names(df)[sapply(df, inherits, what = "Date")]
      
      default_vars <- if (!is.null(roles)) {
        role_vals <- roles()
        intersect(names(role_vals[role_vals %in% c("predictor", "outcome",
                                                   "Predictor", "Outcome")]),
                  all_cols)
      } else {
        all_cols
      }
      
      updateSelectizeInput(session, "tp_vars",
                           choices  = all_cols,
                           selected = default_vars,
                           server   = TRUE)
      
      sortable_vars <- c(date_cols, num_cols)
      updateSelectInput(session, "tp_sortvar",
                        choices  = sortable_vars,
                        selected = if (length(sortable_vars) > 0) sortable_vars[1] else NULL)
    })
    
    output$tp_output <- renderPlot({
      req(get_data(), input$tp_vars)
      plot_tabplot(
        df           = get_data(),
        vars         = input$tp_vars,
        sort_on      = isTRUE(input$tp_sort_on),
        sort_var     = input$tp_sortvar,
        sort_desc    = isTRUE(input$tp_decreasing),
        transform    = input$tp_transform,
        nbin         = input$tp_nbin,
        custom_title = if (nzchar(input$tp_title)) input$tp_title else NULL
      )
    })
    
  })
}