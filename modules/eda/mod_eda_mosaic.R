# =================================================================================
# mod_eda_mosaic.R
# =================================================================================

# ── HELPER ───────────────────────────────────────────────────────────────────

plot_mosaic <- function(df, x_var, y_var, z_var, shade, rot_labels, abbreviate,
                        fontsize, custom_title = NULL) {
  if (z_var == "None") {
    tbl <- table(df[[x_var]], df[[y_var]])
    names(dimnames(tbl)) <- c(x_var, y_var)
  } else {
    tbl <- table(df[[x_var]], df[[y_var]], df[[z_var]])
    names(dimnames(tbl)) <- c(x_var, y_var, z_var)
  }
  
  vars_used  <- c(x_var, y_var, if (z_var != "None") z_var)
  auto_title <- paste0("Mosaic | ", paste(vars_used, collapse = " \u00d7 "))
  
  vcd::mosaic(tbl,
              shade    = shade,
              legend   = shade,
              main     = if (!is.null(custom_title) && nzchar(custom_title)) custom_title else auto_title,
              main_gp  = grid::gpar(fontsize = 18, fontface = "bold"),
              labeling = vcd::labeling_border(
                rot_labels = c(rot_labels, 0, 0, 0),
                gp_labels  = grid::gpar(fontsize = fontsize),
                abbreviate = abbreviate
              ))
}


# ── UI ───────────────────────────────────────────────────────────────────────

eda_mosaic_ui <- function(id) {
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
             Visualise dependency between categorical variables.
             Tile area reflects joint frequency; shading by residuals
             reveals where observed counts deviate from independence.")
      ),
      hr(),
      
      sliderInput(ns("mosaic_max_levels"), "Max levels per variable:",
                  min = 2, max = 20, value = 8, step = 1, width = "100%"),
      hr(),
      
      uiOutput(ns("mosaic_x_ui")),
      uiOutput(ns("mosaic_y_ui")),
      uiOutput(ns("mosaic_z_ui")),
      hr(),
      
      checkboxInput(ns("mosaic_shade"), "Shade by residuals", value = TRUE),
      hr(),
      
      sliderInput(ns("mosaic_rot_labels"), "Rotate Variable 1 labels:",
                  min = 0, max = 360, value = 0, step = 15, width = "100%"),
      checkboxInput(ns("mosaic_abbreviate"), "Abbreviate labels", value = TRUE),
      sliderInput(ns("mosaic_fontsize"), "Label font size:",
                  min = 6, max = 24, value = 10, step = 1, width = "100%"),
      hr(),
      
      textInput(ns("mosaic_title"), "Custom plot title:", placeholder = "Auto-generated if empty")
    ),
    mainPanel(
      width = 9,
      plotOutput(ns("mosaic_output"), height = "80vh")
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

eda_mosaic_server <- function(id, get_data, roles = NULL) {
  moduleServer(id, function(input, output, session) {
    
    mosaic_cat_cols <- reactive({
      df       <- get_data(); req(df)
      max_lvls <- input$mosaic_max_levels; req(max_lvls)
      cols <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
      cols[sapply(cols, function(v) length(unique(na.omit(df[[v]]))) <= max_lvls)]
    })
    
    output$mosaic_x_ui <- renderUI({
      cols <- mosaic_cat_cols()
      selectInput(session$ns("mosaic_x"), "Variable 1 (columns):", choices = cols,
                  selected = if (length(cols) >= 1) cols[1] else NULL)
    })
    
    output$mosaic_y_ui <- renderUI({
      cols <- mosaic_cat_cols()
      selectInput(session$ns("mosaic_y"), "Variable 2 (rows):", choices = cols,
                  selected = if (length(cols) >= 2) cols[2] else cols[1])
    })
    
    output$mosaic_z_ui <- renderUI({
      cols <- mosaic_cat_cols()
      selectInput(session$ns("mosaic_z"), "Variable 3 — optional (sub-rows):",
                  choices = c("None", cols), selected = "None")
    })
    
    output$mosaic_output <- renderPlot({
      req(get_data(), input$mosaic_x, input$mosaic_y, input$mosaic_z)
      plot_mosaic(
        df           = get_data(),
        x_var        = input$mosaic_x,
        y_var        = input$mosaic_y,
        z_var        = input$mosaic_z,
        shade        = isTRUE(input$mosaic_shade),
        rot_labels   = input$mosaic_rot_labels,
        abbreviate   = isTRUE(input$mosaic_abbreviate),
        fontsize     = input$mosaic_fontsize,
        custom_title = if (nzchar(input$mosaic_title)) input$mosaic_title else NULL
      )
    })
    
  })
}