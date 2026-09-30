# =================================================================================
# mod_eda_heatmap.R
# =================================================================================

# ── HELPER ───────────────────────────────────────────────────────────────────

plot_corrgram <- function(cor_mat, order, title = NULL, label_cex = 0.9) {
  par(oma = c(0, 0, 3, 0))
  corrgram::corrgram(
    cor_mat,
    order       = if (order == "FALSE") FALSE else order,
    abs         = FALSE,
    cex.labels  = label_cex,
    font.labels = 2
  )
  if (!is.null(title)) {
    title(main = title, cex.main = 1.8, font.main = 2, outer = TRUE, line = 1)
  }
}

plot_miss_cor <- function(df, order, abs_cor, method = "pearson", label_cex = 0.9,
                          custom_title = NULL) {
  m  <- is.na(df) + 0
  cm <- colMeans(m)
  m  <- m[, cm > 0 & cm < 1, drop = FALSE]
  
  if (ncol(m) < 2) {
    plot.new()
    text(0.5, 0.5,
         "Need at least 2 partially-missing variables\nto compute missingness correlation.",
         cex = 1.2, col = "#C41E3A", adj = 0.5)
    return()
  }
  
  cor_m <- cor(m, method = method)
  if (abs_cor) cor_m <- abs(cor_m)
  
  auto_title <- paste0("Variable Missingness Correlation | ", tools::toTitleCase(method),
                       if (abs_cor) " | Absolute" else "")
  plot_corrgram(
    cor_mat   = cor_m,
    order     = order,
    title     = if (!is.null(custom_title) && nzchar(custom_title)) custom_title else auto_title,
    label_cex = label_cex
  )
}

plot_var_cor <- function(df, vars, method, order, abs_cor, label_cex = 0.9,
                         custom_title = NULL) {
  num_vars <- vars[sapply(vars, function(v) is.numeric(df[[v]]))]
  
  if (length(num_vars) < 2) {
    plot.new()
    text(0.5, 0.5,
         "Need at least 2 numeric variables\nto compute correlation.",
         cex = 1.2, col = "#C41E3A", adj = 0.5)
    return()
  }
  
  df_num  <- df[, num_vars, drop = FALSE]
  cor_mat <- cor(df_num, use = "pairwise.complete.obs", method = method)
  if (abs_cor) cor_mat <- abs(cor_mat)
  
  auto_title <- paste0("Variable Correlation Heatmap | ", tools::toTitleCase(method),
                       if (abs_cor) " | Absolute" else "")
  plot_corrgram(
    cor_mat   = cor_mat,
    order     = order,
    title     = if (!is.null(custom_title) && nzchar(custom_title)) custom_title else auto_title,
    label_cex = label_cex
  )
}


# ── UI ───────────────────────────────────────────────────────────────────────

eda_heatmap_ui <- function(id) {
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
             <b>Variable Correlation</b>: Pairwise correlations between numeric variables. <br><br>
             <b>Missingness Correlation</b>: Variables converted to binary (1 = missing, 0 = present),
             then correlated. Only partially-missing variables are shown.")
      ),
      hr(),
      
      radioButtons(ns("cor_view"), "View:",
                   choices  = c("Variable Correlation"    = "varcor",
                                "Missingness Correlation" = "misscor"),
                   selected = "varcor"),
      hr(),
      
      selectizeInput(ns("cor_vars"), "Variables to include:",
                     choices  = NULL,
                     multiple = TRUE),
      hr(),
      
      selectInput(ns("cor_method"), "Correlation method:",
                  choices  = c("Pearson"  = "pearson",
                               "Spearman" = "spearman",
                               "Kendall"  = "kendall"),
                  selected = "spearman"),
      hr(),
      
      selectInput(ns("cor_order"), "Variable ordering:",
                  choices  = c("OLO (Optimal Leaf)" = "OLO",
                               "AOE (Eigenvector)"  = "TRUE",
                               "HC (Hierarchical)"  = "HC",
                               "Original"           = "FALSE"),
                  selected = "OLO"),
      hr(),
      
      checkboxInput(ns("cor_abs"), "Absolute correlation", value = TRUE),
      hr(),
      
      sliderInput(ns("cor_label_size"), "Diagonal label size:",
                  min = 0.3, max = 2.5, value = 1.5, step = 0.1, width = "100%"),
      hr(),
      
      textInput(ns("cor_title"), "Custom plot title:", placeholder = "Auto-generated if empty")
    ),
    mainPanel(
      width = 9,
      plotOutput(ns("cor_output"), height = "85vh")
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

eda_heatmap_server <- function(id, get_data, roles = NULL) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      df <- get_data(); req(df)
      
      default_vars <- if (!is.null(roles)) {
        role_vals <- roles()
        intersect(names(role_vals[role_vals %in% c("predictor", "outcome",
                                                   "Predictor", "Outcome")]),
                  names(df))
      } else {
        names(df)
      }
      
      updateSelectizeInput(session, "cor_vars",
                           choices  = names(df),
                           selected = default_vars,
                           server   = TRUE)
    })
    
    output$cor_output <- renderPlot({
      req(get_data(), input$cor_vars)
      df  <- get_data()
      sel <- intersect(input$cor_vars, names(df))
      req(length(sel) >= 2)
      df_sel       <- df[, sel, drop = FALSE]
      custom_title <- if (nzchar(input$cor_title)) input$cor_title else NULL
      
      if (input$cor_view == "misscor") {
        plot_miss_cor(df_sel,
                      order        = input$cor_order,
                      abs_cor      = isTRUE(input$cor_abs),
                      method       = input$cor_method,
                      label_cex    = input$cor_label_size,
                      custom_title = custom_title)
      } else {
        plot_var_cor(df_sel,
                     vars         = names(df_sel),
                     method       = input$cor_method,
                     order        = input$cor_order,
                     abs_cor      = isTRUE(input$cor_abs),
                     label_cex    = input$cor_label_size,
                     custom_title = custom_title)
      }
    })
    
  })
}