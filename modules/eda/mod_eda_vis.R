# =================================================================================
# mod_eda_vis.R
# =================================================================================

# ── LIBRARY ──────────────────────────────────────────────────────────────────

library(plotly)
library(visdat)
library(naniar)
library(gridExtra)

# ── UI ───────────────────────────────────────────────────────────────────────

eda_vis_ui <- function(id) {
  ns <- NS(id)
  
  # global label weight fix
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
      
      # tab note
      
      div(
        style = "font-size: 13px; color: #343a40; background-color: white;
             padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
             margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; Tab Note: <br><br> Placeholder for now.")
      ),
      hr(),
      
      # variables
      
      tags$label("Variables to plot:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      selectizeInput(ns("vars"), label = NULL,
                     choices = NULL, multiple = TRUE),
      hr(),
      
      # downsample
      
      tags$label("Downsample limit (observations):",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      sliderInput(ns("downsample"), label = NULL,
                  min = 100, max = 10000, value = 10000,
                  step = 10, width = "100%"),
      hr(),
      
      # view mode
      
      tags$label("View:", style = "font-weight:600; font-size:13px; color:#343a40;"),
      radioButtons(ns("mode"), label = NULL,
                   choices = c(
                     "vis_dat (type + missingness)" = "visdat",
                     "vis_miss (missingness only)"  = "vismiss"
                   ),
                   selected = "visdat"),
      hr(),
      
      # ordering
      
      tags$label("Order:", style = "font-weight:600; font-size:13px; color:#343a40;"),
      checkboxInput(ns("sort_cols"), "Sort columns by missingness rate", value = FALSE),
      checkboxInput(ns("sort_rows"), "Sort rows by missingness pattern", value = FALSE),
      checkboxInput(ns("sort_by_col_on"), "Sort rows by a specific column", value = FALSE),
      conditionalPanel(
        condition = paste0("input['", ns("sort_by_col_on"), "'] == true"),
        selectInput(ns("sort_by_col"), label = NULL, choices = NULL)
      ),
      hr(),
      
      # grouping
      
      tags$label("Group by:", style = "font-weight:600; font-size:13px; color:#343a40;"),
      checkboxInput(ns("group_on"), "Group by categorical variable", value = FALSE),
      conditionalPanel(
        condition = paste0("input['", ns("group_on"), "'] == true"),
        selectInput(ns("group_var"), label = NULL, choices = NULL),
        selectizeInput(ns("group_levels"), label = NULL,
                       choices = NULL, multiple = TRUE,
                       options = list(placeholder = "All levels by default"))
      ),
      hr(),
      
      # little's mcar test
      
      tags$label("Little's MCAR test:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      checkboxInput(ns("mcar"), "Run MCAR test", value = FALSE),
      conditionalPanel(
        condition = paste0("input['", ns("mcar"), "'] == true"),
        div(
          style = "font-size: 12px; color: #495057; background: white;
                   padding: 8px 10px; border-left: 4px solid #0d6efd;
                   border-radius: 6px; margin-top: 6px;",
          HTML("H\u2080: data is MCAR. A significant result
               suggests MAR or MNAR.")
        )
      ),
      hr(),
      
      # custom title
      
      tags$label("Custom plot title:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      textInput(ns("custom_title"), label = NULL,
                placeholder = "Auto-generated if empty"),
      hr(),
      
      # custom x label size
      
      sliderInput(ns("x_text_size"),
                  "X label size:",
                  min = 6, max = 24, value = 13, step = 1)
    ),
    
    mainPanel(
      width = 9,
      plotOutput(ns("plot"), height = "85vh"),
      conditionalPanel(
        condition = paste0("input['", ns("mcar"), "'] == true"),
        hr(),
        h4("Little's MCAR Test Result"),
        verbatimTextOutput(ns("mcar_output"))
      )
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

eda_vis_server <- function(id, get_data, get_roles = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # observers
    
    # populate variable selector — default to predictors + outcome if roles available
    observe({
      df <- get_data()
      
      default_vars <- if (!is.null(get_roles)) {
        role_vals <- get_roles()
        vars <- names(role_vals[role_vals %in% c("predictor", "outcome")])
        intersect(vars, names(df))
      } else {
        names(df)
      }
      
      updateSelectizeInput(session, "vars",
                           choices  = names(df),
                           selected = default_vars,
                           server   = TRUE)
    }) |> bindEvent(get_data())
    
    # update downsample slider max
    observe({
      df <- get_data()
      updateSliderInput(session, "downsample",
                        max   = nrow(df),
                        value = nrow(df))
    })
    
    # populate grouping variable dropdown
    observe({
      req(input$group_on)
      df          <- get_data()
      factor_cols <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
      current     <- isolate(input$group_var)
      
      updateSelectInput(session, "group_var",
                        choices  = c("(none)", factor_cols),
                        selected = if (!is.null(current) &&
                                       current %in% c("(none)", factor_cols)) {
                          current
                        } else {
                          if (length(factor_cols) > 0) factor_cols[1] else "(none)"
                        })
    })
    
    # populate level selector only when a real variable is selected
    observeEvent(input$group_var, {
      req(input$group_var, input$group_var != "(none)")
      lvls <- unique(get_data()[[input$group_var]])
      updateSelectizeInput(session, "group_levels",
                           choices = lvls,
                           selected = lvls)
    })
    
    # populate sort-by-col dropdown
    observe({
      df <- get_data()
      updateSelectInput(session, "sort_by_col", choices = names(df))
    })
    
    # plot data reactive
    
    get_plot_data <- reactive({
      df <- get_data()
      req(df)
      
      # keep only valid columns
      valid_vars <- intersect(input$vars, names(df))
      req(length(valid_vars) > 0)
      
      if (isTRUE(input$sort_by_col_on) &&
          !is.null(input$sort_by_col) &&
          input$sort_by_col %in% names(df)) {
        df <- df[order(df[[input$sort_by_col]]), , drop = FALSE]
      }
      
      if (!is.null(input$downsample) && input$downsample < nrow(df)) {
        samp <- sort(sample(seq_len(nrow(df)), input$downsample))
        df   <- df[samp, , drop = FALSE]
      }
      
      df[, valid_vars, drop = FALSE]
    })
    
    # plot output
    
    output$plot <- renderPlot({
      req(input$mode)
      
      df      <- get_plot_data()
      full_df <- get_data()
      
      # resolve title
      title_text <- if (nzchar(trimws(input$custom_title))) {
        input$custom_title
      } else {
        base <- if (input$mode == "visdat") "Variable Types & Missingness" else "Missingness Pattern"
        
        suffixes <- c()
        if (isTRUE(input$sort_cols))       suffixes <- c(suffixes, "cols sorted by miss rate")
        if (isTRUE(input$sort_rows))       suffixes <- c(suffixes, "rows sorted by pattern")
        if (isTRUE(input$sort_by_col_on) && nzchar(input$sort_by_col))
          suffixes <- c(suffixes, paste("rows sorted by", input$sort_by_col))
        if (isTRUE(input$group_on) && nzchar(input$group_var))
          suffixes <- c(suffixes, paste("grouped by", input$group_var))
        
        if (length(suffixes) > 0) paste0(base, " | ", paste(suffixes, collapse = " | ")) else base
      }
      
      # shared theme
      plot_theme <- ggplot2::theme(
        plot.title    = ggplot2::element_text(size = 24, face = "bold", hjust = 0.5,
                                              margin = ggplot2::margin(b = 4)),
        plot.subtitle = ggplot2::element_text(size = 14, hjust = 0.5, color = "#6c757d"),
        axis.text.x   = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1,
                                              size  = input$x_text_size, face = "bold"),
        axis.text.y   = ggplot2::element_text(size = 14, face = "bold"),
        axis.title.x  = ggplot2::element_text(size = 14, face = "bold"),
        axis.title.y  = ggplot2::element_text(size = 14, face = "bold"),
        legend.text   = ggplot2::element_text(size = 13, face = "bold"),
        legend.title  = ggplot2::element_text(size = 14, face = "bold"),
        axis.ticks.length.x = ggplot2::unit(0, "pt"),
        plot.margin = ggplot2::margin(t = 10, r = 10, b = 80, l = 10)
      )
      
      make_plot <- function(df_sub, subtitle = NULL) {
        
        if (isTRUE(input$sort_rows)) {
          miss_pattern <- apply(df_sub, 1, function(x) paste(as.integer(is.na(x)), collapse = ""))
          df_sub <- df_sub[order(miss_pattern), , drop = FALSE]
        }
        
        if (input$mode == "visdat") {
          miss_pct  <- round(colMeans(is.na(df_sub)) * 100)
          new_names <- paste0(names(df_sub), " (", miss_pct, "%)")
          names(df_sub) <- new_names
        }
        
        p <- if (input$mode == "visdat") {
          visdat::vis_dat(df_sub)
        } else {
          visdat::vis_miss(df_sub, sort_miss = isTRUE(input$sort_cols))
        }
        
        p + ggplot2::labs(
          title    = if (is.null(subtitle)) title_text else subtitle,
          subtitle = if (!is.null(subtitle)) title_text else NULL
        ) + plot_theme
      }
      
      group_on <- isTRUE(input$group_on) &&
        !is.null(input$group_var) &&
        nzchar(input$group_var) &&
        input$group_var != "(none)" &&
        input$group_var %in% names(full_df)
      
      if (!group_on) {
        make_plot(df)
        
      } else {
        # build plot df with group var included so rows stay aligned
        req(input$vars)
        df_with_group <- get_data()
        
        if (isTRUE(input$sort_by_col_on) &&
            !is.null(input$sort_by_col) &&
            input$sort_by_col %in% names(df_with_group)) {
          df_with_group <- df_with_group[order(df_with_group[[input$sort_by_col]]), , drop = FALSE]
        }
        
        if (!is.null(input$downsample) && input$downsample < nrow(df_with_group)) {
          samp          <- sort(sample(seq_len(nrow(df_with_group)), input$downsample))
          df_with_group <- df_with_group[samp, , drop = FALSE]
        }
        
        lvls <- if (length(input$group_levels) > 0) {
          input$group_levels
        } else {
          levels(full_df[[input$group_var]])
        }
        
        if (length(lvls) > 6) {
          plot.new()
          text(0.5, 0.5,
               paste0("Too many levels to display (", length(lvls), ").\n",
                      "Please select 6 or fewer levels."),
               cex = 1.2, col = "#C41E3A", adj = 0.5)
          return()
        }
        
        plots <- lapply(lvls, function(lvl) {
          mask   <- as.character(df_with_group[[input$group_var]]) == lvl
          df_sub <- df_with_group[mask, input$vars, drop = FALSE]
          if (nrow(df_sub) == 0) return(NULL)
          make_plot(df_sub, subtitle = paste(input$group_var, "=", lvl))
        })
        
        plots <- Filter(Negate(is.null), plots)
        if (length(plots) == 0) return()
        
        n     <- length(plots)
        ncols <- min(n, 2)
        nrows <- ceiling(n / ncols)
        gridExtra::grid.arrange(grobs = plots, ncol = ncols, nrow = nrows)
      }
    })
    
    # little's mcar test
    
    output$mcar_output <- renderPrint({
      req(input$mcar)
      df     <- get_data()
      df_num <- df[, sapply(df, is.numeric), drop = FALSE]
      
      if (ncol(df_num) < 2) {
        cat("Need at least 2 numeric variables for Little's MCAR test.\n")
        return(invisible(NULL))
      }
      
      result <- naniar::mcar_test(df_num)
      
      cat("Little's MCAR Test\n")
      cat(sprintf("Chi-square : %.2f\n",  result$statistic))
      cat(sprintf("df         : %d\n",    result$df))
      cat(sprintf("p-value    : %.2e\n",  result$p.value))
      cat(sprintf("Patterns   : %d\n",    result$missing.patterns))
      cat("──────────────────────────────\n")
      if (result$p.value < 0.05) {
        cat("Conclusion : Reject H0 — data is NOT MCAR (likely MAR or MNAR).\n")
      } else {
        cat("Conclusion : Fail to reject H0 — data is consistent with MCAR.\n")
      }
    })
    
  })
}

