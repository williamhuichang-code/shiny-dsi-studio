# =================================================================================
# mod_eda_bar.R
# =================================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

eda_bar_ui <- function(id) {
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
        HTML("&nbsp; <b>Bar Chart</b><br><br>
              Inspect the frequency of each level in a categorical or
              discrete variable. Useful for spotting dominant categories,
              rare levels, and unexpected values.")
      ),
      hr(),
      
      # ── Variables ─────────────────────────────────────────────────────────
      tags$label("Variables:", style = "font-weight:600; font-size:13px; color:#343a40;"),
      selectizeInput(ns("vars"), label = NULL,
                     choices = NULL, multiple = TRUE),
      hr(),
      
      # ── Top N ─────────────────────────────────────────────────────────────
      tags$label("Levels to show:", style = "font-weight:600; font-size:13px; color:#343a40;"),
      sliderInput(ns("top_n"), label = NULL,
                  min = 1, max = 50, value = 8, step = 1, width = "100%"),
      hr(),
      
      # ── Include NA ────────────────────────────────────────────────────────
      checkboxInput(ns("include_na"), "Include NA as a level", value = FALSE),
      hr(),
      
      # ── Grouping ──────────────────────────────────────────────────────────
      tags$label("Group by:", style = "font-weight:600; font-size:13px; color:#343a40;"),
      checkboxInput(ns("group_on"), "Group by categorical variable", value = FALSE),
      conditionalPanel(
        condition = paste0("input['", ns("group_on"), "'] == true"),
        selectInput(ns("group_var"), label = NULL, choices = NULL)
      ),
      hr(),
      
      # ── Stack vars ────────────────────────────────────────────────────────
      checkboxInput(ns("stack_vars"),
                    "Stack by variable name (when multiple selected)",
                    value = FALSE),
      hr(),
      
      # ── Font sizes ────────────────────────────────────────────────────────
      tags$label("Axis text size:", style = "font-weight:600; font-size:13px; color:#343a40;"),
      sliderInput(ns("axis_text_size"), label = NULL,
                  min = 6, max = 24, value = 12, step = 1, width = "100%"),
      
      tags$label("Legend text size:", style = "font-weight:600; font-size:13px; color:#343a40;"),
      sliderInput(ns("legend_text_size"), label = NULL,
                  min = 6, max = 24, value = 11, step = 1, width = "100%"),
      
      tags$label("Title size:", style = "font-weight:600; font-size:13px; color:#343a40;"),
      sliderInput(ns("title_size"), label = NULL,
                  min = 8, max = 36, value = 16, step = 1, width = "100%"),
      hr(),
      
      # ── Custom title ──────────────────────────────────────────────────────
      tags$label("Custom plot title:", style = "font-weight:600; font-size:13px; color:#343a40;"),
      textInput(ns("custom_title"), label = NULL,
                placeholder = "Auto-generated if empty")
    ),
    
    mainPanel(
      width = 9,
      style = "overflow-x: auto;",
      plotOutput(ns("plot"), height = "80vh")
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

eda_bar_server <- function(id, get_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── Populate selectors ────────────────────────────────────────────────
    observe({
      df       <- get_data(); req(df)
      all_cols <- names(df)
      cat_cols <- names(df)[sapply(df, is.factor)]
      
      updateSelectizeInput(session, "vars",
                           choices  = all_cols,
                           selected = if (length(all_cols) > 0) all_cols[1] else NULL,
                           server   = TRUE)
      updateSelectInput(session, "group_var",
                        choices = cat_cols)
    })
    
    # ── Plot ──────────────────────────────────────────────────────────────
    output$plot <- renderPlot({
      req(length(input$vars) > 0)
      
      df       <- get_data(); req(df)
      vars     <- input$vars
      group_on <- isTRUE(input$group_on) &&
        !is.null(input$group_var) &&
        input$group_var %in% names(df)
      
      # auto title
      var_label <- if (length(vars) == 1) vars else paste0(length(vars), " variables")
      auto_title <- if (group_on) {
        paste("Bar Chart for", var_label, "grouped by", input$group_var)
      } else {
        paste("Bar Chart for", var_label)
      }
      plot_title <- if (nzchar(trimws(input$custom_title))) input$custom_title else auto_title
      
      # build long-format df
      long_df <- do.call(rbind, lapply(vars, function(v) {
        data.frame(value = as.character(df[[v]]), variable = v,
                   stringsAsFactors = FALSE)
      }))
      
      if (isTRUE(input$include_na)) {
        long_df$value[is.na(long_df$value)] <- "(NA)"
      } else {
        long_df <- long_df[!is.na(long_df$value), ]
      }
      
      counts_all <- sort(table(long_df$value), decreasing = TRUE)
      top_levels <- names(head(counts_all, input$top_n))
      long_df    <- long_df[long_df$value %in% top_levels, ]
      long_df$value <- factor(long_df$value, levels = top_levels)
      
      # shared theme
      bar_theme <- ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          plot.title   = ggplot2::element_text(size  = input$title_size,
                                               face  = "bold", hjust = 0.5,
                                               margin = ggplot2::margin(b = 12)),
          axis.text.x  = ggplot2::element_text(angle = 45, hjust = 1,
                                               size  = input$axis_text_size),
          axis.text.y  = ggplot2::element_text(size  = input$axis_text_size),
          axis.title.x = ggplot2::element_text(size  = input$axis_text_size + 2,
                                               face  = "bold"),
          axis.title.y = ggplot2::element_text(size  = input$axis_text_size + 2,
                                               face  = "bold"),
          legend.title = ggplot2::element_text(size  = input$legend_text_size + 1,
                                               face  = "bold"),
          legend.text  = ggplot2::element_text(size  = input$legend_text_size)
        )
      
      if (isTRUE(input$stack_vars) && length(vars) > 1) {
        long_df$variable <- factor(long_df$variable, levels = vars)
        fill_vals <- scales::hue_pal(l = 55, c = 40)(length(vars))
        
        ggplot2::ggplot(long_df, ggplot2::aes(x = value, fill = variable)) +
          ggplot2::geom_bar(position = "stack") +
          ggplot2::geom_text(
            stat = "count",
            ggplot2::aes(label = ggplot2::after_stat(count)),
            position = ggplot2::position_stack(vjust = 0.5),
            size = 4.5, fontface = "bold", check_overlap = TRUE
          ) +
          ggplot2::scale_fill_manual(values = setNames(fill_vals, vars)) +
          ggplot2::labs(title = plot_title, x = "Value", y = "Count", fill = "Variable") +
          bar_theme
        
      } else if (group_on) {
        grp_vec <- do.call(rbind, lapply(vars, function(v) {
          data.frame(value = as.character(df[[v]]),
                     grp   = as.character(df[[input$group_var]]),
                     stringsAsFactors = FALSE)
        }))
        if (!isTRUE(input$include_na)) grp_vec <- grp_vec[!is.na(grp_vec$value), ]
        grp_vec <- grp_vec[grp_vec$value %in% top_levels, ]
        grp_vec$value <- factor(grp_vec$value, levels = top_levels)
        grp_vec <- grp_vec[!is.na(grp_vec$grp), ]
        
        n_grp     <- length(unique(grp_vec$grp))
        fill_vals <- viridis::viridis(n_grp, option = "C", begin = 0.1, end = 0.9)
        
        ggplot2::ggplot(grp_vec, ggplot2::aes(x = value, fill = grp)) +
          ggplot2::geom_bar(position = "dodge") +
          ggplot2::scale_fill_manual(values = fill_vals) +
          ggplot2::labs(title = plot_title, x = "Value", y = "Count",
                        fill  = input$group_var) +
          bar_theme
        
      } else {
        plot_df <- data.frame(
          level = factor(names(table(long_df$value)[top_levels]), levels = top_levels),
          count = as.integer(table(long_df$value)[top_levels])
        )
        
        ggplot2::ggplot(plot_df, ggplot2::aes(x = level, y = count)) +
          ggplot2::geom_col() +
          ggplot2::geom_text(ggplot2::aes(label = count),
                             vjust = -0.4, fontface = "bold", size = 4.5) +
          ggplot2::labs(title = plot_title, x = "Value", y = "Count") +
          bar_theme
      }
    })
    
  })
}