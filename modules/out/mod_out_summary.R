# =================================================================================
# mod_out_summary.R
# =================================================================================

out_summary_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width=3,
      style="background-color:#e8f0fe; border-left:2px solid #a8c0fd;
             min-height:100vh; padding-left:20px;",
      div(
        style="font-size:13px; color:#343a40; background:white; padding:10px;
               border-left:4px solid #0d6efd; border-radius:6px; margin-bottom:12px;",
        icon("info-circle", style="color:#0d6efd;"),
        HTML("&nbsp; <b>Outlier Summary</b><br><br>
              Aggregates flags from all active detection methods into a
              cumulative novelty score per observation.<br><br>
              Taller bars = flagged by more methods = more likely a true outlier.")
      ),
      hr(),
      selectInput(ns("id_col"), "ID / label column:", choices=NULL),
      hr(),
      checkboxGroupInput(ns("active_methods"), "Methods to include:",
                         choices  = c("Mahalanobis" = "mahalanobis",
                                      "Cook's Distance" = "cooks",
                                      "LOF"         = "lof",
                                      "SVM"         = "svm",
                                      "Random Forest" = "rf",
                                      "Isolation Forest" = "iforest"),
                         selected = c("mahalanobis", "cooks", "lof", "svm", "rf", "iforest")),
      hr(),
      numericInput(ns("min_count"), "Min flag count to display:",
                   value=3, min=1, step=1),
      helpText("Only show observations flagged by at least this many methods."),
      hr(),
      selectInput(ns("sort_by"), "Sort table by:",
                  choices  = c("Cumulative flag count" = "flag_count",
                               "Row index"             = "row_index"),
                  selected = "flag_count")
    ),
    mainPanel(
      width=9,
      plotOutput(ns("plot"), height="70vh"),
      hr(),
      h5("Outlier Rows (raw data)"),
      DT::dataTableOutput(ns("table"))
    )
  )
}

out_summary_server <- function(id, get_data, get_raw, roles,
                               flagged_mah, flagged_cooks, flagged_lof,
                               flagged_svm, flagged_rf, flagged_iforest,
                               seed = reactive(2026)) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      df     <- get_data(); req(df)
      r      <- roles()
      id_col <- names(r)[r == "obs_id"]
      updateSelectInput(session, "id_col",
                        choices=names(df),
                        selected=if(length(id_col)>0) id_col[1] else names(df)[1])
    })
    
    # ── Aggregate flags ───────────────────────────────────────────────────────
    long_df <- reactive({
      seed()  # direct dependency — re-aggregate whenever seed changes
      req(input$id_col)
      df <- get_data()
      id_labels <- as.character(df[[input$id_col]])

      active <- input$active_methods
      if (is.null(active) || length(active) == 0) return(NULL)
      
      all_flags <- list(
        mahalanobis = flagged_mah(),
        cooks       = flagged_cooks(),
        lof         = flagged_lof(),
        svm         = flagged_svm(),
        rf          = flagged_rf(),
        iforest     = flagged_iforest()
      )
      method_flags <- all_flags[intersect(active, names(all_flags))]
      
      rows <- lapply(names(method_flags), function(method) {
        idx <- method_flags[[method]]
        if (length(idx) == 0) return(NULL)
        data.frame(id=id_labels[idx], Type=method, stringsAsFactors=FALSE)
      })
      rows <- rows[!sapply(rows, is.null)]
      if (length(rows) == 0) return(NULL)
      do.call(rbind, rows)
    })
    
    # ── Shared wide data (NEW) ────────────────────────────────────────────────
    wide_data <- reactive({
      df_long <- long_df()
      req(df_long)
      
      all_methods <- c("mahalanobis","cooks","lof","svm","rf","iforest")
      active      <- intersect(input$active_methods, all_methods)
      
      all_ids <- unique(df_long$id)
      
      wide_df <- data.frame(id = all_ids, stringsAsFactors = FALSE)
      for (m in active) {
        flagged_ids  <- df_long$id[df_long$Type == m]
        wide_df[[m]] <- as.integer(wide_df$id %in% flagged_ids)
      }
      
      method_cols   <- setdiff(names(wide_df), "id")
      wide_df$total <- rowSums(wide_df[, method_cols, drop=FALSE])
      
      wide_df
    })
    
    method_colors <- c(
      mahalanobis = "#2ab7ca",
      cooks       = "#fe4a49",
      lof         = "#27ae60",
      svm         = "#e056fd",
      rf          = "#4a80d4",
      iforest     = "#f39c12"
    )
    
    # ── Plot ──────────────────────────────────────────────────────────────────
    output$plot <- renderPlot({
      wide_df <- wide_data()
      
      if (is.null(wide_df) || nrow(wide_df)==0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label="No outliers flagged yet.\nRun detection methods first.",
                            colour="#495057", size=5) + ggplot2::theme_void()
        return()
      }
      
      method_cols <- setdiff(names(wide_df), c("id","total"))
      wide_df     <- wide_df[wide_df$total >= input$min_count, ]
      
      if (nrow(wide_df)==0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label=paste0("No observations flagged by ≥ ",
                                         input$min_count, " methods."),
                            colour="#495057", size=5) + ggplot2::theme_void()
        return()
      }
      
      # consistent ordering
      wide_df$id <- factor(
        wide_df$id,
        levels = wide_df$id[order(-wide_df$total)]
      )
      
      plot_df <- tidyr::pivot_longer(
        wide_df[, c("id", method_cols)],
        cols      = tidyr::all_of(method_cols),
        names_to  = "Type",
        values_to = "Count"
      )
      plot_df <- plot_df[plot_df$Count > 0, ]
      
      ggplot2::ggplot(plot_df,
                      ggplot2::aes(x=id, y=Count, fill=Type)) +
        ggplot2::geom_col() +
        ggplot2::scale_fill_manual(values=method_colors) +
        ggplot2::labs(
          title = "Observation Novelty (cumulative outlier scores)",
          x     = "ID",
          y     = "Count",
          fill  = "Method") +
        ggplot2::theme(
          plot.title   = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5),
          axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
          axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
          axis.text.x  = ggplot2::element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y  = ggplot2::element_text(size = 12),
          legend.title = ggplot2::element_text(size = 14, face = "bold"),
          legend.text  = ggplot2::element_text(size = 12),
          legend.key.size = grid::unit(2, "lines")
        )
    })
    
    # ── Table ─────────────────────────────────────────────────────────────────
    output$table <- DT::renderDataTable({
      df_long <- long_df()
      if (is.null(df_long) || nrow(df_long) == 0)
        return(data.frame(Message = "No outliers flagged yet."))
      
      counts   <- table(df_long$id)
      keep_ids <- names(counts[counts >= input$min_count])
      if (length(keep_ids) == 0)
        return(data.frame(Message = "No observations meet threshold."))
      
      raw_df    <- get_raw()
      id_labels <- as.character(raw_df[[input$id_col]])
      keep_rows <- which(id_labels %in% keep_ids)
      
      result_df <- raw_df[keep_rows, , drop = FALSE]
      result_df$flag_count <- as.integer(counts[as.character(result_df[[input$id_col]])])
      
      wide_df <- wide_data()
      
      # 🔥 FIXED SORT (aligned with plot)
      result_df <- if (input$sort_by == "flag_count") {
        result_df[order(
          -result_df$flag_count,
          match(result_df[[input$id_col]], wide_df$id)
        ), ]
      } else {
        result_df[order(as.integer(rownames(result_df))), ]
      }
      
      DT::datatable(result_df,
                    options = list(pageLength = 15, scrollX = TRUE, order = list()),
                    rownames = TRUE)
    })
  })
} 