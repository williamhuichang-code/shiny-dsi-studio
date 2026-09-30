# =================================================================================
# mod_out_iforest.R
# =================================================================================

# ── LIBRARY ──────────────────────────────────────────────────────────────────

library(plotly)
library(isotree)      # Isolation Forest


# ── HELPER ───────────────────────────────────────────────────────────────────

out_iforest_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width=3,
      style="background-color:#e8f0fe; border-left:2px solid #a8c0fd;
             min-height:100vh; padding-left:20px;",
      div(
        style="font-size:13px; color:#343a40; background:white; padding:10px;
               border-left:4px solid #0d6efd; border-radius:6px; margin-bottom:12px;
               box-shadow:0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style="color:#0d6efd;"),
        HTML("&nbsp; <b>Isolation Forest</b><br><br>
              Isolates observations by randomly partitioning the feature space.
              Outliers are isolated faster (fewer splits needed) and receive
              higher anomaly scores.<br><br>
              Scores range from <b>0 to 1</b> and can be interpreted as a
              measure of outlierness — the closer to 1, the more isolated and
              anomalous the observation. A score of 0.5 is roughly expected for
              a normal point; scores above 0.6 are commonly used as a starting
              threshold, but this is arbitrary and dataset-dependent.<br><br>
              <b style='color:#0f6e56;'>Strengths:</b><br>
              &nbsp;• <b>Tolerates missing values natively</b> — no imputation
              needed before running.<br>
              &nbsp;• Works on <b>numeric and low-cardinality categorical</b>
              columns natively — no dummy encoding needed.<br>
              &nbsp;• <b>Not sensitive to scale</b> — tree-based splits are
              invariant to monotone transformations.<br>
              &nbsp;• Handles high-dimensional data well.<br><br>
              <b style='color:#856404;'>Limitations:</b><br>
              &nbsp;• <b>Many tuning parameters</b> — knowing how to set
              ntrees, sample size, or other parameters optimally is not easy
              and there is no obvious criterion for optimality. Default values
              are reasonable starting points but results may vary.<br>
              &nbsp;• <b>Manually exclude high-cardinality categoricals</b>
              (e.g. CODE with 500+ levels) — they create uninformative splits
              and dominate the isolation process.<br>
              &nbsp;• The score threshold is arbitrary — 0.6 is a common
              convention but you should adjust based on how many outliers
              you expect in your domain.")
      ),
      hr(),
      
      # ── Column selection ───────────────────────────────────────────────────
      selectInput(ns("id_col"), "ID / label column:", choices=NULL),
      hr(),
      selectizeInput(ns("pred_cols"), "Predictor columns:",
                     choices=NULL, multiple=TRUE,
                     options=list(placeholder="Defaults to Predictor roles")),
      helpText("Low-cardinality categoricals (e.g. GOVERN_TYPE) are fine.
               Exclude high-cardinality columns like CODE — they produce
               uninformative random splits. NAs are tolerated natively —
               no need to remove rows with missing values."),
      hr(),
      
      # ── Score threshold ────────────────────────────────────────────────────
      sliderInput(ns("threshold"), "Outlier score threshold:",
                  min=0.50, max=0.95, value=0.53, step=0.01, width="100%"),
      helpText("Scores above threshold are flagged. 0.60 is a commonly used
               starting point but is arbitrary — adjust based on how many
               outliers you expect. Compare results at different thresholds
               before drawing conclusions."),
      hr(),
      
      # ── Tuning parameters ─────────────────────────────────────────────────
      tags$label("Model parameters (optional tuning):",
                 style="font-weight:600; font-size:13px; color:#343a40;"),
      div(
        style="font-size:12px; color:#856404; background:#fff3cd;
               border-left:3px solid #ffc107; padding:6px 10px;
               border-radius:4px; margin-bottom:10px;",
        icon("triangle-exclamation", style="color:#ffc107;"),
        HTML(" Knowing how to set Isolation Forest parameters optimally is
              not straightforward — there is no obvious criterion for what
              is best. The defaults below are reasonable starting points.
              Vary them and compare results rather than optimising blindly.")
      ),
      numericInput(ns("ntrees"), "Number of trees (ntrees):",
                   value=100, min=10, max=1000, step=10),
      helpText("More trees = more stable scores but slower. 100–500 is typical."),
      hr(),
      numericInput(ns("sample_size"), "Sample size per tree:",
                   value=256, min=32, max=1024, step=32),
      helpText("Number of observations used to build each tree. Smaller values
               make outlier detection more local; larger values more global.
               256 is the original paper's recommendation."),
      hr(),
      checkboxInput(ns("remove_nzv"), "Remove near-zero variance columns", value=TRUE),
      helpText("NZV columns add no useful splits and dilute the random feature
               sampling at each node, similar to RF.")
    ),
    
    mainPanel(
      width=9,
      plotOutput(ns("plot"), height="60vh"),
      hr(),
      verbatimTextOutput(ns("summary")),
      hr(),
      h5("Outlier Rows (raw data)"),
      DT::dataTableOutput(ns("table"))
    )
  )
}

# ── SERVER ───────────────────────────────────────────────────────────────────

out_iforest_server <- function(id, get_data, get_raw, roles, seed = reactive(2026)) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      df     <- get_data(); req(df)
      r      <- roles()
      id_col <- names(r)[r == "obs_id"]
      preds  <- names(r)[r == "predictor"]
      # exclude shadow cols and high-cardinality factors by default
      iforest_pred <- preds[preds %in% names(df) & !grepl("_shadow$", preds)]
      iforest_pred <- iforest_pred[sapply(iforest_pred, function(v) {
        x <- df[[v]]
        if (is.factor(x) || is.character(x)) length(unique(x)) <= 20
        else TRUE
      })]
      
      updateSelectInput(session, "id_col",
                        choices=names(df),
                        selected=if(length(id_col)>0) id_col[1] else names(df)[1])
      updateSelectizeInput(session, "pred_cols",
                           choices=names(df), selected=iforest_pred, server=TRUE)
    })
    
    result <- reactive({
      current_seed <- seed()   # top — always registers dependency before any req() / early return
      req(input$id_col, input$pred_cols)
      df    <- get_data()
      preds <- intersect(input$pred_cols, names(df))
      if (length(preds) < 1) return(NULL)
      
      df_sub <- df[, preds, drop=FALSE]
      if (nrow(df_sub) < 5) return(NULL)
      
      # ── optional NZV removal ───────────────────────────────────────────────
      if (isTRUE(input$remove_nzv)) {
        num_cols <- names(df_sub)[sapply(df_sub, is.numeric)]
        if (length(num_cols) > 0) {
          tryCatch({
            library(recipes)
            rec_nzv <- recipe(~., data=df_sub[, num_cols, drop=FALSE]) |>
              step_nzv(all_predictors()) |>
              prep(training=df_sub[, num_cols, drop=FALSE])
            kept_num <- names(bake(rec_nzv, new_data=NULL))
            other_cols <- setdiff(names(df_sub), num_cols)
            df_sub <- df_sub[, c(kept_num, other_cols), drop=FALSE]
          }, error=function(e) NULL)
        }
      }
      if (is.null(df_sub) || ncol(df_sub) < 1) return(NULL)
      
      # ── fit Isolation Forest ───────────────────────────────────────────────
      tryCatch({
        ntrees      <- max(10, as.integer(input$ntrees))
        sample_size <- max(32, as.integer(input$sample_size))

        set.seed(current_seed)
        itree  <- isotree::isolation.forest(
          df_sub,
          ntrees      = ntrees,
          sample_size = min(sample_size, nrow(df_sub))
        )
        scores <- predict(itree, newdata=df_sub)
        names(scores) <- as.character(df[[input$id_col]])
        is_out <- scores > input$threshold
        
        list(scores=scores, is_out=is_out,
             orig_rows=seq_len(nrow(df)),
             n=nrow(df), n_cols=ncol(df_sub),
             ntrees=ntrees, sample_size=sample_size)
      }, error=function(e) NULL)
    })
    
    output$plot <- renderPlot({
      res <- result()
      if (is.null(res)) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label=paste0(
                              "Could not fit Isolation Forest.\n",
                              "Check: ≥ 1 predictor, isotree installed,\n",
                              "no high-cardinality factor columns."),
                            colour="#C41E3A", size=4.5, hjust=0.5) +
          ggplot2::theme_void()
        return()
      }
      scores    <- res$scores
      threshold <- input$threshold
      n_out     <- sum(res$is_out)
      n         <- res$n
      id        <- seq_len(n)/n
      label     <- ifelse(res$is_out, names(scores), NA_character_)
      Obs       <- ifelse(res$is_out, "outlier", "non-outlier")
      
      ggplot2::ggplot(data.frame(scores, id, label, Obs),
                      ggplot2::aes(y=scores, x=id)) +
        ggplot2::geom_point(ggplot2::aes(colour=Obs), size=2) +
        ggrepel::geom_text_repel(ggplot2::aes(label=label), max.overlaps=50,
                                 size=5, na.rm=TRUE) +
        ggplot2::scale_colour_manual(
          values=c("non-outlier"="#4a80d4","outlier"="#C41E3A")) +
        ggplot2::geom_hline(yintercept=threshold, linetype="dashed") +
        ggplot2::annotate("text", x=0.01, y=threshold*1.02,
                          label=sprintf("threshold=%.2f  (%d outliers)", threshold, n_out),
                          hjust=0, size=5) +
        ggplot2::scale_x_continuous(breaks=c(0,.5,1), labels=c("0%","50%","100%")) +
        ggplot2::scale_y_continuous(limits=c(0,1)) +
        ggplot2::labs(
          title=paste0("Isolation Forest | threshold=", threshold,
                       " | ntrees=", res$ntrees,
                       " | sample_size=", res$sample_size,
                       " | n=", n, " obs | ", res$n_cols, " predictors"),
          y="Anomaly score (0–1)", x="Observations") +
        ggplot2::theme_minimal(base_size = 16) +
        ggplot2::theme(
          plot.title   = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5),
          axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
          axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
          axis.text.x  = ggplot2::element_text(size = 15),
          axis.text.y  = ggplot2::element_text(size = 15),
          legend.title = ggplot2::element_text(size = 16, face = "bold"),
          legend.text  = ggplot2::element_text(size = 15)
        )
    })
    
    output$summary <- renderPrint({
      res <- result()
      if (is.null(res)) { cat("No result.\n"); return(invisible(NULL)) }
      scores    <- res$scores
      threshold <- input$threshold
      is_out    <- res$is_out
      n_out     <- sum(is_out)
      cat(sprintf("Isolation Forest | threshold=%.2f\n", threshold))
      cat(sprintf("ntrees           : %d\n", res$ntrees))
      cat(sprintf("sample_size      : %d\n", res$sample_size))
      cat(sprintf("Predictors used  : %d\n", res$n_cols))
      cat(sprintf("Observations     : %d\n", res$n))
      cat(sprintf("Outliers         : %d  (%.1f%%)\n", n_out, 100*n_out/res$n))
      cat(sprintf("Score range      : [%.4f, %.4f]\n", min(scores), max(scores)))
      cat("──────────────────────────────\n")
      if (n_out==0) cat("No outliers flagged.\n")
      else { cat("Flagged IDs:\n"); cat(paste(names(scores)[is_out], collapse="  "), "\n") }
    })
    
    output$table <- DT::renderDataTable({
      res <- result(); req(res)
      raw_df  <- get_raw()
      out_idx <- res$orig_rows[res$is_out]
      if (length(out_idx)==0) return(data.frame(Message="No outliers flagged."))
      raw_df[out_idx,,drop=FALSE] |>
        DT::datatable(options=list(pageLength=10, dom="tip", scrollX=TRUE),
                      rownames=FALSE)
    })
    
    return(list(
      flagged = reactive({
        res <- result()
        if (is.null(res)) return(integer(0))
        res$orig_rows[res$is_out]
      })
    ))
  })
}