# =================================================================================
# mod_out_rf.R
# =================================================================================

# ── LIBRARY ──────────────────────────────────────────────────────────────────

library(plotly)
library(randomForest) # RF


# ── HELPER ───────────────────────────────────────────────────────────────────

out_rf_ui <- function(id) {
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
        HTML("&nbsp; <b>Random Forest Residuals</b><br><br>
              Trains a Random Forest to predict the response variable, then
              flags observations whose residuals are unusually large (IQR method).<br><br>
              <b style='color:#856404;'>Assumptions to check:</b><br>
              &nbsp;• Numeric response required<br>
              &nbsp;• No missing values<br>
              &nbsp;• Accepts numeric and <b>low-cardinality categorical</b>
              predictors natively — no dummy encoding needed. However,
              <b>manually exclude high-cardinality categoricals</b> (e.g. CODE
              with 500+ levels) as they will dominate splits and produce
              meaningless residuals.<br><br>
              <b style='color:#0f6e56;'>Strengths:</b><br>
              &nbsp;• <b>Non-linear relationships</b> — RF captures complex
              interactions between predictors automatically, making residuals
              more meaningful than a linear model's.<br>
              &nbsp;• <b>Not sensitive to scale or centering</b> — no scaling
              needed, tree splits are invariant to monotone transformations.<br>
              &nbsp;• <b>Uses all outcome and explanatory variables</b> in the
              tree — outliers are defined relative to the full multivariate
              structure, not just pairwise distances.<br>
              &nbsp;• <b>Weakly robust to outliers</b> — RF is somewhat
              resistant to the influence of extreme values during training,
              so flagged residual outliers are more believable than those from
              a standard linear model. Points flagged here are genuinely
              hard to predict given all available information.<br><br>
              <b style='color:#856404;'>Limitation:</b><br>
              &nbsp;• Uses default hyperparameters (ntree=500, mtry=p/3) —
              no full tuning is performed. Results are generally stable but
              may vary slightly between runs due to bootstrap sampling.
              For critical decisions, consider setting a fixed seed or
              averaging over multiple runs.")
      ),
      hr(),
      
      # ── Column selection ───────────────────────────────────────────────────
      selectInput(ns("id_col"), "ID / label column:", choices=NULL),
      hr(),
      selectInput(ns("response_col"), "Response variable (y):", choices=NULL),
      helpText("⚠ Must be numeric. Do not select ID or split columns."),
      hr(),
      selectizeInput(ns("pred_cols"), "Predictor columns:",
                     choices=NULL, multiple=TRUE,
                     options=list(placeholder="Defaults to Predictor roles")),
      helpText("⚠ Drop response (y), ID, and split columns.
               Low-cardinality categoricals (e.g. GOVERN_TYPE) are fine.
               Exclude high-cardinality categoricals like CODE — they dominate
               tree splits and produce unreliable residuals."),
      hr(),
      
      # ── IQR threshold ─────────────────────────────────────────────────────
      numericInput(ns("iqr_k"), "IQR multiplier (k):",
                   value=2.0, min=0.5, step=0.5),
      helpText("Residuals beyond k × IQR are flagged. The standard boxplot
               rule is k=1.5. Use k=2.0–3.0 for a more conservative threshold
               that flags only the most extreme residuals."),
      hr(),
      
      # ── Preprocessing options ──────────────────────────────────────────────
      tags$label("Preprocessing (plot only — does not affect pipeline data):",
                 style="font-weight:600; font-size:13px; color:#343a40;"),
      hr(),
      
      checkboxInput(ns("ignore_na"),
                    "Ignore rows with missing values (complete cases only)",
                    value=TRUE),
      helpText("Random Forest does not tolerate NAs in predictors or response.
               Recommended to keep checked. If your pipeline data is already
               fully imputed, this will have no effect."),
      hr(),
      
      checkboxInput(ns("remove_nzv_lincomb"),
                    "Remove near-zero variance (NZV) columns",
                    value=TRUE),
      helpText("Even though RF won't break with ZV/NZV columns, removing them
               is good practice for three reasons: (1) Computational waste —
               RF still checks dead variables at every node split.
               (2) mtry dilution — if the random feature sample at a split
               contains only NZV columns, that node becomes useless.
               (3) NZV columns can cause overfitting — a tree may create a
               branch that only applies to the 1–2 non-constant rows.
               Linear combinations are NOT removed — RF is tree-based and
               insensitive to linear dependencies between predictors.")
    ),
    
    mainPanel(
      width=9,
      plotOutput(ns("plot"), height="40vh"),
      hr(),
      verbatimTextOutput(ns("summary")),
      hr(),
      h5("Outlier Rows (raw data)"),
      DT::dataTableOutput(ns("table"))
    )
  )
}

# ── SERVER ───────────────────────────────────────────────────────────────────

out_rf_server <- function(id, get_data, get_raw, roles, seed = reactive(2026)) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      df     <- get_data(); req(df)
      r      <- roles()
      id_col <- names(r)[r == "obs_id"]
      resp   <- names(r)[r == "outcome"]
      preds  <- names(r)[r == "predictor"]
      # exclude shadow cols and high-cardinality factors by default
      rf_pred <- preds[preds %in% names(df) & !grepl("_shadow$", preds)]
      rf_pred <- rf_pred[sapply(rf_pred, function(v) {
        x <- df[[v]]
        if (is.factor(x) || is.character(x)) length(unique(x)) <= 20
        else TRUE
      })]
      
      updateSelectInput(session, "id_col",
                        choices=names(df),
                        selected=if(length(id_col)>0) id_col[1] else names(df)[1])
      updateSelectInput(session, "response_col",
                        choices=names(df),
                        selected=if(length(resp)>0) resp[1] else names(df)[1])
      updateSelectizeInput(session, "pred_cols",
                           choices=names(df), selected=rf_pred, server=TRUE)
    })
    
    result <- reactive({
      current_seed <- seed()   # top — always registers dependency before any req() / early return
      req(input$id_col, input$response_col, input$pred_cols)
      df       <- get_data()
      response <- input$response_col
      preds    <- setdiff(input$pred_cols, response)
      if (length(preds) < 1 || !is.numeric(df[[response]])) return(NULL)
      
      cols    <- intersect(union(preds, response), names(df))
      df_sub  <- df[, cols, drop=FALSE]
      
      # ── 1. handle NAs ─────────────────────────────────────────────────────
      if (isTRUE(input$ignore_na)) {
        mask   <- complete.cases(df_sub)
        orig   <- which(mask)
        df_sub <- df_sub[mask, , drop=FALSE]
      } else {
        orig <- seq_len(nrow(df_sub))
      }
      if (nrow(df_sub) < 10) return(NULL)
      
      # ── 2. remove NZV and lincomb from numeric predictors only ────────────
      tryCatch({
        if (isTRUE(input$remove_nzv_lincomb)) {
          library(recipes)
          rec <- recipe(as.formula(paste(response,"~.")), data=df_sub) |>
            step_nzv(all_predictors()) |>
            prep(training=df_sub)
          df_sub <- bake(rec, new_data=NULL)
        }
        if (ncol(df_sub) < 2) return(NULL)
        
        # ── 3. fit Random Forest ───────────────────────────────────────────
        set.seed(current_seed)
        gen_model <- randomForest::randomForest(
          as.formula(paste(response,"~.")), data=df_sub)
        
        resids <- df_sub[[response]] - predict(gen_model, newdata=df_sub)
        names(resids) <- as.character(df[[input$id_col]])[orig]
        
        k      <- input$iqr_k
        limits <- boxplot.stats(x=resids, coef=k)$stats
        is_out <- resids < limits[1] | resids > limits[5]
        
        list(resids=resids, is_out=is_out, limits=limits,
             orig_rows=orig, k=k, n_rows=nrow(df_sub), n_cols=ncol(df_sub)-1)
      }, error=function(e) NULL)
    })
    
    output$plot <- renderPlot({
      res <- result()
      if (is.null(res)) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label=paste0(
                              "Could not fit Random Forest.\n",
                              "Check: numeric response, ≥ 1 predictor,\n",
                              "no remaining NAs, randomForest installed.\n",
                              "Also check for high-cardinality factor columns."),
                            colour="#C41E3A", size=5, hjust=0.5) +
          ggplot2::theme_void()
        return()
      }
      resids  <- res$resids
      k       <- res$k
      label   <- ifelse(res$is_out, names(resids), NA_character_)
      plot_df <- data.frame(
        resids = resids,
        Obs    = ifelse(res$is_out, "outlier", "non-outlier"),
        label  = label)
      
      ggplot2::ggplot(plot_df, ggplot2::aes(x=resids, y=0)) +
        ggplot2::geom_boxplot(coef=k, outlier.colour="#C41E3A") +
        ggrepel::geom_text_repel(ggplot2::aes(label=label), max.overlaps=50,
                                 na.rm=TRUE, size=5) +
        ggplot2::labs(
          title=paste0("RF Residuals | IQR k=", k,
                       " | n=", res$n_rows, " rows | ",
                       res$n_cols, " predictors"),
          x="Residuals") +
        ggplot2::theme_minimal(base_size=16) +
        ggplot2::theme(
          plot.title   = ggplot2::element_text(size=20, face="bold", hjust=0.5),
          axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
          axis.title.y = ggplot2::element_blank(),
          axis.text.x  = ggplot2::element_text(size = 15),
          axis.text.y  = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          )
    })
    
    output$summary <- renderPrint({
      res <- result()
      if (is.null(res)) { cat("No result.\n"); return(invisible(NULL)) }
      resids <- res$resids
      n_out  <- sum(res$is_out)
      cat(sprintf("RF Residual Outliers | IQR k=%.1f\n", res$k))
      cat(sprintf("Predictors used : %d\n", res$n_cols))
      cat(sprintf("Complete rows   : %d\n", res$n_rows))
      cat(sprintf("SD of residuals : %.4f\n", sd(resids)))
      cat(sprintf("Whisker limits  : [%.4f, %.4f]\n", res$limits[1], res$limits[5]))
      cat(sprintf("Outliers        : %d  (%.1f%%)\n", n_out, 100*n_out/res$n_rows))
      cat("──────────────────────────────\n")
      if (n_out==0) cat("No outliers flagged.\n")
      else { cat("Flagged IDs:\n"); cat(paste(names(resids)[res$is_out], collapse="  "), "\n") }
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