# =================================================================================
# mod_out_mahalanobis.R
# =================================================================================

out_mahalanobis_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 3,
      style = "background-color:#e8f0fe; border-left:2px solid #a8c0fd;
               min-height:100vh; padding-left:20px;",
      
      div(
        style = "font-size:13px; color:#343a40; background:white; padding:10px;
                 border-left:4px solid #0d6efd; border-radius:6px; margin-bottom:12px;
                 box-shadow:0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style="color:#0d6efd;"),
        HTML("&nbsp; <b>Mahalanobis Distance</b><br><br>
              Properly config before trusting any outlier result. <br><br>
              <b style='color:#856404;'>Assumptions to check:</b><br>
              &nbsp;• Numeric predictors only<br>
              &nbsp;• No missing values<br>
              &nbsp;• No zero/near-zero variance<br>
              &nbsp;• No linear dependencies<br>
              &nbsp;• Approximate multivariate normality<br>
              &nbsp;• <b>Single homogeneous cluster</b> — if your data
              has natural subgroups (e.g. Train vs Test), results may be
              misleading. Consider filtering to one group before running.")
      ),
      hr(),
      
      # ── Column selection ───────────────────────────────────────────────────
      tags$label("Predictor columns (numeric):",
                 style="font-weight:600; font-size:13px; color:#343a40;"),
      helpText("⚠ Drop the response (y) and any ID / split columns — include predictors only."),
      selectizeInput(ns("pred_cols"), label=NULL,
                     choices=NULL, multiple=TRUE,
                     options=list(placeholder="Defaults to Predictor roles")),
      hr(),
      
      # ── ID column ─────────────────────────────────────────────────────────
      selectInput(ns("id_col"), "ID / label column:", choices=NULL),
      hr(),
      
      # ── Threshold ─────────────────────────────────────────────────────────
      sliderInput(ns("threshold_p"), "Chi-squared threshold (p):",
                  min=0.90, max=0.999, value=0.995, step=0.001, width="100%"),
      helpText("Higher p = stricter, fewer outliers flagged."),
      hr(),
      
      # ── Preprocessing options ──────────────────────────────────────────────
      tags$label("Preprocessing (for plot only — does not affect pipeline data):",
                 style="font-weight:600; font-size:13px; color:#343a40;"),
      hr(),
      
      checkboxInput(ns("encode_nominal"),
                    "Encode nominal columns as dummies",
                    value=FALSE),
      helpText("Enable if you selected any categorical columns above."),
      hr(),
      
      checkboxInput(ns("ignore_na"),
                    "Ignore rows with missing values (complete cases only)",
                    value=TRUE),
      helpText("Mahalanobis does not tolerate NAs. Recommended to keep checked."),
      hr(),
      
      checkboxInput(ns("remove_nzv_lincomb"),
                    "Remove zero-variance and linearly dependent columns",
                    value=TRUE),
      helpText("Uncheck to compare — singular covariance matrix will cause errors if unchecked."),
      hr(),
      
      tags$label("Normality transform:",
                 style="font-weight:600; font-size:13px; color:#343a40;"),
      radioButtons(ns("transform"), label=NULL,
                   choices=c(
                     "None"         = "none",
                     "Yeo-Johnson"  = "yeojohnson",
                     "Box-Cox (strictly positive data only)" = "boxcox"
                   ),
                   selected="yeojohnson"),
      helpText("Yeo-Johnson works on any data. Box-Cox requires all values > 0.")
    ),
    
    mainPanel(
      width=9,
      plotOutput(ns("plot"), height="60vh"),
      hr(),
      h5("Outlier Rows (raw data)"),
      DT::dataTableOutput(ns("table"))
    )
  )
}

# ── SERVER ───────────────────────────────────────────────────────────────────

out_mahalanobis_server <- function(id, get_data, get_raw, roles, seed = reactive(2026)) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── Populate inputs ───────────────────────────────────────────────────────
    observe({
      df        <- get_data(); req(df)
      r         <- roles()
      id_col    <- names(r)[r == "obs_id"]
      pred_cols <- names(r)[r == "predictor"]
      # default: numeric predictors only, no shadow cols
      num_pred  <- pred_cols[sapply(pred_cols, function(v)
        v %in% names(df) && is.numeric(df[[v]]) && !grepl("_shadow$", v))]
      
      updateSelectInput(session, "id_col",
                        choices  = names(df),
                        selected = if (length(id_col) > 0) id_col[1] else names(df)[1])
      updateSelectizeInput(session, "pred_cols",
                           choices  = names(df),
                           selected = num_pred, server=TRUE)
    })
    
    # ── Core computation (plot only — never modifies get_data()) ──────────────
    result <- reactive({
      seed()   # top — always registers dependency before any req() / early return (Mahalanobis is deterministic)
      req(input$id_col, input$pred_cols)
      df   <- get_data()
      cols <- intersect(input$pred_cols, names(df))
      if (length(cols) < 2) return(NULL)
      
      df_sub <- df[, cols, drop=FALSE]
      
      # ── 1. handle NAs ──────────────────────────────────────────────────────
      if (isTRUE(input$ignore_na)) {
        mask      <- complete.cases(df_sub)
        orig_rows <- which(mask)
        df_sub    <- df_sub[mask, , drop=FALSE]
      } else {
        orig_rows <- seq_len(nrow(df_sub))
      }
      if (nrow(df_sub) < 3) return(NULL)
      
      # ── 2. encode nominals ────────────────────────────────────────────────
      if (isTRUE(input$encode_nominal)) {
        fac_cols <- names(df_sub)[sapply(df_sub, function(x) is.factor(x) || is.character(x))]
        if (length(fac_cols) > 0) {
          tryCatch({
            library(recipes)
            df_sub <- recipe(~., data=df_sub) |>
              step_dummy(all_of(fac_cols)) |>
              prep(training=df_sub) |>
              bake(new_data=NULL)
          }, error=function(e) NULL)
        }
      } else {
        # keep only numeric cols if not encoding
        df_sub <- df_sub[, sapply(df_sub, is.numeric), drop=FALSE]
      }
      if (ncol(df_sub) < 2) return(NULL)
      
      # ── 3. remove NZV and linear combos ───────────────────────────────────
      if (isTRUE(input$remove_nzv_lincomb)) {
        tryCatch({
          library(recipes)
          df_sub <- recipe(~., data=df_sub) |>
            step_nzv(all_predictors()) |>
            step_lincomb(all_numeric_predictors()) |>
            prep(training=df_sub) |>
            bake(new_data=NULL)
        }, error=function(e) NULL)
      }
      if (is.null(df_sub) || ncol(df_sub) < 2) return(NULL)
      
      # ── 4. normality transform ─────────────────────────────────────────────
      if (input$transform != "none") {
        tryCatch({
          library(recipes)
          rec <- recipe(~., data=df_sub)
          if (input$transform == "yeojohnson") {
            rec <- rec |> step_YeoJohnson(all_numeric_predictors())
          } else if (input$transform == "boxcox") {
            pos_cols <- names(df_sub)[sapply(names(df_sub), function(v)
              all(df_sub[[v]] > 0, na.rm=TRUE))]
            if (length(pos_cols) > 0)
              rec <- rec |> step_BoxCox(all_of(pos_cols))
          }
          df_sub <- rec |> prep(training=df_sub) |> bake(new_data=NULL)
        }, error=function(e) NULL)
      }
      if (is.null(df_sub) || ncol(df_sub) < 2) return(NULL)
      
      # ── 5. guard: n must exceed p ─────────────────────────────────────────
      if (nrow(df_sub) <= ncol(df_sub)) return(NULL)
      
      # ── 6. compute Mahalanobis D² ─────────────────────────────────────────
      tryCatch({
        Covar     <- var(df_sub)
        Means     <- colMeans(df_sub)
        md2       <- mahalanobis(x=df_sub, center=Means, cov=Covar)
        threshold <- qchisq(p=input$threshold_p, df=ncol(df_sub))
        is_out    <- md2 > threshold
        id_labels <- as.character(df[[input$id_col]])[orig_rows]
        
        list(md2=md2, threshold=threshold, is_out=is_out,
             id_labels=id_labels, orig_rows=orig_rows,
             n_processed=ncol(df_sub), n_rows=nrow(df_sub))
      }, error=function(e) NULL)
    })
    
    # ── Plot ──────────────────────────────────────────────────────────────────
    output$plot <- renderPlot({
      res <- result()
      if (is.null(res)) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label=paste0(
                              "Could not compute Mahalanobis D².\n",
                              "Check: ≥ 2 numeric predictors, n > p,\n",
                              "no remaining NAs, non-singular covariance matrix."),
                            colour="#C41E3A", size=4.5, hjust=0.5) +
          ggplot2::theme_void()
        return()
      }
      
      md2       <- res$md2
      threshold <- res$threshold
      n_out     <- sum(res$is_out)
      id        <- seq_along(md2) / length(md2)
      label     <- ifelse(res$is_out, res$id_labels, NA_character_)
      Obs       <- ifelse(res$is_out, "outlier", "non-outlier")
      
      transform_label <- switch(input$transform,
                                "none"        = "no transform",
                                "yeojohnson"  = "Yeo-Johnson",
                                "boxcox"      = "Box-Cox")
      
      ggplot2::ggplot(data.frame(md2, id, label, Obs),
                      ggplot2::aes(y=md2, x=id)) +
        ggplot2::geom_point(ggplot2::aes(colour=Obs), size=2) +
        ggrepel::geom_text_repel(ggplot2::aes(label=label), max.overlaps=50,
                                 size=5, na.rm=TRUE) +
        ggplot2::scale_colour_manual(
          values=c("non-outlier"="#4a80d4","outlier"="#C41E3A")) +
        ggplot2::geom_hline(yintercept=threshold, linetype="dashed") +
        ggplot2::scale_x_continuous(breaks=c(0,.5,1), labels=c("0%","50%","100%")) +
        ggplot2::annotate("text", x=0.01, y=threshold*1.05,
                          label=sprintf("χ²=%.1f  (%d outliers)", threshold, n_out),
                          hjust=0, size=5) +
        ggplot2::labs(
          title=paste0("Mahalanobis D² | p=", input$threshold_p,
                       " | χ²(", res$n_processed, ")=", round(threshold,1),
                       " | n=", res$n_rows, " | ", transform_label),
          y="Mahalanobis D²", x="Complete observations") +
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
    
    # ── Outlier table ─────────────────────────────────────────────────────────
    output$table <- DT::renderDataTable({
      res <- result(); req(res)
      raw_df  <- get_raw()
      out_idx <- res$orig_rows[res$is_out]
      if (length(out_idx)==0) return(data.frame(Message="No outliers flagged."))
      raw_df[out_idx,,drop=FALSE] |>
        DT::datatable(options=list(pageLength=10, dom="tip", scrollX=TRUE),
                      rownames=FALSE)
    })
    
    # ── Return flagged row indices ────────────────────────────────────────────
    return(list(
      flagged = reactive({
        res <- result()
        if (is.null(res)) return(integer(0))
        res$orig_rows[res$is_out]
      })
    ))
  })
}