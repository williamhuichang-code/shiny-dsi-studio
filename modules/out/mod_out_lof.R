# =================================================================================
# mod_out_lof.R
# =================================================================================

# ── LIBRARY ──────────────────────────────────────────────────────────────────

library(plotly)
library(dbscan)       # LOF


# ── HELPER ───────────────────────────────────────────────────────────────────

out_lof_ui <- function(id) {
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
        HTML("&nbsp; <b>Local Outlier Factor (LOF)</b><br><br>
              Flags observations whose <b>local neighbourhood density</b> is
              significantly lower than their neighbours' densities. A point can
              be surrounded by other points and still be flagged if those
              surrounding points are much more tightly packed among themselves.<br><br>
              LOF is <b>local and non-parametric</b> — it makes no distributional
              assumption and handles multiple clusters and non-convex shapes.
              A uniformly sparse region is not flagged; only points that are
              sparse <i>relative to their local context</i> are.<br><br>
              lof ≈ 1: similar density to neighbours (not an outlier)<br>
              lof &gt;&gt; 1: much sparser than neighbours (probable outlier)<br><br>
              <b style='color:#856404;'>Assumptions to check:</b><br>
              &nbsp;• Numeric variables only (no distributional assumption)<br>
              &nbsp;• No missing values<br>
              &nbsp;• No zero/near-zero variance<br>
              &nbsp;• No linear dependencies<br>
              &nbsp;• <b>Sensitive to scale</b> — LOF uses Euclidean distance,
              so variables on larger scales dominate neighbourhood calculations.
              Always scale unless variables are already on comparable units.
              Heavily skewed variables can also distort distances even after
              scaling — an optional distance stabilisation transform can help.<br><br>
              Uses <code>dbscan::lof()</code>.")
      ),
      hr(),
      
      # ── Column selection ───────────────────────────────────────────────────
      selectInput(ns("id_col"), "ID / label column:", choices=NULL),
      hr(),
      selectizeInput(ns("pred_cols"), "Predictor columns:",
                     choices=NULL, multiple=TRUE,
                     options=list(placeholder="Defaults to Predictor roles")),
      helpText("⚠ Drop response (y), ID, and split columns — predictors only."),
      hr(),
      
      # ── LOF parameters ────────────────────────────────────────────────────
      numericInput(ns("min_pts"), "minPts (nearest neighbours):",
                   value=4, min=2, max=50, step=1),
      helpText("Controls neighbourhood size. Small values (3–6) detect fine-grained
               local outliers. Larger values smooth out local density differences.
               Try several values and compare."),
      hr(),
      sliderInput(ns("threshold"), "LOF threshold:",
                  min=1.0, max=5.0, value=1.5, step=0.1, width="100%"),
      helpText("Observations with LOF > threshold are flagged. LOF = 1 means
               same density as neighbours. Values above 1.5–2.0 are typically
               worth investigating."),
      hr(),
      
      # ── Preprocessing options ──────────────────────────────────────────────
      tags$label("Preprocessing (plot only — does not affect pipeline data):",
                 style="font-weight:600; font-size:13px; color:#343a40;"),
      hr(),
      
      checkboxInput(ns("encode_nominal"),
                    "Encode nominal predictors as dummies",
                    value=FALSE),
      helpText("Enable if you selected categorical predictor columns.
               Shadow columns (_shadow) are already binary — no encoding needed."),
      hr(),
      
      checkboxInput(ns("ignore_na"),
                    "Ignore rows with missing values (complete cases only)",
                    value=TRUE),
      helpText("LOF does not tolerate NAs. Recommended to keep checked."),
      hr(),
      
      checkboxInput(ns("remove_nzv_lincomb"),
                    "Remove zero-variance and linearly dependent columns",
                    value=TRUE),
      helpText("Uncheck to compare — near-zero variance columns produce
               meaningless distances in LOF."),
      hr(),
      
      tags$label("Distance stabilisation transform (optional):",
                 style="font-weight:600; font-size:13px; color:#343a40;"),
      radioButtons(ns("transform"), label=NULL,
                   choices=c(
                     "None"        = "none",
                     "Yeo-Johnson" = "yeojohnson",
                     "Box-Cox (strictly positive data only)" = "boxcox"
                   ),
                   selected="none"),
      helpText("LOF makes no normality assumption — this is not a distributional
               requirement. Only useful when variables are heavily skewed, causing
               extreme values to dominate Euclidean distances. Leave as None if
               data is already well-behaved. Yeo-Johnson works on any data.
               Box-Cox requires all values > 0."),
      hr(),
      
      checkboxInput(ns("scale"), "Scale predictors (divide by SD)", value=FALSE),
      helpText("LOF is distance-based and highly sensitive to scale differences
               between variables. Strongly recommended to keep checked unless
               all variables are already on comparable units.")
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

out_lof_server <- function(id, get_data, get_raw, roles, seed = reactive(2026)) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      df     <- get_data(); req(df)
      r      <- roles()
      id_col <- names(r)[r == "obs_id"]
      preds  <- names(r)[r == "predictor"]
      num_pred <- preds[sapply(preds, function(v)
        v %in% names(df) && is.numeric(df[[v]]) && !grepl("_shadow$", v))]
      updateSelectInput(session, "id_col",
                        choices=names(df),
                        selected=if(length(id_col)>0) id_col[1] else names(df)[1])
      updateSelectizeInput(session, "pred_cols",
                           choices=names(df), selected=num_pred, server=TRUE)
    })
    
    result <- reactive({
      seed()   # top — always registers dependency before any req() / early return (LOF is deterministic)
      req(input$id_col, input$pred_cols)
      df   <- get_data()
      cols <- intersect(input$pred_cols, names(df))
      if (length(cols) < 2) return(NULL)
      
      df_sub <- df[, cols, drop=FALSE]
      
      # ── 1. handle NAs ─────────────────────────────────────────────────────
      if (isTRUE(input$ignore_na)) {
        mask   <- complete.cases(df_sub)
        orig   <- which(mask)
        df_sub <- df_sub[mask, , drop=FALSE]
      } else {
        orig <- seq_len(nrow(df_sub))
      }
      if (nrow(df_sub) < 5) return(NULL)
      
      # ── 2. encode nominals ────────────────────────────────────────────────
      if (isTRUE(input$encode_nominal)) {
        fac_cols <- names(df_sub)[sapply(df_sub, function(x)
          is.factor(x) || is.character(x))]
        if (length(fac_cols) > 0) {
          tryCatch({
            library(recipes)
            df_sub <- recipe(~., data=df_sub) |>
              step_dummy(all_of(fac_cols), one_hot=FALSE) |>
              prep(training=df_sub) |>
              bake(new_data=NULL)
          }, error=function(e) NULL)
        }
      }
      
      # keep only numeric
      df_sub <- df_sub[, sapply(df_sub, is.numeric), drop=FALSE]
      if (is.null(df_sub) || ncol(df_sub) < 2) return(NULL)
      
      # ── 3. remove NZV and lincomb ─────────────────────────────────────────
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
      if (is.null(df_sub) || ncol(df_sub) < 1) return(NULL)
      
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
      if (is.null(df_sub) || ncol(df_sub) < 1) return(NULL)
      
      # ── 5. scale ──────────────────────────────────────────────────────────
      if (isTRUE(input$scale)) {
        tryCatch({
          library(recipes)
          df_sub <- recipe(~., data=df_sub) |>
            step_scale(all_numeric_predictors()) |>
            prep(training=df_sub) |>
            bake(new_data=NULL)
        }, error=function(e) NULL)
      }
      if (is.null(df_sub) || ncol(df_sub) < 1) return(NULL)
      
      # ── 6. compute LOF ────────────────────────────────────────────────────
      tryCatch({
        mat    <- as.matrix(df_sub)
        scores <- dbscan::lof(mat, minPts=input$min_pts)
        is_out <- scores > input$threshold
        list(scores=scores, is_out=is_out, orig_rows=orig,
             n_rows=nrow(df_sub), n_cols=ncol(df_sub))
      }, error=function(e) NULL)
    })
    
    output$plot <- renderPlot({
      res <- result()
      if (is.null(res)) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label=paste0(
                              "Could not compute LOF.\n",
                              "Check: ≥ 2 numeric predictors, no remaining NAs,\n",
                              "dbscan installed."),
                            colour="#C41E3A", size=4.5, hjust=0.5) +
          ggplot2::theme_void()
        return()
      }
      df        <- get_data()
      scores    <- res$scores
      threshold <- input$threshold
      n_out     <- sum(res$is_out)
      id        <- seq_along(scores)/length(scores)
      id_labels <- as.character(df[[input$id_col]])[res$orig_rows]
      label     <- ifelse(res$is_out, id_labels, NA_character_)
      Obs       <- ifelse(res$is_out, "outlier", "non-outlier")
      
      transform_label <- switch(input$transform,
                                "none"       = "no transform",
                                "yeojohnson" = "Yeo-Johnson",
                                "boxcox"     = "Box-Cox")
      
      ggplot2::ggplot(data.frame(scores, id, label, Obs),
                      ggplot2::aes(y=scores, x=id)) +
        ggplot2::geom_point(ggplot2::aes(colour=Obs), size=2) +
        ggrepel::geom_text_repel(ggplot2::aes(label=label), max.overlaps=50,
                                 size=5, na.rm=TRUE) +
        ggplot2::scale_colour_manual(
          values=c("non-outlier"="#4a80d4","outlier"="#C41E3A")) +
        ggplot2::geom_hline(yintercept=threshold, linetype="dashed") +
        ggplot2::scale_x_continuous(breaks=c(0,.5,1), labels=c("0%","50%","100%")) +
        ggplot2::scale_y_continuous(limits=c(0,NA)) +
        ggplot2::annotate("text", x=0.01, y=threshold*1.05,
                          label=sprintf("threshold=%.1f  (%d outliers)", threshold, n_out),
                          hjust=0, size=5) +
        ggplot2::labs(
          title=paste0("LOF | minPts=", input$min_pts,
                       " | threshold=", threshold,
                       " | ", transform_label,
                       if(isTRUE(input$scale)) " + scaled" else "",
                       " | n=", res$n_rows, " rows × ", res$n_cols, " cols"),
          y="LOF score", x="Complete observations") +
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