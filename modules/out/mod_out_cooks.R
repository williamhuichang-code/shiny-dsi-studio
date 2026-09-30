# =================================================================================
# mod_out_cooks.R
# =================================================================================

out_cooks_ui <- function(id) {
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
        HTML("&nbsp; <b>Cook's Distance</b><br><br>
              Measures each observation's influence on the linear regression fit —
              combining leverage (unusual predictor values) and residual (poor fit).<br><br>
              <b style='color:#856404;'>Assumptions to check:</b><br>
              &nbsp;• Linear relationship between y and predictors<br>
              &nbsp;• Numeric predictors only (encode nominals if needed)<br>
              &nbsp;• No missing values<br>
              &nbsp;• No zero/near-zero variance<br>
              &nbsp;• No linear dependencies<br>
              &nbsp;• No strong multicollinearity (handled via correlation filter)<br>
              &nbsp;• No interaction effects — Cook's uses a main-effects-only GLM.
              If interactions are present, influential points may be missed or
              falsely flagged.<br><br>
              <b style='color:#0f6e56;'>Interpreting position:</b><br>
              If flagged points are scattered randomly across the x-axis
              (observation index), the influence is homogeneous across the dataset.
              If they cluster at one end, there may be a sequential or
              group-level pattern worth investigating.")
      ),
      hr(),
      
      # ── Column selection ───────────────────────────────────────────────────
      selectInput(ns("id_col"), "ID / label column:", choices=NULL),
      hr(),
      selectInput(ns("response_col"), "Response variable (y):", choices=NULL),
      helpText("⚠ Select only a numeric outcome. Do not use ID or split columns."),
      hr(),
      selectizeInput(ns("pred_cols"), "Predictor columns:",
                     choices=NULL, multiple=TRUE,
                     options=list(placeholder="Defaults to Predictor roles")),
      helpText("⚠ Drop ID, split, and response columns from predictors."),
      hr(),
      
      # ── Threshold ─────────────────────────────────────────────────────────
      sliderInput(ns("threshold_mult"), "Threshold multiplier (k × mean D_c):",
                  min=1, max=10, value=4, step=0.25, width="100%"),
      helpText("Standard rule: 4 × mean. If overall D_c values are low, try an
               absolute cut-off of 0.25 or 0.5 instead."),
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
      helpText("Cook's Distance does not tolerate NAs. Recommended to keep checked."),
      hr(),
      
      checkboxInput(ns("remove_nzv_lincomb"),
                    "Remove zero-variance and linearly dependent columns",
                    value=TRUE),
      helpText("Uncheck to compare — GLM will fail if singular columns remain."),
      hr(),
      
      checkboxInput(ns("remove_corr"),
                    "Remove highly correlated predictors (r > 0.9)",
                    value=TRUE),
      helpText("Strong multicollinearity inflates Cook's Distance and makes
               influential point detection unreliable. Recommended to keep checked."),
      hr(),
      tags$label("Normality transform (predictors only):",
                 style="font-weight:600; font-size:13px; color:#343a40;"),
      radioButtons(ns("transform"), label=NULL,
                   choices=c(
                     "None"        = "none",
                     "Yeo-Johnson" = "yeojohnson",
                     "Box-Cox (strictly positive data only)" = "boxcox"
                   ),
                   selected="yeojohnson"),
      helpText("Applied to predictors only — response (y) is never transformed.
               Yeo-Johnson works on any data. Box-Cox requires all values > 0.
               Skewed predictors without transform may cause Cook's to over-flag
               valid observations in the tails.")
    ),
    
    mainPanel(
      width = 9,
      plotOutput(ns("plot"), height="60vh"),
      hr(),
      h5("Outlier Rows (raw data)"),
      DT::dataTableOutput(ns("table"))
    )
  )
}

# ── SERVER ───────────────────────────────────────────────────────────────────

out_cooks_server <- function(id, get_data, get_raw, roles, seed = reactive(2026)) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      df      <- get_data(); req(df)
      r       <- roles()
      id_col  <- names(r)[r == "obs_id"]
      resp    <- names(r)[r == "outcome"]
      preds   <- names(r)[r == "predictor"]
      num_pred <- preds[sapply(preds, function(v)
        v %in% names(df) && is.numeric(df[[v]]) && !grepl("_shadow$", v))]
      
      updateSelectInput(session, "id_col",
                        choices=names(df),
                        selected=if(length(id_col)>0) id_col[1] else names(df)[1])
      updateSelectInput(session, "response_col",
                        choices=names(df),
                        selected=if(length(resp)>0) resp[1] else names(df)[1])
      updateSelectizeInput(session, "pred_cols",
                           choices=names(df), selected=num_pred, server=TRUE)
    })
    
    processed <- reactive({
      seed()   # top — always registers dependency before any req() / early return (Cook's is deterministic)
      req(input$response_col, input$pred_cols)
      df       <- get_data()
      response <- input$response_col
      preds    <- setdiff(input$pred_cols, response)
      if (length(preds) < 1) return(NULL)
      if (!is.numeric(df[[response]])) return(NULL)
      
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
      if (nrow(df_sub) < 3) return(NULL)
      
      # ── 2. encode nominals ────────────────────────────────────────────────
      if (isTRUE(input$encode_nominal)) {
        fac_cols <- names(df_sub)[sapply(names(df_sub), function(v)
          v != response && (is.factor(df_sub[[v]]) || is.character(df_sub[[v]])))]
        if (length(fac_cols) > 0) {
          tryCatch({
            library(recipes)
            df_sub <- recipe(as.formula(paste(response,"~.")), data=df_sub) |>
              step_dummy(all_of(fac_cols), one_hot=FALSE) |>
              prep(training=df_sub) |>
              bake(new_data=NULL)
          }, error=function(e) NULL)
        }
      }
      
      # keep only numeric predictors + response
      num_cols <- names(df_sub)[sapply(df_sub, is.numeric)]
      df_sub   <- df_sub[, num_cols, drop=FALSE]
      if (is.null(df_sub) || ncol(df_sub) < 2) return(NULL)
      
      # ── 3. remove NZV, lincomb, correlation ───────────────────────────────
      tryCatch({
        library(recipes)
        rec <- recipe(as.formula(paste(response,"~.")), data=df_sub)
        
        if (isTRUE(input$remove_nzv_lincomb)) {
          rec <- rec |>
            step_nzv(all_predictors()) |>
            step_lincomb(all_numeric_predictors())
        }
        if (isTRUE(input$remove_corr)) {
          rec <- rec |> step_corr(all_numeric_predictors(), threshold=0.9)
        }
        
        prepped <- prep(rec, training=df_sub)
        df_prepped <- bake(prepped, new_data=NULL)
        
        # ── apply normality transform to predictors only ───────────────────
        if (input$transform != "none") {
          pred_only_cols <- setdiff(names(df_prepped), response)
          pred_only_cols <- pred_only_cols[sapply(pred_only_cols, function(v)
            is.numeric(df_prepped[[v]]))]
          if (length(pred_only_cols) > 0) {
            tryCatch({
              transform_rec <- recipe(~., data=df_prepped[, pred_only_cols, drop=FALSE])
              if (input$transform == "yeojohnson") {
                transform_rec <- transform_rec |>
                  step_YeoJohnson(all_numeric_predictors())
              } else if (input$transform == "boxcox") {
                pos_cols <- pred_only_cols[sapply(pred_only_cols, function(v)
                  all(df_prepped[[v]] > 0, na.rm=TRUE))]
                if (length(pos_cols) > 0)
                  transform_rec <- transform_rec |> step_BoxCox(all_of(pos_cols))
              }
              transformed <- transform_rec |>
                prep(training=df_prepped[, pred_only_cols, drop=FALSE]) |>
                bake(new_data=NULL)
              df_prepped[, names(transformed)] <- transformed
            }, error=function(e) NULL)
          }
        }
        baked <- df_prepped
        
        # n > p guard
        n_pred <- ncol(baked) - 1  # exclude response
        if (nrow(baked) <= n_pred + 1) return(NULL)
        
        list(baked=baked, orig_rows=orig)
      }, error=function(e) NULL)
    })
    
    result <- reactive({
      res <- processed(); req(res)
      baked    <- res$baked
      response <- input$response_col
      if (ncol(baked) < 2 || nrow(baked) < 3) return(NULL)
      tryCatch({
        lmod      <- glm(as.formula(paste(response,"~.")), data=baked, family=gaussian)
        dc        <- cooks.distance(lmod)
        threshold <- input$threshold_mult * mean(dc, na.rm=TRUE)
        is_out    <- dc > threshold
        list(dc=dc, threshold=threshold, is_out=is_out,
             orig_rows=res$orig_rows, n_rows=nrow(baked))
      }, error=function(e) NULL)
    })
    
    output$plot <- renderPlot({
      res <- result()
      if (is.null(res)) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label=paste0(
                              "Could not fit model.\n",
                              "Check: numeric response, ≥ 1 predictor,\n",
                              "n > p, no remaining NAs."),
                            colour="#C41E3A", size=4.5, hjust=0.5) +
          ggplot2::theme_void()
        return()
      }
      df        <- get_data()
      dc        <- res$dc
      threshold <- res$threshold
      n_out     <- sum(res$is_out)
      id        <- seq_along(dc)/length(dc)
      id_labels <- as.character(df[[input$id_col]])[res$orig_rows]
      label     <- ifelse(res$is_out, id_labels, NA_character_)
      Obs       <- ifelse(res$is_out, "outlier", "non-outlier")
      
      ggplot2::ggplot(data.frame(dc, id, label, Obs), ggplot2::aes(y=dc, x=id)) +
        ggplot2::geom_point(ggplot2::aes(colour=Obs), size=2) +
        ggrepel::geom_text_repel(ggplot2::aes(label=label), max.overlaps=50,
                                 size=5, na.rm=TRUE) +
        ggplot2::scale_colour_manual(
          values=c("non-outlier"="#4a80d4","outlier"="#C41E3A")) +
        ggplot2::geom_hline(yintercept=threshold, linetype="dashed") +
        ggplot2::scale_x_continuous(breaks=c(0,.5,1), labels=c("0%","50%","100%")) +
        ggplot2::annotate("text", x=0.01, y=threshold*1.05,
                          label=sprintf("threshold=%.4f  (%d outliers)", threshold, n_out),
                          hjust=0, size=5) +
        ggplot2::labs(
          title=paste0("Cook's Distance | threshold=", input$threshold_mult,
                       " × mean(D_c)=", round(threshold,4),
                       " | n=", res$n_rows, " complete rows",
                       " | ", switch(input$transform,
                                     "none"       = "no transform",
                                     "yeojohnson" = "Yeo-Johnson",
                                     "boxcox"     = "Box-Cox")),
          y="Cook's distance", x="Complete observations") +
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
        DT::datatable(options=list(pageLength=10, dom="tip", scrollX=TRUE), rownames=FALSE)
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