# =================================================================================
# mod_out_svm.R
# =================================================================================

# ── LIBRARY ──────────────────────────────────────────────────────────────────

library(plotly)
library(e1071)        # SVM


# ── HELPER ───────────────────────────────────────────────────────────────────

out_svm_ui <- function(id) {
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
        HTML("&nbsp; <b>One-Class SVM</b><br><br>
              Learns a boundary around the majority of training data.
              Observations outside the boundary are flagged as outliers.<br><br>
              <b style='color:#856404;'>Assumptions to check:</b><br>
              &nbsp;• Numeric variables only<br>
              &nbsp;• No missing values<br>
              &nbsp;• <b>Sensitive to scale</b> — the SVM kernel computes
              distances or dot products between observations. Variables on
              larger scales dominate the boundary shape. Always scale unless
              variables are already on comparable units.<br>
              &nbsp;• <b>nu</b> is an upper bound on the fraction of outliers
              and a lower bound on the fraction of support vectors. Setting it
              too low may miss real outliers; too high flags too many.<br>
              &nbsp;• <b>Kernel choice</b> affects the boundary shape —
              linear is fastest and most interpretable; radial handles
              non-linear boundaries but is harder to tune.<br>
              &nbsp;• Results can be unstable — small changes in nu or kernel
              can change which points are flagged significantly. Use alongside
              other methods and check the Summary tab for agreement.")
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
      
      # ── SVM parameters ────────────────────────────────────────────────────
      sliderInput(ns("nu"), "nu (outlier fraction):",
                  min=0.001, max=0.5, value=0.05, step=0.005, width="100%"),
      helpText("~nu × n observations will be flagged. Start at 0.05 (5%) and
               adjust based on domain knowledge of expected outlier rate."),
      hr(),
      selectInput(ns("kernel"), "Kernel:",
                  choices=c("linear","polynomial","radial","sigmoid"),
                  selected="radial"),
      helpText("Radial (RBF) handles non-linear boundaries and is the most
               common choice. Linear is faster but assumes a convex boundary."),
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
      helpText("One-class SVM does not tolerate NAs. Recommended to keep checked."),
      hr(),
      
      checkboxInput(ns("remove_nzv_lincomb"),
                    "Remove zero-variance and linearly dependent columns",
                    value=TRUE),
      helpText("Near-zero variance columns add noise to the boundary without
               contributing useful information."),
      hr(),
      
      checkboxInput(ns("scale"), "Scale predictors (divide by SD)", value=FALSE),
      helpText("One-class SVM is highly sensitive to scale. Strongly recommended
               to keep checked unless variables are already on comparable units.
               If your data is already centred and scaled from the pipeline,
               you may uncheck this to avoid double-scaling.")
    ),
    
    mainPanel(
      width=9,
      verbatimTextOutput(ns("summary")),
      hr(),
      h5("Outlier Rows (raw data)"),
      DT::dataTableOutput(ns("table"))
    )
  )
}

# ── SERVER ───────────────────────────────────────────────────────────────────

out_svm_server <- function(id, get_data, get_raw, roles, seed = reactive(2026)) {
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
      current_seed <- seed()   # top — always registers dependency before any req() / early return
      req(input$id_col, input$pred_cols)
      df   <- get_data()
      cols <- intersect(input$pred_cols, names(df))
      if (length(cols) < 1) return(NULL)
      
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
      if (is.null(df_sub) || ncol(df_sub) < 1) return(NULL)
      
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
      
      # ── 4. scale ──────────────────────────────────────────────────────────
      if (isTRUE(input$scale)) {
        tryCatch({
          library(recipes)
          df_sub <- recipe(~., data=df_sub) |>
            step_normalize(all_numeric_predictors()) |>
            prep(training=df_sub) |>
            bake(new_data=NULL)
        }, error=function(e) NULL)
      }
      if (is.null(df_sub) || ncol(df_sub) < 1) return(NULL)
      
      # ── 5. fit one-class SVM ──────────────────────────────────────────────
      tryCatch({
        mat   <- as.matrix(df_sub)
        set.seed(current_seed)
        model <- e1071::svm(mat, y=NULL, type="one-classification",
                            nu=input$nu, scale=FALSE,  # scaling handled above
                            kernel=input$kernel)
        is_out <- !predict(model, mat)
        list(is_out=is_out, orig_rows=orig, n_total=nrow(mat), n_cols=ncol(mat))
      }, error=function(e) NULL)
    })
    
    output$summary <- renderPrint({
      res <- result()
      if (is.null(res)) {
        cat("Could not fit one-class SVM.\n")
        cat("Check: ≥ 1 numeric predictor, no remaining NAs, e1071 installed.\n")
        return(invisible(NULL))
      }
      df        <- get_data()
      is_out    <- res$is_out
      orig_rows <- res$orig_rows
      n_out     <- sum(is_out)
      out_ids   <- as.character(df[[input$id_col]])[orig_rows[is_out]]
      
      cat(sprintf("One-Class SVM | kernel=%s | nu=%.4f\n",
                  input$kernel, input$nu))
      cat(sprintf("Scale         : %s\n", if(isTRUE(input$scale)) "TRUE (normalised)" else "FALSE"))
      cat(sprintf("Complete rows : %d\n", res$n_total))
      cat(sprintf("Predictors    : %d\n", res$n_cols))
      cat(sprintf("Outliers      : %d  (%.1f%%)\n", n_out, 100*n_out/res$n_total))
      cat("──────────────────────────────\n")
      if (n_out==0) cat("No outliers flagged.\n")
      else { cat("Flagged IDs:\n"); cat(paste(out_ids, collapse="  "), "\n") }
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