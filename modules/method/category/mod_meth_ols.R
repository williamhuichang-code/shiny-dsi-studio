# =================================================================================
# mod_meth_ols.R  — OLS category module  (LM + RLM + GLMnet + PLS)
# =================================================================================
# Three methods assembled under one shared sidebar.
# The active method is tracked via the method_inner tabsetPanel.
#
# UI:     meth_ols_ui(id, pp_choices, default_preprocess)
# Server: meth_ols_server(id, get_data, roles, seed,
#                          general_preprocess, glmnet_preprocess,
#                          pls_preprocess, rlm_preprocess, lm_preprocess, pp_choices)
# Returns: list(models = reactiveValues, effective_seed = reactive)
# =================================================================================


# ── UI ───────────────────────────────────────────────────────────────────────

meth_ols_ui <- function(id,
                        pp_choices         = character(0),
                        default_preprocess = character(0),
                        model_seed         = 2026) {
  ns <- NS(id)

  # GLMnet-specific controls — injected into "Model Specific Configs" section.
  # They are further gated by method_inner == "glmnet" so they vanish when
  # the user switches to PLS.
  glmnet_panels <- tagList(

    conditionalPanel(
      condition = sprintf("input['%s'] === 'glmnet'", ns("method_inner")),

      tags$label("Penalty type:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      selectInput(ns("glmnet_penalty"), NULL,
                  choices  = c("Elastic net (tune alpha + lambda)" = "elasticnet",
                               "Ridge (alpha = 0)"                 = "ridge",
                               "Lasso (alpha = 1)"                 = "lasso"),
                  selected = "elasticnet", width = "100%"),

      # Fixed-alpha hint shown for ridge / lasso
      conditionalPanel(
        condition = sprintf("input['%s'] !== 'elasticnet'", ns("glmnet_penalty")),
        div(
          style = "font-size:12px; color:#185FA5; background:#e8f0fe;
                   border-left:3px solid #a8c0fd; padding:6px 10px;
                   border-radius:4px; margin-bottom:8px;",
          icon("circle-info", style = "color:#185FA5; margin-right:4px;"),
          HTML("Alpha is <strong>fixed</strong> by the penalty type — only lambda is tuned.")
        )
      ),

      tags$label("Tuning grid:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      selectInput(ns("glmnet_grid_type"), NULL,
                  choices  = c("Tune length default"      = "tunelength",
                               "Custom alpha/lambda grid" = "custom"),
                  selected = "custom", width = "100%"),

      # Custom grid controls
      conditionalPanel(
        condition = sprintf("input['%s'] === 'custom'", ns("glmnet_grid_type")),

        conditionalPanel(
          condition = sprintf("input['%s'] === 'elasticnet'", ns("glmnet_penalty")),
          tags$label("Alpha minimum:",
                     style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
          sliderInput(ns("glmnet_alpha_min"),  NULL, min = 0,    max = 1,   value = 0.1,  step = 0.05, width = "100%"),
          tags$label("Alpha maximum:",
                     style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
          sliderInput(ns("glmnet_alpha_max"),  NULL, min = 0,    max = 1,   value = 1.0,  step = 0.05, width = "100%"),
          tags$label("Alpha step:",
                     style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
          sliderInput(ns("glmnet_alpha_step"), NULL, min = 0.01, max = 0.5, value = 0.23, step = 0.01, width = "100%")
        ),

        tags$label("Log10 lambda minimum:",
                   style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
        sliderInput(ns("glmnet_log_lam_min"), NULL, min = -6, max = 2,   value = -5,  step = 0.5, width = "100%"),
        tags$label("Log10 lambda maximum:",
                   style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
        sliderInput(ns("glmnet_log_lam_max"), NULL, min = -2, max = 6,   value = -1,  step = 0.5, width = "100%"),
        tags$label("Lambda values:",
                   style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
        sliderInput(ns("glmnet_lam_n"),       NULL, min = 5,  max = 100, value = 5,   step = 1,   width = "100%")
      )
    ),

    # ── PLS-specific controls ────────────────────────────────────────────────
    conditionalPanel(
      condition = sprintf("input['%s'] === 'pls'", ns("method_inner")),

      tags$label("Tuning grid:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      selectInput(ns("pls_grid_type"), NULL,
                  choices  = c("Tune length default"  = "tunelength",
                               "Custom ncomp range"   = "custom"),
                  selected = "custom", width = "100%"),

      conditionalPanel(
        condition = sprintf("input['%s'] === 'custom'", ns("pls_grid_type")),

        tags$label("ncomp minimum:",
                   style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
        sliderInput(ns("pls_ncomp_min"), NULL, min = 1,  max = 80, value = 1,  step = 1, width = "100%"),
        tags$label("ncomp maximum:",
                   style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
        sliderInput(ns("pls_ncomp_max"), NULL, min = 1,  max = 80, value = 25, step = 1, width = "100%")
      )
    )
  )

  fluidRow(
    column(9,
      tabsetPanel(type = "tabs", id = ns("method_inner"),
        tabPanel("lm",     value = "lm",     style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "lm",     has_tuning = TRUE)),
        tabPanel("rlm",    value = "rlm",    style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "rlm",    has_tuning = TRUE)),
        tabPanel("glmnet", value = "glmnet", style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "glmnet",
                   tuning_controls = div(
                     style = "margin-bottom:6px;",
                     checkboxInput(ns("glmnet_fixed_y"),
                                   "Shared y-axis across panels",
                                   value = FALSE)
                   ))),
        tabPanel("pls",    value = "pls",    style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "pls"))
      )
    ),
    column(3,
      .meth_sidebar_ui(ns,
                       model_seed         = model_seed,
                       pp_choices         = pp_choices,
                       default_preprocess = default_preprocess,
                       specific_panels    = glmnet_panels)
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

meth_ols_server <- function(id, get_data, roles,
                             seed               = reactive(2026),
                             model_seed         = NULL,
                             general_preprocess = NULL,
                             glmnet_preprocess  = NULL,
                             pls_preprocess     = NULL,
                             rlm_preprocess     = NULL,
                             lm_preprocess      = NULL,
                             pp_choices         = character(0)) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    models <- reactiveValues()

    # ── Common setup ──────────────────────────────────────────────────────────
    setup          <- .meth_common_server_setup(input, output, session, get_data, roles, seed,
                                                model_seed = model_seed)
    effective_seed <- setup$effective_seed
    get_train      <- setup$get_train

    # Active method driven by the method_inner tab
    current_method <- reactive({ input$method_inner %||% "lm" })

    # ── PLS tune_length default: set slider to 25 when PLS tab is active ─────
    # PLS needs tuneLength=25 to find its real ncomp optimum (template uses 25).
    # Other methods default to 5. Slider reverts when switching away from PLS.
    observe({
      if (current_method() == "pls") {
        updateSliderInput(session, "tune_length", value = 25)
      } else {
        updateSliderInput(session, "tune_length", value = 5)
      }
    }) |> bindEvent(current_method())

    # ── Standard output renders ───────────────────────────────────────────────
    .meth_register_outputs(output, "lm",     models, ns)
    .meth_register_outputs(output, "rlm",    models, ns)
    .meth_register_outputs(output, "glmnet", models, ns)
    .meth_register_outputs(output, "pls",    models, ns)

    # ── Override GLMnet tuning plot: facet by λ, colour gradient for α ────────
    # The default plot(mod) puts all λ values in the legend, which is unreadable.
    # Here each panel answers: "under this λ, what α works best?"
    # Falls back to a simple RMSE-vs-λ line when only one α was searched
    # (i.e. ridge or lasso fixed-penalty runs).
    output[["glmnet_tune_plot"]] <- renderPlot({
      mod <- models[["glmnet"]]; req(mod)
      df  <- mod$results

      n_alpha <- length(unique(df$alpha))

      if (n_alpha <= 1) {
        # ── Single-alpha run (ridge / lasso) ── plain RMSE vs log10(λ) ────────
        has_sd <- !all(is.na(df$RMSESD))
        p <- ggplot2::ggplot(df, ggplot2::aes(x = log10(lambda), y = RMSE))
        if (has_sd)
          p <- p + ggplot2::geom_ribbon(
            ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
            fill = "#a8c0fd", alpha = 0.3
          )
        p +
          ggplot2::geom_line(colour = "#185FA5", linewidth = 1) +
          ggplot2::geom_point(colour = "#185FA5", size = 2) +
          ggplot2::labs(
            x        = expression(log[10](lambda)),
            y        = paste0("RMSE (", .resample_label(mod), ")"),
            title    = paste0("GLMnet tuning  (α = ", unique(df$alpha), ")"),
            subtitle = "Regularisation strength vs resampled RMSE"
          ) +
          ggplot2::theme_bw(base_size = 13) +
          ggplot2::theme(
            plot.title    = ggplot2::element_text(face = "bold", size = 14),
            plot.subtitle = ggplot2::element_text(colour = "#6c757d", size = 11),
            axis.text     = ggplot2::element_text(size = 13),
            axis.title    = ggplot2::element_text(size = 13, face = "bold")
          )

      } else {
        # ── Elastic-net run: facet by λ, colour gradient for α ────────────────
        # Pick up to 9 representative λ values, log-spaced; always include best λ
        lambda_vals <- sort(unique(df$lambda))
        best_alpha  <- mod$bestTune$alpha
        best_lam    <- mod$bestTune$lambda
        n_facets    <- min(9, length(lambda_vals))
        idx         <- round(seq(1, length(lambda_vals), length.out = n_facets))
        lam_sub     <- sort(unique(c(lambda_vals[idx], best_lam)))

        df_sub <- df[df$lambda %in% lam_sub, ]
        df_sub$lam_label <- factor(
          paste0("λ = ", signif(df_sub$lambda, 3)),
          levels = paste0("λ = ", signif(lam_sub, 3))
        )

        # Single best row for red circle
        best_row <- df_sub[abs(df_sub$alpha  - best_alpha) < 1e-9 &
                           abs(df_sub$lambda - best_lam)   < 1e-9, ]
        best_row$lam_label <- factor(paste0("λ = ", signif(best_row$lambda, 3)),
                                     levels = levels(df_sub$lam_label))

        has_sd    <- !all(is.na(df_sub$RMSESD))
        y_scales  <- if (isTRUE(input$glmnet_fixed_y)) "fixed" else "free_y"

        p <- ggplot2::ggplot(df_sub,
                             ggplot2::aes(x = alpha, y = RMSE,
                                          colour = alpha, fill = alpha))
        if (has_sd)
          p <- p + ggplot2::geom_ribbon(
            ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
            alpha = 0.15, colour = NA
          )
        p +
          ggplot2::geom_line(linewidth = 0.9) +
          ggplot2::geom_point(size = 2.5) +
          ggplot2::geom_point(data   = best_row,
                              colour = "#dc3545", size = 7, shape = 1, stroke = 1.8,
                              inherit.aes = FALSE,
                              ggplot2::aes(x = alpha, y = RMSE)) +
          ggplot2::scale_colour_viridis_c(name = "α", option = "plasma") +
          ggplot2::scale_fill_viridis_c(guide = "none", option = "plasma") +
          ggplot2::facet_wrap(~ lam_label, scales = y_scales) +
          ggplot2::labs(
            x        = "Mixing Percentage (α)",
            y        = paste0("RMSE (", .resample_label(mod), ")"),
            title    = "GLMnet tuning: RMSE by α, faceted by λ",
            subtitle = "Each panel: “under this λ, what α works best?”  |  red circle = best (α, λ)"
          ) +
          ggplot2::theme_bw(base_size = 13) +
          ggplot2::theme(
            strip.text      = ggplot2::element_text(face = "bold", size = 11),
            legend.position = "right",
            plot.title      = ggplot2::element_text(face = "bold", size = 14),
            plot.subtitle   = ggplot2::element_text(colour = "#6c757d", size = 11),
            axis.text       = ggplot2::element_text(size = 13),
            axis.title      = ggplot2::element_text(size = 13, face = "bold")
          )
      }
    })

    # ── Override PLS tuning plot: ncomp vs RMSE with styled axes ─────────────
    output[["pls_tune_plot"]] <- renderPlot({
      mod      <- models[["pls"]]; req(mod)
      df       <- mod$results
      has_sd   <- !all(is.na(df$RMSESD))
      best_nc  <- mod$bestTune$ncomp
      best_row <- df[df$ncomp == best_nc, ]

      p <- ggplot2::ggplot(df, ggplot2::aes(x = ncomp, y = RMSE))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          fill = "#17a2b8", alpha = 0.2
        )
      p +
        ggplot2::geom_line(colour = "#17a2b8", linewidth = 1) +
        ggplot2::geom_point(colour = "#17a2b8", size = 2.5) +
        ggplot2::geom_point(data   = best_row,
                            colour = "#dc3545", size = 7, shape = 1, stroke = 1.8) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::labs(
          x        = "Number of Components",
          y        = paste0("RMSE (", .resample_label(mod), ")"),
          title    = "PLS tuning: components vs RMSE",
          subtitle = "Resampled RMSE ± 1 SD  |  red circle = best ncomp"
        ) +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(
          plot.title    = ggplot2::element_text(face = "bold", size = 14),
          plot.subtitle = ggplot2::element_text(colour = "#6c757d", size = 11),
          axis.text     = ggplot2::element_text(size = 13),
          axis.title    = ggplot2::element_text(size = 13, face = "bold")
        )
    })

    # ── LM tuning plot: RMSE for intercept TRUE vs FALSE ─────────────────────
    # Single binary tuning param: intercept (TRUE/FALSE).
    output[["lm_tune_plot"]] <- renderPlot({
      mod <- models[["lm"]]; req(mod)
      df  <- mod$results

      has_sd  <- !all(is.na(df$RMSESD))
      best_ic <- mod$bestTune$intercept

      df$label <- ifelse(df$intercept, "With intercept", "No intercept")

      p <- ggplot2::ggplot(df, ggplot2::aes(x = label, y = RMSE,
                                             colour = label, fill = label))
      if (has_sd)
        p <- p + ggplot2::geom_errorbar(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          width = 0.12, linewidth = 0.8
        )
      p +
        ggplot2::geom_point(size = 5) +
        ggplot2::geom_point(
          data   = df[df$intercept == best_ic, ],
          colour = "#dc3545", size = 7, shape = 1, stroke = 1.8
        ) +
        ggplot2::scale_colour_manual(
          values = c("With intercept" = "#185FA5", "No intercept" = "#6c757d"),
          guide  = "none"
        ) +
        ggplot2::scale_fill_manual(
          values = c("With intercept" = "#185FA5", "No intercept" = "#6c757d"),
          guide  = "none"
        ) +
        ggplot2::labs(
          x        = "Intercept setting",
          y        = paste0("RMSE (", .resample_label(mod), ")"),
          title    = "LM tuning: intercept TRUE vs FALSE",
          subtitle = "Red circle = selected best  |  error bars = ± 1 SD"
        ) +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(
          plot.title    = ggplot2::element_text(face = "bold", size = 14),
          plot.subtitle = ggplot2::element_text(colour = "#6c757d", size = 11),
          axis.text     = ggplot2::element_text(size = 13),
          axis.title    = ggplot2::element_text(size = 13, face = "bold")
        )
    })

    # ── RLM tuning plot: RMSE per intercept × psi combination ────────────────
    # Two categorical tuning params: intercept (TRUE/FALSE), psi (3 options).
    output[["rlm_tune_plot"]] <- renderPlot({
      mod <- models[["rlm"]]; req(mod)
      df  <- mod$results

      has_sd <- !all(is.na(df$RMSESD))

      best_combo_intercept <- mod$bestTune$intercept
      best_combo_psi       <- mod$bestTune$psi

      df$intercept_label <- paste0("intercept = ", df$intercept)

      p <- ggplot2::ggplot(df, ggplot2::aes(x = psi, y = RMSE, colour = psi))
      if (has_sd)
        p <- p + ggplot2::geom_errorbar(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          width = 0.2
        )
      p +
        ggplot2::geom_point(size = 4) +
        ggplot2::geom_point(
          data = df[df$intercept == best_combo_intercept &
                    df$psi       == best_combo_psi, ],
          colour = "#dc3545", size = 6, shape = 1, stroke = 1.5
        ) +
        ggplot2::facet_wrap(~ intercept_label) +
        ggplot2::scale_colour_manual(
          values = c("psi.huber"    = "#185FA5",
                     "psi.hampel"   = "#0F6E56",
                     "psi.bisquare" = "#BA7517")
        ) +
        ggplot2::labs(
          x        = "Psi function",
          y        = paste0("RMSE (", .resample_label(mod), ")"),
          colour   = "psi",
          title    = "RLM tuning: RMSE per intercept × psi combination",
          subtitle = "Red circle = best combination  |  error bars = ± 1 SD"
        ) +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(
          strip.text      = ggplot2::element_text(face = "bold", size = 11),
          legend.position = "right",
          plot.title      = ggplot2::element_text(face = "bold", size = 14),
          plot.subtitle   = ggplot2::element_text(colour = "#6c757d", size = 11),
          axis.text       = ggplot2::element_text(size = 12),
          axis.title      = ggplot2::element_text(size = 13, face = "bold")
        )
    })

    # ── Preprocessing selector update ─────────────────────────────────────────
    # Fires when the active method tab or config mode changes.
    observe({
      method   <- current_method()
      mode     <- input$config_mode
      selected <- if (is.null(mode) || mode == "general") {
        general_preprocess %||% character(0)
      } else {
        switch(method,
               "lm"     = lm_preprocess     %||% general_preprocess %||% character(0),
               "rlm"    = rlm_preprocess    %||% general_preprocess %||% character(0),
               "glmnet" = glmnet_preprocess %||% general_preprocess %||% character(0),
               "pls"    = pls_preprocess    %||% general_preprocess %||% character(0),
               general_preprocess %||% character(0))
      }
      updateSelectizeInput(session, "preprocess",
                           choices  = pp_choices,
                           selected = selected)
    }) |> bindEvent(current_method(), input$config_mode)

    # ── GLMnet tuning grid builder ────────────────────────────────────────────
    build_glmnet_grid <- function() {
      penalty  <- input$glmnet_penalty
      alpha_g  <- switch(penalty,
        "ridge"      = 0,
        "lasso"      = 1,
        "elasticnet" = seq(input$glmnet_alpha_min, input$glmnet_alpha_max,
                           by = input$glmnet_alpha_step)
      )
      lambda_g <- 10^seq(input$glmnet_log_lam_min, input$glmnet_log_lam_max,
                         length.out = input$glmnet_lam_n)
      expand.grid(alpha = alpha_g, lambda = lambda_g)
    }

    # ── Train / Load / Delete ─────────────────────────────────────────────────
    .meth_action_dispatcher(
      input, output, session,
      models         = models,
      current_method = current_method,
      roles          = roles,
      train_fns = list(

        lm = function() {
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(
            input, eseed, train_df[[names(r)[r == "outcome"][1]]]
          )
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          caret::train(rec, data = train_df, method = "lm",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.pass)
        },

        glmnet = function() {
          library(glmnet)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(
            input, eseed, train_df[[names(r)[r == "outcome"][1]]]
          )
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)

          if (isTRUE(input$config_mode == "specific") &&
              isTRUE(input$glmnet_grid_type == "custom")) {
            caret::train(rec, data = train_df, method = "glmnet",
                         metric = "RMSE", trControl = tr_ctrl,
                         tuneGrid = build_glmnet_grid(), na.action = na.pass)

          } else if (isTRUE(input$config_mode == "specific") &&
                     input$glmnet_penalty %in% c("ridge", "lasso")) {
            alpha_fixed <- if (input$glmnet_penalty == "ridge") 0 else 1
            caret::train(rec, data = train_df, method = "glmnet",
                         metric = "RMSE", trControl = tr_ctrl, na.action = na.pass,
                         tuneGrid = expand.grid(alpha  = alpha_fixed,
                                                lambda = 10^seq(3, -4, length.out = 50)))
          } else {
            caret::train(rec, data = train_df, method = "glmnet",
                         metric = "RMSE", trControl = tr_ctrl,
                         tuneLength = input$tune_length %||% 5, na.action = na.pass)
          }
        },

        pls = function() {
          library(pls)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(
            input, eseed, train_df[[names(r)[r == "outcome"][1]]]
          )
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          if (isTRUE(input$config_mode == "specific") &&
              isTRUE(input$pls_grid_type == "custom")) {
            ncomp_g <- seq(input$pls_ncomp_min, input$pls_ncomp_max, by = 1L)
            caret::train(rec, data = train_df, method = "pls",
                         metric = "RMSE", trControl = tr_ctrl,
                         tuneGrid = data.frame(ncomp = ncomp_g), na.action = na.pass)
          } else {
            caret::train(rec, data = train_df, method = "pls",
                         metric = "RMSE", trControl = tr_ctrl,
                         tuneLength = input$tune_length %||% 25, na.action = na.pass)
          }
        },

        rlm = function() {
          library(MASS)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(
            input, eseed, train_df[[names(r)[r == "outcome"][1]]]
          )
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          caret::train(rec, data = train_df, method = "rlm",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.pass)
        }

      )
    )

    # ── Return ────────────────────────────────────────────────────────────────
    return(list(models = models, effective_seed = effective_seed))
  })
}
