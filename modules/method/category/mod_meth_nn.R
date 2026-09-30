# =================================================================================
# mod_meth_nn.R  — Neural Network category
#   qrnn / brnn / pcaNNet / mlpWeightDecay / mlpML / monmlp
# =================================================================================
#
# UI:     meth_nn_ui(id, pp_choices, default_preprocess, model_seed)
# Server: meth_nn_server(id, get_data, roles, seed, model_seed,
#                         general_preprocess,
#                         qrnn_preprocess / brnn_preprocess / pcannet_preprocess /
#                         mlpwd_preprocess / mlpml_preprocess / monmlp_preprocess,
#                         pp_choices)
# Returns: list(models = reactiveValues, effective_seed = reactive)
# =================================================================================


# ── UI ───────────────────────────────────────────────────────────────────────

meth_nn_ui <- function(id,
                       pp_choices         = character(0),
                       default_preprocess = character(0),
                       model_seed         = NULL) {
  ns <- NS(id)

  fluidRow(
    column(9,
      tabsetPanel(type = "tabs", id = ns("method_inner"),
        tabPanel("qrnn",          value = "qrnn",          style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "qrnn",          has_tuning = TRUE)),
        tabPanel("brnn",          value = "brnn",          style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "brnn",          has_tuning = TRUE)),
        tabPanel("mlpWeightDecay",value = "mlpWeightDecay",style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "mlpWeightDecay",has_tuning = TRUE)),
        tabPanel("monmlp",        value = "monmlp",        style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "monmlp",        has_tuning = TRUE)),
        tabPanel("mlpML",         value = "mlpML",         style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "mlpML",         has_tuning = TRUE)),
        tabPanel("pcaNNet",       value = "pcaNNet",       style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "pcaNNet",       has_tuning = TRUE))
      )
    ),
    column(3,
      .meth_sidebar_ui(ns,
                       model_seed         = model_seed,
                       pp_choices         = pp_choices,
                       default_preprocess = default_preprocess,
                       specific_panels    = tagList(

        # ── brnn-specific controls ───────────────────────────────────────────
        conditionalPanel(
          condition = sprintf("input['%s'] === 'brnn'", ns("method_inner")),

          tags$label("Tuning grid:", style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
          selectInput(ns("brnn_grid_type"), NULL,
                      choices  = c("Tune length default"  = "tunelength",
                                   "Custom neurons range" = "custom"),
                      selected = "custom", width = "100%"),

          conditionalPanel(
            condition = sprintf("input['%s'] === 'custom'", ns("brnn_grid_type")),

            tags$label("Neurons minimum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("brnn_neurons_min"),  NULL, min = 1, max = 5,  value = 1,  step = 1, width = "100%"),
            tags$label("Neurons maximum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("brnn_neurons_max"),  NULL, min = 2, max = 20, value = 10, step = 1, width = "100%"),
            tags$label("Neurons step:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("brnn_neurons_step"), NULL, min = 1, max = 5,  value = 1,  step = 1, width = "100%")
          )
        )

      ))
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

meth_nn_server <- function(id, get_data, roles,
                            seed               = reactive(2026),
                            model_seed         = NULL,
                            general_preprocess = NULL,
                            qrnn_preprocess    = NULL,
                            brnn_preprocess    = NULL,
                            pcannet_preprocess = NULL,
                            mlpwd_preprocess   = NULL,
                            mlpml_preprocess   = NULL,
                            monmlp_preprocess  = NULL,
                            pp_choices         = character(0)) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    models <- reactiveValues()

    # ── Common setup ──────────────────────────────────────────────────────────
    setup          <- .meth_common_server_setup(input, output, session, get_data, roles, seed,
                                                model_seed = model_seed)
    effective_seed <- setup$effective_seed
    get_train      <- setup$get_train

    current_method <- reactive({ input$method_inner %||% "qrnn" })

    # ── Standard output renders ───────────────────────────────────────────────
    .meth_register_outputs(output, "qrnn",          models, ns)
    .meth_register_outputs(output, "brnn",          models, ns)
    .meth_register_outputs(output, "pcaNNet",       models, ns)
    .meth_register_outputs(output, "mlpWeightDecay",models, ns)
    .meth_register_outputs(output, "mlpML",         models, ns)
    .meth_register_outputs(output, "monmlp",        models, ns)

    # ── QRNN tuning plot: facet by bag, x = n.hidden, colour = log10(penalty) ─
    output[["qrnn_tune_plot"]] <- renderPlot({
      mod <- models[["qrnn"]]; req(mod)
      df  <- mod$results

      has_sd   <- !all(is.na(df$RMSESD))
      best_nh  <- mod$bestTune$n.hidden
      best_pen <- mod$bestTune$penalty
      best_bag <- mod$bestTune$bag

      df$bag_label <- ifelse(df$bag, "bag = TRUE", "bag = FALSE")

      p <- ggplot2::ggplot(df,
                           ggplot2::aes(x      = n.hidden,
                                        y      = RMSE,
                                        colour = log10(penalty + 1e-10),
                                        group  = penalty))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD,
                       fill = log10(penalty + 1e-10)),
          alpha = 0.12, colour = NA
        )
      p +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::geom_vline(
          data = data.frame(bag_label = ifelse(best_bag, "bag = TRUE", "bag = FALSE"),
                            xint      = best_nh),
          ggplot2::aes(xintercept = xint),
          linetype = "dashed", colour = "#dc3545", linewidth = 0.7,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_colour_viridis_c(name = "log₁₀(penalty)", option = "plasma") +
        ggplot2::scale_fill_viridis_c(guide = "none",             option = "plasma") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::facet_wrap(~ bag_label) +
        ggplot2::labs(x        = "Hidden units (n.hidden)",
                      y        = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "QRNN tuning: hidden units vs RMSE, faceted by bagging",
                      subtitle = "Colour = weight-decay penalty  |  dashed = best n.hidden") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(strip.text      = ggplot2::element_text(face = "bold", size = 11),
                       legend.position = "right",
                       plot.title      = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle   = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text       = ggplot2::element_text(size = 13),
                       axis.title      = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── BRNN tuning plot: RMSE vs neurons ─────────────────────────────────────
    output[["brnn_tune_plot"]] <- renderPlot({
      mod    <- models[["brnn"]]; req(mod)
      df     <- mod$results
      has_sd <- !all(is.na(df$RMSESD))
      best_n <- mod$bestTune$neurons

      p <- ggplot2::ggplot(df, ggplot2::aes(x = neurons, y = RMSE))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          fill = "#fd7e14", alpha = 0.2
        )
      p +
        ggplot2::geom_line(colour = "#fd7e14", linewidth = 1) +
        ggplot2::geom_point(colour = "#fd7e14", size = 2.5) +
        ggplot2::geom_vline(xintercept = best_n,
                            linetype = "dashed", colour = "#dc3545", linewidth = 0.8) +
        ggplot2::annotate("text",
                          x = best_n, y = max(df$RMSE, na.rm = TRUE),
                          label = paste0("best neurons = ", best_n),
                          hjust = -0.1, vjust = 1,
                          colour = "#dc3545", size = 4, fontface = "bold") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::labs(x        = "Number of neurons",
                      y        = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "BRNN tuning: neurons vs RMSE",
                      subtitle = "Resampled RMSE ± 1 SD  |  dashed line = best neurons") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(plot.title    = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text     = ggplot2::element_text(size = 13),
                       axis.title    = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── pcaNNet tuning plot: x = size, colour = log10(decay) ─────────────────
    output[["pcaNNet_tune_plot"]] <- renderPlot({
      mod    <- models[["pcaNNet"]]; req(mod)
      df     <- mod$results
      has_sd <- !all(is.na(df$RMSESD))

      best_size  <- mod$bestTune$size
      best_decay <- mod$bestTune$decay

      p <- ggplot2::ggplot(df,
                           ggplot2::aes(x      = size,
                                        y      = RMSE,
                                        colour = log10(decay + 1e-10),
                                        group  = factor(decay)))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD,
                       fill = log10(decay + 1e-10)),
          alpha = 0.12, colour = NA
        )
      p +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::geom_vline(xintercept = best_size,
                            linetype = "dashed", colour = "#dc3545", linewidth = 0.8) +
        ggplot2::scale_colour_viridis_c(name = "log₁₀(decay)", option = "magma") +
        ggplot2::scale_fill_viridis_c(guide = "none",            option = "magma") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::labs(x        = "Hidden units (size)",
                      y        = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "pcaNNet tuning: size vs RMSE",
                      subtitle = "Colour = weight decay  |  dashed = best size") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(legend.position = "right",
                       plot.title      = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle   = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text       = ggplot2::element_text(size = 13),
                       axis.title      = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── mlpWeightDecay tuning plot: x = size, colour = log10(decay) ──────────
    output[["mlpWeightDecay_tune_plot"]] <- renderPlot({
      mod    <- models[["mlpWeightDecay"]]; req(mod)
      df     <- mod$results
      has_sd <- !all(is.na(df$RMSESD))

      best_size  <- mod$bestTune$size
      best_decay <- mod$bestTune$decay

      p <- ggplot2::ggplot(df,
                           ggplot2::aes(x      = size,
                                        y      = RMSE,
                                        colour = log10(decay + 1e-10),
                                        group  = factor(decay)))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD,
                       fill = log10(decay + 1e-10)),
          alpha = 0.12, colour = NA
        )
      p +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::geom_vline(xintercept = best_size,
                            linetype = "dashed", colour = "#dc3545", linewidth = 0.8) +
        ggplot2::scale_colour_viridis_c(name = "log₁₀(decay)", option = "inferno") +
        ggplot2::scale_fill_viridis_c(guide = "none",            option = "inferno") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::labs(x        = "Hidden units (size)",
                      y        = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "MLP Weight Decay tuning: size vs RMSE",
                      subtitle = "Colour = weight decay  |  dashed = best size") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(legend.position = "right",
                       plot.title      = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle   = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text       = ggplot2::element_text(size = 13),
                       axis.title      = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── mlpML tuning plot: facet by layer3, x = layer1, colour = layer2 ──────
    output[["mlpML_tune_plot"]] <- renderPlot({
      mod <- models[["mlpML"]]; req(mod)
      df  <- mod$results

      has_sd <- !all(is.na(df$RMSESD))
      best_l1 <- mod$bestTune$layer1
      best_l2 <- mod$bestTune$layer2
      best_l3 <- mod$bestTune$layer3

      df$l3_label <- paste0("layer3 = ", df$layer3)

      p <- ggplot2::ggplot(df,
                           ggplot2::aes(x      = layer1,
                                        y      = RMSE,
                                        colour = factor(layer2),
                                        group  = factor(layer2)))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD,
                       fill = factor(layer2)),
          alpha = 0.12, colour = NA
        )
      p +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::geom_vline(
          data = data.frame(l3_label = paste0("layer3 = ", best_l3), xint = best_l1),
          ggplot2::aes(xintercept = xint),
          linetype = "dashed", colour = "#dc3545", linewidth = 0.7,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_colour_viridis_d(name = "layer2", option = "viridis") +
        ggplot2::scale_fill_viridis_d(guide = "none",    option = "viridis") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::facet_wrap(~ l3_label, scales = "free_y") +
        ggplot2::labs(x        = "Neurons in layer 1",
                      y        = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "MLP Multi-Layer tuning: layer1 vs RMSE, faceted by layer3",
                      subtitle = "Colour = layer2 neurons  |  dashed = best layer1") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(strip.text      = ggplot2::element_text(face = "bold", size = 11),
                       legend.position = "right",
                       plot.title      = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle   = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text       = ggplot2::element_text(size = 13),
                       axis.title      = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── monmlp tuning plot: facet by n.ensemble, x = hidden1 ─────────────────
    output[["monmlp_tune_plot"]] <- renderPlot({
      mod <- models[["monmlp"]]; req(mod)
      df  <- mod$results

      has_sd    <- !all(is.na(df$RMSESD))
      best_h1   <- mod$bestTune$hidden1
      best_ens  <- mod$bestTune$n.ensemble

      df$ens_label <- paste0("n.ensemble = ", df$n.ensemble)

      p <- ggplot2::ggplot(df, ggplot2::aes(x = hidden1, y = RMSE))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          fill = "#198754", alpha = 0.2
        )
      p +
        ggplot2::geom_line(colour = "#198754", linewidth = 0.9) +
        ggplot2::geom_point(colour = "#198754", size = 2.5) +
        ggplot2::geom_vline(
          data = data.frame(ens_label = paste0("n.ensemble = ", best_ens), xint = best_h1),
          ggplot2::aes(xintercept = xint),
          linetype = "dashed", colour = "#dc3545", linewidth = 0.7,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::facet_wrap(~ ens_label, scales = "free_y") +
        ggplot2::labs(x        = "Hidden units in layer 1 (hidden1)",
                      y        = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "monmlp tuning: hidden1 vs RMSE, faceted by n.ensemble",
                      subtitle = "Resampled RMSE ± 1 SD  |  dashed = best hidden1") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(strip.text    = ggplot2::element_text(face = "bold", size = 11),
                       plot.title    = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text     = ggplot2::element_text(size = 13),
                       axis.title    = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── Preprocessing selector update ─────────────────────────────────────────
    observe({
      method   <- current_method()
      mode     <- input$config_mode
      selected <- if (is.null(mode) || mode == "general") {
        general_preprocess %||% character(0)
      } else {
        switch(method,
               "qrnn"          = qrnn_preprocess    %||% general_preprocess %||% character(0),
               "brnn"          = brnn_preprocess    %||% general_preprocess %||% character(0),
               "pcaNNet"       = pcannet_preprocess %||% general_preprocess %||% character(0),
               "mlpWeightDecay"= mlpwd_preprocess   %||% general_preprocess %||% character(0),
               "mlpML"         = mlpml_preprocess   %||% general_preprocess %||% character(0),
               "monmlp"        = monmlp_preprocess  %||% general_preprocess %||% character(0),
               general_preprocess %||% character(0))
      }
      updateSelectizeInput(session, "preprocess",
                           choices  = pp_choices,
                           selected = selected)
    }) |> bindEvent(current_method(), input$config_mode)

    # ── Train / Load / Delete ─────────────────────────────────────────────────
    .meth_action_dispatcher(
      input, output, session,
      models         = models,
      current_method = current_method,
      roles          = roles,
      train_fns = list(

        qrnn = function() {
          library(qrnn)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          # NOTE: na.action omitted — qrnn::qrnn.fit() passes data directly to nlm() which
          # cannot handle NAs. Ensure NAs are handled in the preprocessing pipeline.
          caret::train(rec, data = train_df, method = "qrnn",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5)
        },

        brnn = function() {
          library(brnn)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          if (isTRUE(input$config_mode == "specific") &&
              isTRUE(input$brnn_grid_type == "custom")) {
            neurons_g <- seq(input$brnn_neurons_min, input$brnn_neurons_max,
                             by = input$brnn_neurons_step)
            caret::train(rec, data = train_df, method = "brnn",
                         metric = "RMSE", trControl = tr_ctrl,
                         tuneGrid = data.frame(neurons = neurons_g), na.action = na.pass)
          } else {
            caret::train(rec, data = train_df, method = "brnn",
                         metric = "RMSE", trControl = tr_ctrl,
                         tuneLength = input$tune_length %||% 5, na.action = na.pass)
          }
        },

        pcaNNet = function() {
          library(nnet)
          train_df    <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed       <- effective_seed()
          r           <- roles()
          outcome_col <- names(r)[r == "outcome"][1]
          tr_ctrl     <- .meth_build_tr_control(input, eseed, train_df[[outcome_col]])
          rec         <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          # Response standardised to mean=0, sd=1 — pcaNNet's internal PCA makes the
          # scale mismatch especially severe without normalisation (predictions → ≈0).
          .meth_nnet_normalised_train(rec, train_df, outcome_col, "pcaNNet",
                                      tr_ctrl, input$tune_length %||% 5)
        },

        mlpWeightDecay = function() {
          library(nnet)
          train_df    <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed       <- effective_seed()
          r           <- roles()
          outcome_col <- names(r)[r == "outcome"][1]
          tr_ctrl     <- .meth_build_tr_control(input, eseed, train_df[[outcome_col]])
          rec         <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          # Response standardised to mean=0, sd=1 — nnet's optimizer diverges on
          # large-scale responses, collapsing predictions to near-zero without normalisation.
          .meth_nnet_normalised_train(rec, train_df, outcome_col, "mlpWeightDecay",
                                      tr_ctrl, input$tune_length %||% 5)
        },

        mlpML = function() {
          library(RSNNS)
          train_df    <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed       <- effective_seed()
          r           <- roles()
          outcome_col <- names(r)[r == "outcome"][1]
          tr_ctrl     <- .meth_build_tr_control(input, eseed, train_df[[outcome_col]])
          rec         <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          # Response standardised to mean=0, sd=1 — RSNNS shares the same scale
          # sensitivity as nnet; predictions collapse to near-zero without normalisation.
          .meth_nnet_normalised_train(rec, train_df, outcome_col, "mlpML",
                                      tr_ctrl, input$tune_length %||% 5)
        },

        monmlp = function() {
          library(monmlp)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          # NOTE: na.action omitted — monmlp::monmlp.fit() does not accept na.action argument.
          caret::train(rec, data = train_df, method = "monmlp",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5)
        }

      )
    )

    # ── Return ────────────────────────────────────────────────────────────────
    return(list(models = models, effective_seed = effective_seed))
  })
}
