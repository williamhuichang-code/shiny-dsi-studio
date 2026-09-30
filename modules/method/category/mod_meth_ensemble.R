# =================================================================================
# mod_meth_ensemble.R  — Ensemble Methods category  (Ranger + bagEarth + avNNet)
# =================================================================================
# UI:     meth_ensemble_ui(id, pp_choices, default_preprocess, model_seed)
# Server: meth_ensemble_server(id, get_data, roles, seed, model_seed,
#                               general_preprocess, ranger_preprocess,
#                               bagearth_preprocess, avnnet_preprocess, pp_choices)
# Returns: list(models = reactiveValues, effective_seed = reactive)
# =================================================================================


# ── UI ───────────────────────────────────────────────────────────────────────

meth_ensemble_ui <- function(id,
                              pp_choices         = character(0),
                              default_preprocess = character(0),
                              model_seed         = NULL) {
  ns <- NS(id)

  fluidRow(
    column(9,
      tabsetPanel(type = "tabs", id = ns("method_inner"),
        tabPanel("ranger",   value = "ranger",   style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "ranger",   has_tuning = TRUE)),
        tabPanel("bagEarth", value = "bagEarth", style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "bagEarth", has_tuning = TRUE)),
        tabPanel("avNNet",   value = "avNNet",   style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "avNNet",   has_tuning = TRUE))
      )
    ),
    column(3,
      .meth_sidebar_ui(ns,
                       model_seed         = model_seed,
                       pp_choices         = pp_choices,
                       default_preprocess = default_preprocess,
                       specific_panels    = NULL)
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

meth_ensemble_server <- function(id, get_data, roles,
                                  seed                 = reactive(2026),
                                  model_seed           = NULL,
                                  general_preprocess   = NULL,
                                  ranger_preprocess    = NULL,
                                  bagearth_preprocess  = NULL,
                                  avnnet_preprocess    = NULL,
                                  pp_choices           = character(0)) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    models <- reactiveValues()

    # ── Common setup ──────────────────────────────────────────────────────────
    setup          <- .meth_common_server_setup(input, output, session, get_data, roles, seed,
                                                model_seed = model_seed)
    effective_seed <- setup$effective_seed
    get_train      <- setup$get_train

    current_method <- reactive({ input$method_inner %||% "ranger" })

    # ── Standard output renders ───────────────────────────────────────────────
    .meth_register_outputs(output, "ranger",   models, ns)
    .meth_register_outputs(output, "bagEarth", models, ns)
    .meth_register_outputs(output, "avNNet",   models, ns)

    # ── Ranger tuning plot: facet by splitrule, x = mtry, colour = min.node.size
    output[["ranger_tune_plot"]] <- renderPlot({
      mod <- models[["ranger"]]; req(mod)
      df  <- mod$results

      has_sd <- !all(is.na(df$RMSESD))

      best_mtry      <- mod$bestTune$mtry
      best_splitrule <- mod$bestTune$splitrule
      best_mns       <- mod$bestTune$min.node.size

      p <- ggplot2::ggplot(df,
                           ggplot2::aes(x      = mtry,
                                        y      = RMSE,
                                        colour = factor(min.node.size),
                                        group  = factor(min.node.size)))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD,
                       fill = factor(min.node.size)),
          alpha = 0.1, colour = NA
        )
      p +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::geom_vline(
          data = data.frame(splitrule = best_splitrule, xint = best_mtry),
          ggplot2::aes(xintercept = xint),
          linetype = "dashed", colour = "#dc3545", linewidth = 0.7,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_colour_viridis_d(name = "min.node.size", option = "viridis") +
        ggplot2::scale_fill_viridis_d(guide = "none",           option = "viridis") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::facet_wrap(~ splitrule) +
        ggplot2::labs(x        = "Variables per split (mtry)",
                      y        = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "Ranger tuning: mtry vs RMSE, faceted by split rule",
                      subtitle = "Lines = min.node.size levels  |  dashed = best mtry") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(strip.text      = ggplot2::element_text(face = "bold", size = 11),
                       legend.position = "right",
                       plot.title      = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle   = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text       = ggplot2::element_text(size = 13),
                       axis.title      = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── bagEarth tuning plot: facet by degree, x = nprune ────────────────────
    output[["bagEarth_tune_plot"]] <- renderPlot({
      mod <- models[["bagEarth"]]; req(mod)
      df  <- mod$results

      has_sd      <- !all(is.na(df$RMSESD))
      best_nprune <- mod$bestTune$nprune
      best_degree <- mod$bestTune$degree

      df$deg_label <- paste0("degree = ", df$degree)

      p <- ggplot2::ggplot(df, ggplot2::aes(x = nprune, y = RMSE,
                                             colour = factor(degree),
                                             group  = factor(degree)))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD,
                       fill = factor(degree)),
          alpha = 0.15, colour = NA
        )
      p +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::geom_vline(
          data = data.frame(deg_label = paste0("degree = ", best_degree),
                            xint      = best_nprune),
          ggplot2::aes(xintercept = xint),
          linetype = "dashed", colour = "#dc3545", linewidth = 0.7,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_colour_viridis_d(name = "degree", option = "viridis") +
        ggplot2::scale_fill_viridis_d(guide = "none",    option = "viridis") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::facet_wrap(~ deg_label, scales = "free_y") +
        ggplot2::labs(x        = "Retained terms (nprune)",
                      y        = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "bagEarth tuning: nprune vs RMSE, faceted by degree",
                      subtitle = "degree = max interaction depth  |  dashed = best nprune") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(strip.text      = ggplot2::element_text(face = "bold", size = 11),
                       legend.position = "right",
                       plot.title      = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle   = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text       = ggplot2::element_text(size = 13),
                       axis.title      = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── avNNet tuning plot: facet by bag, x = size, colour = log10(decay) ───────
    output[["avNNet_tune_plot"]] <- renderPlot({
      mod <- models[["avNNet"]]; req(mod)
      df  <- mod$results

      has_sd     <- !all(is.na(df$RMSESD))
      best_size  <- mod$bestTune$size
      best_decay <- mod$bestTune$decay
      best_bag   <- mod$bestTune$bag

      df$bag_label <- ifelse(df$bag, "bag = TRUE", "bag = FALSE")

      p <- ggplot2::ggplot(df,
                           ggplot2::aes(x      = size,
                                        y      = RMSE,
                                        colour = log10(decay + 1e-10),
                                        group  = decay))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD,
                       fill = log10(decay + 1e-10)),
          alpha = 0.12, colour = NA
        )
      p +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::geom_vline(
          data = data.frame(bag_label = ifelse(best_bag, "bag = TRUE", "bag = FALSE"),
                            xint      = best_size),
          ggplot2::aes(xintercept = xint),
          linetype = "dashed", colour = "#dc3545", linewidth = 0.7,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_colour_viridis_c(name = "log₁₀(decay)", option = "plasma") +
        ggplot2::scale_fill_viridis_c(guide = "none",             option = "plasma") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::facet_wrap(~ bag_label) +
        ggplot2::labs(x        = "Hidden units (size)",
                      y        = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "avNNet tuning: size vs RMSE, faceted by bagging",
                      subtitle = "Colour = weight decay  |  dashed = best size") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(strip.text      = ggplot2::element_text(face = "bold", size = 11),
                       legend.position = "right",
                       plot.title      = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle   = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text       = ggplot2::element_text(size = 13),
                       axis.title      = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── Preprocessing selector update ─────────────────────────────────────────
    observe({
      method   <- current_method()
      mode     <- input$config_mode
      selected <- if (is.null(mode) || mode == "general") {
        general_preprocess %||% character(0)
      } else {
        switch(method,
               "ranger"   = ranger_preprocess   %||% general_preprocess %||% character(0),
               "bagEarth" = bagearth_preprocess  %||% general_preprocess %||% character(0),
               "avNNet"   = avnnet_preprocess    %||% general_preprocess %||% character(0),
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

        ranger = function() {
          library(ranger)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          # NOTE: na.action is intentionally omitted for ranger.
          # ranger internally does `na.action == "na.fail"` as a string comparison,
          # which throws "comparison is possible only for atomic and list types" when
          # na.action is a function object (e.g. na.omit). Handle NAs via recipe
          # preprocessing steps (naomit / imputation) instead.
          caret::train(rec, data = train_df, method = "ranger",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5,
                       num.threads = 1)   # disable ranger's own threading — caret handles parallelism
        },

        bagEarth = function() {
          library(earth)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          # bagEarth wraps earth — same na.fail restriction applies.
          caret::train(rec, data = train_df, method = "bagEarth",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.fail)
        },

        avNNet = function() {
          library(nnet)
          train_df    <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed       <- effective_seed()
          r           <- roles()
          outcome_col <- names(r)[r == "outcome"][1]
          tr_ctrl     <- .meth_build_tr_control(input, eseed, train_df[[outcome_col]])
          rec         <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          # Response standardised to mean=0, sd=1 — avNNet averages multiple nnet
          # models; same response-scale sensitivity as mlpWeightDecay applies.
          .meth_nnet_normalised_train(rec, train_df, outcome_col, "avNNet",
                                      tr_ctrl, input$tune_length %||% 5)
        }

      )
    )

    # ── Return ────────────────────────────────────────────────────────────────
    return(list(models = models, effective_seed = effective_seed))
  })
}
