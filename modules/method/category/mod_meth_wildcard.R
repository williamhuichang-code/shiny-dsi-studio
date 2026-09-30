# =================================================================================
# mod_meth_wildcard.R  — WildCard category
#                        (Earth / MARS + gcvEarth + gam + gamSpline + gamboost + PPR)
# =================================================================================
# UI:     meth_wildcard_ui(id, pp_choices, default_preprocess, model_seed)
# Server: meth_wildcard_server(id, get_data, roles, seed, model_seed,
#                               general_preprocess, earth_preprocess,
#                               gcvearth_preprocess, gam_preprocess,
#                               gamspline_preprocess, gamboost_preprocess,
#                               m5_preprocess, ppr_preprocess, pp_choices)
# Returns: list(models = reactiveValues, effective_seed = reactive)
# =================================================================================


# ── UI ───────────────────────────────────────────────────────────────────────

meth_wildcard_ui <- function(id,
                              pp_choices         = character(0),
                              default_preprocess = character(0),
                              model_seed         = NULL) {
  ns <- NS(id)

  fluidRow(
    column(9,
      tabsetPanel(type = "tabs", id = ns("method_inner"),
        tabPanel("earth",     value = "earth",     style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "earth",     has_tuning = TRUE)),
        tabPanel("gcvEarth",  value = "gcvEarth",  style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "gcvEarth",  has_tuning = TRUE)),
        tabPanel("gam",       value = "gam",       style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "gam",       has_tuning = TRUE)),
        tabPanel("gamSpline", value = "gamSpline", style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "gamSpline", has_tuning = TRUE)),
        tabPanel("gamboost",  value = "gamboost",  style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "gamboost",  has_tuning = TRUE)),
        tabPanel("ppr",       value = "ppr",       style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "ppr",       has_tuning = TRUE))
        # tabPanel("M5",    value = "M5",    style = "padding-top:12px;",
        #          .meth_subtabs_ui(ns, "M5",    has_tuning = TRUE))
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

meth_wildcard_server <- function(id, get_data, roles,
                                  seed                 = reactive(2026),
                                  model_seed           = NULL,
                                  general_preprocess   = NULL,
                                  earth_preprocess     = NULL,
                                  gcvearth_preprocess  = NULL,
                                  gam_preprocess       = NULL,
                                  gamspline_preprocess = NULL,
                                  gamboost_preprocess  = NULL,
                                  m5_preprocess        = NULL,
                                  ppr_preprocess       = NULL,
                                  pp_choices           = character(0)) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    models <- reactiveValues()

    # ── Common setup ──────────────────────────────────────────────────────────
    setup          <- .meth_common_server_setup(input, output, session, get_data, roles, seed,
                                                model_seed = model_seed)
    effective_seed <- setup$effective_seed
    get_train      <- setup$get_train

    current_method <- reactive({ input$method_inner %||% "earth" })

    # ── Standard output renders ───────────────────────────────────────────────
    .meth_register_outputs(output, "earth",     models, ns)
    .meth_register_outputs(output, "gcvEarth",  models, ns)
    .meth_register_outputs(output, "gam",       models, ns)
    .meth_register_outputs(output, "gamSpline", models, ns)
    .meth_register_outputs(output, "gamboost",  models, ns)
    .meth_register_outputs(output, "M5",        models, ns)
    .meth_register_outputs(output, "ppr",       models, ns)

    # ── Earth tuning plot: facet by degree, x = nprune ───────────────────────
    .earth_tune_plot <- function(mod) {
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
                      subtitle = "degree = max interaction depth  |  dashed = best nprune") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(strip.text      = ggplot2::element_text(face = "bold", size = 11),
                       legend.position = "right",
                       plot.title      = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle   = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text       = ggplot2::element_text(size = 13),
                       axis.title      = ggplot2::element_text(size = 13, face = "bold"))
    }

    output[["earth_tune_plot"]] <- renderPlot({
      mod <- models[["earth"]]; req(mod)
      .earth_tune_plot(mod) +
        ggplot2::labs(title = "Earth (MARS) tuning: nprune vs RMSE, faceted by degree")
    })

    output[["gcvEarth_tune_plot"]] <- renderPlot({
      mod <- models[["gcvEarth"]]; req(mod)
      .earth_tune_plot(mod) +
        ggplot2::labs(title = "gcvEarth tuning: nprune vs RMSE, faceted by degree")
    })

    # ── gamSpline tuning plot: RMSE vs df ────────────────────────────────────
    # Single tuning parameter: df (degrees of freedom per spline term).
    output[["gamSpline_tune_plot"]] <- renderPlot({
      mod    <- models[["gamSpline"]]; req(mod)
      df     <- mod$results
      has_sd <- !all(is.na(df$RMSESD))
      best_df <- mod$bestTune$df

      p <- ggplot2::ggplot(df, ggplot2::aes(x = df, y = RMSE))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          fill = "#6f42c1", alpha = 0.2
        )
      p +
        ggplot2::geom_line(colour = "#6f42c1", linewidth = 1) +
        ggplot2::geom_point(colour = "#6f42c1", size = 2.5) +
        ggplot2::geom_vline(xintercept = best_df,
                            linetype = "dashed", colour = "#dc3545", linewidth = 0.8) +
        ggplot2::annotate("text",
                          x     = best_df,
                          y     = max(df$RMSE, na.rm = TRUE),
                          label = paste0("best df = ", best_df),
                          hjust = -0.1, vjust = 1,
                          colour = "#dc3545", size = 4, fontface = "bold") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::labs(x        = "Degrees of freedom (df)",
                      y        = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "GAM Spline tuning: df vs RMSE",
                      subtitle = "Resampled RMSE ± 1 SD  |  dashed line = best df") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(plot.title    = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text     = ggplot2::element_text(size = 13),
                       axis.title    = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── gamboost tuning plot: RMSE vs mstop, coloured by prune ───────────────
    output[["gamboost_tune_plot"]] <- renderPlot({
      mod    <- models[["gamboost"]]; req(mod)
      df     <- mod$results
      has_sd <- !all(is.na(df$RMSESD))

      best_mstop <- mod$bestTune$mstop
      best_prune <- mod$bestTune$prune

      p <- ggplot2::ggplot(df, ggplot2::aes(x = mstop, y = RMSE,
                                             colour = prune, group = prune))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD,
                       fill = prune),
          alpha = 0.15, colour = NA
        )
      p +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::geom_vline(
          data = data.frame(prune = best_prune, xint = best_mstop),
          ggplot2::aes(xintercept = xint),
          linetype = "dashed", colour = "#dc3545", linewidth = 0.7,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_colour_manual(values = c("yes" = "#6f42c1", "no" = "#fd7e14")) +
        ggplot2::scale_fill_manual(guide = "none", values = c("yes" = "#6f42c1", "no" = "#fd7e14")) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::labs(x        = "Boosting iterations (mstop)",
                      y        = paste0("RMSE (", .resample_label(mod), ")"),
                      colour   = "prune",
                      title    = "gamboost tuning: mstop vs RMSE",
                      subtitle = "Lines = prune (yes/no)  |  dashed = best mstop") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(legend.position = "right",
                       plot.title      = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle   = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text       = ggplot2::element_text(size = 13),
                       axis.title      = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── M5 tuning plot ────────────────────────────────────────────────────────
    output[["M5_tune_plot"]] <- renderPlot({
      mod <- models[["M5"]]; req(mod)
      df  <- mod$results

      has_sd <- !all(is.na(df$RMSESD))

      df$combo <- paste0(
        "pruned=", df$pruned, "\n",
        "smoothed=", df$smoothed, "\n",
        "rules=", df$rules
      )

      best_combo <- paste0(
        "pruned=", mod$bestTune$pruned, "\n",
        "smoothed=", mod$bestTune$smoothed, "\n",
        "rules=", mod$bestTune$rules
      )

      p <- ggplot2::ggplot(df, ggplot2::aes(x = combo, y = RMSE, colour = rules))
      if (has_sd)
        p <- p + ggplot2::geom_errorbar(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          width = 0.2
        )
      p +
        ggplot2::geom_point(size = 4) +
        ggplot2::geom_point(data = df[df$combo == best_combo, ],
                            colour = "#dc3545", size = 6, shape = 1, stroke = 1.5) +
        ggplot2::scale_colour_manual(values = c("yes" = "#0d6efd", "no" = "#6c757d")) +
        ggplot2::labs(x        = "Parameter combination",
                      y        = paste0("RMSE (", .resample_label(mod), ")"),
                      colour   = "rules",
                      title    = "M5 tuning: RMSE per pruned × smoothed × rules combination",
                      subtitle = "Red circle = best combination  |  error bars = ± 1 SD") +
        ggplot2::theme_bw(base_size = 12) +
        ggplot2::theme(axis.text.x     = ggplot2::element_text(size = 9),
                       legend.position = "right",
                       plot.title      = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle   = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.title      = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── PPR tuning plot: RMSE vs nterms ──────────────────────────────────────
    output[["ppr_tune_plot"]] <- renderPlot({
      mod    <- models[["ppr"]]; req(mod)
      df     <- mod$results
      has_sd <- !all(is.na(df$RMSESD))
      best_n <- mod$bestTune$nterms

      p <- ggplot2::ggplot(df, ggplot2::aes(x = nterms, y = RMSE))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          fill = "#20c997", alpha = 0.2
        )
      p +
        ggplot2::geom_line(colour = "#20c997", linewidth = 1) +
        ggplot2::geom_point(colour = "#20c997", size = 2.5) +
        ggplot2::geom_vline(xintercept = best_n,
                            linetype = "dashed", colour = "#dc3545", linewidth = 0.8) +
        ggplot2::annotate("text",
                          x     = best_n,
                          y     = max(df$RMSE, na.rm = TRUE),
                          label = paste0("best nterms = ", best_n),
                          hjust = -0.1, vjust = 1,
                          colour = "#dc3545", size = 4, fontface = "bold") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::labs(x        = "Number of terms (nterms)",
                      y        = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "PPR tuning: nterms vs RMSE",
                      subtitle = "Resampled RMSE ± 1 SD  |  dashed line = best nterms") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(plot.title    = ggplot2::element_text(face = "bold", size = 14),
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
               "earth"     = earth_preprocess      %||% general_preprocess %||% character(0),
               "gcvEarth"  = gcvearth_preprocess   %||% general_preprocess %||% character(0),
               "gam"       = gam_preprocess        %||% general_preprocess %||% character(0),
               "gamSpline" = gamspline_preprocess   %||% general_preprocess %||% character(0),
               "gamboost"  = gamboost_preprocess    %||% general_preprocess %||% character(0),
               "M5"        = m5_preprocess         %||% general_preprocess %||% character(0),
               "ppr"       = ppr_preprocess        %||% general_preprocess %||% character(0),
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

        earth = function() {
          library(earth)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          # earth only accepts na.action = na.fail — NAs must be handled in the recipe.
          caret::train(rec, data = train_df, method = "earth",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.fail)
        },

        gcvEarth = function() {
          library(earth)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          # gcvEarth uses the earth package — same na.fail restriction applies.
          caret::train(rec, data = train_df, method = "gcvEarth",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.fail)
        },

        gam = function() {
          library(gam)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          caret::train(rec, data = train_df, method = "gam",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.pass)
        },

        gamSpline = function() {
          library(gam)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          caret::train(rec, data = train_df, method = "gamSpline",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.pass)
        },

        gamboost = function() {
          library(mboost)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          # NOTE: na.omit required — mboost cannot handle NA rows internally.
          # baselearner = "bols": bbs() B-spline base learners fail with singular
          # design matrices on this dataset regardless of resampling method — low-
          # cardinality predictors don't provide enough unique values for a spline
          # basis. bols() uses linear base learners; boosting still approximates
          # nonlinear relationships additively across many iterations.
          caret::train(rec, data = train_df, method = "gamboost",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.omit,
                       baselearner = "bols")
        },

        M5 = function() {
          library(RWeka)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          caret::train(rec, data = train_df, method = "M5",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.pass)
        },

        ppr = function() {
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          caret::train(rec, data = train_df, method = "ppr",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.pass)
        }

      )
    )

    # ── Return ────────────────────────────────────────────────────────────────
    return(list(models = models, effective_seed = effective_seed))
  })
}
