# =================================================================================
# mod_meth_tree.R  — Tree Based category  (Rpart + Cubist + EVTree + RF)
# =================================================================================
# UI:     meth_tree_ui(id, pp_choices, default_preprocess, model_seed)
# Server: meth_tree_server(id, get_data, roles, seed, model_seed,
#                           general_preprocess, rpart_preprocess,
#                           cubist_preprocess, evtree_preprocess,
#                           rf_preprocess, pp_choices)
# Returns: list(models = reactiveValues, effective_seed = reactive)
# =================================================================================


# ── UI ───────────────────────────────────────────────────────────────────────

meth_tree_ui <- function(id,
                         pp_choices         = character(0),
                         default_preprocess = character(0),
                         model_seed         = NULL) {
  ns <- NS(id)

  fluidRow(
    column(9,
      tabsetPanel(type = "tabs", id = ns("method_inner"),
        tabPanel("rpart",  value = "rpart",  style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "rpart",  has_tuning = TRUE)),
        tabPanel("cubist", value = "cubist", style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "cubist", has_tuning = TRUE))
        # tabPanel("evtree", value = "evtree", style = "padding-top:12px;",
        #          .meth_subtabs_ui(ns, "evtree", has_tuning = TRUE)),
        # tabPanel("rf",     value = "rf",     style = "padding-top:12px;",
        #          .meth_subtabs_ui(ns, "rf",     has_tuning = TRUE))
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

meth_tree_server <- function(id, get_data, roles,
                              seed               = reactive(2026),
                              model_seed         = NULL,
                              general_preprocess = NULL,
                              rpart_preprocess   = NULL,
                              cubist_preprocess  = NULL,
                              evtree_preprocess  = NULL,
                              rf_preprocess      = NULL,
                              pp_choices         = character(0)) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    models <- reactiveValues()

    # ── Common setup ──────────────────────────────────────────────────────────
    setup          <- .meth_common_server_setup(input, output, session, get_data, roles, seed,
                                                model_seed = model_seed)
    effective_seed <- setup$effective_seed
    get_train      <- setup$get_train

    current_method <- reactive({ input$method_inner %||% "rpart" })

    # ── Standard output renders ───────────────────────────────────────────────
    .meth_register_outputs(output, "rpart",  models, ns)
    .meth_register_outputs(output, "cubist", models, ns)
    .meth_register_outputs(output, "evtree", models, ns)
    .meth_register_outputs(output, "rf",     models, ns)

    # ── Rpart tuning plot: RMSE vs log10(cp) ─────────────────────────────────
    output[["rpart_tune_plot"]] <- renderPlot({
      mod    <- models[["rpart"]]; req(mod)
      df     <- mod$results
      has_sd <- !all(is.na(df$RMSESD))
      best_cp <- mod$bestTune$cp

      p <- ggplot2::ggplot(df, ggplot2::aes(x = log10(cp), y = RMSE))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          fill = "#28a745", alpha = 0.2)
      p +
        ggplot2::geom_line(colour = "#28a745", linewidth = 1) +
        ggplot2::geom_point(colour = "#28a745", size = 2.5) +
        ggplot2::geom_vline(xintercept = log10(best_cp),
                            linetype = "dashed", colour = "#dc3545", linewidth = 0.8) +
        ggplot2::annotate("text", x = log10(best_cp), y = max(df$RMSE, na.rm = TRUE),
                          label = paste0("best cp = ", signif(best_cp, 3)),
                          hjust = -0.1, vjust = 1, colour = "#dc3545", size = 4, fontface = "bold") +
        ggplot2::labs(x = expression(log[10](cp)), y = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "Rpart tuning: complexity parameter vs RMSE",
                      subtitle = "Resampled RMSE ± 1 SD  |  dashed line = best cp") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(plot.title    = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text     = ggplot2::element_text(size = 13),
                       axis.title    = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── Cubist tuning plot: facet by committees, x = neighbors ───────────────
    output[["cubist_tune_plot"]] <- renderPlot({
      mod <- models[["cubist"]]; req(mod)
      df  <- mod$results

      comm_vals <- sort(unique(df$committees))
      n_facets  <- min(9, length(comm_vals))
      idx       <- round(seq(1, length(comm_vals), length.out = n_facets))
      comm_sub  <- comm_vals[idx]

      df_sub <- df[df$committees %in% comm_sub, ]
      df_sub$comm_label <- factor(
        paste0("committees = ", df_sub$committees),
        levels = paste0("committees = ", comm_sub)
      )

      has_sd     <- !all(is.na(df_sub$RMSESD))
      best_comm  <- mod$bestTune$committees
      best_neigh <- mod$bestTune$neighbors

      p <- ggplot2::ggplot(df_sub, ggplot2::aes(x = neighbors, y = RMSE))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          fill = "#e83e8c", alpha = 0.2)
      p +
        ggplot2::geom_line(colour = "#e83e8c", linewidth = 0.9) +
        ggplot2::geom_point(colour = "#e83e8c", size = 2.5) +
        ggplot2::geom_vline(
          data = data.frame(comm_label = paste0("committees = ", best_comm),
                            xint       = best_neigh),
          ggplot2::aes(xintercept = xint),
          linetype = "dashed", colour = "#dc3545", linewidth = 0.7,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::facet_wrap(~ comm_label, scales = "free_y") +
        ggplot2::labs(x = "Neighbors (k for instance correction)",
                      y = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "Cubist tuning: neighbors vs RMSE, faceted by committees",
                      subtitle = "Each panel: 'under this ensemble size, how many neighbors?'  |  dashed = best") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(strip.text    = ggplot2::element_text(face = "bold", size = 11),
                       plot.title    = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text     = ggplot2::element_text(size = 13),
                       axis.title    = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── EVTree tuning plot: RMSE vs alpha ────────────────────────────────────
    output[["evtree_tune_plot"]] <- renderPlot({
      mod    <- models[["evtree"]]; req(mod)
      df     <- mod$results
      has_sd <- !all(is.na(df$RMSESD))
      best_alpha <- mod$bestTune$alpha

      p <- ggplot2::ggplot(df, ggplot2::aes(x = alpha, y = RMSE))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          fill = "#fd7e14", alpha = 0.2)
      p +
        ggplot2::geom_line(colour = "#fd7e14", linewidth = 1) +
        ggplot2::geom_point(colour = "#fd7e14", size = 2.5) +
        ggplot2::geom_vline(xintercept = best_alpha,
                            linetype = "dashed", colour = "#dc3545", linewidth = 0.8) +
        ggplot2::annotate("text", x = best_alpha, y = max(df$RMSE, na.rm = TRUE),
                          label = paste0("best α = ", signif(best_alpha, 3)),
                          hjust = -0.1, vjust = 1, colour = "#dc3545", size = 4, fontface = "bold") +
        ggplot2::labs(x = "Complexity parameter (α)", y = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "EVTree tuning: complexity parameter vs RMSE",
                      subtitle = "Higher α penalises tree size more  |  dashed line = best α") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(plot.title    = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text     = ggplot2::element_text(size = 13),
                       axis.title    = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── RF tuning plot: RMSE vs mtry ─────────────────────────────────────────
    output[["rf_tune_plot"]] <- renderPlot({
      mod    <- models[["rf"]]; req(mod)
      df     <- mod$results
      has_sd <- !all(is.na(df$RMSESD))
      best_mtry <- mod$bestTune$mtry

      p <- ggplot2::ggplot(df, ggplot2::aes(x = mtry, y = RMSE))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          fill = "#20c997", alpha = 0.2)
      p +
        ggplot2::geom_line(colour = "#20c997", linewidth = 1) +
        ggplot2::geom_point(colour = "#20c997", size = 2.5) +
        ggplot2::geom_vline(xintercept = best_mtry,
                            linetype = "dashed", colour = "#dc3545", linewidth = 0.8) +
        ggplot2::annotate("text", x = best_mtry, y = max(df$RMSE, na.rm = TRUE),
                          label = paste0("best mtry = ", best_mtry),
                          hjust = -0.1, vjust = 1, colour = "#dc3545", size = 4, fontface = "bold") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::labs(x = "Variables per split (mtry)", y = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "Random Forest tuning: mtry vs RMSE",
                      subtitle = "Resampled RMSE ± 1 SD  |  dashed line = best mtry") +
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
               "rpart"  = rpart_preprocess  %||% general_preprocess %||% character(0),
               "cubist" = cubist_preprocess  %||% general_preprocess %||% character(0),
               "evtree" = evtree_preprocess  %||% general_preprocess %||% character(0),
               "rf"     = rf_preprocess      %||% general_preprocess %||% character(0),
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

        rpart = function() {
          library(rpart)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          caret::train(rec, data = train_df, method = "rpart",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.rpart)
        },

        cubist = function() {
          library(Cubist)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          caret::train(rec, data = train_df, method = "cubist",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.pass)
        },

        evtree = function() {
          library(evtree)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          caret::train(rec, data = train_df, method = "evtree",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.pass)
        },

        rf = function() {
          library(randomForest)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          caret::train(rec, data = train_df, method = "rf",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.roughfix)
        }

      )
    )

    # ── Return ────────────────────────────────────────────────────────────────
    return(list(models = models, effective_seed = effective_seed))
  })
}
