# =================================================================================
# mod_meth_kernel.R  — Kernel Methods category
#   svmLinear / svmPoly / svmRadial / svmRadialSigma /
#   gaussprLinear / gaussprPoly / gaussprRadial /
#   krlsPoly / krlsRadial
# =================================================================================
#
# UI:     meth_kernel_ui(id, pp_choices, default_preprocess, model_seed)
# Server: meth_kernel_server(id, get_data, roles, seed, model_seed,
#                             general_preprocess,
#                             svmlinear_preprocess / svm_preprocess /
#                             svmradial_preprocess / svmpoly_preprocess /
#                             krlspoly_preprocess / krlsradial_preprocess /
#                             gp_preprocess / gaussprpoly_preprocess /
#                             gaussprlinear_preprocess, pp_choices)
# Returns: list(models = reactiveValues, effective_seed = reactive)
# =================================================================================


# ── UI ───────────────────────────────────────────────────────────────────────

meth_kernel_ui <- function(id,
                            pp_choices         = character(0),
                            default_preprocess = character(0),
                            model_seed         = NULL) {
  ns <- NS(id)

  fluidRow(
    column(9,
      tabsetPanel(type = "tabs", id = ns("method_inner"),
        tabPanel("svmLinear",      value = "svmLinear",      style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "svmLinear",      has_tuning = TRUE)),
        tabPanel("svmPoly",        value = "svmPoly",        style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "svmPoly",        has_tuning = TRUE)),
        tabPanel("svmRadial",      value = "svmRadial",      style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "svmRadial",      has_tuning = TRUE)),
        tabPanel("svmRadialSigma", value = "svmRadialSigma", style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "svmRadialSigma", has_tuning = TRUE)),
        tabPanel("gaussprLinear",  value = "gaussprLinear",  style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "gaussprLinear",  has_tuning = FALSE)),
        tabPanel("gaussprPoly",    value = "gaussprPoly",    style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "gaussprPoly",    has_tuning = TRUE)),
        tabPanel("gaussprRadial",  value = "gaussprRadial",  style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "gaussprRadial",  has_tuning = TRUE)),
        tabPanel("krlsPoly",       value = "krlsPoly",       style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "krlsPoly",       has_tuning = TRUE)),
        tabPanel("krlsRadial",     value = "krlsRadial",     style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "krlsRadial",     has_tuning = TRUE))
      )
    ),
    column(3,
      .meth_sidebar_ui(ns,
                       model_seed         = model_seed,
                       pp_choices         = pp_choices,
                       default_preprocess = default_preprocess,
                       specific_panels    = tagList(

        # ── svmRadialSigma-specific controls ────────────────────────────────
        conditionalPanel(
          condition = sprintf("input['%s'] === 'svmRadialSigma'", ns("method_inner")),

          tags$label("Tuning grid:", style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
          selectInput(ns("svm_grid_type"), NULL,
                      choices  = c("Tune length default" = "tunelength",
                                   "Custom C/sigma grid" = "custom"),
                      selected = "custom", width = "100%"),

          conditionalPanel(
            condition = sprintf("input['%s'] === 'custom'", ns("svm_grid_type")),

            tags$label("Log10 C minimum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("svm_log_c_min"),   NULL, min = -3, max = 2,  value = -1, step = 0.1, width = "100%"),
            tags$label("Log10 C maximum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("svm_log_c_max"),   NULL, min = -1, max = 5,  value =  1, step = 0.1, width = "100%"),
            tags$label("C values:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("svm_c_n"),         NULL, min = 1,  max = 20, value =  5, step = 1,   width = "100%"),

            tags$label("Log10 sigma minimum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("svm_log_sig_min"), NULL, min = -5, max = 0,  value = -3, step = 0.1, width = "100%"),
            tags$label("Log10 sigma maximum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("svm_log_sig_max"), NULL, min = -3, max = 3,  value = -1, step = 0.1, width = "100%"),
            tags$label("Sigma values:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("svm_sig_n"),       NULL, min = 1,  max = 20, value =  5, step = 1,   width = "100%")
          )
        ),

        # ── svmPoly-specific controls ────────────────────────────────────────
        conditionalPanel(
          condition = sprintf("input['%s'] === 'svmPoly'", ns("method_inner")),

          tags$label("Tuning grid:", style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
          selectInput(ns("svmpoly_grid_type"), NULL,
                      choices  = c("Tune length default"       = "tunelength",
                                   "Custom degree/scale/C grid" = "custom"),
                      selected = "custom", width = "100%"),

          conditionalPanel(
            condition = sprintf("input['%s'] === 'custom'", ns("svmpoly_grid_type")),

            tags$label("Degree minimum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("svmpoly_deg_min"),       NULL, min = 1,  max = 3,  value = 1,  step = 1,   width = "100%"),
            tags$label("Degree maximum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("svmpoly_deg_max"),       NULL, min = 1,  max = 6,  value = 3,  step = 1,   width = "100%"),

            tags$label("Log10 scale minimum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("svmpoly_log_scale_min"), NULL, min = -3, max = 0,  value = -1, step = 0.5, width = "100%"),
            tags$label("Log10 scale maximum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("svmpoly_log_scale_max"), NULL, min = -1.5, max = 2,  value =  2, step = 0.5, width = "100%"),
            tags$label("Scale values:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("svmpoly_scale_n"),       NULL, min = 2,  max = 10, value =  3, step = 1,   width = "100%"),

            tags$label("Log10 C minimum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("svmpoly_log_c_min"),     NULL, min = -3, max = 2,  value = -1, step = 0.5, width = "100%"),
            tags$label("Log10 C maximum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("svmpoly_log_c_max"),     NULL, min = -1, max = 5,  value =  1, step = 0.5, width = "100%"),
            tags$label("C values:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("svmpoly_c_n"),           NULL, min = 2,  max = 20, value =  3, step = 1,   width = "100%")
          )
        ),

        # ── gaussprPoly-specific controls ────────────────────────────────────
        conditionalPanel(
          condition = sprintf("input['%s'] === 'gaussprPoly'", ns("method_inner")),

          tags$label("Tuning grid:", style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
          selectInput(ns("gaussprpoly_grid_type"), NULL,
                      choices  = c("Tune length default"      = "tunelength",
                                   "Custom degree/scale grid" = "custom"),
                      selected = "custom", width = "100%"),

          conditionalPanel(
            condition = sprintf("input['%s'] === 'custom'", ns("gaussprpoly_grid_type")),

            tags$label("Degree minimum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("gaussprpoly_deg_min"),       NULL, min = 1,  max = 4,  value = 1,  step = 1,   width = "100%"),
            tags$label("Degree maximum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("gaussprpoly_deg_max"),       NULL, min = 1,  max = 8,  value = 5,  step = 1,   width = "100%"),

            tags$label("Log10 scale minimum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("gaussprpoly_log_scale_min"), NULL, min = -3, max = 0,  value = -2, step = 0.5, width = "100%"),
            tags$label("Log10 scale maximum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("gaussprpoly_log_scale_max"), NULL, min = -1, max = 2,  value =  0, step = 0.5, width = "100%"),
            tags$label("Scale values:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("gaussprpoly_scale_n"),       NULL, min = 2,  max = 10, value =  3, step = 1,   width = "100%")
          )
        ),

        # ── krlsPoly-specific controls ────────────────────────────────────────
        conditionalPanel(
          condition = sprintf("input['%s'] === 'krlsPoly'", ns("method_inner")),

          tags$label("Tuning grid:", style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
          selectInput(ns("krlspoly_grid_type"), NULL,
                      choices  = c("Tune length default"       = "tunelength",
                                   "Custom degree/lambda grid" = "custom"),
                      selected = "custom", width = "100%"),

          conditionalPanel(
            condition = sprintf("input['%s'] === 'custom'", ns("krlspoly_grid_type")),

            tags$label("Degree minimum (≥ 2 recommended):",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("krlspoly_deg_min"),     NULL, min = 1,  max = 5,  value = 2,  step = 1,   width = "100%"),
            tags$label("Degree maximum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("krlspoly_deg_max"),     NULL, min = 2,  max = 8,  value = 4,  step = 1,   width = "100%"),

            tags$label("Log10 lambda minimum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("krlspoly_log_lam_min"), NULL, min = -5, max = 0,  value = -3, step = 0.5, width = "100%"),
            tags$label("Log10 lambda maximum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("krlspoly_log_lam_max"), NULL, min = -2, max = 4,  value =  1, step = 0.5, width = "100%"),
            tags$label("Lambda values:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("krlspoly_lam_n"),       NULL, min = 3,  max = 20, value =  7, step = 1,   width = "100%")
          )
        )

      ))
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

meth_kernel_server <- function(id, get_data, roles,
                                seed                     = reactive(2026),
                                model_seed               = NULL,
                                general_preprocess       = NULL,
                                svmlinear_preprocess     = NULL,
                                svmpoly_preprocess       = NULL,
                                svmradial_preprocess     = NULL,
                                svm_preprocess           = NULL,
                                krlspoly_preprocess      = NULL,
                                krlsradial_preprocess    = NULL,
                                gp_preprocess            = NULL,
                                gaussprpoly_preprocess   = NULL,
                                gaussprlinear_preprocess = NULL,
                                pp_choices               = character(0)) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    models <- reactiveValues()

    # ── Common setup ──────────────────────────────────────────────────────────
    setup          <- .meth_common_server_setup(input, output, session, get_data, roles, seed,
                                                model_seed = model_seed)
    effective_seed <- setup$effective_seed
    get_train      <- setup$get_train

    current_method <- reactive({ input$method_inner %||% "svmLinear" })

    # ── Standard output renders ───────────────────────────────────────────────
    .meth_register_outputs(output, "svmLinear",      models, ns)
    .meth_register_outputs(output, "svmPoly",        models, ns)
    .meth_register_outputs(output, "svmRadial",      models, ns)
    .meth_register_outputs(output, "svmRadialSigma", models, ns)
    .meth_register_outputs(output, "gaussprLinear",  models, ns)
    .meth_register_outputs(output, "gaussprPoly",    models, ns)
    .meth_register_outputs(output, "gaussprRadial",  models, ns)
    .meth_register_outputs(output, "krlsPoly",       models, ns)
    .meth_register_outputs(output, "krlsRadial",     models, ns)

    # ── Helper: simple C-only SVM tuning plot ────────────────────────────────
    .svm_c_tune_plot <- function(mod, title) {
      df     <- mod$results
      has_sd <- !all(is.na(df$RMSESD))
      best_C <- mod$bestTune$C

      p <- ggplot2::ggplot(df, ggplot2::aes(x = log10(C), y = RMSE))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          fill = "#0d6efd", alpha = 0.2)
      p +
        ggplot2::geom_line(colour = "#0d6efd", linewidth = 1) +
        ggplot2::geom_point(colour = "#0d6efd", size = 2.5) +
        ggplot2::geom_vline(xintercept = log10(best_C),
                            linetype = "dashed", colour = "#dc3545", linewidth = 0.8) +
        ggplot2::annotate("text",
                          x = log10(best_C), y = max(df$RMSE, na.rm = TRUE),
                          label = paste0("best C = ", signif(best_C, 3)),
                          hjust = -0.1, vjust = 1,
                          colour = "#dc3545", size = 4, fontface = "bold") +
        ggplot2::labs(x = expression(log[10](C)), y = paste0("RMSE (", .resample_label(mod), ")"),
                      title = title,
                      subtitle = "Resampled RMSE ± 1 SD  |  dashed line = best C") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(plot.title    = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text     = ggplot2::element_text(size = 13),
                       axis.title    = ggplot2::element_text(size = 13, face = "bold"))
    }

    # ── svmLinear tuning plot ─────────────────────────────────────────────────
    output[["svmLinear_tune_plot"]] <- renderPlot({
      mod <- models[["svmLinear"]]; req(mod)
      .svm_c_tune_plot(mod, "SVM Linear tuning: C vs RMSE")
    })

    # ── svmRadial tuning plot ─────────────────────────────────────────────────
    output[["svmRadial_tune_plot"]] <- renderPlot({
      mod <- models[["svmRadial"]]; req(mod)
      .svm_c_tune_plot(mod, "SVM Radial tuning: C vs RMSE (σ estimated via sigest)")
    })

    # ── svmRadialSigma tuning plot: facet by σ, x = log10(C) ─────────────────
    output[["svmRadialSigma_tune_plot"]] <- renderPlot({
      mod    <- models[["svmRadialSigma"]]; req(mod)
      df     <- mod$results

      best_C   <- mod$bestTune$C
      best_sig <- mod$bestTune$sigma

      sigma_vals <- sort(unique(df$sigma))
      n_facets   <- min(9, length(sigma_vals))
      idx        <- round(seq(1, length(sigma_vals), length.out = n_facets))
      sig_sub    <- sort(unique(c(sigma_vals[idx], best_sig)))

      df_sub <- df[df$sigma %in% sig_sub, ]
      df_sub$sig_label <- factor(
        paste0("σ = ", signif(df_sub$sigma, 3)),
        levels = paste0("σ = ", signif(sig_sub, 3))
      )

      # Single best row for red circle
      best_row <- df_sub[abs(df_sub$sigma - best_sig) < 1e-9 &
                         abs(df_sub$C     - best_C)   < 1e-9, ]
      best_row$sig_label <- factor(paste0("σ = ", signif(best_row$sigma, 3)),
                                   levels = levels(df_sub$sig_label))

      has_sd <- !all(is.na(df_sub$RMSESD))

      p <- ggplot2::ggplot(df_sub, ggplot2::aes(x = log10(C), y = RMSE,
                                                  colour = log10(C), fill = log10(C)))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          alpha = 0.15, colour = NA)
      p +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::geom_point(data = best_row,
                            colour = "#dc3545", size = 7, shape = 1, stroke = 1.8,
                            inherit.aes = FALSE,
                            ggplot2::aes(x = log10(C), y = RMSE)) +
        ggplot2::scale_colour_viridis_c(name = "log₁₀(C)", option = "viridis") +
        ggplot2::scale_fill_viridis_c(guide = "none", option = "viridis") +
        ggplot2::facet_wrap(~ sig_label, scales = "free_y") +
        ggplot2::labs(x = expression(log[10](C)), y = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "SVM Radial Sigma tuning: RMSE by C, faceted by σ",
                      subtitle = "Each panel: 'under this σ, what C works best?'  |  red circle = best (C, σ)") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(strip.text      = ggplot2::element_text(face = "bold", size = 11),
                       legend.position = "right",
                       plot.title      = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle   = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text       = ggplot2::element_text(size = 13),
                       axis.title      = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── svmPoly tuning plot: facet by degree, x = log10(C), colour = log10(scale)
    output[["svmPoly_tune_plot"]] <- renderPlot({
      mod <- models[["svmPoly"]]; req(mod)
      df  <- mod$results

      has_sd     <- !all(is.na(df$RMSESD))
      best_deg   <- mod$bestTune$degree
      best_C     <- mod$bestTune$C

      df$deg_label <- paste0("degree = ", df$degree)

      p <- ggplot2::ggplot(df,
                           ggplot2::aes(x      = log10(C),
                                        y      = RMSE,
                                        colour = log10(scale + 1e-10),
                                        group  = factor(scale)))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD,
                       fill = log10(scale + 1e-10)),
          alpha = 0.12, colour = NA)
      p +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::geom_vline(
          data = data.frame(deg_label = paste0("degree = ", best_deg),
                            xint      = log10(best_C)),
          ggplot2::aes(xintercept = xint),
          linetype = "dashed", colour = "#dc3545", linewidth = 0.7,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_colour_viridis_c(name = "log₁₀(scale)", option = "plasma") +
        ggplot2::scale_fill_viridis_c(guide = "none",            option = "plasma") +
        ggplot2::facet_wrap(~ deg_label, scales = "fixed") +
        ggplot2::labs(x = expression(log[10](C)), y = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "SVM Poly tuning: RMSE by C, faceted by degree",
                      subtitle = "Colour = scale parameter  |  dashed = best C") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(strip.text      = ggplot2::element_text(face = "bold", size = 11),
                       legend.position = "right",
                       plot.title      = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle   = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text       = ggplot2::element_text(size = 13),
                       axis.title      = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── gaussprRadial tuning plot: RMSE vs σ ─────────────────────────────────
    output[["gaussprRadial_tune_plot"]] <- renderPlot({
      mod    <- models[["gaussprRadial"]]; req(mod)
      df     <- mod$results
      has_sd <- !all(is.na(df$RMSESD))
      best_sigma <- mod$bestTune$sigma

      p <- ggplot2::ggplot(df, ggplot2::aes(x = sigma, y = RMSE))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          fill = "#6610f2", alpha = 0.2)
      p +
        ggplot2::geom_line(colour = "#6610f2", linewidth = 1) +
        ggplot2::geom_point(colour = "#6610f2", size = 2.5) +
        ggplot2::geom_vline(xintercept = best_sigma,
                            linetype = "dashed", colour = "#dc3545", linewidth = 0.8) +
        ggplot2::annotate("text",
                          x = best_sigma, y = max(df$RMSE, na.rm = TRUE),
                          label = paste0("best σ = ", signif(best_sigma, 3)),
                          hjust = -0.1, vjust = 1,
                          colour = "#dc3545", size = 4, fontface = "bold") +
        ggplot2::labs(x = "Length-scale parameter (σ)", y = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "Gaussian Process Radial tuning: σ vs RMSE",
                      subtitle = "Resampled RMSE ± 1 SD  |  dashed line = best σ") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(plot.title    = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text     = ggplot2::element_text(size = 13),
                       axis.title    = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── gaussprPoly tuning plot: facet by degree, x = log10(scale) ──────────
    output[["gaussprPoly_tune_plot"]] <- renderPlot({
      mod    <- models[["gaussprPoly"]]; req(mod)
      df     <- mod$results
      has_sd <- !all(is.na(df$RMSESD))
      best_d <- mod$bestTune$degree
      best_s <- mod$bestTune$scale

      # Single best row (exact degree + scale match)
      best_row <- df[abs(df$degree - best_d) < 1e-9 & abs(df$scale - best_s) < 1e-9, ]

      df$deg_label <- paste0("degree = ", df$degree)
      best_row$deg_label <- paste0("degree = ", best_row$degree)

      p <- ggplot2::ggplot(df, ggplot2::aes(x = log10(scale), y = RMSE))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          fill = "#6610f2", alpha = 0.2)
      p +
        ggplot2::geom_line(colour = "#6610f2", linewidth = 0.9) +
        ggplot2::geom_point(colour = "#6610f2", size = 3) +
        ggplot2::geom_point(data   = best_row,
                            colour = "#dc3545", size = 6, shape = 1, stroke = 1.5) +
        ggplot2::facet_wrap(~ deg_label, scales = "fixed") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::labs(x        = expression(log[10](scale)),
                      y        = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "Gaussian Process Poly tuning: scale vs RMSE, faceted by degree",
                      subtitle = "Red circle = best (degree, scale)  |  shaded = ± 1 SD") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(strip.text    = ggplot2::element_text(face = "bold", size = 11),
                       plot.title    = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text     = ggplot2::element_text(size = 13),
                       axis.title    = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── krlsPoly tuning plot: facet by degree, x = log10(lambda) ─────────────
    output[["krlsPoly_tune_plot"]] <- renderPlot({
      mod <- models[["krlsPoly"]]; req(mod)
      df  <- mod$results

      has_sd   <- !all(is.na(df$RMSESD))
      best_deg <- mod$bestTune$degree
      best_lam <- mod$bestTune$lambda

      df$deg_label <- paste0("degree = ", df$degree)

      p <- ggplot2::ggplot(df, ggplot2::aes(x = log10(lambda + 1e-10), y = RMSE))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          fill = "#0dcaf0", alpha = 0.2)
      p +
        ggplot2::geom_line(colour = "#0dcaf0", linewidth = 0.9) +
        ggplot2::geom_point(colour = "#0dcaf0", size = 2.5) +
        ggplot2::geom_vline(
          data = data.frame(deg_label = paste0("degree = ", best_deg),
                            xint      = log10(best_lam + 1e-10)),
          ggplot2::aes(xintercept = xint),
          linetype = "dashed", colour = "#dc3545", linewidth = 0.7,
          inherit.aes = FALSE
        ) +
        ggplot2::facet_wrap(~ deg_label, scales = "free_y") +
        ggplot2::labs(x = expression(log[10](lambda)), y = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "KRLS Poly tuning: RMSE by lambda, faceted by degree",
                      subtitle = "Resampled RMSE ± 1 SD  |  dashed = best lambda") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(strip.text    = ggplot2::element_text(face = "bold", size = 11),
                       plot.title    = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text     = ggplot2::element_text(size = 13),
                       axis.title    = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── krlsRadial tuning plot: facet by sigma, x = log10(lambda) ────────────
    output[["krlsRadial_tune_plot"]] <- renderPlot({
      mod <- models[["krlsRadial"]]; req(mod)
      df  <- mod$results

      has_sd   <- !all(is.na(df$RMSESD))
      best_sig <- mod$bestTune$sigma
      best_lam <- mod$bestTune$lambda

      sigma_vals <- sort(unique(df$sigma))
      n_facets   <- min(9, length(sigma_vals))
      idx        <- round(seq(1, length(sigma_vals), length.out = n_facets))
      sig_sub    <- sigma_vals[idx]

      df_sub <- df[df$sigma %in% sig_sub, ]
      df_sub$sig_label <- factor(
        paste0("σ = ", signif(df_sub$sigma, 3)),
        levels = paste0("σ = ", signif(sig_sub, 3))
      )

      p <- ggplot2::ggplot(df_sub, ggplot2::aes(x = log10(lambda + 1e-10), y = RMSE))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          fill = "#0dcaf0", alpha = 0.2)
      p +
        ggplot2::geom_line(colour = "#0dcaf0", linewidth = 0.9) +
        ggplot2::geom_point(colour = "#0dcaf0", size = 2.5) +
        ggplot2::geom_vline(
          data = data.frame(sig_label = paste0("σ = ", signif(best_sig, 3)),
                            xint      = log10(best_lam + 1e-10)),
          ggplot2::aes(xintercept = xint),
          linetype = "dashed", colour = "#dc3545", linewidth = 0.7,
          inherit.aes = FALSE
        ) +
        ggplot2::facet_wrap(~ sig_label, scales = "free_y") +
        ggplot2::labs(x = expression(log[10](lambda)), y = paste0("RMSE (", .resample_label(mod), ")"),
                      title    = "KRLS Radial tuning: RMSE by lambda, faceted by sigma",
                      subtitle = "Resampled RMSE ± 1 SD  |  dashed = best lambda") +
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
               "svmLinear"      = svmlinear_preprocess     %||% general_preprocess %||% character(0),
               "svmPoly"        = svmpoly_preprocess        %||% general_preprocess %||% character(0),
               "svmRadial"      = svmradial_preprocess      %||% general_preprocess %||% character(0),
               "svmRadialSigma" = svm_preprocess            %||% general_preprocess %||% character(0),
               "gaussprLinear"  = gaussprlinear_preprocess  %||% general_preprocess %||% character(0),
               "gaussprPoly"    = gaussprpoly_preprocess    %||% general_preprocess %||% character(0),
               "gaussprRadial"  = gp_preprocess             %||% general_preprocess %||% character(0),
               "krlsPoly"       = krlspoly_preprocess       %||% general_preprocess %||% character(0),
               "krlsRadial"     = krlsradial_preprocess     %||% general_preprocess %||% character(0),
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

        svmLinear = function() {
          library(kernlab)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          caret::train(rec, data = train_df, method = "svmLinear",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.pass)
        },

        svmPoly = function() {
          library(kernlab)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)

          build_svmpoly_grid <- function() {
            deg_g   <- seq(input$svmpoly_deg_min, input$svmpoly_deg_max, by = 1L)
            scale_g <- 10^seq(input$svmpoly_log_scale_min, input$svmpoly_log_scale_max,
                              length.out = input$svmpoly_scale_n)
            C_g     <- 10^seq(input$svmpoly_log_c_min, input$svmpoly_log_c_max,
                              length.out = input$svmpoly_c_n)
            expand.grid(degree = deg_g, scale = scale_g, C = C_g)
          }

          set.seed(eseed)
          if (isTRUE(input$config_mode == "specific") &&
              isTRUE(input$svmpoly_grid_type == "custom")) {
            caret::train(rec, data = train_df, method = "svmPoly",
                         metric = "RMSE", trControl = tr_ctrl,
                         tuneGrid = build_svmpoly_grid(), na.action = na.pass)
          } else {
            caret::train(rec, data = train_df, method = "svmPoly",
                         metric = "RMSE", trControl = tr_ctrl,
                         tuneLength = input$tune_length %||% 5, na.action = na.pass)
          }
        },

        svmRadial = function() {
          library(kernlab)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          caret::train(rec, data = train_df, method = "svmRadial",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.pass)
        },

        svmRadialSigma = function() {
          library(kernlab)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)

          build_svm_grid <- function() {
            C_g   <- 10^seq(input$svm_log_c_min,   input$svm_log_c_max,   length.out = input$svm_c_n)
            sig_g <- 10^seq(input$svm_log_sig_min, input$svm_log_sig_max, length.out = input$svm_sig_n)
            expand.grid(sigma = sig_g, C = C_g)
          }

          set.seed(eseed)
          if (isTRUE(input$config_mode == "specific") &&
              isTRUE(input$svm_grid_type == "custom")) {
            caret::train(rec, data = train_df, method = "svmRadialSigma",
                         metric = "RMSE", trControl = tr_ctrl,
                         tuneGrid = build_svm_grid(), na.action = na.pass)
          } else {
            caret::train(rec, data = train_df, method = "svmRadialSigma",
                         metric = "RMSE", trControl = tr_ctrl,
                         tuneLength = input$tune_length %||% 5, na.action = na.pass)
          }
        },

        gaussprLinear = function() {
          library(kernlab)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          # gaussprLinear has no tuning parameters — tuneLength is ignored by caret
          caret::train(rec, data = train_df, method = "gaussprLinear",
                       metric = "RMSE", trControl = tr_ctrl,
                       na.action = na.pass)
        },

        gaussprPoly = function() {
          library(kernlab)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)

          build_gaussprpoly_grid <- function() {
            deg_g   <- seq(input$gaussprpoly_deg_min, input$gaussprpoly_deg_max, by = 1L)
            scale_g <- 10^seq(input$gaussprpoly_log_scale_min, input$gaussprpoly_log_scale_max,
                              length.out = input$gaussprpoly_scale_n)
            expand.grid(degree = deg_g, scale = scale_g)
          }

          set.seed(eseed)
          if (isTRUE(input$config_mode == "specific") &&
              isTRUE(input$gaussprpoly_grid_type == "custom")) {
            caret::train(rec, data = train_df, method = "gaussprPoly",
                         metric = "RMSE", trControl = tr_ctrl,
                         tuneGrid = build_gaussprpoly_grid(), na.action = na.pass)
          } else {
            caret::train(rec, data = train_df, method = "gaussprPoly",
                         metric = "RMSE", trControl = tr_ctrl,
                         tuneLength = input$tune_length %||% 5, na.action = na.pass)
          }
        },

        gaussprRadial = function() {
          library(kernlab)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          caret::train(rec, data = train_df, method = "gaussprRadial",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.pass)
        },

        krlsPoly = function() {
          library(KRLS)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)

          build_krlspoly_grid <- function() {
            deg_g <- seq(input$krlspoly_deg_min, input$krlspoly_deg_max, by = 1L)
            lam_g <- 10^seq(input$krlspoly_log_lam_min, input$krlspoly_log_lam_max,
                            length.out = input$krlspoly_lam_n)
            expand.grid(degree = deg_g, lambda = lam_g)
          }

          set.seed(eseed)
          # NOTE: na.action omitted — KRLS::krls() does not accept na.action argument.
          if (isTRUE(input$config_mode == "specific") &&
              isTRUE(input$krlspoly_grid_type == "custom")) {
            caret::train(rec, data = train_df, method = "krlsPoly",
                         metric = "RMSE", trControl = tr_ctrl,
                         tuneGrid = build_krlspoly_grid())
          } else {
            caret::train(rec, data = train_df, method = "krlsPoly",
                         metric = "RMSE", trControl = tr_ctrl,
                         tuneLength = input$tune_length %||% 5)
          }
        },

        krlsRadial = function() {
          library(KRLS)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          # NOTE: na.action omitted — KRLS::krls() does not accept na.action argument.
          caret::train(rec, data = train_df, method = "krlsRadial",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5)
        }

      )
    )

    # ── Return ────────────────────────────────────────────────────────────────
    return(list(models = models, effective_seed = effective_seed))
  })
}
