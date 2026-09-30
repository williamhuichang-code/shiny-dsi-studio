# =================================================================================
# mod_meth_null.R  — Null (baseline) category module
# =================================================================================
# Single method: null.  No hyperparameter tuning; recipe is used only to define
# the formula, not to apply preprocessing steps.
#
# UI:     meth_null_ui(id, pp_choices, default_preprocess)
# Server: meth_null_server(id, get_data, roles, seed, general_preprocess, pp_choices)
# Returns: list(models = reactiveValues, effective_seed = reactive)
# =================================================================================


# ── UI ───────────────────────────────────────────────────────────────────────

meth_null_ui <- function(id,
                         pp_choices         = character(0),
                         default_preprocess = character(0),
                         model_seed         = 2026) {
  ns <- NS(id)

  fluidRow(
    column(9,
      tabsetPanel(type = "tabs", id = ns("method_inner"),
        tabPanel("null", value = "null", style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "null", has_tuning = TRUE))
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

meth_null_server <- function(id, get_data, roles,
                              seed               = reactive(2026),
                              model_seed         = NULL,
                              general_preprocess = NULL,
                              pp_choices         = character(0)) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    models <- reactiveValues()

    # ── Common setup: effective_seed, global_seed_display, get_train ──────────
    setup          <- .meth_common_server_setup(input, output, session, get_data, roles, seed,
                                                model_seed = model_seed)
    effective_seed <- setup$effective_seed
    get_train      <- setup$get_train

    # ── Standard output renders for the null method ───────────────────────────
    .meth_register_outputs(output, "null", models, ns)

    # ── Preprocessing selector — reset on config mode change ─────────────────
    # (null model ignores preprocessing, but we still let the user configure
    #  it so the recipe object is properly recorded)
    observe({
      selected <- general_preprocess %||% character(0)
      updateSelectizeInput(session, "preprocess",
                           choices  = pp_choices,
                           selected = selected)
    }) |> bindEvent(input$config_mode)

    # ── Train / Load / Delete ─────────────────────────────────────────────────
    .meth_action_dispatcher(
      input, output, session,
      models         = models,
      current_method = reactive("null"),
      roles          = roles,
      train_fns = list(

        null = function() {
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()

          # Use the full train_df (including any NA-outcome rows) so that
          # createResample receives the same y vector as the template's
          # getTrControl(), which also uses getTrainData() without filtering.
          # Some OOB positions may have NA Response → caret emits a
          # "missing values in resampled performance" warning and drops those
          # resamples from the mean, exactly as the template behaves.
          outcome_col <- names(r)[r == "outcome"][1]
          tr_ctrl <- .meth_build_tr_control(
            input, eseed, train_df[[outcome_col]]
          )
          # Null model: recipe defines the formula only, no preprocessing applied
          rec <- .meth_build_recipe(train_df, input$preprocess,
                                    .meth_get_cfg(input), r, is_null = TRUE)
          set.seed(eseed)
          caret::train(rec, data = train_df, method = "null",
                       metric = "RMSE", trControl = tr_ctrl)
        }

      )
    )

    # ── Return ────────────────────────────────────────────────────────────────
    return(list(models = models, effective_seed = effective_seed))
  })
}
