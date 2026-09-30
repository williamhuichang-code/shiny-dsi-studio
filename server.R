# =================================================================================
# server.R
# =================================================================================

server <- function(input, output, session) {
  
  if (!IS_HOSTED) {
    if (!dir.exists("./SavedModels")) dir.create("./SavedModels")
    shiny::onSessionEnded(stopApp)   # local use only: closing the browser tab stops R
  }
  
  # ── RAW DATA ─────────────────────────────────────────────────────────────
  
  # an uploaded CSV takes priority; picking a bundled file again switches back
  uploaded <- reactiveVal(NULL)
  observeEvent(input$upload_file, uploaded(input$upload_file))
  observeEvent(input$selected_file, uploaded(NULL), ignoreInit = TRUE)
  
  get_raw <- reactive({
    up <- uploaded()
    if (!is.null(up)) {
      return(read.csv(up$datapath,
                      header = TRUE,
                      na.strings = c('NA', 'N/A'),
                      stringsAsFactors = TRUE))
    }
    req(input$selected_file)
    req(input$selected_file != "(none)")
    read.csv(file.path(DATA_WD, input$selected_file),
             header = TRUE,
             na.strings = c('NA', 'N/A'),
             stringsAsFactors = TRUE)
  })
  
  
  # ── DOMAIN CONFIGS ────────────────────────────────────────────────────────
  
  config         <- data_roles_server("data_roles", get_raw,
                                      default_roles = if (exists("task_specific_roles")) task_specific_roles else NULL,
                                      auto_split    = if (exists("AUTO_SPLIT"))  AUTO_SPLIT  else FALSE,
                                      split_ratio   = if (exists("SPLIT_RATIO")) SPLIT_RATIO else NULL)
  config_data    <- config$data            # reactive df (raw + optional split col)
  roles          <- config$roles           # reactive named list of role assignments
  important_vars <- config$important_vars  # reactive character vector
  seed_in_use    <- config$seed            # reactive integer
  
  
  # ── PIPELINE (MANUAL) ─────────────────────────────────────────────────────
  
  # Note: 
  #   works like OOP in python (e.g., df.variants().shadow().excessive())
  #   but R doesn't have class and class methods
  #   for modularised app design, this is the best choice already
  
  # OOP for missingness and outlier strategies
  variant      <- miss_variants_server("miss_variants",   config_data)
  shadow       <- miss_shadow_server("miss_shadow",       variant$data)
  napp         <- miss_napp_server("miss_napp",           shadow$data)
  excessive    <- miss_excessive_server("miss_excessive", napp$data, roles)
  out_response <- out_response_server("out_response",     excessive$data, get_raw, roles,
                                      default_omit_ids = if (exists("A3_omit_ids")) A3_omit_ids else NULL)
  
  # exploratory diagnostics for imputation and standarisation choices
  impute    <- miss_impute_server("miss_impute",       out_response$data, roles, seed_in_use)
  transform <- miss_transform_server("miss_transform", impute$data,       roles, seed_in_use)
  
  # get data for different purposes (modelling and diagnose respectively)
  get_model_data    <- out_response$data
  get_diagnose_data <- transform$data
  
  # download df at any stage
  data_download_server("data_download", stages = list(
    "Raw"       = get_raw,
    "Processed" = get_diagnose_data
  ))
  
  # EDA visualisations
  eda_datatable_server("eda_datatable",     get_diagnose_data, get_raw)
  eda_summary_server("eda_summary",         get_diagnose_data)
  eda_cloud_server("eda_cloud",             get_diagnose_data)
  eda_vis_server("eda_vis",                 get_diagnose_data, roles)
  eda_upset_server("eda_upset",             get_diagnose_data, roles)
  eda_rising_server("eda_rising",           get_diagnose_data, roles)
  eda_mosaic_server("eda_mosaic",           get_diagnose_data, roles)
  eda_tabplot_server("eda_tabplot",         get_diagnose_data, roles)
  eda_heatmap_server("eda_heatmap",         get_diagnose_data, roles)
  eda_ggpairs_server("eda_ggpairs",         get_diagnose_data, roles)
  eda_bar_server("eda_bar",                 get_diagnose_data)
  eda_boxplot_server("eda_boxplot",        get_diagnose_data)
  eda_interaction_server("eda_interaction", get_diagnose_data, roles)
  
  # diagnostics visualisations
  miss_rpart_server("miss_rpart",                  get_diagnose_data, roles)
  miss_importance_server("miss_importance",        get_diagnose_data, roles)
  out_histogram_server("out_hist",                 get_diagnose_data, roles)
  out_boxplot_server("out_boxplot",                get_diagnose_data, get_raw, roles)
  out_bagplot_server("out_bagplot",                get_diagnose_data, get_raw, roles)
  out_mah     <- out_mahalanobis_server("out_mah", get_diagnose_data, get_raw, roles, seed = seed_in_use)
  out_cooks   <- out_cooks_server("out_cooks",     get_diagnose_data, get_raw, roles, seed = seed_in_use)
  out_lof     <- out_lof_server("out_lof",         get_diagnose_data, get_raw, roles, seed = seed_in_use)
  out_svm     <- out_svm_server("out_svm",         get_diagnose_data, get_raw, roles, seed = seed_in_use)
  out_rf      <- out_rf_server("out_rf",           get_diagnose_data, get_raw, roles, seed = seed_in_use)
  out_iforest <- out_iforest_server("out_iforest", get_diagnose_data, get_raw, roles, seed = seed_in_use)
  out_summary <- out_summary_server("out_summary", get_diagnose_data, get_raw, roles,
                                    out_mah$flagged, out_cooks$flagged,
                                    out_lof$flagged, out_svm$flagged,
                                    out_rf$flagged,  out_iforest$flagged,
                                    seed = seed_in_use)
  
  
  # ── AVAILABLE METHODS ────────────────────────────────────────────────────
  meth_available_server("meth_available",
                        exclude_tags   = if (exists("av_exclude_tags"))   av_exclude_tags   else NULL,
                        highlight_tags = if (exists("av_highlight_tags")) av_highlight_tags else NULL
  )
  
  # ── PIPELINE (AUTO) ───────────────────────────────────────────────────────
  
  # Methods modules — one server instance per active category
  meth_null <- meth_null_server("meth_null", get_model_data, roles,
                                seed               = seed_in_use,
                                model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL,
                                general_preprocess = if (exists("general_initial")) general_initial else NULL,
                                pp_choices         = ppchoices)
  
  meth_ols  <- meth_ols_server("meth_ols",  get_model_data, roles,
                               seed               = seed_in_use,
                               model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL,
                               general_preprocess = if (exists("general_initial")) general_initial else NULL,
                               glmnet_preprocess  = if (exists("glmnet_initial"))  glmnet_initial  else NULL,
                               pls_preprocess     = if (exists("pls_initial"))     pls_initial     else NULL,
                               rlm_preprocess     = if (exists("rlm_initial"))     rlm_initial     else NULL,
                               lm_preprocess      = if (exists("lm_initial"))      lm_initial      else NULL,
                               pp_choices         = ppchoices)
  
  meth_tree <- meth_tree_server("meth_tree", get_model_data, roles,
                                seed               = seed_in_use,
                                model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL,
                                general_preprocess = if (exists("general_initial")) general_initial else NULL,
                                rpart_preprocess   = if (exists("rpart_initial"))   rpart_initial   else NULL,
                                pp_choices         = ppchoices)
  
  meth_kernel <- meth_kernel_server("meth_kernel", get_model_data, roles,
                                    seed                     = seed_in_use,
                                    model_seed               = if (exists("MODEL_SEED"))           MODEL_SEED          else NULL,
                                    general_preprocess       = if (exists("general_initial"))      general_initial     else NULL,
                                    svm_preprocess           = if (exists("svmrs_initial"))        svmrs_initial       else NULL,
                                    svmpoly_preprocess       = if (exists("svmpoly_initial"))      svmpoly_initial     else NULL,
                                    krlspoly_preprocess      = if (exists("krlspoly_initial"))     krlspoly_initial    else NULL,
                                    gp_preprocess            = if (exists("gausspr_initial"))      gausspr_initial     else NULL,
                                    gaussprpoly_preprocess   = if (exists("gaussprpoly_initial"))  gaussprpoly_initial else NULL,
                                    gaussprlinear_preprocess = if (exists("gausspr_initial"))      gausspr_initial     else NULL,
                                    pp_choices               = ppchoices)
  
  meth_ensemble <- meth_ensemble_server("meth_ensemble", get_model_data, roles,
                                        seed               = seed_in_use,
                                        model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL,
                                        general_preprocess  = if (exists("general_initial"))   general_initial  else NULL,
                                        ranger_preprocess   = if (exists("ranger_initial"))    ranger_initial   else NULL,
                                        bagearth_preprocess = if (exists("bagearth_initial"))  bagearth_initial else NULL,
                                        avnnet_preprocess   = if (exists("avnnet_initial"))    avnnet_initial   else NULL,
                                        pp_choices          = ppchoices)
  
  meth_nn <- meth_nn_server("meth_nn", get_model_data, roles,
                            seed               = seed_in_use,
                            model_seed         = if (exists("MODEL_SEED"))        MODEL_SEED       else NULL,
                            general_preprocess = if (exists("general_initial"))   general_initial  else NULL,
                            qrnn_preprocess    = if (exists("qrnn_initial"))      qrnn_initial     else NULL,
                            brnn_preprocess    = if (exists("brnn_initial"))      brnn_initial     else NULL,
                            pcannet_preprocess = if (exists("pcannet_initial"))   pcannet_initial  else NULL,
                            mlpwd_preprocess   = if (exists("mlpwd_initial"))     mlpwd_initial    else NULL,
                            mlpml_preprocess   = if (exists("mlpml_initial"))     mlpml_initial    else NULL,
                            monmlp_preprocess  = if (exists("monmlp_initial"))    monmlp_initial   else NULL,
                            pp_choices         = ppchoices)
  
  meth_wildcard <- meth_wildcard_server("meth_wildcard", get_model_data, roles,
                                        seed               = seed_in_use,
                                        model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL,
                                        general_preprocess = if (exists("general_initial")) general_initial else NULL,
                                        earth_preprocess   = if (exists("earth_initial"))   earth_initial   else NULL,
                                        m5_preprocess      = if (exists("m5_initial"))      m5_initial      else NULL,
                                        ppr_preprocess     = if (exists("ppr_initial"))     ppr_initial     else NULL,
                                        pp_choices         = ppchoices)
  
  # ── Aggregate all trained models for Model Selection ──────────────────────
  # Each method module returns $models (reactiveValues). Merge here in the app
  # file so neither module depends on the other.
  get_all_models <- reactive({
    all <- c(
      reactiveValuesToList(meth_null$models),
      reactiveValuesToList(meth_ols$models),
      reactiveValuesToList(meth_tree$models),
      reactiveValuesToList(meth_kernel$models),
      reactiveValuesToList(meth_ensemble$models),
      reactiveValuesToList(meth_nn$models),
      reactiveValuesToList(meth_wildcard$models)
    )
    Filter(Negate(is.null), all)
  })
  
  meth_select <- meth_select_server("meth_select", get_models = get_all_models)
  
  meth_perf_server("meth_perf",
                   get_data   = get_model_data,
                   roles      = roles,
                   get_models = get_all_models,
                   choice     = meth_select$choice)
  
  
  
}




