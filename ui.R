# =================================================================================
# ui.R
# =================================================================================

ui <- dashboardPage(
  
  preloader = list(
    html  = tagList(
      waiter::spin_flower(),
      h4("Loading My DSI Studio...", style = "color:#D6DEE6; margin-top:10px;")
    ),
    color = "#49545C"
  ),
  
  # ── HEADER ─────────────────────────────────────────────────────────────────
  
  header = dashboardHeader(
    title = "DSI Studio",
    selectInput(
      inputId  = "selected_file",
      label    = NULL,
      choices  = file_choices_with_none,
      selected = default_selected,
      width    = "200px"
    ),
    tags$div(
      style = "margin: 0 0 0 10px; padding-top: 16px;",
      fileInput(
        inputId     = "upload_file",
        label       = NULL,
        accept      = c(".csv", "text/csv"),
        buttonLabel = "Upload CSV",
        placeholder = "or use your own data",
        width       = "260px"
      )
    ),
    tags$li(
      class = "dropdown",
      tags$div(
        style = "padding: 0 30px; font-size: 20px; font-weight: 700; color: #212529; line-height: 50px;",
        "Reusable data analysis & modelling app (R Shiny) \u2003 | \u2003 William Hui Chang"
      )
    )
  ),
  
  
  # ── SIDEBAR NAV ────────────────────────────────────────────────────────────
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      
      menuItem("Config", tabName = "config", icon = icon("sliders"),
               menuSubItem("Data Roles",     tabName = "data_roles"),
               menuSubItem("Download Data",  tabName = "data_download", icon = icon("download"))
               # more future subtabs here
      ),
      
      menuItem("EDA", tabName = "eda", icon = icon("chart-bar"),
               menuSubItem("Data Table",   tabName = "eda_datatable"),
               menuSubItem("Data Summary", tabName = "eda_summary"),
               menuSubItem("Word Cloud",   tabName = "eda_cloud"),
               menuSubItem("Vis Miss",     tabName = "eda_vis_miss"),
               menuSubItem("Miss Upset",   tabName = "eda_upset"),
               menuSubItem("Rising Value", tabName = "eda_rising"),
               menuSubItem("Mosaic",       tabName = "eda_mosaic"),
               menuSubItem("Tabplot",      tabName = "eda_tabplot"),
               menuSubItem("Heatmap",      tabName = "eda_heatmap"),
               menuSubItem("GGPairs",      tabName = "eda_ggpairs"),
               menuSubItem("Bar Chart",    tabName = "eda_bar"),
               menuSubItem("Box Plot",     tabName = "eda_boxplot"),
               menuSubItem("Interaction", tabName = "eda_interaction")
               # more future subtabs here
      ),
      
      menuItem("Miss Strategy", tabName = "miss", icon = icon("circle-question"),
               menuSubItem("Variants",        tabName = "miss_variants"),
               menuSubItem("Shadow Vars",     tabName = "miss_shadow"),
               menuSubItem("Not Applicable",  tabName = "miss_napp"),
               menuSubItem("Excessive Miss",  tabName = "miss_excessive"),
               menuSubItem("Diag: Imputation", tabName = "miss_impute"),
               menuSubItem("Diag: Transform",  tabName = "miss_transform"),
               menuSubItem("Diag: Rpart",      tabName = "miss_rpart"),
               menuSubItem("Diag: Importance", tabName = "miss_importance")
               # more future subtabs here
      ),
      
      menuItem("Out Strategy", tabName = "out", icon = icon("triangle-exclamation"),
               menuSubItem("Histogram",        tabName = "out_hist"),
               menuSubItem("Boxplot",          tabName = "out_boxplot"),
               menuSubItem("Bagplot",          tabName = "out_bagplot"),
               menuSubItem("Mahalanobis",      tabName = "out_mah"),
               menuSubItem("Cook's Distance",  tabName = "out_cooks"),
               menuSubItem("LOF",              tabName = "out_lof"),
               menuSubItem("SVM",              tabName = "out_svm"),
               menuSubItem("Random Forest",    tabName = "out_rf"),
               menuSubItem("Isolation Forest", tabName = "out_iforest"),
               menuSubItem("Summary",          tabName = "out_summary"),
               menuSubItem("Response",         tabName = "out_response")
               # more future subtabs here
      ),
      
      menuItem("Available Methods", icon = icon("list-check"),
               menuSubItem("Method Table", tabName = "meth_av_table", icon = icon("table")),
               menuSubItem("Method Map",   tabName = "meth_av_map",   icon = icon("map"))
      ),
      
      menuItem("Methods", icon = icon("flask"),
               menuSubItem("Null",            tabName = "meth_null",     icon = icon("minus")),
               menuSubItem("OLS",             tabName = "meth_ols",      icon = icon("chart-line")),
               menuSubItem("Tree Based",      tabName = "meth_tree",     icon = icon("tree")),
               menuSubItem("Kernel Methods",  tabName = "meth_kernel",   icon = icon("circle-nodes")),
               menuSubItem("Ensembles",       tabName = "meth_ensemble", icon = icon("layer-group")),
               menuSubItem("Neural Networks", tabName = "meth_nn",       icon = icon("brain")),
               menuSubItem("WildCards",       tabName = "meth_wildcard", icon = icon("wand-magic-sparkles"))
      ),
      
      menuItem("Model Selection", tabName = "meth_select", icon = icon("trophy")),
      menuItem("Performance",    tabName = "meth_perf",   icon = icon("gauge-high"))
      
    )
  ),
  
  
  # ── BODY ───────────────────────────────────────────────────────────────────
  
  body = dashboardBody(
    tabItems(
      
      # Config
      tabItem(tabName = "data_roles",       data_roles_ui("data_roles",
                                                          default_seed = if (exists("SPLIT_SEED"))  SPLIT_SEED  else NULL,
                                                          split_ratio  = if (exists("SPLIT_RATIO")) SPLIT_RATIO else NULL)),
      tabItem(tabName = "data_download",    data_download_ui("data_download")),
      
      # EDA
      tabItem(tabName = "eda_datatable", eda_datatable_ui("eda_datatable")),
      tabItem(tabName = "eda_summary",   eda_summary_ui("eda_summary")),
      tabItem(tabName = "eda_cloud",     eda_cloud_ui("eda_cloud")),
      tabItem(tabName = "eda_vis_miss",  eda_vis_ui("eda_vis")),
      tabItem(tabName = "eda_upset",     eda_upset_ui("eda_upset")),
      tabItem(tabName = "eda_rising",    eda_rising_ui("eda_rising")),
      tabItem(tabName = "eda_mosaic",    eda_mosaic_ui("eda_mosaic")),
      tabItem(tabName = "eda_tabplot",   eda_tabplot_ui("eda_tabplot")),
      tabItem(tabName = "eda_heatmap",   eda_heatmap_ui("eda_heatmap")),
      tabItem(tabName = "eda_ggpairs",   eda_ggpairs_ui("eda_ggpairs")),
      tabItem(tabName = "eda_bar",       eda_bar_ui("eda_bar")),
      tabItem(tabName = "eda_boxplot",   eda_boxplot_ui("eda_boxplot")),
      tabItem(tabName = "eda_interaction", eda_interaction_ui("eda_interaction")),
      
      # Miss Strategy
      tabItem(tabName = "miss_variants",   miss_variants_ui("miss_variants")),
      tabItem(tabName = "miss_shadow",     miss_shadow_ui("miss_shadow")),
      tabItem(tabName = "miss_napp",       miss_napp_ui("miss_napp")),
      tabItem(tabName = "miss_excessive",  miss_excessive_ui("miss_excessive")),
      tabItem(tabName = "miss_rpart",      miss_rpart_ui("miss_rpart")),
      tabItem(tabName = "miss_impute",     miss_impute_ui("miss_impute")),
      tabItem(tabName = "miss_transform",  miss_transform_ui("miss_transform")),
      tabItem(tabName = "miss_importance", miss_importance_ui("miss_importance")),
      
      # Out Strategy
      tabItem(tabName = "out_hist",    out_histogram_ui("out_hist")),
      tabItem(tabName = "out_boxplot", out_boxplot_ui("out_boxplot")),
      tabItem(tabName = "out_bagplot", out_bagplot_ui("out_bagplot")),
      tabItem(tabName = "out_mah",     out_mahalanobis_ui("out_mah")),
      tabItem(tabName = "out_cooks",   out_cooks_ui("out_cooks")),
      tabItem(tabName = "out_lof",     out_lof_ui("out_lof")),
      tabItem(tabName = "out_svm",     out_svm_ui("out_svm")),
      tabItem(tabName = "out_rf",      out_rf_ui("out_rf")),
      tabItem(tabName = "out_iforest", out_iforest_ui("out_iforest")),
      tabItem(tabName = "out_summary", out_summary_ui("out_summary")),
      tabItem(tabName = "out_response", out_response_ui("out_response")),
      
      # Available Methods explorer — two sidebar sub-items, one shared server
      tabItem(tabName = "meth_av_table",
              meth_available_table_ui("meth_available")),
      tabItem(tabName = "meth_av_map",
              meth_available_map_ui("meth_available")),
      
      # Methods — one tabItem per category
      tabItem(tabName = "meth_null",
              meth_null_ui("meth_null",
                           pp_choices         = ppchoices,
                           default_preprocess = if (exists("general_initial")) general_initial else character(0),
                           model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL)),
      
      tabItem(tabName = "meth_ols",
              meth_ols_ui("meth_ols",
                          pp_choices         = ppchoices,
                          default_preprocess = if (exists("general_initial")) general_initial else character(0),
                          model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL)),
      
      tabItem(tabName = "meth_tree",
              meth_tree_ui("meth_tree",
                           pp_choices         = ppchoices,
                           default_preprocess = if (exists("general_initial")) general_initial else character(0),
                           model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL)),
      
      tabItem(tabName = "meth_nn",
              meth_nn_ui("meth_nn",
                         pp_choices         = ppchoices,
                         default_preprocess = if (exists("general_initial")) general_initial else character(0),
                         model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL)),
      tabItem(tabName = "meth_kernel",
              meth_kernel_ui("meth_kernel",
                             pp_choices         = ppchoices,
                             default_preprocess = if (exists("general_initial")) general_initial else character(0),
                             model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL)),
      tabItem(tabName = "meth_ensemble",
              meth_ensemble_ui("meth_ensemble",
                               pp_choices         = ppchoices,
                               default_preprocess = if (exists("general_initial")) general_initial else character(0),
                               model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL)),
      tabItem(tabName = "meth_wildcard",
              meth_wildcard_ui("meth_wildcard",
                               pp_choices         = ppchoices,
                               default_preprocess = if (exists("general_initial")) general_initial else character(0),
                               model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL)),
      
      tabItem(tabName = "meth_select",
              meth_select_ui("meth_select")),
      
      tabItem(tabName = "meth_perf",
              meth_perf_ui("meth_perf"))
      
    )
  ),
  
  controlbar = NULL,
  footer     = NULL
)





