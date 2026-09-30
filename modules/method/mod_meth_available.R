# =================================================================================
# mod_meth_available.R  — Available Methods explorer (table + similarity map)
# =================================================================================
# Lets users browse, filter and spatially compare all regression-capable caret
# methods via a filterable DT table and an MDS similarity map.
#
# UI (two separate functions — one per sidebar sub-item / tabItem):
#   meth_available_table_ui(id)  — filter sidebar + DT table
#   meth_available_map_ui(id)    — map controls sidebar + similarity map
#
# Server (single call, shared namespace for both tabs):
#   meth_available_server(id, exclude_tags, highlight_tags)
#
# The two tabItems share the same module id so the server's reactives feed both.
# bs4Dash pre-renders all tabItems in the DOM, so filter inputs (in the table tab)
# are always present and readable by the server even when the map tab is active.
# =================================================================================

library(DT)
library(ggplot2)
library(ggrepel)


# ── Shared CSS (injected by both UIs) ────────────────────────────────────────

.av_css <- tags$style(HTML("
  .shiny-input-container label,
  .radio label, .checkbox label {
    font-weight: 400 !important;
    font-size: 13px;
  }
"))


# ── UI: Method Table tab ─────────────────────────────────────────────────────
# Contains: filter sidebar (right) + DT table (main)
# Inputs: method type, exclude tags, lit highlights.
# Group colour inputs (av_g1–av_g6) live in the map tab — they are always in
# the DOM (bs4Dash pre-renders all tabItems) so the server reads them from there.

meth_available_table_ui <- function(id) {
  ns <- NS(id)

  tagList(
    .av_css,

    sidebarLayout(
      position = "right",

      sidebarPanel(
        width = 3,
        style = "background-color:#f4f6fb; border-left:3px solid #6a9fd8;
                 min-height:100vh; padding:16px 14px;",

        # Info box
        div(
          style = "font-size:13px; color:#343a40; background-color:white;
                   padding:10px; border-left:4px solid #0d6efd; border-radius:6px;
                   margin-bottom:12px;",
          icon("circle-info", style = "color:#0d6efd;"), HTML("&nbsp;"),
          HTML("<strong>How to use:</strong><br>
               Filter methods using the controls below.
               The table updates instantly. Configure colour
               groups and map layout in the
               <strong>Method Map</strong> tab.")
        ),

        # ── 1st: Model Constraints ────────────────────────────────────────────
        div(style = "background:#f8d7da; border-left:3px solid #dc3545;
                     padding:6px 10px; border-radius:4px; margin-bottom:6px;",
            tags$label("1st — Model Constraints",
                        style = "font-weight:700; font-size:13px; color:#842029;")),
        div(style = "font-size:11px; color:#6c757d; margin-bottom:6px;",
            "Determines which model points appear on the map"),

        tags$label("Method type",
                   style = "font-weight:600; font-size:13px; color:#343a40;
                            display:block; margin-bottom:4px; margin-top:10px;"),
        checkboxInput(ns("av_type_reg"),  "Regression methods",     value = TRUE),
        checkboxInput(ns("av_type_cls"),  "Classification methods", value = FALSE),
        div(style = "margin:4px 0 4px 4px; font-size:11px; font-weight:700;
                     color:#6c757d; letter-spacing:1px;",
            "── OR ──"),
        checkboxInput(ns("av_type_both"), "Methods that can handle both", value = FALSE),

        tags$label("Exclude tags (ANY disqualifies)",
                   style = "font-weight:600; font-size:13px; color:#343a40;
                            display:block; margin-bottom:4px; margin-top:10px;"),
        selectizeInput(ns("av_flt_exclude"), label = NULL, choices = NULL,
                       multiple = TRUE,
                       options  = list(placeholder = "e.g. Two Class Only")),
        hr(),

        # ── 2nd: Literature-Informed Highlights ──────────────────────────────
        div(style = "background:#fff3cd; border-left:3px solid #ffc107;
                     padding:6px 10px; border-radius:4px; margin-bottom:6px;",
            tags$label("2nd — Literature-Informed Highlights",
                        style = "font-weight:700; font-size:13px; color:#664d03;")),
        div(style = "font-size:11px; color:#6c757d; margin-bottom:6px;",
            "OR logic — bold highlighting based on domain research"),
        selectizeInput(ns("av_flt_any"), label = NULL, choices = NULL,
                       multiple = TRUE,
                       options  = list(placeholder = "e.g. Regularization")),
        hr(),

        tags$label("Matching methods",
                   style = "font-weight:600; font-size:13px; color:#343a40;
                            display:block; margin-bottom:4px; margin-top:10px;"),
        verbatimTextOutput(ns("av_filter_summary"))
      ),

      mainPanel(
        width = 9,
        h4("Filtered caret methods",
           style = "border-left:3px solid #534AB7; padding-left:8px;
                    font-size:14px; margin-top:4px; margin-bottom:8px;"),
        shinycssloaders::withSpinner(DT::dataTableOutput(ns("av_method_table")))
      )
    )
  )
}


# ── UI: Method Map tab ───────────────────────────────────────────────────────
# Contains: map controls sidebar (right) + similarity map (main)
# Filter inputs live in the table tab; they're always in the DOM (bs4Dash
# pre-renders all tabItems), so the server can read them here too.

meth_available_map_ui <- function(id) {
  ns <- NS(id)

  tagList(
    .av_css,

    sidebarLayout(
      position = "right",

      sidebarPanel(
        width = 3,
        style = "background-color:#f4f6fb; border-left:3px solid #6a9fd8;
                 min-height:100vh; padding:16px 14px;",

        # Info note
        div(
          style = "font-size:13px; color:#343a40; background-color:white;
                   padding:10px; border-left:4px solid #ffc107; border-radius:6px;
                   margin-bottom:12px;",
          icon("circle-info", style = "color:#ffc107;"), HTML("&nbsp;"),
          HTML("<strong>Tip:</strong><br>
               Set model type, exclusions and highlights in the
               <strong>Method Table</strong> tab — they apply here too.
               Colour groups and map layout are configured below.")
        ),

        # ── 3rd: Sample Model Flavours ────────────────────────────────────────
        div(style = "background:#d1ecf1; border-left:3px solid #0dcaf0;
                     padding:6px 10px; border-radius:4px; margin-bottom:6px;",
            tags$label("3rd — Sample Model Flavours",
                        style = "font-weight:700; font-size:13px; color:#055160;")),
        div(style = "font-size:11px; color:#6c757d; margin-bottom:6px;",
            "Colours model points on the map by group"),

        div(style = "border-left:3px solid #534AB7; padding-left:8px;
                     margin-bottom:2px; margin-top:10px;",
            tags$label("Group 1",
                        style = "font-weight:600; font-size:13px; color:#534AB7;")),
        selectizeInput(ns("av_g1"), label = NULL, choices = NULL, multiple = TRUE,
                       options = list(placeholder = "e.g. Neural Network")),

        div(style = "border-left:3px solid #0F6E56; padding-left:8px;
                     margin-bottom:2px; margin-top:10px;",
            tags$label("Group 2",
                        style = "font-weight:600; font-size:13px; color:#0F6E56;")),
        selectizeInput(ns("av_g2"), label = NULL, choices = NULL, multiple = TRUE,
                       options = list(placeholder = "e.g. Linear Regression")),

        div(style = "border-left:3px solid #BA7517; padding-left:8px;
                     margin-bottom:2px; margin-top:10px;",
            tags$label("Group 3",
                        style = "font-weight:600; font-size:13px; color:#BA7517;")),
        selectizeInput(ns("av_g3"), label = NULL, choices = NULL, multiple = TRUE,
                       options = list(placeholder = "e.g. Tree-Based Model")),

        div(style = "border-left:3px solid #993C1D; padding-left:8px;
                     margin-bottom:2px; margin-top:10px;",
            tags$label("Group 4",
                        style = "font-weight:600; font-size:13px; color:#993C1D;")),
        selectizeInput(ns("av_g4"), label = NULL, choices = NULL, multiple = TRUE,
                       options = list(placeholder = "e.g. Kernel Method")),

        div(style = "border-left:3px solid #1a6ebd; padding-left:8px;
                     margin-bottom:2px; margin-top:10px;",
            tags$label("Group 5",
                        style = "font-weight:600; font-size:13px; color:#1a6ebd;")),
        selectizeInput(ns("av_g5"), label = NULL, choices = NULL, multiple = TRUE,
                       options = list(placeholder = "e.g. Ensemble Model")),

        div(style = "border-left:3px solid #d63384; padding-left:8px;
                     margin-bottom:2px; margin-top:10px;",
            tags$label("Group 6 — Wildcard",
                        style = "font-weight:600; font-size:13px; color:#d63384;")),
        selectizeInput(ns("av_g6"), label = NULL, choices = NULL, multiple = TRUE,
                       options = list(placeholder = "e.g. anything...")),
        checkboxInput(ns("av_g6_or"),
                      "Match ANY tag (OR) — colour methods with at least one matching tag",
                      value = TRUE),
        hr(),

        # ── Map Configs ───────────────────────────────────────────────────────
        div(style = "background:#e2e3e5; border-left:3px solid #6c757d;
                     padding:6px 10px; border-radius:4px; margin-bottom:6px;",
            tags$label("Map Configs",
                        style = "font-weight:700; font-size:13px; color:#41464b;")),

        tags$label("Map distance metric",
                   style = "font-weight:600; font-size:13px; color:#343a40;
                            display:block; margin-bottom:4px; margin-top:10px;"),
        selectInput(ns("av_map_dist"), label = NULL,
                    choices  = c("euclidean", "manhattan", "binary", "canberra"),
                    selected = "manhattan"),

        tags$label("Label size",
                   style = "font-weight:600; font-size:13px; color:#343a40;
                            display:block; margin-bottom:4px; margin-top:10px;"),
        sliderInput(ns("av_map_label_size"), label = NULL,
                    min = 1, max = 5, value = 3, step = 0.5, width = "100%"),

        tags$label("Max label overlaps",
                   style = "font-weight:600; font-size:13px; color:#343a40;
                            display:block; margin-bottom:4px; margin-top:10px;"),
        sliderInput(ns("av_map_overlaps"), label = NULL,
                    min = 10, max = 100, value = 40, step = 5, width = "100%"),
        hr(),

        tags$label("Matching methods (from table filters)",
                   style = "font-weight:600; font-size:13px; color:#343a40;
                            display:block; margin-bottom:4px; margin-top:10px;"),
        verbatimTextOutput(ns("av_filter_summary_map"))
      ),

      mainPanel(
        width = 9,
        h4("Similarity map — colour groups highlighted",
           style = "border-left:3px solid #534AB7; padding-left:8px;
                    font-size:14px; margin-top:4px; margin-bottom:8px;"),
        shinycssloaders::withSpinner(plotOutput(ns("av_map_plot"), height = "80vh"))
      )
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

meth_available_server <- function(id,
                                   exclude_tags   = NULL,
                                   highlight_tags = NULL) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Group palette — order matches priority (first match wins on map)
    av_group_colours <- c(
      "1"    = "#534AB7",  # purple  — Group 1
      "2"    = "#0F6E56",  # teal    — Group 2
      "3"    = "#BA7517",  # amber   — Group 3
      "4"    = "#993C1D",  # coral   — Group 4
      "5"    = "#1a6ebd",  # blue    — Group 5
      "6"    = "#d63384",  # pink    — Group 6
      "none" = "#cccccc"   # grey    — unmatched
    )

    av_mi <- caret::getModelInfo()

    # ── Plain data-frame of all methods (no HTML, fast to compute) ────────────
    av_methods_plain <- reactive({
      n <- length(av_mi)
      Label <- Tags_plain <- Packages_plain <- Hyperparams <- character(n)
      Regression <- Classification <- ClassProbs <- logical(n)
      for (i in seq_len(n)) {
        m                 <- av_mi[[i]]
        Label[i]          <- m$label
        libs              <- na.omit(m$library[nchar(m$library) > 0])
        Packages_plain[i] <- paste(libs, collapse = "\n")
        Tags_plain[i]     <- paste(m$tags, collapse = "|")
        d                 <- m$parameters
        Hyperparams[i]    <- paste(paste0(d$parameter, " - ", d$label,
                                          " [", d$class, "]"), collapse = "\n")
        Regression[i]     <- "Regression"     %in% m$type
        Classification[i] <- "Classification" %in% m$type
        ClassProbs[i]     <- is.function(m$prob)
      }
      data.frame(Model          = names(av_mi),
                 Label          = Label,
                 Packages_plain = Packages_plain,
                 Tags_plain     = Tags_plain,
                 Hyperparams    = Hyperparams,
                 Regression     = Regression,
                 Classification = Classification,
                 ClassProbs     = ClassProbs,
                 stringsAsFactors = FALSE)
    })

    # ── HTML packages column (checks which libs are installed) ────────────────
    av_packages_html <- reactive({
      df   <- av_methods_plain()
      html <- character(nrow(df))
      for (i in seq_len(nrow(df))) {
        libs <- strsplit(df$Packages_plain[i], "\n")[[1]]
        libs <- libs[nchar(libs) > 0]
        if (length(libs) > 0) {
          present <- suppressWarnings(
            sapply(libs, function(l)
              requireNamespace(l, quietly = TRUE))
          )
          check   <- ifelse(present, "", as.character(icon("ban")))
          html[i] <- paste(paste(libs, check), collapse = "<br/>")
        }
      }
      data.frame(Model = df$Model, Packages_html = html,
                 stringsAsFactors = FALSE)
    })

    # ── Binary tag matrix for MDS ─────────────────────────────────────────────
    av_wide_matrix <- reactive({
      tags <- lapply(av_mi, `[[`, "tags")
      Reg  <- sapply(av_mi, function(m) as.integer("Regression"     %in% m$type))
      Cls  <- sapply(av_mi, function(m) as.integer("Classification" %in% m$type))
      all_tags <- sort(unique(unlist(tags)))
      all_tags <- all_tags[nchar(all_tags) > 0]
      dat <- matrix(0L, nrow = length(av_mi), ncol = length(all_tags),
                    dimnames = list(names(av_mi), all_tags))
      for (i in seq_along(tags)) {
        matched <- intersect(tags[[i]], all_tags)
        if (length(matched) > 0) dat[i, matched] <- 1L
      }
      as.data.frame(cbind(Regression = Reg, Classification = Cls, dat))
    })

    # ── Combine three type checkboxes into a character vector (OR logic) ────────
    av_type_checked <- reactive({
      c(if (isTRUE(input$av_type_reg))  "reg",
        if (isTRUE(input$av_type_cls))  "cls",
        if (isTRUE(input$av_type_both)) "both")
    })

    # ── Default exclude / highlight — fall back to nothing selected if not supplied
    default_exclude   <- exclude_tags   %||% character(0)
    default_highlight <- highlight_tags %||% character(0)

    # ── Observer 1: populate exclude selector with ALL tags (runs once) ───────
    observe({
      all_tags <- sort(unique(unlist(lapply(av_mi, `[[`, "tags"))))
      all_tags <- all_tags[nchar(all_tags) > 0]
      updateSelectizeInput(session, "av_flt_exclude",
                           choices  = all_tags,
                           server   = TRUE,
                           selected = intersect(default_exclude, all_tags))
    })

    # ── Observer 2: populate sections 2 & 3 with non-excluded tags ───────────
    observe({
      all_tags <- sort(unique(unlist(lapply(av_mi, `[[`, "tags"))))
      all_tags <- all_tags[nchar(all_tags) > 0]
      available_tags <- all_tags[!all_tags %in% input$av_flt_exclude]

      default_lit_tags <- intersect(available_tags, default_highlight)
      current_lit_tags <- isolate(input$av_flt_any)

      updateSelectizeInput(session, "av_flt_any",
                           choices  = available_tags,
                           selected = if (length(current_lit_tags) > 0)
                             current_lit_tags else default_lit_tags,
                           server = TRUE)

      updateSelectizeInput(session, "av_g1", choices = available_tags, server = TRUE,
                           selected = intersect("Neural Network",                       available_tags))
      updateSelectizeInput(session, "av_g2", choices = available_tags, server = TRUE,
                           selected = intersect("Linear Regression",                    available_tags))
      updateSelectizeInput(session, "av_g3", choices = available_tags, server = TRUE,
                           selected = intersect("Tree-Based Model",                     available_tags))
      updateSelectizeInput(session, "av_g4", choices = available_tags, server = TRUE,
                           selected = intersect("Kernel Method",                        available_tags))
      updateSelectizeInput(session, "av_g5", choices = available_tags, server = TRUE,
                           selected = intersect("Ensemble Model",                       available_tags))
      updateSelectizeInput(session, "av_g6", choices = available_tags, server = TRUE,
                           selected = intersect(c("Multivariate Adaptive Regression Splines",
                                                  "Feature Extraction",
                                                  "Generalized Additive Model"), available_tags))
    }) |> bindEvent(input$av_flt_exclude, ignoreNULL = FALSE)

    # ── Helper: does a Tags_plain string match ALL (or ANY) tags in a group? ──
    .matches_group <- function(tags_plain, group_tags, any_logic = FALSE) {
      if (length(group_tags) == 0) return(FALSE)
      hits <- sapply(group_tags, function(t) grepl(t, tags_plain, ignore.case = TRUE))
      if (any_logic) any(hits) else all(hits)
    }

    # ── Base filter (type checkboxes = OR logic, + exclude tags) ─────────────
    av_base_df <- reactive({
      df      <- av_methods_plain()
      checked <- av_type_checked()
      if (length(checked) == 0) {
        # nothing checked → show all
        keep <- rep(TRUE, nrow(df))
      } else {
        keep <- rep(FALSE, nrow(df))
        if ("reg"  %in% checked) keep <- keep | df$Regression
        if ("cls"  %in% checked) keep <- keep | df$Classification
        if ("both" %in% checked) keep <- keep | (df$Regression & df$Classification)
      }
      df <- df[keep, ]
      for (tag in input$av_flt_exclude)
        df <- df[!grepl(tag, df$Tags_plain, ignore.case = TRUE), ]
      df
    })

    # ── Assign group membership (first match wins) ────────────────────────────
    av_grouped_df <- reactive({
      df <- av_base_df()
      g_inputs  <- list(
        "1" = input$av_g1, "2" = input$av_g2, "3" = input$av_g3,
        "4" = input$av_g4, "5" = input$av_g5, "6" = input$av_g6
      )
      g6_or <- isTRUE(input$av_g6_or)
      df$Group <- sapply(df$Tags_plain, function(tp) {
        matched <- "none"
        for (g in names(g_inputs)) {
          any_logic <- (g == "6" && g6_or)
          if (.matches_group(tp, g_inputs[[g]], any_logic)) { matched <- g; break }
        }
        matched
      })
      df
    })

    # ── All type/exclude-filtered methods, with group assignments for colouring ─
    av_filtered_df <- reactive({
      av_grouped_df()
    })

    # ── Sidebar count (table tab) ─────────────────────────────────────────────
    output$av_filter_summary <- renderPrint({
      df <- av_filtered_df()
      cat(nrow(df), "method(s)\n")
      if (nrow(df) > 0 && nrow(df) <= 30)
        cat(paste(sort(df$Model), collapse = ", "))
    })

    # ── Sidebar count (map tab — separate output ID to avoid DOM duplication) ─
    output$av_filter_summary_map <- renderPrint({
      df <- av_filtered_df()
      cat(nrow(df), "method(s)\n")
      if (nrow(df) > 0 && nrow(df) <= 30)
        cat(paste(sort(df$Model), collapse = ", "))
    })

    # ── Method table ──────────────────────────────────────────────────────────
    output$av_method_table <- DT::renderDataTable({
      filt <- av_filtered_df()
      html <- av_packages_html()
      df   <- merge(filt, html, by = "Model")

      display <- data.frame(
        Model           = df$Model,
        Label           = df$Label,
        Packages        = df$Packages_html,
        Tags            = gsub("\\|", ", ", df$Tags_plain),
        Hyperparameters = df$Hyperparams,
        Regression      = df$Regression,
        Classification  = df$Classification,
        ClassProbs      = df$ClassProbs,
        Group           = df$Group,
        stringsAsFactors = FALSE
      )

      DT::datatable(display,
                    escape     = FALSE,
                    rownames   = FALSE,
                    selection  = "none",
                    options    = list(pageLength  = 10,
                                      lengthMenu  = c(5, 10, 25, 50),
                                      scrollX     = TRUE))
    })

    # ── Method map ────────────────────────────────────────────────────────────
    output$av_map_plot <- renderPlot({
      wide    <- av_wide_matrix()
      # Restrict map to exactly the same models as the table (type + exclude tags)
      base_models <- av_base_df()$Model
      wide_sub    <- wide[rownames(wide) %in% base_models, ]
      req(nrow(wide_sub) >= 3)

      lit_boost <- 3

      d   <- stats::dist(wide_sub, method = input$av_map_dist %||% "manhattan")
      dd  <- stats::cmdscale(d, k = 2)
      df_map <- data.frame(Model = rownames(dd), X1 = dd[, 1], X2 = dd[, 2],
                           stringsAsFactors = FALSE)

      grp_df <- av_grouped_df()[, c("Model", "Group")]
      df_map <- merge(df_map, grp_df, by = "Model", all.x = TRUE)
      df_map$Group[is.na(df_map$Group)] <- "none"

      p <- ggplot2::ggplot(mapping = ggplot2::aes(x = X1, y = X2, label = Model)) +
        ggplot2::ggtitle(paste(nrow(wide_sub), "Methods — coloured by group")) +
        ggplot2::xlab("Coordinate 1") + ggplot2::ylab("Coordinate 2") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 18))

      label_size <- input$av_map_label_size %||% 3
      overlaps   <- input$av_map_overlaps   %||% 40

      .add_layer <- function(p, df_sub, col, size_pt, size_txt, face) {
        if (nrow(df_sub) == 0) return(p)
        p +
          ggplot2::geom_point(data = df_sub, colour = col, size = size_pt) +
          ggrepel::geom_text_repel(data         = df_sub,
                                   size         = size_txt,
                                   colour       = col,
                                   fontface     = face,
                                   max.overlaps = overlaps,
                                   box.padding  = 0.4,
                                   na.rm        = TRUE)
      }

      tags_lookup <- av_base_df()[, c("Model", "Tags_plain")]
      has_lit     <- length(input$av_flt_any) > 0
      pattern     <- if (has_lit) paste(input$av_flt_any, collapse = "|") else ""

      .split_lit <- function(df_sub) {
        if (!has_lit) {
          df_sub$LitHit <- FALSE
        } else {
          df_sub <- merge(df_sub, tags_lookup, by = "Model", all.x = TRUE)
          df_sub$LitHit <- grepl(pattern, df_sub$Tags_plain, ignore.case = TRUE)
        }
        list(plain = df_sub[!df_sub$LitHit, ],
             lit   = df_sub[ df_sub$LitHit, ])
      }

      df_grey <- df_map[df_map$Group == "none", ]
      if (nrow(df_grey) > 0) {
        sp <- .split_lit(df_grey)
        p  <- .add_layer(p, sp$plain, "#cccccc", 1.5, label_size,             "plain")
        p  <- .add_layer(p, sp$lit,   "#888888", 2.0, label_size + lit_boost, "bold")
      }

      for (g in c("6", "5", "4", "3", "2", "1")) {
        df_g <- df_map[df_map$Group == g, ]
        if (nrow(df_g) > 0) {
          col <- av_group_colours[g]
          sp  <- .split_lit(df_g)
          p   <- .add_layer(p, sp$plain, col, 2.0, label_size,             "plain")
          p   <- .add_layer(p, sp$lit,   col, 2.5, label_size + lit_boost, "bold")
        }
      }

      active_groups <- sort(unique(df_map$Group[df_map$Group != "none"]))
      if (length(active_groups) > 0) {
        group_names <- c("1" = "Neural Network", "2" = "OLS",      "3" = "Tree-Based",
                         "4" = "Kernel",         "5" = "Ensemble", "6" = "Wildcard")
        p <- p + ggplot2::labs(
          caption = paste(
            sapply(active_groups, function(g)
              paste0("Group ", g, " (", group_names[g], ")")),
            collapse = "   "
          )
        )
      }
      p
    })

  })
}
