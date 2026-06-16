# mod_results.R
# Slide-in results drawer.
# Tabs: ICER Table | CE Plane | Tornado | PSA Scatter | CEAC | Price Threshold

mod_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML("
      /* Widen drawer to match design */
      .results-drawer { width: min(940px, 88vw) !important; }

      /* Price threshold stat grid */
      .pt-grid {
        display: grid; grid-template-columns: repeat(4, 1fr);
        gap: 12px; margin-top: 16px;
      }
      .pt-stat {
        border: 1px solid #e5e5e5; border-radius: 4px;
        padding: 13px; text-align: center;
      }
      .pt-val {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 18px; font-weight: 600;
      }
      .pt-lbl {
        font-size: 10px; color: #737373; margin-top: 4px;
        text-transform: uppercase; letter-spacing: 0.05em;
      }
      .pt-chart-wrap { height: 320px; }

      /* Interpretation block */
      .interp-block {
        border: 1px solid #e5e5e5; border-radius: 4px;
        padding: 15px; font-size: 13px; line-height: 1.75;
        margin-top: 16px;
      }
      .interp-block.ok   { border-left: 3px solid #047857; }
      .interp-block.warn { border-left: 3px solid #FE7501; }

      /* PSA CE-at-threshold table */
      .psa-ce-table {
        width: 100%; border-collapse: collapse; font-size: 12px;
        margin-bottom: 14px;
      }
      .psa-ce-table th {
        font-size: 10px; text-transform: uppercase; letter-spacing: 0.07em;
        color: #737373; font-weight: 700; padding: 5px 10px;
        border-bottom: 1.5px solid #0a0a0a; white-space: nowrap;
        background: #fff; text-align: right;
      }
      .psa-ce-table th:first-child { text-align: left; }
      .psa-ce-table td {
        padding: 6px 10px; border-bottom: 1px solid #f0f0f0;
        text-align: right; font-family: 'IBM Plex Mono', monospace;
        font-size: 12px; font-weight: 600;
      }
      .psa-ce-table td:first-child {
        text-align: left; font-family: inherit; font-weight: 400;
        display: flex; align-items: center; gap: 7px;
      }
      .psa-dot {
        width: 8px; height: 8px; border-radius: 50%; flex: 0 0 auto;
        display: inline-block;
      }
      .psa-pct-hi { color: #047857; }
      .psa-pct-lo { color: #b91c1c; }

      /* Focal strategy picker */
      .focal-picker-row {
        display: flex; align-items: center; gap: 10px; flex-wrap: wrap;
        padding: 0 0 10px; margin-bottom: 6px;
        border-bottom: 1px solid #e5e5e5;
      }
      .focal-picker-row .form-group { margin-bottom: 0; }
      .focal-picker-lbl {
        font-size: 11px; font-weight: 700; color: #737373;
        text-transform: uppercase; letter-spacing: 0.05em;
      }
      .focal-picker-hint {
        font-size: 12px; color: #a3a3a3;
      }
    "))),

    div(
      id      = ns("drawer_overlay"),
      class   = "drawer-overlay",
      onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("overlay_click"))
    ),

    div(id = ns("results_drawer"), class = "results-drawer",
      div(class = "drawer-header",
        h4("Analysis Results", style = "margin:0; font-size:15px;"),
        tags$button(
          id      = ns("close_btn"),
          class   = "drawer-close",
          type    = "button",
          onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("close_btn")),
          HTML("&times;")
        )
      ),

      div(class = "drawer-body",
        tabsetPanel(id = ns("tabs"), type = "pills",

          tabPanel("ICER Table",
            div(class = "drawer-tab-body",
              div(class = "card mb-3",
                div(class = "card-header", "ICER Results"),
                div(class = "card-body p-0",
                  shinycssloaders::withSpinner(
                    DT::dataTableOutput(ns("icer_dt")),
                    type = 4, color = "#27AAE1", size = 0.6
                  )
                )
              ),
              uiOutput(ns("interpretation"))
            )
          ),

          tabPanel("CE Plane",
            div(class = "drawer-tab-body",
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(ns("ce_plane_plot"), height = "320px"),
                type = 4, color = "#27AAE1", size = 0.6
              )
            )
          ),

          tabPanel("Tornado",
            div(class = "drawer-tab-body",
              div(class = "focal-picker-row",
                span(class = "focal-picker-lbl", "Focal strategy"),
                selectInput(ns("focal_strategy"), NULL, choices = NULL, width = "200px"),
                uiOutput(ns("tornado_threshold_selector"), inline = TRUE)
              ),
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(ns("tornado_plot"), height = "360px"),
                type = 4, color = "#27AAE1", size = 0.6
              )
            )
          ),

          tabPanel("PSA Scatter",
            div(class = "drawer-tab-body",
              uiOutput(ns("psa_chips")),
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(ns("psa_scatter_plot"), height = "380px"),
                type = 4, color = "#27AAE1", size = 0.6
              )
            )
          ),

          tabPanel("CEAC",
            div(class = "drawer-tab-body",
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(ns("ceac_plot"), height = "320px"),
                type = 4, color = "#27AAE1", size = 0.6
              )
            )
          ),

          tabPanel("Price Threshold",
            div(class = "drawer-tab-body",
              div(class = "card",
                div(class = "card-header",
                  span("Price Threshold Analysis"),
                  uiOutput(ns("pt_subtitle"), inline = TRUE)
                ),
                div(class = "card-body",
                  div(class = "focal-picker-row",
                    span(class = "focal-picker-lbl", "Focal strategy"),
                    uiOutput(ns("pt_focal_ui"), inline = TRUE),
                    uiOutput(ns("pt_threshold_selector"), inline = TRUE)
                  ),
                  div(class = "pt-chart-wrap",
                    shinycssloaders::withSpinner(
                      plotly::plotlyOutput(ns("pt_chart"), height = "300px"),
                      type = 4, color = "#27AAE1", size = 0.6
                    )
                  ),
                  uiOutput(ns("pt_stats"))
                )
              )
            )
          )
        )
      )
    )
  )
}

mod_results_server <- function(id, results, parameters, open_trigger,
                               psa_results = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    # ── Open / close ──────────────────────────────────────────────────────
    observeEvent(open_trigger(), {
      shinyjs::addClass(id = "results_drawer", class = "drawer-open")
      shinyjs::addClass(id = "drawer_overlay", class = "drawer-open")
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    .close <- function() {
      shinyjs::removeClass(id = "results_drawer", class = "drawer-open")
      shinyjs::removeClass(id = "drawer_overlay", class = "drawer-open")
    }
    observeEvent(input$close_btn,     .close())
    observeEvent(input$overlay_click, .close())

    # ── Focal strategy: Tornado (also used by PSA Scatter & CEAC) ───────────
    observeEvent(results(), {
      r <- as.data.frame(results())
      if (nrow(r) < 2L) return()

      non_ref <- r[-1L, , drop = FALSE]
      fin     <- non_ref[is.finite(non_ref$ICER), ]
      default_focal <- if (nrow(fin) > 0L) fin$Strategy[1L] else non_ref$Strategy[1L]

      updateSelectInput(session, "focal_strategy",
                        choices = non_ref$Strategy, selected = default_focal)
      updateSelectInput(session, "pt_focal",
                        choices = non_ref$Strategy, selected = default_focal)
    }, ignoreNULL = TRUE)

    # PT focal ui (renders inside the PT tab)
    output$pt_focal_ui <- renderUI({
      req(results())
      r       <- as.data.frame(results())
      non_ref <- r[-1L, , drop = FALSE]
      selectInput(session$ns("pt_focal"), NULL,
                  choices = non_ref$Strategy, width = "200px")
    })

    # Tornado threshold selector
    output$tornado_threshold_selector <- renderUI({
      req(parameters())
      tv <- parameters()$thresholds
      selectInput(session$ns("tornado_threshold"), NULL,
        choices  = setNames(as.character(tv),
                            paste0(names(tv), " — KES ", format(tv, big.mark = ","))),
        selected = as.character(min(tv)),
        width    = "200px"
      )
    })

    tornado_thr <- reactive({
      tv  <- parameters()$thresholds %||% parameters()$threshold
      sel <- suppressWarnings(as.numeric(input$tornado_threshold))
      if (length(sel) == 1L && is.finite(sel) && sel %in% tv) sel else min(tv, na.rm = TRUE)
    })

    # Reference row + the user-selected focal (non-reference) row.
    focal_pair <- reactive({
      req(results(), input$focal_strategy)
      r   <- as.data.frame(results())
      ref <- r[1L, ]
      foc <- r[r$Strategy == input$focal_strategy, ][1L, ]
      if (nrow(foc) == 0L || is.na(foc$Strategy)) return(NULL)

      inc_cost   <- foc$Cost   - ref$Cost
      inc_effect <- foc$Effect - ref$Effect
      icer <- if (is.finite(inc_effect) && inc_effect != 0) inc_cost / inc_effect else NA_real_

      list(ref = ref, focal = foc,
           inc_cost = inc_cost, inc_effect = inc_effect, icer = icer)
    })

    pt_focal_pair <- reactive({
      req(results(), input$pt_focal)
      r   <- as.data.frame(results())
      ref <- r[1L, ]
      foc <- r[r$Strategy == input$pt_focal, ][1L, ]
      if (nrow(foc) == 0L || is.na(foc$Strategy)) return(NULL)

      inc_cost   <- foc$Cost   - ref$Cost
      inc_effect <- foc$Effect - ref$Effect
      icer <- if (is.finite(inc_effect) && inc_effect != 0) inc_cost / inc_effect else NA_real_

      list(ref = ref, focal = foc,
           inc_cost = inc_cost, inc_effect = inc_effect, icer = icer)
    })

    # ── ICER table ────────────────────────────────────────────────────────
    output$icer_dt <- DT::renderDataTable({
      req(results(), parameters())
      r  <- as.data.frame(results())
      ol <- parameters()$outcome_label %||% "unit"

      # Drop dampack Status column — redundant with interpretation block
      r <- r[, setdiff(names(r), "Status"), drop = FALSE]

      # Rename ICER column to carry unit
      icer_col <- paste0("ICER (KES / ", ol, ")")
      if ("ICER" %in% names(r)) names(r)[names(r) == "ICER"] <- icer_col

      cols_cur   <- intersect(c("Cost", "Inc_Cost"), names(r))
      cols_round <- intersect(c("Effect", "Inc_Effect"), names(r))

      dt <- DT::datatable(r,
        options  = list(dom = "t", pageLength = 20, scrollX = FALSE,
                        searching = FALSE, ordering = FALSE, autoWidth = TRUE),
        rownames = FALSE,
        class    = "table table-sm"
      )
      if (length(cols_cur)   > 0) dt <- DT::formatCurrency(dt, cols_cur, currency = "KES ", digits = 0)
      if (length(cols_round) > 0) dt <- DT::formatRound(dt, cols_round, digits = 2L)
      if (icer_col %in% names(r)) dt <- DT::formatCurrency(dt, icer_col, currency = "", digits = 0)
      dt
    }, server = FALSE)

    # ── Interpretation ────────────────────────────────────────────────────
    output$interpretation <- renderUI({
      req(results(), parameters())
      r       <- as.data.frame(results())
      thr_vec <- parameters()$thresholds %||% c(parameters()$threshold)
      ol      <- parameters()$outcome_label %||% "unit"

      rows       <- r[order(r$Cost), ]
      has_status <- "Status" %in% names(rows)

      .ce_badge <- function(ce, lbl) {
        col <- if (ce) "#047857" else "#b91c1c"
        bg  <- if (ce) "#dcfce7" else "#fee2e2"
        tags$span(
          style = paste0(
            "display:inline-block; margin:1px 3px; padding:1px 7px; border-radius:3px;",
            "font-size:11px; font-weight:600; color:", col, "; background:", bg, ";"),
          paste0(if (ce) "✓ " else "✗ ", lbl)
        )
      }

      lines <- lapply(seq_len(nrow(rows)), function(i) {
        s  <- rows[i, ]
        st <- if (has_status) toupper(s$Status) else ""

        if (st == "D")
          return(tags$div(style = "margin-bottom:6px;", paste0(
            "• ", s$Strategy, " — dominated: a less costly strategy is at least as effective.")))
        if (st == "ED")
          return(tags$div(style = "margin-bottom:6px;", paste0(
            "• ", s$Strategy, " — extendedly dominated: a combination of other strategies ",
            "offers the same benefit at lower cost.")))

        if (is.na(s$ICER) && (is.na(s$Inc_Cost) || is.na(s$Inc_Effect))) return(NULL)

        if (is.finite(s$ICER)) {
          badges <- lapply(seq_along(thr_vec), function(k)
            .ce_badge(s$ICER <= thr_vec[k], names(thr_vec)[k]))
          return(
            tags$div(style = "margin-bottom:8px;",
              tags$div(paste0("• ", s$Strategy, " — ICER: KES ",
                              format(round(s$ICER), big.mark = ","), " per ", ol)),
              tags$div(style = "margin-left:14px; margin-top:2px;",
                       do.call(tagList, badges))
            )
          )
        }
        NULL
      })
      lines <- Filter(Negate(is.null), lines)

      ce_at <- names(thr_vec)[vapply(thr_vec, function(th)
        any(is.finite(rows$ICER) & rows$ICER <= th), logical(1L))]
      has_any_ce <- length(ce_at) > 0
      cls <- if (has_any_ce) "interp-block ok" else "interp-block warn"

      summary_txt <- if (has_any_ce)
        paste0("Cost-effective at: ", paste(ce_at, collapse = ", "), ".")
      else
        "No strategy is cost-effective at any selected threshold."

      div(class = cls,
        do.call(tagList, lines),
        if (length(lines) > 0)
          tags$div(style = "margin-top:10px; font-weight:700;", summary_txt)
      )
    })

    # ── CE Plane (deterministic) ──────────────────────────────────────────
    output$ce_plane_plot <- plotly::renderPlotly({
      req(results(), parameters())
      r   <- as.data.frame(results())
      thr <- parameters()$threshold
      ol  <- parameters()$outcome_label %||% "unit"
      has_status <- "Status" %in% names(r)

      ref     <- r[1L, ]
      non_ref <- r[-1L, , drop = FALSE]

      # dampack leaves Inc_Cost/Inc_Effect NA for dominated (D) and extendedly
      # dominated (ED) strategies; fall back to raw incremental values vs the
      # reference so they plot at their true position rather than at (0, 0).
      non_ref$Inc_Effect <- ifelse(is.finite(non_ref$Inc_Effect),
                                    non_ref$Inc_Effect, non_ref$Effect - ref$Effect)
      non_ref$Inc_Cost   <- ifelse(is.finite(non_ref$Inc_Cost),
                                    non_ref$Inc_Cost, non_ref$Cost - ref$Cost)

      palette <- c("#27AAE1", "#FE7501", "#084887", "#EFCA08", "#497048", "#006D77", "#8E44AD", "#D11060")

      x_vals <- c(0, non_ref$Inc_Effect)
      x_pad  <- max(diff(range(x_vals, na.rm = TRUE)) * 0.15, 1)
      x_lo   <- min(x_vals, na.rm = TRUE) - x_pad
      x_hi   <- max(x_vals, na.rm = TRUE) + x_pad

      p <- plotly::plot_ly()

      # CE threshold lines (one per selected threshold)
      thr_vec  <- parameters()$thresholds %||% c(thr)
      thr_cols <- c("#dc2626", "#ea580c", "#7c3aed", "#0369a1")
      for (.k in seq_along(thr_vec)) {
        p <- p |>
          plotly::add_lines(
            x    = c(x_lo, x_hi),
            y    = thr_vec[.k] * c(x_lo, x_hi),
            name = paste0(names(thr_vec)[.k], " — KES ", format(thr_vec[.k], big.mark = ",")),
            line = list(
              color = thr_cols[((.k - 1L) %% 4L) + 1L],
              width = 1.5, dash = "dash"
            ),
            hovertemplate = "CE threshold<extra></extra>"
          )
      }

      # Reference point (diamond)
      p <- p |>
        plotly::add_markers(
          x = 0, y = 0,
          name   = ref$Strategy,
          marker = list(color = "#404040", size = 11, symbol = "diamond"),
          hovertemplate = paste0("<b>", ref$Strategy, "</b> (reference)<extra></extra>")
        )

      # Non-reference deterministic points
      for (i in seq_len(nrow(non_ref))) {
        s        <- non_ref[i, ]
        col      <- palette[((i - 1L) %% length(palette)) + 1L]
        st       <- if (has_status) toupper(s$Status) else ""
        icer_txt <- if (is.finite(s$ICER))
          paste0("KES ", format(round(s$ICER), big.mark = ","))
        else if (st %in% c("D", "ED")) "Dominated"
        else "—"
        p <- p |>
          plotly::add_markers(
            x           = s$Inc_Effect,
            y           = s$Inc_Cost,
            name        = s$Strategy,
            legendgroup = s$Strategy,
            marker      = list(color = col, size = 11, symbol = "circle"),
            hovertemplate = paste0(
              "<b>", s$Strategy, "</b><br>",
              "Δ Effect: %{x:.2f} ", ol, "<br>",
              "Δ Cost: KES %{y:,.0f}<br>",
              "ICER: ", icer_txt, "<extra></extra>"
            )
          )
      }

      p |>
        plotly::layout(
          xaxis = list(
            title         = paste0("Incremental effect (", ol, ")"),
            gridcolor     = "#ededed",
            zerolinecolor = "#a3a3a3", zerolinewidth = 1.5
          ),
          yaxis = list(
            title         = "Incremental cost (KES)",
            tickformat    = ",.0f",
            gridcolor     = "#ededed",
            zerolinecolor = "#a3a3a3", zerolinewidth = 1.5
          ),
          plot_bgcolor  = "#ffffff",
          paper_bgcolor = "#ffffff",
          margin = list(l = 70, r = 20, b = 55, t = 10),
          legend = list(orientation = "h", y = -0.28, font = list(size = 11)),
          font   = list(family = "Archivo, system-ui, sans-serif",
                        size = 12, color = "#0a0a0a")
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # ── Tornado (one-way SA on ICER) ─────────────────────────────────────
    output$tornado_plot <- plotly::renderPlotly({
      req(results(), parameters())
      r   <- as.data.frame(results())
      thr <- tornado_thr()
      ol  <- parameters()$outcome_label %||% "unit"

      .empty <- function(msg)
        plotly::plot_ly() |>
          plotly::layout(
            annotations  = list(list(text = msg, showarrow = FALSE,
              x = 0.5, y = 0.5, xref = "paper", yref = "paper",
              font = list(size = 13, color = "#737373"))),
            paper_bgcolor = "#fff", plot_bgcolor = "#fff"
          )

      fp <- focal_pair()
      if (is.null(fp) || !is.finite(fp$icer))
        return(.empty("No valid comparison available."))

      ref        <- fp$ref
      focal      <- fp$focal
      base_icer  <- fp$icer
      ref_name   <- ref$Strategy
      focal_name <- focal$Strategy

      sd_base <- data.frame(
        strategy = r$Strategy,
        cost     = r$Cost,
        effect   = r$Effect,
        stringsAsFactors = FALSE
      )

      # Compute ICER of focal vs ref with fixed comparator names
      .icer <- function(sd) {
        r_row <- sd[sd$strategy == ref_name,   ]
        f_row <- sd[sd$strategy == focal_name, ]
        if (nrow(r_row) == 0L || nrow(f_row) == 0L) return(NA_real_)
        inc_e <- f_row$effect[1L] - r_row$effect[1L]
        if (!is.finite(inc_e) || inc_e == 0) return(NA_real_)
        (f_row$cost[1L] - r_row$cost[1L]) / inc_e
      }

      # Vary each parameter ±20%
      rows <- lapply(seq_len(nrow(sd_base)), function(i) {
        s   <- sd_base$strategy[i]
        bc  <- sd_base$cost[i];   be <- sd_base$effect[i]
        lo_c <- sd_base; lo_c$cost[i]   <- bc * 0.80
        hi_c <- sd_base; hi_c$cost[i]   <- bc * 1.20
        lo_e <- sd_base; lo_e$effect[i] <- be * 0.80
        hi_e <- sd_base; hi_e$effect[i] <- be * 1.20
        list(
          data.frame(parameter = paste0(s, ": cost"),
            icer_lo = .icer(lo_c), icer_hi = .icer(hi_c),
            stringsAsFactors = FALSE),
          data.frame(parameter = paste0(s, ": effect"),
            icer_lo = .icer(lo_e), icer_hi = .icer(hi_e),
            stringsAsFactors = FALSE)
        )
      })

      td <- do.call(rbind, unlist(rows, recursive = FALSE))
      td <- td[is.finite(td$icer_lo) & is.finite(td$icer_hi), ]
      td$range <- abs(td$icer_hi - td$icer_lo)
      td <- td[td$range > 0, ]

      if (nrow(td) == 0L) return(.empty("No parameter sensitivity detected."))

      td <- td[order(td$range), ]   # ascending → widest bar at top

      p <- plotly::plot_ly()
      for (i in seq_len(nrow(td))) {
        row <- td[i, ]
        lo  <- min(row$icer_lo, row$icer_hi)
        hi  <- max(row$icer_lo, row$icer_hi)
        p <- p |>
          plotly::add_bars(
            x           = hi - lo,
            base        = lo,
            y           = row$parameter,
            orientation = "h",
            marker      = list(color = "#27AAE1",
                               line  = list(color = "#fff", width = 0.5)),
            hovertemplate = paste0(
              "<b>", row$parameter, "</b><br>",
              "Low: KES ", format(round(lo), big.mark = ","), "<br>",
              "High: KES ", format(round(hi), big.mark = ","), "<br>",
              "Range: KES ", format(round(hi - lo), big.mark = ","),
              "<extra></extra>"
            ),
            showlegend = FALSE
          )
      }

      p |>
        plotly::layout(
          title = list(
            text = paste0("ICER sensitivity: ", focal_name, " vs ", ref_name,
                          " (±20% each parameter)"),
            font = list(size = 12, color = "#404040"), x = 0
          ),
          xaxis = list(
            title         = paste0("ICER (KES per ", ol, ")"),
            tickformat    = ",.0f",
            gridcolor     = "#ededed",
            zerolinecolor = "#d4d4d4"
          ),
          yaxis  = list(title = "", tickfont = list(size = 11),
                        categoryorder = "array",
                        categoryarray = td$parameter),
          shapes = list(
            list(type = "line", x0 = base_icer, x1 = base_icer,
                 y0 = 0, y1 = 1, yref = "paper",
                 line = list(color = "#404040", width = 1.5, dash = "dot")),
            list(type = "line", x0 = thr, x1 = thr,
                 y0 = 0, y1 = 1, yref = "paper",
                 line = list(color = "#dc2626", width = 1.5, dash = "dash"))
          ),
          annotations = list(
            list(x = base_icer, y = 1.05, xref = "x", yref = "paper",
                 text = "Base case", showarrow = FALSE,
                 font = list(size = 10, color = "#404040")),
            list(x = thr, y = 1.05, xref = "x", yref = "paper",
                 text = paste0("Threshold (", names(parameters()$thresholds[parameters()$thresholds == thr]), ")"),
                 showarrow = FALSE,
                 font = list(size = 10, color = "#dc2626"))
          ),
          plot_bgcolor  = "#ffffff",
          paper_bgcolor = "#ffffff",
          margin = list(l = 190, r = 30, b = 55, t = 45),
          font   = list(family = "Archivo, system-ui, sans-serif",
                        size = 12, color = "#0a0a0a")
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # ── PSA Scatter ───────────────────────────────────────────────────────
    # Returns list(non_ref, ref, ref_j, cost_m, eff_m) or NULL if PSA / results
    # are unavailable, shared by the chips and the scatter plot.
    psa_scatter_data <- reactive({
      req(results())
      psa <- psa_results()
      if (is.null(psa)) return(NULL)

      r       <- as.data.frame(results())
      ref     <- r[1L, ]
      non_ref <- r[-1L, , drop = FALSE]

      cost_m <- as.matrix(psa$cost)
      eff_m  <- as.matrix(psa$effectiveness)
      ref_j  <- match(make.names(ref$Strategy), psa$strategies)
      if (is.na(ref_j)) ref_j <- 1L

      list(non_ref = non_ref, ref = ref, ref_j = ref_j,
           cost_m = cost_m, eff_m = eff_m)
    })

    output$psa_chips <- renderUI({
      req(parameters())
      d <- psa_scatter_data()
      if (is.null(d)) return(NULL)
      thr_vec <- parameters()$thresholds %||% c(parameters()$threshold)
      palette <- c("#27AAE1", "#FE7501", "#084887", "#EFCA08", "#497048", "#006D77", "#8E44AD", "#D11060")

      ref_c <- d$cost_m[, d$ref_j]
      ref_e <- d$eff_m[,  d$ref_j]

      # Build pct matrix: strategies × thresholds
      pct_mat <- lapply(seq_len(nrow(d$non_ref)), function(i) {
        s_name <- d$non_ref$Strategy[i]
        s_j    <- match(make.names(s_name), colnames(d$cost_m))
        if (is.na(s_j)) return(NULL)
        inc_c <- d$cost_m[, s_j] - ref_c
        inc_e <- d$eff_m[,  s_j] - ref_e
        pcts  <- vapply(thr_vec, function(th) mean(inc_c <= th * inc_e) * 100, numeric(1L))
        list(name = s_name, pcts = pcts,
             col  = palette[((i - 1L) %% length(palette)) + 1L])
      })
      pct_mat <- Filter(Negate(is.null), pct_mat)
      if (length(pct_mat) == 0L) return(NULL)

      # Header row
      hdr <- tags$tr(
        tags$th("Strategy"),
        lapply(names(thr_vec), tags$th)
      )

      # Data rows
      body_rows <- lapply(pct_mat, function(row) {
        cells <- lapply(row$pcts, function(p) {
          cls <- if (p >= 50) "psa-pct-hi" else "psa-pct-lo"
          tags$td(class = cls, paste0(round(p), "%"))
        })
        tags$tr(
          tags$td(
            tags$span(class = "psa-dot",
                      style = paste0("background:", row$col, ";")),
            row$name
          ),
          do.call(tagList, cells)
        )
      })

      tags$table(class = "psa-ce-table",
        tags$thead(hdr),
        tags$tbody(do.call(tagList, body_rows))
      )
    })

    output$psa_scatter_plot <- plotly::renderPlotly({
      req(parameters())
      thr <- parameters()$threshold
      ol  <- parameters()$outcome_label %||% "unit"

      .empty <- function(msg)
        plotly::plot_ly() |>
          plotly::layout(
            annotations  = list(list(text = msg, showarrow = FALSE,
              x = 0.5, y = 0.5, xref = "paper", yref = "paper",
              font = list(size = 13, color = "#737373"))),
            paper_bgcolor = "#fff", plot_bgcolor = "#fff"
          )

      d <- psa_scatter_data()
      if (is.null(d) || nrow(d$non_ref) == 0L) return(.empty("PSA results not available."))

      palette <- c("#27AAE1", "#FE7501", "#084887", "#EFCA08", "#497048", "#006D77", "#8E44AD", "#D11060")
      ref_c <- d$cost_m[, d$ref_j]
      ref_e <- d$eff_m[,  d$ref_j]

      p <- plotly::plot_ly()
      x_vals <- 0
      y_vals <- 0

      for (i in seq_len(nrow(d$non_ref))) {
        s_name <- d$non_ref$Strategy[i]
        s_j    <- match(make.names(s_name), colnames(d$cost_m))
        if (is.na(s_j)) next

        col   <- palette[((i - 1L) %% length(palette)) + 1L]
        inc_c <- d$cost_m[, s_j] - ref_c
        inc_e <- d$eff_m[,  s_j] - ref_e
        x_vals <- c(x_vals, inc_e)
        y_vals <- c(y_vals, inc_c)

        p <- p |>
          plotly::add_markers(
            x      = inc_e,
            y      = inc_c,
            name   = s_name,
            marker = list(color = col, size = 4, opacity = 0.3, symbol = "circle"),
            hovertemplate = paste0(
              "<b>", s_name, "</b><br>",
              "Δ Effect: %{x:.2f} ", ol, "<br>",
              "Δ Cost: KES %{y:,.0f}<extra></extra>"
            )
          )
      }

      x_pad <- max(diff(range(x_vals, na.rm = TRUE)) * 0.08, 1)
      y_pad <- max(diff(range(y_vals, na.rm = TRUE)) * 0.08, 1)
      x_lo  <- min(x_vals, na.rm = TRUE) - x_pad
      x_hi  <- max(x_vals, na.rm = TRUE) + x_pad
      y_lo  <- min(y_vals, na.rm = TRUE) - y_pad
      y_hi  <- max(y_vals, na.rm = TRUE) + y_pad

      # CE threshold lines (one per selected threshold)
      {
        thr_vec  <- parameters()$thresholds %||% c(thr)
        thr_cols <- c("#dc2626", "#ea580c", "#7c3aed", "#0369a1")
        for (.k in seq_along(thr_vec)) {
          p <- p |>
            plotly::add_lines(
              x    = c(x_lo, x_hi),
              y    = thr_vec[.k] * c(x_lo, x_hi),
              name = paste0(names(thr_vec)[.k], " — KES ", format(thr_vec[.k], big.mark = ",")),
              line = list(
                color = thr_cols[((.k - 1L) %% 4L) + 1L],
                width = 1.5, dash = "dash"
              ),
              hovertemplate = "CE threshold<extra></extra>"
            )
        }
      }

      # Reference point (diamond, at origin)
      p <- p |>
        plotly::add_markers(
          x = 0, y = 0,
          name   = d$ref$Strategy,
          marker = list(color = "#404040", size = 11, symbol = "diamond"),
          hovertemplate = paste0("<b>", d$ref$Strategy, "</b> (reference)<extra></extra>")
        )

      p |>
        plotly::layout(
          xaxis = list(
            title         = paste0("Incremental effect (", ol, ")"),
            range         = c(x_lo, x_hi),
            gridcolor     = "#ededed",
            zerolinecolor = "#a3a3a3", zerolinewidth = 1.5
          ),
          yaxis = list(
            title         = "Incremental cost (KES)",
            range         = c(y_lo, y_hi),
            tickformat    = ",.0f",
            gridcolor     = "#ededed",
            zerolinecolor = "#a3a3a3", zerolinewidth = 1.5
          ),
          plot_bgcolor  = "#ffffff",
          paper_bgcolor = "#ffffff",
          margin = list(l = 70, r = 20, b = 55, t = 10),
          legend = list(orientation = "h", y = -0.28, font = list(size = 11)),
          font   = list(family = "Archivo, system-ui, sans-serif",
                        size = 12, color = "#0a0a0a")
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # ── CEAC ─────────────────────────────────────────────────────────────
    output$ceac_plot <- plotly::renderPlotly({
      req(psa_results(), parameters())
      psa <- psa_results()
      thr <- parameters()$threshold
      ol  <- parameters()$outcome_label %||% "unit"

      strats  <- psa$strategies
      n_strat <- length(strats)
      cost_m  <- as.matrix(psa$cost)
      eff_m   <- as.matrix(psa$effectiveness)

      # WTP range: 0 to 3× the highest selected threshold, 200 points
      thr_vec <- parameters()$thresholds %||% c(thr)
      wtp_seq <- seq(0, max(thr_vec, na.rm = TRUE) * 3, length.out = 200)

      # For each WTP: proportion of iterations each strategy has maximum NMB
      ceac_mat <- t(vapply(wtp_seq, function(lambda) {
        nmb  <- eff_m * lambda - cost_m       # n_iter × n_strat
        best <- max.col(nmb, ties.method = "first")
        vapply(seq_len(n_strat), function(j) mean(best == j), numeric(1L))
      }, numeric(n_strat)))
      # ceac_mat: length(wtp_seq) × n_strat

      palette <- c("#27AAE1", "#FE7501", "#084887", "#EFCA08", "#497048", "#006D77", "#8E44AD", "#D11060")
      p <- plotly::plot_ly()

      fp <- focal_pair()
      focal_j <- if (!is.null(fp)) match(make.names(fp$focal$Strategy), strats) else NA_integer_

      for (j in seq_len(n_strat)) {
        col      <- palette[((j - 1L) %% length(palette)) + 1L]
        is_focal <- !is.na(focal_j) && j == focal_j
        p <- p |>
          plotly::add_lines(
            x    = wtp_seq,
            y    = ceac_mat[, j],
            name = if (is_focal) paste0(strats[j], " (focal)") else strats[j],
            line = list(color = col, width = if (is_focal) 3 else 1.5),
            hovertemplate = paste0(
              "<b>", strats[j], "</b><br>",
              "WTP: KES %{x:,.0f}<br>",
              "P(optimal): %{y:.1%}<extra></extra>"
            )
          )
      }

      # Vertical reference lines for each selected threshold
      thr_cols <- c("#dc2626", "#ea580c", "#7c3aed", "#0369a1")
      for (.k in seq_along(thr_vec)) {
        p <- p |>
          plotly::add_lines(
            x    = c(thr_vec[.k], thr_vec[.k]),
            y    = c(0, 1),
            name = paste0(names(thr_vec)[.k], " — KES ", format(thr_vec[.k], big.mark = ",")),
            line = list(
              color = thr_cols[((.k - 1L) %% 4L) + 1L],
              width = 1.5, dash = "dash"
            ),
            hovertemplate = paste0(
              "Threshold: KES ", format(thr_vec[.k], big.mark = ","), "<extra></extra>"
            )
          )
      }

      p |>
        plotly::layout(
          xaxis = list(
            title         = paste0("Willingness-to-pay (KES per ", ol, ")"),
            tickformat    = ",.0f",
            gridcolor     = "#ededed",
            zerolinecolor = "#d4d4d4"
          ),
          yaxis = list(
            title      = "Probability cost-effective",
            range      = c(0, 1),
            tickformat = ".0%",
            gridcolor  = "#ededed"
          ),
          plot_bgcolor  = "#ffffff",
          paper_bgcolor = "#ffffff",
          margin = list(l = 70, r = 20, b = 60, t = 10),
          legend = list(orientation = "h", y = -0.28, font = list(size = 11)),
          font   = list(family = "Archivo, system-ui, sans-serif",
                        size = 12, color = "#0a0a0a")
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # ── Price Threshold: threshold selector ──────────────────────────────
    output$pt_threshold_selector <- renderUI({
      req(parameters())
      tv <- parameters()$thresholds
      if (length(tv) <= 1L) return(NULL)
      div(style = "margin-bottom:10px;",
        selectInput(
          session$ns("pt_threshold"),
          label  = tags$span(
            style = "font-size:11px; font-weight:700; text-transform:uppercase;
                     letter-spacing:0.05em; color:#737373;",
            "Compare against threshold"
          ),
          choices  = setNames(as.character(tv), paste0(names(tv), " — KES ", format(tv, big.mark = ","))),
          selected = as.character(min(tv)),
          width    = "100%"
        )
      )
    })

    pt_thr <- reactive({
      tv  <- parameters()$thresholds %||% parameters()$threshold
      sel <- suppressWarnings(as.numeric(input$pt_threshold))
      if (length(sel) == 1L && is.finite(sel) && sel %in% tv) sel else min(tv, na.rm = TRUE)
    })

    # ── Price Threshold: subtitle ─────────────────────────────────────────
    output$pt_subtitle <- renderUI({
      req(parameters())
      thr <- pt_thr()
      ol  <- parameters()$outcome_label %||% "unit"
      tags$span(
        style = "font-size:12px; color:#737373; margin-left:8px; font-weight:400;",
        paste0("λ = KES ", format(thr, big.mark = ","), " per ", ol)
      )
    })

    # ── Price Threshold: chart ────────────────────────────────────────────
    output$pt_chart <- plotly::renderPlotly({
      req(results(), parameters())
      r   <- as.data.frame(results())
      thr <- pt_thr()
      ol  <- parameters()$outcome_label %||% "unit"

      # Sort by cost; reference is cheapest
      r   <- r[order(r$Cost), ]
      ref <- r[1L, ]
      has_status <- "Status" %in% names(r)

      non_ref <- r[seq(2L, nrow(r)), ]
      non_ref <- non_ref[is.finite(non_ref$Cost) & is.finite(non_ref$Effect), ]
      if (has_status) non_ref <- non_ref[!(toupper(non_ref$Status) %in% c("D", "ED")), ]

      if (nrow(non_ref) == 0L) {
        return(plotly::plot_ly() |>
          plotly::layout(annotations = list(list(
            text = "No strategies to plot", showarrow = FALSE,
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            font = list(size = 13, color = "#737373")
          )), paper_bgcolor = "#fff", plot_bgcolor = "#fff"))
      }

      palette <- c("#27AAE1", "#FE7501", "#084887", "#EFCA08", "#497048", "#006D77", "#8E44AD", "#D11060")
      p <- plotly::plot_ly()

      fp <- pt_focal_pair()
      focal_name <- if (!is.null(fp)) fp$focal$Strategy else NA_character_

      x_min <- min(non_ref$Cost) * 0.1

      # x_max must cover the break-even price for every strategy
      break_evens <- vapply(seq_len(nrow(non_ref)), function(i) {
        inc_e <- non_ref[i, ]$Effect - ref$Effect
        if (!is.finite(inc_e) || inc_e <= 0) return(non_ref[i, ]$Cost)
        ref$Cost + thr * inc_e
      }, numeric(1L))
      x_max <- max(max(non_ref$Cost) * 3.0, max(break_evens, na.rm = TRUE)) * 1.15

      for (i in seq_len(nrow(non_ref))) {
        s     <- non_ref[i, ]
        inc_e <- s$Effect - ref$Effect
        if (!is.finite(inc_e) || inc_e <= 0) next

        is_focal <- identical(s$Strategy, focal_name)

        prices <- seq(x_min, x_max, length.out = 100)
        icers  <- (prices - ref$Cost) / inc_e
        # Marker sits on this strategy's own curve (vs-reference ICER at its
        # current price) — not dampack's frontier-relative ICER, which for
        # non-focal strategies would place it off the curve.
        icer_now <- (s$Cost - ref$Cost) / inc_e
        col    <- palette[((i - 1L) %% length(palette)) + 1L]

        p <- p |>
          plotly::add_lines(
            x         = prices,
            y         = icers,
            name      = if (is_focal) paste0(s$Strategy, " (focal)") else s$Strategy,
            line      = list(color = col, width = if (is_focal) 3 else 1.5),
            hovertemplate = paste0(
              s$Strategy, "<br>Price: KES %{x:,.0f}<br>ICER: KES %{y:,.0f}<extra></extra>"
            )
          ) |>
          plotly::add_markers(
            x         = s$Cost,
            y         = icer_now,
            showlegend = FALSE,
            marker    = list(
              color  = if (is.finite(icer_now) && icer_now <= thr) "#047857" else "#dc2626",
              size   = if (is_focal) 10 else 7,
              symbol = "circle"
            ),
            hovertemplate = paste0(
              "Current: KES %{x:,.0f}<br>ICER: KES %{y:,.0f}<extra></extra>"
            )
          )
      }

      # CE threshold line
      p <- p |>
        plotly::add_lines(
          x         = c(x_min, x_max),
          y         = c(thr, thr),
          name      = "CE threshold",
          line      = list(color = "#dc2626", width = 1.5, dash = "dash"),
          hovertemplate = paste0("Threshold: KES ", format(thr, big.mark = ","), "<extra></extra>")
        )

      p |>
        plotly::layout(
          xaxis = list(
            title       = paste0("Cost of intervention (KES)"),
            tickformat  = ",.0f",
            gridcolor   = "#ededed",
            zerolinecolor = "#d4d4d4"
          ),
          yaxis = list(
            title       = paste0("ICER (KES per ", ol, ")"),
            tickformat  = ",.0f",
            gridcolor   = "#ededed",
            zerolinecolor = "#d4d4d4"
          ),
          plot_bgcolor  = "#ffffff",
          paper_bgcolor = "#ffffff",
          margin        = list(l = 70, r = 20, b = 55, t = 10),
          legend        = list(orientation = "h", y = -0.22, font = list(size = 11)),
          font          = list(family = "Archivo, system-ui, sans-serif", size = 12,
                               color  = "#0a0a0a")
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # ── Price Threshold: stat boxes ───────────────────────────────────────
    output$pt_stats <- renderUI({
      req(parameters())
      thr <- pt_thr()

      fp <- pt_focal_pair()
      if (is.null(fp) || !is.finite(fp$icer)) return(NULL)

      ref    <- fp$ref
      target <- fp$focal
      inc_e  <- fp$inc_effect
      icer   <- fp$icer

      break_even <- ref$Cost + thr * inc_e
      headroom   <- break_even - target$Cost

      fmt_k <- function(x) paste0("KES ", format(round(x), big.mark = ","))
      pos_col <- "#047857"
      neg_col <- "#b91c1c"

      div(class = "pt-grid",
        div(class = "pt-stat",
          div(class = "pt-val", fmt_k(target$Cost)),
          div(class = "pt-lbl", paste("Current price:", target$Strategy))
        ),
        div(class = "pt-stat",
          div(class = "pt-val",
              style = paste0("color:", if (break_even > target$Cost) pos_col else neg_col),
              fmt_k(break_even)),
          div(class = "pt-lbl", "Break-even price")
        ),
        div(class = "pt-stat",
          div(class = "pt-val", fmt_k(icer)),
          div(class = "pt-lbl", "Current ICER")
        ),
        div(class = "pt-stat",
          div(class = "pt-val",
              style = paste0("color:", if (headroom >= 0) pos_col else neg_col),
              fmt_k(abs(headroom))),
          div(class = "pt-lbl",
              if (headroom >= 0) "Cost headroom" else "Reduction needed")
        )
      )
    })

  })
}

# ── Helpers ────────────────────────────────────────────────────────────────────

.block_d_placeholder <- function(title, subtitle) {
  div(class = "plot-placeholder",
    p(class = "plot-placeholder-label", title),
    p(class = "plot-placeholder-sub",   subtitle)
  )
}
