# mod_results.R
# Slide-in results drawer.
# Tabs: ICER Table | CE Plane | Tornado | Price Threshold

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
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(ns("tornado_plot"), height = "380px"),
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
      r   <- as.data.frame(results())
      thr <- parameters()$threshold
      ol  <- parameters()$outcome_label %||% "unit"

      rows <- r[order(r$Cost), ]
      has_status <- "Status" %in% names(rows)

      lines <- lapply(seq_len(nrow(rows)), function(i) {
        s <- rows[i, ]
        st <- if (has_status) toupper(s$Status) else ""
        if (is.na(s$ICER) && (is.na(s$Inc_Cost) || is.na(s$Inc_Effect))) return(NULL)
        if (st == "D" || st == "ED" || (!is.finite(s$ICER) && is.finite(s$Inc_Cost) && s$Inc_Cost > 0 && s$Inc_Effect <= 0))
          return(tags$div(paste0("• ", s$Strategy, " — dominated (more costly, less effective).")))
        if (is.finite(s$ICER)) {
          ce <- s$ICER <= thr
          return(tags$div(
            paste0("• ", s$Strategy, " — ",
                   if (ce) "cost-effective" else "not cost-effective",
                   " at KES ", format(round(s$ICER), big.mark = ","),
                   " per ", ol, ".")))
        }
        NULL
      })
      lines <- Filter(Negate(is.null), lines)

      has_ce <- any(is.finite(rows$ICER) & rows$ICER <= thr)
      cls <- if (has_ce) "interp-block ok" else "interp-block warn"

      div(class = cls,
        do.call(tagList, lines),
        if (length(lines) > 0)
          tags$div(style = "margin-top:10px; font-weight:700;",
            if (has_ce) "At least one strategy is cost-effective at this threshold."
            else "No strategy is cost-effective at this threshold.")
      )
    })

    # ── CE Plane (deterministic + PSA cloud) ─────────────────────────────
    output$ce_plane_plot <- plotly::renderPlotly({
      req(results(), parameters())
      r   <- as.data.frame(results())
      thr <- parameters()$threshold
      ol  <- parameters()$outcome_label %||% "unit"

      r$Inc_Effect[is.na(r$Inc_Effect)] <- 0
      r$Inc_Cost[is.na(r$Inc_Cost)]     <- 0

      ref     <- r[1L, ]
      non_ref <- r[-1L, , drop = FALSE]
      palette <- c("#27AAE1", "#FE7501", "#084887", "#EFCA08", "#497048", "#006D77", "#8E44AD", "#D11060")

      psa      <- psa_results()
      has_psa  <- !is.null(psa)
      cost_m   <- if (has_psa) as.matrix(psa$cost)         else NULL
      eff_m    <- if (has_psa) as.matrix(psa$effectiveness) else NULL
      # dampack applies make.names() to strategy names internally
      ref_j    <- if (has_psa) match(make.names(ref$Strategy), psa$strategies) else NA_integer_
      if (is.na(ref_j)) ref_j <- 1L

      # X range — incorporate PSA spread when available
      x_vals <- c(0, non_ref$Inc_Effect)
      if (has_psa && !is.na(ref_j)) {
        ref_e_psa <- eff_m[, ref_j]
        for (s_name in non_ref$Strategy) {
          s_j <- match(make.names(s_name), psa$strategies)
          if (!is.na(s_j)) x_vals <- c(x_vals, eff_m[, s_j] - ref_e_psa)
        }
      }
      x_pad <- max(diff(range(x_vals, na.rm = TRUE)) * 0.15, 1)
      x_lo  <- min(x_vals, na.rm = TRUE) - x_pad
      x_hi  <- max(x_vals, na.rm = TRUE) + x_pad

      p <- plotly::plot_ly()

      # PSA cloud (drawn first so deterministic points sit on top)
      if (has_psa) {
        ref_c_psa <- cost_m[, ref_j]
        ref_e_psa <- eff_m[,  ref_j]
        for (i in seq_len(nrow(non_ref))) {
          s_name <- non_ref[i, "Strategy"]
          s_j    <- match(make.names(s_name), psa$strategies)
          if (is.na(s_j)) next
          col    <- palette[((i - 1L) %% length(palette)) + 1L]
          inc_c  <- cost_m[, s_j] - ref_c_psa
          inc_e  <- eff_m[,  s_j] - ref_e_psa
          p <- p |>
            plotly::add_markers(
              x           = inc_e,
              y           = inc_c,
              legendgroup = s_name,
              showlegend  = FALSE,
              marker      = list(color = col, size = 3, opacity = 0.15, symbol = "circle"),
              hoverinfo   = "none"
            )
        }
      }

      # CE threshold line
      p <- p |>
        plotly::add_lines(
          x    = c(x_lo, x_hi),
          y    = thr * c(x_lo, x_hi),
          name = paste0("CE threshold (KES ", format(thr, big.mark = ","), " / ", ol, ")"),
          line = list(color = "#dc2626", width = 1.5, dash = "dash"),
          hovertemplate = "CE threshold<extra></extra>"
        )

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
        icer_txt <- if (is.finite(s$ICER))
          paste0("KES ", format(round(s$ICER), big.mark = ",")) else "—"
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
      thr <- parameters()$threshold
      ol  <- parameters()$outcome_label %||% "unit"

      r <- r[order(r$Cost), ]
      ref     <- r[1L, ]
      non_ref <- r[-1L, , drop = FALSE]

      # Focal strategy: first non-reference with a finite ICER
      fin <- non_ref[is.finite(non_ref$ICER), ]
      focal <- if (nrow(fin) > 0L) fin[1L, ] else non_ref[1L, ]

      .empty <- function(msg)
        plotly::plot_ly() |>
          plotly::layout(
            annotations  = list(list(text = msg, showarrow = FALSE,
              x = 0.5, y = 0.5, xref = "paper", yref = "paper",
              font = list(size = 13, color = "#737373"))),
            paper_bgcolor = "#fff", plot_bgcolor = "#fff"
          )

      if (nrow(focal) == 0L || is.na(focal$Strategy))
        return(.empty("No valid comparison available."))

      base_icer  <- if (is.finite(focal$ICER)) focal$ICER else
                      (focal$Cost - ref$Cost) / (focal$Effect - ref$Effect)
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
                 text = "Threshold", showarrow = FALSE,
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

      # WTP range: 0 to 3× threshold, 200 points
      wtp_seq <- seq(0, thr * 3, length.out = 200)

      # For each WTP: proportion of iterations each strategy has maximum NMB
      ceac_mat <- t(vapply(wtp_seq, function(lambda) {
        nmb  <- eff_m * lambda - cost_m       # n_iter × n_strat
        best <- max.col(nmb, ties.method = "first")
        vapply(seq_len(n_strat), function(j) mean(best == j), numeric(1L))
      }, numeric(n_strat)))
      # ceac_mat: length(wtp_seq) × n_strat

      palette <- c("#27AAE1", "#FE7501", "#084887", "#EFCA08", "#497048", "#006D77", "#8E44AD", "#D11060")
      p <- plotly::plot_ly()

      for (j in seq_len(n_strat)) {
        col <- palette[((j - 1L) %% length(palette)) + 1L]
        p <- p |>
          plotly::add_lines(
            x    = wtp_seq,
            y    = ceac_mat[, j],
            name = strats[j],
            line = list(color = col, width = 2),
            hovertemplate = paste0(
              "<b>", strats[j], "</b><br>",
              "WTP: KES %{x:,.0f}<br>",
              "P(optimal): %{y:.1%}<extra></extra>"
            )
          )
      }

      # Vertical line at current threshold
      p <- p |>
        plotly::add_lines(
          x    = c(thr, thr),
          y    = c(0, 1),
          name = "Current threshold",
          line = list(color = "#dc2626", width = 1.5, dash = "dash"),
          hovertemplate = paste0(
            "Threshold: KES ", format(thr, big.mark = ","), "<extra></extra>"
          )
        )

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

    # ── Price Threshold: subtitle ─────────────────────────────────────────
    output$pt_subtitle <- renderUI({
      req(parameters())
      thr <- parameters()$threshold
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
      thr <- parameters()$threshold
      ol  <- parameters()$outcome_label %||% "unit"

      # Sort by cost; reference is cheapest
      r   <- r[order(r$Cost), ]
      ref <- r[1L, ]

      non_ref <- r[seq(2L, nrow(r)), ]
      non_ref <- non_ref[is.finite(non_ref$Cost) & is.finite(non_ref$Effect), ]

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

        prices <- seq(x_min, x_max, length.out = 100)
        icers  <- (prices - ref$Cost) / inc_e
        col    <- palette[((i - 1L) %% length(palette)) + 1L]

        p <- p |>
          plotly::add_lines(
            x         = prices,
            y         = icers,
            name      = s$Strategy,
            line      = list(color = col, width = 2),
            hovertemplate = paste0(
              s$Strategy, "<br>Price: KES %{x:,.0f}<br>ICER: KES %{y:,.0f}<extra></extra>"
            )
          ) |>
          plotly::add_markers(
            x         = s$Cost,
            y         = s$ICER,
            showlegend = FALSE,
            marker    = list(
              color  = if (is.finite(s$ICER) && s$ICER <= thr) "#047857" else "#dc2626",
              size   = 9,
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
      req(results(), parameters())
      r   <- as.data.frame(results())
      thr <- parameters()$threshold

      r      <- r[order(r$Cost), ]
      ref    <- r[1L, ]
      target <- r[r$Cost > ref$Cost & is.finite(r$ICER), ][1L, ]

      if (is.na(target$Strategy)) return(NULL)

      inc_e      <- target$Effect - ref$Effect
      if (!is.finite(inc_e) || inc_e == 0) return(NULL)

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
          div(class = "pt-val", fmt_k(target$ICER)),
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
