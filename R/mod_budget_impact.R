# mod_budget_impact.R
# Budget Impact tab — Reference vs New scenario over an editable multi-year
# horizon, built on the per-strategy costs from the Analysis tab.

mod_budget_impact_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(tags$style(HTML("
      .bia-wrap { max-width: 1200px; margin: 0 auto; padding: 24px; }

      .bia-pg-hdr {
        padding: 20px 0 16px; border-bottom: 1px solid #e5e5e5; margin-bottom: 20px;
      }
      .bia-pg-hdr h1 { font-size: 22px; font-weight: 700; margin: 0 0 4px; }
      .bia-pg-hdr p  { font-size: 14px; color: #737373; margin: 0; }

      .bia-card-hdr {
        display: flex; align-items: center; justify-content: space-between;
        padding: 11px 15px; border-bottom: 1px solid #e5e5e5;
        background: #fff; border-radius: 4px 4px 0 0;
      }
      .bia-card-hdr-lbl {
        font-size: 11px; font-weight: 700; color: #737373;
        text-transform: uppercase; letter-spacing: 0.05em;
      }

      .bia-fld-lbl {
        font-size: 11px; font-weight: 700; color: #737373;
        text-transform: uppercase; letter-spacing: 0.05em;
        margin: 12px 0 4px; display: block;
      }
      .bia-fld-lbl:first-child { margin-top: 0; }
      .bia-helper-note { font-size: 12px; color: #a3a3a3; margin: 4px 0 0; }
      .bia-save-status { font-size: 11px; color: #497048; margin: 6px 0 0; }

      .bia-grid {
        display: grid; grid-template-columns: repeat(auto-fit, minmax(120px, 1fr));
        gap: 12px; margin-bottom: 16px;
      }
      .bia-stat { border: 1px solid #e5e5e5; border-radius: 4px; padding: 13px; text-align: center; }
      .bia-val  { font-family: 'IBM Plex Mono', monospace; font-size: 16px; font-weight: 600; }
      .bia-lbl  { font-size: 10px; color: #737373; margin-top: 4px;
                  text-transform: uppercase; letter-spacing: 0.05em; }
      .bia-chart-wrap { height: 320px; }

      .bia-section { margin-top: 16px; }
    "))),

    div(class = "bia-wrap",

      div(class = "bia-pg-hdr",
        tags$h1("Budget Impact"),
        tags$p("Estimate the budget required to deliver each strategy to the target population over time, based on the per-strategy costs from the Analysis tab.")
      ),

      uiOutput(ns("empty_state")),
      uiOutput(ns("body_ui"))
    )
  )
}

#' @param id              Module namespace ID
#' @param results         Reactive returning the dampack ICER results data frame (or NULL)
#' @param strategies_data Reactive returning the strategies df (strategy, cost, effect, source, n_studies)
mod_budget_impact_server <- function(id, results, strategies_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ramp <- reactiveValues(shares = list())

    base_df <- reactive({
      r <- results()
      req(r)
      d <- as.data.frame(r)
      df <- data.frame(strategy = d$Strategy, cost = d$Cost, stringsAsFactors = FALSE)
      df[order(df$cost), ]
    })

    # ── Empty state ──────────────────────────────────────────────────────────
    output$empty_state <- renderUI({
      if (!is.null(results())) return(NULL)
      div(class = "alert alert-warning", style = "margin-top: 8px;",
        "Run an analysis on the Analysis tab first — Budget Impact uses its per-strategy costs."
      )
    })

    # ── Uptake ramp (wide df: strategy, year_1, year_2, ...) ────────────────
    ramp_df <- reactive({
      bd <- base_df()
      non_ref <- bd$strategy[-1]
      n_years <- input$time_horizon_years %||% 2
      if (length(non_ref) == 0L || n_years < 1L) {
        return(data.frame(strategy = character(0)))
      }
      cols <- lapply(seq_len(n_years), function(y) {
        vapply(non_ref, function(s) {
          v <- ramp$shares[[paste0(s, "_year_", y)]]
          if (is.null(v) || is.na(v)) 1 else v
        }, numeric(1))
      })
      names(cols) <- paste0("year_", seq_len(n_years))
      do.call(data.frame, c(list(strategy = non_ref, stringsAsFactors = FALSE), cols))
    })

    bia_computed <- reactive({
      req(input$target_population, input$time_horizon_years)
      compute_budget_impact_data(
        base_df(), input$target_population, ramp_df(), input$time_horizon_years
      )
    })

    # ── Body (everything that needs a completed analysis) ───────────────────
    output$body_ui <- renderUI({
      req(results())

      tagList(
        fluidRow(
          column(4,
            div(class = "card",
              div(class = "bia-card-hdr", span("Inputs", class = "bia-card-hdr-lbl")),
              div(class = "card-body",

                tags$label("Time horizon (years)", class = "bia-fld-lbl"),
                numericInput(ns("time_horizon_years"), NULL, value = 2, min = 1, max = 5, step = 1, width = "100%"),

                tags$label("Target population", class = "bia-fld-lbl"),
                numericInput(ns("target_population"), NULL, value = 0, min = 0, step = 1000, width = "100%"),
                tags$p(class = "bia-helper-note",
                  "Manual for now — target population isn't yet produced by this tool. It will be sourced from the epi team's outputs once that data handoff is defined."),

                hr(),

                tags$label("Run label", class = "bia-fld-lbl"),
                textInput(ns("run_label"), NULL, placeholder = "e.g. Mass vaccination - 2026 Q2", width = "100%"),
                actionButton(ns("save_run"), "Save scenario",
                  class = "btn btn-primary", style = "width: 100%; margin-top: 4px;"),
                uiOutput(ns("save_status"))
              )
            )
          ),

          column(8,
            div(class = "card",
              div(class = "bia-card-hdr", span("Uptake by year", class = "bia-card-hdr-lbl")),
              div(class = "card-body p-0",
                shinycssloaders::withSpinner(
                  DT::dataTableOutput(ns("ramp_table")),
                  type = 4, color = "#27AAE1", size = 0.6
                )
              ),
              uiOutput(ns("ramp_warning"))
            )
          )
        ),

        div(class = "card bia-section",
          div(class = "bia-card-hdr", span("Results", class = "bia-card-hdr-lbl")),
          div(class = "card-body",
            uiOutput(ns("bia_stats")),
            div(class = "bia-chart-wrap",
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(ns("bia_chart"), height = "300px"),
                type = 4, color = "#27AAE1", size = 0.6
              )
            ),
            div(style = "margin-top: 16px;",
              DT::dataTableOutput(ns("by_year_table"))
            )
          )
        ),

        div(class = "card bia-section",
          div(class = "bia-card-hdr", span("Saved runs", class = "bia-card-hdr-lbl")),
          div(class = "card-body p-0",
            DT::dataTableOutput(ns("saved_runs_table"))
          )
        )
      )
    })

    # ── Uptake ramp table ────────────────────────────────────────────────────
    # ramp_redraw forces output$ramp_table to re-render after a rejected edit
    # (e.g. to the reference row), snapping the displayed value back to the
    # true computed residual — without this, DT's own JS leaves whatever the
    # user typed sitting in the cell even though it was never actually used.
    ramp_redraw <- reactiveVal(0L)

    output$ramp_table <- DT::renderDataTable({
      ramp_redraw()
      bd <- base_df()
      req(nrow(bd) > 0L)
      rd <- ramp_df()
      n_years <- input$time_horizon_years %||% 2

      year_cols <- lapply(seq_len(n_years), function(y) {
        ycol <- paste0("year_", y)
        non_ref_vals <- if (nrow(rd) > 0L) rd[[ycol]] else numeric(0)
        ref_val <- 1 - sum(non_ref_vals)
        c(ref_val, non_ref_vals) * 100
      })
      names(year_cols) <- paste0("Year ", seq_len(n_years), " (%)")

      display <- data.frame(
        Strategy = bd$strategy,
        `Cost per case (KES)` = bd$cost,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      for (nm in names(year_cols)) display[[nm]] <- year_cols[[nm]]

      dt <- DT::datatable(
        display,
        editable = list(target = "cell", disable = list(columns = c(0L, 1L))),
        options = list(
          dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE,
          rowCallback = DT::JS(
            "function(row, data, index) {",
            "  if (index === 0) {",
            "    $(row).css({'font-weight':'600','background':'#fafafa'});",
            "    $(row).find('td').slice(2).css({",
            "      'pointer-events':'none','cursor':'default','opacity':'0.65'",
            "    });",
            "  }",
            "}"
          )
        ),
        rownames = FALSE, class = "table table-sm"
      )
      dt <- DT::formatCurrency(dt, "Cost per case (KES)", currency = "KES ", digits = 0)
      DT::formatRound(dt, names(year_cols), digits = 0L)
    }, server = FALSE)

    observeEvent(input$ramp_table_cell_edit, {
      info <- input$ramp_table_cell_edit
      row <- info$row
      col <- info$col
      if (col < 2L) return()    # Strategy / Cost per case columns are read-only
      if (row == 1L) {
        ramp_redraw(ramp_redraw() + 1L)   # reference row's share is a computed residual — snap back
        return()
      }

      bd <- base_df()
      strategy <- bd$strategy[row]
      year <- col - 1L
      v <- suppressWarnings(as.numeric(info$value))
      if (!is.finite(v) || v < 0) v <- 0
      ramp$shares[[paste0(strategy, "_year_", year)]] <- v / 100
    })

    output$ramp_warning <- renderUI({
      rd <- ramp_df()
      n_years <- input$time_horizon_years %||% 2
      if (nrow(rd) == 0L) return(NULL)

      over_years <- Filter(function(y) sum(rd[[paste0("year_", y)]]) > 1,
                            seq_len(n_years))
      if (length(over_years) == 0L) return(NULL)

      div(class = "alert alert-warning",
        style = "margin: 0 15px 12px; padding: 6px 10px; font-size: 12px;",
        paste0(
          "Year", if (length(over_years) > 1L) "s " else " ",
          paste(over_years, collapse = ", "),
          ": uptake shares sum to more than 100% — the reference strategy's residual share would be negative. Adjust the values above if that's unintended."
        )
      )
    })

    # ── Results: stat boxes, chart, by-year table ───────────────────────────
    output$bia_stats <- renderUI({
      bc <- bia_computed()
      fmt_k <- function(x) paste0("KES ", format(round(x), big.mark = ","))
      by_year <- bc$by_year

      year_boxes <- lapply(seq_len(nrow(by_year)), function(i) {
        div(class = "bia-stat",
          div(class = "bia-val", fmt_k(by_year$budget_impact[i])),
          div(class = "bia-lbl", paste("Year", by_year$year[i], "impact"))
        )
      })

      div(class = "bia-grid",
        year_boxes,
        div(class = "bia-stat",
          div(class = "bia-val", fmt_k(sum(by_year$budget_impact))),
          div(class = "bia-lbl", "Cumulative impact")
        )
      )
    })

    output$bia_chart <- plotly::renderPlotly({
      by_year <- bia_computed()$by_year
      req(nrow(by_year) > 0L)

      plotly::plot_ly() |>
        plotly::add_bars(
          x = paste0("Year ", by_year$year), y = by_year$reference_total,
          name = "Reference", marker = list(color = "#737373")
        ) |>
        plotly::add_bars(
          x = paste0("Year ", by_year$year), y = by_year$new_total,
          name = "New", marker = list(color = "#27AAE1")
        ) |>
        plotly::layout(
          barmode = "group",
          xaxis  = list(title = ""),
          yaxis  = list(title = "Total cost (KES)", tickformat = ",.0f"),
          plot_bgcolor  = "#ffffff",
          paper_bgcolor = "#ffffff",
          legend = list(orientation = "h", y = -0.2, font = list(size = 11)),
          margin = list(l = 70, r = 20, b = 40, t = 20),
          font   = list(family = "Archivo, system-ui, sans-serif", size = 12, color = "#0a0a0a")
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    output$by_year_table <- DT::renderDataTable({
      by_year <- bia_computed()$by_year
      req(nrow(by_year) > 0L)

      display <- data.frame(
        Year = by_year$year,
        `Reference total (KES)` = by_year$reference_total,
        `New total (KES)` = by_year$new_total,
        `Budget impact (KES)` = by_year$budget_impact,
        `Cumulative impact (KES)` = by_year$cumulative_budget_impact,
        check.names = FALSE, stringsAsFactors = FALSE
      )

      dt <- DT::datatable(
        display,
        options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE),
        rownames = FALSE, class = "table table-sm"
      )
      DT::formatCurrency(dt, names(display)[-1], currency = "KES ", digits = 0)
    }, server = FALSE)

    # ── Save scenario ────────────────────────────────────────────────────────
    last_saved <- reactiveVal(NULL)

    observeEvent(input$save_run, {
      bc <- bia_computed()
      rows <- bc$by_strategy_year
      req(nrow(rows) > 0L)

      sd <- strategies_data()
      cost_source <- sd$source[match(rows$strategy, sd$strategy)]
      cost_source[is.na(cost_source)] <- "manual"

      rows_df <- data.frame(
        run_label           = input$run_label %||% "",
        year                = rows$year,
        strategy            = rows$strategy,
        is_reference        = rows$is_reference,
        cost_per_case       = rows$cost_per_case,
        cost_source         = cost_source,
        target_population   = input$target_population,
        uptake_share         = rows$share,
        reference_total      = NA_real_,
        new_total            = NA_real_,
        budget_impact         = NA_real_,
        cumulative_budget_impact = NA_real_,
        time_horizon_years   = bc$time_horizon_years,
        outcome_type         = NA_character_,
        submitted_by         = "",
        stringsAsFactors = FALSE
      )
      by_year_lookup <- bc$by_year[match(rows$year, bc$by_year$year), ]
      rows_df$reference_total          <- by_year_lookup$reference_total
      rows_df$new_total                <- by_year_lookup$new_total
      rows_df$budget_impact            <- by_year_lookup$budget_impact
      rows_df$cumulative_budget_impact <- by_year_lookup$cumulative_budget_impact

      ok <- gs_write_bia_run(rows_df)
      if (!ok) {
        showNotification(
          "Could not save the Budget Impact run — check the connection and try again.",
          type = "warning", duration = 6
        )
        return()
      }

      last_saved(Sys.time())
      updateTextInput(session, "run_label", value = "")
      saved_runs_refresh(saved_runs_refresh() + 1L)
    })

    output$save_status <- renderUI({
      ts <- last_saved()
      if (is.null(ts)) return(NULL)
      div(class = "bia-save-status", paste0("Saved ✓ ", format(ts, "%H:%M:%S")))
    })

    # ── Saved runs table ─────────────────────────────────────────────────────
    saved_runs_refresh <- reactiveVal(0L)

    output$saved_runs_table <- DT::renderDataTable({
      saved_runs_refresh()
      d <- gs_read_bia_runs()
      if (nrow(d) == 0L) {
        return(DT::datatable(
          data.frame(Message = "No saved runs yet."),
          options = list(dom = "t"), rownames = FALSE, colnames = ""
        ))
      }

      # One row per run_id: the final year's cumulative_budget_impact is the
      # run's total — take the max-year row rather than aggregating, since
      # cumulative impact need not be monotonic (e.g. cost-saving strategies).
      d <- d[order(d$run_id, -d$year), ]
      agg <- d[!duplicated(d$run_id), c("run_id", "run_label", "submitted_at", "cumulative_budget_impact")]
      agg <- agg[order(agg$submitted_at, decreasing = TRUE), ]

      display <- data.frame(
        `Run label` = agg$run_label,
        `Cumulative impact (KES)` = agg$cumulative_budget_impact,
        `Saved at` = agg$submitted_at,
        check.names = FALSE, stringsAsFactors = FALSE
      )

      dt <- DT::datatable(
        display,
        options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE),
        rownames = FALSE, class = "table table-sm"
      )
      DT::formatCurrency(dt, "Cumulative impact (KES)", currency = "KES ", digits = 0)
    }, server = FALSE)
  })
}
