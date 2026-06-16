# mod_synthesis.R
# Evidence Synthesis module — intervention-scoped view of study data.
# Data source: Google Sheets (via gs_backend.R).
# Synthesis math: synth_functions.R (unchanged).

TARGET_YEAR <- 2027L  # keep in sync with fetch_factors.R

mod_synthesis_ui <- function(id) {
  ns <- NS(id)

  tagList(
    mod_study_entry_ui(ns("entry")),   # hidden download button anchor

    tags$head(tags$style(HTML("
      .synth-wrap { max-width: 1480px; margin: 0 auto; padding: 24px; }

      /* Intervention selector bar */
      .synth-intv-bar {
        display: flex; align-items: center; gap: 16px;
        margin-bottom: 20px;
      }
      .synth-intv-bar .form-group { margin: 0; flex: 1; }
      .synth-intv-bar .form-group label { display: none; }

      .synth-card { border: 1px solid #e5e5e5; border-radius: 4px; }
      .synth-card-header {
        display: flex; align-items: center; justify-content: space-between;
        padding: 11px 15px; border-bottom: 1px solid #e5e5e5;
        font-size: 13px; font-weight: 600;
        background: #fff; border-radius: 4px 4px 0 0;
      }
      .synth-card-actions { display: flex; align-items: center; gap: 8px; }
      .synth-pool-ctrl { display: flex; align-items: center; gap: 8px; }
      .synth-pool-ctrl .shiny-input-container { margin: 0; }
      .synth-pool-ctrl .shiny-input-container > label { display: none; }
      .synth-pool-ctrl .radio-inline label {
        font-size: 12px; font-weight: 500; color: #404040; cursor: pointer;
      }
      .synth-pool-lbl { font-size: 12px; color: #737373; white-space: nowrap; }

      .synth-table { border-collapse: collapse; width: 100%; font-size: 13px; }
      .synth-th th {
        font-size: 10px; text-transform: uppercase; letter-spacing: 0.07em;
        color: #737373; font-weight: 600; padding: 8px 12px;
        border-bottom: 1.5px solid #0a0a0a; white-space: nowrap; background: #fff;
      }
      .synth-r { text-align: right !important; }
      .synth-chk {
        width: 28px !important; max-width: 28px !important;
        text-align: center !important; padding: 0 4px !important;
        vertical-align: middle !important;
      }
      .synth-chk .form-group         { margin-bottom: 0 !important; }
      .synth-chk .shiny-input-container { width: auto !important; }
      .synth-chk .checkbox           { margin: 0 !important; padding: 0 !important; }
      .synth-chk .checkbox label     { padding-left: 0 !important; min-height: 0 !important; }
      .synth-chk .shiny-input-container > label { display: none; }
      .synth-chk input[type='checkbox'] { margin: 0 !important; cursor: pointer; }
      .synth-excluded td { opacity: 0.35; }

      .synth-grp td {
        background: #fafafa; font-size: 11px; font-weight: 700;
        text-transform: uppercase; letter-spacing: 0.05em;
        color: #404040; padding: 6px 12px; border-top: 1px solid #e5e5e5;
      }
      .synth-study td {
        padding: 8px 12px; border-bottom: 1px solid #f0f0f0; vertical-align: top;
      }
      .synth-author  { font-weight: 500; }
      .synth-journal { font-size: 11px; color: #737373; }
      .synth-step    { background: #fcfcfc; }
      .synth-kes     { font-weight: 600; }
      .synth-scenario {
        display: inline-block; background: #f0f9ff; color: #0369a1;
        font-size: 10px; font-weight: 600; padding: 1px 5px; border-radius: 2px;
        text-transform: uppercase; letter-spacing: 0.04em; margin-top: 2px;
      }
      .synth-scenario.base { background: #dcfce7; color: #15803d; }

      .synth-pool-row td {
        padding: 9px 12px; border-top: 1.5px solid #27AAE1;
        border-bottom: 2px solid #e5e5e5; background: #f0f9ff;
      }
      .synth-pool-none td {
        padding: 9px 12px; border-top: 1px solid #e5e5e5;
        background: #fafafa; color: #a3a3a3; font-size: 12px; font-style: italic;
      }
      .synth-pool-lbl-cell { font-weight: 600; font-size: 13px; }
      .synth-range { font-size: 11px; color: #737373; }
      .synth-icer-flag {
        display: inline-block; background: #fffbeb; color: #b45309;
        border: 1px solid #fde68a; border-radius: 2px;
        padding: 1px 6px; font-size: 11px; margin-left: 6px;
      }
      .synth-empty {
        padding: 48px 24px; text-align: center; color: #737373;
        font-size: 13px;
      }
      .synth-empty-title { font-size: 15px; font-weight: 600; color: #404040; margin-bottom: 6px; }

      .synth-demo-banner {
        display: flex; align-items: center; gap: 10px;
        background: #fffbeb; border: 1px solid #fde68a;
        border-radius: 4px; padding: 8px 14px; margin-bottom: 12px;
        font-size: 12px; color: #92400e;
      }
      .synth-demo-banner strong { font-weight: 700; }
      .synth-scenario.demo { background: #fef3c7; color: #92400e; }

      .synth-footer {
        display: flex; align-items: center;
        justify-content: flex-end; margin-top: 16px; gap: 12px;
      }
      .btn-synth-send {
        background: #27AAE1 !important; color: #fff !important;
        border: none !important; border-radius: 4px !important;
        padding: 10px 20px !important; font-size: 13px !important;
        font-weight: 600 !important; white-space: nowrap;
      }
      .btn-synth-send:hover, .btn-synth-send:focus {
        background: #1c8ec0 !important; color: #fff !important;
      }
      .btn-synth-send:disabled {
        background: #d4d4d4 !important; cursor: not-allowed !important;
      }
    "))),

    div(class = "synth-wrap",

      # ── Intervention selector ────────────────────────────────────────────
      div(class = "synth-intv-bar",
        selectInput(ns("selected_intervention"), NULL,
          choices  = c("Select an intervention…" = ""),
          selected = "",
          width    = "100%"
        )
      ),

      # ── Studies card ─────────────────────────────────────────────────────
      div(class = "card synth-card",
        div(class = "synth-card-header",
          span("Study cost database — standardisation & pooling"),
          div(class = "synth-card-actions",
            div(class = "synth-pool-ctrl",
              span("Pooling:", class = "synth-pool-lbl"),
              radioButtons(ns("pool_method"), NULL,
                choices  = c("Simple mean" = "mean",
                             "Sample-weighted" = "weighted",
                             "Inverse-variance" = "ivw"),
                selected = "weighted", inline = TRUE
              )
            ),
            actionButton(ns("add_study_btn"),   "+ Add study",
                         class = "btn btn-sm btn-outline-secondary"),
            actionButton(ns("upload_csv_btn"),  "↑ Upload CSV",
                         class = "btn btn-sm btn-outline-secondary")
          )
        ),
        uiOutput(ns("study_table"))
      ),

      div(class = "synth-footer",
        actionButton(ns("send_btn"),
          label = uiOutput(ns("send_label"), inline = TRUE),
          class = "btn btn-synth-send")
      )
    )
  )
}

# ── Server ─────────────────────────────────────────────────────────────────────

mod_synthesis_server <- function(id, factors, interventions,
                                 write_enabled = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      sent_strategies = NULL,
      sent_prov       = list(),
      send_count      = 0L,
      included        = NULL    # named logical: record_id → TRUE/FALSE
    )

    # ── Populate intervention dropdown ──────────────────────────────────────
    observe({
      req(nrow(interventions) > 0L)
      choices <- c("Select an intervention…" = "",
                   setNames(interventions$intervention, interventions$intervention))
      updateSelectInput(session, "selected_intervention", choices = choices)
    })

    # ── Study entry sub-module ──────────────────────────────────────────────
    selected_pkg <- reactive({
      req(input$selected_intervention, nzchar(input$selected_intervention))
      hit <- interventions$benefit_package[
        interventions$intervention == input$selected_intervention]
      if (length(hit) > 0L) hit[1L] else ""
    })

    entry_out <- mod_study_entry_server(
      "entry",
      intervention    = reactive(input$selected_intervention),
      benefit_package = selected_pkg,
      add_trigger     = reactive(input$add_study_btn),
      upload_trigger  = reactive(input$upload_csv_btn),
      write_enabled   = write_enabled,
      factors         = factors
    )

    # ── Load studies from GS whenever intervention changes or study added ───
    studies_raw <- reactive({
      intv <- input$selected_intervention
      req(nzchar(intv))
      entry_out$entries_added()
      d <- gs_read_studies(intv)
      if (nrow(d) == 0L) gs_load_demo_studies(intv) else d
    })

    # Reset inclusion when studies data changes
    observeEvent(studies_raw(), {
      d <- studies_raw()
      if (nrow(d) == 0L) { rv$included <- NULL; return() }
      ids     <- d$record_id
      current <- rv$included
      # Preserve existing checked state; new rows default to TRUE
      rv$included <- setNames(
        vapply(ids, function(id) {
          if (!is.null(current) && id %in% names(current)) current[[id]] else TRUE
        }, logical(1L)),
        ids
      )
      # Create per-row checkbox observers
      lapply(ids, function(rid) {
        cb <- paste0("inc_", gsub("[^a-zA-Z0-9]", "_", rid))
        observeEvent(input[[cb]], {
          if (!is.null(rv$included) && rid %in% names(rv$included))
            rv$included[rid] <- isTRUE(input[[cb]])
        }, ignoreNULL = TRUE, ignoreInit = TRUE)
      })
    }, ignoreNULL = FALSE)

    included_studies <- reactive({
      d <- studies_raw()
      if (nrow(d) == 0L || is.null(rv$included)) return(d[0L, ])
      keep <- rv$included[d$record_id]
      keep[is.na(keep)] <- TRUE
      d[keep, ]
    })

    # ── Outcome measure per strategy (for propagation to analysis tab) ──────
    outcome_by_strategy <- reactive({
      d <- included_studies()
      if (nrow(d) == 0L || !"outcome_measure" %in% names(d)) return(list())
      strat_val <- ifelse(
        !is.na(d$strategy) & nzchar(trimws(as.character(d$strategy))),
        as.character(d$strategy),
        input$selected_intervention
      )
      strategies <- unique(strat_val)
      sapply(setNames(strategies, strategies), function(s) {
        oms <- unique(d$outcome_measure[strat_val == s & !is.na(d$outcome_measure)])
        if (length(oms) == 1L) oms else if (length(oms) == 0L) NA_character_ else "mixed"
      }, USE.NAMES = TRUE)
    })

    # ── Pooled data ─────────────────────────────────────────────────────────
    pooled_data <- reactive({
      d <- included_studies()
      if (nrow(d) == 0L) return(structure(list(), reference = NA_character_))

      # Build studies df compatible with synth_functions.R:
      # needs: id, strategy, author, year, currency, cost, effect, n
      # strategy falls back to the intervention name for rows without an explicit strategy
      strat_val <- ifelse(
        !is.na(d$strategy) & nzchar(trimws(as.character(d$strategy))),
        as.character(d$strategy),
        input$selected_intervention
      )
      stud <- data.frame(
        id       = d$record_id,
        strategy = strat_val,
        author   = d$authors,
        journal  = d$source_type,
        year     = as.integer(d$year),
        country  = d$country,
        currency = d$currency,
        cost     = as.numeric(d$cost),
        effect   = as.numeric(d$effect),
        n        = as.integer(d$n),
        stringsAsFactors = FALSE
      )
      # Use currency_year where provided, else publication year
      stud$year <- ifelse(!is.na(d$currency_year) & is.finite(as.numeric(d$currency_year)),
                          as.integer(d$currency_year), stud$year)
      stud <- stud[is.finite(stud$cost) & is.finite(stud$effect) &
                   is.finite(stud$n)    & stud$n >= 1L, ]
      if (nrow(stud) == 0L) return(structure(list(), reference = NA_character_))

      tryCatch(
        synth_pool_all(stud, factors, method = input$pool_method),
        error = function(e) { message("[synth] ", e$message); structure(list(), reference = NA_character_) }
      )
    })

    # ── Study table ─────────────────────────────────────────────────────────
    output$study_table <- renderUI({
      intv <- input$selected_intervention
      if (is.null(intv) || !nzchar(intv)) {
        return(div(class = "synth-empty",
          div(class = "synth-empty-title", "Select an intervention above"),
          "Studies for that intervention will appear here."
        ))
      }

      d <- studies_raw()
      if (nrow(d) == 0L) {
        return(div(class = "synth-empty",
          div(class = "synth-empty-title", "No studies yet"),
          "Click \"+ Add study\" or upload a CSV to get started."
        ))
      }

      pd        <- pooled_data()
      ref_strat <- attr(pd, "reference") %||% intv
      ppp_yr    <- factors$ppp$year
      is_demo   <- all(grepl("^DEMO-", d$record_id))

      demo_banner <- if (is_demo) {
        div(class = "synth-demo-banner",
          tags$span("⚠️"),
          tags$span(
            tags$strong("Sample data — arthroplasty CEA demonstration. "),
            "Upload studies for ", tags$em(intv),
            " to replace this with real evidence."
          )
        )
      } else NULL

      header <- tags$tr(class = "synth-th",
        tags$th(class = "synth-chk", ""),
        tags$th("Study"),
        tags$th("Country"),
        tags$th("Yr"),
        tags$th(class = "synth-r", "Reported cost"),
        tags$th(class = "synth-r", paste0("Step 1 (", ppp_yr, " prices)")),
        tags$th(class = "synth-r", paste0("KES ", ppp_yr, " (PPP)")),
        tags$th(class = "synth-r", paste0("KES ", TARGET_YEAR, " (PPP)")),
        tags$th(class = "synth-r", "Effect"),
        tags$th(class = "synth-r", "n"),
        tags$th(class = "synth-r", "ICER")
      )

      # Build per-study rows
      # We run standardisation here for display only (pooled_data has the math)
      body_rows    <- tagList()
      included_ids <- if (!is.null(rv$included)) names(rv$included[rv$included]) else d$record_id

      # Resolve strategy for each row; fall back to intervention name for legacy rows
      row_strats <- ifelse(
        !is.na(d$strategy) & nzchar(trimws(as.character(d$strategy))),
        as.character(d$strategy),
        intv
      )
      strats <- unique(row_strats)

      for (strat in strats) {
        strat_d <- d[row_strats == strat, , drop = FALSE]

        # Strategy group header
        body_rows <- tagAppendChild(body_rows,
          tags$tr(class = "synth-grp",
            tags$td(colspan = "11", toupper(strat))
          )
        )

        for (i in seq_len(nrow(strat_d))) {
          s         <- strat_d[i, ]
          rid       <- s$record_id
          cb_id     <- paste0("inc_", gsub("[^a-zA-Z0-9]", "_", rid))
          is_incl   <- isTRUE(rv$included[rid]) | is.na(rv$included[rid]) | is.null(rv$included[rid])
          row_class <- paste("synth-study", if (!is_incl) "synth-excluded" else "")

          s_df <- data.frame(
            id = rid, strategy = strat,
            author = s$authors, journal = s$source_type %||% "",
            year     = as.integer(s$year %||% NA),
            country  = as.character(s$country %||% ""),
            currency = as.character(s$currency %||% ""),
            cost     = as.numeric(s$cost %||% NA),
            effect   = as.numeric(s$effect %||% NA),
            n        = as.integer(s$n %||% NA),
            stringsAsFactors = FALSE
          )
          if (!is.na(s$currency_year) && nzchar(as.character(s$currency_year %||% "")))
            s_df$year <- as.integer(s$currency_year)

          std <- tryCatch(synth_standardize(s_df, factors), error = function(e) NULL)

          scen     <- s$scenario %||% ""
          scen_cls <- paste("synth-scenario", if (scen == "base_case") "base" else "")
          scen_lbl <- switch(scen,
            base_case             = "Base case",
            probabilistic         = "PSA",
            lower_bound           = "Lower",
            upper_bound           = "Upper",
            threshold_sensitivity = "Threshold",
            subgroup              = "Subgroup",
            other_sensitivity     = "Sensitivity",
            ""
          )

          row_is_demo <- grepl("^DEMO-", rid)
          body_rows <- tagAppendChild(body_rows,
            tags$tr(class = row_class,
              tags$td(class = "synth-chk",
                checkboxInput(ns(cb_id), NULL, value = is_incl)
              ),
              tags$td(
                div(s$authors %||% "—", class = "synth-author"),
                if (row_is_demo) div(class = "synth-scenario demo", "Sample"),
                if (nzchar(scen_lbl)) div(class = scen_cls, scen_lbl),
                div(s$source_type %||% "", class = "synth-journal")
              ),
              tags$td(s$country %||% "—"),
              tags$td(s$year    %||% "—"),
              tags$td(class = "synth-r",
                fmt_cur(as.numeric(s$cost %||% NA), as.character(s$currency %||% ""))),
              tags$td(class = "synth-r synth-step",
                if (!is.null(std)) fmt_cur(std$inflated_original, as.character(s$currency %||% "")) else "—"),
              tags$td(class = "synth-r synth-step",
                if (!is.null(std)) fmt_kes(std$kes_ppp_yr) else "—"),
              tags$td(class = "synth-r synth-step synth-kes",
                if (!is.null(std)) fmt_kes(std$kes_ppp) else "—"),
              tags$td(class = "synth-r",
                format(as.numeric(s$effect %||% NA), big.mark = ","),
                if (!is.na(s$outcome_measure %||% NA) && nzchar(s$outcome_measure %||% ""))
                  tags$br(),
                if (!is.na(s$outcome_measure %||% NA) && nzchar(s$outcome_measure %||% ""))
                  tags$small(style = "color:#737373; font-size:10px;",
                    switch(s$outcome_measure,
                      daly      = "DALY",
                      qaly      = "QALY",
                      lyg       = "LY",
                      lives     = "lives",
                      hosp_days = "hosp. days",
                      s$outcome_measure))
              ),
              tags$td(class = "synth-r", format(as.integer(s$n %||% NA), big.mark = ",")),
              tags$td(class = "synth-r synth-reported-icer", {
                ri <- suppressWarnings(as.numeric(s$reported_icer %||% NA))
                if (is.finite(ri))
                  div(fmt_cur(ri, as.character(s$currency %||% "")),
                      tags$small(style = "color:#737373;", " reported"))
                else
                  tags$span("—", style = "color:#d4d4d4;")
              })
            )
          )
        }

        # Pooled row for this strategy
        pool <- pd[[strat]]
        if (!is.null(pool)) {
          icer_cell <- if (!is.finite(pool$icer_pooled_ppp)) {
            tags$span("Reference", style = "color:#737373; font-style:italic; font-size:12px;")
          } else {
            tagList(
              div(class = "synth-kes", fmt_kes(pool$icer_pooled_ppp),
                  tags$small(style = "color:#0369a1; font-weight:400;", " calc.")),
              if (is.finite(pool$mean_icer_ppp))
                div(class = "synth-range",
                    paste0("mean: ", fmt_kes(pool$mean_icer_ppp)),
                    if (is.finite(pool$icer_method_diff_pct) &&
                        abs(pool$icer_method_diff_pct) > 10)
                      tags$span(class = "synth-icer-flag",
                                fmt_pct(pool$icer_method_diff_pct)))
            )
          }

          body_rows <- tagAppendChild(body_rows,
            tags$tr(class = "synth-pool-row",
              tags$td(),
              tags$td(
                span(paste0("Pooled · ", pool$n, if (pool$n == 1L) " study" else " studies"),
                     class = "synth-pool-lbl-cell")
              ),
              tags$td("—"), tags$td("—"),
              tags$td(class = "synth-r", "—"),
              tags$td(class = "synth-r synth-step", "—"),
              tags$td(class = "synth-r synth-step", fmt_kes(pool$cost_ppp_yr)),
              tags$td(class = "synth-r synth-step synth-kes", fmt_kes(pool$cost_ppp)),
              tags$td(class = "synth-r",
                round(pool$effect, 1L),
                {
                  om <- outcome_by_strategy()[[strat]]
                  if (!is.null(om) && !is.na(om) && om != "mixed")
                    tagList(tags$br(),
                      tags$small(style = "color:#737373; font-size:10px;",
                        switch(om, daly="DALY", qaly="QALY", lyg="LY",
                               lives="lives", hosp_days="hosp. days", om)))
                  else NULL
                }
              ),
              tags$td(class = "synth-r", format(pool$sum_n, big.mark = ",")),
              tags$td(class = "synth-r", icer_cell)
            )
          )
        } else {
          body_rows <- tagAppendChild(body_rows,
            tags$tr(class = "synth-pool-none",
              tags$td(), tags$td(colspan = "10",
                "All studies excluded — check at least one to pool")
            )
          )
        }
      }

      # Mixed-outcome warning for included studies
      obs <- outcome_by_strategy()
      mixed_om_warning <- if (length(obs) > 0 && any(obs == "mixed", na.rm = TRUE)) {
        div(class = "alert alert-warning",
          style = "font-size:12px; padding:8px 12px; margin-bottom:8px;",
          tags$strong("Mixed outcome types detected. "),
          "Pooling effects across different outcome measures is methodologically unsound. ",
          "Uncheck inconsistent studies before sending to analysis."
        )
      } else NULL

      tagList(
        mixed_om_warning,
        demo_banner,
        tags$table(class = "synth-table",
          tags$thead(header),
          tags$tbody(body_rows)
        )
      )
    })

    # ── Send button label ────────────────────────────────────────────────────
    output$send_label <- renderUI({
      pd <- pooled_data()
      n  <- length(pd)
      if (n == 0L) return("No strategies ready")
      paste0("Send ", n, " strateg", if (n == 1L) "y" else "ies", " to Analysis →")
    })

    # ── Send to Analysis ─────────────────────────────────────────────────────
    observeEvent(input$send_btn, {
      pd         <- pooled_data()
      strategies <- names(pd)
      if (length(strategies) == 0L) {
        showNotification("No pooled strategies to send.", type = "warning", duration = 4)
        return()
      }

      obs     <- outcome_by_strategy()
      sent_df <- do.call(rbind, lapply(strategies, function(strat) {
        p <- pd[[strat]]
        data.frame(
          strategy        = strat,
          cost            = p$cost_ppp,
          effect          = p$effect,
          source          = "literature",
          n_studies       = p$n,
          outcome_measure = obs[[strat]] %||% NA_character_,
          stringsAsFactors = FALSE
        )
      }))

      rv$sent_strategies <- sent_df
      rv$sent_prov       <- pd
      rv$send_count      <- rv$send_count + 1L
    })

    list(
      sent_strategies = reactive(rv$sent_strategies),
      sent_prov       = reactive(rv$sent_prov),
      send_count      = reactive(rv$send_count)
    )
  })
}
