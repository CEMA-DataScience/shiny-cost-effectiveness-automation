# mod_input.R
# Analysis module — strategy entry and parameters only.
# All results live in the charts drawer (mod_results.R).

mod_input_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(tags$style(HTML("
      .anl-wrap { max-width: 1200px; margin: 0 auto; padding: 24px; }

      .anl-pg-hdr {
        padding: 20px 0 16px; border-bottom: 1px solid #e5e5e5; margin-bottom: 20px;
      }
      .anl-pg-hdr h1 { font-size: 22px; font-weight: 700; margin: 0 0 4px; }
      .anl-pg-hdr p  { font-size: 14px; color: #737373; margin: 0; }

      .anl-card-hdr {
        display: flex; align-items: center; justify-content: space-between;
        padding: 11px 15px; border-bottom: 1px solid #e5e5e5;
        background: #fff; border-radius: 4px 4px 0 0;
      }
      .anl-card-hdr-lbl {
        font-size: 11px; font-weight: 700; color: #737373;
        text-transform: uppercase; letter-spacing: 0.05em;
      }
      .anl-tbl-acts { display: flex; gap: 6px; align-items: center; }

      .anl-src-lit { display: inline-block; background: #dff3fb; color: #1c8ec0;
        font-size: 11px; font-weight: 600; padding: 1px 6px; border-radius: 2px; }
      .anl-src-mod { display: inline-block; background: #fef9c3; color: #b45309;
        font-size: 11px; font-weight: 600; padding: 1px 6px; border-radius: 2px; }
      .anl-src-man { display: inline-block; background: #f5f5f4; color: #737373;
        font-size: 11px; font-weight: 500; padding: 1px 6px; border-radius: 2px;
        border: 1px solid #e5e5e5; }

      .anl-fld-lbl {
        font-size: 11px; font-weight: 700; color: #737373;
        text-transform: uppercase; letter-spacing: 0.05em;
        margin: 12px 0 4px; display: block;
      }
      .anl-fld-lbl:first-child { margin-top: 0; }

      .anl-run-btn {
        width: 100% !important; padding: 10px !important;
        font-size: 14px !important; font-weight: 600 !important;
      }
    "))),

    div(class = "anl-wrap",

      div(class = "anl-pg-hdr",
        tags$h1("Cost-Effectiveness Analysis"),
        tags$p("Review strategies from Evidence Synthesis, set parameters, then run analysis.")
      ),

      fluidRow(
        column(8,
          div(class = "card",
            div(class = "anl-card-hdr",
              span("Strategies", class = "anl-card-hdr-lbl"),
              div(class = "anl-tbl-acts",
                actionButton(ns("add_row"), "Add row",
                  class = "btn btn-sm btn-outline-secondary"),
                actionButton(ns("remove_row"), "Remove last",
                  class = "btn btn-sm btn-outline-secondary"),
                uiOutput(ns("load_sample_btn"), inline = TRUE)
              )
            ),
            div(class = "card-body p-0",
              DT::dataTableOutput(ns("strategies_table"))
            )
          )
        ),

        column(4,
          div(class = "card",
            div(class = "anl-card-hdr",
              span("Parameters", class = "anl-card-hdr-lbl")
            ),
            div(class = "card-body",

              tags$label("Effect measure", class = "anl-fld-lbl"),
              selectInput(ns("outcome_type"), NULL,
                choices = c(
                  "DALYs averted"                   = "daly",
                  "QALYs gained"                    = "qaly",
                  "Life years gained"               = "lyg",
                  "Lives saved"                     = "lives",
                  "Days of hospitalisation averted" = "hosp_days"
                ),
                selected = "daly",
                width    = "100%"
              ),

              tags$label("Cost-effectiveness thresholds", class = "anl-fld-lbl"),
              uiOutput(ns("threshold_ui")),

              hr(),

              uiOutput(ns("validation_msg")),

              actionButton(ns("run_analysis"), "Run Analysis",
                class = "btn btn-primary anl-run-btn")
            )
          )
        )
      )
    )
  )
}

#' @param id                Module namespace ID
#' @param inject_strategies Reactive returning pooled strategy df from Synthesis
mod_input_server <- function(id, inject_strategies = NULL) {
  moduleServer(id, function(input, output, session) {

    inject_count <- reactiveVal(0L)

    values <- reactiveValues(
      strategies_data = data.frame(
        strategy  = c("Status Quo", "Intervention"),
        cost      = c(100000, 250000),
        effect    = c(10.0,   16.0),
        source    = c("manual", "manual"),
        n_studies = c(NA_integer_, NA_integer_),
        stringsAsFactors = FALSE
      ),
      from_synthesis = FALSE
    )

    # ── Outcome-reactive threshold UI ─────────────────────────────────────
    output$threshold_ui <- renderUI({
      ot  <- input$outcome_type %||% "daly"
      sns <- session$ns
      if (ot %in% c("daly", "qaly")) {
        checkboxGroupInput(sns("gdp_thresholds"), NULL,
          choices = c(
            "0.5× GDP — KES 154,000" = "154000",
            "1× GDP — KES 308,000"   = "308000",
            "3× GDP — KES 924,000"   = "924000"
          ),
          selected = c("154000", "308000", "924000")
        )
      } else if (ot == "hosp_days") {
        checkboxGroupInput(sns("sha_thresholds"), NULL,
          choices = c(
            "Level 3 — KES 2,240 / day" = "2240",
            "Level 4 — KES 3,360 / day" = "3360",
            "Level 5 — KES 3,920 / day" = "3920",
            "Level 6 — KES 4,480 / day" = "4480"
          ),
          selected = c("2240", "3360", "3920", "4480")
        )
      } else {
        numericInput(sns("vsly_value"), NULL,
          value = 2000000, min = 1, step = 100000, width = "100%")
      }
    })

    # ── Accept strategies from Evidence Synthesis ──────────────────────────
    if (!is.null(inject_strategies)) {
      observeEvent(inject_strategies(), {
        d <- inject_strategies()
        if (is.null(d) || nrow(d) == 0L) return()
        values$strategies_data <- data.frame(
          strategy  = d$strategy,
          cost      = d$cost,
          effect    = d$effect,
          source    = d$source,
          n_studies = d$n_studies,
          stringsAsFactors = FALSE
        )
        values$from_synthesis <- TRUE
        inject_count(inject_count() + 1L)
        # Auto-set outcome_type from synthesis outcome_measure
        if ("outcome_measure" %in% names(d)) {
          om <- unique(d$outcome_measure)
          om <- om[!is.na(om) & om != "mixed"]
          if (length(om) == 1L && om %in% c("daly", "qaly", "lyg", "lives", "hosp_days"))
            updateSelectInput(session, "outcome_type", selected = om)
        }
      }, ignoreNULL = TRUE, ignoreInit = TRUE)
    }

    # "Load sample" hidden when populated from Synthesis
    output$load_sample_btn <- renderUI({
      if (isTRUE(values$from_synthesis)) return(NULL)
      actionButton(session$ns("load_sample"), "Load sample",
        class = "btn btn-sm btn-outline-secondary")
    })

    # ── Strategies table ───────────────────────────────────────────────────
    output$strategies_table <- DT::renderDataTable({
      d <- values$strategies_data
      n <- nrow(d)
      if (n == 0L) return(DT::datatable(data.frame()))

      ref_cost   <- d$cost[1L]
      ref_effect <- d$effect[1L]

      icer_col <- vapply(seq_len(n), function(i) {
        if (i == 1L) return("Reference")
        dc <- d$cost[i]   - ref_cost
        de <- d$effect[i] - ref_effect
        if (!is.finite(de) || de == 0) return("—")
        if (de < 0)                    return("Dominated")
        paste0("KES ", format(round(dc / de), big.mark = ","))
      }, character(1L))

      src_col <- vapply(seq_len(n), function(i) {
        switch(d$source[i],
          literature = paste0('<span class="anl-src-lit">Lit · ', d$n_studies[i], ' studies</span>'),
          modified   = '<span class="anl-src-mod">Modified</span>',
          '<span class="anl-src-man">Manual</span>'
        )
      }, character(1L))

      display <- data.frame(
        Strategy     = d$strategy,
        `Cost (KES)` = d$cost,
        Effect       = d$effect,
        Source       = src_col,
        ICER         = icer_col,
        check.names  = FALSE,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        display,
        escape   = FALSE,
        editable = list(target = "cell", disable = list(columns = c(3L, 4L))),
        options  = list(
          dom        = "t",
          pageLength = 20,
          scrollX    = FALSE,
          searching  = FALSE,
          ordering   = FALSE,
          autoWidth  = FALSE,
          columnDefs = list(
            list(width = "28%", targets = 0L),
            list(width = "20%", targets = 1L),
            list(width = "12%", targets = 2L),
            list(width = "18%", targets = 3L),
            list(width = "22%", targets = 4L)
          ),
          rowCallback = DT::JS(
            "function(row, data, index) {",
            "  if (index === 0) $(row).css({'font-weight':'600','background':'#fafafa'});",
            "}"
          )
        ),
        rownames = FALSE,
        class    = "table table-sm"
      ) |>
        DT::formatCurrency("Cost (KES)", currency = "KES ", digits = 0) |>
        DT::formatRound("Effect", digits = 1L)

    }, server = FALSE)

    # ── Handle cell edits ──────────────────────────────────────────────────
    observeEvent(input$strategies_table_cell_edit, {
      info     <- input$strategies_table_cell_edit
      row      <- info$row
      col      <- info$col
      prev_src <- values$strategies_data$source[row]

      if      (col == 0L) values$strategies_data[row, "strategy"] <- as.character(info$value)
      else if (col == 1L) values$strategies_data[row, "cost"]     <- as.numeric(info$value)
      else if (col == 2L) values$strategies_data[row, "effect"]   <- as.numeric(info$value)

      if (col %in% c(0L, 1L, 2L))
        values$strategies_data[row, "source"] <-
          if (prev_src == "literature") "modified" else "manual"
    })

    # ── Row add / remove ───────────────────────────────────────────────────
    observeEvent(input$add_row, {
      new_row <- data.frame(
        strategy  = paste0("Strategy ", nrow(values$strategies_data) + 1L),
        cost = 0, effect = 0,
        source    = "manual",
        n_studies = NA_integer_,
        stringsAsFactors = FALSE
      )
      values$strategies_data <- rbind(values$strategies_data, new_row)
      values$from_synthesis  <- FALSE
    })

    observeEvent(input$remove_row, {
      if (nrow(values$strategies_data) > 2L)
        values$strategies_data <- values$strategies_data[-nrow(values$strategies_data), ]
      else
        showNotification("Need at least 2 strategies.", type = "warning", duration = 3)
    })

    observeEvent(input$load_sample, {
      s <- create_sample_data()
      s$source    <- "manual"
      s$n_studies <- NA_integer_
      values$strategies_data <- s
      values$from_synthesis  <- FALSE
    })

    # ── Validation ─────────────────────────────────────────────────────────
    validation_result <- reactive(validate_cea_data(values$strategies_data))

    output$validation_msg <- renderUI({
      v   <- validation_result()
      cls <- if (v$valid) "alert alert-success" else "alert alert-warning"
      txt <- if (v$valid)
        paste0("Ready — ", nrow(values$strategies_data), " strategies")
      else v$message
      div(class = cls,
          style = "margin-bottom:10px; padding:8px 12px; font-size:13px;", txt)
    })

    # ── Return interface ───────────────────────────────────────────────────
    list(
      strategies_data = reactive(values$strategies_data),
      parameters = reactive({
        ot <- input$outcome_type %||% "daly"

        thr_vec <- if (ot %in% c("daly", "qaly")) {
          all_gdp <- c("154000" = "0.5× GDP", "308000" = "1× GDP", "924000" = "3× GDP")
          sel <- input$gdp_thresholds %||% names(all_gdp)
          sel <- sel[sel %in% names(all_gdp)]
          if (length(sel) == 0L) sel <- "154000"
          setNames(as.numeric(sel), all_gdp[sel])
        } else if (ot == "hosp_days") {
          all_sha <- c("2240" = "Level 3", "3360" = "Level 4",
                       "3920" = "Level 5", "4480" = "Level 6")
          sel <- input$sha_thresholds %||% names(all_sha)
          sel <- sel[sel %in% names(all_sha)]
          if (length(sel) == 0L) sel <- "2240"
          setNames(as.numeric(sel), all_sha[sel])
        } else {
          v <- input$vsly_value %||% 2000000
          if (!is.finite(v) || v <= 0) v <- 2000000
          setNames(v, paste0("VSLY — KES ", format(round(v), big.mark = ",")))
        }

        ol <- switch(ot,
          daly      = "DALY averted",
          qaly      = "QALY gained",
          lyg       = "life year gained",
          lives     = "life saved",
          hosp_days = "day of hospitalisation averted"
        )
        list(
          outcome_type  = ot,
          outcome_label = ol,
          threshold     = min(thr_vec, na.rm = TRUE),
          thresholds    = thr_vec,
          psa_enabled   = FALSE
        )
      }),
      analysis_ready = reactive(validation_result()$valid),
      run_trigger    = reactive(input$run_analysis),
      injected_count = inject_count
    )
  })
}
