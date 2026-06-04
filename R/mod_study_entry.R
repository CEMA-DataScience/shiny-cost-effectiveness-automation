# mod_study_entry.R
# Handles study data entry: manual form and bulk CSV upload.
# Triggered by parent module (mod_synthesis); writes to Google Sheets.
#
# Choice lists (countries, currencies) are derived from wbstats and FACTORS
# at module server startup — not hardcoded.

# Columns included in the downloadable CSV template.
# record_id, reference_id, submitted_at are app-generated and excluded.
# submitted_by is excluded — it is per-session, not per-row in bulk uploads.
CSV_TEMPLATE_COLS <- c(
  "strategy", "authors", "year", "source_type",
  "indication", "population", "comparator", "country",
  "currency", "currency_year", "perspective", "time_horizon", "discount_rate",
  "outcome_measure", "cost", "effect", "n",
  "scenario", "reported_icer", "threshold_referenced", "conclusion"
)

SCENARIO_CHOICES <- c(
  "Base case"                   = "base_case",
  "Probabilistic central"       = "probabilistic",
  "Lower bound"                 = "lower_bound",
  "Upper bound"                 = "upper_bound",
  "Threshold sensitivity"       = "threshold_sensitivity",
  "Subgroup analysis"           = "subgroup",
  "Other sensitivity analysis"  = "other_sensitivity"
)

SOURCE_CHOICES <- c(
  "Journal article"     = "journal",
  "HTA report"          = "hta",
  "Conference abstract" = "conference",
  "Grey literature"     = "grey",
  "Other"               = "other"
)

PERSPECTIVE_CHOICES <- c(
  "Not stated"    = "not_stated",
  "Payer"         = "payer",
  "Health system" = "health_system",
  "Societal"      = "societal"
)

OUTCOME_CHOICES <- c(
  "Select..." = "",
  "DALY averted"                   = "daly",
  "QALY gained"                    = "qaly",
  "Life year gained"               = "lyg",
  "Life saved"                     = "lives",
  "Day of hospitalisation averted" = "hosp_days"
)

# ── UI ─────────────────────────────────────────────────────────────────────────
# Minimal — modals are rendered server-side.
# The download button must be in the DOM (not inside a modal) for the
# downloadHandler to work; it is hidden visually.

mod_study_entry_ui <- function(id) {
  ns <- NS(id)
  tags$span(style = "display:none;",
    downloadButton(ns("csv_template_dl"), ""))
}

# ── Server ─────────────────────────────────────────────────────────────────────

#' @param id              Module namespace
#' @param intervention    Reactive string  — selected intervention name
#' @param benefit_package Reactive string  — benefit package for pre-filling indication
#' @param add_trigger     Reactive integer — fires when "+ Add study" clicked
#' @param upload_trigger  Reactive integer — fires when "↑ Upload CSV" clicked
#' @param write_enabled   Reactive logical — FALSE disables GS writes
#' @param factors         Output of load_factors() — for currency/country lists
mod_study_entry_server <- function(id,
                                   intervention,
                                   benefit_package  = reactive(""),
                                   add_trigger      = reactive(NULL),
                                   upload_trigger   = reactive(NULL),
                                   write_enabled    = reactive(TRUE),
                                   factors          = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    entries_added <- reactiveVal(0L)

    # ── Choice lists derived from loaded data (computed once) ───────────────
    country_choices <- local({
      d <- wbstats::wb_cachelist$countries
      d <- d[!is.na(d$iso3c) & nzchar(d$iso3c) & nzchar(d$country), ]
      d <- d[order(d$country), ]
      setNames(d$iso3c, paste0(d$country, " (", d$iso3c, ")"))
    })
    country_choices <- c("Select country..." = "", country_choices)

    currency_choices <- if (!is.null(factors)) {
      codes   <- sort(unique(c(names(factors$ppp$rates), names(factors$fx$rates))))
      codes   <- codes[nzchar(codes)]
      iso_map <- factors$iso3c_map
      labels <- vapply(codes, function(cc) {
        iso  <- iso_map[cc]
        ctry <- if (!is.null(iso) && !is.na(iso)) {
          hit <- wbstats::wb_cachelist$countries$country[
            wbstats::wb_cachelist$countries$iso3c == iso][1L]
          if (!is.na(hit) && nzchar(hit)) paste0(cc, " — ", hit) else cc
        } else cc
        ctry
      }, character(1L))
      c("Select currency..." = "", setNames(codes, labels))
    } else {
      c("Select currency..." = "")
    }

    # ── CSV template download ───────────────────────────────────────────────
    output$csv_template_dl <- downloadHandler(
      filename = function()
        paste0("cea_studies_template_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        tpl <- as.data.frame(
          matrix(NA_character_, nrow = 1L, ncol = length(CSV_TEMPLATE_COLS),
                 dimnames = list(NULL, CSV_TEMPLATE_COLS)),
          stringsAsFactors = FALSE
        )
        tpl$strategy        <- ""
        tpl$authors         <- "Author et al."
        tpl$year            <- as.integer(format(Sys.Date(), "%Y"))
        tpl$source_type     <- "journal"
        tpl$currency        <- "KES"
        tpl$outcome_measure <- "daly"
        tpl$cost            <- 500000
        tpl$effect          <- 1000
        tpl$n               <- 500
        tpl$scenario        <- "base_case"
        write.csv(tpl, file, row.names = FALSE, na = "")
      }
    )

    # ── Form modal ──────────────────────────────────────────────────────────
    observeEvent(add_trigger(), {
      req(add_trigger())
      intv <- intervention()
      pkg  <- benefit_package()

      showModal(modalDialog(
        title = tagList(
          tags$span(style = "font-size:13px; color:#737373; font-weight:400;",
                    "Intervention: "),
          tags$span(intv, style = "font-size:13px; color:#27AAE1; font-weight:600;")
        ),
        size      = "l",
        easyClose = FALSE,
        footer    = tagList(
          modalButton("Cancel"),
          actionButton(ns("submit_study"), "Save study", class = "btn btn-primary")
        ),

        tags$style(HTML("
          .se-hdr {
            font-size:10px; font-weight:700; text-transform:uppercase;
            letter-spacing:0.06em; color:#737373;
            padding:10px 0 6px; border-bottom:1px solid #e5e5e5; margin-bottom:10px;
          }
          .se-req::after { content:' *'; color:#b91c1c; }
          .se-strategy-wrap {
            background:#f8fafc; border:1px solid #e2e8f0; border-radius:4px;
            padding:12px 14px; margin-bottom:14px;
          }
          .se-strategy-wrap .form-group { margin-bottom:0; }
        ")),

        # ── Strategy (the arm this study belongs to) ───────────────────────
        div(class = "se-strategy-wrap",
          div(style = "font-size:11px; font-weight:700; text-transform:uppercase;
                       letter-spacing:0.06em; color:#475569; margin-bottom:6px;",
              "Strategy (CEA comparison arm)"),
          fluidRow(
            column(12, textInput(ns("strategy"),
                                 tags$span(class = "se-req", "Strategy name"),
                                 value       = "",
                                 placeholder = "e.g. Primary THA · Revision THA · Conservative management"))
          ),
          div(style = "font-size:11px; color:#64748b; margin-top:4px;",
              "Each arm in the CEA is a strategy. Enter one study per arm per row.",
              " To add the comparator arm, save this study then add a new study with the comparator as the strategy.")
        ),

        # ── Required study fields ──────────────────────────────────────────
        div(class = "se-hdr", "Study — required"),
        fluidRow(
          column(7, textInput(ns("authors"), tags$span(class="se-req","Authors"),
                              placeholder = "e.g. Ochieng et al.")),
          column(2, numericInput(ns("year"), tags$span(class="se-req","Year"),
                                 value = as.integer(format(Sys.Date(),"%Y")),
                                 min = 1990L, max = 2035L, step = 1L)),
          column(3, selectInput(ns("source_type"), "Source type",
                                choices = SOURCE_CHOICES, selected = "journal"))
        ),
        fluidRow(
          column(4, selectInput(ns("country"),  tags$span(class="se-req","Country"),
                                choices = country_choices)),
          column(4, selectInput(ns("currency"), tags$span(class="se-req","Currency"),
                                choices = currency_choices)),
          column(4, numericInput(ns("currency_year"),
                                 tags$span("Currency year",
                                           tags$small(style="color:#737373;"," (if ≠ pub. year)")),
                                 value = NA_real_, min = 1990L, max = 2035L, step = 1L))
        ),
        fluidRow(
          column(3, numericInput(ns("cost"),   tags$span(class="se-req","Cost"),
                                 value = NA_real_, min = 0)),
          column(3, numericInput(ns("effect"), tags$span(class="se-req","Effect"),
                                 value = NA_real_, min = 0)),
          column(4, selectInput(ns("outcome_measure"),
                                tags$span(class="se-req","Outcome measure"),
                                choices = OUTCOME_CHOICES)),
          column(2, numericInput(ns("n"), tags$span(class="se-req","n"),
                                 value = NA_real_, min = 1L, step = 1L))
        ),

        uiOutput(ns("form_error")),

        # ── Scenario + reported ICER ───────────────────────────────────────
        div(class = "se-hdr", "Reported results"),
        fluidRow(
          column(5, selectInput(ns("scenario"), "Scenario",
                                choices = SCENARIO_CHOICES, selected = "base_case")),
          column(4, numericInput(ns("reported_icer"), "Reported ICER (original currency)",
                                 value = NA_real_, min = 0)),
          column(3, textInput(ns("threshold_referenced"), "Threshold referenced",
                              placeholder = "e.g. 1× GDP"))
        ),

        # ── Optional metadata ─────────────────────────────────────────────
        tags$details(
          tags$summary(
            div(class = "se-hdr",
                style = "cursor:pointer; margin-bottom:0;",
                "Study context (click to expand)")
          ),
          br(),
          fluidRow(
            column(6, textInput(ns("indication"), "Indication",
                                value = pkg,
                                placeholder = "e.g. Type 2 diabetes")),
            column(6, textInput(ns("comparator"),
                                tags$span("Study's reference arm",
                                          tags$small(style = "color:#737373; font-weight:400;",
                                                     " (context only)")),
                                placeholder = "e.g. No treatment — add as a strategy for CEA"))
          ),
          fluidRow(
            column(6, textInput(ns("population"), "Population",
                                placeholder = "e.g. Adults 18–65, HIV+")),
            column(3, selectInput(ns("perspective"), "Perspective",
                                  choices = PERSPECTIVE_CHOICES)),
            column(3, textInput(ns("time_horizon"), "Time horizon",
                                placeholder = "e.g. Lifetime"))
          ),
          fluidRow(
            column(3, numericInput(ns("discount_rate"), "Discount rate (%)",
                                   value = NA_real_, min = 0, max = 20, step = 0.5)),
            column(9, textAreaInput(ns("conclusion"), "Authors’ conclusion",
                                    placeholder = "e.g. Cost-effective at 1× GDP threshold",
                                    rows = 2L))
          )
        ),

        hr(),
        fluidRow(
          column(5, textInput(ns("submitted_by"), "Your name (optional)"))
        )
      ))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Validation and submit
    output$form_error <- renderUI(NULL)

    observeEvent(input$submit_study, {
      errs <- character(0L)
      if (!nzchar(trimws(input$strategy %||% "")))     errs <- c(errs, "Strategy / comparison arm required.")
      if (!nzchar(trimws(input$authors %||% "")))      errs <- c(errs, "Authors required.")
      if (is.na(input$year) || input$year < 1990L)     errs <- c(errs, "Valid year required.")
      if (!nzchar(input$country   %||% ""))            errs <- c(errs, "Country required.")
      if (!nzchar(input$currency  %||% ""))            errs <- c(errs, "Currency required.")
      if (is.na(input$cost)   || input$cost   <= 0)    errs <- c(errs, "Cost must be > 0.")
      if (is.na(input$effect) || input$effect <= 0)    errs <- c(errs, "Effect must be > 0.")
      if (!nzchar(input$outcome_measure %||% ""))      errs <- c(errs, "Outcome measure required.")
      if (is.na(input$n)      || input$n      < 1L)    errs <- c(errs, "Sample size n ≥ 1 required.")

      if (length(errs) > 0L) {
        output$form_error <- renderUI(
          div(class = "alert alert-warning", style = "margin-top:8px; font-size:13px;",
              tags$ul(style = "margin:0; padding-left:18px;", lapply(errs, tags$li)))
        )
        return()
      }
      output$form_error <- renderUI(NULL)

      study <- list(
        intervention         = intervention(),
        strategy             = trimws(input$strategy %||% intervention()),
        authors              = trimws(input$authors),
        year                 = as.integer(input$year),
        source_type          = input$source_type,
        indication           = trimws(input$indication          %||% ""),
        population           = trimws(input$population          %||% ""),
        comparator           = trimws(input$comparator          %||% ""),
        country              = input$country,
        currency             = input$currency,
        currency_year        = if (is.na(input$currency_year)) NA_integer_
                               else as.integer(input$currency_year),
        perspective          = input$perspective,
        time_horizon         = trimws(input$time_horizon        %||% ""),
        discount_rate        = input$discount_rate,
        outcome_measure      = input$outcome_measure,
        cost                 = input$cost,
        effect               = input$effect,
        n                    = as.integer(input$n),
        scenario             = input$scenario,
        reported_icer        = input$reported_icer,
        threshold_referenced = trimws(input$threshold_referenced %||% ""),
        conclusion           = trimws(input$conclusion           %||% ""),
        submitted_by         = trimws(input$submitted_by         %||% "")
      )

      if (isTRUE(write_enabled())) {
        ok <- gs_write_study(study)
        if (!ok) {
          output$form_error <- renderUI(
            div(class = "alert alert-warning",
                "Failed to save — check your connection and try again.")
          )
          return()
        }
      } else {
        message("[study_entry] Write disabled — study not saved.")
      }

      removeModal()
      entries_added(entries_added() + 1L)
    })

    # ── CSV upload modal ────────────────────────────────────────────────────
    csv_valid   <- reactiveVal(NULL)
    csv_invalid <- reactiveVal(NULL)

    observeEvent(upload_trigger(), {
      req(upload_trigger())
      csv_valid(NULL); csv_invalid(NULL)

      showModal(modalDialog(
        title     = "Upload studies from CSV",
        size      = "l",
        easyClose = FALSE,
        footer    = tagList(
          modalButton("Cancel"),
          uiOutput(ns("csv_confirm_btn"))
        ),
        div(style = "margin-bottom:12px;",
          tags$a(
            href    = "#",
            onclick = sprintf(
              "document.getElementById('%s').click(); return false;",
              ns("csv_template_dl")
            ),
            class = "btn btn-sm btn-outline-secondary",
            "↓ Download template"
          ),
          tags$span(style = "font-size:12px; color:#737373; margin-left:10px;",
            "Columns: ", paste(CSV_TEMPLATE_COLS, collapse = ", "))
        ),
        fileInput(ns("csv_file"), NULL,
                  accept      = c(".csv", "text/csv"),
                  placeholder = "Choose CSV file…"),
        uiOutput(ns("csv_preview_ui"))
      ))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Parse uploaded file
    observeEvent(input$csv_file, {
      req(input$csv_file)
      csv_valid(NULL); csv_invalid(NULL)

      tryCatch({
        raw        <- read.csv(input$csv_file$datapath, stringsAsFactors = FALSE,
                               na.strings = c("", "NA", "N/A"))
        names(raw) <- trimws(tolower(gsub("[^a-z0-9_]", "_", names(raw))))
        col_norm   <- setNames(trimws(tolower(gsub("[^a-z0-9_]", "_",
                                                   CSV_TEMPLATE_COLS))),
                               CSV_TEMPLATE_COLS)

        aligned <- as.data.frame(
          lapply(CSV_TEMPLATE_COLS, function(col) {
            hit <- raw[[col_norm[col]]]
            if (is.null(hit)) NA_character_ else as.character(hit)
          }),
          col.names        = CSV_TEMPLATE_COLS,
          stringsAsFactors = FALSE
        )

        # Intervention is always the app-selected topic — never from the CSV
        aligned$intervention <- intervention()

        # Strategy blank → use intervention name as fallback for single-strategy uploads
        blank_strat <- is.na(aligned$strategy) | !nzchar(trimws(aligned$strategy))
        if (any(blank_strat)) aligned$strategy[blank_strat] <- intervention()

        required <- c("strategy", "authors", "year", "currency", "cost", "effect",
                      "outcome_measure", "n")
        ok <- apply(aligned[, required, drop = FALSE], 1L, function(r)
          all(!is.na(r) & nzchar(trimws(as.character(r))))
        )
        csv_valid(aligned[ok,  , drop = FALSE])
        csv_invalid(aligned[!ok, , drop = FALSE])
      }, error = function(e) {
        showNotification(paste("Could not parse CSV:", e$message),
                         type = "warning", duration = 6)
      })
    })

    output$csv_preview_ui <- renderUI({
      vd <- csv_valid(); ivd <- csv_invalid()
      if (is.null(vd)) return(NULL)
      show_cols <- c("strategy", "authors", "year", "currency",
                     "cost", "effect", "outcome_measure", "scenario", "n")
      tagList(
        if (nrow(vd) > 0L) tagList(
          div(style = "color:#047857; font-size:13px; margin-bottom:6px;",
              "✓ ", nrow(vd), " valid row", if (nrow(vd) != 1L) "s",
              " (showing first 5):"),
          DT::datatable(
            head(vd[, intersect(show_cols, names(vd))], 5L),
            options  = list(dom = "t", scrollX = TRUE),
            rownames = FALSE, class = "table table-sm"
          )
        ),
        if (!is.null(ivd) && nrow(ivd) > 0L)
          div(style = "color:#b45309; font-size:12px; margin-top:8px;",
              "⚠ ", nrow(ivd), " row", if (nrow(ivd) != 1L) "s",
              " skipped — missing one or more required fields: ",
              paste(required, collapse = ", "), ".")
      )
    })

    output$csv_confirm_btn <- renderUI({
      vd <- csv_valid()
      if (is.null(vd) || nrow(vd) == 0L) return(NULL)
      actionButton(ns("csv_confirm"),
                   paste0("Upload ", nrow(vd), " row",
                          if (nrow(vd) != 1L) "s"),
                   class = "btn btn-primary")
    })

    observeEvent(input$csv_confirm, {
      vd <- csv_valid()
      req(!is.null(vd), nrow(vd) > 0L)

      if (!isTRUE(write_enabled())) {
        showNotification("Write not available — check service account credentials.",
                         type = "warning")
        return()
      }

      n_ok <- 0L
      withProgress(message = "Uploading…", value = 0, {
        for (i in seq_len(nrow(vd))) {
          row <- as.list(vd[i, ])
          row$year          <- suppressWarnings(as.integer(row$year))
          row$currency_year <- suppressWarnings(as.integer(row$currency_year))
          row$cost          <- suppressWarnings(as.numeric(row$cost))
          row$effect        <- suppressWarnings(as.numeric(row$effect))
          row$n             <- suppressWarnings(as.integer(row$n))
          row$discount_rate <- suppressWarnings(as.numeric(row$discount_rate))
          row$reported_icer <- suppressWarnings(as.numeric(row$reported_icer))
          if (gs_write_study(row)) n_ok <- n_ok + 1L
          incProgress(1 / nrow(vd))
        }
      })

      removeModal()
      if (n_ok > 0L) entries_added(entries_added() + n_ok)
      if (n_ok < nrow(vd))
        showNotification(
          paste0(n_ok, " of ", nrow(vd), " saved; ",
                 nrow(vd) - n_ok, " failed."),
          type = "warning", duration = 8
        )
    })

    list(entries_added = entries_added)
  })
}
