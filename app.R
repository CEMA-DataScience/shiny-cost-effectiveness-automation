# app.R — Cost-Effectiveness Analysis Tool

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(plotly)
library(dampack)
library(shinycssloaders)
library(shinyjs)
library(markdown)
library(wbstats)
library(googlesheets4)

source("R/cea_functions.R")
source("R/fetch_factors.R")
source("R/synth_functions.R")
source("R/gs_backend.R")
source("R/mod_study_entry.R")
source("R/mod_synthesis.R")
source("R/mod_rcema_transform.R")
source("R/mod_input.R")
source("R/mod_results.R")
source("R/budget_impact_functions.R")
source("R/mod_budget_impact.R")

# ── Static assets ─────────────────────────────────────────────────────────────

# Write CSV upload template as a static file so the browser can download it
# directly (avoids Shiny session-keyed download URL issues).
local({
  dir.create("www", showWarnings = FALSE)
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
  write.csv(tpl, "www/cea_studies_template.csv", row.names = FALSE, na = "")
})

# ── Startup: auth + shared data ───────────────────────────────────────────────

GS_WRITE_ENABLED <- gs_init()
gs_ensure_headers()
gs_ensure_bia_headers()

# Interventions list: read once, shared across all sessions.
INTERVENTIONS <- gs_read_interventions()
message("[app] Loaded ", nrow(INTERVENTIONS), " interventions.")

# Load conversion factors once at startup (cached to data/factors_cache.rds).
FACTORS <- load_factors()

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- navbarPage(
  id    = "main_nav",
  title = "Cost-Effectiveness Analysis",
  theme = bslib::bs_theme(version = 4, primary = "#27AAE1", bg = "#ffffff", fg = "#0a0a0a") |>
    bslib::bs_add_rules("
      .navbar, .navbar.navbar-default, .navbar.navbar-light, .navbar.navbar-dark {
        background-color: #ffffff !important;
        background-image: none !important;
        border-bottom: 2px solid #0a0a0a !important;
        box-shadow: none !important;
      }
      .navbar-brand { color: #0a0a0a !important; font-weight: 700 !important; }
      .navbar-nav > li > a, .navbar-nav .nav-link {
        color: #737373 !important;
      }
      .navbar-nav > li.active > a,
      .navbar-nav > li.active > a:hover,
      .navbar-nav > li.active > a:focus,
      .navbar-nav .nav-link.active {
        color: #0a0a0a !important;
        background: transparent !important;
        border-bottom: 2px solid #27AAE1 !important;
        font-weight: 600 !important;
      }
      .navbar-nav > li > a:hover, .navbar-nav .nav-link:hover {
        color: #0a0a0a !important;
        background: transparent !important;
      }
      .navbar-toggle .icon-bar { background-color: #0a0a0a !important; }
      .navbar-toggle { border-color: #e5e5e5 !important; }
    "),
  header = tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    useShinyjs()
  ),
  footer = mod_results_ui("analysis_results"),

  tabPanel("Transform",
    mod_rcema_transform_ui("rcema_transform")
  ),

  tabPanel("Evidence Synthesis",
    mod_synthesis_ui("synthesis")
  ),

  tabPanel("Analysis",
    mod_input_ui("icer_calculation")
  ),

  tabPanel("Budget Impact",
    mod_budget_impact_ui("budget_impact")
  ),

  tabPanel("Methods",
    tags$iframe(
      src   = "methods.html",
      style = "width:100%; height: calc(100vh - 52px); border: none; display: block;"
    )
  ),

  tabPanel("Help",
    tags$style(HTML("
      .help-wrap { max-width: 860px; margin: 0 auto; padding: 28px 24px 48px; }
      .help-section { margin-bottom: 36px; }
      .help-section h2 {
        font-size: 15px; font-weight: 700; text-transform: uppercase;
        letter-spacing: 0.07em; color: #737373;
        border-bottom: 1.5px solid #e5e5e5; padding-bottom: 6px;
        margin-bottom: 16px;
      }
      .help-step { display: flex; gap: 14px; margin-bottom: 16px; }
      .help-step-num {
        flex-shrink: 0; width: 26px; height: 26px; border-radius: 50%;
        background: #27AAE1; color: #fff; font-size: 13px; font-weight: 700;
        display: flex; align-items: center; justify-content: center;
        margin-top: 1px;
      }
      .help-step-body h5 { font-size: 14px; font-weight: 600; margin: 0 0 4px; }
      .help-step-body p  { font-size: 13px; color: #404040; margin: 0; }
      .help-kv { display: flex; gap: 0; margin-bottom: 6px; font-size: 13px; }
      .help-kv dt { width: 220px; flex-shrink: 0; font-weight: 600; color: #0a0a0a; }
      .help-kv dd { color: #404040; margin: 0; }
      .help-note { font-size: 12px; color: #737373; margin-top: 8px; }
      .help-ol { font-size: 13px; color: #404040; padding-left: 18px; margin: 0; }
      .help-ol li { margin-bottom: 6px; }
      .help-threshold-tbl { width: 100%; font-size: 13px; border-collapse: collapse; }
      .help-threshold-tbl th {
        font-size: 11px; text-transform: uppercase; letter-spacing: 0.06em;
        color: #737373; font-weight: 600; padding: 5px 10px;
        border-bottom: 1.5px solid #0a0a0a; text-align: left;
      }
      .help-threshold-tbl td { padding: 6px 10px; border-bottom: 1px solid #f0f0f0; }
    ")),
    div(class = "help-wrap",

      # ── Section 1: Workflow ──────────────────────────────────────────────
      div(class = "help-section",
        tags$h2("Workflow"),
        div(class = "help-step",
          div(class = "help-step-num", "1"),
          div(class = "help-step-body",
            tags$h5("Transform"),
            tags$p("Upload a CSV exported from RCEMA (the AI-assisted extraction tool
                    used to pull cost and outcome data from published papers). The tab
                    checks data quality, auto-resolves country codes and currencies, and
                    lets you standardise strategy names. Save directly to the Evidence
                    Synthesis database; the app switches there automatically.")
          )
        ),
        div(class = "help-step",
          div(class = "help-step-num", "2"),
          div(class = "help-step-body",
            tags$h5("Evidence Synthesis"),
            tags$p("Select an SHA intervention to see its saved studies with costs
                    standardised to KES ", TARGET_YEAR, ". Use the checkboxes to include
                    or exclude rows — useful when studies report different outcome measures
                    (DALYs, QALYs, days averted). You can also add studies manually or
                    upload a CSV. Click ", tags$em("Send to Analysis"), " when ready.")
          )
        ),
        div(class = "help-step",
          div(class = "help-step-num", "3"),
          div(class = "help-step-body",
            tags$h5("Analysis"),
            tags$p("Review and edit the strategy table, set the outcome measure and
                    cost-effectiveness threshold, then run. Results open in the side
                    drawer: ICER table, CE plane, tornado, CEAC, and price threshold.")
          )
        )
      ),

      # ── Section 2: Data preparation ─────────────────────────────────────
      div(class = "help-section",
        tags$h2("Data preparation"),

        tags$p(style = "font-size:13px; margin-bottom:14px;",
          "Studies must have at minimum a ", tags$strong("strategy name"),
          ", ", tags$strong("cost"), " (numeric, any currency), and ",
          tags$strong("effect"), " (numeric, e.g. DALYs averted, QALYs gained)
           before they can be pooled and sent to Analysis. ICER values from papers
           are stored for reference but are not used to derive effects."),

        tags$p(style = "font-size:13px; font-weight:600; margin-bottom:6px;",
          "Currency resolution (Transform tab)"),
        tags$p(style = "font-size:13px; margin-bottom:8px;",
          "Each study's currency is resolved to an ISO 4217 code automatically,
           in three steps:"),
        tags$ol(class = "help-ol",
          tags$li(tags$strong("Direct — "), "normalises the currency column: handles
            symbols (", tags$code("$"), ", ", tags$code("£"), ", ",
            tags$code("KSh."), "), abbreviations, and full names
            (\"Kenyan shilling\", \"naira\")."),
          tags$li(tags$strong("Extracted from cost — "), "scans cost values for
            embedded codes when the currency column is absent
            (e.g. \"KSh. 45,000\" → KES, \"$1,500\" → USD)."),
          tags$li(tags$strong("Inferred from country — "), "maps the study's
            ISO alpha-3 country to its primary currency as a last resort
            (KEN → KES, NGA → NGN, GBR → GBP).")
        ),
        tags$p(class = "help-note",
          "Inferred currencies are marked † in the save preview — verify before
           saving. Currency determines which PPP and inflation series are applied
           in Evidence Synthesis; see the Methods tab for the standardisation steps."),

        tags$p(style = "font-size:13px; font-weight:600; margin: 14px 0 6px;",
          "Manual entry and CSV upload"),
        tags$p(style = "font-size:13px;",
          "Use the ", tags$em("Add study"), " button in Evidence Synthesis to enter
           a single study manually, or upload a CSV using the template available on
           the Transform tab. One row per strategy per study; numeric cost, effect,
           and n are required.")
      ),

      # ── Section 3: Results quick-reference ──────────────────────────────
      div(class = "help-section",
        tags$h2("Results quick-reference"),
        tags$dl(
          div(class = "help-kv",
            tags$dt("ICER table"),
            tags$dd("Incremental cost per additional unit of health gained versus
                     the next non-dominated comparator. Strategies below the threshold
                     are cost-effective.")
          ),
          div(class = "help-kv",
            tags$dt("CE plane"),
            tags$dd("Deterministic point + 1,000 PSA draws per strategy plotted on
                     incremental cost vs. incremental effect axes.")
          ),
          div(class = "help-kv",
            tags$dt("Tornado"),
            tags$dd("One-way sensitivity: each parameter varied ±20%; bars show the
                     resulting ICER range, sorted by impact.")
          ),
          div(class = "help-kv",
            tags$dt("CEAC"),
            tags$dd("Probability each strategy is cost-effective as willingness-to-pay
                     varies from 0 to 3× the chosen threshold.")
          ),
          div(class = "help-kv",
            tags$dt("Price threshold"),
            tags$dd("Maximum unit cost at which a strategy remains cost-effective
                     (break-even price) and headroom above current cost.")
          ),
          div(class = "help-kv",
            tags$dt("PPP vs exchange rate"),
            tags$dd("PPP adjusts for domestic price levels; exchange rate uses the
                     CBK market rate. PPP is the primary path; FX is shown for
                     sensitivity. See Methods for full derivation.")
          )
        )
      ),

      # ── Section 4: Kenya thresholds ─────────────────────────────────────
      div(class = "help-section",
        tags$h2("Kenya cost-effectiveness thresholds"),
        tags$table(class = "help-threshold-tbl",
          tags$thead(tags$tr(
            tags$th("Threshold"), tags$th("Value"), tags$th("Outcome measure")
          )),
          tags$tbody(
            tags$tr(
              tags$td("0.5× GDP per capita"),
              tags$td("KES 154,000"),
              tags$td("per DALY or QALY averted")
            ),
            tags$tr(
              tags$td("SHA Level 3"),
              tags$td("KES 2,240"),
              tags$td("per day averted")
            ),
            tags$tr(
              tags$td("SHA Level 4"),
              tags$td("KES 2,800"),
              tags$td("per day averted")
            ),
            tags$tr(
              tags$td("SHA Level 5–6"),
              tags$td("KES 4,480"),
              tags$td("per day averted")
            )
          )
        ),
        tags$p(class = "help-note",
          "Thresholds are indicative. The SHA benefit package review applies
           multi-criteria decision analysis alongside cost-effectiveness results.")
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  app_state <- reactiveValues(
    last_results = NULL,
    psa_results  = NULL,
    sent_prov    = list()
  )

  open_drawer_trigger <- reactiveVal(0L)

  # RCEMA Transform module — loaded before Synthesis so preset reactive is ready
  transform_out <- mod_rcema_transform_server("rcema_transform",
                                              interventions = INTERVENTIONS,
                                              factors       = FACTORS)

  # When Transform saves studies, switch to Synthesis and preset the dropdown
  preset_intv <- reactiveVal(NULL)

  observeEvent(transform_out$save_count(), {
    preset_intv(transform_out$saved_intervention())
    updateNavbarPage(session, "main_nav", selected = "Evidence Synthesis")
  }, ignoreInit = TRUE)

  # Evidence Synthesis module
  synthesis_out <- mod_synthesis_server("synthesis",
                                        factors             = FACTORS,
                                        interventions       = INTERVENTIONS,
                                        write_enabled       = reactive(GS_WRITE_ENABLED),
                                        preset_intervention = preset_intv)

  # Strategy injection into Analysis — from Synthesis only
  injected_strategies <- reactiveVal(NULL)

  observeEvent(synthesis_out$send_count(), {
    injected_strategies(synthesis_out$sent_strategies())
  }, ignoreInit = TRUE)

  # Analysis input module
  input_data <- mod_input_server(
    "icer_calculation",
    inject_strategies = injected_strategies
  )

  # ── Analysis run helper ───────────────────────────────────────────────────
  .run_analysis <- function() {
    strategies <- input_data$strategies_data()
    tryCatch({
      icer_result <- dampack::calculate_icers(
        cost       = strategies$cost,
        effect     = strategies$effect,
        strategies = strategies$strategy
      )
      app_state$last_results <- icer_result

      prov <- if (length(app_state$sent_prov) > 0) app_state$sent_prov else NULL
      app_state$psa_results  <- generate_psa_samples(strategies, n_iter = 1000L, prov = prov)

      open_drawer_trigger(open_drawer_trigger() + 1L)
    }, error = function(e) {
      showNotification(paste("Analysis failed:", e$message), duration = 8, type = "warning")
    })
  }

  # Auto-run when Synthesis sends strategies → switch tab + run + open drawer
  observeEvent(synthesis_out$send_count(), {
    app_state$sent_prov <- synthesis_out$sent_prov()
    updateNavbarPage(session, "main_nav", selected = "Analysis")
  }, ignoreInit = TRUE)

  observeEvent(input_data$injected_count(), {
    req(input_data$analysis_ready())
    .run_analysis()
  }, ignoreInit = TRUE)

  # Manual run
  observeEvent(input_data$run_trigger(), {
    req(input_data$analysis_ready())
    .run_analysis()
  })


  # Charts drawer
  mod_results_server(
    "analysis_results",
    results      = reactive(app_state$last_results),
    parameters   = input_data$parameters,
    open_trigger = open_drawer_trigger,
    psa_results  = reactive(app_state$psa_results)
  )

  mod_budget_impact_server(
    "budget_impact",
    results         = reactive(app_state$last_results),
    strategies_data = input_data$strategies_data
  )
}

shinyApp(ui = ui, server = server)
