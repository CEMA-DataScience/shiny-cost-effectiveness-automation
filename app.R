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
source("R/mod_input.R")
source("R/mod_results.R")

# ── Startup: auth + shared data ───────────────────────────────────────────────

GS_WRITE_ENABLED <- gs_init()
gs_ensure_headers()

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

  tabPanel("Evidence Synthesis",
    mod_synthesis_ui("synthesis")
  ),

  tabPanel("Analysis",
    mod_input_ui("icer_calculation")
  ),

  tabPanel("Help",
    div(class = "container-fluid", style = "margin-top:20px;",
      fluidRow(
        column(6,
          div(class = "card",
            div(class = "card-header", "Getting Started"),
            div(class = "card-body",
              tags$h5("1. Evidence Synthesis"),
              tags$p("Review published study costs standardised to KES 2027, then click
                      Send to Analysis. The analysis runs automatically and results open
                      in the side drawer."),
              tags$h5("2. Analysis"),
              tags$p("Set the effect measure and threshold. Re-run any time after
                      editing strategy data."),
              tags$hr(),
              tags$h5("Cost-Effectiveness Thresholds (Kenya)"),
              tags$ul(
                tags$li("0.5× GDP per capita: KES 154,000 per DALY/QALY"),
                tags$li("SHA Level 3–6: KES 2,240 – 4,480 per day averted")
              )
            )
          )
        ),
        column(6,
          div(class = "card",
            div(class = "card-header", "Understanding Results"),
            div(class = "card-body",
              tags$h5("ICER Table"),
              tags$p("Incremental cost per additional unit of health outcome versus the reference."),
              tags$h5("Price Threshold Analysis"),
              tags$p("Shows the maximum cost at which each strategy would be cost-effective at
                      the chosen threshold (break-even price), how much headroom exists, and the
                      ICER curve as a function of price."),
              tags$h5("PPP vs Exchange Rate"),
              tags$p("PPP-adjusted costs (from Synthesis) reflect purchasing power;
                      exchange-rate costs reflect market conversion.")
            )
          )
        )
      ),
      br(),
      fluidRow(
        column(12,
          div(class = "card",
            div(class = "card-header", "Sample Data"),
            div(class = "card-body",
              DT::dataTableOutput("sample_table")
            )
          )
        )
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

  # Evidence Synthesis module
  synthesis_out <- mod_synthesis_server("synthesis",
                                        factors        = FACTORS,
                                        interventions  = INTERVENTIONS,
                                        write_enabled  = reactive(GS_WRITE_ENABLED))

  # Analysis input module
  input_data <- mod_input_server(
    "icer_calculation",
    inject_strategies = synthesis_out$sent_strategies
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

  # Help tab sample table
  output$sample_table <- DT::renderDataTable({
    d <- create_sample_data()
    d$ICER <- c("Reference", "KES 10,000", "KES 8,293")
    DT::datatable(d,
      options = list(dom = "t", pageLength = 10, searching = FALSE, ordering = FALSE),
      rownames = FALSE
    ) |> DT::formatCurrency("cost", currency = "KES ", digits = 0)
  }, server = FALSE)

  # Charts drawer
  mod_results_server(
    "analysis_results",
    results      = reactive(app_state$last_results),
    parameters   = input_data$parameters,
    open_trigger = open_drawer_trigger,
    psa_results  = reactive(app_state$psa_results)
  )
}

shinyApp(ui = ui, server = server)
