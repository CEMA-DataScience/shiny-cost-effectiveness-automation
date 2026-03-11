# Main Shiny App - CEA Tool
# Entry point for the cost-effectiveness analysis application

# Load required libraries
library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(dampack)
library(shinycssloaders)
library(shinyjs)
library(markdown)

# Source R functions
source("R/cea_functions.R")
source("R/mod_input.R")

# Define UI - Modern single page layout
ui <- navbarPage(
  title = "Cost-Effectiveness Analysis Tool",
  theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),

  # Main Analysis Tab
  tabPanel("Analysis",
    icon = icon("calculator"),

    # Initialize shinyjs
    useShinyjs(),

    # Custom CSS
    tags$head(
      tags$style(HTML("
        body { background-color: #f8f9fa; }
        .navbar-brand { font-weight: bold; }
        .card {
          box-shadow: 0 0.125rem 0.25rem rgba(0, 0, 0, 0.075);
          border: 1px solid rgba(0,0,0,.125);
        }
        .alert {
          margin: 15px 0;
          padding: 12px;
          border-radius: 6px;
          border: 1px solid transparent;
        }
        .alert-success {
          background-color: #d4edda;
          border-color: #c3e6cb;
          color: #155724;
        }
        .alert-warning {
          background-color: #fff3cd;
          border-color: #ffeaa7;
          color: #856404;
        }
        .alert-info {
          background-color: #d1ecf1;
          border-color: #bee5eb;
          color: #0c5460;
        }
        .badge-secondary {
          background-color: #6c757d;
          color: white;
          padding: 4px 8px;
          border-radius: 4px;
          font-size: 12px;
        }
        .btn-success { background-color: #28a745; border-color: #28a745; }
        .btn-primary { background-color: #007bff; border-color: #007bff; }
        .section-header {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 20px;
          margin-bottom: 30px;
          border-radius: 8px;
        }
      "))
    ),

    # Header Section
    div(class = "section-header",
      h1("💰 Cost-Effectiveness Analysis", style = "margin: 0; font-weight: 300;"),
      p("Analyze intervention strategies for cost-effectiveness",
        style = "margin: 10px 0 0 0; opacity: 0.9; font-size: 18px;")
    ),

    # Main Content
    mod_input_ui("input_module"),

    # Results Section (initially hidden)
    div(id = "results_section", style = "display: none;",
      hr(),
      h2("📊 Analysis Results"),
      uiOutput("results_content")
    )
  ),

  # Help Tab
  tabPanel("Help",
    icon = icon("question-circle"),

    div(class = "container-fluid", style = "margin-top: 20px;",

      fluidRow(
        column(6,
          div(class = "card",
            div(class = "card-header bg-primary text-white",
              h4("🚀 Getting Started", style = "margin: 0;")
            ),
            div(class = "card-body",
              h5("1. Define Strategies"),
              p("Enter your intervention strategies with costs and health effects from your disease model."),

              h5("2. Set Parameters"),
              p("Choose outcome type (QALYs, DALYs, etc.) and cost-effectiveness threshold."),

              h5("3. Run Analysis"),
              p("Click 'Run Analysis' to generate ICER tables and interpretations."),

              hr(),

              h5("Input Requirements"),
              tags$ul(
                tags$li("Strategy name (e.g., 'Mass Vaccination')"),
                tags$li("Total cost in USD"),
                tags$li("Health effect (QALYs, DALYs, lives saved, etc.)")
              )
            )
          )
        ),

        column(6,
          div(class = "card",
            div(class = "card-header bg-info text-white",
              h4("📈 Understanding Results", style = "margin: 0;")
            ),
            div(class = "card-body",
              h5("ICER (Incremental Cost-Effectiveness Ratio)"),
              p("Cost per additional unit of health outcome compared to the reference strategy."),

              h5("Cost-Effectiveness Thresholds (Kenya)"),
              tags$ul(
                tags$li("0.5× GDP per capita: KES 154,000 per QALY/DALY"),
                tags$li("SHA Hospital Rates per day averted: Level 3 (KES 2,240), Level 4 (KES 3,360), Level 5 (KES 3,920), Level 6 (KES 4,480)")
              ),

              h5("Interpretation"),
              p(tags$strong("Cost-effective: "), "ICER below threshold"),
              p(tags$strong("Not cost-effective: "), "ICER above threshold"),
              p(tags$strong("Dominated: "), "More expensive, less effective")
            )
          )
        )
      ),

      br(),

      fluidRow(
        column(12,
          div(class = "card",
            div(class = "card-header bg-success text-white",
              h4("📋 Sample Data", style = "margin: 0;")
            ),
            div(class = "card-body",
              h5("Example: Infectious Disease Interventions (Kenya)"),
              DT::dataTableOutput("sample_table")
            )
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {

  # Reactive values for storing results
  values <- reactiveValues(last_results = NULL)

  # Input module
  input_data <- mod_input_server("input_module")

  # Sample table for help
  output$sample_table <- DT::renderDataTable({
    sample_data <- create_sample_data()
    sample_data$ICER <- c("Reference", "KES 10,000", "KES 8,293")

    DT::datatable(sample_data,
      options = list(
        dom = 't',
        pageLength = 10,
        scrollX = TRUE,
        searching = FALSE,
        ordering = FALSE
      ),
      rownames = FALSE
    ) %>%
    DT::formatCurrency(c("cost"), currency = "KES ", digits = 0)
  }, server = FALSE)

  # Results handling with better error debugging
  observeEvent(input_data$run_trigger(), {
    req(input_data$analysis_ready())

    # Get input data
    strategies <- input_data$strategies_data()
    params <- input_data$parameters()

    cat("Running analysis with data:\n")
    print(strategies)
    cat("Parameters:\n")
    print(params)

    # Perform ICER calculation with detailed error handling
    tryCatch({

      # Debug: Check data before passing to dampack
      if (nrow(strategies) < 2) {
        stop("Need at least 2 strategies")
      }

      # Test dampack directly with minimal example
      cat("Testing dampack calculation...\n")
      icer_result <- dampack::calculate_icers(
        cost = strategies$cost,
        effect = strategies$effect,
        strategies = strategies$strategy
      )

      cat("dampack result:\n")
      print(icer_result)

      # Show success message
      showNotification(
        "✅ Analysis completed successfully!",
        duration = 5,
        type = "message"
      )

      # Store results and show results section
      values$last_results <- icer_result
      show("results_section")

    }, error = function(e) {
      cat("Error in analysis:", e$message, "\n")
      print(e)

      showNotification(
        paste("❌ Analysis failed:", e$message),
        duration = 8,
        type = "warning"
      )
    })
  })

  # Render ICER results table
  output$results_content <- renderUI({
    req(values$last_results)

    # Create table directly in renderUI (no circular reference)
    results_table <- DT::datatable(values$last_results,
      options = list(
        dom = 't',
        pageLength = 10,
        scrollX = FALSE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    ) %>%
    DT::formatCurrency(c("Cost", "Inc_Cost"), currency = "KES ", digits = 0) %>%
    DT::formatRound(c("Effect", "Inc_Effect"), digits = 1) %>%
    DT::formatCurrency("ICER", currency = "KES ", digits = 0)

    div(class = "card",
      div(class = "card-header bg-primary text-white",
        h4("✅ ICER Results", style = "margin: 0;")
      ),
      div(class = "card-body",
        p("Incremental Cost-Effectiveness Ratios vs. reference strategy:"),
        results_table,
        br(),

        # Cost-effectiveness interpretation
        div(class = "card",
          div(class = "card-header bg-success text-white",
            h5("💡 Interpretation & Recommendations", style = "margin: 0;")
          ),
          div(class = "card-body",
            # Get parameters for interpretation
            tags$div(
              style = "white-space: pre-wrap;",
              HTML(markdown::renderMarkdown(
                text = generate_cea_interpretation(
                  list(results = values$last_results, success = TRUE),
                  input_data$parameters()
                )
              ))
            )
          )
        )
      )
    )
  })

}

# Run the app
shinyApp(ui = ui, server = server)