# Input Module - Strategy input and parameter setting
# UI and Server functions for the analysis setup

#' Input Module UI
#' @param id Module namespace ID
mod_input_ui <- function(id) {
  ns <- NS(id)

  div(class = "container-fluid",

    # Analysis Setup Header
    fluidRow(
      column(12,
        div(class = "alert alert-info",
          icon("info-circle"),
          " Configure your cost-effectiveness analysis below, then click 'Run Analysis'"
        )
      )
    ),

    fluidRow(

      # Strategy Input Panel
      column(8,
        div(class = "card",
          div(class = "card-header bg-primary text-white",
            h4("📊 Define Strategies", style = "margin: 0;")
          ),
          div(class = "card-body",

            p("💡 Double-click any cell in the table below to edit strategy data:"),

            # Control buttons
            fluidRow(
              column(4,
                actionButton(ns("add_row"), "Add Strategy",
                            icon = icon("plus"), class = "btn-success")
              ),
              column(4,
                actionButton(ns("remove_row"), "Remove Last",
                            icon = icon("minus"), class = "btn-warning")
              ),
              column(4,
                actionButton(ns("load_sample"), "Load Sample Data",
                            icon = icon("download"), class = "btn-outline-secondary")
              )
            ),

            br(),

            # Editable strategies table
            h5("📊 Strategies:"),
            helpText("First strategy is automatically the reference. Double-click cells to edit."),

            shinycssloaders::withSpinner(
              DT::dataTableOutput(ns("strategies_table"))
            )

          )
          )
        ),

      # Parameters Panel
      column(4,
        div(class = "card",
          div(class = "card-header bg-info text-white",
            h4("⚙️ Analysis Parameters", style = "margin: 0;")
          ),
          div(class = "card-body",

            # Outcome Type
            h5("Health Outcome Type:"),
            radioButtons(ns("outcome_type"), "",
              choices = list(
                "Quality-Adjusted Life Years (QALYs)" = "qaly",
                "Disability-Adjusted Life Years (DALYs)" = "daly",
                "Life Years Gained" = "lyg",
                "Lives Saved" = "lives",
                "Days of Hospitalisation Averted" = "hosp_days"
              ),
              selected = "hosp_days"
            ),

            hr(),

            # ICER Threshold - Kenyan Context
            h5("Cost-Effectiveness Threshold (KES):"),
            div(
              radioButtons(ns("threshold_type"), "Select threshold:",
                choices = list(
                  "0.5× GDP per capita (154,000 KES)" = "gdp",
                  "SHA Hospital Rates (per day averted)" = "sha"
                ),
                selected = "sha"
              ),

              # SHA rates (conditional)
              conditionalPanel(
                condition = paste0("input['", ns("threshold_type"), "'] == 'sha'"),
                selectInput(ns("sha_level"), "Hospital Level:",
                  choices = list(
                    "Level 3 (2,240 KES/day)" = 2240,
                    "Level 4 (3,360 KES/day)" = 3360,
                    "Level 5 (3,920 KES/day)" = 3920,
                    "Level 6 (4,480 KES/day)" = 4480
                  ),
                  selected = 3360
                )
              ),

              # Custom threshold option
              numericInput(ns("custom_threshold"), "Custom threshold (KES):",
                          value = 154000, min = 0, step = 1000),

              helpText("💡 GDP: willing to pay per QALY/DALY; SHA: cost per day of hospitalisation averted")
            ),

            hr(),

            # PSA Options
            h5("Uncertainty Analysis:"),
            div(
              checkboxInput(ns("enable_psa"), "Enable Probabilistic Sensitivity Analysis",
                           value = FALSE),  # Start simple

              conditionalPanel(
                condition = paste0("input['", ns("enable_psa"), "']"),
                br(),
                radioButtons(ns("uncertainty_method"), "Parameter Uncertainty:",
                  choices = list(
                    "Standard ±20% (recommended)" = "standard",
                    "Custom ranges" = "custom"
                  ),
                  selected = "standard"
                ),

                conditionalPanel(
                  condition = paste0("input['", ns("uncertainty_method"), "'] == 'custom'"),
                  numericInput(ns("cost_cv"), "Cost CV (%):", value = 20, min = 0, max = 100),
                  numericInput(ns("effect_cv"), "Effect CV (%):", value = 15, min = 0, max = 100)
                ),

                numericInput(ns("psa_iterations"), "PSA Iterations:",
                            value = 1000, min = 100, max = 10000, step = 100)
              )
            ),

            hr(),

            # Run Analysis Button
            div(style = "text-align: center;",
              actionButton(ns("run_analysis"), "Run Cost-Effectiveness Analysis",
                          icon = icon("calculator"),
                          class = "btn-primary btn-lg",
                          style = "width: 100%; height: 60px; font-size: 16px; font-weight: bold;")
            ),

            br(),

            # Validation Messages
            div(id = ns("validation_messages"))

          )
        )
      )
    )
  )
}

#' Input Module Server
#' @param id Module namespace ID
#' @return Reactive values with analysis inputs
mod_input_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    # Reactive values to store table data
    values <- reactiveValues(
      strategies_data = data.frame(
        strategy = c("Status Quo", "Mass Vaccination"),
        cost = c(125000, 280000),
        effect = c(12.5, 18.2),
        stringsAsFactors = FALSE
      ),
      analysis_ready = FALSE
    )

    # Render editable strategies table
    output$strategies_table <- DT::renderDataTable({

      # Calculate ICER for display
      display_data <- values$strategies_data
      if (nrow(display_data) >= 2) {
        ref_cost <- display_data$cost[1]
        ref_effect <- display_data$effect[1]

        display_data$incremental_cost <- display_data$cost - ref_cost
        display_data$incremental_effect <- display_data$effect - ref_effect
        display_data$icer <- ifelse(
          display_data$incremental_effect == 0,
          "Reference",
          paste0("KES ", format(round(display_data$incremental_cost / display_data$incremental_effect), big.mark = ","))
        )
      } else {
        display_data$icer <- "Reference"
      }

      # Add reference indicator
      display_data$reference <- c("✓", rep("", nrow(display_data) - 1))

      # Reorder columns for display
      display_data <- display_data[, c("strategy", "cost", "effect", "reference", "icer")]
      names(display_data) <- c("Strategy", "Cost (KES)", "Effect", "Ref", "ICER (KES/unit)")

      DT::datatable(
        display_data,
        editable = list(target = 'cell', disable = list(columns = c(3, 4))),  # Can't edit Ref or ICER columns
        options = list(
          dom = 't',
          pageLength = 20,
          scrollX = TRUE,
          searching = FALSE,
          ordering = FALSE,
          autoWidth = FALSE,
          columnDefs = list(
            list(width = '25%', targets = 0),  # Strategy name
            list(width = '20%', targets = 1),  # Cost
            list(width = '15%', targets = 2),  # Effect
            list(width = '8%', targets = 3),   # Reference
            list(width = '32%', targets = 4)   # ICER
          )
        ),
        rownames = FALSE
      ) %>%
      DT::formatCurrency(c("Cost (KES)"), currency = "KES ", digits = 0) %>%
      DT::formatRound(c("Effect"), digits = 0)

    }, server = FALSE)

    # Handle table edits
    observeEvent(input$strategies_table_cell_edit, {
      info <- input$strategies_table_cell_edit

      # Get the edited value
      new_value <- info$value
      row <- info$row
      col <- info$col + 1  # DT is 0-indexed, R is 1-indexed

      # Update the strategies data
      if (col == 1) {  # Strategy name
        values$strategies_data[row, "strategy"] <- new_value
      } else if (col == 2) {  # Cost
        values$strategies_data[row, "cost"] <- as.numeric(new_value)
      } else if (col == 3) {  # Effect
        values$strategies_data[row, "effect"] <- as.numeric(new_value)
      }
    })

    # Add new row
    observeEvent(input$add_row, {
      new_row <- data.frame(
        strategy = paste("Strategy", nrow(values$strategies_data) + 1),
        cost = 0,
        effect = 0,
        stringsAsFactors = FALSE
      )
      values$strategies_data <- rbind(values$strategies_data, new_row)
      showNotification("New strategy added. Double-click cells to edit.", duration = 3, type = "message")
    })

    # Remove last row
    observeEvent(input$remove_row, {
      if (nrow(values$strategies_data) > 2) {
        values$strategies_data <- values$strategies_data[-nrow(values$strategies_data), ]
        showNotification("Last strategy removed.", duration = 2, type = "message")
      } else {
        showNotification("Need at least 2 strategies for comparison.", duration = 3, type = "warning")
      }
    })

    # Load sample data
    observeEvent(input$load_sample, {
      sample_data <- create_sample_data()
      values$strategies_data <- sample_data
      showNotification("Sample data loaded (3 strategies)!", duration = 2, type = "message")
    })

    # Get clean strategies data
    strategies_clean <- reactive({
      values$strategies_data
    })

    # Validation
    validation_result <- reactive({
      validate_cea_data(strategies_clean())
    })

    # Update validation messages
    observe({
      validation <- validation_result()

      if (!validation$valid) {
        output$validation_messages <- renderUI({
          div(class = "alert alert-warning",
            icon("exclamation-triangle"),
            " ", validation$message
          )
        })
        values$analysis_ready <- FALSE
      } else {
        output$validation_messages <- renderUI({
          div(class = "alert alert-success",
            icon("check"),
            " Ready for analysis (", nrow(strategies_clean()), " strategies)"
          )
        })
        values$analysis_ready <- TRUE
      }
    })

    # Return reactive analysis inputs
    list(
      strategies_data = strategies_clean,
      parameters = reactive({

        # Calculate threshold based on selection
        threshold_value <- if (input$threshold_type == "gdp") {
          154000  # 0.5 GDP per capita
        } else if (input$threshold_type == "sha") {
          as.numeric(input$sha_level)  # Selected SHA rate
        } else {
          input$custom_threshold
        }

        list(
          outcome_type = input$outcome_type,
          threshold = threshold_value,
          threshold_type = input$threshold_type,
          sha_level = input$sha_level,
          enable_psa = input$enable_psa,
          uncertainty_method = input$uncertainty_method,
          cost_cv = input$cost_cv,
          effect_cv = input$effect_cv,
          psa_iterations = input$psa_iterations
        )
      }),
      analysis_ready = reactive(values$analysis_ready),
      run_trigger = reactive(input$run_analysis)
    )

  })
}