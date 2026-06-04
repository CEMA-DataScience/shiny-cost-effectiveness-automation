# CEA Functions - Core cost-effectiveness analysis logic
# Wrapper functions around dampack for the Shiny app

#' Validate CEA Input Data
#' @param strategies_df Data frame with columns: strategy, cost, effect
#' @return List with valid (TRUE/FALSE) and message
validate_cea_data <- function(strategies_df) {

  # Check required columns
  required_cols <- c("strategy", "cost", "effect")
  missing_cols <- setdiff(required_cols, names(strategies_df))

  if (length(missing_cols) > 0) {
    return(list(
      valid = FALSE,
      message = paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    ))
  }

  # Check for at least 2 strategies
  if (nrow(strategies_df) < 2) {
    return(list(
      valid = FALSE,
      message = "At least 2 strategies required for comparison"
    ))
  }

  # Check for valid numeric values
  if (any(is.na(strategies_df$cost)) || any(strategies_df$cost < 0)) {
    return(list(
      valid = FALSE,
      message = "All costs must be non-negative numbers"
    ))
  }

  if (any(is.na(strategies_df$effect)) || any(strategies_df$effect < 0)) {
    return(list(
      valid = FALSE,
      message = "All effects must be non-negative numbers"
    ))
  }

  # Check for duplicate strategy names
  if (anyDuplicated(strategies_df$strategy)) {
    return(list(
      valid = FALSE,
      message = "Strategy names must be unique"
    ))
  }

  return(list(valid = TRUE, message = "Data validation passed"))
}

#' Run PSA using dampack.
#'
#' Generates parameter samples with dampack::gen_psa_samp() then runs a
#' pass-through model with dampack::run_psa(). Returns a dampack psa object
#' whose $cost and $effectiveness matrices (n_iter × n_strategies) are used
#' directly for the CE plane cloud and CEAC.
#'
#' Distributions:
#'   Cost   — Gamma(shape, scale); always positive, right-skewed.
#'   Effect — Normal, truncated at 0 inside the model function.
#'
#' SD source (cost and effect):
#'   Synthesis ≥2 studies: (observed max − min) / 3.92
#'   Otherwise:            20% CV
#'
#' @param strategies_df  Data frame: strategy, cost, effect
#' @param n_iter         PSA iterations (default 1 000)
#' @param prov           Named list from synth_pool_all() — carries low/high ranges
#' @return dampack psa object, or NULL on failure
generate_psa_samples <- function(strategies_df, n_iter = 1000L, prov = NULL) {

  strategies <- strategies_df$strategy
  n_strat    <- length(strategies)

  cost_mat <- matrix(NA_real_, nrow = n_iter, ncol = n_strat)
  eff_mat  <- matrix(NA_real_, nrow = n_iter, ncol = n_strat)

  for (i in seq_len(n_strat)) {
    strat     <- strategies[i]
    cost_mean <- strategies_df$cost[i]
    eff_mean  <- strategies_df$effect[i]

    p            <- if (!is.null(prov)) prov[[strat]] else NULL
    has_cost_rng <- !is.null(p) && isTRUE(p$n >= 2L) &&
                    is.finite(p$low_ppp) && is.finite(p$high_ppp) &&
                    p$high_ppp > p$low_ppp
    has_eff_rng  <- !is.null(p) && isTRUE(p$n >= 2L) &&
                    is.finite(p$low_effect) && is.finite(p$high_effect) &&
                    p$high_effect > p$low_effect

    cost_sd <- if (has_cost_rng) (p$high_ppp    - p$low_ppp)    / 3.92 else cost_mean * 0.20
    eff_sd  <- if (has_eff_rng)  (p$high_effect - p$low_effect) / 3.92 else eff_mean  * 0.20

    if (cost_mean <= 0 || cost_sd <= 0) {
      cost_mat[, i] <- pmax(cost_mean, 0)
    } else {
      cost_shape    <- (cost_mean / cost_sd)^2
      cost_scale    <- cost_sd^2 / cost_mean
      cost_mat[, i] <- rgamma(n_iter, shape = cost_shape, scale = cost_scale)
    }

    if (eff_mean <= 0 || eff_sd <= 0) {
      eff_mat[, i] <- pmax(eff_mean, 0)
    } else {
      eff_mat[, i] <- pmax(rnorm(n_iter, mean = eff_mean, sd = eff_sd), 0)
    }
  }

  cost_df <- as.data.frame(cost_mat)
  eff_df  <- as.data.frame(eff_mat)
  names(cost_df) <- strategies
  names(eff_df)  <- strategies

  param_df        <- cbind(cost_df, eff_df)
  names(param_df) <- c(paste0(strategies, "_cost"), paste0(strategies, "_effect"))

  tryCatch(
    dampack::make_psa_obj(
      cost          = cost_df,
      effectiveness = eff_df,
      parameters    = param_df,
      strategies    = strategies,
      currency      = "KES"
    ),
    error = function(e) { message("PSA object creation failed: ", e$message); NULL }
  )
}

#' Calculate ICERs using dampack
#' @param strategies_df Data frame with strategy, cost, effect columns
#' @param ref_strategy Name of reference strategy (optional)
#' @return List with ICER results and summary
calculate_icers <- function(strategies_df, ref_strategy = NULL) {

  # Validate data first
  validation <- validate_cea_data(strategies_df)
  if (!validation$valid) {
    stop(validation$message)
  }

  strategies_df <- strategies_df[order(strategies_df$cost), ]

  # Prepare data for dampack
  cost <- strategies_df$cost
  effect <- strategies_df$effect
  strategies <- strategies_df$strategy

  tryCatch({

    # Calculate ICERs using dampack
    icer_results <- dampack::calculate_icers(
      cost = cost,
      effect = effect,
      strategies = strategies
    )

    # Add cost-effectiveness status based on common thresholds
    # This will be made dynamic later
    default_threshold <- 50000
    icer_results$cost_effective <- ifelse(
      is.na(icer_results$ICER),
      "Reference",
      ifelse(icer_results$ICER <= default_threshold, "Cost-effective", "Not cost-effective")
    )

    # Create summary
    summary_stats <- list(
      n_strategies = nrow(strategies_df),
      reference = ref_strategy,
      cost_effective_count = sum(icer_results$cost_effective == "Cost-effective", na.rm = TRUE)
    )

    return(list(
      results = icer_results,
      summary = summary_stats,
      success = TRUE
    ))

  }, error = function(e) {
    return(list(
      results = NULL,
      summary = NULL,
      success = FALSE,
      error = paste("ICER calculation failed:", e$message)
    ))
  })
}

#' Prepare data for Cost-Effectiveness Plane
#' @param strategies_df Data frame with strategy, cost, effect columns
#' @param ref_strategy Reference strategy name
#' @return Data frame ready for plotting
prepare_ce_plane_data <- function(strategies_df, ref_strategy = NULL) {

  if (is.null(ref_strategy)) {
    ref_strategy <- strategies_df$strategy[which.min(strategies_df$cost)]
  }

  # Get reference values
  ref_cost <- strategies_df$cost[strategies_df$strategy == ref_strategy]
  ref_effect <- strategies_df$effect[strategies_df$strategy == ref_strategy]

  # Calculate incremental values
  strategies_df$incremental_cost <- strategies_df$cost - ref_cost
  strategies_df$incremental_effect <- strategies_df$effect - ref_effect

  # Calculate ICER for plotting
  strategies_df$icer <- ifelse(
    strategies_df$incremental_effect == 0,
    NA,
    strategies_df$incremental_cost / strategies_df$incremental_effect
  )

  # Add plotting categories
  strategies_df$is_reference <- strategies_df$strategy == ref_strategy

  return(strategies_df)
}

#' Generate summary text for results
#' @param icer_results Results from calculate_icers
#' @param threshold Cost-effectiveness threshold
#' @return Character string with interpretation
generate_cea_summary <- function(icer_results, threshold = 50000) {

  if (!icer_results$success) {
    return("Analysis failed. Please check your input data.")
  }

  results_df <- icer_results$results
  n_cost_effective <- sum(!is.na(results_df$ICER) & results_df$ICER <= threshold)
  n_total <- nrow(results_df) - 1  # Exclude reference

  if (n_cost_effective == 0) {
    summary_text <- paste0("No interventions are cost-effective at the $",
                          format(threshold, big.mark = ","), " threshold.")
  } else if (n_cost_effective == n_total) {
    summary_text <- paste0("All ", n_total, " interventions are cost-effective at the $",
                          format(threshold, big.mark = ","), " threshold.")
  } else {
    summary_text <- paste0(n_cost_effective, " of ", n_total,
                          " interventions are cost-effective at the $",
                          format(threshold, big.mark = ","), " threshold.")
  }

  return(summary_text)
}

#' Generate cost-effectiveness interpretation for Kenyan context
#' @param icer_results Results from calculate_icers
#' @param params Analysis parameters including threshold info
#' @return Character string with detailed interpretation
generate_cea_interpretation <- function(icer_results, params) {

  if (!icer_results$success) {
    return("Analysis failed. Please check your input data.")
  }

  results_df <- icer_results$results
  threshold <- params$threshold
  outcome_type <- params$outcome_type
  threshold_type <- params$threshold_type

  # Get threshold description
  threshold_desc <- if (threshold_type == "gdp") {
    "0.5× GDP per capita (KES 154,000)"
  } else if (threshold_type == "sha") {
    sha_level <- switch(as.character(params$sha_level),
      "2240" = "Level 3", "3360" = "Level 4",
      "3920" = "Level 5", "4480" = "Level 6")
    paste0("SHA ", sha_level, " rate (KES ", format(threshold, big.mark = ","), " per day averted)")
  } else {
    paste0("custom threshold (KES ", format(threshold, big.mark = ","), ")")
  }

  # Outcome unit
  outcome_unit <- switch(outcome_type,
    "qaly" = "QALY", "daly" = "DALY", "lyg" = "life year gained",
    "lives" = "life saved", "hosp_days" = "day of hospitalisation averted")

  # Analyze each intervention
  interventions <- results_df[results_df$Strategy != results_df$Strategy[1], ]  # Exclude reference

  interpretation_parts <- c()

  # Header
  interpretation_parts <- c(interpretation_parts,
    paste0("**Cost-Effectiveness Analysis Results:**"),
    paste0("Threshold: ", threshold_desc),
    paste0("Outcome: ", outcome_unit), "")

  # Reference strategy
  ref_strategy <- results_df$Strategy[1]
  interpretation_parts <- c(interpretation_parts,
    paste0("**Reference Strategy:** ", ref_strategy), "")

  # Analyze each intervention
  for (i in 1:nrow(interventions)) {
    strategy <- interventions$Strategy[i]
    icer_value <- interventions$ICER[i]
    status <- interventions$Status[i]

    if (is.na(icer_value) || status == "D") {
      interpretation_parts <- c(interpretation_parts,
        paste0("• **", strategy, ":** Dominated - more expensive and less effective than other options."))
    } else if (icer_value <= threshold) {
      interpretation_parts <- c(interpretation_parts,
        paste0("• **", strategy, ":** Cost-effective (KES ", format(round(icer_value), big.mark = ","),
               " per ", outcome_unit, ") - **RECOMMENDED**."))
    } else {
      interpretation_parts <- c(interpretation_parts,
        paste0("• **", strategy, ":** Not cost-effective (KES ", format(round(icer_value), big.mark = ","),
               " per ", outcome_unit, ") - exceeds threshold."))
    }
  }

  # Overall recommendation
  cost_effective_strategies <- interventions$Strategy[
    !is.na(interventions$ICER) & interventions$ICER <= threshold & interventions$Status != "D"
  ]

  interpretation_parts <- c(interpretation_parts, "")

  if (length(cost_effective_strategies) == 0) {
    interpretation_parts <- c(interpretation_parts,
      "**RECOMMENDATION:** No interventions are cost-effective at this threshold. Consider the status quo.")
  } else if (length(cost_effective_strategies) == 1) {
    interpretation_parts <- c(interpretation_parts,
      paste0("**RECOMMENDATION:** Implement **", cost_effective_strategies[1],
             "** as it is the only cost-effective intervention."))
  } else {
    # Find most cost-effective
    best_strategy <- interventions$Strategy[
      which.min(interventions$ICER[!is.na(interventions$ICER) & interventions$Status != "D"])
    ]
    interpretation_parts <- c(interpretation_parts,
      paste0("**RECOMMENDATION:** **", best_strategy,
             "** is the most cost-effective intervention, followed by: ",
             paste(cost_effective_strategies[cost_effective_strategies != best_strategy], collapse = ", "), "."))
  }

  return(paste(interpretation_parts, collapse = "\n"))
}

#' Create sample strategy data for testing - Kenyan context
#' @return Data frame with example strategies in KES
create_sample_data <- function() {
  data.frame(
    strategy = c("Status Quo", "Mass Vaccination", "Community Health Education"),
    cost     = c(2500000, 12500000, 6800000),
    effect   = c(450, 1250, 820),
    stringsAsFactors = FALSE
  )
}

#' Published study database for Evidence Synthesis
#' Each row is one study reporting cost + effect for one strategy.
#' Costs are in the study's original currency; standardisation to KES 2027
#' is performed by synth_standardize() in synth_functions.R.
#' @return Data frame with columns: id, strategy, author, journal, year,
#'   currency, cost, effect, n
create_sample_studies_data <- function() {
  data.frame(
    id       = c("s1",               "s2",             "s3",
                 "s4",                        "s5",              "s6"),
    strategy = c("Mass Vaccination", "Mass Vaccination", "Mass Vaccination",
                 "Community Health Education", "Community Health Education", "Status Quo"),
    author   = c("Ochieng et al.",  "Sharma et al.",  "van der Merwe et al.",
                 "Mbeki et al.",    "Wanjiru et al.", "MoH Kenya (baseline)"),
    journal  = c("PLOS Med",        "Value Health",   "Cost Eff Resour Alloc",
                 "Trop Med Int Health", "East Afr Med J", "National HTA report"),
    year     = c(2019L, 2021L, 2018L, 2020L, 2022L, 2023L),
    currency = c("USD", "INR", "ZAR", "TZS", "KES", "KES"),
    cost     = c(142000, 9800000, 1850000, 95000000, 5400000, 2300000),
    effect   = c(1180,   1320,    1090,    760,       880,     450),
    n        = c(520L,   880L,    410L,    300L,      640L,    1000L),
    stringsAsFactors = FALSE
  )
}