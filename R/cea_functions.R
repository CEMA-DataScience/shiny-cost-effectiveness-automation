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

  results_df   <- icer_results$results
  threshold    <- params$threshold
  outcome_type <- params$outcome_type

  # Get threshold description from named thresholds vector (if available)
  threshold_desc <- if (!is.null(params$thresholds) && length(params$thresholds) > 0) {
    tv <- params$thresholds
    paste(paste0(names(tv), " (KES ", format(tv, big.mark = ","), ")"), collapse = ", ")
  } else {
    paste0("KES ", format(threshold, big.mark = ","))
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

#' Compute tornado (one-way sensitivity) data for a focal pair.
#'
#' Perturbs each strategy's cost and effect ±20%, recomputes the focal vs
#' reference ICER, and returns a sorted data frame ready for a horizontal bar
#' chart.
#'
#' @param strategies_df Data frame: strategy (chr), cost (num), effect (num).
#'   All strategies required; ref and focal are identified by name.
#' @param ref_name   Name of the reference strategy.
#' @param focal_name Name of the focal (non-reference) strategy.
#' @param threshold  WTP threshold (numeric); carried through for convenience.
#' @return List: tornado_df (parameter, icer_lo, icer_hi, range), base_icer, threshold.
compute_tornado_data <- function(strategies_df, ref_name, focal_name, threshold) {
  .icer_scalar <- function(sd) {
    r_row <- sd[sd$strategy == ref_name,   ]
    f_row <- sd[sd$strategy == focal_name, ]
    if (nrow(r_row) == 0L || nrow(f_row) == 0L) return(NA_real_)
    inc_e <- f_row$effect[1L] - r_row$effect[1L]
    if (!is.finite(inc_e) || inc_e == 0) return(NA_real_)
    (f_row$cost[1L] - r_row$cost[1L]) / inc_e
  }

  base_icer <- .icer_scalar(strategies_df)

  rows <- lapply(seq_len(nrow(strategies_df)), function(i) {
    s   <- strategies_df$strategy[i]
    bc  <- strategies_df$cost[i];   be <- strategies_df$effect[i]
    lo_c <- strategies_df; lo_c$cost[i]   <- bc * 0.80
    hi_c <- strategies_df; hi_c$cost[i]   <- bc * 1.20
    lo_e <- strategies_df; lo_e$effect[i] <- be * 0.80
    hi_e <- strategies_df; hi_e$effect[i] <- be * 1.20
    list(
      data.frame(parameter = paste0(s, ": cost"),
        icer_lo = .icer_scalar(lo_c), icer_hi = .icer_scalar(hi_c),
        stringsAsFactors = FALSE),
      data.frame(parameter = paste0(s, ": effect"),
        icer_lo = .icer_scalar(lo_e), icer_hi = .icer_scalar(hi_e),
        stringsAsFactors = FALSE)
    )
  })

  td <- do.call(rbind, unlist(rows, recursive = FALSE))
  td <- td[is.finite(td$icer_lo) & is.finite(td$icer_hi), ]
  td$range <- abs(td$icer_hi - td$icer_lo)
  td <- td[td$range > 0, ]
  td <- td[order(td$range), ]   # ascending → widest bar at top in plotly

  list(tornado_df = td, base_icer = base_icer, threshold = threshold)
}

#' Compute CEAC data from a dampack PSA object.
#'
#' For each WTP value in a 200-point sweep (0 to 3× max threshold), returns
#' the proportion of PSA iterations in which each strategy has maximum NMB.
#'
#' @param psa        dampack psa object with $strategies, $cost, $effectiveness.
#' @param thresholds Numeric vector of WTP thresholds; sweep extends to 3× max.
#' @return List: wtp_seq (length 200), ceac_mat (200 × n_strat), strategies.
compute_ceac_data <- function(psa, thresholds) {
  strats  <- psa$strategies
  n_strat <- length(strats)
  cost_m  <- as.matrix(psa$cost)
  eff_m   <- as.matrix(psa$effectiveness)

  wtp_seq <- seq(0, max(thresholds, na.rm = TRUE) * 3, length.out = 200)

  ceac_mat <- t(vapply(wtp_seq, function(lambda) {
    nmb  <- eff_m * lambda - cost_m
    best <- max.col(nmb, ties.method = "first")
    vapply(seq_len(n_strat), function(j) mean(best == j), numeric(1L))
  }, numeric(n_strat)))

  list(wtp_seq = wtp_seq, ceac_mat = ceac_mat, strategies = strats)
}

#' Compute price threshold curve data for all non-dominated strategies.
#'
#' For each non-reference, non-dominated strategy, generates the ICER-vs-price
#' curve (100 points), the current ICER marker, and the break-even price and
#' cost headroom at the given WTP threshold.
#'
#' @param strategies_df Data frame: strategy (chr), cost (num), effect (num),
#'   optionally status (chr). Dominated rows (status "D"/"ED") are excluded.
#' @param threshold WTP threshold (numeric).
#' @return List: curves (one list per strategy with prices, icers, icer_now,
#'   cost_now, break_even, headroom), ref_strategy, ref_cost, x_min, x_max,
#'   threshold.
compute_price_threshold_data <- function(strategies_df, threshold) {
  df <- strategies_df[order(strategies_df$cost), ]
  ref <- df[1L, ]

  has_status <- "status" %in% names(df)
  non_ref <- df[seq(2L, nrow(df)), , drop = FALSE]
  non_ref <- non_ref[is.finite(non_ref$cost) & is.finite(non_ref$effect), ]
  if (has_status)
    non_ref <- non_ref[!(toupper(non_ref$status) %in% c("D", "ED")), ]

  empty <- list(curves = list(), ref_strategy = ref$strategy,
                ref_cost = ref$cost, x_min = 0, x_max = 0, threshold = threshold)
  if (nrow(non_ref) == 0L) return(empty)

  x_min <- min(non_ref$cost) * 0.1

  break_evens <- vapply(seq_len(nrow(non_ref)), function(i) {
    inc_e <- non_ref[i, ]$effect - ref$effect
    if (!is.finite(inc_e) || inc_e <= 0) return(non_ref[i, ]$cost)
    ref$cost + threshold * inc_e
  }, numeric(1L))
  x_max <- max(max(non_ref$cost) * 3.0, max(break_evens, na.rm = TRUE)) * 1.15

  curves <- list()
  for (i in seq_len(nrow(non_ref))) {
    s     <- non_ref[i, ]
    inc_e <- s$effect - ref$effect
    if (!is.finite(inc_e) || inc_e <= 0) next

    prices     <- seq(x_min, x_max, length.out = 100)
    icers      <- (prices - ref$cost) / inc_e
    icer_now   <- (s$cost - ref$cost) / inc_e
    break_even <- ref$cost + threshold * inc_e
    headroom   <- break_even - s$cost

    curves[[length(curves) + 1L]] <- list(
      strategy   = s$strategy,
      prices     = prices,
      icers      = icers,
      icer_now   = icer_now,
      cost_now   = s$cost,
      break_even = break_even,
      headroom   = headroom
    )
  }

  list(curves = curves, ref_strategy = ref$strategy, ref_cost = ref$cost,
       x_min = x_min, x_max = x_max, threshold = threshold)
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