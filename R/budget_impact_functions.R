# budget_impact_functions.R
# Plain calculation logic for the Budget Impact tab — no Shiny/plotly deps.

#' Compute Budget Impact (Reference vs New) over a multi-year horizon.
#'
#' Reference scenario assumes the target population stays entirely on the
#' reference (lowest-cost) strategy. New scenario applies the supplied
#' per-strategy, per-year uptake shares; the reference strategy's New-scenario
#' share each year is the residual (1 - sum of other shares).
#'
#' @param strategies_df Data frame: strategy (chr), cost (num, per-case). Row
#'   order does not matter — sorted internally lowest-cost-first, same
#'   reference convention as compute_price_threshold_data() in cea_functions.R.
#' @param target_population Numeric scalar, constant across the horizon.
#' @param uptake_ramp Data frame: strategy (chr, non-reference strategies),
#'   one column per year named year_1, year_2, ... — values are shares in
#'   [0, 1]. Missing strategies/years default to 1 (100%).
#' @param time_horizon_years Integer, number of years (1-5).
#' @return List:
#'   by_year          — df: year, reference_total, new_total, budget_impact, cumulative_budget_impact
#'   by_strategy_year — df: year, strategy, is_reference, cost_per_case, share, contribution
#'   ref_strategy, time_horizon_years
compute_budget_impact_data <- function(strategies_df, target_population,
                                        uptake_ramp, time_horizon_years) {
  df  <- strategies_df[order(strategies_df$cost), ]
  ref <- df[1L, ]
  non_ref <- df[-1L, , drop = FALSE]

  by_strategy_year <- list()
  by_year <- list()
  cumulative <- 0

  for (y in seq_len(time_horizon_years)) {
    ycol <- paste0("year_", y)
    shares <- setNames(
      if (ycol %in% names(uptake_ramp)) uptake_ramp[[ycol]] else rep(1, nrow(non_ref)),
      uptake_ramp$strategy
    )
    shares <- shares[non_ref$strategy]
    shares[is.na(shares)] <- 1
    shares <- pmax(shares, 0)
    ref_share <- 1 - sum(shares)

    new_total <- target_population * (sum(shares * non_ref$cost) + max(ref_share, 0) * ref$cost)
    reference_total <- target_population * ref$cost
    bi <- new_total - reference_total
    cumulative <- cumulative + bi

    by_year[[y]] <- data.frame(
      year = y, reference_total = reference_total, new_total = new_total,
      budget_impact = bi, cumulative_budget_impact = cumulative
    )

    by_strategy_year[[length(by_strategy_year) + 1L]] <- data.frame(
      year = y, strategy = ref$strategy, is_reference = TRUE,
      cost_per_case = ref$cost, share = ref_share,
      contribution = target_population * max(ref_share, 0) * ref$cost,
      stringsAsFactors = FALSE
    )
    for (i in seq_len(nrow(non_ref))) {
      by_strategy_year[[length(by_strategy_year) + 1L]] <- data.frame(
        year = y, strategy = non_ref$strategy[i], is_reference = FALSE,
        cost_per_case = non_ref$cost[i], share = shares[non_ref$strategy[i]],
        contribution = target_population * shares[non_ref$strategy[i]] * non_ref$cost[i],
        stringsAsFactors = FALSE
      )
    }
  }

  list(
    by_year            = do.call(rbind, by_year),
    by_strategy_year   = do.call(rbind, by_strategy_year),
    ref_strategy       = ref$strategy,
    time_horizon_years = time_horizon_years
  )
}
