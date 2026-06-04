# synth_functions.R
# Business logic for the Evidence Synthesis module.
#
# Cost standardisation follows a three-step procedure:
#   Step 1 — inflate source cost from study year to PPP reference year
#             in the original currency, using the source-country inflation series.
#   Step 2 — convert to KES using the World Bank ICP PPP factor for the
#             PPP reference year.
#   Step 3 — inflate the KES cost from the PPP reference year to the
#             target year using Kenya's inflation series.
# An equivalent three-step procedure applies for the exchange-rate path,
# pivoting on the CBK anchor date rather than the PPP reference year.
#
# Inflation is applied year-by-year where historical data are available.
# Years with no data (typically the most recent year and future years)
# use the mean of the available historical series as a projection.

# ── Internal: compound inflation factor ──────────────────────────────────────
# Computes the compound growth factor from from_year to to_year for a given
# currency, using the raw annual-rate series stored in factors$inflation.
# For years with no data the mean of the available series is used.
#
# @param currency   ISO 4217 currency code
# @param from_year  Integer start year (inclusive)
# @param to_year    Integer end year (exclusive, i.e. compound up to to_year - 1)
# @param inflation  Named list: currency → named numeric (year_str → rate fraction)
# @return Numeric compound factor (>= 1 for positive inflation)

.compound_factor <- function(currency, from_year, to_year, inflation) {
  from_year <- as.integer(from_year)
  to_year   <- as.integer(to_year)
  if (is.na(from_year) || is.na(to_year) || from_year >= to_year) return(1)

  raw <- inflation[[currency]]
  if (is.null(raw) || !is.numeric(raw) || length(raw) == 0L) return(1)

  valid <- raw[is.finite(raw)]
  if (length(valid) == 0L) return(1)

  mean_rate <- mean(valid)   # used as projection for years with no data

  compound <- 1
  for (yr in seq(from_year, to_year - 1L)) {
    rate <- valid[as.character(yr)]
    compound <- compound * (1 + if (!is.na(rate)) rate else mean_rate)
  }
  compound
}

#' Standardise a single study row to target-year KES.
#'
#' Applies the three-step procedure for both the PPP and exchange-rate paths.
#'
#' @param study_row   Single-row data.frame from the studies table
#' @param factors     Output of load_factors()
#' @param target_year Integer target year
#' @return Named list:
#'   $inflated_original   Cost in source currency at PPP reference year (Step 1 result)
#'   $cf_source_to_ppp    Compound factor for Step 1 (PPP path)
#'   $cf_kes_to_target    Compound factor for Step 3 (PPP path, KES leg)
#'   $kes_ppp             Final KES cost via PPP path (all three steps)
#'   $kes_fx              Final KES cost via exchange-rate path (all three steps)
#'   $ppp_to_kes          PPP conversion rate used (KES per 1 unit of source currency)
#'   $fx_to_kes           Exchange rate used (KES per 1 unit of source currency)
#'   $ppp_fx_diff_pct     Percent by which PPP cost exceeds FX cost; NA if either unavailable
synth_standardize <- function(study_row, factors, target_year = TARGET_YEAR) {
  currency <- as.character(study_row$currency)[1L]
  ppp_year <- factors$ppp$year
  fx_year  <- as.integer(format(factors$fx$date, "%Y"))

  # ── PPP path ────────────────────────────────────────────────────────────────
  # Step 1: inflate source currency from study year to PPP reference year
  cf1  <- .compound_factor(currency, study_row$year, ppp_year, factors$inflation)
  c_ppp_yr <- study_row$cost * cf1

  # Step 2: PPP conversion to KES at PPP reference year
  ppp_rate    <- factors$ppp$rates[[currency]]
  c_kes_ppp_yr <- if (!is.null(ppp_rate) && is.finite(ppp_rate))
    c_ppp_yr * ppp_rate else NA_real_

  # Step 3: inflate KES from PPP reference year to target year
  cf3_ppp <- .compound_factor("KES", ppp_year, target_year, factors$inflation)
  kes_ppp <- c_kes_ppp_yr * cf3_ppp

  # ── FX path ─────────────────────────────────────────────────────────────────
  # Step 1: inflate source currency from study year to FX anchor year
  cf1_fx   <- .compound_factor(currency, study_row$year, fx_year, factors$inflation)
  c_fx_yr  <- study_row$cost * cf1_fx

  # Step 2: exchange-rate conversion to KES at anchor year
  fx_rate      <- factors$fx$rates[[currency]]
  c_kes_fx_yr  <- if (!is.null(fx_rate) && is.finite(fx_rate))
    c_fx_yr * fx_rate else NA_real_

  # Step 3: inflate KES from FX anchor year to target year
  cf3_fx  <- .compound_factor("KES", fx_year, target_year, factors$inflation)
  kes_fx  <- c_kes_fx_yr * cf3_fx

  # ── Diagnostics ─────────────────────────────────────────────────────────────
  diff_pct <- if (!is.na(kes_ppp) && !is.na(kes_fx) && is.finite(kes_fx) && kes_fx != 0)
    (kes_ppp - kes_fx) / kes_fx * 100 else NA_real_

  list(
    inflated_original  = c_ppp_yr,       # Step 1: source currency at PPP year
    kes_ppp_yr         = c_kes_ppp_yr,   # Step 2: KES at PPP reference year
    kes_ppp            = kes_ppp,        # Step 3: KES at target year (2027)
    cf_source_to_ppp   = cf1,
    cf_kes_to_target   = cf3_ppp,
    ppp_to_kes         = ppp_rate,
    # FX path retained for provenance but not displayed in main table
    kes_fx             = kes_fx,
    fx_to_kes          = fx_rate,
    ppp_fx_diff_pct    = diff_pct
  )
}

#' Pool studies for one strategy group.
#' Returns both (a) ICER computed from pooled cost and effect, and
#' (b) mean of per-study ICERs — so callers can display both and flag divergence.
#'
#' @param strat_studies  Rows of the studies table for one strategy
#' @param std_list       List of synth_standardize() outputs (same order)
#' @param ref_pooled     Pooled result for the reference strategy (enables ICER computation)
#' @param method         "weighted" | "mean" | "ivw"
synth_pool <- function(strat_studies, std_list, ref_pooled = NULL, method = "weighted") {
  ns    <- strat_studies$n
  sum_n <- sum(ns)

  costs_ppp    <- vapply(std_list, `[[`, numeric(1L), "kes_ppp")
  costs_ppp_yr <- vapply(std_list, `[[`, numeric(1L), "kes_ppp_yr")
  costs_fx     <- vapply(std_list, `[[`, numeric(1L), "kes_fx")
  effects      <- strat_studies$effect

  .pool <- function(costs, wts, m) {
    valid <- is.finite(costs)
    if (!any(valid)) return(NA_real_)
    cv <- costs[valid]; wv <- wts[valid]
    switch(m,
      mean     = mean(cv),
      weighted = sum(cv * wv) / sum(wv),
      ivw      = { w <- wv^2; sum(cv * w) / sum(w) }
    )
  }

  cost_ppp    <- .pool(costs_ppp,    ns, method)
  cost_ppp_yr <- .pool(costs_ppp_yr, ns, method)
  cost_fx     <- .pool(costs_fx,     ns, method)
  effect      <- sum(effects * ns) / sum_n

  icer_pooled_ppp <- if (!is.null(ref_pooled) && is.finite(cost_ppp)) {
    inc_c <- cost_ppp - ref_pooled$cost_ppp
    inc_e <- effect   - ref_pooled$effect
    if (is.finite(inc_e) && inc_e > 0) inc_c / inc_e else NA_real_
  } else NA_real_

  icer_pooled_fx <- if (!is.null(ref_pooled) && is.finite(cost_fx)) {
    inc_c <- cost_fx - ref_pooled$cost_fx
    inc_e <- effect  - ref_pooled$effect
    if (is.finite(inc_e) && inc_e > 0) inc_c / inc_e else NA_real_
  } else NA_real_

  mean_icer_ppp <- if (!is.null(ref_pooled) && any(is.finite(costs_ppp))) {
    study_icers <- (costs_ppp - ref_pooled$cost_ppp) / (effects - ref_pooled$effect)
    study_icers <- study_icers[is.finite(study_icers)]
    if (length(study_icers) > 0L) mean(study_icers) else NA_real_
  } else NA_real_

  mean_icer_fx <- if (!is.null(ref_pooled) && any(is.finite(costs_fx))) {
    study_icers <- (costs_fx - ref_pooled$cost_fx) / (effects - ref_pooled$effect)
    study_icers <- study_icers[is.finite(study_icers)]
    if (length(study_icers) > 0L) mean(study_icers) else NA_real_
  } else NA_real_

  icer_method_diff_pct <- if (is.finite(icer_pooled_ppp) && is.finite(mean_icer_ppp) &&
                               mean_icer_ppp != 0)
    (icer_pooled_ppp - mean_icer_ppp) / abs(mean_icer_ppp) * 100 else NA_real_

  list(
    cost_ppp             = cost_ppp,
    cost_ppp_yr          = cost_ppp_yr,
    cost_fx              = cost_fx,
    effect               = effect,
    low_ppp              = if (any(is.finite(costs_ppp))) min(costs_ppp[is.finite(costs_ppp)]) else NA_real_,
    high_ppp             = if (any(is.finite(costs_ppp))) max(costs_ppp[is.finite(costs_ppp)]) else NA_real_,
    low_effect           = if (length(effects) >= 2L) min(effects) else NA_real_,
    high_effect          = if (length(effects) >= 2L) max(effects) else NA_real_,
    low_fx               = if (any(is.finite(costs_fx)))  min(costs_fx[is.finite(costs_fx)])  else NA_real_,
    high_fx              = if (any(is.finite(costs_fx)))  max(costs_fx[is.finite(costs_fx)])  else NA_real_,
    n                    = nrow(strat_studies),
    sum_n                = sum_n,
    icer_pooled_ppp      = icer_pooled_ppp,
    icer_pooled_fx       = icer_pooled_fx,
    mean_icer_ppp        = mean_icer_ppp,
    mean_icer_fx         = mean_icer_fx,
    icer_method_diff_pct = icer_method_diff_pct,
    ppp_fx_diff_pct      = if (is.finite(cost_ppp) && is.finite(cost_fx) && cost_fx != 0)
                             (cost_ppp - cost_fx) / cost_fx * 100 else NA_real_,
    std_list             = std_list,
    method               = method
  )
}

#' Pool all strategies and compute cross-strategy ICERs.
#' Reference strategy is identified as the one with the lowest pooled PPP cost.
#'
#' @param studies  Studies data frame (from cea_functions.R)
#' @param factors  Output of load_factors()
#' @param method   Pooling method
#' @return Named list: strategy → synth_pool() result, with ICER fields populated
synth_pool_all <- function(studies, factors, method = "weighted") {
  strategies <- unique(studies$strategy)

  pools <- lapply(setNames(strategies, strategies), function(strat) {
    rows     <- studies[studies$strategy == strat, ]
    std_list <- lapply(seq_len(nrow(rows)),
                       function(i) synth_standardize(rows[i, ], factors))
    synth_pool(rows, std_list, ref_pooled = NULL, method = method)
  })

  ppp_costs <- vapply(pools, `[[`, numeric(1L), "cost_ppp")
  ref_strat <- names(which.min(ppp_costs))
  ref       <- pools[[ref_strat]]

  for (strat in strategies) {
    rows     <- studies[studies$strategy == strat, ]
    std_list <- pools[[strat]]$std_list
    pools[[strat]] <- synth_pool(rows, std_list,
                                 ref_pooled = if (strat == ref_strat) NULL else ref,
                                 method = method)
  }

  attr(pools, "reference") <- ref_strat
  pools
}

# ── Formatting helpers ─────────────────────────────────────────────────────────

fmt_kes <- function(x, digits = 0L) {
  if (is.na(x) || !is.finite(x)) return("—")
  paste0("KES ", formatC(round(x, digits), format = "f", digits = digits, big.mark = ","))
}

fmt_cur <- function(x, code, digits = 0L) {
  if (is.na(x) || !is.finite(x)) return("—")
  paste0(code, " ", formatC(round(x, digits), format = "f", digits = digits, big.mark = ","))
}

fmt_pct <- function(x, digits = 1L) {
  if (is.na(x) || !is.finite(x)) return("—")
  sprintf("%+.1f%%", round(x, digits))
}

fmt_icer <- function(x) {
  if (is.na(x) || !is.finite(x)) return("—")
  fmt_kes(x)
}
