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
# Computes the compound growth factor from from_year to to_year for a single
# annual-rate series. For years with no data the mean of the available series
# is used as a projection. Callers resolve which series to use — e.g.
# factors$inflation_by_iso3c[[iso3c]] for the PPP path, or
# factors$inflation[[currency]] for the FX path.
#
# @param series     Named numeric (year_str → rate fraction), or NULL
# @param from_year  Integer start year (inclusive)
# @param to_year    Integer end year (exclusive, i.e. compound up to to_year - 1)
# @return Numeric compound factor (>= 1 for positive inflation); 1 if `series`
#   is unavailable or the range is empty/invalid.

.compound_factor <- function(series, from_year, to_year) {
  from_year <- as.integer(from_year)
  to_year   <- as.integer(to_year)
  if (is.na(from_year) || is.na(to_year) || from_year >= to_year) return(1)

  if (is.null(series) || !is.numeric(series) || length(series) == 0L) return(1)

  valid <- series[is.finite(series)]
  if (length(valid) == 0L) return(1)

  mean_rate <- mean(valid)   # used as projection for years with no data

  compound <- 1
  for (yr in seq(from_year, to_year - 1L)) {
    rate <- valid[as.character(yr)]
    compound <- compound * (1 + if (!is.na(rate)) rate else mean_rate)
  }
  compound
}

# ── Internal: nearest-year lookup ─────────────────────────────────────────────
# Looks up `year` in a year-keyed series (year_str → value), falling back to
# the closest available year if `year` itself has no entry. Appropriate for
# exchange-rate series, which trend over time rather than fluctuate around a
# stable mean (unlike inflation, where .compound_factor() uses the series mean
# as its fallback for missing years).
#
# @param series  Named numeric (year_str → value), or NULL
# @param year    Integer year
# @return Numeric value, or NA_real_ if `series` is empty/NULL or has no
#   finite values.

.nearest_year_value <- function(series, year) {
  if (is.null(series) || length(series) == 0L) return(NA_real_)
  year <- as.integer(year)
  if (is.na(year)) return(NA_real_)

  key <- as.character(year)
  if (key %in% names(series)) {
    val <- series[[key]]
    if (is.finite(val)) return(unname(val))
  }

  years <- suppressWarnings(as.integer(names(series)))
  valid <- !is.na(years) & is.finite(series)
  if (!any(valid)) return(NA_real_)

  closest <- years[valid][which.min(abs(years[valid] - year))]
  unname(series[[as.character(closest)]])
}

# ── Internal: currency → USD rate at a given year ─────────────────────────────
# Used for Step 0 back-conversion in synth_standardize(): when a study's
# reported currency differs from its own country's currency, the cost is
# converted via the ratio of two currencies' rates against USD. Any country
# that uses `currency` as its own currency gives the same rate (FX doesn't
# vary by which member of a union you ask), so the first member with FCRF
# data for `year` is used.
#
# @param currency       ISO 4217 currency code
# @param year           Integer year
# @param fcrf_by_iso3c  factors$fcrf_by_iso3c — named list ISO3C → named
#                        numeric (year_str → LCU per USD)
# @return Numeric LCU-per-USD rate, or NA_real_ if unresolvable.

.currency_to_usd_rate <- function(currency, year, fcrf_by_iso3c) {
  if (identical(currency, "USD")) return(1)

  for (iso in .currency_members(currency)) {
    val <- .nearest_year_value(fcrf_by_iso3c[[iso]], year)
    if (!is.na(val) && is.finite(val) && val != 0) return(val)
  }
  NA_real_
}

#' Standardise a single study row to target-year KES.
#'
#' Applies the three-step procedure for both the PPP and exchange-rate paths.
#'
#' For the PPP path, the study's `country` is resolved to an ISO3C and PPP /
#' inflation are taken from THAT COUNTRY'S OWN series
#' (factors$ppp$rates_by_iso3c, factors$inflation_by_iso3c) — PPP conversion
#' factors reflect domestic price levels, which genuinely differ between
#' members of a currency union (e.g. Germany vs Portugal, both EUR), so a
#' single per-currency representative would be misleading.
#'
#' If the study's reported `currency` differs from its own country's currency
#' (e.g. a Canadian study costed in USD, or a Swiss study costed in EUR), a
#' Step 0 first back-converts the cost into the country's own currency using
#' historical exchange rates against USD (factors$fcrf_by_iso3c, at the study
#' year) before Steps 1-3 run.
#'
#' If the study's country can't be resolved to an ISO3C, or World Bank has no
#' PPP/inflation/FX data for it, the PPP-path fields are NA (renders as "—") —
#' there is no representative-country fallback. The exchange-rate path is
#' unaffected: it remains keyed by `currency` via factors$inflation/factors$fx.
#'
#' @param study_row   Single-row data.frame from the studies table
#' @param factors     Output of load_factors()
#' @param target_year Integer target year
#' @return Named list:
#'   $inflated_original   Cost in the study country's own currency at the PPP
#'                         reference year (Step 1 result, after Step 0 if needed)
#'   $cf_source_to_ppp    Compound factor for Step 1 (PPP path)
#'   $cf_kes_to_target    Compound factor for Step 3 (PPP path, KES leg)
#'   $kes_ppp             Final KES cost via PPP path (all steps); NA if the
#'                         study's country/currency could not be resolved
#'   $kes_fx              Final KES cost via exchange-rate path (all three steps)
#'   $ppp_to_kes          PPP conversion rate used (KES per unit of the study
#'                         country's own currency); NA if unresolved
#'   $fx_to_kes           Exchange rate used (KES per 1 unit of source currency)
#'   $ppp_fx_diff_pct     Percent by which PPP cost exceeds FX cost; NA if either unavailable
synth_standardize <- function(study_row, factors, target_year = TARGET_YEAR) {
  currency   <- as.character(study_row$currency)[1L]
  study_year <- as.integer(study_row$year)
  ppp_year   <- factors$ppp$year
  fx_year    <- as.integer(format(factors$fx$date, "%Y"))

  # ── Resolve study country and its own currency ──────────────────────────────
  country_raw <- as.character(study_row$country)[1L]
  study_iso3c <- countrycode::countrycode(country_raw,
                                           origin = "country.name",
                                           destination = "iso3c", warn = FALSE)
  # Fall back to treating the value as an ISO3C code directly (e.g. "GHA", "KEN")
  if (is.na(study_iso3c)) {
    candidate <- toupper(trimws(country_raw))
    if (nchar(candidate) == 3L &&
        !is.null(countrycode::countrycode(candidate, origin = "iso3c",
                                          destination = "iso3c", warn = FALSE)))
      study_iso3c <- candidate
  }

  country_currency <- NA_character_
  if (!is.na(study_iso3c) && study_iso3c %in% names(factors$iso3c_currency_map))
    country_currency <- factors$iso3c_currency_map[[study_iso3c]]

  # ── PPP path ────────────────────────────────────────────────────────────────
  ppp_rate <- NA_real_
  infl_ppp <- NULL
  cost_own <- NA_real_  # cost in the study country's own currency

  if (!is.na(country_currency)) {
    ppp_rate <- factors$ppp$rates_by_iso3c[[study_iso3c]] %||% NA_real_
    infl_ppp <- factors$inflation_by_iso3c[[study_iso3c]]

    if (identical(country_currency, currency)) {
      # Study already reports in the country's own currency.
      cost_own <- study_row$cost
    } else {
      # Step 0: reported currency differs from the country's own currency —
      # back-convert via exchange rates against USD at the study year.
      rate_country <- .nearest_year_value(factors$fcrf_by_iso3c[[study_iso3c]], study_year)
      rate_study   <- .currency_to_usd_rate(currency, study_year, factors$fcrf_by_iso3c)
      if (!is.na(rate_country) && !is.na(rate_study) && rate_study != 0)
        cost_own <- study_row$cost * (rate_country / rate_study)
    }
  }

  # Step 1: inflate to PPP reference year, in the country's own currency
  cf1      <- .compound_factor(infl_ppp, study_year, ppp_year)
  c_ppp_yr <- if (!is.na(cost_own)) cost_own * cf1 else NA_real_

  # Step 2: PPP conversion to KES at the PPP reference year
  c_kes_ppp_yr <- if (!is.na(c_ppp_yr) && is.finite(ppp_rate))
    c_ppp_yr * ppp_rate else NA_real_

  # Step 3: inflate KES from the PPP reference year to the target year
  cf3_ppp <- .compound_factor(factors$inflation_by_iso3c[["KEN"]], ppp_year, target_year)
  kes_ppp <- c_kes_ppp_yr * cf3_ppp

  # ── FX path ─────────────────────────────────────────────────────────────────
  # Step 1: inflate source currency from study year to FX anchor year
  cf1_fx  <- .compound_factor(factors$inflation[[currency]], study_year, fx_year)
  c_fx_yr <- study_row$cost * cf1_fx

  # Step 2: exchange-rate conversion to KES at anchor year
  fx_rate     <- factors$fx$rates[[currency]]
  c_kes_fx_yr <- if (!is.null(fx_rate) && is.finite(fx_rate))
    c_fx_yr * fx_rate else NA_real_

  # Step 3: inflate KES from FX anchor year to target year
  cf3_fx <- .compound_factor(factors$inflation[["KES"]], fx_year, target_year)
  kes_fx <- c_kes_fx_yr * cf3_fx

  # ── Diagnostics ─────────────────────────────────────────────────────────────
  diff_pct <- if (!is.na(kes_ppp) && !is.na(kes_fx) && is.finite(kes_fx) && kes_fx != 0)
    (kes_ppp - kes_fx) / kes_fx * 100 else NA_real_

  list(
    inflated_original  = c_ppp_yr,       # Step 1: country's own currency at PPP year
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

  .pool <- function(vals, wts, m) {
    valid <- is.finite(vals)
    if (!any(valid)) return(NA_real_)
    cv <- vals[valid]; wv <- wts[valid]
    switch(m,
      mean     = mean(cv),
      weighted = sum(cv * wv) / sum(wv),
      ivw      = { w <- wv^2; sum(cv * w) / sum(w) }
    )
  }

  cost_ppp    <- .pool(costs_ppp,    ns, method)
  cost_ppp_yr <- .pool(costs_ppp_yr, ns, method)
  cost_fx     <- .pool(costs_fx,     ns, method)
  effect      <- .pool(effects,      ns, method)

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
    .pool(study_icers, ns, method)
  } else NA_real_

  mean_icer_fx <- if (!is.null(ref_pooled) && any(is.finite(costs_fx))) {
    study_icers <- (costs_fx - ref_pooled$cost_fx) / (effects - ref_pooled$effect)
    .pool(study_icers, ns, method)
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
