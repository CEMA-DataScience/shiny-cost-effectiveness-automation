# fetch_factors.R
# Fetches and caches conversion factors for Evidence Synthesis standardisation.
#
# Structure of the returned factors object (format_version = "3"):
#   $ppp$rates_by_iso3c  named list ISO3C → KES per unit of that country's own
#                        currency (PPP basis)
#   $ppp$year            integer PPP reference year
#   $fx$rates            named list iso4217c → KES/unit (CBK, anchored to FX_ANCHOR_DATE)
#   $fx$date             Date used for the CBK anchor
#   $inflation           named list iso4217c → named numeric (year_str → rate);
#                        one representative country per currency — used by the FX path
#   $inflation_by_iso3c  named list ISO3C → named numeric (year_str → rate);
#                        that country's own GDP deflator series — used by the PPP path
#   $fcrf_by_iso3c       named list ISO3C → named numeric (year_str → LCU per USD);
#                        used for Step-0 back-conversion when a study's reported
#                        currency differs from its own country's currency
#   $iso3c_map           named character vector iso4217c → ISO3C (one representative
#                        per currency; feeds $inflation, the FX path)
#   $iso3c_currency_map  named character vector ISO3C → iso4217c (each country's
#                        own currency; used to resolve a study's country)
#   $currencies          character vector of all currencies in this cache
#   $fetched_at          POSIXct
#   $format_version      "3"
#
# Design principles:
#   - PPP and inflation for the PPP path are resolved per study by COUNTRY
#     (via $rates_by_iso3c / $inflation_by_iso3c and countrycode), not by
#     currency — PPP conversion factors reflect domestic price levels, which
#     genuinely differ between members of a currency union (e.g. Germany vs
#     Portugal, both EUR).
#   - If a study's reported currency differs from its own country's currency
#     (e.g. a Canadian study costed in USD), the cost is first back-converted
#     to the country's own currency using historical exchange rates
#     ($fcrf_by_iso3c) before the PPP/inflation steps — see synth_standardize().
#   - If a study's country can't be resolved, or World Bank has no PPP/FCRF
#     data for it, the PPP path returns NA (rendered as "—") rather than
#     substituting a guessed representative country.
#   - The FX path remains currency-keyed ($fx$rates, $inflation): market
#     exchange rates don't vary by which member of a currency union you're
#     in, so a single representative per currency is valid there.
#   - Inflation is stored as raw year-by-year series so callers can compute
#     any summary statistic (mean, geometric mean, trimmed mean, etc.)
#     and exclude anomalous years (e.g. 2020-2021).
#   - Cache carries a format_version field; mismatches trigger a rebuild.

library(wbstats)
library(rvest)
library(httr2)
library(dplyr)
library(countrycode)

CACHE_PATH           <- "data/factors_cache.rds"
FX_ANCHOR_DATE       <- as.Date("2026-04-30")
TARGET_YEAR          <- 2027L
FACTORS_FORMAT_VERSION <- "3"

# ── Currency → ISO3C overrides ────────────────────────────────────────────────
# ISO 4217 and ISO 3166 are different standards. Single-country currencies
# (KES, TZS, INR, NGN, ZAR, etc.) resolve automatically via countrycode::codelist.
# This table exists only for currencies that are structurally ambiguous:
#   (a) supranational monetary unions, and
#   (b) reserve currencies adopted by multiple countries/territories where the
#       first codelist match would return a territory rather than the issuing country.
#
# Used only to pick ONE representative ISO3C per currency for the FX path
# ($iso3c_map, $inflation). The PPP path resolves PPP/inflation per study by
# the study's own country (via $iso3c_currency_map), so it does not use this
# table — that is the whole point of the per-country redesign.
CURRENCY_ISO3C_OVERRIDES <- c(
  EUR = "EMU",   # Eurozone — WB ICP uses EMU as the monetary-union aggregate
  XOF = "SEN",   # West African CFA franc — Senegal as WB ICP representative
  XAF = "CMR",   # Central African CFA franc — Cameroon as WB ICP representative
  XCD = "LCA",   # Eastern Caribbean dollar — Saint Lucia
  USD = "USA",   # also used by Ecuador, El Salvador, and territories
  GBP = "GBR",   # also used by Channel Islands and Falklands
  AUD = "AUS",   # also used by Pacific island nations
  NZD = "NZL"    # also used by Cook Islands and Niue
)

# ── CBK label → ISO iso4217c code ────────────────────────────────────────────
# Maps the descriptive labels used in the CBK exchange rate table to ISO 4217.
CBK_LABEL_ISO <- c(
  "US DOLLAR"        = "USD",
  "STG POUND"        = "GBP",
  "EURO"             = "EUR",
  "SA RAND"          = "ZAR",
  "IND RUPEE"        = "INR",
  "AE DIRHAM"        = "AED",
  "CAN $"            = "CAD",
  "S FRANC"          = "CHF",
  "JPY (100)"        = "JPY",
  "SW KRONER"        = "SEK",
  "NOR KRONER"       = "NOK",
  "DAN KRONER"       = "DKK",
  "HONGKONG DOLLAR"  = "HKD",
  "SINGAPORE DOLLAR" = "SGD",
  "SAUDI RIYAL"      = "SAR",
  "CHINESE YUAN"     = "CNY",
  "AUSTRALIAN $"     = "AUD",
  "KES / TSHS"       = "TZS",
  "KES / USHS"       = "UGX",
  "KES / RWF"        = "RWF",
  "KES / BIF"        = "BIF"
)

# Currencies where CBK quotes "1 KES = X foreign units" — must invert to get KES/unit.
CBK_INVERTED <- c("TZS", "UGX", "RWF", "BIF")

# ── Currency → ISO3C lookup ───────────────────────────────────────────────────
# Queries countrycode::codelist as a data frame (the countrycode() function does
# not accept "iso4217c" as an origin). For each iso4217c, takes the first
# ISO3C match in the codelist; overrides are applied afterwards for the small set
# of currencies where the first match would be a territory rather than the issuer.
# Returns a named character vector: iso4217c → ISO3C (NA entries excluded).
.iso3c_for <- function(currencies) {
  currencies <- unique(currencies)

  cl <- countrycode::codelist
  cl <- cl[!is.na(cl$iso4217c) & !is.na(cl$iso3c),
           c("iso4217c", "iso3c")]

  iso <- setNames(
    cl$iso3c[match(currencies, cl$iso4217c)],
    currencies
  )

  for (curr in intersect(names(CURRENCY_ISO3C_OVERRIDES), currencies)) {
    iso[[curr]] <- CURRENCY_ISO3C_OVERRIDES[[curr]]
  }

  missing <- names(iso)[is.na(iso)]
  if (length(missing) > 0L)
    warning("No ISO3C mapping for: ", paste(missing, collapse = ", "),
            ". PPP and inflation data unavailable for these currencies.")

  iso[!is.na(iso)]
}

# ── Dynamic country/currency lookups ──────────────────────────────────────────
# Derived at runtime from countrycode::codelist — no hardcoded country lists.
# These drive the per-country PPP path in synth_standardize(): a study's
# country is resolved to an ISO3C, and that ISO3C's OWN currency is compared
# against the study's reported currency (CURRENCY_ISO3C_OVERRIDES is not used
# here — it exists only for the FX-path representatives above).

# Every ISO3C with both an iso3c and iso4217c entry in countrycode::codelist,
# plus Kenya. This is the set of countries fetched for $ppp$rates_by_iso3c,
# $inflation_by_iso3c and $fcrf_by_iso3c.
.all_iso3c <- function() {
  cl  <- countrycode::codelist
  iso <- unique(cl$iso3c[!is.na(cl$iso3c) & !is.na(cl$iso4217c)])
  unique(c("KEN", iso))
}

# ISO3C → that country's own ISO 4217 currency code.
.iso3c_currency_map <- function() {
  cl <- countrycode::codelist
  cl <- cl[!is.na(cl$iso3c) & !is.na(cl$iso4217c), c("iso3c", "iso4217c")]
  cl <- cl[!duplicated(cl$iso3c), ]
  setNames(cl$iso4217c, cl$iso3c)
}

# All ISO3C codes whose own currency is `currency` (e.g. "EUR" → every
# Eurozone member). Used for Step-0 cross-rate lookups in synth_standardize():
# a currency/USD rate doesn't depend on which member of the union you ask, so
# any member with FCRF data for the needed year will do.
.currency_members <- function(currency) {
  cl <- countrycode::codelist
  cl <- cl[!is.na(cl$iso3c) & !is.na(cl$iso4217c), c("iso3c", "iso4217c")]
  unique(cl$iso3c[cl$iso4217c == currency])
}

# ── World Bank PPP factors ────────────────────────────────────────────────────
# Returns list(rates_by_iso3c = named list ISO3C→KES per unit of that
# country's own currency, year = integer). Uses the most recent ICP year for
# which Kenya has data as the PPP reference year.
#
# Fetched once for every country in `all_iso3c` (i.e. .all_iso3c()) so that
# synth_standardize() can resolve PPP by the study's own country, rather than
# by a single representative per currency.

.fetch_wb_ppp <- function(all_iso3c) {
  raw <- wbstats::wb_data(
    indicator   = "PA.NUS.PPP",
    country     = "all",
    start_date  = TARGET_YEAR - 10L,
    end_date    = TARGET_YEAR - 1L,
    return_wide = FALSE
  ) |>
    filter(!is.na(value), iso3c %in% all_iso3c)

  ke_data <- raw[raw$iso3c == "KEN", ]
  if (nrow(ke_data) == 0L) stop("Kenya PPP factor not available from World Bank.")
  ppp_year <- max(ke_data$date)
  kes_ppp  <- ke_data$value[ke_data$date == ppp_year][1L]

  rates_by_iso3c <- list(KEN = 1)
  for (iso in setdiff(unique(raw$iso3c), "KEN")) {
    rows  <- raw[raw$iso3c == iso, ]
    # Use the PPP year value if available; otherwise the most recent
    exact <- rows[rows$date == ppp_year, ]
    row   <- if (nrow(exact) > 0L) exact else rows[which.max(rows$date), ]
    cp    <- row$value[1L]
    if (is.na(cp) || !is.finite(cp) || cp == 0) next
    rates_by_iso3c[[iso]] <- unname(kes_ppp / cp)
  }

  list(rates_by_iso3c = rates_by_iso3c, year = as.integer(ppp_year))
}

# ── CBK exchange rates ────────────────────────────────────────────────────────
# Returns list(rates = named list iso4217c→KES/unit, date = Date used)
# Rates are keyed by ISO 4217 code; anchored to FX_ANCHOR_DATE.

.fetch_cbk_fx <- function(anchor_date = FX_ANCHOR_DATE) {
  message("[fetch_factors] Fetching CBK exchange rate records...")

  resp <- tryCatch(
    request("https://www.centralbank.go.ke/wp-admin/admin-ajax.php") |>
      req_url_query(action = "get_wdtable", table_id = "193") |>
      req_body_form(draw = "1", start = "0", length = "-1") |>
      req_error(is_error = \(r) FALSE) |>
      req_timeout(60) |>
      req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp) || resp_status(resp) >= 400L) return(NULL)

  parsed <- tryCatch(jsonlite::fromJSON(resp_body_string(resp)), error = function(e) NULL)
  if (is.null(parsed) || !("data" %in% names(parsed))) return(NULL)

  df <- as.data.frame(parsed$data, stringsAsFactors = FALSE)
  names(df) <- c("date_str", "iso4217c_label", "rate_str")
  df$rate <- suppressWarnings(as.numeric(df$rate_str))
  df$date <- suppressWarnings(as.Date(df$date_str, format = "%d/%m/%Y"))
  df <- df[!is.na(df$date) & !is.na(df$rate), ]

  eligible <- df[df$date <= anchor_date, ]
  if (nrow(eligible) == 0L) eligible <- df
  use_date <- max(eligible$date)
  day_df   <- eligible[eligible$date == use_date, ]

  if (!identical(use_date, anchor_date))
    message(sprintf("[fetch_factors] FX anchor %s not in CBK data; using %s.",
                    format(anchor_date), format(use_date)))

  rates <- list(KES = 1)
  for (label in unique(day_df$iso4217c_label)) {
    iso <- CBK_LABEL_ISO[label]
    if (is.na(iso)) next
    rows <- day_df[day_df$iso4217c_label == label, ]
    rate <- rows$rate[1L]
    if (label == "JPY (100)") rate <- rate / 100
    rates[[iso]] <- if (iso %in% CBK_INVERTED) 1 / rate else rate
  }

  list(rates = rates, date = use_date)
}

# ── Kenya CPI rates: raw annual series from CBK/KNBS ────────────────────────
# Returns named numeric vector: year_string → rate fraction (e.g. "2023" = 0.065)

.fetch_cbk_inflation_rates <- function() {
  page <- tryCatch(
    rvest::read_html("https://www.centralbank.go.ke/inflation-rates/"),
    error = function(e) NULL
  )
  if (is.null(page)) return(NULL)

  tbl <- tryCatch(
    rvest::html_table(page, fill = TRUE, convert = FALSE)[[1L]],
    error = function(e) NULL
  )
  if (is.null(tbl) || ncol(tbl) < 3L) return(NULL)

  names(tbl) <- c("year", "month", "annual_avg", "m12")
  tbl <- tbl[grepl("^\\d{4}$", trimws(tbl$year)), ]
  tbl$rate <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", tbl$annual_avg)))

  valid <- tbl[!is.na(tbl$rate) & is.finite(tbl$rate), ]
  if (nrow(valid) == 0L) return(NULL)

  # The CBK table has multiple rows per year (one per month, each carrying the
  # same annual_avg). Average within each year to get one value per year.
  raw  <- setNames(valid$rate / 100, trimws(valid$year))
  grp  <- tapply(raw, names(raw), mean)
  setNames(as.numeric(grp), names(grp))
}

# ── Non-Kenya inflation: raw annual GDP deflator series from World Bank ───────
# Returns named list: ISO3C → named numeric vector (year_string → rate fraction).
# Covers every country in `all_iso3c` (feeds $inflation_by_iso3c, the PPP
# path) plus every representative ISO3C in `iso3c_map` (feeds $inflation, the
# FX path) — the latter includes supranational aggregates such as "EMU" that
# are not themselves ISO3C countries but do carry World Bank deflator series.

.fetch_wb_inflation_rates <- function(all_iso3c, iso3c_map, start_year = 2010L) {
  query_iso3c <- setdiff(unique(c(all_iso3c, unname(iso3c_map))), "KEN")
  if (length(query_iso3c) == 0L) return(list())

  raw <- tryCatch(
    wbstats::wb_data(
      indicator   = "NY.GDP.DEFL.KD.ZG",
      country     = "all",
      start_date  = start_year,
      end_date    = TARGET_YEAR - 1L,
      return_wide = FALSE
    ) |> filter(!is.na(value) & is.finite(value), iso3c %in% query_iso3c),
    error = function(e) { warning("WB deflator fetch failed: ", e$message); NULL }
  )
  if (is.null(raw)) return(list())

  result <- list()
  for (iso in unique(raw$iso3c)) {
    rows <- raw[raw$iso3c == iso, ]
    result[[iso]] <- setNames(rows$value / 100, as.character(rows$date))
  }
  result
}

# ── Historical exchange rates: World Bank official rate (LCU per USD) ────────
# Returns named list: ISO3C → named numeric vector (year_string → LCU per USD).
# Used by synth_standardize() for Step-0 back-conversion when a study's
# reported currency differs from its own country's currency (e.g. a Canadian
# study costed in USD): the cost is converted via
# (rate for the country's own currency) / (rate for the study's reported
# currency) at the study year. USD itself has a constant rate of 1.

.fetch_wb_fcrf <- function(all_iso3c, start_year = 1990L) {
  raw <- tryCatch(
    wbstats::wb_data(
      indicator   = "PA.NUS.FCRF",
      country     = "all",
      start_date  = start_year,
      end_date    = TARGET_YEAR - 1L,
      return_wide = FALSE
    ) |> filter(!is.na(value) & is.finite(value), iso3c %in% all_iso3c),
    error = function(e) { warning("WB FCRF fetch failed: ", e$message); NULL }
  )
  if (is.null(raw)) return(list())

  result <- list()
  for (iso in unique(raw$iso3c)) {
    rows <- raw[raw$iso3c == iso, ]
    result[[iso]] <- setNames(rows$value, as.character(rows$date))
  }
  result
}

# ── Build comprehensive iso3c map from all known WB currencies ────────────────
# Derives iso3c_map for every iso4217c code in countrycode::codelist, then
# applies CURRENCY_ISO3C_OVERRIDES for supranational/ambiguous currencies.
# This means PPP and inflation are always fetched for the full WB dataset,
# not just currencies that happen to appear in current studies.

.build_full_iso3c_map <- function() {
  cl <- countrycode::codelist
  cl <- cl[!is.na(cl$iso4217c) & !is.na(cl$iso3c), c("iso4217c", "iso3c")]
  cl <- cl[!duplicated(cl$iso4217c), ]
  all_currencies <- unique(c(cl$iso4217c, names(CURRENCY_ISO3C_OVERRIDES), "KES"))
  .iso3c_for(all_currencies)
}

# ── Assemble all factors ──────────────────────────────────────────────────────

.build_factors <- function() {
  all_iso3c <- .all_iso3c()
  iso3c_map <- .build_full_iso3c_map()

  message("[fetch_factors] Fetching WB PPP factors (per-country)...")
  ppp <- tryCatch(.fetch_wb_ppp(all_iso3c), error = function(e) {
    warning("PPP fetch failed: ", e$message)
    list(rates_by_iso3c = list(KEN = 1), year = TARGET_YEAR - 5L)
  })

  message("[fetch_factors] Fetching CBK exchange rates for ", format(FX_ANCHOR_DATE), "...")
  fx <- tryCatch(.fetch_cbk_fx(), error = function(e) {
    warning("CBK FX fetch failed: ", e$message)
    list(rates = list(KES = 1), date = FX_ANCHOR_DATE)
  })
  if (is.null(fx)) fx <- list(rates = list(KES = 1), date = FX_ANCHOR_DATE)

  message("[fetch_factors] Fetching CBK/KNBS Kenya CPI series...")
  ke_rates <- tryCatch(.fetch_cbk_inflation_rates(), error = function(e) {
    warning("CBK inflation fetch failed: ", e$message); NULL
  })

  message("[fetch_factors] Fetching WB GDP deflator series (per-country)...")
  deflator_by_iso3c <- tryCatch(.fetch_wb_inflation_rates(all_iso3c, iso3c_map), error = function(e) {
    warning("WB deflator fetch failed: ", e$message); list()
  })

  inflation_by_iso3c <- deflator_by_iso3c
  inflation_by_iso3c[["KEN"]] <- ke_rates %||% c("2023" = 0.065)

  message("[fetch_factors] Fetching WB historical exchange rates (per-country)...")
  fcrf_by_iso3c <- tryCatch(.fetch_wb_fcrf(all_iso3c), error = function(e) {
    warning("WB FCRF fetch failed: ", e$message); list()
  })

  # Currency-keyed inflation, retained for the FX path: market exchange rates
  # don't have PPP's per-country price-level ambiguity, so one representative
  # per currency (from iso3c_map) is fine here.
  all_currencies <- names(iso3c_map)
  inflation <- list()
  for (curr in all_currencies) {
    iso <- iso3c_map[[curr]]
    inflation[[curr]] <- if (identical(iso, "KEN")) {
      ke_rates %||% c("2023" = 0.065)
    } else {
      deflator_by_iso3c[[iso]] %||% c("2023" = 0.05)
    }
  }

  list(
    ppp                = ppp,
    fx                 = fx,
    inflation          = inflation,
    inflation_by_iso3c = inflation_by_iso3c,
    fcrf_by_iso3c      = fcrf_by_iso3c,
    iso3c_map          = iso3c_map,
    iso3c_currency_map = .iso3c_currency_map(),
    currencies         = all_currencies,
    fetched_at         = Sys.time(),
    format_version     = FACTORS_FORMAT_VERSION
  )
}

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 0L) ||
                               (length(x) == 1L && is.na(x))) y else x

# ── Fallback ──────────────────────────────────────────────────────────────────

.empty_factors <- function() {
  warning("[fetch_factors] All fetches failed and no cache available. ",
          "Conversion factors unavailable — standardised costs will show as missing.")
  list(
    ppp                = list(rates_by_iso3c = list(KEN = 1), year = TARGET_YEAR - 1L),
    fx                 = list(rates = list(KES = 1), date = FX_ANCHOR_DATE),
    inflation          = list(),
    inflation_by_iso3c = list(),
    fcrf_by_iso3c      = list(),
    iso3c_map          = c(KES = "KEN"),
    iso3c_currency_map = c(KEN = "KES"),
    currencies         = "KES",
    fetched_at         = Sys.time(),
    format_version     = FACTORS_FORMAT_VERSION,
    is_fallback        = TRUE
  )
}

# ── Public entry point ────────────────────────────────────────────────────────

#' Load conversion factors, reading from cache when available.
#' Covers the full WB country dataset — no currencies argument needed.
#'
#' @param cache_path    Path to .rds cache file.
#' @param max_age_days  Days before cache is considered stale. Default Inf (permanent).
#' @param force_refresh Ignore cache and re-fetch.
#' @return List with $ppp, $fx, $inflation, $inflation_by_iso3c, $fcrf_by_iso3c,
#'   $iso3c_map, $iso3c_currency_map, $currencies, $fetched_at
load_factors <- function(cache_path    = CACHE_PATH,
                         max_age_days  = Inf,
                         force_refresh = FALSE) {
  if (!force_refresh && file.exists(cache_path)) {
    cached    <- readRDS(cache_path)
    age_days  <- as.numeric(difftime(Sys.time(), cached$fetched_at, units = "days"))
    wrong_fmt <- !identical(cached$format_version, FACTORS_FORMAT_VERSION)

    if (wrong_fmt) {
      message("[fetch_factors] Cache format outdated (v", cached$format_version %||% "1",
              " → v", FACTORS_FORMAT_VERSION, ") — rebuilding.")
    } else if (age_days < max_age_days) {
      message(sprintf("[fetch_factors] Using cached factors (built %s, %d currencies).",
                      format(cached$fetched_at, "%Y-%m-%d"), length(cached$currencies)))
      return(cached)
    }
  }

  factors <- tryCatch(
    .build_factors(),
    error = function(e) { warning("Factor build failed: ", e$message); NULL }
  )

  if (is.null(factors)) {
    if (file.exists(cache_path)) {
      message("[fetch_factors] Fetch failed; falling back to existing cache (may be stale).")
      return(readRDS(cache_path))
    }
    return(.empty_factors())
  }

  dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(factors, cache_path)
  message("[fetch_factors] Factors built and cached to ", cache_path,
          " (", length(factors$currencies), " currencies).")
  factors
}
