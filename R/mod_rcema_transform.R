# mod_rcema_transform.R
# RCEMA Data Quality & Synthesis Importer
#
# Primary purpose : diagnose quality of RCEMA extraction output and generate
#                   actionable feedback for data extractors.
# Secondary purpose: save study rows to the Evidence Synthesis database so that
#                   PPP standardisation, pooling and analysis can proceed normally.
#
# Design principles:
#  - No derivation of effects from ICERs.  ICERs are stored as reported_icer for
#    reference / pooled-mean display in Synthesis; they do not produce strategy rows.
#  - One STUDIES_COLS entry per role per RCEMA row:
#      intervention row  strategy = intervention_name, cost = intervention_cost
#      comparator row    strategy = comparator_name,   cost = comparator_cost
#    Both are saved when present; pooling in Synthesis is at strategy level so the
#    role label (intervention vs comparator) is just metadata.
#  - The SHA intervention field is chosen by the user in the save modal — it links
#    the studies to the correct Synthesis dropdown entry.

# ── Column-detection heuristics ───────────────────────────────────────────────

RCEMA_COL_HINTS <- list(
  intervention_name    = c("intervention"),
  comparator_name      = c("comparator"),
  intervention_cost    = c("cost_of_intervention", "intervention_cost",
                           "cost_intervention", "total_cost_per_patient",
                           "total_cost_of_intervention"),
  comparator_cost      = c("cost_of_comparator", "comparator_cost",
                           "total_cost_of_comparator", "cost_comparator"),
  icer                 = c("icer", "icers", "icer_value",
                           "incremental_cost_effectiveness_ration__icer_",
                           "incremental_cost_effectiveness_ratio__icer_",
                           "icer_incremental_cost_effectiveness_ratio"),
  effect               = c("effect", "dalys", "qalys", "lys",
                           "effect_of_intervention", "health_outcome",
                           "daly", "qaly"),
  outcome_measure      = c("outcome_measure", "outcome"),
  currency             = c("currency"),
  currency_year        = c("currency_year"),
  year                 = c("year_of_publication", "publication_year", "year"),
  country              = c("country", "country_of_study", "country_region"),
  authors              = c("authors", "first_author"),
  n                    = c("sample_size", "n", "total_sample",
                           "sample_size_n", "number_of_patients"),
  source_type          = c("source_type", "study_design", "study_type"),
  indication           = c("indication"),
  population           = c("population", "study_population"),
  perspective          = c("perspective"),
  time_horizon         = c("time_horizon"),
  discount_rate        = c("discount_rate"),
  threshold_referenced = c("threshold_referenced", "threshold_refrenced",
                           "threshold_referenced"),
  conclusion           = c("conclusion", "conclusion_authors_note_",
                           "conclusion_authors_note")
)

# ── Country normalisation ──────────────────────────────────────────────────────

.normalize_country_vec <- function(x) {
  vapply(as.character(x), function(v) {
    v <- trimws(v)
    if (is.na(v) || !nzchar(v) ||
        tolower(v) %in% c("not_available", "na", "n/a")) return(NA_character_)
    if (grepl("^[A-Z]{3}$", v)) return(v)
    tokens <- trimws(strsplit(v, "[;,/]")[[1L]])
    first  <- tokens[nzchar(tokens)][1L]
    if (is.na(first)) return(NA_character_)
    # Leading 2- or 3-letter code: "ISR - Israel", "US - United States"
    m <- regmatches(first,
                    regexpr("^[A-Z]{2,3}(?=[-[:space:]])", first, perl = TRUE))
    if (length(m) == 1L) {
      origin <- if (nchar(m) == 3L) "iso3c" else "iso2c"
      ok <- tryCatch(
        suppressWarnings(countrycode::countrycode(m, origin, "iso3c")),
        error = function(e) NA_character_
      )
      if (!is.na(ok)) return(ok)
    }
    # Standalone ISO alpha-2
    if (grepl("^[A-Z]{2}$", first)) {
      ok <- tryCatch(
        suppressWarnings(countrycode::countrycode(first, "iso2c", "iso3c")),
        error = function(e) NA_character_
      )
      if (!is.na(ok)) return(ok)
    }
    # Country name (full string and after stripping any leading code prefix)
    name_part <- trimws(sub("^[A-Z]{2,3}[-[:space:]]+", "", first))
    for (candidate in unique(c(first, name_part))) {
      iso <- tryCatch(
        suppressWarnings(
          countrycode::countrycode(candidate, "country.name", "iso3c")
        ),
        error = function(e) NA_character_
      )
      if (!is.na(iso)) return(iso)
    }
    first
  }, character(1L), USE.NAMES = FALSE)
}

# ── Currency resolution ────────────────────────────────────────────────────────
# Three-step chain applied per row in parse_rcema():
#   1. Normalise whatever is in the currency column (symbols, abbreviations, names)
#   2. If still NA: scan intervention/comparator cost text for embedded codes
#   3. If still NA: infer from the study's ISO3C country code
# Source is tracked per row so the quality card can explain the breakdown.

# Symbols and regional abbreviations → ISO 4217.  Only entries that are NOT
# already valid ISO 4217 codes need to be here; bare three-letter codes (USD,
# KES, NGN …) are validated directly against the factors$currencies set.
.CURRENCY_SYMBOL_MAP <- c(
  "$"    = "USD", "US$"  = "USD", "U.S.$" = "USD",
  "£"    = "GBP", "€"    = "EUR", "¥"    = "JPY",
  "₦"    = "NGN", "₹"    = "INR", "R$"   = "BRL",
  "KSh"  = "KES", "KSh." = "KES", "Ksh"  = "KES", "ksh"  = "KES",
  "K.Sh" = "KES", "Kes"  = "KES",
  "TSh"  = "TZS", "TSh." = "TZS", "Tsh"  = "TZS",
  "USh"  = "UGX", "Ush"  = "UGX",
  "FRw"  = "RWF", "Frw"  = "RWF",
  "Br"   = "ETB",
  "GH¢"  = "GHS", "GHC"  = "GHS",
  "FCFA" = "XOF", "CFA"  = "XOF", "F.CFA" = "XOF",
  "CFA franc" = "XOF",
  # Common full-name fragments (lowercase keys, checked case-insensitively)
  "us dollar"    = "USD", "us dollars"        = "USD",
  "kenyan shilling" = "KES", "kenya shilling" = "KES",
  "tanzanian shilling" = "TZS", "ugandan shilling" = "UGX",
  "rwandan franc" = "RWF", "ethiopian birr"   = "ETB",
  "nigerian naira" = "NGN", "naira"            = "NGN",
  "ghanaian cedi"  = "GHS", "cedi"             = "GHS",
  "south african rand" = "ZAR", "rand"          = "ZAR",
  "british pound"  = "GBP", "pound sterling"   = "GBP",
  "euro"           = "EUR", "euros"             = "EUR",
  "indian rupee"   = "INR", "rupee"             = "INR",
  "japanese yen"   = "JPY", "chinese yuan"     = "CNY",
  "canadian dollar" = "CAD", "australian dollar" = "AUD",
  "swiss franc"    = "CHF", "zambian kwacha"   = "ZMW",
  "malawian kwacha" = "MWK"
)

# Normalise one currency string → ISO 4217 code or NA.
# iso4217_codes: character vector of all valid codes (from factors$currencies).
.norm_one_currency <- function(v, iso4217_codes) {
  v <- trimws(v)
  if (is.na(v) || !nzchar(v) ||
      tolower(v) %in% c("not_available", "na", "n/a", "nr", "not reported",
                        "not applicable", "not stated"))
    return(NA_character_)

  # Already a valid ISO code (exact case)
  if (v %in% iso4217_codes) return(v)

  # Symbol/abbreviation map — case-sensitive first (catches $, £, KSh, etc.)
  lu <- .CURRENCY_SYMBOL_MAP[v]
  if (!is.na(lu) && nzchar(lu)) return(lu)

  # Uppercase version → ISO check + symbol map
  vu <- toupper(v)
  if (vu %in% iso4217_codes) return(vu)
  lu <- .CURRENCY_SYMBOL_MAP[vu]
  if (!is.na(lu) && nzchar(lu)) return(lu)

  # Lowercase version → full-name map
  vl <- tolower(v)
  lu <- .CURRENCY_SYMBOL_MAP[vl]
  if (!is.na(lu) && nzchar(lu)) return(lu)

  # Substring of a known name fragment
  for (nm in names(.CURRENCY_SYMBOL_MAP)) {
    if (nchar(nm) > 4L && grepl(nm, vl, fixed = TRUE))
      return(.CURRENCY_SYMBOL_MAP[[nm]])
  }

  # Last resort: any ISO code token embedded in the string ("USD 2019", "2020 KES")
  toks <- regmatches(v, gregexpr("\\b[A-Z]{3}\\b", v))[[1L]]
  valid <- toks[toks %in% iso4217_codes]
  if (length(valid) > 0L) return(valid[1L])

  NA_character_
}

.normalize_currency_vec <- function(x, iso4217_codes) {
  vapply(as.character(x), .norm_one_currency, character(1L),
         iso4217_codes = iso4217_codes, USE.NAMES = FALSE)
}

# Extract currency code from a cost-value string ("$1,500", "KSh 450", "USD2020")
.extract_currency_from_cost_vec <- function(x, iso4217_codes) {
  vapply(as.character(x), function(v) {
    v <- trimws(v)
    if (is.na(v) || !nzchar(v)) return(NA_character_)

    # Longer/specific symbols first to avoid false positives
    if (grepl("KSh\\.?|K\\.Sh|Ksh",    v)) return("KES")
    if (grepl("TSh\\.?|Tsh",            v)) return("TZS")
    if (grepl("USh\\.?|Ush",            v)) return("UGX")
    if (grepl("GH¢|GHC",               v)) return("GHS")
    if (grepl("FCFA|F\\.CFA|CFA",       v, ignore.case = TRUE)) return("XOF")
    if (grepl("FRw|Frw",                v)) return("RWF")
    if (grepl("₦",                 v)) return("NGN")   # ₦
    if (grepl("₹",                 v)) return("INR")   # ₹
    if (grepl("£",                 v)) return("GBP")   # £
    if (grepl("€",                 v)) return("EUR")   # €
    if (grepl("US\\$|U\\.S\\.\\$",      v)) return("USD")
    if (grepl("R\\$",                   v)) return("BRL")
    if (grepl("\\$",                    v)) return("USD")

    # 3-letter ISO code adjacent to digits: "USD 1500", "USD1500", "KES1500"
    m <- regmatches(v, regexpr("[A-Z]{3}(?=[[:space:]]*[0-9])", v, perl = TRUE))
    if (length(m) == 1L && m %in% iso4217_codes) return(m)

    # Any ISO code token anywhere
    toks <- regmatches(v, gregexpr("\\b[A-Z]{3}\\b", v))[[1L]]
    valid <- toks[toks %in% iso4217_codes]
    if (length(valid) > 0L) return(valid[1L])

    NA_character_
  }, character(1L), USE.NAMES = FALSE)
}

# ── Detection helpers ──────────────────────────────────────────────────────────

.detect_col <- function(names_vec, hints, raw = NULL) {
  for (h in hints) {
    if (h %in% names_vec) {
      if (is.null(raw)) return(h)
      vals <- raw[[h]]
      if (any(!is.na(vals) & nzchar(trimws(as.character(vals))) &
              as.character(vals) != "not_available")) return(h)
    }
  }
  NA_character_
}

.has_currency_embedded <- function(x) {
  x <- x[!is.na(x) & nzchar(x) & x != "not_available"]
  if (length(x) == 0L) return(FALSE)
  any(grepl("[$£€¥]|\\b[A-Z]{2,3}\\s+[0-9]|[0-9]\\s+[A-Z]{2,3}\\b|KSh\\.|Ksh",
            x))
}

.has_range <- function(x) {
  x <- x[!is.na(x) & nzchar(x) & x != "not_available"]
  if (length(x) == 0L) return(FALSE)
  any(grepl("[0-9][[:space:]]*[-–—][[:space:]]*[0-9]|[0-9][[:space:]]*;[[:space:]]*[0-9]",
            x))
}

.is_mostly_numeric <- function(x) {
  x <- x[!is.na(x) & nzchar(x) & x != "not_available"]
  if (length(x) == 0L) return(FALSE)
  clean <- gsub("[$£€¥,]", "", x)
  clean <- gsub("^[A-Z]{2,3}[[:space:]]*", "", clean)
  clean <- gsub("[[:space:]]*[A-Z]{2,3}$", "", clean)
  mean(!is.na(suppressWarnings(as.numeric(trimws(clean))))) > 0.5
}

.has_narrative <- function(x) {
  x <- x[!is.na(x) & nzchar(x) & x != "not_available"]
  if (length(x) == 0L) return(FALSE)
  any(nchar(x) > 40 | grepl("[a-z]{3,}[[:space:]][a-z]{3,}", tolower(x)))
}

# ── Column quality assessment ──────────────────────────────────────────────────

.assess_columns <- function(raw, detected, n) {
  assess_one <- function(role, label, required = FALSE) {
    col     <- detected[[role]]
    has_col <- !is.na(col) && col %in% names(raw)

    if (!has_col)
      return(list(role = role, label = label, col = NA_character_,
                  n_filled = 0L, pct = 0L,
                  currency_embedded = FALSE, has_range = FALSE,
                  has_narrative = FALSE, is_numeric = FALSE,
                  status = if (required) "blocking" else "missing",
                  issues = character(), required = required))

    vals       <- trimws(as.character(raw[[col]]))
    vals[vals == "not_available"] <- NA_character_
    filled     <- !is.na(vals) & nzchar(vals)
    n_filled   <- sum(filled)
    pct        <- round(100L * n_filled / n)
    fv         <- vals[filled]

    cur_emb  <- .has_currency_embedded(fv)
    has_rng  <- .has_range(fv)
    is_num   <- .is_mostly_numeric(fv)
    has_narr <- !is_num && .has_narrative(fv)

    issues <- character()
    if (cur_emb) issues <- c(issues, "currency embedded in amount")
    if (has_rng) issues <- c(issues, "ranges present")
    if (has_narr && role %in% c("intervention_cost", "comparator_cost",
                                "icer", "effect"))
      issues <- c(issues, "non-numeric values")

    status <- if (n_filled == 0L) {
      if (required) "blocking" else "missing"
    } else if (required && !is_num) {
      "blocking"
    } else if (length(issues) > 0L) {
      "issues"
    } else {
      "ok"
    }

    list(role = role, label = label, col = col,
         n_filled = n_filled, pct = pct,
         currency_embedded = cur_emb, has_range = has_rng,
         has_narrative = has_narr, is_numeric = is_num,
         status = status, issues = issues, required = required)
  }

  list(
    intervention_name = assess_one("intervention_name", "Intervention name"),
    comparator_name   = assess_one("comparator_name",   "Comparator name"),
    intervention_cost = assess_one("intervention_cost", "Intervention cost",  TRUE),
    comparator_cost   = assess_one("comparator_cost",   "Comparator cost",    TRUE),
    icer              = assess_one("icer",              "ICER (reported)",    TRUE),
    effect            = assess_one("effect",            "Effect (numeric)",   TRUE),
    outcome_measure   = assess_one("outcome_measure",   "Outcome measure"),
    currency          = assess_one("currency",          "Currency"),
    currency_year     = assess_one("currency_year",     "Currency year"),
    country           = assess_one("country",           "Country"),
    year              = assess_one("year",              "Publication year"),
    authors           = assess_one("authors",           "Authors")
  )
}

# ── Extractor feedback report ──────────────────────────────────────────────────

.generate_feedback <- function(raw, detected, quality, n_intv_names, n_comp_names,
                                filename = "") {
  n  <- nrow(raw)
  hr <- strrep("-", 60)

  lines <- c(
    "DATA QUALITY REPORT — RCEMA EXTRACTION",
    paste0("File:           ", if (nzchar(filename)) filename else "(uploaded)"),
    paste0("Date:           ", format(Sys.Date(), "%Y-%m-%d")),
    paste0("Rows analysed:  ", n),
    "", hr, "COLUMN COMPLETENESS", hr, ""
  )

  for (q in quality) {
    sym <- switch(q$status,
                  ok       = "[OK]      ",
                  issues   = "[WARNING] ",
                  blocking = "[MISSING] ",
                  missing  = "[MISSING] ",
                            "[?]       ")
    col_note   <- if (!is.na(q$col)) paste0(" (column: '", q$col, "')") else ""
    issue_note <- if (length(q$issues) > 0L)
      paste0(" — ", paste(q$issues, collapse = "; ")) else ""
    lines <- c(lines, paste0(sym, q$label, ": ", q$n_filled, "/", n,
                              " (", q$pct, "%)", col_note, issue_note))
  }

  lines <- c(lines, "", hr, "ACTIONS REQUIRED", hr, "")
  action_n <- 0L

  if (quality$effect$status %in% c("blocking", "missing")) {
    action_n <- action_n + 1L
    lines <- c(lines,
      paste0(action_n, ". NUMERIC EFFECT NOT RECORDED (",
             n - quality$effect$n_filled, "/", n, " rows)"),
      "   Record DALYs averted, QALYs gained, life-years saved, etc. as a plain",
      "   number in a dedicated 'effect' column.",
      "   Do not use narrative descriptions — e.g. 'wound closure achieved in 7/7'",
      "   is not a usable health outcome value.",
      "")
  }

  if (quality$icer$status %in% c("blocking", "missing")) {
    action_n <- action_n + 1L
    lines <- c(lines,
      paste0(action_n, ". ICER NOT RECORDED OR NOT NUMERIC (",
             n - quality$icer$n_filled, "/", n, " rows)"),
      "   If the paper reports a cost-effectiveness ratio, record it as a plain",
      "   number (e.g. 1500) with no currency symbol, slash, or units.",
      "     Correct:   1500",
      "     Incorrect: '$1,500/DALY'  'USD 1500'  '~1,500'",
      "")
  }

  if (quality$comparator_cost$status %in% c("blocking", "missing")) {
    action_n <- action_n + 1L
    lines <- c(lines,
      paste0(action_n, ". COMPARATOR COST NOT RECORDED (",
             n - quality$comparator_cost$n_filled, "/", n, " rows)"),
      "   Record the total cost per patient for the comparator as a plain number",
      "   in a dedicated column (e.g. 'cost_of_comparator').",
      "")
  }

  if (quality$intervention_cost$currency_embedded ||
      quality$comparator_cost$currency_embedded) {
    action_n <- action_n + 1L
    lines <- c(lines,
      paste0(action_n, ". CURRENCY EMBEDDED IN COST VALUES"),
      "   Currency codes have been auto-extracted from cost values for this upload,",
      "   but for reliability please use a dedicated column going forward.",
      "     Correct:   cost = 80        currency = USD",
      "     Incorrect: cost = '$ 80'    cost = 'USD 1326'   cost = 'KSh. 65,000'",
      "   Use ISO 4217 three-letter codes: USD, KES, GBP, EUR, NGN, etc.",
      "")
  }

  if (quality$intervention_cost$has_range || quality$comparator_cost$has_range ||
      quality$icer$has_range) {
    action_n <- action_n + 1L
    lines <- c(lines,
      paste0(action_n, ". RANGES DETECTED IN COST OR ICER COLUMNS"),
      "   Create separate rows per scenario instead of a range in one cell:",
      "     Row 1: base case (point estimate) — scenario = 'base_case'",
      "     Row 2: lower bound               — scenario = 'lower_bound'",
      "     Row 3: upper bound               — scenario = 'upper_bound'",
      "")
  }

  if (n_intv_names > 5L || n_comp_names > 5L) {
    action_n <- action_n + 1L
    lines <- c(lines,
      paste0(action_n, ". STRATEGY NAMES NOT STANDARDIZED"),
      paste0("   ", n_intv_names, " unique intervention names and ",
             n_comp_names, " unique comparator names found."),
      "   Use an identical name for the same strategy across all papers.",
      "   The strategy name becomes the label in all charts and tables.",
      "")
  }

  if (!is.na(quality$country$col) && quality$country$n_filled > 0L) {
    col_vals   <- as.character(raw[[quality$country$col]])
    col_vals   <- col_vals[!is.na(col_vals) & col_vals != "not_available" &
                           nzchar(col_vals)]
    unresolved <- col_vals[!grepl("^[A-Z]{3}$", trimws(col_vals))]
    if (length(unresolved) > 0L) {
      action_n <- action_n + 1L
      lines <- c(lines,
        paste0(action_n, ". ", length(unresolved),
               " COUNTRY VALUE(S) COULD NOT BE AUTO-RESOLVED"),
        "   Country names and codes are normalised automatically (e.g. 'Kenya' -> KEN,",
        "   'ISR - Israel' -> ISR), but these values were not recognised:",
        paste0("     ", paste(head(unique(unresolved), 8L), collapse = "  ")),
        "   Use ISO 3166-1 alpha-3 codes or standard English country names.",
        "")
    }
  }

  if (action_n == 0L)
    lines <- c(lines, "No major data quality issues found.", "")

  lines <- c(lines,
    hr, "EXPECTED FORMAT", hr, "",
    "Download the CEA studies template (link on this page) for the expected",
    "column layout. One row = one study for one strategy.",
    "  Required numeric : cost, effect, n (sample size)",
    "  Required text    : strategy, country (ISO alpha-3), currency (ISO 4217),",
    "                     currency_year, outcome_measure",
    "  Optional         : scenario (base_case / lower_bound / upper_bound),",
    "                     reported_icer, authors, year, perspective, time_horizon",
    "")

  paste(lines, collapse = "\n")
}

# ── Extraction helpers ─────────────────────────────────────────────────────────

.extract_first_numeric <- function(x) {
  x <- as.character(x)
  m <- regexpr("[0-9][0-9,\\.]*", x)
  suppressWarnings(as.numeric(
    ifelse(m > 0L, gsub(",", "", regmatches(x, m)), NA_character_)
  ))
}

.strip_wide_cols <- function(raw) {
  dp <- "_(confidence|page|snippet|original_ai_value|edited_by|edited_at)$"
  raw[, !grepl(dp, names(raw)), drop = FALSE]
}

# ── Core parse ────────────────────────────────────────────────────────────────
# Returns a list:
#   raw       — normalised data frame with one row per RCEMA row, plus derived cols
#   detected  — named list of detected column names
#   is_wide   — logical

parse_rcema <- function(raw, name_map = NULL, factors = NULL) {
  is_wide <- any(grepl("_confidence$", names(raw)))
  if (is_wide) raw <- .strip_wide_cols(raw)
  names(raw) <- trimws(tolower(gsub("[^a-z0-9_]", "_", names(raw))))

  detected <- lapply(RCEMA_COL_HINTS,
                     function(hints) .detect_col(names(raw), hints, raw))

  get_num <- function(col, extract_fallback = TRUE) {
    nm <- detected[[col]]
    if (is.na(nm) || !nm %in% names(raw)) return(rep(NA_real_, nrow(raw)))
    x <- as.character(raw[[nm]])
    x[tolower(x) %in% c("not_available", "na", "n/a")] <- NA_character_
    direct <- suppressWarnings(as.numeric(gsub(",", "", x)))
    if (extract_fallback) {
      need <- is.na(direct) & !is.na(x)
      if (any(need)) direct[need] <- .extract_first_numeric(x[need])
    }
    direct
  }

  get_str <- function(col) {
    nm <- detected[[col]]
    if (is.na(nm) || !nm %in% names(raw)) return(rep(NA_character_, nrow(raw)))
    v <- trimws(as.character(raw[[nm]]))
    v[tolower(v) %in% c("not_available", "na", "n/a")] <- NA_character_
    v
  }

  intv_name <- get_str("intervention_name")
  comp_name <- get_str("comparator_name")

  # Apply normalisation map
  if (!is.null(name_map) && nrow(name_map) > 0L) {
    for (i in seq_len(nrow(name_map))) {
      orig <- name_map$original_name[i]
      std  <- trimws(name_map$standardized_name[i])
      if (!is.na(orig) && nzchar(std)) {
        intv_name[!is.na(intv_name) & intv_name == orig] <- std
        comp_name[!is.na(comp_name) & comp_name == orig] <- std
      }
    }
  }

  intv_cost   <- get_num("intervention_cost")
  comp_cost   <- get_num("comparator_cost")
  # ICER: extract first numeric but do NOT use for effect derivation
  icer_raw    <- if (!is.na(detected$icer) && detected$icer %in% names(raw)) {
    v <- as.character(raw[[detected$icer]])
    v[tolower(v) %in% c("not_available", "na", "n/a")] <- NA_character_
    v
  } else rep(NA_character_, nrow(raw))
  icer_num    <- suppressWarnings(as.numeric(gsub(",", "", icer_raw)))
  icer_num    <- ifelse(is.na(icer_num), .extract_first_numeric(icer_raw), icer_num)

  # Effect: clean numerics ONLY — no extraction from narrative sentences
  effect_num  <- get_num("effect", extract_fallback = FALSE)

  outcome_m   <- get_str("outcome_measure")
  country     <- .normalize_country_vec(get_str("country"))

  # ── Currency: three-step resolution ──────────────────────────────────────
  # Build validation set + country→currency map from factors (preferred) or
  # countrycode::codelist (fallback so module works even without factors).
  iso4217_codes <- if (!is.null(factors) && length(factors$currencies) > 0L)
    factors$currencies
  else {
    cl <- countrycode::codelist
    unique(cl$iso4217c[!is.na(cl$iso4217c)])
  }
  iso3c_cur_map <- if (!is.null(factors) && !is.null(factors$iso3c_currency_map))
    factors$iso3c_currency_map
  else {
    cl  <- countrycode::codelist
    cl  <- cl[!is.na(cl$iso3c) & !is.na(cl$iso4217c), c("iso3c", "iso4217c")]
    cl  <- cl[!duplicated(cl$iso3c), ]
    setNames(cl$iso4217c, cl$iso3c)
  }

  # Step 1: normalise currency column
  currency_raw  <- get_str("currency")
  currency      <- .normalize_currency_vec(currency_raw, iso4217_codes)
  currency_source <- rep("direct", nrow(raw))
  currency_source[is.na(currency)] <- "missing"  # tentative; overwritten below

  # Step 2: extract from cost-value text for rows still missing currency
  need <- is.na(currency)
  if (any(need)) {
    intv_cost_col <- detected$intervention_cost
    comp_cost_col <- detected$comparator_cost
    for (cost_col in c(intv_cost_col, comp_cost_col)) {
      still <- is.na(currency)
      if (!any(still) || is.na(cost_col) || !cost_col %in% names(raw)) next
      extracted <- .extract_currency_from_cost_vec(
        as.character(raw[[cost_col]]), iso4217_codes)
      hit <- still & !is.na(extracted)
      currency[hit]        <- extracted[hit]
      currency_source[hit] <- "extracted from cost"
    }
  }

  # Step 3: infer from country for rows still missing currency
  still <- is.na(currency)
  if (any(still) && any(!is.na(country))) {
    inferred <- iso3c_cur_map[country]       # NA where country not in map
    hit <- still & !is.na(inferred)
    currency[hit]        <- inferred[hit]
    currency_source[hit] <- "inferred from country"
  }

  # Mark what truly couldn't be resolved
  currency_source[is.na(currency)] <- "missing"

  currency_yr <- get_str("currency_year")
  authors     <- get_str("authors")
  yr          <- get_num("year")
  n_patients  <- get_num("n")
  source_type <- get_str("source_type")
  indication  <- get_str("indication")
  population  <- get_str("population")
  perspective <- get_str("perspective")
  time_horiz  <- get_str("time_horizon")
  disc_rate   <- get_num("discount_rate")
  threshold   <- get_str("threshold_referenced")
  conclusion  <- get_str("conclusion")

  # An intervention row is saveable when it has a name and at least one value
  save_intv <- !is.na(intv_name) & nzchar(trimws(intv_name)) &
               (is.finite(intv_cost) | is.finite(icer_num) | is.finite(effect_num))
  # A comparator row is saveable when it has a name and a cost
  save_comp <- !is.na(comp_name) & nzchar(trimws(comp_name)) &
               is.finite(comp_cost)

  data.frame(
    intv_name   = intv_name,
    comp_name   = comp_name,
    intv_cost   = intv_cost,
    comp_cost   = comp_cost,
    icer        = icer_num,
    effect      = effect_num,
    outcome_m   = outcome_m,
    currency        = currency,
    currency_source = currency_source,
    currency_yr     = currency_yr,
    country         = country,
    authors     = authors,
    year        = yr,
    n_patients  = n_patients,
    source_type = source_type,
    indication  = indication,
    population  = population,
    perspective = perspective,
    time_horiz  = time_horiz,
    disc_rate   = disc_rate,
    threshold   = threshold,
    conclusion  = conclusion,
    save_intv   = save_intv,
    save_comp   = save_comp,
    stringsAsFactors = FALSE
  )
}

# ── Mapping to STUDIES_COLS ───────────────────────────────────────────────────
# Converts one row of parse_rcema output to a STUDIES_COLS-format list.
# role = "intervention" or "comparator"

.to_study_row <- function(row, role, sha_intervention, submitted_by) {
  is_intv <- role == "intervention"
  d <- setNames(
    as.list(rep(NA_character_, length(STUDIES_COLS))),
    STUDIES_COLS
  )
  d$intervention   <- sha_intervention
  d$strategy       <- if (is_intv) row$intv_name else row$comp_name
  d$comparator     <- if (is_intv) row$comp_name else NA_character_
  d$cost           <- if (is_intv && is.finite(row$intv_cost)) row$intv_cost
                      else if (!is_intv && is.finite(row$comp_cost)) row$comp_cost
                      else NA_real_
  d$effect         <- if (is_intv && is.finite(row$effect)) row$effect else NA_real_
  d$reported_icer  <- if (is_intv && is.finite(row$icer)) row$icer else NA_real_
  d$outcome_measure <- row$outcome_m
  d$currency       <- row$currency
  d$currency_year  <- if (is.finite(row$currency_yr)) row$currency_yr else NA_real_
  d$country        <- row$country
  d$authors        <- row$authors
  d$year           <- if (is.finite(row$year)) row$year else NA_real_
  d$n              <- if (is.finite(row$n_patients)) row$n_patients else NA_real_
  d$source_type    <- row$source_type
  d$indication     <- row$indication
  d$population     <- row$population
  d$perspective    <- row$perspective
  d$time_horizon   <- row$time_horiz
  d$discount_rate  <- if (is.finite(row$disc_rate)) row$disc_rate else NA_real_
  d$threshold_referenced <- row$threshold
  d$conclusion     <- row$conclusion
  d$scenario       <- "base_case"
  d$submitted_by   <- submitted_by
  d
}

# ── UI ─────────────────────────────────────────────────────────────────────────

mod_rcema_transform_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(tags$style(HTML("
      .rcema-wrap { max-width: 1100px; margin: 0 auto; padding: 24px; }

      .rcema-pg-hdr { padding: 20px 0 16px; border-bottom: 1px solid #e5e5e5;
        margin-bottom: 20px; }
      .rcema-pg-hdr h1 { font-size: 22px; font-weight: 700; margin: 0 0 4px; }
      .rcema-pg-hdr p  { font-size: 14px; color: #737373; margin: 0; }

      .rcema-card { border: 1px solid #e5e5e5; border-radius: 4px;
        margin-bottom: 16px; }
      .rcema-card-hdr { display: flex; align-items: center; gap: 10px;
        padding: 11px 15px; border-bottom: 1px solid #e5e5e5;
        font-size: 13px; font-weight: 600; background: #fff;
        border-radius: 4px 4px 0 0; }
      .rcema-card-body { padding: 16px; }

      .rcema-badge { display: inline-block; font-size: 11px; font-weight: 600;
        padding: 2px 8px; border-radius: 3px; white-space: nowrap; }
      .rcema-badge-wide     { background: #dff3fb; color: #1c8ec0; }
      .rcema-badge-simple   { background: #dcfce7; color: #15803d; }
      .rcema-badge-ok       { background: #dcfce7; color: #15803d; }
      .rcema-badge-issues   { background: #fef9c3; color: #b45309; }
      .rcema-badge-blocking { background: #fee2e2; color: #b91c1c; }
      .rcema-badge-missing  { background: #f5f5f4; color: #737373;
        border: 1px solid #e5e5e5; }

      .rcema-qual-tbl { width: 100%; font-size: 13px; border-collapse: collapse; }
      .rcema-qual-tbl th { font-size: 11px; text-transform: uppercase;
        letter-spacing: 0.06em; color: #737373; font-weight: 600;
        padding: 6px 10px; border-bottom: 1.5px solid #0a0a0a; text-align: left; }
      .rcema-qual-tbl td { padding: 7px 10px; border-bottom: 1px solid #f0f0f0;
        vertical-align: middle; }
      .rcema-qual-tbl tr.rcema-req td:first-child { font-weight: 600; }

      .rcema-mono { font-family: monospace; font-size: 12px; }
      .rcema-pct-bar  { display: inline-block; width: 52px; height: 7px;
        background: #f0f0f0; border-radius: 2px; vertical-align: middle;
        margin-right: 5px; overflow: hidden; }
      .rcema-pct-fill { height: 100%; border-radius: 2px; }

      .rcema-stat { display: inline-block; background: #f5f5f4;
        border: 1px solid #e5e5e5; border-radius: 4px;
        padding: 10px 16px; margin-right: 10px; margin-bottom: 8px; }
      .rcema-stat-n { font-size: 22px; font-weight: 700; color: #0a0a0a; }
      .rcema-stat-l { font-size: 12px; color: #737373; }
      .rcema-stat-ok   .rcema-stat-n { color: #15803d; }
      .rcema-stat-warn .rcema-stat-n { color: #b45309; }
      .rcema-stat-bad  .rcema-stat-n { color: #b91c1c; }

      .rcema-feedback-box { font-family: 'SFMono-Regular', Consolas,
        'Liberation Mono', Menlo, monospace; font-size: 12px;
        background: #f8f9fa; border: 1px solid #e5e5e5; border-radius: 4px;
        padding: 12px 14px; white-space: pre-wrap; line-height: 1.6;
        max-height: 340px; overflow-y: auto; color: #0a0a0a; }

      .rcema-save-tbl { width: 100%; font-size: 12px; border-collapse: collapse; }
      .rcema-save-tbl th { font-size: 11px; text-transform: uppercase;
        letter-spacing: 0.06em; color: #737373; font-weight: 600;
        padding: 5px 8px; border-bottom: 1.5px solid #0a0a0a; text-align: left; }
      .rcema-save-tbl td { padding: 5px 8px; border-bottom: 1px solid #f0f0f0;
        vertical-align: top; max-width: 200px; overflow: hidden;
        text-overflow: ellipsis; white-space: nowrap; }

      .rcema-footer { display: flex; gap: 10px; margin-top: 12px; }
      .rcema-save-btn { font-weight: 600 !important; }
    "))),

    div(class = "rcema-wrap",

      div(class = "rcema-pg-hdr",
        tags$h1("RCEMA Data Quality & Synthesis Importer"),
        tags$p("Upload a CSV from RCEMA to check data quality, receive extractor
                feedback, and save study rows to the Evidence Synthesis database."),
        div(style = "margin-top: 8px; font-size: 13px; color: #737373;",
          "Entering data manually? Use the ",
          tags$a(href = "cea_studies_template.csv",
                 download = "cea_studies_template.csv",
                 "CEA studies template"),
          " — one row per strategy per study, with numeric cost, effect, and n."
        )
      ),

      # Card 1 — Upload
      div(class = "rcema-card",
        div(class = "rcema-card-hdr", "1. Upload RCEMA CSV"),
        div(class = "rcema-card-body",
          p(style = "font-size: 13px; color: #737373; margin-bottom: 12px;",
            "Upload any CSV exported from RCEMA. Wide format (with
             confidence/snippet columns) is handled automatically."),
          fluidRow(
            column(7,
              fileInput(ns("rcema_file"), NULL,
                        accept = c(".csv", "text/csv"),
                        placeholder = "Choose RCEMA CSV…",
                        width = "100%")
            ),
            column(5,
              div(style = "padding-top: 4px;",
                span(style = "font-size: 12px; color: #737373; margin-right: 8px;",
                     "Try an example:"),
                actionButton(ns("load_caffeine"), "Caffeine citrate",
                             class = "btn btn-sm btn-outline-secondary"),
                actionButton(ns("load_topclosure"), "Top closure",
                             class = "btn btn-sm btn-outline-secondary",
                             style = "margin-left: 4px;")
              )
            )
          ),
          uiOutput(ns("format_badge_ui"))
        )
      ),

      # Card 2 — Quality report
      uiOutput(ns("quality_ui")),

      # Card 3 — Name normalisation
      uiOutput(ns("normalize_ui")),

      # Card 4 — Save preview
      uiOutput(ns("save_ui"))
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_rcema_transform_server <- function(id, interventions, factors = NULL) {
  moduleServer(id, function(input, output, session) {

    rv <- reactiveValues(
      raw_orig   = NULL,
      raw_data   = NULL,   # column-name-normalised (for QA)
      row_df     = NULL,   # parse_rcema() output
      quality    = NULL,
      name_map   = NULL,
      filename   = "",
      saved_intervention = NULL,
      save_count = 0L
    )

    # ── Load & parse ──────────────────────────────────────────────────────
    .do_load <- function(path, filename = "") {
      raw <- tryCatch(
        read.csv(path, stringsAsFactors = FALSE,
                 na.strings = c("", "NA", "N/A"), fileEncoding = "UTF-8-BOM"),
        error = function(e) {
          showNotification(paste("Could not read file:", e$message),
                           type = "warning", duration = 6)
          NULL
        }
      )
      if (is.null(raw) || nrow(raw) == 0L) return()

      raw_norm <- raw
      is_wide  <- any(grepl("_confidence$", names(raw_norm)))
      if (is_wide) {
        dp <- "_(confidence|page|snippet|original_ai_value|edited_by|edited_at)$"
        raw_norm <- raw_norm[, !grepl(dp, names(raw_norm)), drop = FALSE]
      }
      names(raw_norm) <- trimws(tolower(gsub("[^a-z0-9_]", "_", names(raw_norm))))

      det_qa <- lapply(RCEMA_COL_HINTS,
                       function(h) .detect_col(names(raw_norm), h, raw_norm))

      # Auto-normalise country before QA so the report sees clean codes
      ctry_col <- det_qa$country
      if (!is.na(ctry_col) && ctry_col %in% names(raw_norm))
        raw_norm[[ctry_col]] <- .normalize_country_vec(raw_norm[[ctry_col]])

      quality <- .assess_columns(raw_norm, det_qa, nrow(raw_norm))

      # Build name map
      get_uniq <- function(col) {
        if (is.na(col) || !col %in% names(raw_norm)) return(character(0L))
        v <- trimws(as.character(raw_norm[[col]]))
        sort(unique(v[!is.na(v) & nzchar(v) & v != "not_available"]))
      }
      intv_names <- get_uniq(det_qa$intervention_name)
      comp_names <- get_uniq(det_qa$comparator_name)
      all_names  <- unique(c(intv_names, comp_names))
      name_type  <- ifelse(all_names %in% intv_names & all_names %in% comp_names,
                           "Both",
                    ifelse(all_names %in% intv_names, "Intervention", "Comparator"))
      ord <- order(factor(name_type,
                          levels = c("Intervention", "Comparator", "Both")),
                   all_names)
      name_map <- data.frame(
        type              = name_type[ord],
        original_name     = all_names[ord],
        standardized_name = all_names[ord],
        stringsAsFactors  = FALSE
      )

      rv$raw_orig <- raw
      rv$raw_data <- raw_norm
      rv$quality  <- quality
      rv$name_map <- name_map
      rv$filename <- filename
      rv$row_df   <- parse_rcema(raw, name_map = NULL, factors = factors)
    }

    observeEvent(input$rcema_file, {
      req(input$rcema_file)
      .do_load(input$rcema_file$datapath, input$rcema_file$name)
    })
    observeEvent(input$load_caffeine, {
      .do_load("data/caffeine-citrate-updated.csv", "caffeine-citrate-updated.csv")
    })
    observeEvent(input$load_topclosure, {
      f <- "data/top closure extraction results.csv"
      if (!file.exists(f)) f <- "data/TOP CLOSURE SHINY APP RESULTS.csv"
      if (file.exists(f)) .do_load(f, basename(f))
      else showNotification("Top closure file not found in data/",
                            type = "warning", duration = 4)
    })

    # Re-parse when name map is edited
    observeEvent(input$name_tbl_cell_edit, {
      info <- input$name_tbl_cell_edit
      # cols 0-indexed: 0=type(disabled), 1=original(disabled), 2=standardized → R col 3
      rv$name_map[info$row, info$col + 1L] <- info$value
      req(!is.null(rv$raw_orig))
      rv$row_df <- parse_rcema(rv$raw_orig, name_map = rv$name_map, factors = factors)
    })

    # ── Format badge ──────────────────────────────────────────────────────
    output$format_badge_ui <- renderUI({
      r <- rv$row_df
      if (is.null(r)) return(NULL)
      is_wide <- any(grepl("_confidence$", names(rv$raw_orig)))
      cls <- if (is_wide) "rcema-badge-wide" else "rcema-badge-simple"
      lbl <- if (is_wide)
        "Wide format (confidence/snippet columns stripped)" else "Compact format"
      div(style = "margin-top: 6px;",
        span(class = paste("rcema-badge", cls), lbl),
        span(style = "font-size: 12px; color: #737373; margin-left: 10px;",
             nrow(r), " rows loaded")
      )
    })

    # ── Quality report ────────────────────────────────────────────────────
    output$quality_ui <- renderUI({
      q  <- rv$quality
      r  <- rv$row_df
      nm <- rv$name_map
      if (is.null(q) || is.null(r)) return(NULL)

      n           <- nrow(rv$raw_data)
      n_blocking  <- sum(vapply(q, function(x) x$status == "blocking", logical(1L)))
      n_issues_w  <- sum(vapply(q, function(x) x$status == "issues",   logical(1L)))
      n_saveable  <- sum(r$save_intv) + sum(r$save_comp)

      stat_box <- function(val, lbl, cls = "") {
        div(class = paste("rcema-stat", cls),
          div(class = "rcema-stat-n", val),
          div(class = "rcema-stat-l", lbl)
        )
      }

      # Currency source breakdown (from the parsed row_df)
      cur_src_note <- if (!is.null(r) && "currency_source" %in% names(r)) {
        src  <- r$currency_source
        n_d  <- sum(src == "direct",                  na.rm = TRUE)
        n_ex <- sum(src == "extracted from cost",     na.rm = TRUE)
        n_in <- sum(src == "inferred from country",   na.rm = TRUE)
        n_m  <- sum(src == "missing",                 na.rm = TRUE)
        parts <- character()
        if (n_d  > 0L) parts <- c(parts, paste0(n_d,  " direct"))
        if (n_ex > 0L) parts <- c(parts, paste0(n_ex, " from cost text"))
        if (n_in > 0L) parts <- c(parts, paste0(n_in, " inferred"))
        if (n_m  > 0L) parts <- c(parts, paste0(n_m,  " unresolved"))
        if (length(parts) > 0L) paste(parts, collapse = " · ") else NULL
      } else NULL

      tbl_rows <- lapply(q, function(qi) {
        badge_cls <- switch(qi$status,
          ok = "rcema-badge-ok", issues = "rcema-badge-issues",
          blocking = "rcema-badge-blocking", "rcema-badge-missing")
        badge_lbl <- switch(qi$status,
          ok       = "✓ ready",
          issues   = paste0("⚠ ", paste(qi$issues, collapse = " · ")),
          blocking = if (qi$n_filled == 0L) "✗ missing"
                     else "⚠ not numeric",
          "not found")
        bar_col <- switch(qi$status,
          ok = "#15803d", issues = "#d97706",
          blocking = "#dc2626", "#d4d4d4")
        pct_html <- HTML(paste0(
          '<div class="rcema-pct-bar"><div class="rcema-pct-fill" style="width:',
          qi$pct, '%; background:', bar_col,
          ';"></div></div><span style="font-size:12px;">',
          qi$n_filled, '/', n, ' (', qi$pct, '%)</span>'
        ))
        row_cls <- if (qi$required) "rcema-req" else ""

        # Currency row: append resolved-count badge + source breakdown note
        status_cell <- if (qi$role == "currency" && !is.null(cur_src_note)) {
          n_resolved <- n - (if (!is.null(r)) sum(r$currency_source == "missing") else 0L)
          res_cls    <- if (n_resolved == n) "rcema-badge-ok"
                        else if (n_resolved > 0L) "rcema-badge-issues"
                        else "rcema-badge-blocking"
          tagList(
            span(class = paste("rcema-badge", res_cls),
                 paste0(n_resolved, "/", n, " resolved")),
            tags$br(),
            tags$span(style = "font-size: 11px; color: #737373;", cur_src_note)
          )
        } else {
          span(class = paste("rcema-badge", badge_cls), badge_lbl)
        }

        tags$tr(class = row_cls,
          tags$td(qi$label),
          tags$td(if (!is.na(qi$col)) span(class = "rcema-mono", qi$col)
                  else tags$em(style = "color:#aaa;", "—")),
          tags$td(pct_html),
          tags$td(status_cell)
        )
      })

      n_intv_names <- {
        col <- rv$row_df  # use parsed names
        length(unique(na.omit(r$intv_name)))
      }
      n_comp_names <- length(unique(na.omit(r$comp_name)))

      feedback_txt <- .generate_feedback(
        rv$raw_data, lapply(RCEMA_COL_HINTS,
          function(h) .detect_col(names(rv$raw_data), h, rv$raw_data)),
        q, n_intv_names, n_comp_names, rv$filename)

      div(class = "rcema-card",
        div(class = "rcema-card-hdr", "2. Data Quality Report"),
        div(class = "rcema-card-body",
          div(style = "margin-bottom: 14px;",
            stat_box(n, "rows loaded"),
            stat_box(n_saveable, "study entries to save",
                     if (n_saveable > 0L) "rcema-stat-ok" else "rcema-stat-bad"),
            stat_box(n_blocking, "required columns missing / non-numeric",
                     if (n_blocking > 0L) "rcema-stat-bad" else "rcema-stat-ok"),
            stat_box(n_issues_w, "columns with format issues",
                     if (n_issues_w > 0L) "rcema-stat-warn" else "")
          ),
          tags$table(class = "rcema-qual-tbl",
            tags$thead(tags$tr(tags$th("Field"), tags$th("Detected column"),
                               tags$th("Completeness"), tags$th("Status"))),
            tags$tbody(tbl_rows)
          ),
          tags$p(style = "font-size: 12px; color: #737373; margin-top: 8px;",
            tags$em("Country names/codes are auto-resolved to ISO alpha-3
                     (e.g. \"Kenya\" → KEN, \"ISR - Israel\" → ISR, \"US\" → USA).
                     For multi-country cells only the first country is used.")),
          tags$hr(style = "margin: 12px 0;"),
          tags$p(style = "font-size: 13px; font-weight: 600; margin-bottom: 8px;",
            "Feedback for data extractors"),
          div(class = "rcema-feedback-box", feedback_txt),
          div(class = "rcema-footer",
            downloadButton(session$ns("dl_feedback"),
                           "↓ Download feedback report (.txt)",
                           class = "btn btn-sm btn-outline-secondary")
          )
        )
      )
    })

    output$dl_feedback <- downloadHandler(
      filename = function() {
        nm <- gsub("[^a-z0-9]", "_",
                   tolower(tools::file_path_sans_ext(rv$filename)))
        if (!nzchar(nm)) nm <- "rcema"
        paste0("data_quality_", nm, "_", format(Sys.Date(), "%Y%m%d"), ".txt")
      },
      content = function(file) {
        q  <- rv$quality
        r  <- rv$row_df
        n_intv <- length(unique(na.omit(r$intv_name)))
        n_comp <- length(unique(na.omit(r$comp_name)))
        det <- lapply(RCEMA_COL_HINTS,
                      function(h) .detect_col(names(rv$raw_data), h, rv$raw_data))
        txt <- .generate_feedback(rv$raw_data, det, q, n_intv, n_comp, rv$filename)
        writeLines(txt, file)
      }
    )

    # ── Name normalisation ────────────────────────────────────────────────
    output$normalize_ui <- renderUI({
      nm <- rv$name_map
      if (is.null(nm) || nrow(nm) < 2L) return(NULL)
      r      <- rv$row_df
      n_intv <- length(unique(na.omit(r$intv_name)))
      n_comp <- length(unique(na.omit(r$comp_name)))

      div(class = "rcema-card",
        div(class = "rcema-card-hdr", "3. Normalize Strategy Names"),
        div(class = "rcema-card-body",
          p(style = "font-size: 13px; color: #737373; margin-bottom: 10px;",
            tags$strong(n_intv), " unique intervention names and ",
            tags$strong(n_comp), " unique comparator names. ",
            "Edit the ", tags$strong("Standardized name"), " column to merge
             equivalent strategies before saving.",
            tags$br(),
            "The ", tags$strong("Type"), " column shows whether each name appears
             as an intervention, a comparator, or both. Edits only affect rows of
             that type — renaming a comparator never changes intervention grouping."
          ),
          DT::dataTableOutput(session$ns("name_tbl")),
          p(style = "font-size: 12px; color: #737373; margin-top: 8px;",
            "Click any cell in the Standardized name column to edit.
             Save preview (below) updates automatically.")
        )
      )
    })

    output$name_tbl <- DT::renderDataTable({
      nm <- rv$name_map
      req(!is.null(nm), nrow(nm) > 0L)
      DT::datatable(
        nm,
        colnames  = c("Type", "Original name (from file)", "Standardized name"),
        editable  = list(target = "cell", disable = list(columns = c(0L, 1L))),
        rownames  = FALSE,
        selection = "none",
        options   = list(dom = "t", pageLength = 100, scrollX = TRUE,
                         ordering = FALSE,
                         columnDefs = list(
                           list(width = "12%", targets = 0L),
                           list(width = "44%", targets = 1L),
                           list(width = "44%", targets = 2L)
                         )),
        class = "table table-sm"
      ) |>
        DT::formatStyle("type",
          color = DT::styleEqual(
            c("Intervention", "Comparator", "Both"),
            c("#1c8ec0",       "#b45309",    "#15803d")
          ),
          fontWeight = "600", fontSize = "11px"
        )
    }, server = FALSE)

    # ── Save preview ──────────────────────────────────────────────────────
    output$save_ui <- renderUI({
      r  <- rv$row_df
      nm <- rv$name_map
      if (is.null(r)) return(NULL)

      card_n     <- if (!is.null(nm) && nrow(nm) >= 2L) "4" else "3"
      n_intv_ent <- sum(r$save_intv)
      n_comp_ent <- sum(r$save_comp)
      n_total    <- n_intv_ent + n_comp_ent
      n_with_icer   <- sum(r$save_intv & is.finite(r$icer))
      n_with_effect <- sum(r$save_intv & is.finite(r$effect))

      stat_box <- function(val, lbl, cls = "") {
        div(class = paste("rcema-stat", cls),
          div(class = "rcema-stat-n", val),
          div(class = "rcema-stat-l", lbl)
        )
      }

      content <- if (n_total == 0L) {
        div(class = "alert alert-info", style = "font-size: 13px; margin: 0;",
          tags$strong("Nothing to save yet. "),
          "No rows have a strategy name combined with a cost, ICER, or effect value.
           Review the Data Quality Report above for guidance on what to fix."
        )
      } else {
        save_rows <- r[r$save_intv | r$save_comp, , drop = FALSE]
        tbl_data  <- data.frame(
          Role       = ifelse(save_rows$save_intv, "Intervention", "Comparator"),
          Strategy   = ifelse(save_rows$save_intv, save_rows$intv_name,
                                                   save_rows$comp_name),
          Cost       = ifelse(
            save_rows$save_intv & is.finite(save_rows$intv_cost),
            formatC(save_rows$intv_cost, format = "f", digits = 0, big.mark = ","),
            ifelse(
              !save_rows$save_intv & is.finite(save_rows$comp_cost),
              formatC(save_rows$comp_cost, format = "f", digits = 0, big.mark = ","),
              "—"
            )
          ),
          ICER       = ifelse(is.finite(save_rows$icer) & save_rows$save_intv,
                              formatC(save_rows$icer, format = "f", digits = 0,
                                      big.mark = ","),
                              "—"),
          Effect     = ifelse(is.finite(save_rows$effect) & save_rows$save_intv,
                              formatC(save_rows$effect, format = "f", digits = 3),
                              "—"),
          Outcome    = save_rows$outcome_m,
          Country    = save_rows$country,
          Currency   = ifelse(
            is.na(save_rows$currency), "—",
            paste0(save_rows$currency,
                   ifelse(save_rows$currency_source == "direct", "",
                   ifelse(save_rows$currency_source == "extracted from cost", " *",
                   ifelse(save_rows$currency_source == "inferred from country", " †", ""))))
          ),
          stringsAsFactors = FALSE
        )
        tagList(
          p(style = "font-size: 13px; color: #737373; margin-bottom: 10px;",
            n_total, " study entries ready to save: ",
            n_intv_ent, " intervention rows, ", n_comp_ent, " comparator rows.",
            if (n_with_icer > 0L)
              paste0(" ", n_with_icer, " have a reported ICER (stored for reference)."),
            if (n_with_effect > 0L)
              paste0(" ", n_with_effect, " have a directly reported numeric effect.")
          ),
          div(style = "overflow-x: auto;",
            tags$table(class = "rcema-save-tbl",
              tags$thead(tags$tr(
                lapply(names(tbl_data), function(h) tags$th(h))
              )),
              tags$tbody(
                lapply(seq_len(nrow(tbl_data)), function(i) {
                  tags$tr(lapply(tbl_data[i, ], function(v)
                    tags$td(if (is.na(v)) "—" else as.character(v))))
                })
              )
            )
          ),
          {
            has_ex <- any(save_rows$currency_source == "extracted from cost",  na.rm = TRUE)
            has_in <- any(save_rows$currency_source == "inferred from country", na.rm = TRUE)
            if (has_ex || has_in)
              tags$p(style = "font-size: 11px; color: #737373; margin-top: 6px;",
                if (has_ex) "* currency extracted from cost column text.  " else NULL,
                if (has_in) "† currency inferred from country code — verify before saving." else NULL
              )
          }
        )
      }

      save_btn <- if (n_total > 0L)
        actionButton(session$ns("open_save_modal"),
                     paste0("Save ", n_total, " entries to Evidence Synthesis →"),
                     class = "btn btn-primary rcema-save-btn")
      else NULL

      div(class = "rcema-card",
        div(class = "rcema-card-hdr",
            paste0(card_n, ". Save to Evidence Synthesis")),
        div(class = "rcema-card-body",
          content,
          if (!is.null(save_btn))
            div(class = "rcema-footer", save_btn)
        )
      )
    })

    # ── Save modal ────────────────────────────────────────────────────────
    observeEvent(input$open_save_modal, {
      r      <- rv$row_df
      n_tot  <- sum(r$save_intv) + sum(r$save_comp)
      intv_df <- if (!is.null(interventions) && nrow(interventions) > 0L)
        interventions else data.frame(intervention = character(), stringsAsFactors = FALSE)
      intv_choices <- setNames(intv_df$intervention, intv_df$intervention)

      showModal(modalDialog(
        title = "Save to Evidence Synthesis",
        tags$p(style = "font-size: 13px;",
          n_tot, " study entries will be saved to the database and linked to
           the SHA intervention you select below. The Evidence Synthesis tab
           will open automatically so you can review, standardise costs to KES,
           and pool before analysis."
        ),
        selectInput(session$ns("sha_intv_select"),
                    "SHA intervention:",
                    choices  = c("Select…" = "", intv_choices),
                    selected = ""),
        textInput(session$ns("submitter_name"),
                  "Your name (for audit trail):",
                  placeholder = "e.g. Jane Doe"),
        tags$p(style = "font-size: 12px; color: #737373; margin-top: 4px;",
          "ICER values are stored as reported_icer for reference.
           They are not used to derive effects — the Synthesis tab will show
           the mean reported ICER separately alongside pooled costs."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_save"),
                       "Save and open Synthesis →",
                       class = "btn btn-primary")
        ),
        easyClose = FALSE,
        size = "m"
      ))
    })

    observeEvent(input$confirm_save, {
      sha_intv  <- input$sha_intv_select
      submitter <- trimws(input$submitter_name)
      if (!nzchar(sha_intv)) {
        showNotification("Please select an SHA intervention.",
                         type = "warning", duration = 4)
        return()
      }
      if (!nzchar(submitter)) submitter <- "rcema_transform"

      r         <- rv$row_df
      n_saved   <- 0L
      n_failed  <- 0L

      for (i in seq_len(nrow(r))) {
        if (r$save_intv[i]) {
          row <- .to_study_row(r[i, ], "intervention", sha_intv, submitter)
          if (gs_write_study(row)) n_saved <- n_saved + 1L else n_failed <- n_failed + 1L
        }
        if (r$save_comp[i]) {
          row <- .to_study_row(r[i, ], "comparator", sha_intv, submitter)
          if (gs_write_study(row)) n_saved <- n_saved + 1L else n_failed <- n_failed + 1L
        }
      }

      removeModal()

      if (n_failed > 0L)
        showNotification(
          paste0(n_saved, " saved; ", n_failed, " failed — check GS connection."),
          type = "warning", duration = 8)
      else
        showNotification(paste0(n_saved, " study entries saved."),
                         type = "message", duration = 4)

      rv$saved_intervention <- sha_intv
      rv$save_count         <- rv$save_count + 1L
    })

    list(
      saved_intervention = reactive(rv$saved_intervention),
      save_count         = reactive(rv$save_count)
    )
  })
}
