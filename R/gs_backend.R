# gs_backend.R
# Google Sheets read/write backend.
#
# Two sheets:
#   INTERVENTIONS_ID  — public, read-only, the SHA prioritisation matrix
#   STUDIES_ID        — private, read/write via service account
#
# Call gs_init() once at app startup. After that use the public helpers.

INTERVENTIONS_ID <- "1PLNi0FvORu-uGWrH5o56rtCSEmF1dKwULSu07-3MZ5s"
STUDIES_ID       <- "1L0bouGvP3VpzaG993JchhkkhRf9GHwtm4C--ZCpmJHs"
STUDIES_SHEET    <- "Sheet1"
KEY_PATH         <- ".secrets/cema-cea-tool.json"

# Column schema for the studies sheet (order matters — matches sheet columns).
STUDIES_COLS <- c(
  "record_id", "intervention", "strategy", "reference_id",
  "authors", "year", "source_type",
  "indication", "population", "comparator",
  "country", "currency", "currency_year",
  "perspective", "time_horizon", "discount_rate",
  "outcome_measure", "cost", "effect", "n",
  "scenario", "reported_icer", "threshold_referenced", "conclusion",
  "submitted_by", "submitted_at"
)

# ── Authentication ─────────────────────────────────────────────────────────────

#' Initialise Google Sheets auth.
#' Checks GS_SERVICE_ACCOUNT_JSON env var first (shinyapps.io / CI),
#' then falls back to the local key file (development).
#' Returns TRUE if write-capable, FALSE if read-only.
gs_init <- function() {
  json_env <- Sys.getenv("GS_SERVICE_ACCOUNT_JSON", unset = "")

  if (nzchar(json_env)) {
    tryCatch({
      key_file <- tempfile(fileext = ".json")
      writeLines(json_env, key_file)
      googlesheets4::gs4_auth(path = key_file)
      message("[gs] Authenticated via GS_SERVICE_ACCOUNT_JSON env var.")
      return(invisible(TRUE))
    }, error = function(e) {
      warning("[gs] Env var auth failed: ", e$message, "\nTrying local key file.")
    })
  }

  if (file.exists(KEY_PATH)) {
    tryCatch({
      googlesheets4::gs4_auth(path = KEY_PATH)
      message("[gs] Authenticated via local service account file.")
      return(invisible(TRUE))
    }, error = function(e) {
      warning("[gs] Local key auth failed: ", e$message,
              "\nFalling back to read-only mode.")
    })
  } else {
    message("[gs] No credentials found — read-only mode.")
  }

  googlesheets4::gs4_deauth()
  invisible(FALSE)
}

# ── Interventions (read-only, public) ─────────────────────────────────────────

#' Read and clean the SHA prioritisation matrix.
#' Returns a data frame with columns: reference_id, intervention, benefit_package.
#' Row 1 of the sheet is a merged title; headers are on row 2 (skip = 1).
#' Category-header rows (no Routing/Decision) are dropped.
gs_read_interventions <- function() {
  tryCatch({
    raw <- suppressMessages(
      googlesheets4::read_sheet(
        INTERVENTIONS_ID,
        sheet     = "Merged Interventions",
        skip      = 1,
        col_types = "c"
      )
    )

    # Flatten any list columns (merged cells come back as lists)
    raw[] <- lapply(raw, function(col) {
      if (is.list(col))
        vapply(col, function(v) paste(unlist(v), collapse = "; "), character(1L))
      else col
    })

    # Keep rows that have both an intervention name and a routing decision
    has_name    <- !is.na(raw[["Proposed Intervention"]]) &
                   nchar(trimws(raw[["Proposed Intervention"]])) > 0
    has_routing <- !is.na(raw[["Routing / Decision"]]) &
                   nchar(trimws(raw[["Routing / Decision"]])) > 0
    d <- raw[has_name & has_routing, ]

    data.frame(
      reference_id    = trimws(as.character(d[["Reference"]])),
      intervention    = trimws(as.character(d[["Proposed Intervention"]])),
      benefit_package = trimws(as.character(d[["Benefit Package"]])),
      routing         = trimws(as.character(d[["Routing / Decision"]])),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    warning("[gs] Could not read interventions sheet: ", e$message)
    data.frame(reference_id = character(), intervention = character(),
               benefit_package = character(), routing = character(),
               stringsAsFactors = FALSE)
  })
}

# ── Studies (read/write) ───────────────────────────────────────────────────────

#' Ensure the studies sheet has the correct header row.
#' Safe to call every startup — only writes if expected columns are absent.
#' Uses sheet_write() with a 0-row data frame, which writes headers only.
gs_ensure_headers <- function() {
  tryCatch({
    existing_cols <- suppressMessages(
      names(googlesheets4::read_sheet(STUDIES_ID, sheet = STUDIES_SHEET, n_max = 0))
    )
    if (!all(STUDIES_COLS %in% existing_cols)) {
      empty_df <- setNames(
        data.frame(matrix(ncol = length(STUDIES_COLS), nrow = 0L),
                   stringsAsFactors = FALSE),
        STUDIES_COLS
      )
      googlesheets4::sheet_write(empty_df, ss = STUDIES_ID, sheet = STUDIES_SHEET)
      message("[gs] Headers written to studies sheet.")
    } else {
      message("[gs] Studies sheet headers OK.")
    }
  }, error = function(e) {
    warning("[gs] Could not ensure headers: ", e$message)
  })
}

#' Read all studies, optionally filtered by intervention name.
#' Returns a data frame with STUDIES_COLS columns.
gs_read_studies <- function(intervention = NULL) {
  tryCatch({
    d <- suppressMessages(
      googlesheets4::read_sheet(STUDIES_ID, sheet = STUDIES_SHEET,
                                col_types = "c")
    )
    if (nrow(d) == 0L) return(.empty_studies())

    # Coerce numeric columns
    for (col in c("year", "currency_year", "discount_rate",
                  "cost", "effect", "n", "reported_icer")) {
      if (col %in% names(d))
        d[[col]] <- suppressWarnings(as.numeric(d[[col]]))
    }

    if (!is.null(intervention))
      d <- d[!is.na(d$intervention) & d$intervention == intervention, ]

    d
  }, error = function(e) {
    warning("[gs] Could not read studies: ", e$message)
    .empty_studies()
  })
}

#' Append one study row to the sheet.
#' @param study Named list or single-row data frame with fields from STUDIES_COLS.
#' @return TRUE on success, FALSE on failure.
gs_write_study <- function(study) {
  study <- as.list(study)

  # App-managed fields
  study$record_id   <- paste0("STUDY-", format(Sys.time(), "%Y%m%d%H%M%S"),
                               "-", sample(1000L:9999L, 1L))
  study$submitted_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Build a one-row data frame in column order
  row_df <- as.data.frame(
    lapply(STUDIES_COLS, function(col) {
      v <- study[[col]]
      if (is.null(v) || (length(v) == 1L && is.na(v))) NA_character_
      else as.character(v)
    }),
    col.names     = STUDIES_COLS,
    stringsAsFactors = FALSE
  )

  tryCatch({
    googlesheets4::sheet_append(STUDIES_ID, data = row_df,
                                sheet = STUDIES_SHEET)
    message("[gs] Study appended: ", study$record_id)
    TRUE
  }, error = function(e) {
    warning("[gs] Write failed: ", e$message)
    FALSE
  })
}

# ── Demo / sample data ────────────────────────────────────────────────────────

#' Parse an RCEMA-format CSV into STUDIES_COLS layout for demo display.
#' @param path         Path to the RCEMA CSV file.
#' @param intervention String label for the demo intervention name.
#' @param prefix       record_id prefix, e.g. "DEMO-ART".
#' @param cost_col     RCEMA column name to map to `cost`.
#' @param icer_col     RCEMA column name to map to `reported_icer`.
#' @param wide         TRUE = strip _confidence/_page/_snippet/etc. columns first.
.load_rcema_demo <- function(path, intervention, prefix,
                              cost_col = NULL, icer_col = NULL, wide = FALSE) {
  if (!file.exists(path)) return(.empty_studies())
  raw <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA", "N/A"),
             fileEncoding = "UTF-8-BOM"),
    error = function(e) NULL
  )
  if (is.null(raw) || nrow(raw) == 0L) return(.empty_studies())

  if (wide) {
    drop_pat <- "_(confidence|page|snippet|original_ai_value|edited_by|edited_at)$"
    raw <- raw[, !grepl(drop_pat, names(raw)), drop = FALSE]
  }
  names(raw) <- trimws(tolower(gsub("[^a-z0-9_]", "_", names(raw))))

  n <- nrow(raw)
  d <- as.data.frame(
    matrix(NA_character_, nrow = n, ncol = length(STUDIES_COLS),
           dimnames = list(NULL, STUDIES_COLS)),
    stringsAsFactors = FALSE
  )

  d$record_id    <- sprintf("%s-%03d", prefix, seq_len(n))
  d$intervention <- intervention
  d$submitted_by <- "demo"
  d$strategy     <- if ("intervention" %in% names(raw))
    as.character(raw$intervention) else intervention

  shared <- list(
    authors              = c("authors"),
    year                 = c("year_of_publication"),
    source_type          = c("source_type"),
    indication           = c("indication"),
    population           = c("population"),
    comparator           = c("comparator"),
    country              = c("country", "country_of_study"),
    currency             = c("currency"),
    currency_year        = c("currency_year"),
    perspective          = c("perspective"),
    time_horizon         = c("time_horizon"),
    discount_rate        = c("discount_rate"),
    outcome_measure      = c("outcome_measure"),
    conclusion           = c("conclusion"),
    threshold_referenced = c("threshold_referenced", "threshold_refrenced")
  )
  for (col in names(shared)) {
    hit <- shared[[col]][shared[[col]] %in% names(raw)][1L]
    if (!is.na(hit)) d[[col]] <- as.character(raw[[hit]])
  }

  if (!is.null(cost_col) && cost_col %in% names(raw)) {
    cost_raw <- as.character(raw[[cost_col]])
    m <- regexpr("[0-9][0-9,\\.]*", cost_raw)
    d$cost <- suppressWarnings(
      as.numeric(ifelse(m > 0L, gsub(",", "", regmatches(cost_raw, m)), NA_character_))
    )
  }

  if (!is.null(icer_col) && icer_col %in% names(raw)) {
    icer_raw <- as.character(raw[[icer_col]])
    m <- regexpr("[0-9][0-9,\\.]*", icer_raw)
    extracted <- ifelse(m > 0L,
                        gsub(",", "", regmatches(icer_raw, m)),
                        NA_character_)
    d$reported_icer <- suppressWarnings(as.numeric(extracted))
  }

  for (col in c("year", "currency_year", "discount_rate",
                "cost", "effect", "n", "reported_icer"))
    d[[col]] <- suppressWarnings(as.numeric(d[[col]]))

  d
}

#' Load arthroplasty demo studies (STUDIES_COLS format with cost/effect/n).
gs_load_demo_arthroplasty <- function() {
  path <- "data/demo_arthroplasty.csv"
  if (!file.exists(path)) return(.empty_studies())
  raw <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA", "N/A")),
    error = function(e) NULL
  )
  if (is.null(raw) || nrow(raw) == 0L) return(.empty_studies())
  names(raw) <- trimws(tolower(gsub("[^a-z0-9_]", "_", names(raw))))
  n <- nrow(raw)
  d <- as.data.frame(
    matrix(NA_character_, nrow = n, ncol = length(STUDIES_COLS),
           dimnames = list(NULL, STUDIES_COLS)),
    stringsAsFactors = FALSE
  )
  d$record_id    <- sprintf("DEMO-ART-%03d", seq_len(n))
  d$intervention <- "Arthroplasty [Demo]"
  d$submitted_by <- "demo"
  for (col in intersect(names(raw), STUDIES_COLS))
    d[[col]] <- as.character(raw[[col]])
  for (col in c("year", "currency_year", "discount_rate",
                "cost", "effect", "n", "reported_icer"))
    d[[col]] <- suppressWarnings(as.numeric(d[[col]]))
  d
}

#' Load caffeine-citrate demo studies (STUDIES_COLS format with cost/effect/n).
gs_load_demo_caffeine <- function() {
  path <- "data/demo_caffeine_citrate.csv"
  if (!file.exists(path)) return(.empty_studies())
  raw <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA", "N/A")),
    error = function(e) NULL
  )
  if (is.null(raw) || nrow(raw) == 0L) return(.empty_studies())
  names(raw) <- trimws(tolower(gsub("[^a-z0-9_]", "_", names(raw))))
  n <- nrow(raw)
  d <- as.data.frame(
    matrix(NA_character_, nrow = n, ncol = length(STUDIES_COLS),
           dimnames = list(NULL, STUDIES_COLS)),
    stringsAsFactors = FALSE
  )
  d$record_id    <- sprintf("DEMO-CAF-%03d", seq_len(n))
  d$intervention <- "Caffeine citrate [Demo]"
  d$submitted_by <- "demo"
  for (col in intersect(names(raw), STUDIES_COLS))
    d[[col]] <- as.character(raw[[col]])
  for (col in c("year", "currency_year", "discount_rate",
                "cost", "effect", "n", "reported_icer"))
    d[[col]] <- suppressWarnings(as.numeric(d[[col]]))
  d
}

#' Load the bundled demo studies CSV and format it to match STUDIES_COLS.
#' record_ids are prefixed "DEMO-" so callers can detect sample rows.
#' @param intervention  String — the currently selected intervention name.
gs_load_demo_studies <- function(intervention = "") {
  path <- "data/demo_arthroplasty.csv"
  if (!file.exists(path)) return(.empty_studies())

  raw <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA", "N/A")),
    error = function(e) NULL
  )
  if (is.null(raw) || nrow(raw) == 0L) return(.empty_studies())

  names(raw) <- trimws(tolower(gsub("[^a-z0-9_]", "_", names(raw))))
  n <- nrow(raw)

  d <- as.data.frame(
    matrix(NA_character_, nrow = n, ncol = length(STUDIES_COLS),
           dimnames = list(NULL, STUDIES_COLS)),
    stringsAsFactors = FALSE
  )

  d$record_id    <- sprintf("DEMO-%03d", seq_len(n))
  d$intervention <- intervention
  d$submitted_by <- "sample"

  for (col in intersect(names(raw), STUDIES_COLS))
    d[[col]] <- as.character(raw[[col]])

  for (col in c("year", "currency_year", "discount_rate",
                "cost", "effect", "n", "reported_icer"))
    d[[col]] <- suppressWarnings(as.numeric(d[[col]]))

  d
}

# ── Internal ───────────────────────────────────────────────────────────────────

.empty_studies <- function() {
  d <- as.data.frame(
    matrix(nrow = 0L, ncol = length(STUDIES_COLS),
           dimnames = list(NULL, STUDIES_COLS)),
    stringsAsFactors = FALSE
  )
  for (col in c("year", "currency_year", "discount_rate",
                "cost", "effect", "n", "reported_icer"))
    d[[col]] <- numeric(0L)
  d
}
