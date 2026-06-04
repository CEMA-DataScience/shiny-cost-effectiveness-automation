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
