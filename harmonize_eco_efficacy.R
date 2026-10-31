# =============================================================================
# Data Harmonization Function for Eco-Efficacy Data Log
# Based on: eco-efficacy_data-log_-_detailed_template.csv
# Schema defined by row 1 (column names) and row 2 (field descriptions)
# =============================================================================


# --- Expected schema ---------------------------------------------------------

EXPECTED_COLS <- c(
  "data_finder", "title", "climate_adaptation_action", "outcomes",
  "climate_type", "avg_precip", "min_temp", "max_temp", "description",
  "doi/package_url", "datatable_url", "creator", "date_downloaded",
  "date_creation", "taxa_coverage", "geo_coverage", "time_coverage",
  "method", "format", "variables", "license", "restrictions_on_use",
  "citation", "reference_publication", "status", "metadata_checker",
  "comments", "***"
)

# Columns that must not be blank for a record to be usable
REQUIRED_COLS <- c(
  "data_finder", "title", "climate_adaptation_action", "outcomes",
  "doi/package_url", "status"
)

# Allowed values for controlled-vocabulary columns
ALLOWED_STATUS <- c("found", "explore", "keep", "discard", "pending")

ALLOWED_FORMATS <- c(
  "csv", "txt", "json", "netcdf", "geojson", "geotiff",
  "xlsx", "tsv", "shapefile", "hdf5", "rdata", "rds"
)

ALLOWED_LICENSES <- c(
  "cc0", "cc by", "cc by 4.0", "cc by-sa", "cc by-nc",
  "cc by-nc-sa", "public domain", "open data commons", "other"
)


# --- Helper: normalise a free-text vector to lowercase, trimmed --------------

normalise <- function(x) trimws(tolower(x))


# --- Main harmonization function ---------------------------------------------

#' harmonize_eco_data
#'
#' Reads the eco-efficacy data log CSV, skips the description row (row 2),
#' and runs a suite of checks on the data rows.
#'
#' @param filepath  Path to the CSV file.
#' @param skip_desc Logical; if TRUE (default) the second row (field
#'                  descriptions) is dropped before checks.
#' @param verbose   Logical; print a summary after checking (default TRUE).
#'
#' @return A list with elements:
#'   \item{data}{The cleaned data.frame (description row removed).}
#'   \item{issues}{A data.frame of every issue found: row, column, message.}
#'   \item{passed}{Logical scalar; TRUE only when zero issues are found.}

harmonize_eco_data <- function(filepath,
                               skip_desc = TRUE,
                               verbose   = TRUE) {

  # ---- 1. Load -----------------------------------------------------------
  raw <- read.csv(
    filepath,
    header           = TRUE,
    stringsAsFactors = FALSE,
    na.strings       = c("", "NA", "N/A", "n/a"),
    check.names      = FALSE   # keep special characters in col names
  )

  # ---- 2. Drop description row (row 2 of the file = row 1 of raw) --------
  if (skip_desc && nrow(raw) >= 1) {
    raw <- raw[-1, , drop = FALSE]
    rownames(raw) <- seq_len(nrow(raw))
  }

  issues <- data.frame(
    row     = integer(),
    column  = character(),
    message = character(),
    stringsAsFactors = FALSE
  )

  add_issue <- function(row, col, msg) {
    issues <<- rbind(
      issues,
      data.frame(row = row, column = col, message = msg,
                 stringsAsFactors = FALSE)
    )
  }

  # ---- 3. Column presence check ------------------------------------------
  missing_cols  <- setdiff(EXPECTED_COLS, names(raw))
  extra_cols    <- setdiff(names(raw), EXPECTED_COLS)

  if (length(missing_cols) > 0) {
    add_issue(NA, paste(missing_cols, collapse = ", "),
              "Expected column(s) not found in file")
  }
  if (length(extra_cols) > 0) {
    add_issue(NA, paste(extra_cols, collapse = ", "),
              "Unexpected extra column(s) found in file")
  }

  # Work only with columns that are actually present
  present_required <- intersect(REQUIRED_COLS, names(raw))

  # ---- 4. Row-level checks -----------------------------------------------
  for (i in seq_len(nrow(raw))) {

    row_data <- raw[i, ]

    ## 4a. Required fields must not be NA / blank
    for (col in present_required) {
      val <- row_data[[col]]
      if (is.na(val) || trimws(val) == "") {
        add_issue(i, col, "Required field is missing or blank")
      }
    }

    ## 4b. `status` must be a recognised value
    if ("status" %in% names(raw)) {
      s <- normalise(row_data[["status"]])
      if (!is.na(s) && s != "" && !(s %in% ALLOWED_STATUS)) {
        add_issue(i, "status",
                  paste0("Unrecognised status value: '", row_data[["status"]],
                         "'. Expected one of: ",
                         paste(ALLOWED_STATUS, collapse = ", ")))
      }
    }

    ## 4c. `format` should contain only recognised format tokens
    if ("format" %in% names(raw)) {
      fmt_raw <- row_data[["format"]]
      if (!is.na(fmt_raw) && trimws(fmt_raw) != "") {
        # formats may be comma- or semicolon-separated
        tokens <- strsplit(fmt_raw, "[,;/]+")[[1]]
        tokens <- normalise(tokens)
        tokens <- tokens[tokens != ""]
        bad <- tokens[!tokens %in% ALLOWED_FORMATS]
        if (length(bad) > 0) {
          add_issue(i, "format",
                    paste0("Unrecognised format token(s): ",
                           paste0("'", bad, "'", collapse = ", "),
                           ". Expected one of: ",
                           paste(ALLOWED_FORMATS, collapse = ", ")))
        }
      }
    }

    ## 4d. `license` should match a recognised licence family
    if ("license" %in% names(raw)) {
      lic <- normalise(row_data[["license"]])
      if (!is.na(lic) && lic != "") {
        # partial match: any recognised token contained in the licence string?
        matched <- any(sapply(ALLOWED_LICENSES, function(l) grepl(l, lic,
                                                                    fixed = TRUE)))
        if (!matched) {
          add_issue(i, "license",
                    paste0("Licence value could not be matched to a ",
                           "recognised type: '", row_data[["license"]], "'"))
        }
      }
    }

    ## 4e. `doi/package_url` should look like a DOI or URL
    if ("doi/package_url" %in% names(raw)) {
      doi_val <- trimws(row_data[["doi/package_url"]])
      if (!is.na(doi_val) && doi_val != "") {
        is_doi <- grepl("^10\\.", doi_val) ||
                  grepl("doi\\.org", doi_val, ignore.case = TRUE)
        is_url <- grepl("^https?://", doi_val, ignore.case = TRUE)
        if (!is_doi && !is_url) {
          add_issue(i, "doi/package_url",
                    paste0("Value does not look like a DOI or URL: '",
                           doi_val, "'"))
        }
      }
    }

    ## 4f. `date_downloaded` and `date_creation` — recognisable date format?
    for (date_col in c("date_downloaded", "date_creation")) {
      if (date_col %in% names(raw)) {
        d_raw <- trimws(row_data[[date_col]])
        if (!is.na(d_raw) && d_raw != "" &&
            !grepl("not yet", d_raw, ignore.case = TRUE)) {
          # Accept YYYY-MM-DD, MM/DD/YYYY, or bare year (YYYY)
          ok <- grepl("^\\d{4}-\\d{2}-\\d{2}$", d_raw) ||
                grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", d_raw) ||
                grepl("^\\d{4}$", d_raw)
          if (!ok) {
            add_issue(i, date_col,
                      paste0("Date format not recognised: '", d_raw,
                             "'. Expected YYYY-MM-DD, MM/DD/YYYY, or YYYY."))
          }
        }
      }
    }

  } # end row loop

  # ---- 5. Report ---------------------------------------------------------
  passed <- nrow(issues) == 0

  if (verbose) {
    cat("\n========================================\n")
    cat("  Eco-Efficacy Data Harmonization Check\n")
    cat("========================================\n")
    cat(sprintf("  Data rows checked : %d\n", nrow(raw)))
    cat(sprintf("  Issues found      : %d\n", nrow(issues)))
    cat(sprintf("  Overall result    : %s\n",
                if (passed) "PASS \u2713" else "FAIL \u2717"))
    cat("========================================\n\n")

    if (!passed) {
      cat("Issues:\n")
      for (j in seq_len(nrow(issues))) {
        row_label <- if (is.na(issues$row[j])) "FILE" else
                       paste0("Row ", issues$row[j])
        cat(sprintf("  [%s | %s] %s\n",
                    row_label, issues$column[j], issues$message[j]))
      }
      cat("\n")
    }
  }

  invisible(list(data = raw, issues = issues, passed = passed))
}


# =============================================================================
# Example usage
# =============================================================================

# result <- harmonize_eco_data("eco-efficacy_data-log_-_detailed_template.csv")
#
# # Inspect issues as a data frame
# View(result$issues)
#
# # Access the cleaned data
# View(result$data)
