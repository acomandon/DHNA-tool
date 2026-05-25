# R/validate.R — lightweight validation checkpoints for the data pipeline.
#
# Sourced by 00_run_all.R; each stage script calls these helpers in a short
# "# Validation" block at its end. The goal is to make a data refresh (Goal #2)
# fail loudly instead of silently producing wrong numbers.
#
# Severity is tiered:
#   "error" — structural breakage (empty data, weights far from 1, an all-NA
#             rank column). Aborts the run via stop().
#   "warn"  — soft anomaly (match rate a bit low, some NAs). Prints a warning
#             and continues.
#
# Every check prints a one-line result (cat, so it appears inline during a
# sourced run); failures additionally stop() or warning().

# Core reporter -------------------------------------------------------------
dhna_check <- function(pass, label, detail = "", severity = c("error", "warn")) {
  severity <- match.arg(severity)
  tag <- if (isTRUE(pass)) "PASS" else toupper(severity)
  line <- sprintf("  [%s] %s%s", tag, label,
                  if (nzchar(detail)) paste0(" — ", detail) else "")
  cat(line, "\n", sep = "")
  if (!isTRUE(pass)) {
    if (severity == "error") stop(line, call. = FALSE) else warning(line, call. = FALSE)
  }
  invisible(isTRUE(pass))
}

validation_banner <- function(stage) {
  cat(sprintf("\n== Validation: %s ==\n", stage))
}

# Checks --------------------------------------------------------------------

# Crosswalk weights in `wt_col` should sum to ~1 within each `by_col` group
# (each source unit allocates its quantity across destinations).
check_weights_sum_to_one <- function(xwalk, by_col, wt_col, tol = 0.01,
                                     severity = "error") {
  dev <- xwalk %>%
    dplyr::group_by(.data[[by_col]]) %>%
    dplyr::summarise(s = sum(.data[[wt_col]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::summarise(max_dev = max(abs(s - 1), na.rm = TRUE)) %>%
    dplyr::pull(max_dev)
  dhna_check(dev <= tol,
             sprintf("crosswalk %s sums to 1 per %s", wt_col, by_col),
             sprintf("max deviation %.4f (tol %.2f)", dev, tol),
             severity)
}

# Data frame has at least `min_rows` rows.
check_min_rows <- function(df, name, min_rows, severity = "error") {
  n <- nrow(df)
  dhna_check(n >= min_rows, sprintf("%s row count", name),
             sprintf("%d rows (min %d)", n, min_rows), severity)
}

# Row count exactly matches a reference (e.g. a left join did not fan out).
check_rows_equal <- function(df, name, expected, expected_name, severity = "error") {
  n <- nrow(df)
  dhna_check(n == expected,
             sprintf("%s row count matches %s", name, expected_name),
             sprintf("%d vs %d", n, expected), severity)
}

# `actual` of `expected` keys covered, at least `min_frac`.
check_coverage <- function(actual, expected, label, min_frac = 0.95,
                           severity = "warn") {
  frac <- if (expected > 0) actual / expected else 0
  dhna_check(frac >= min_frac, label,
             sprintf("%d of %d (%.1f%%, min %.0f%%)",
                     actual, expected, frac * 100, min_frac * 100),
             severity)
}

# Share of non-NA values in `col` is at least `min_frac`.
check_na_share <- function(df, col, min_frac = 0.5, severity = "warn") {
  frac <- mean(!is.na(df[[col]]))
  dhna_check(frac >= min_frac, sprintf("%s non-NA share", col),
             sprintf("%.1f%% non-NA (min %.0f%%)", frac * 100, min_frac * 100),
             severity)
}

# All non-NA values of numeric `col` lie within [lo, hi].
check_range <- function(df, col, lo, hi, severity = "warn") {
  vals <- df[[col]][!is.na(df[[col]])]
  ok <- length(vals) == 0 || (min(vals) >= lo & max(vals) <= hi)
  detail <- if (length(vals)) sprintf("observed [%.0f, %.0f]", min(vals), max(vals)) else "no values"
  dhna_check(ok, sprintf("%s within [%.0f, %.0f]", col, lo, hi), detail, severity)
}

# Column is not entirely NA.
check_not_all_na <- function(df, col, severity = "error") {
  ok <- any(!is.na(df[[col]]))
  dhna_check(ok, sprintf("%s not all-NA", col),
             if (ok) "has values" else "ALL NA", severity)
}

# `col` contains every value in `required`.
check_values_present <- function(df, col, required, severity = "error") {
  missing <- setdiff(required, unique(df[[col]]))
  dhna_check(length(missing) == 0,
             sprintf("%s contains {%s}", col, paste(required, collapse = ", ")),
             if (length(missing)) sprintf("missing %s", paste(missing, collapse = ", ")) else "all present",
             severity)
}
