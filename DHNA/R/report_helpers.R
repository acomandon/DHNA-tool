# PDF report rendering for the Affordability panel.
#
# render_assessment_report(params, pdf_path)
#   - Renders DHNA/report/assessment_report.Rmd to a self-contained HTML in
#     a temp dir, then prints it to PDF with headless Chrome via pagedown.
#   - Returns invisible(pdf_path).
#
# Requirements: rmarkdown, pagedown, chromote (and a Chrome/Chromium binary
# discoverable by chromote::find_chrome()).

render_assessment_report <- function(params, pdf_path) {
  rmd_path <- here::here("DHNA", "report", "assessment_report.Rmd")
  if (!file.exists(rmd_path)) {
    stop("Report template not found at ", rmd_path)
  }

  # Use a fresh intermediates_dir so concurrent users don't clobber each other,
  # and so the rmd doesn't try to write next to the source file.
  work_dir <- tempfile("dhna_report_")
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  html_path <- file.path(work_dir, "assessment_report.html")

  rmarkdown::render(
    input             = rmd_path,
    output_file       = html_path,
    output_dir        = work_dir,
    intermediates_dir = work_dir,
    knit_root_dir     = work_dir,
    params            = params,
    envir             = new.env(parent = globalenv()),
    quiet             = TRUE
  )

  pagedown::chrome_print(
    input    = html_path,
    output   = pdf_path,
    verbose  = 0,
    timeout  = 120
  )

  invisible(pdf_path)
}

# Build the params list consumed by the report template.
# Accepts the values you'd otherwise pass to the value-box outputs.
build_report_params <- function(tenure,
                                location_label,
                                bg_id,
                                proj_size,
                                inputs,
                                context,
                                messages) {
  list(
    tenure         = tenure,
    location_label = location_label %||% "",
    bg_id          = as.character(bg_id %||% ""),
    proj_size      = as.integer(proj_size %||% 0L),
    inputs         = inputs %||% list(),
    context        = context %||% list(),
    messages       = as.character(messages %||% character(0)),
    tool_name      = "DHNA — Displacement and Housing Need Assessment",
    generated_at   = format(Sys.time(), "%Y-%m-%d %H:%M %Z")
  )
}

# Null-coalescing helper (avoid a hard dep on rlang for the app side).
`%||%` <- function(a, b) if (is.null(a)) b else a
