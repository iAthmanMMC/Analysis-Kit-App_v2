# =============================================================================
# Server piece: the Checks tab
# =============================================================================
#
# Validation is cheap and reactive: every change in the builder, and every new
# dataset, reruns it, so problems show while the setup is being made rather
# than after a run. The analysis itself still waits for the button.
# =============================================================================

#' Render the Checks Tab
#'
#' @param ctx A list of reactives: `dataset_ok`, `setup_ok`, `import_state`,
#'   `workbook`, `dataset`, `spec`, `problems`, `coverage`.
ak_checks_server <- function(input, output, session, ctx) {
  output$loa_status <- shiny::renderUI({
    imported <- ctx$import_state()
    if (inherits(imported, "loa_error")) return(ak_status(imported$message, "error"))
    if (!ctx$setup_ok()) {
      return(ak_status(
        "Nothing is set up yet. Add analyses on the Analysis variables tab, or import a List of Analysis."
      ))
    }
    if (!ctx$dataset_ok()) {
      return(ak_status(
        "Upload a dataset as well: the variable checks compare the setup with it.",
        "warning"
      ))
    }
    wb <- ctx$workbook()
    ak_status(sprintf("Checking: %s.", wb$filename), "success")
  })

  output$loa_sheets <- ak_table({
    shiny::req(ctx$setup_ok())
    ak_sheet_summary(ctx$workbook())
  })

  output$check_badges <- shiny::renderUI({
    shiny::req(ctx$dataset_ok(), ctx$setup_ok())
    ak_badges(ak_problem_counts(ctx$problems()))
  })

  output$loa_problems <- ak_table({
    shiny::req(ctx$dataset_ok(), ctx$setup_ok())
    display <- ak_problems_display(ctx$problems())
    shiny::validate(shiny::need(nrow(display) > 0, "Nothing to fix."))
    display
  })

  # Shown before the run: rows whose confidence interval is reported go
  # through the survey package, whose cost grows with the disaggregation.
  output$engine_note <- shiny::renderUI({
    shiny::req(ctx$dataset_ok(), ctx$setup_ok())
    split <- loa_engine_split(ctx$spec())
    if (split$survey == 0) {
      return(ak_status(
        "No analysis needs a confidence interval calculated, so the whole run uses the fast tabulation engine.",
        "success"
      ))
    }
    ak_engine_note(split)
  })

  output$coverage_note <- shiny::renderUI({
    shiny::req(ctx$dataset_ok(), ctx$setup_ok())
    coverage <- ctx$coverage()
    name <- ctx$dataset()$filename
    if (nrow(coverage) == 0) return(ak_status("The setup does not name any dataset variables."))

    missing <- sum(!coverage$present)
    total <- length(unique(coverage$variable))
    if (missing == 0) {
      return(ak_status(
        sprintf("All %d variable(s) named in the setup are in %s.", total, name),
        "success"
      ))
    }
    ak_status(
      sprintf(
        "%d of %d variable(s) named in the setup are not in %s.",
        length(unique(coverage$variable[!coverage$present])), total, name
      ),
      if (any(loa_coverage_severity(coverage$role[!coverage$present]) == "error")) {
        "error"
      } else {
        "warning"
      }
    )
  })

  output$coverage_table <- ak_table({
    shiny::req(ctx$dataset_ok(), ctx$setup_ok())
    summary <- loa_coverage_summary(ctx$coverage())
    shiny::validate(shiny::need(nrow(summary) > 0, "No variables referenced."))
    summary
  })
}
