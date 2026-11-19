# =============================================================================
# Server piece: running, the Results tab and the downloads
# =============================================================================

#' Wire the Run Button, the Results Tab and the Downloads
#'
#' @param ctx A list: reactives `step_states`, `spec`, `dataset`,
#'   `output_folder`, `sheets`; reactiveVals `running`, `analysis_results`,
#'   `run_error`, `run_log`, `saved_path`; and `mode` (the destination mode).
ak_run_server <- function(input, output, session, ctx) {
  mode <- ctx$mode

  output$run_control <- shiny::renderUI({
    ready <- ak_can_run(ctx$step_states()) && !ctx$running()
    button <- shiny::actionButton(
      "run",
      if (ctx$running()) "Running..." else "Run analyses",
      class = "btn-primary"
    )
    if (ready) button else shiny::tagAppendAttributes(button, disabled = NA)
  })

  output$run_hint <- shiny::renderUI({
    states <- ctx$step_states()
    if (identical(unname(states[["destination"]]), "active")) {
      return(ak_status("Choose an output folder to enable the run."))
    }
    if (!ak_can_run(states)) {
      return(ak_status("Upload a dataset, set up the analysis and clear any problems to run."))
    }
    shiny::tagList()
  })

  # observeEvent: an edit or an upload must never silently rerun an expensive
  # analysis, or silently write a file.
  shiny::observeEvent(input$run, {
    # The button is a convenience, not the guard: a folder that vanished is
    # caught below, and a setup with a fatal problem is refused by
    # run_analysis_spec() itself.
    spec <- ctx$spec()
    dataset <- ctx$dataset()$data
    dataset_name <- ctx$dataset()$filename
    folder <- ctx$output_folder()

    ctx$analysis_results(NULL)
    ctx$run_error(NULL)
    ctx$saved_path(NULL)
    ctx$run_log(character(0))
    ctx$running(TRUE)
    on.exit(ctx$running(FALSE), add = TRUE)

    folder_issue <- ak_check_folder(folder)
    if (!is.null(folder_issue)) {
      ctx$run_error(folder_issue)
      return(invisible(NULL))
    }

    shiny::withProgress(message = "Running analyses", value = 0, {
      shiny::setProgress(0.03, detail = sprintf(
        "%d analyses across %d grouping variable(s)",
        nrow(spec$loa), length(spec$group_variables)
      ))

      # The pipeline reports progress through message(); each message closes a
      # fraction of the remaining distance, the log is flushed once per phase,
      # and the bar moves at most five times a second.
      step <- 0L
      lines <- character(0)
      last_tick <- Sys.time()

      note <- function(text) {
        text <- trimws(sub("^-->\\s*", "", text))
        if (nzchar(text)) lines[[length(lines) + 1L]] <<- text
        text
      }
      flush_log <- function() {
        if (length(lines) > 0) {
          ctx$run_log(c(ctx$run_log(), lines))
          lines <<- character(0)
        }
      }
      # force(): a throttled tick never reaches setProgress(), and left as a
      # promise the log entry would never be recorded.
      tick <- function(text) {
        force(text)
        step <<- step + 1L
        now <- Sys.time()
        if (as.numeric(difftime(now, last_tick, units = "secs")) < 0.2) return(invisible(NULL))
        last_tick <<- now
        shiny::setProgress(0.03 + 0.72 * (1 - 0.88^step), detail = text)
      }
      capture <- function(expr, on_message) {
        withCallingHandlers(
          tryCatch(expr, error = function(error) {
            structure(list(message = conditionMessage(error)), class = "run_error")
          }),
          message = function(m) {
            on_message(conditionMessage(m))
            invokeRestart("muffleMessage")
          },
          warning = function(w) {
            note(paste0("Warning: ", conditionMessage(w)))
            invokeRestart("muffleWarning")
          }
        )
      }

      result <- capture(run_analysis_spec(dataset, spec, verbose = TRUE),
                        function(text) tick(note(text)))
      flush_log()

      if (inherits(result, "run_error")) {
        ctx$run_error(result$message)
        return(invisible(NULL))
      }

      shiny::setProgress(0.78, detail = "building the results workbook")
      if (isTRUE(spec$settings$include_dataset)) {
        note("The dataset will be added to the workbook as a 'dataset' sheet")
        flush_log()
      }

      written <- capture(
        ak_export_results(
          results = result, spec = spec, folder = folder,
          dataset_name = dataset_name, dataset = dataset, verbose = TRUE
        ),
        note
      )
      flush_log()

      if (inherits(written, "run_error")) {
        # The analysis succeeded; only the saving failed. Keep the results.
        ctx$analysis_results(result)
        ctx$run_error(paste0(
          "The analysis finished, but the results could not be saved: ",
          written$message
        ))
        return(invisible(NULL))
      }

      shiny::setProgress(1, detail = basename(written))
      ctx$analysis_results(result)
      ctx$saved_path(written)
      note(paste0("Saved to ", written))
      flush_log()
    })
  })

  output$resultsReady <- shiny::reactive(!is.null(ctx$analysis_results()))
  shiny::outputOptions(output, "resultsReady", suspendWhenHidden = FALSE)

  output$results_status <- shiny::renderUI({
    if (!is.null(ctx$run_error())) return(ak_status(ctx$run_error(), "error"))
    if (ctx$running()) return(ak_status("Running the analyses..."))
    if (is.null(ctx$analysis_results())) {
      if (!ak_can_run(ctx$step_states())) {
        return(ak_status(
          "Upload a dataset, set up the analysis, clear any problems and choose an output folder, then click Run analyses."
        ))
      }
      return(ak_status("Ready. Click Run analyses to produce the results."))
    }
    results <- ctx$analysis_results()
    ak_status(
      sprintf(
        if (mode$pick_folder) {
          "Produced %d rows and %d columns. Saved as %s in %s."
        } else {
          "Produced %d rows and %d columns. Download %s using the button above.%s"
        },
        nrow(results$combined_results), ncol(results$combined_results),
        basename(ctx$saved_path()),
        if (mode$pick_folder) dirname(ctx$saved_path()) else ""
      ),
      "success"
    )
  })

  output$results_summary <- ak_table({
    results <- ctx$analysis_results()
    shiny::req(results)
    saved <- ctx$saved_path()
    data.frame(
      Measure = c(
        "Saved as", if (mode$pick_folder) "Folder" else "Delivery",
        "Result rows", "Result columns", "Grouping variables",
        "Selection counts", "Choice combinations", "Exclusive combinations",
        "Excluded choices"
      ),
      Value = c(
        if (is.null(saved)) "not saved" else basename(saved),
        if (is.null(saved)) "-" else if (mode$pick_folder) dirname(saved) else "Download using the button above",
        format(nrow(results$combined_results), big.mark = ","),
        format(ncol(results$combined_results), big.mark = ","),
        format(length(unique(stats::na.omit(results$column_map$group_variable)))),
        format(nrow(results$selection_counts)),
        format(nrow(results$choice_combinations)),
        format(nrow(results$exclusive_combinations %||% data.frame())),
        format(nrow(results$excluded_choices))
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })

  output$results_preview <- ak_table(
    {
      results <- ctx$analysis_results()
      shiny::req(results)
      # Previewed as exported, separator rows and all; at most 12 columns, as a
      # wide table can run to thousands.
      wide <- ak_prepare_for_export(
        results$combined_results,
        ak_export_settings(results, ctx$spec())$layout
      )
      utils::head(wide[, seq_len(min(ncol(wide), 12L)), drop = FALSE], 10L)
    },
    spacing = "xs"
  )

  # Above the summary: these percentages get published, and the caveat has to
  # be in front of whoever exports the file.
  output$exclusive_note <- shiny::renderUI({
    results <- ctx$analysis_results()
    if (is.null(results)) return(shiny::tagList())
    ak_exclusive_base_panel(results$choice_combinations, results$exclusive_combinations)
  })

  output$run_log <- shiny::renderUI({
    entries <- ctx$run_log()
    if (length(entries) == 0) return(ak_status("The pipeline reported nothing."))
    shiny::tags$ul(
      class = "ak-log",
      lapply(entries, function(line) {
        shiny::tags$li(class = if (startsWith(line, "Warning:")) "ak-log-warning" else NULL, line)
      })
    )
  })

  # -- downloads ---------------------------------------------------------------
  output$template <- shiny::downloadHandler(
    filename = "analysiskit_loa_template.xlsx",
    content = function(file) {
      # Shipped with the package, so it always matches the code that runs.
      template <- system.file("extdata", "loa_template.xlsx", package = "analysiskit")
      shiny::validate(shiny::need(
        nzchar(template) && file.exists(template),
        "The template is missing from the analysiskit installation."
      ))
      file.copy(template, file, overwrite = TRUE)
    }
  )

  output$setup_download <- shiny::downloadHandler(
    filename = function() {
      paste0("analysiskit_setup_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".xlsx")
    },
    content = function(file) ak_write_setup(ctx$sheets(), file)
  )

  output$results_file <- shiny::downloadHandler(
    filename = function() basename(ctx$saved_path()),
    content = function(file) file.copy(ctx$saved_path(), file, overwrite = TRUE)
  )

  output$download_control <- shiny::renderUI({
    if (is.null(ctx$saved_path())) return(shiny::tagList())
    shiny::tags$div(
      class = "ak-download",
      shiny::downloadButton("results_file", "Download the results workbook",
                            class = "btn-primary")
    )
  })
}
