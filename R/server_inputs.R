# =============================================================================
# Server pieces: the dataset, the optional List of Analysis import, and the
# destination folder
# =============================================================================
#
# These are plain functions over the app's own input/output, not modules: the
# reactives they return are assigned to top-level names in app.R's server, so
# the outputs keep their ids and the tests keep reaching them by name.
# =============================================================================


#' Read the Dataset Upload and Render the Dataset Tab
#' @return A list of reactives: `state`, `data`, `ok`.
ak_dataset_server <- function(input, output, session) {
  # Read once, when the file arrives: an observer rather than a reactive, so a
  # 100 MB upload is not re-read on every invalidation.
  state <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$dataset, {
    shiny::withProgress(message = "Reading the dataset", value = 0, {
      shiny::setProgress(0.3, detail = input$dataset$name)
      result <- tryCatch(
        read_uploaded_dataset(input$dataset),
        error = function(error) {
          structure(list(message = conditionMessage(error)), class = "dataset_error")
        }
      )
      shiny::setProgress(0.9, detail = "profiling the columns")
      state(result)
      shiny::setProgress(1, detail = "done")
    })
  })

  data <- shiny::reactive({
    shiny::req(state())
    state()
  })

  ok <- shiny::reactive({
    !is.null(state()) && !inherits(state(), "dataset_error")
  })

  output$datasetReady <- shiny::reactive(ok())
  shiny::outputOptions(output, "datasetReady", suspendWhenHidden = FALSE)

  output$dataset_overview <- ak_table({
    result <- data()
    shiny::validate(shiny::need(!inherits(result, "dataset_error"), result$message))
    dataset_overview(result)
  })

  output$dataset_columns <- ak_table({
    result <- data()
    shiny::validate(shiny::need(!inherits(result, "dataset_error"), result$message))
    dataset_columns(result)
  })

  output$dataset_preview <- ak_table(
    {
      result <- data()
      shiny::validate(shiny::need(!inherits(result, "dataset_error"), result$message))
      utils::head(result$data, 10L)
    },
    spacing = "xs",
    rownames = TRUE
  )

  list(state = state, data = data, ok = ok)
}


#' Import a List of Analysis Into the Builder
#'
#' The upload no longer drives the run directly: it fills the builder tabs,
#' where it can be checked and changed. Left untouched, it produces exactly the
#' specification uploading it always did.
#'
#' @return A list: `state` (the read workbook, a `loa_error`, or `NULL`),
#'   `failed` (reactive).
ak_import_server <- function(input, output, session, store) {
  state <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$loa, {
    shiny::withProgress(message = "Importing the List of Analysis", value = 0, {
      shiny::setProgress(0.3, detail = input$loa$name)
      result <- tryCatch(
        read_loa_workbook(input$loa$datapath, filename = input$loa$name),
        error = function(error) {
          structure(list(message = conditionMessage(error)), class = "loa_error")
        }
      )
      state(result)
      if (!inherits(result, "loa_error")) {
        store$load(result$sheets, list(
          filename = result$filename,
          format = result$format,
          unknown_sheets = result$unknown_sheets,
          ignored_sheets = result$ignored_sheets,
          duplicate_sheets = result$duplicate_sheets
        ))
        shiny::showNotification(
          sprintf("Imported %s. Its setup is now in the tabs, ready to check or change.",
                  result$filename),
          type = "message"
        )
      }
      shiny::setProgress(1, detail = "done")
    })
  })

  shiny::observeEvent(input$start_over, {
    shiny::showModal(shiny::modalDialog(
      title = "Start the setup again?",
      "Every analysis, disaggregation and setting will be cleared. The dataset stays loaded.",
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("start_over_confirm", "Clear the setup", class = "btn-primary")
      )
    ))
  })
  shiny::observeEvent(input$start_over_confirm, {
    store$clear()
    state(NULL)
    shiny::removeModal()
  })

  list(
    state = state,
    failed = shiny::reactive(inherits(state(), "loa_error"))
  )
}


#' The Output Folder, or the Download When Served
#'
#' Unchanged from the upload-only app; see docs/deployment.md.
#' @return A list: `mode`, `volumes` (only when the picker is offered),
#'   `session_output_dir`, `output_folder`, `folder_problem`.
ak_destination_server <- function(input, output, session) {
  mode <- ak_destination_mode()

  # Registered only where the picker is offered: served, shinyDirChoose() would
  # answer directory listings of the container's filesystem.
  volumes <- NULL
  if (mode$pick_folder) {
    volumes <- c(Home = path.expand("~"), shinyFiles::getVolumes()())
    shinyFiles::shinyDirChoose(input, "folder", roots = volumes, session = session)
  }

  # Served, a folder of this session's own, removed when the session ends.
  session_output_dir <- if (mode$pick_folder) {
    NULL
  } else {
    dir <- ak_session_output_dir(session$token)
    session$onSessionEnded(function() unlink(dir, recursive = TRUE, force = TRUE))
    dir
  }

  output$folder_label <- shiny::renderUI({
    shiny::tags$label(class = "control-label", mode$label)
  })

  output$folder_control <- shiny::renderUI({
    if (!mode$pick_folder) return(shiny::tagList())
    shinyFiles::shinyDirButton(
      "folder",
      label = "Choose folder...",
      title = "Select the folder to save the results workbook into",
      class = "btn-default"
    )
  })

  output_folder <- shiny::reactive({
    if (!mode$pick_folder) return(session_output_dir)
    chosen <- shinyFiles::parseDirPath(volumes, input$folder)
    if (length(chosen) == 0 || !nzchar(chosen)) NULL else as.character(chosen)
  })

  folder_problem <- shiny::reactive({
    if (is.null(output_folder())) NULL else ak_check_folder(output_folder())
  })

  output$folder_status <- shiny::renderUI({
    if (!mode$pick_folder || is.null(output_folder())) {
      return(ak_status(mode$explanation, mode$status_type))
    }
    if (!is.null(folder_problem())) return(ak_status(folder_problem(), "error"))
    ak_status(paste0("Saving to ", output_folder()), "success")
  })

  list(
    mode = mode,
    volumes = volumes,
    session_output_dir = session_output_dir,
    output_folder = output_folder,
    folder_problem = folder_problem
  )
}


#' renderTable in the House Style
#'
#' substitute() + quoted = TRUE, not renderTable(expr): passing the expression
#' as an ordinary argument turns it into a promise, and a req() inside it then
#' leaves the promise interrupted ("restarting interrupted promise evaluation").
ak_table <- function(expr, spacing = "s", rownames = FALSE) {
  shiny::renderTable(
    substitute(expr),
    striped = TRUE,
    bordered = FALSE,
    spacing = spacing,
    rownames = rownames,
    env = parent.frame(),
    quoted = TRUE
  )
}
