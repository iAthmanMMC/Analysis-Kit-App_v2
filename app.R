# Analysis Kit - Mixed Migration Centre
#
# app.R assembles the interface and connects the pieces; nothing else.
#
# The analysis is set up on screen: the builder tabs edit the same sheets the
# List of Analysis template has (R/setup_state.R), and the analysiskit package
# reads, checks, runs and exports them exactly as it would an uploaded
# workbook. A workbook can still be imported to fill the tabs, and the setup
# can be downloaded as one to keep or reuse.
#
#   R/setup_state.R        the sheets, the dataset dictionary, settings (pure)
#   R/setup_store.R        the reactive store the tabs share, table helpers
#   R/mod_*.R              one module per builder tab
#   R/server_inputs.R      dataset, import, destination folder
#   R/server_checks.R      the Checks tab
#   R/server_run.R         the run, the Results tab and the downloads
#   R/ui_components.R      tracker, badges, status messages

# Packages first: a missing one should be a clear message on launch, not an
# error halfway through a run. See R/setup_packages.R.
source("R/setup_packages.R")
source("R/deployment.R")

# Nothing is installed on a served deployment: its library is built from
# manifest.json and is read-only once running.
ak_ensure_packages(install = !ak_is_server())
ak_attach_packages()

options(shiny.maxRequestSize = 100 * 1024^2)

for (file in c(
  "R/ui_components.R", "R/setup_state.R", "R/setup_store.R",
  "R/mod_analysis_vars.R", "R/mod_disaggregation.R", "R/mod_route.R",
  "R/server_inputs.R", "R/server_checks.R", "R/server_run.R"
)) {
  source(file)
}


ui <- fluidPage(
  includeCSS("www/styles.css"),
  tags$header(
    class = "app-header",
    titlePanel("Analysis Kit App"),
    tags$img(src = "logo.png", alt = "Mixed Migration Centre", class = "app-logo")
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput("dataset", "Upload dataset", accept = c(".csv", ".xlsx")),
      tags$div(
        class = "ak-import",
        fileInput(
          "loa", "Import a List of Analysis (optional)",
          accept = c(".csv", ".xlsx"),
          placeholder = "Fills the setup tabs"
        ),
        tags$div(
          class = "ak-template",
          downloadLink("setup_download", "Download this setup"),
          tags$span(class = "ak-sep", "·"),
          downloadLink("template", "Blank template"),
          tags$span(class = "ak-sep", "·"),
          actionLink("start_over", "Start over")
        )
      ),
      tags$div(
        class = "ak-destination",
        uiOutput("folder_label"),
        uiOutput("folder_control"),
        uiOutput("folder_status")
      ),
      uiOutput("run_control"),
      uiOutput("run_hint")
    ),
    mainPanel(
      width = 9,
      uiOutput("progress_tracker"),
      uiOutput("status"),
      tabsetPanel(
        id = "panels",
        type = "tabs",
        tabPanel(
          "Dataset",
          conditionalPanel(
            condition = "output.datasetReady",
            ak_section(
              "Results workbook",
              checkboxInput("include_dataset",
                            "Include the dataset in the results workbook", value = TRUE),
              ak_help(
                "Adds the dataset as a sheet named dataset, after the analysis ",
                "sheets. It is personal data: store and share the workbook accordingly."
              )
            ),
            ak_section("Dataset overview", tableOutput("dataset_overview")),
            ak_section("Column profile",
                       div(class = "table-scroll", tableOutput("dataset_columns"))),
            ak_section("First 10 rows",
                       div(class = "table-scroll", tableOutput("dataset_preview")))
          )
        ),
        tabPanel("Analysis variables", mod_analysis_vars_ui("vars")),
        tabPanel("Disaggregations", mod_disaggregation_ui("groups")),
        tabPanel("Route analysis", mod_route_ui("route")),
        tabPanel(
          "Checks",
          uiOutput("loa_status"),
          conditionalPanel(
            condition = "output.loaReady",
            ak_section(
              "Checks",
              uiOutput("check_badges"),
              div(class = "table-scroll ak-problems", tableOutput("loa_problems"))
            ),
            ak_section("Estimation engine", uiOutput("engine_note")),
            ak_section(
              "Variable coverage",
              uiOutput("coverage_note"),
              div(class = "table-scroll", tableOutput("coverage_table"))
            ),
            ak_section("Sheets in the setup", tableOutput("loa_sheets"))
          )
        ),
        tabPanel(
          "Results",
          uiOutput("results_status"),
          conditionalPanel(
            condition = "output.resultsReady",
            uiOutput("exclusive_note"),
            ak_section("Analyses run", uiOutput("download_control"),
                       tableOutput("results_summary")),
            ak_section("Results preview",
                       div(class = "table-scroll", tableOutput("results_preview"))),
            ak_section("Run log", uiOutput("run_log"))
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  # -- inputs ------------------------------------------------------------------
  dataset_srv <- ak_dataset_server(input, output, session)
  dataset_state <- dataset_srv$state
  uploaded_dataset <- dataset_srv$data
  dataset_ok <- dataset_srv$ok

  setup <- ak_setup_store()
  import_srv <- ak_import_server(input, output, session, setup)

  dest_srv <- ak_destination_server(input, output, session)
  destination_mode <- dest_srv$mode
  session_output_dir <- dest_srv$session_output_dir
  output_folder <- dest_srv$output_folder
  folder_problem <- dest_srv$folder_problem
  # Only where the picker is offered; see ak_destination_server().
  if (!is.null(dest_srv$volumes)) volumes <- dest_srv$volumes

  # -- the setup -----------------------------------------------------------------
  setup_workbook <- reactive(setup$workbook())
  setup_started <- reactive(ak_setup_started(setup$sheets()))

  # "The List of Analysis is in": something is set up and the last import, if
  # any, did not fail.
  loa_ok <- reactive(setup_started() && !import_srv$failed())
  uploaded_loa <- setup_workbook
  output$loaReady <- reactive(loa_ok())
  outputOptions(output, "loaReady", suspendWhenHidden = FALSE)

  # A setting as it stands, typed, with the package default behind it.
  settings_defaults <- ak_settings_defaults()
  settings_schema <- analysiskit::loa_settings_schema()
  settings_value <- function(key) {
    i <- match(key, settings_schema$setting)
    sheet <- setup$sheet(settings_schema$sheet[i])
    ak_setting_value(sheet, key, settings_schema$type[i], settings_defaults[[key]])
  }

  # The two settings the dictionary reads, held in a reactiveVal: it only
  # invalidates when they really change, so editing any other part of the
  # setup does not re-profile a 700-column export or reset the pickers built
  # from it.
  dictionary_params <- reactiveVal(list(sm_separator = "/", skip_label_row = TRUE))
  observe({
    dictionary_params(list(
      sm_separator = settings_value("sm_separator"),
      skip_label_row = settings_value("skip_label_row")
    ))
  })

  dictionary <- reactive({
    if (!dataset_ok()) return(NULL)
    params <- dictionary_params()
    ak_dataset_dictionary(
      uploaded_dataset()$data,
      sm_separator = params$sm_separator,
      skip_label_row = params$skip_label_row
    )
  })
  dataset_columns <- reactive(if (dataset_ok()) names(uploaded_dataset()$data) else character(0))

  # The dataset checkbox is a run option, kept out of the setup sheets so an
  # imported workbook still checks exactly as uploading it would. An import
  # that sets include_dataset moves the checkbox to its value.
  include_dataset <- reactive(isTRUE(input$include_dataset %||% TRUE))
  observeEvent(setup$version(), {
    settings <- setup$sheet("settings")
    if (!is.null(ak_setting_text(settings, "include_dataset"))) {
      updateCheckboxInput(session, "include_dataset",
                          value = ak_setting_value(settings, "include_dataset", "lgl", FALSE))
    }
  }, ignoreInit = TRUE)

  # -- checks --------------------------------------------------------------------
  # No progress bar: validation takes milliseconds and feeds several outputs.
  analysis_spec <- reactive({
    req(dataset_ok(), loa_ok())
    build_analysis_spec(setup_workbook(), uploaded_dataset()$data)
  })
  loa_problems <- reactive(analysis_spec()$problems)
  variable_coverage <- reactive({
    req(dataset_ok(), loa_ok())
    loa_variable_coverage(setup_workbook(), uploaded_dataset()$data)
  })
  tab_problems <- reactive({
    if (!dataset_ok() || !loa_ok()) return(NULL)
    loa_problems()
  })

  ak_checks_server(input, output, session, list(
    dataset_ok = dataset_ok, setup_ok = loa_ok, import_state = import_srv$state,
    workbook = setup_workbook, dataset = uploaded_dataset, spec = analysis_spec,
    problems = loa_problems, coverage = variable_coverage
  ))

  # -- builder tabs ----------------------------------------------------------------
  mod_analysis_vars_server("vars", setup, dictionary, tab_problems)
  mod_disaggregation_server("groups", setup, dictionary, tab_problems)
  mod_route_server("route", setup, tab_problems, dataset_columns, settings_value)

  # -- tracker and status ------------------------------------------------------------
  running <- reactiveVal(FALSE)
  analysis_results <- reactiveVal(NULL)
  run_error <- reactiveVal(NULL)
  run_log <- reactiveVal(character(0))
  saved_path <- reactiveVal(NULL)

  step_states <- reactive({
    ak_step_states(
      dataset_loaded = dataset_ok(),
      dataset_failed = !is.null(input$dataset) && !dataset_ok(),
      loa_loaded = loa_ok(),
      loa_failed = import_srv$failed(),
      problems = if (dataset_ok() && loa_ok()) loa_problems() else NULL,
      destination_chosen = destination_mode$settled ||
        (!is.null(output_folder()) && is.null(folder_problem())),
      destination_failed = !is.null(folder_problem()),
      results_ready = !is.null(analysis_results()),
      running = running()
    )
  })

  output$progress_tracker <- renderUI(
    ak_step_tracker(step_states(), destination_mode$step_label)
  )

  output$status <- renderUI({
    if (is.null(input$dataset)) {
      return(ak_status("Upload a CSV or XLSX dataset to begin."))
    }
    if (!dataset_ok()) return(ak_status(uploaded_dataset()$message, "error"))
    if (import_srv$failed()) return(ak_status(import_srv$state()$message, "error"))
    if (!loa_ok()) {
      return(ak_status(
        sprintf(
          "Loaded %s. Set up the analysis in the tabs below - start with Analysis variables - or import a List of Analysis.",
          uploaded_dataset()$filename
        ),
        "success"
      ))
    }
    if (!is.null(run_error())) return(ak_status(run_error(), "error"))
    if (!is.null(saved_path())) {
      return(ak_status(
        if (destination_mode$pick_folder) {
          sprintf("Analyses complete. Saved as %s in %s.",
                  basename(saved_path()), dirname(saved_path()))
        } else {
          sprintf("Analyses complete. Download %s from the Results tab.",
                  basename(saved_path()))
        },
        "success"
      ))
    }

    counts <- ak_problem_counts(loa_problems())
    if (counts[["error"]] > 0L) {
      return(ak_status(sprintf(
        "The setup has %d problem(s) that must be fixed before running. See the Checks tab.",
        counts[["error"]]
      ), "error"))
    }
    if (counts[["warning"]] > 0L) {
      return(ak_status(sprintf(
        "Ready to run, with %d warning(s) worth reading first. See the Checks tab.",
        counts[["warning"]]
      ), "warning"))
    }
    ak_status("Every check passed. Ready to run the analyses.", "success")
  })

  # -- run, results, downloads --------------------------------------------------------
  ak_run_server(input, output, session, list(
    step_states = step_states, spec = analysis_spec, dataset = uploaded_dataset,
    output_folder = output_folder, sheets = setup$sheets, mode = destination_mode,
    include_dataset = include_dataset,
    running = running, analysis_results = analysis_results, run_error = run_error,
    run_log = run_log, saved_path = saved_path
  ))
}

shinyApp(ui, server)
