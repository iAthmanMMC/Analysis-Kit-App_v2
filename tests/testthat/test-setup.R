# The analysis setup made on screen: the helpers in R/setup_state.R and
# R/setup_store.R, and the server wiring that runs a setup built without any
# uploaded List of Analysis.

# -- the dataset dictionary ----------------------------------------------------

test_that("the dictionary lists each question once, with its label and type", {
  d <- ak_dataset_dictionary(fixture_dataset(10))

  expect_equal(d$variable, c("Q27", "Q31", "Q29", "Q78"))
  expect_equal(d$type, c("select_one", "select_one", "numeric", "select_multiple"))
  expect_equal(d$label[d$variable == "Q27"], "What is your gender?")
})

test_that("a select_multiple's choices are its child column suffixes", {
  d <- ak_dataset_dictionary(fixture_dataset(10))
  expect_equal(
    d$choices[[match("Q78", d$variable)]],
    c("Economic reasons",
      "Armed conflict, generalised violence, and insecurity",
      "Lack of services")
  )
})

test_that("a select_multiple is found even when the export has no parent column", {
  data <- fixture_dataset(6)
  data$Q78 <- NULL
  d <- ak_dataset_dictionary(data)
  expect_equal(d$type[d$variable == "Q78"], "select_multiple")
  expect_true(is.na(d$label[d$variable == "Q78"]))
})

test_that("the label row is not counted as an answer", {
  d <- ak_dataset_dictionary(fixture_dataset(4))
  expect_false("What is your gender?" %in% d$choices[[match("Q27", d$variable)]])
})

test_that("analysis types are suggested from question types", {
  expect_equal(
    ak_suggest_analysis_type(c("select_one", "select_multiple", "numeric", "text")),
    c("prop_select_one", "prop_select_multiple", "mean", NA)
  )
})

# -- settings ------------------------------------------------------------------

test_that("every setting in the package schema has a default entry", {
  defaults <- ak_settings_defaults()
  schema <- analysiskit::loa_settings_schema()
  expect_setequal(names(defaults), schema$setting)
  expect_equal(defaults$sm_separator, "/")
  expect_equal(defaults$engine, "auto")
  expect_equal(defaults$value_columns, c("stat", "n", "n_total"))
  expect_equal(defaults$route_top_countries, 10)
  expect_null(defaults$weight_column)
})

test_that("a setting equal to its default is not written", {
  sheet <- ak_set_setting(NULL, "engine", "fast", "enum", "auto")
  expect_equal(sheet$value, "fast")
  sheet <- ak_set_setting(sheet, "engine", "auto", "enum", "auto")
  expect_equal(nrow(sheet), 0L)
})

test_that("edge spaces survive the round trip through the sheet", {
  sheet <- ak_set_setting(NULL, "count_combinations_joiner", " & ", "chr", " + ")
  expect_equal(sheet$value, "\" & \"")
  expect_equal(ak_setting_text(sheet, "count_combinations_joiner"), " & ")
})

test_that("lists and logicals are written the way the reader parses them", {
  sheet <- ak_set_setting(NULL, "value_columns", c("stat", "n", "n_total", "n_w"),
                          "chr[]", c("stat", "n", "n_total"))
  sheet <- ak_set_setting(sheet, "include_dataset", TRUE, "lgl", FALSE)
  parsed <- loa_parse_settings(sheet)
  expect_false(loa_has_errors(parsed$problems))
  expect_equal(parsed$settings$value_columns, c("stat", "n", "n_total", "n_w"))
  expect_true(parsed$settings$include_dataset)
})

test_that("each settings key falls into a group", {
  groups <- ak_setting_group(analysiskit::loa_settings_schema()$setting)
  expect_true(all(nzchar(groups)))
  expect_equal(ak_setting_group("weight_column"), "Data and design")
  expect_equal(ak_setting_group("count_exclusive_combinations_heading"), "Single choices")
})

# -- the workbook the package reads ----------------------------------------------

test_that("an imported workbook, untouched, gives the spec uploading it gives", {
  wb <- read_loa_workbook(write_fixture_xlsx(fixture_sheets(), tempfile(fileext = ".xlsx")))
  ds <- fixture_dataset(40)

  direct <- build_analysis_spec(wb, ds)
  via_builder <- build_analysis_spec(
    ak_setup_workbook(ak_normalise_sheets(wb$sheets), list(filename = wb$filename, format = "XLSX")),
    ds
  )
  expect_identical(via_builder, direct)
})

test_that("a downloaded setup reads back into the same spec", {
  wb <- read_loa_workbook(write_fixture_xlsx(fixture_sheets(), tempfile(fileext = ".xlsx")))
  ds <- fixture_dataset(40)
  sheets <- ak_normalise_sheets(wb$sheets)

  path <- ak_write_setup(sheets, tempfile(fileext = ".xlsx"))
  back <- read_loa_workbook(path)

  a <- build_analysis_spec(wb, ds)
  b <- build_analysis_spec(back, ds)
  a$source <- b$source <- NULL
  expect_equal(b, a)
  expect_true("readme" %in% back$ignored_sheets)
})

test_that("untouched tabs leave their sheets out, so defaults apply", {
  sheets <- ak_empty_sheets()
  sheets$analysis_variables <- data.frame(
    analysis_type = "prop_select_one", analysis_var = "Q27", stringsAsFactors = FALSE
  )
  wb <- ak_setup_workbook(sheets)
  expect_equal(names(wb$sheets), "analysis_variables")
  expect_equal(wb$filename, "Set up in the app")
})

test_that("nothing set up means nothing started", {
  expect_false(ak_setup_started(ak_empty_sheets()))
  sheets <- ak_empty_sheets()
  sheets$settings <- ak_set_setting(NULL, "engine", "fast", "enum", "auto")
  expect_true(ak_setup_started(sheets))
})

# -- table helpers -------------------------------------------------------------------

test_that("rows move up and down and keep their selection", {
  df <- data.frame(x = c("a", "b", "c", "d"), stringsAsFactors = FALSE)
  up <- ak_move_rows(df, c(2, 3), -1)
  expect_equal(up$df$x, c("b", "c", "a", "d"))
  expect_equal(up$rows, c(1, 2))
  down <- ak_move_rows(df, 4, 1)
  expect_equal(down$df$x, df$x)
})

test_that("a cell edit lands in the right cell and blanks become missing", {
  df <- data.frame(a = c("x", "y"), b = c("1", "2"), level = c(NA, 0.95),
                   stringsAsFactors = FALSE)
  info <- data.frame(row = c(1, 2, 2), col = c(1, 0, 2), value = c("9", "", "95"))
  out <- ak_apply_cell_edit(df, info)
  expect_equal(out$b[1], "9")
  expect_true(is.na(out$a[2]))
  expect_equal(out$level[2], 95)
})

test_that("route coverage names the questions a block is missing", {
  cov <- ak_route_coverage(c("Q47", "Q48", "Q72", "QN8/Work"))
  transit <- cov[cov$block == "transit_country", ]
  expect_equal(transit$found, 4L)
  expect_match(transit$missing, "Q49")
})

# -- the server, with nothing uploaded but the dataset --------------------------

test_that("an analysis set up on screen is checked and runs without a workbook", {
  skip_without_app()
  destination_dir <- temp_destination()

  shiny::testServer(app_dir, {
    session$setInputs(dataset = upload(dataset_file(fixture_dataset(40))))
    expect_equal(step_states()[["loa"]], "active")
    expect_false(loa_ok())

    setup$set("analysis_variables", data.frame(
      analysis_type = c("prop_select_one", "prop_select_multiple", "mean"),
      analysis_var = c("Q27", "Q78", "Q29"),
      stringsAsFactors = FALSE
    ))
    setup$set("disaggregation_variables", data.frame(
      raw_data_name = "Q31", new_name = "Travelling_with_children", include = "TRUE",
      stringsAsFactors = FALSE
    ))

    expect_true(loa_ok())
    expect_equal(step_states()[["loa"]], "done")
    expect_false(loa_has_errors(loa_problems()))
    expect_equal(analysis_spec()$group_variables, c("Overall", "Travelling_with_children"))
    expect_equal(analysis_spec()$source$filename, "Set up in the app")

    session$setInputs(folder = pick_folder(destination_dir))
    expect_true(ak_can_run(step_states()))
    session$setInputs(run = 1)

    expect_null(run_error())
    expect_false(is.null(analysis_results()))
    expect_true(file.exists(saved_path()))
  })
})

test_that("a problem made on screen blocks the run like one in a workbook", {
  skip_without_app()

  shiny::testServer(app_dir, {
    session$setInputs(dataset = upload(dataset_file()))
    setup$set("analysis_variables", data.frame(
      analysis_type = "prop_select_one", analysis_var = "Q27", stringsAsFactors = FALSE
    ))
    setup$set("disaggregation_variables", data.frame(
      raw_data_name = "Q31", new_name = "Overall", stringsAsFactors = FALSE
    ))

    expect_true(loa_has_errors(loa_problems()))
    expect_equal(step_states()[["checks"]], "error")
    expect_false(ak_can_run(step_states()))
  })
})

test_that("importing a workbook fills the setup, and editing it is recorded", {
  skip_without_app()

  shiny::testServer(app_dir, {
    session$setInputs(dataset = upload(dataset_file()))
    session$setInputs(loa = upload(loa_file(), "round_8_loa.xlsx"))

    expect_equal(nrow(setup$sheet("analysis_variables")), 3L)
    expect_equal(setup$sheet("disaggregation_variables")$raw_data_name, c("Q27", "Q31"))
    expect_equal(analysis_spec()$source$filename, "round_8_loa.xlsx")

    setup$set("count_selections", setup$sheet("count_selections")[0, , drop = FALSE])
    expect_equal(analysis_spec()$source$filename, "round_8_loa.xlsx (edited in the app)")
    expect_length(analysis_spec()$count_selections, 0L)
  })
})

test_that("the setup downloads as a workbook that imports back", {
  skip_without_app()

  shiny::testServer(app_dir, {
    session$setInputs(dataset = upload(dataset_file()))
    session$setInputs(loa = upload(loa_file()))
    written <- output$setup_download

    expect_true(file.exists(written))
    sheets <- readxl::excel_sheets(written)
    expect_true(all(c("analysis_variables", "disaggregation_variables", "settings") %in% sheets))
  })
})

test_that("the settings tab writes only what differs from the default", {
  skip_without_app()

  shiny::testServer(app_dir, {
    session$setInputs(`settings-s_engine` = "fast")
    expect_equal(ak_setting_text(setup$sheet("settings"), "engine"), "fast")
    session$setInputs(`settings-s_engine` = "auto")
    expect_null(ak_setting_text(setup$sheet("settings"), "engine"))

    session$setInputs(`route-s_places_stop` = TRUE)
    expect_equal(ak_setting_text(setup$sheet("route_analysis"), "places_stop"), "TRUE")
  })
})
