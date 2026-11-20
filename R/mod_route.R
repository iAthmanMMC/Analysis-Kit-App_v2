# =============================================================================
# Tab: Route analysis  (sheet route_analysis)
# =============================================================================
#
# Generated from analysiskit::loa_settings_schema(), so a key the package adds
# appears here without a change to the app. Defaults come from
# ak_settings_defaults(), which reads the pipeline's own formals. A value equal
# to its default is not written into the sheet, exactly as "blank = default"
# in the template. The run-wide settings sheet is not edited on screen: it
# keeps the template defaults unless an imported workbook sets them.
# =============================================================================

# Columns written per group block that the pipeline knows how to produce.
ak_value_column_choices <- function() {
  c("stat", "n", "n_total", "n_w", "n_w_total", "stat_low", "stat_upp")
}

# Settings whose value is a dataset column, picked rather than typed.
ak_column_settings <- function() c("weight_column", "strata_column", "route_gender_var")


#' One Input for One Setting
#' @param ns Namespace function.
#' @param key Setting name.
#' @param type Schema type.
#' @param values Comma-separated allowed values for an enum.
#' @param default The default value.
#' @return A Shiny tag.
ak_setting_input <- function(ns, key, type, values, default) {
  id <- ns(paste0("s_", key))
  label <- shiny::tags$span(shiny::tags$code(key))
  help <- ak_setting_help(key)
  input <- if (key %in% ak_column_settings()) {
    shiny::selectizeInput(id, label, choices = c("", default %||% ""),
                          selected = default %||% "",
                          options = list(create = TRUE, placeholder = "None"))
  } else if (key == "value_columns") {
    shiny::checkboxGroupInput(id, label, choices = ak_value_column_choices(),
                              selected = default, inline = TRUE)
  } else if (key == "extra_columns") {
    shiny::selectizeInput(id, label, choices = character(0), multiple = TRUE,
                          options = list(placeholder = "None"))
  } else {
    switch(
      type,
      lgl = shiny::checkboxInput(id, label, value = isTRUE(default)),
      enum = shiny::selectInput(id, label, choices = strsplit(values, ",", fixed = TRUE)[[1]],
                                selected = default),
      num = shiny::numericInput(id, label, value = default %||% NA),
      shiny::textInput(id, label, value = default %||% "")
    )
  }
  shiny::tags$div(
    class = "ak-setting",
    input,
    if (!is.null(help)) shiny::tags$div(class = "ak-help ak-setting-help", help)
  )
}


#' Keep a Settings Sheet and Its Inputs in Step
#'
#' @param input,session The module's input and session.
#' @param store The setup store.
#' @param sheet_name `settings` or `route_analysis`.
#' @param schema Rows of loa_settings_schema() for this sheet.
#' @param defaults From ak_settings_defaults().
#' @return A function refreshing given keys' inputs from the sheet.
ak_settings_sync <- function(input, session, store, sheet_name, schema, defaults) {
  refresh <- function(keys = schema$setting) {
    sheet <- shiny::isolate(store$sheet(sheet_name))
    for (key in keys) {
      i <- match(key, schema$setting)
      type <- schema$type[i]
      id <- paste0("s_", key)
      value <- ak_setting_value(sheet, key, type, defaults[[key]])
      if (key %in% ak_column_settings()) {
        shiny::updateSelectizeInput(session, id, selected = value %||% "")
      } else if (key %in% c("value_columns", "extra_columns")) {
        if (key == "value_columns") {
          shiny::updateCheckboxGroupInput(session, id, selected = value %||% character(0))
        } else {
          shiny::updateSelectizeInput(session, id, selected = value %||% character(0))
        }
      } else {
        switch(
          type,
          lgl = shiny::updateCheckboxInput(session, id, value = isTRUE(value)),
          enum = shiny::updateSelectInput(session, id, selected = value),
          num = shiny::updateNumericInput(session, id, value = value %||% NA),
          shiny::updateTextInput(session, id, value = value %||% "")
        )
      }
    }
  }

  shiny::observeEvent(store$version(), refresh(), ignoreInit = TRUE)

  lapply(seq_len(nrow(schema)), function(i) {
    key <- schema$setting[i]
    type <- schema$type[i]
    # Not ignoreInit: an input's first value is its default, which writes
    # nothing, and the store ignores a set that changes nothing.
    shiny::observeEvent(input[[paste0("s_", key)]], ignoreNULL = FALSE, {
      value <- input[[paste0("s_", key)]]
      if (type == "num" && !is.null(value) && is.na(value)) value <- NULL
      sheet <- store$sheet(sheet_name)
      store$set(sheet_name, ak_set_setting(sheet, key, value, type, defaults[[key]]))
    })
  })

  refresh
}


# -----------------------------------------------------------------------------
# Route analysis tab
# -----------------------------------------------------------------------------

mod_route_ui <- function(id) {
  ns <- shiny::NS(id)
  schema <- analysiskit::loa_settings_schema()
  schema <- schema[schema$sheet == "route_analysis", , drop = FALSE]
  defaults <- ak_settings_defaults()
  blocks <- names(analysiskit:::ck_route_blocks())
  input_for <- function(key) {
    i <- match(key, schema$setting)
    ak_setting_input(ns, key, schema$type[i], schema$values[i], defaults[[key]])
  }

  shiny::tagList(
    ak_section(
      "Route analysis",
      ak_help(
        "Pools the repeated 4Mi route questions across every slot. Each block ",
        "switched on gets a results sheet of its own. No rows are needed on the ",
        "Analysis variables tab: the question codes are fixed."
      ),
      input_for("route_full_sample_base"),
      shiny::tags$div(
        class = "ak-route-blocks",
        lapply(blocks, function(b) {
          shiny::tags$div(class = "ak-route-block", input_for(b),
                          shiny::uiOutput(ns(paste0("cover_", b))))
        })
      )
    ),
    ak_section(
      "How the route tables are laid out",
      shiny::tags$div(
        class = "ak-settings-grid",
        lapply(setdiff(schema$setting, c(blocks, "route_full_sample_base")), input_for)
      )
    ),
    shiny::uiOutput(ns("problems"))
  )
}


mod_route_server <- function(id, store, problems, dataset_columns, settings_value) {
  shiny::moduleServer(id, function(input, output, session) {
    schema <- analysiskit::loa_settings_schema()
    schema <- schema[schema$sheet == "route_analysis", , drop = FALSE]
    defaults <- ak_settings_defaults()
    ak_settings_sync(input, session, store, "route_analysis", schema, defaults)

    shiny::observe({
      cols <- dataset_columns()
      selected <- ak_setting_text(shiny::isolate(store$sheet("route_analysis")),
                                  "route_gender_var") %||% defaults$route_gender_var
      shiny::updateSelectizeInput(session, "s_route_gender_var",
                                  choices = unique(c(selected, cols)),
                                  selected = selected, server = TRUE)
    })

    coverage <- shiny::reactive({
      cols <- dataset_columns()
      if (length(cols) == 0L) return(NULL)
      ak_route_coverage(cols, settings_value("sm_separator") %||% "/")
    })

    for (b in names(analysiskit:::ck_route_blocks())) local({
      block <- b
      output[[paste0("cover_", block)]] <- shiny::renderUI({
        cov <- coverage()
        if (is.null(cov)) return(ak_status("Upload a dataset to check its questions."))
        row <- cov[cov$block == block, ]
        on <- isTRUE(input[[paste0("s_", block)]])
        if (row$found == row$total) {
          ak_status(sprintf("All %d questions are in the dataset.", row$total),
                    if (on) "success" else "neutral")
        } else if (row$found == 0L) {
          ak_status(sprintf("None of its %d questions are in the dataset%s.", row$total,
                            if (on) " - this block will be skipped" else ""),
                    if (on) "warning" else "neutral")
        } else {
          ak_status(sprintf("%d of %d questions found. Missing: %s",
                            row$found, row$total, row$missing),
                    if (on) "warning" else "neutral")
        }
      })
    })

    output$problems <- shiny::renderUI(ak_sheet_problems_ui(problems(), "route_analysis"))
  })
}
