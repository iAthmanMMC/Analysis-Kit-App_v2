# =============================================================================
# Tab: Analysis variables  (sheet analysis_variables)
# =============================================================================
#
# Left as a browser of the dataset's questions, which the analyst picks from;
# below it the list of analyses to run, edited in the table for free-text
# cells and through a form for the cells that must name a real variable.
# =============================================================================

ak_analysis_type_labels <- function() {
  c(
    "Proportion - single choice" = "prop_select_one",
    "Proportion - multiple choice" = "prop_select_multiple",
    "Mean" = "mean",
    "Median" = "median",
    "Ratio" = "ratio"
  )
}

ak_level_choices <- function() {
  c("None (fast, no interval)" = "", "90%" = "0.9", "95%" = "0.95", "99%" = "0.99")
}


mod_analysis_vars_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    ak_section(
      "Questions in the dataset",
      ak_help(
        "Select one or more questions and add them to the analysis. The ",
        "analysis type is suggested from what the export looks like; you can ",
        "change it afterwards."
      ),
      shiny::uiOutput(ns("browser_status")),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::selectInput(
            ns("type_filter"), "Show",
            choices = c(
              "All questions" = "", "Single choice" = "select_one",
              "Multiple choice" = "select_multiple", "Numeric" = "numeric",
              "Text" = "text"
            )
          )
        ),
        shiny::column(
          6,
          shiny::selectInput(
            ns("add_as"), "Add selected as",
            choices = c("Suggested type" = "", ak_analysis_type_labels()[1:4])
          )
        )
      ),
      DT::DTOutput(ns("browser")),
      ak_toolbar(
        shiny::actionButton(ns("add"), "Add selected", class = "btn-primary",
                            icon = shiny::icon("plus")),
        shiny::actionButton(ns("add_ratio"), "Add a ratio",
                            icon = shiny::icon("divide"))
      )
    ),
    ak_section(
      "Analyses to run",
      ak_help(
        "One row per analysis, in the order they appear in the results. ",
        "Click a cell in the level, full-sample or metadata columns to edit it; ",
        "use Edit row to change the question or the analysis type."
      ),
      ak_toolbar(
        shiny::actionButton(ns("edit"), "Edit row", icon = shiny::icon("pen")),
        shiny::actionButton(ns("up"), NULL, icon = shiny::icon("arrow-up"),
                            title = "Move up"),
        shiny::actionButton(ns("down"), NULL, icon = shiny::icon("arrow-down"),
                            title = "Move down"),
        shiny::actionButton(ns("remove"), "Remove", icon = shiny::icon("trash")),
        shiny::actionButton(ns("clear"), "Remove all", class = "btn-link")
      ),
      DT::DTOutput(ns("list")),
      shiny::tags$details(
        class = "ak-details",
        shiny::tags$summary("Set a value on several rows, or add a metadata column"),
        shiny::fluidRow(
          shiny::column(4, shiny::selectInput(ns("bulk_column"), "Column", choices = NULL)),
          shiny::column(4, shiny::textInput(ns("bulk_value"), "Value")),
          shiny::column(4, shiny::tags$label(class = "control-label", shiny::HTML("&nbsp;")),
                        shiny::actionButton(ns("bulk_apply"), "Apply to selected rows",
                                            class = "btn-block"))
        ),
        shiny::fluidRow(
          shiny::column(8, shiny::textInput(
            ns("new_column"), "New metadata column",
            placeholder = "e.g. sector or indicator"
          )),
          shiny::column(4, shiny::tags$label(class = "control-label", shiny::HTML("&nbsp;")),
                        shiny::actionButton(ns("add_column"), "Add column",
                                            class = "btn-block"))
        ),
        ak_help(
          "Metadata columns are carried into the results only when they are ",
          "listed under Output shape > extra_columns on the Settings tab."
        )
      ),
      shiny::uiOutput(ns("engine_hint")),
      shiny::uiOutput(ns("problems"))
    )
  )
}


mod_analysis_vars_server <- function(id, store, dictionary, problems) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    sheet <- shiny::reactive(store$sheet("analysis_variables"))

    # -- browser -------------------------------------------------------------
    browser_data <- shiny::reactive({
      d <- dictionary()
      shiny::req(d)
      if (nzchar(input$type_filter %||% "")) {
        d <- d[d$type == input$type_filter, , drop = FALSE]
      }
      data.frame(
        Question = d$variable,
        Label = ak_truncate(d$label, 80L),
        Type = d$type,
        Choices = ifelse(
          d$n_choices > 0,
          paste0(d$n_choices, ": ", ak_truncate(
            vapply(d$choices, paste, character(1), collapse = "; "), 60L
          )),
          ""
        ),
        Answered = sprintf("%.0f%%", 100 * d$answered),
        stringsAsFactors = FALSE
      )
    })

    output$browser_status <- shiny::renderUI({
      if (is.null(dictionary())) {
        return(ak_status("Upload a dataset to see its questions."))
      }
      shiny::tagList()
    })

    ak_dt_bind(output, session, "browser", browser_data, page_length = 10L)

    shiny::observeEvent(input$add, {
      d <- dictionary()
      picked <- browser_data()$Question[input$browser_rows_selected]
      if (length(picked) == 0L) {
        shiny::showNotification("Select at least one question first.", type = "warning")
        return()
      }
      types <- d$type[match(picked, d$variable)]
      chosen <- input$add_as %||% ""
      analysis_type <- if (nzchar(chosen)) rep(chosen, length(picked)) else ak_suggest_analysis_type(types)

      skipped <- picked[is.na(analysis_type)]
      keep <- !is.na(analysis_type)
      rows <- data.frame(
        analysis_type = analysis_type[keep],
        analysis_var = picked[keep],
        stringsAsFactors = FALSE
      )
      current <- sheet()
      dup <- paste(rows$analysis_type, rows$analysis_var) %in%
        paste(current$analysis_type, current$analysis_var)
      rows <- rows[!dup, , drop = FALSE]

      if (nrow(rows) > 0L) store$set("analysis_variables", ak_bind_rows(current, rows))
      msg <- c(
        if (nrow(rows) > 0L) sprintf("Added %d analysis(es).", nrow(rows)),
        if (any(dup)) sprintf("%d already in the list.", sum(dup)),
        if (length(skipped)) sprintf(
          "No type suggested for %s - choose one under 'Add selected as'.",
          paste(skipped, collapse = ", ")
        )
      )
      shiny::showNotification(paste(msg, collapse = " "),
                              type = if (length(skipped)) "warning" else "message")
      DT::selectRows(DT::dataTableProxy("browser", session = session), NULL)
    })

    # -- the list ------------------------------------------------------------
    labels <- shiny::reactive({
      d <- dictionary()
      if (is.null(d)) return(character(0))
      stats::setNames(d$label, d$variable)
    })

    list_data <- shiny::reactive({
      df <- sheet()
      lab <- labels()
      df$question_label <- if (nrow(df) == 0L) character(0) else {
        key <- ifelse(df$analysis_type %in% "ratio",
                      paste(df$analysis_var_numerator %||% NA, "/", df$analysis_var_denominator %||% NA),
                      df$analysis_var)
        out <- unname(lab[df$analysis_var])
        out[is.na(out)] <- ""
        out[df$analysis_type %in% "ratio"] <- key[df$analysis_type %in% "ratio"]
        ak_truncate(out, 60L)
      }
      df
    })

    editable <- shiny::reactive({
      setdiff(names(sheet()), c("analysis_type", "analysis_var", "group_var",
                                "analysis_var_numerator", "analysis_var_denominator"))
    })

    shiny::observe({
      cols <- editable()
      shiny::updateSelectInput(session, "bulk_column", choices = cols,
                               selected = shiny::isolate(input$bulk_column) %||% cols[1])
    })

    # Editable columns are read when the table renders; a new metadata column
    # changes the columns, which re-renders it with the new set.
    ak_dt_bind(output, session, "list", list_data,
               editable_columns = editable, page_length = 25L)

    shiny::observeEvent(input$list_cell_edit, {
      df <- sheet()
      info <- input$list_cell_edit
      info <- info[info$col + 1L <= ncol(df), , drop = FALSE]
      store$set("analysis_variables", ak_apply_cell_edit(df, info))
    })

    selected <- shiny::reactive(input$list_rows_selected %||% integer(0))

    move <- function(direction) {
      res <- ak_move_rows(sheet(), selected(), direction)
      store$set("analysis_variables", res$df)
      DT::selectRows(DT::dataTableProxy("list", session = session), res$rows)
    }
    shiny::observeEvent(input$up, move(-1))
    shiny::observeEvent(input$down, move(1))

    shiny::observeEvent(input$remove, {
      rows <- selected()
      if (length(rows) == 0L) return()
      df <- sheet()
      store$set("analysis_variables", df[-rows, , drop = FALSE])
      DT::selectRows(DT::dataTableProxy("list", session = session), NULL)
    })

    shiny::observeEvent(input$clear, {
      shiny::showModal(shiny::modalDialog(
        title = "Remove every analysis?",
        "The list of analyses will be emptied. Disaggregations and settings are kept.",
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("clear_confirm"), "Remove all", class = "btn-primary")
        )
      ))
    })
    shiny::observeEvent(input$clear_confirm, {
      store$set("analysis_variables", sheet()[0, , drop = FALSE])
      shiny::removeModal()
    })

    shiny::observeEvent(input$bulk_apply, {
      rows <- selected()
      col <- input$bulk_column
      if (length(rows) == 0L || is.null(col) || !nzchar(col)) {
        shiny::showNotification("Select rows in the list and a column first.", type = "warning")
        return()
      }
      df <- sheet()
      value <- trimws(input$bulk_value)
      if (is.numeric(df[[col]])) {
        df[[col]][rows] <- suppressWarnings(as.numeric(value))
      } else {
        if (is.logical(df[[col]])) df[[col]] <- as.character(df[[col]])
        df[[col]][rows] <- if (nzchar(value)) value else NA_character_
      }
      store$set("analysis_variables", df)
    })

    shiny::observeEvent(input$add_column, {
      name <- trimws(input$new_column)
      df <- sheet()
      if (!nzchar(name) || make.names(name) != name) {
        shiny::showNotification("Use a column name made of letters, numbers and underscores.",
                                type = "warning")
        return()
      }
      if (name %in% names(df)) {
        shiny::showNotification("That column is already in the list.", type = "warning")
        return()
      }
      df[[name]] <- rep(NA_character_, nrow(df))
      store$set("analysis_variables", df)
      shiny::updateTextInput(session, "new_column", value = "")
    })

    # -- edit form -----------------------------------------------------------
    editing <- shiny::reactiveVal(NULL)  # row number, or 0 for a new row

    edit_form <- function(row, values) {
      d <- dictionary()
      all_vars <- ak_variable_choices(d)
      numeric_vars <- ak_variable_choices(d, "numeric")
      get <- function(col) {
        v <- values[[col]]
        if (is.null(v) || length(v) == 0L || is.na(v)) "" else as.character(v)
      }
      shiny::modalDialog(
        title = if (row == 0L) "Add a ratio" else "Edit analysis",
        size = "m",
        easyClose = TRUE,
        shiny::selectInput(ns("f_type"), "Analysis type",
                           choices = ak_analysis_type_labels(),
                           selected = get("analysis_type")),
        shiny::conditionalPanel(
          sprintf("input['%s'] != 'ratio'", ns("f_type")),
          shiny::selectizeInput(ns("f_var"), "Question", choices = c("", all_vars),
                                selected = get("analysis_var"))
        ),
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'ratio'", ns("f_type")),
          ak_help("sum(numerator) / sum(denominator) within each group - for example children per household."),
          shiny::selectizeInput(ns("f_num"), "Numerator",
                                choices = c("", numeric_vars, all_vars),
                                selected = get("analysis_var_numerator")),
          shiny::selectizeInput(ns("f_den"), "Denominator",
                                choices = c("", numeric_vars, all_vars),
                                selected = get("analysis_var_denominator")),
          shiny::checkboxInput(ns("f_na0"), "Count a missing numerator as 0",
                               value = toupper(get("numerator_NA_to_0")) == "TRUE"),
          shiny::checkboxInput(ns("f_den0"), "Leave out respondents whose denominator is 0",
                               value = toupper(get("filter_denominator_0")) == "TRUE")
        ),
        shiny::selectInput(
          ns("f_level"), "Confidence interval",
          choices = ak_level_choices(),
          selected = {
            l <- suppressWarnings(as.numeric(get("level")))
            if (is.na(l)) "" else as.character(if (l > 1) l / 100 else l)
          }
        ),
        ak_help("Only set this on rows that publish a confidence interval: those rows use the slower survey engine."),
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'prop_select_one' || input['%s'] == 'prop_select_multiple'",
                  ns("f_type"), ns("f_type")),
          shiny::checkboxInput(ns("f_full"), "Out of the full sample (those not asked stay in the base)",
                               value = toupper(get("full_sample_base")) == "TRUE"),
          shiny::textInput(ns("f_full_label"), "Label for those not asked",
                           value = get("full_sample_label"),
                           placeholder = "Not asked / no response")
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("f_save"), "Save", class = "btn-primary")
        )
      )
    }

    shiny::observeEvent(input$edit, {
      rows <- selected()
      if (length(rows) != 1L) {
        shiny::showNotification("Select one row in the list to edit.", type = "warning")
        return()
      }
      editing(rows)
      shiny::showModal(edit_form(rows, as.list(sheet()[rows, , drop = FALSE])))
    })

    shiny::observeEvent(input$add_ratio, {
      editing(0L)
      shiny::showModal(edit_form(0L, list(analysis_type = "ratio")))
    })

    shiny::observeEvent(input$f_save, {
      row <- editing()
      type <- input$f_type
      is_ratio <- identical(type, "ratio")
      is_prop <- type %in% c("prop_select_one", "prop_select_multiple")
      blank <- function(x) if (is.null(x) || !nzchar(x)) NA_character_ else x

      if (is_ratio && (!nzchar(input$f_num %||% "") || !nzchar(input$f_den %||% ""))) {
        shiny::showNotification("A ratio needs both a numerator and a denominator.", type = "error")
        return()
      }
      if (!is_ratio && !nzchar(input$f_var %||% "")) {
        shiny::showNotification("Choose the question to analyse.", type = "error")
        return()
      }

      values <- list(
        analysis_type = type,
        analysis_var = if (is_ratio) NA_character_ else input$f_var,
        level = suppressWarnings(as.numeric(input$f_level)),
        full_sample_base = if (is_prop && isTRUE(input$f_full)) "TRUE" else NA_character_,
        full_sample_label = if (is_prop) blank(trimws(input$f_full_label)) else NA_character_
      )
      if (is_ratio) {
        values$analysis_var_numerator <- input$f_num
        values$analysis_var_denominator <- input$f_den
        values$numerator_NA_to_0 <- if (isTRUE(input$f_na0)) "TRUE" else NA_character_
        values$filter_denominator_0 <- if (isTRUE(input$f_den0)) "TRUE" else NA_character_
      } else {
        # Values left in the ratio columns of a non-ratio row only raise a
        # warning; clear them when the row stops being a ratio.
        for (col in intersect(ak_ratio_columns(), names(sheet()))) values[[col]] <- NA_character_
      }

      new_row <- as.data.frame(values, stringsAsFactors = FALSE)
      df <- sheet()
      if (row == 0L) {
        store$set("analysis_variables", ak_bind_rows(df, new_row))
      } else {
        df <- ak_bind_rows(df, new_row[0, , drop = FALSE])
        for (col in names(values)) {
          if (is.logical(df[[col]])) df[[col]] <- as.character(df[[col]])
          df[[col]][row] <- values[[col]]
        }
        store$set("analysis_variables", df)
      }
      shiny::removeModal()
    })

    # -- notes -----------------------------------------------------------------
    output$engine_hint <- shiny::renderUI({
      df <- sheet()
      if (nrow(df) == 0L || !"level" %in% names(df)) return(shiny::tagList())
      n <- sum(!is.na(suppressWarnings(as.numeric(df$level))))
      if (n == 0L) return(shiny::tagList())
      ak_status(sprintf(
        paste0("%d row(s) set a confidence interval. They only reach the results ",
               "when stat_low / stat_upp are in Settings > value_columns; the ",
               "Checks tab says which engine each will use."), n
      ))
    })

    output$problems <- shiny::renderUI(
      ak_sheet_problems_ui(problems(), "analysis_variables")
    )
  })
}
