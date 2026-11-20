# =============================================================================
# Tab: Analysis variables  (sheet analysis_variables)
# =============================================================================
#
# Above, a browser of the dataset's questions; the analysis type of each one
# added is inferred from the export. Below, the list of analyses to run, whose
# type, theme and full-sample cells are edited in place.
#
# "theme" on screen is the `sector` column of the List of Analysis: one sheet
# of the results workbook per theme.
# =============================================================================

ak_analysis_type_labels <- function() {
  c(
    "Proportion - single choice" = "prop_select_one",
    "Proportion - multiple choice" = "prop_select_multiple",
    "Mean" = "mean",
    "Median" = "median"
  )
}


#' The Analysis Types a Question of a Given Type Can Take
#' @param type One inferred type from ak_dataset_dictionary(), or `NA`.
#' @return A character vector of analysis types.
ak_allowed_analysis_types <- function(type) {
  switch(
    if (is.na(type)) "unknown" else type,
    select_one = "prop_select_one",
    select_multiple = "prop_select_multiple",
    numeric = c("mean", "median"),
    unname(ak_analysis_type_labels())
  )
}


#' What Each Column of the Analyses Table Does
#' @return A named character vector.
ak_analysis_column_help <- function() {
  c(
    analysis_type = "How the question is summarised: % per choice, or the mean or median of a number.",
    analysis_var = "The column in the dataset.",
    theme = "Groups analyses into sheets of the results workbook. Blank: the 'analysis' sheet.",
    full_sample_base = "Ticked: % out of everyone, those not asked included. Unticked: out of those who answered.",
    full_sample_label = "Row label for those not asked, when ticked. Blank: Not asked / no response.",
    question = "The question's label in the dataset."
  )
}


#' The Analyses Table as Shown, With Its Cell Controls
#'
#' Pure, so the shape of the table can be tested without a browser. Every
#' control carries the sheet row and the sheet column it edits.
#'
#' @param df The analysis_variables sheet.
#' @param dictionary From ak_dataset_dictionary(), or `NULL`.
#' @return A data frame of HTML strings.
ak_analysis_list_view <- function(df, dictionary) {
  n <- nrow(df)
  esc <- htmltools::htmlEscape
  get <- function(col) {
    v <- if (col %in% names(df)) as.character(df[[col]]) else rep(NA_character_, n)
    v[is.na(v)] <- ""
    trimws(v)
  }
  type <- get("analysis_type")
  var <- get("analysis_var")
  qtype <- if (is.null(dictionary)) rep(NA_character_, n) else dictionary$type[match(var, dictionary$variable)]
  label <- if (is.null(dictionary)) rep("", n) else dictionary$label[match(var, dictionary$variable)]
  label[is.na(label)] <- ""
  is_ratio <- type == "ratio"
  label[is_ratio] <- paste(get("analysis_var_numerator"), "/", get("analysis_var_denominator"))[is_ratio]
  is_prop <- type %in% c("prop_select_one", "prop_select_multiple")
  full <- toupper(get("full_sample_base")) %in% c("TRUE", "T", "YES", "1")
  stop_click <- 'onclick="event.stopPropagation()"'

  type_cell <- vapply(seq_len(n), function(i) {
    allowed <- ak_allowed_analysis_types(qtype[i])
    if (nzchar(type[i]) && !type[i] %in% allowed) allowed <- c(type[i], allowed)
    if (length(allowed) == 1L) return(esc(allowed))
    options <- paste0(
      '<option value="', esc(allowed), '"',
      ifelse(allowed == type[i], " selected", ""), ">", esc(allowed), "</option>",
      collapse = ""
    )
    sprintf(
      '<select class="ak-cell-input form-control input-sm" data-row="%d" data-col="analysis_type" %s>%s</select>',
      i, stop_click, options
    )
  }, character(1))

  text_cell <- function(col, values, placeholder, disabled = rep(FALSE, n)) {
    sprintf(
      '<input type="text" class="ak-cell-input form-control input-sm" data-row="%d" data-col="%s" value="%s" placeholder="%s"%s %s>',
      seq_len(n), col, esc(values), placeholder,
      ifelse(disabled, " disabled", ""), stop_click
    )
  }

  full_cell <- ifelse(
    is_prop,
    sprintf(
      '<input type="checkbox" class="ak-cell-input" data-row="%d" data-col="full_sample_base"%s %s>',
      seq_len(n), ifelse(full, " checked", ""), stop_click
    ),
    "-"
  )
  label_cell <- ifelse(
    is_prop,
    text_cell("full_sample_label", get("full_sample_label"),
              "Not asked / no response", disabled = !full),
    "-"
  )

  data.frame(
    analysis_type = type_cell,
    analysis_var = esc(var),
    theme = text_cell("sector", get("sector"), "analysis"),
    full_sample_base = full_cell,
    full_sample_label = label_cell,
    question = esc(ak_truncate(label, 70L)),
    stringsAsFactors = FALSE
  )
}


#' Set One Cell of a Sheet, Adding the Column When Needed
#' @param df The sheet.
#' @param row,col Row number and column name.
#' @param value The value; `NA` for blank.
#' @return The sheet.
ak_set_cell <- function(df, row, col, value) {
  if (!col %in% names(df)) df[[col]] <- rep(NA_character_, nrow(df))
  if (is.logical(df[[col]])) df[[col]] <- as.character(df[[col]])
  if (is.numeric(df[[col]])) value <- suppressWarnings(as.numeric(value))
  df[[col]][row] <- value
  df
}


#' Make Sure Themes Reach the Results
#'
#' The package carries a metadata column such as `sector` into the results
#' only when `settings/extra_columns` names it, and splits the workbook into
#' one sheet per sector only then. Called whenever a theme is set on screen.
#'
#' @param settings The settings sheet.
#' @param analysis The analysis_variables sheet.
#' @return The settings sheet.
ak_ensure_theme_setting <- function(settings, analysis) {
  sector <- if ("sector" %in% names(analysis)) as.character(analysis$sector) else character(0)
  if (!any(!is.na(sector) & nzchar(trimws(sector)))) return(settings)
  current <- ak_setting_value(settings, "extra_columns", "chr[]", character(0))
  current <- current[nzchar(current)]
  if ("sector" %in% current) return(settings)
  ak_set_setting(settings, "extra_columns", c(current, "sector"), "chr[]", NULL)
}


mod_analysis_vars_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    ak_section(
      "Questions in the dataset",
      ak_help(
        "Select one or more questions and click Add selected. The analysis ",
        "type is worked out from the export: % per choice for single- and ",
        "multiple-choice questions, the mean for numbers. Change it in the ",
        "list below."
      ),
      shiny::uiOutput(ns("browser_status")),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::selectInput(
            ns("type_filter"), "Show",
            choices = c(
              "All" = "all", "Single choice" = "select_one",
              "Multiple choice" = "select_multiple", "Numeric" = "numeric",
              "Text" = "text"
            ),
            selected = "all"
          )
        )
      ),
      DT::DTOutput(ns("browser")),
      ak_toolbar(
        shiny::actionButton(ns("add"), "Add selected", class = "btn-primary",
                            icon = shiny::icon("plus"))
      )
    ),
    ak_section(
      "Analyses to run",
      ak_help(
        "One row per analysis, in the order they appear in the results. Change ",
        "the type, theme and full-sample cells directly in the table; select ",
        "rows to move or remove them."
      ),
      ak_toolbar(
        shiny::actionButton(ns("up"), NULL, icon = shiny::icon("arrow-up"),
                            title = "Move up"),
        shiny::actionButton(ns("down"), NULL, icon = shiny::icon("arrow-down"),
                            title = "Move down"),
        shiny::actionButton(ns("remove"), "Remove", icon = shiny::icon("trash")),
        shiny::actionButton(ns("clear"), "Remove all", class = "btn-link")
      ),
      DT::DTOutput(ns("list")),
      shiny::fluidRow(
        shiny::column(6, shiny::textInput(
          ns("bulk_theme"), "Theme for the selected rows",
          placeholder = "e.g. Protection"
        )),
        shiny::column(4, shiny::tags$label(class = "control-label", shiny::HTML("&nbsp;")),
                      shiny::actionButton(ns("bulk_apply"), "Apply to selected rows",
                                          class = "btn-block"))
      ),
      shiny::uiOutput(ns("level_note")),
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
      filter <- input$type_filter %||% "all"
      if (!identical(filter, "all")) {
        d <- d[d$type == filter, , drop = FALSE]
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

    ak_dt_bind(output, session, "browser", browser_data, height = "360px")

    shiny::observeEvent(input$add, {
      d <- dictionary()
      picked <- browser_data()$Question[input$browser_rows_selected]
      if (length(picked) == 0L) {
        shiny::showNotification("Select at least one question first.", type = "warning")
        return()
      }
      types <- d$type[match(picked, d$variable)]
      analysis_type <- ak_suggest_analysis_type(types)

      skipped <- picked[is.na(analysis_type)]
      keep <- !is.na(analysis_type)
      rows <- data.frame(
        analysis_type = analysis_type[keep],
        analysis_var = picked[keep],
        stringsAsFactors = FALSE
      )
      current <- sheet()
      dup <- rows$analysis_var %in% current$analysis_var
      rows <- rows[!dup, , drop = FALSE]

      if (nrow(rows) > 0L) store$set("analysis_variables", ak_bind_rows(current, rows))
      msg <- c(
        if (nrow(rows) > 0L) sprintf("Added %d analysis(es).", nrow(rows)),
        if (any(dup)) sprintf("%d already in the list.", sum(dup)),
        if (length(skipped)) sprintf(
          "Not added, as only choice and numeric questions can be analysed: %s.",
          paste(skipped, collapse = ", ")
        )
      )
      shiny::showNotification(paste(msg, collapse = " "),
                              type = if (length(skipped)) "warning" else "message")
      DT::selectRows(DT::dataTableProxy("browser", session = session), NULL)
    })

    # -- the list ------------------------------------------------------------
    list_data <- shiny::reactive(ak_analysis_list_view(sheet(), dictionary()))

    # Every control in the table reports the sheet row and column it edits.
    ak_dt_bind(output, session, "list", list_data, height = "480px",
               descriptions = ak_analysis_column_help(), escape = FALSE,
               callback = ak_cell_input_js(ns("cell")), ordering = FALSE)

    shiny::observeEvent(input$cell, {
      ev <- input$cell
      df <- sheet()
      row <- as.integer(ev$row)
      col <- ev$col
      if (length(row) != 1L || is.na(row) || row < 1L || row > nrow(df) ||
          !col %in% c("analysis_type", "sector", "full_sample_base", "full_sample_label")) {
        return()
      }
      value <- if (identical(col, "full_sample_base")) {
        if (isTRUE(ev$value)) "TRUE" else NA_character_
      } else {
        v <- trimws(as.character(ev$value %||% ""))
        if (nzchar(v)) v else NA_character_
      }
      df <- ak_set_cell(df, row, col, value)
      store$set("analysis_variables", df)
      if (identical(col, "sector")) {
        store$set("settings", ak_ensure_theme_setting(store$sheet("settings"), df))
      }
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
        "The list of analyses will be emptied. Disaggregations are kept.",
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
      if (length(rows) == 0L) {
        shiny::showNotification("Select rows in the list first.", type = "warning")
        return()
      }
      value <- trimws(input$bulk_theme %||% "")
      df <- ak_set_cell(sheet(), rows, "sector", if (nzchar(value)) value else NA_character_)
      store$set("analysis_variables", df)
      store$set("settings", ak_ensure_theme_setting(store$sheet("settings"), df))
    })

    # -- notes -----------------------------------------------------------------
    # The level column is not shown, but an imported workbook may still set it.
    output$level_note <- shiny::renderUI({
      df <- sheet()
      if (nrow(df) == 0L || !"level" %in% names(df)) return(shiny::tagList())
      n <- sum(!is.na(suppressWarnings(as.numeric(df$level))))
      if (n == 0L) return(shiny::tagList())
      ak_status(sprintf(
        paste0("%d row(s) of the imported List of Analysis set a confidence ",
               "level. Those rows use the slower survey engine."), n
      ))
    })

    output$problems <- shiny::renderUI(
      ak_sheet_problems_ui(problems(), "analysis_variables")
    )
  })
}
