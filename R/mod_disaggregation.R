# =============================================================================
# Tab: Disaggregations  (sheet disaggregation_variables)
# =============================================================================

#' The Suggested Output Name of Each Disaggregation Variable
#' @param raw Question codes.
#' @param dictionary From ak_dataset_dictionary(), or `NULL`.
#' @return A character vector.
ak_disaggregation_suggestions <- function(raw, dictionary) {
  labels <- if (is.null(dictionary)) rep(NA_character_, length(raw)) else {
    dictionary$label[match(raw, dictionary$variable)]
  }
  as.character(mapply(ak_suggest_new_name, raw, labels, USE.NAMES = FALSE))
}


#' The Disaggregations Table as Shown, With Its Cell Controls
#'
#' new_name is a text box whose placeholder is the suggested name. A row
#' still holding the suggestion shows an empty box, so the placeholder is what
#' is used; a name typed over it shows as typed. A blank include means
#' included, as the reader treats it.
#'
#' @param df The disaggregation_variables sheet.
#' @param dictionary From ak_dataset_dictionary(), or `NULL`.
#' @return A data frame of HTML strings.
ak_disaggregation_list_view <- function(df, dictionary) {
  n <- nrow(df)
  rows <- seq_len(n)
  raw <- as.character(df$raw_data_name)
  name <- as.character(df$new_name)
  name[is.na(name)] <- ""
  suggested <- ak_disaggregation_suggestions(raw, dictionary)
  include <- if ("include" %in% names(df)) as.character(df$include) else rep(NA, n)
  label <- if (is.null(dictionary)) rep("", n) else dictionary$label[match(raw, dictionary$variable)]
  label[is.na(label)] <- ""

  data.frame(
    raw_data_name = htmltools::htmlEscape(raw),
    new_name = ak_text_cell(rows, "new_name", ifelse(name == suggested, "", name), suggested),
    include = ak_checkbox_cell(rows, "include",
                               is.na(include) | toupper(trimws(include)) != "FALSE"),
    question_label = htmltools::htmlEscape(ak_truncate(label, 60L)),
    stringsAsFactors = FALSE
  )
}


mod_disaggregation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    ak_section(
      "Disaggregations",
      ak_help(
        "Every analysis is reported overall and for each group of these ",
        "variables, one column block per variable in this order. Overall is ",
        "always included first and is not listed here."
      ),
      shiny::fluidRow(
        shiny::column(
          9,
          shiny::selectizeInput(
            ns("pick"), "Variables to add", choices = NULL, multiple = TRUE,
            options = list(placeholder = "Search by code or question")
          )
        ),
        shiny::column(
          3,
          shiny::tags$label(class = "control-label", shiny::HTML("&nbsp;")),
          shiny::actionButton(ns("add"), "Add", class = "btn-primary btn-block",
                              icon = shiny::icon("plus"))
        )
      ),
      shiny::tags$div(class = "ak-chip-row",
                      shiny::tags$span(class = "ak-chip ak-chip-fixed", "Overall")),
      ak_toolbar(
        shiny::actionButton(ns("up"), NULL, icon = shiny::icon("arrow-up"), title = "Move up"),
        shiny::actionButton(ns("down"), NULL, icon = shiny::icon("arrow-down"), title = "Move down"),
        shiny::actionButton(ns("remove"), "Remove", icon = shiny::icon("trash"))
      ),
      DT::DTOutput(ns("list")),
      shiny::uiOutput(ns("problems"))
    )
  )
}


mod_disaggregation_server <- function(id, store, dictionary, problems) {
  shiny::moduleServer(id, function(input, output, session) {
    sheet <- shiny::reactive(store$sheet("disaggregation_variables"))

    shiny::observe({
      d <- dictionary()
      used <- sheet()$raw_data_name
      choices <- if (is.null(d)) character(0) else {
        # Single-choice questions are what disaggregations normally are, so
        # they are listed first.
        d <- d[order(d$type != "select_one", seq_len(nrow(d))), , drop = FALSE]
        d <- d[d$type %in% c("select_one", "numeric", "text") & !d$variable %in% used, , drop = FALSE]
        ak_variable_choices(d)
      }
      shiny::updateSelectizeInput(session, "pick", choices = choices, server = TRUE)
    })

    shiny::observeEvent(input$add, {
      picked <- input$pick
      if (length(picked) == 0L) return()
      d <- dictionary()
      labels <- d$label[match(picked, d$variable)]
      new_names <- mapply(ak_suggest_new_name, picked, labels, USE.NAMES = FALSE)
      new_names <- make.unique(c(sheet()$new_name, new_names), sep = "_")[
        nrow(sheet()) + seq_along(new_names)
      ]
      rows <- data.frame(raw_data_name = picked, new_name = new_names,
                         include = "TRUE", stringsAsFactors = FALSE)
      store$set("disaggregation_variables", ak_bind_rows(sheet(), rows))
      shiny::updateSelectizeInput(session, "pick", selected = character(0))
    })

    list_data <- shiny::reactive(ak_disaggregation_list_view(sheet(), dictionary()))

    ak_dt_bind(output, session, "list", list_data, height = "480px",
               ordering = FALSE, escape = FALSE,
               callback = ak_cell_input_js(session$ns("cell")),
               descriptions = c(
                 raw_data_name = "The column in the dataset.",
                 new_name = "Its heading in the results. Left empty, the suggestion shown is used.",
                 include = "Ticked: reported. Unticked: kept here, left out of the run.",
                 question_label = "The question's label in the dataset."
               ))

    shiny::observeEvent(input$cell, {
      ev <- input$cell
      df <- sheet()
      row <- as.integer(ev$row)
      if (length(row) != 1L || is.na(row) || row < 1L || row > nrow(df)) return()
      if (identical(ev$col, "include")) {
        df <- ak_set_cell(df, row, "include", if (isTRUE(ev$value)) "TRUE" else "FALSE")
      } else if (identical(ev$col, "new_name")) {
        typed <- trimws(as.character(ev$value %||% ""))
        df <- ak_set_cell(df, row, "new_name", if (nzchar(typed)) {
          typed
        } else {
          ak_disaggregation_suggestions(df$raw_data_name[row], dictionary())
        })
      } else {
        return()
      }
      store$set("disaggregation_variables", df)
    })

    selected <- shiny::reactive(input$list_rows_selected %||% integer(0))
    proxy <- function() DT::dataTableProxy("list", session = session)

    move <- function(direction) {
      res <- ak_move_rows(sheet(), selected(), direction)
      store$set("disaggregation_variables", res$df)
      DT::selectRows(proxy(), res$rows)
    }
    shiny::observeEvent(input$up, move(-1))
    shiny::observeEvent(input$down, move(1))

    shiny::observeEvent(input$remove, {
      rows <- selected()
      if (length(rows) == 0L) return()
      store$set("disaggregation_variables", sheet()[-rows, , drop = FALSE])
      DT::selectRows(proxy(), NULL)
    })

    output$problems <- shiny::renderUI(
      ak_sheet_problems_ui(problems(), "disaggregation_variables")
    )
  })
}
