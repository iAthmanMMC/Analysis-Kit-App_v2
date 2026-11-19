# =============================================================================
# Tab: Multiple choice
#   count_selections, count_combinations, count_exclusive_combinations,
#   exclude_choices
# =============================================================================

mod_choices_ui <- function(id) {
  ns <- shiny::NS(id)

  combination_card <- function(prefix, title, help) {
    ak_section(
      title,
      ak_help(help),
      shiny::fluidRow(
        shiny::column(6, shiny::selectInput(ns(paste0(prefix, "_parent")),
                                            "Multiple-choice question", choices = NULL)),
        shiny::column(6, shiny::uiOutput(ns(paste0(prefix, "_limit"))))
      ),
      shiny::checkboxGroupInput(ns(paste0(prefix, "_choices")), "Choices", choices = character(0)),
      ak_toolbar(
        shiny::actionButton(ns(paste0(prefix, "_add")), "Add ticked choices",
                            class = "btn-primary", icon = shiny::icon("plus")),
        shiny::actionButton(ns(paste0(prefix, "_up")), NULL, icon = shiny::icon("arrow-up"), title = "Move up"),
        shiny::actionButton(ns(paste0(prefix, "_down")), NULL, icon = shiny::icon("arrow-down"), title = "Move down"),
        shiny::actionButton(ns(paste0(prefix, "_remove")), "Remove", icon = shiny::icon("trash"))
      ),
      DT::DTOutput(ns(paste0(prefix, "_list"))),
      ak_help("Click a display_name to give the choice a short row label; blank uses the full label.")
    )
  }

  shiny::tagList(
    ak_section(
      "Selection counts",
      ak_help(
        "For each question chosen here the results add how many choices each ",
        "respondent selected: none, exactly one, more than one. The labels and ",
        "order are on the Settings tab."
      ),
      shiny::selectizeInput(ns("count_selections"), "Multiple-choice questions",
                            choices = NULL, multiple = TRUE,
                            options = list(placeholder = "None"))
    ),
    combination_card(
      "comb", "Choice combinations",
      paste0(
        "Respondents who selected more than one choice: which listed choices ",
        "they combined, and whether anything else was selected too."
      )
    ),
    combination_card(
      "excl", "Single choices",
      paste0(
        "The other half of the same question: respondents who selected exactly ",
        "one choice. It shares its denominator with Choice combinations, so the ",
        "two blocks together add to 100%."
      )
    ),
    ak_section(
      "Excluded choices",
      ak_help(
        "A respondent who picked one of these leaves the denominator of that ",
        "question altogether - the row is not merely hidden. Typical: Don't ",
        "know, Refused. Pick from the labels in the dataset or type your own."
      ),
      shiny::selectizeInput(ns("exclude"), "Choices to exclude", choices = NULL,
                            multiple = TRUE,
                            options = list(create = TRUE, placeholder = "None"))
    ),
    shiny::uiOutput(ns("problems"))
  )
}


mod_choices_server <- function(id, store, dictionary, problems, settings_value) {
  shiny::moduleServer(id, function(input, output, session) {
    sm <- shiny::reactive({
      d <- dictionary()
      if (is.null(d)) return(d)
      d[d$type == "select_multiple", , drop = FALSE]
    })

    # -- selection counts ----------------------------------------------------
    shiny::observe({
      store$version()
      chosen <- shiny::isolate(store$sheet("count_selections"))$analysis_var
      shiny::updateSelectizeInput(
        session, "count_selections",
        choices = c(ak_variable_choices(sm()), stats::setNames(chosen, chosen))[
          !duplicated(c(ak_variable_choices(sm()), chosen))
        ],
        selected = chosen
      )
    })

    shiny::observeEvent(input$count_selections, ignoreNULL = FALSE, ignoreInit = TRUE, {
      current <- store$sheet("count_selections")
      wanted <- input$count_selections %||% character(0)
      if (identical(sort(current$analysis_var), sort(wanted))) return()
      kept <- current[current$analysis_var %in% wanted, , drop = FALSE]
      new <- setdiff(wanted, current$analysis_var)
      df <- ak_bind_rows(kept, data.frame(analysis_var = new, stringsAsFactors = FALSE))
      df <- df[match(wanted, df$analysis_var), , drop = FALSE]
      store$set("count_selections", df)
    })

    # -- combinations (both kinds) -------------------------------------------
    combination_server <- function(prefix, sheet_name, limit_key) {
      id <- function(x) paste0(prefix, "_", x)
      sheet <- shiny::reactive(store$sheet(sheet_name))

      shiny::observe({
        shiny::updateSelectInput(session, id("parent"),
                                 choices = c("Choose a question" = "", ak_variable_choices(sm())),
                                 selected = shiny::isolate(input[[id("parent")]]))
      })

      shiny::observe({
        parent <- input[[id("parent")]]
        d <- sm()
        choices <- if (is.null(d) || !nzchar(parent %||% "")) character(0) else {
          d$choices[[match(parent, d$variable)]]
        }
        already <- sheet()$choice_label[sheet()$analysis_var %in% parent]
        shiny::updateCheckboxGroupInput(
          session, id("choices"),
          choices = if (length(choices)) choices else character(0),
          selected = intersect(already, choices)
        )
      })

      output[[id("limit")]] <- shiny::renderUI({
        parent <- input[[id("parent")]]
        if (!nzchar(parent %||% "")) return(shiny::tagList())
        n <- sum(sheet()$analysis_var %in% parent)
        max <- settings_value(limit_key)
        ak_status(sprintf("%d of at most %s choices listed for %s.", n, max, parent),
                  if (n > max) "error" else "neutral")
      })

      shiny::observeEvent(input[[id("add")]], {
        parent <- input[[id("parent")]]
        ticked <- input[[id("choices")]]
        if (!nzchar(parent %||% "") || length(ticked) == 0L) {
          shiny::showNotification("Choose a question and tick at least one choice.", type = "warning")
          return()
        }
        df <- sheet()
        # Ticking reflects the list: unticked choices of this question go.
        drop <- df$analysis_var %in% parent & !df$choice_label %in% ticked
        df <- df[!drop, , drop = FALSE]
        new <- setdiff(ticked, df$choice_label[df$analysis_var %in% parent])
        rows <- data.frame(analysis_var = rep(parent, length(new)), choice_label = new,
                           stringsAsFactors = FALSE)
        store$set(sheet_name, ak_bind_rows(df, rows))
      })

      ak_dt_bind(output, session, id("list"), sheet,
                 editable_columns = c("display_name", "include"), page_length = 10L)

      shiny::observeEvent(input[[id("list_cell_edit")]], {
        store$set(sheet_name, ak_apply_cell_edit(sheet(), input[[id("list_cell_edit")]]))
      })

      selected <- shiny::reactive(input[[id("list_rows_selected")]] %||% integer(0))
      move <- function(direction) {
        res <- ak_move_rows(sheet(), selected(), direction)
        store$set(sheet_name, res$df)
        DT::selectRows(DT::dataTableProxy(id("list"), session = session), res$rows)
      }
      shiny::observeEvent(input[[id("up")]], move(-1))
      shiny::observeEvent(input[[id("down")]], move(1))
      shiny::observeEvent(input[[id("remove")]], {
        rows <- selected()
        if (length(rows) == 0L) return()
        store$set(sheet_name, sheet()[-rows, , drop = FALSE])
        DT::selectRows(DT::dataTableProxy(id("list"), session = session), NULL)
      })
    }

    combination_server("comb", "count_combinations", "max_combination_choices")
    combination_server("excl", "count_exclusive_combinations", "max_exclusive_choices")

    # -- excluded choices ------------------------------------------------------
    shiny::observe({
      store$version()
      d <- dictionary()
      chosen <- shiny::isolate(store$sheet("exclude_choices"))$choice_label
      known <- if (is.null(d)) character(0) else unique(unlist(d$choices[d$type %in% c("select_one", "select_multiple")]))
      common <- c("Don't know", "Refused", "Prefer not to say", "Not applicable")
      options <- unique(c(chosen, common, sort(known)))
      shiny::updateSelectizeInput(session, "exclude", choices = options,
                                  selected = chosen, server = TRUE)
    })

    shiny::observeEvent(input$exclude, ignoreNULL = FALSE, ignoreInit = TRUE, {
      current <- store$sheet("exclude_choices")
      wanted <- input$exclude %||% character(0)
      if (identical(sort(current$choice_label), sort(wanted))) return()
      kept <- current[current$choice_label %in% wanted, , drop = FALSE]
      new <- setdiff(wanted, current$choice_label)
      store$set("exclude_choices",
                ak_bind_rows(kept, data.frame(choice_label = new, stringsAsFactors = FALSE)))
    })

    output$problems <- shiny::renderUI(
      ak_sheet_problems_ui(problems(), c(
        "count_selections", "count_combinations",
        "count_exclusive_combinations", "exclude_choices"
      ))
    )
  })
}
