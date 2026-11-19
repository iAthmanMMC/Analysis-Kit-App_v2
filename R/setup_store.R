# =============================================================================
# The setup store: one place the builder tabs read and write
# =============================================================================
#
# Each tab owns one or two sheets and edits them through store$set(). An import
# replaces every sheet at once through store$load() and bumps store$version(),
# which is the signal for the tabs to refresh the inputs they show (an input
# never drives the store and is driven by it in the same breath, so there is
# no loop: a refreshed input writes back the value the sheet already holds,
# and an unchanged sheet is not stored again).
# =============================================================================


#' Create the Setup Store
#' @return A list of functions over one reactiveValues.
ak_setup_store <- function() {
  rv <- shiny::reactiveValues(
    sheets = ak_empty_sheets(),
    meta = list(),
    edited = FALSE,
    version = 0L
  )

  list(
    # The whole list of sheets, as a reactive read.
    sheets = function() rv$sheets,

    # One sheet.
    sheet = function(name) rv$sheets[[name]],

    # Replace one sheet; a no-op when nothing changed.
    set = function(name, df) {
      rownames(df) <- NULL
      current <- shiny::isolate(rv$sheets[[name]])
      if (identical(current, df)) return(invisible(FALSE))
      sheets <- shiny::isolate(rv$sheets)
      sheets[[name]] <- df
      rv$sheets <- sheets
      rv$edited <- TRUE
      invisible(TRUE)
    },

    # Replace everything, from an import.
    load = function(sheets, meta = list()) {
      rv$sheets <- ak_normalise_sheets(sheets)
      rv$meta <- meta
      rv$edited <- FALSE
      rv$version <- shiny::isolate(rv$version) + 1L
    },

    # Start again from nothing.
    clear = function() {
      rv$sheets <- ak_empty_sheets()
      rv$meta <- list()
      rv$edited <- FALSE
      rv$version <- shiny::isolate(rv$version) + 1L
    },

    version = function() rv$version,

    # The workbook the package reads. Its filename is what the results readme
    # records as the List of Analysis, so it says where the setup came from.
    workbook = function() {
      meta <- rv$meta
      if (is.null(meta$filename)) {
        meta$filename <- "Set up in the app"
        meta$format <- "App"
      } else if (isTRUE(rv$edited)) {
        meta$filename <- paste0(meta$filename, " (edited in the app)")
        meta$format <- "App"
      }
      ak_setup_workbook(rv$sheets, meta)
    }
  )
}


# -----------------------------------------------------------------------------
# Small shared pieces for the builder tabs
# -----------------------------------------------------------------------------

#' A DT Table in the House Style, Bound to a Reactive Data Frame
#'
#' Rendered once per change of columns, then updated in place through a proxy,
#' so editing a cell or adding a row keeps the page, the search and the
#' selection the analyst was looking at.
#'
#' @param output,session The module's output and session.
#' @param id Output id.
#' @param data A reactive returning the data frame to show.
#' @param editable_columns Column names that can be edited in the cell, or a
#'   reactive returning them.
#' @param selection `"multiple"` or `"single"`.
#' @param page_length Rows per page.
#' @param descriptions Optional named character vector, one short description
#'   per column, shown as a second header row under the column names.
#' @param escape Passed to DT::datatable(); `FALSE` for columns that hold
#'   input controls built by the module.
#' @param callback Optional JavaScript (DT::JS) run when the table is drawn.
#' @param ordering Allow sorting by clicking a header.
#' @return The proxy, invisibly.
ak_dt_bind <- function(output, session, id, data, editable_columns = character(0),
                       selection = "multiple", page_length = 10L,
                       descriptions = NULL, escape = TRUE, callback = NULL,
                       ordering = TRUE) {
  columns <- shiny::reactive(names(data()))

  output[[id]] <- DT::renderDT({
    cols <- columns()
    df <- shiny::isolate(data())
    editable <- if (is.function(editable_columns)) {
      shiny::isolate(editable_columns())
    } else {
      editable_columns
    }
    disabled <- which(!cols %in% editable) - 1L
    extra <- list()
    if (!is.null(descriptions)) {
      extra$container <- htmltools::tags$table(
        class = "display",
        htmltools::tags$thead(
          htmltools::tags$tr(lapply(cols, htmltools::tags$th)),
          htmltools::tags$tr(
            class = "ak-col-help",
            lapply(cols, function(col) htmltools::tags$th(unname(descriptions[col]) %||% ""))
          )
        )
      )
    }
    if (!is.null(callback)) extra$callback <- callback
    do.call(DT::datatable, c(list(
      df,
      rownames = FALSE,
      escape = escape,
      selection = selection,
      class = "compact stripe hover ak-dt",
      editable = if (length(editable) > 0L) {
        list(target = "cell", disable = list(columns = disabled))
      } else {
        FALSE
      },
      options = list(
        pageLength = page_length,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        dom = "ftip",
        ordering = ordering,
        language = list(emptyTable = "Nothing added yet.")
      )
    ), extra))
  }, server = TRUE)

  proxy <- DT::dataTableProxy(id, session = session)
  shiny::observeEvent(data(), {
    DT::replaceData(
      proxy, data(),
      rownames = FALSE, resetPaging = FALSE, clearSelection = "none"
    )
  }, ignoreInit = TRUE)

  invisible(proxy)
}


#' Apply a DT Cell Edit to a Data Frame
#'
#' @param df The data frame shown (with `rownames = FALSE`).
#' @param info `input$<id>_cell_edit`.
#' @return The edited data frame.
ak_apply_cell_edit <- function(df, info) {
  for (k in seq_len(nrow(info))) {
    row <- info$row[k]
    col <- info$col[k] + 1L
    if (row < 1L || row > nrow(df) || col < 1L || col > ncol(df)) next
    value <- trimws(as.character(info$value[k]))
    df[row, col] <- if (is.numeric(df[[col]])) {
      suppressWarnings(as.numeric(value))
    } else if (!nzchar(value)) {
      NA
    } else {
      value
    }
  }
  df
}


#' Move Rows Up or Down
#' @param df A data frame.
#' @param rows Selected row numbers.
#' @param direction `-1` up, `1` down.
#' @return A list with the reordered `df` and the new positions of `rows`.
ak_move_rows <- function(df, rows, direction) {
  n <- nrow(df)
  rows <- sort(unique(rows[rows >= 1 & rows <= n]))
  order <- seq_len(n)
  if (length(rows) == 0L || n < 2L) return(list(df = df, rows = rows))
  sel <- order %in% rows
  # Walk towards the direction of travel, so a block of selected rows moves
  # together; a selected row already at the edge stays and blocks the rest.
  steps <- if (direction < 0) 2:n else (n - 1L):1
  for (i in steps) {
    j <- i + if (direction < 0) -1L else 1L
    if (sel[i] && !sel[j]) {
      order[c(i, j)] <- order[c(j, i)]
      sel[c(i, j)] <- sel[c(j, i)]
    }
  }
  df <- df[order, , drop = FALSE]
  rownames(df) <- NULL
  list(df = df, rows = which(sel))
}


#' Add Rows to a Sheet, Filling Columns It Does Not Have
#' @param df The sheet.
#' @param rows A data frame of new rows.
#' @return The combined data frame.
ak_bind_rows <- function(df, rows) {
  for (col in setdiff(names(rows), names(df))) {
    df[[col]] <- if (is.numeric(rows[[col]])) rep(NA_real_, nrow(df)) else rep(NA_character_, nrow(df))
  }
  for (col in setdiff(names(df), names(rows))) {
    rows[[col]] <- if (is.numeric(df[[col]])) rep(NA_real_, nrow(rows)) else rep(NA_character_, nrow(rows))
  }
  # A column readxl read as logical (all blank) must take text now.
  for (col in names(df)) {
    if (is.logical(df[[col]]) && !is.logical(rows[[col]])) {
      df[[col]] <- if (is.numeric(rows[[col]])) as.numeric(df[[col]]) else as.character(df[[col]])
    }
    if (is.logical(rows[[col]]) && !is.logical(df[[col]])) {
      rows[[col]] <- if (is.numeric(df[[col]])) as.numeric(rows[[col]]) else as.character(rows[[col]])
    }
  }
  out <- rbind(df, rows[names(df)])
  rownames(out) <- NULL
  out
}


#' The Problems the Checks Found in One Sheet
#'
#' Shown at the foot of the tab that owns the sheet, so a problem is read where
#' it can be fixed.
#'
#' @param problems The problems table, or `NULL`.
#' @param sheets Sheet names the tab owns.
#' @return A Shiny tag.
ak_sheet_problems_ui <- function(problems, sheets) {
  if (is.null(problems) || nrow(problems) == 0L) return(shiny::tagList())
  hit <- problems[problems$sheet %in% sheets, , drop = FALSE]
  if (nrow(hit) == 0L) return(shiny::tagList())
  shiny::tags$div(
    class = "ak-tab-problems",
    lapply(seq_len(nrow(hit)), function(i) {
      row <- if (!is.na(hit$row[i])) paste0("Row ", hit$row[i], ": ") else ""
      ak_status(paste0(row, hit$message[i]),
                if (hit$severity[i] == "error") "error" else "warning")
    })
  )
}


#' A Row of Buttons Above a Table
#' @param ... Buttons.
#' @return A Shiny tag.
ak_toolbar <- function(...) shiny::tags$div(class = "ak-toolbar", ...)


#' A Short Explanation Under a Heading
#' @param ... Text.
#' @return A Shiny tag.
ak_help <- function(...) shiny::tags$p(class = "ak-help", ...)
