# =============================================================================
# The analysis setup, held as List of Analysis sheets
# =============================================================================
#
# The builder tabs edit the same eight sheets the loa_template.xlsx has, as
# plain data frames. ak_setup_workbook() wraps them in the shape
# read_loa_workbook() returns, so build_analysis_spec(), validate_loa() and
# run_analysis_spec() in analysiskit see no difference between a workbook that
# was uploaded and one that was set up on screen. Every check, every default
# and the results workbook therefore stay exactly as they are.
#
# Everything here is pure: no Shiny, testable directly.
# =============================================================================


#' The Sheets the Builder Edits, in Template Order
#' @return A character vector.
ak_setup_sheet_names <- function() {
  c(
    "analysis_variables",
    "disaggregation_variables",
    "count_selections",
    "count_combinations",
    "count_exclusive_combinations",
    "exclude_choices",
    "settings",
    "route_analysis"
  )
}


#' The Columns Each Sheet Starts With
#' @return A named list of character vectors.
ak_setup_columns <- function() {
  list(
    analysis_variables = c(
      "analysis_type", "analysis_var", "level",
      "full_sample_base", "full_sample_label"
    ),
    disaggregation_variables = c("raw_data_name", "new_name", "include"),
    count_selections = c("analysis_var", "include"),
    count_combinations = c("analysis_var", "choice_label", "display_name", "include"),
    count_exclusive_combinations = c("analysis_var", "choice_label", "display_name", "include"),
    exclude_choices = c("choice_label", "include"),
    settings = c("setting", "value"),
    route_analysis = c("setting", "value")
  )
}


#' The Columns a Ratio Row Needs
#' @return A character vector.
ak_ratio_columns <- function() {
  c(
    "analysis_var_numerator", "analysis_var_denominator",
    "numerator_NA_to_0", "filter_denominator_0"
  )
}


#' Columns of analysis_variables the Reader Interprets
#'
#' Anything else in that sheet is metadata (such as `sector`), carried to the
#' output only when named in `settings/extra_columns`.
#' @return A character vector.
ak_analysis_known_columns <- function() {
  c(
    ak_setup_columns()$analysis_variables,
    ak_ratio_columns(),
    "group_var", "description"
  )
}


#' An Empty Data Frame With the Given Character Columns
#' @param columns Column names.
#' @return A zero-row data frame.
ak_empty_frame <- function(columns) {
  df <- as.data.frame(
    stats::setNames(
      replicate(length(columns), character(0), simplify = FALSE),
      columns
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if ("level" %in% columns) df$level <- numeric(0)
  df
}


#' One Empty Data Frame per Sheet
#' @return A named list of zero-row data frames.
ak_empty_sheets <- function() {
  lapply(ak_setup_columns(), ak_empty_frame)
}


#' Bring Imported Sheets Into the Builder's Shape
#'
#' Every builder sheet is present afterwards, so the tabs never test for a
#' missing sheet. An imported sheet keeps exactly the columns it had - only a
#' missing *required* column is added - so an imported workbook that is not
#' edited reads into the same specification as uploading it would. Optional
#' columns (`level`, `full_sample_base`, ...) are added by the tabs only when a
#' value is set. Sheets the builder does not edit are kept too, so validation
#' still reports them.
#'
#' @param sheets A named list of data frames, as read.
#' @return A named list of data frames.
ak_normalise_sheets <- function(sheets) {
  out <- sheets %||% list()
  columns <- ak_setup_columns()
  required <- list(
    analysis_variables = c("analysis_type", "analysis_var"),
    disaggregation_variables = c("raw_data_name", "new_name"),
    count_selections = "analysis_var",
    count_combinations = c("analysis_var", "choice_label"),
    count_exclusive_combinations = c("analysis_var", "choice_label"),
    exclude_choices = "choice_label",
    settings = c("setting", "value"),
    route_analysis = c("setting", "value")
  )

  for (nm in names(columns)) {
    df <- out[[nm]]
    if (is.null(df)) {
      out[[nm]] <- ak_empty_frame(columns[[nm]])
      next
    }
    df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
    for (col in setdiff(required[[nm]], names(df))) {
      df[[col]] <- if (col == "level") {
        rep(NA_real_, nrow(df))
      } else {
        rep(NA_character_, nrow(df))
      }
    }
    rownames(df) <- NULL
    out[[nm]] <- df
  }
  out
}


#' Is a Sheet Empty or Missing
#' @param df A data frame or `NULL`.
#' @return Logical.
ak_sheet_empty <- function(df) is.null(df) || nrow(df) == 0L


#' Has Anything Been Set Up Yet
#'
#' True as soon as any sheet carries a row: an analysis, a disaggregation, a
#' changed setting, a route block switched on.
#'
#' @param sheets The builder sheets.
#' @return Logical.
ak_setup_started <- function(sheets) {
  any(!vapply(sheets, ak_sheet_empty, logical(1)))
}


#' Wrap the Builder Sheets as a Workbook
#'
#' The same list read_loa_workbook() returns. Empty optional sheets are left
#' out, which is how the reader already treats them ("not requested"), so an
#' untouched tab can never change a default. `analysis_variables` is always
#' kept, because it is the required sheet and the validator should say so when
#' it is empty.
#'
#' Columns are passed as they are: an imported workbook that is not edited
#' produces exactly the specification uploading it would.
#'
#' @param sheets The builder sheets.
#' @param meta A list with `filename`, `format`, and optionally
#'   `unknown_sheets`, `ignored_sheets`, `duplicate_sheets` from an import.
#' @return A workbook list.
ak_setup_workbook <- function(sheets, meta = list()) {
  keep <- vapply(
    names(sheets),
    function(nm) nm == "analysis_variables" || !ak_sheet_empty(sheets[[nm]]),
    logical(1)
  )
  sheets <- sheets[keep]

  list(
    sheets = sheets,
    format = meta$format %||% "App",
    filename = meta$filename %||% "Set up in the app",
    sheet_names = names(sheets),
    unknown_sheets = meta$unknown_sheets %||% character(0),
    ignored_sheets = meta$ignored_sheets %||% character(0),
    duplicate_sheets = meta$duplicate_sheets %||% character(0)
  )
}


#' Write the Setup Out as a List of Analysis Workbook
#'
#' The inverse of read_loa_workbook(): a workbook made here reads back into the
#' same specification, so a setup can be kept, shared and reused next round.
#'
#' @param sheets The builder sheets.
#' @param path Where to write the `.xlsx`.
#' @return `path`, invisibly.
ak_write_setup <- function(sheets, path) {
  wb <- ak_setup_workbook(sheets)
  readme <- data.frame(
    `Analysis Kit - List of Analysis` = c(
      "Set up in the Analysis Kit app.",
      "Import it into the app to carry on, or upload it with run_analysis_locally().",
      "Raw dataset codes are used throughout; the disaggregation_variables rename applies to the output only.",
      "The full specification is in loa-schema.md in the analysiskit package."
    ),
    check.names = FALSE
  )
  out <- c(list(readme = readme), wb$sheets)
  openxlsx::write.xlsx(out, path, overwrite = TRUE)
  invisible(path)
}


# -----------------------------------------------------------------------------
# The dataset dictionary
# -----------------------------------------------------------------------------

#' Describe Every Question in a Dataset
#'
#' One row per question, so the builder can offer real variables rather than
#' asking for codes to be typed. A select_multiple question appears once, under
#' its parent, whether or not the export carries the concatenated parent
#' column; its choices are the suffixes of its child columns, which is exactly
#' what the combination sheets must match.
#'
#' The type is inferred from the export and is only a suggestion: a numeric
#' column with four distinct values may well be a coded select_one.
#'
#' @param data The dataset, with the label row as row 1 when `skip_label_row`.
#' @param sm_separator Separator between a select_multiple parent and choice.
#' @param skip_label_row Logical. Row 1 holds the question labels.
#' @param max_choices Above this many distinct text values a column is treated
#'   as free text rather than a select_one.
#' @return A data frame with `variable`, `label`, `type`, `n_choices`,
#'   `choices` (a list column), `answered` (share of rows with a value) and
#'   `columns` (a list column of the dataset columns it covers).
ak_dataset_dictionary <- function(data,
                                  sm_separator = "/",
                                  skip_label_row = TRUE,
                                  max_choices = 60L) {
  empty <- data.frame(
    variable = character(0), label = character(0), type = character(0),
    n_choices = integer(0), answered = numeric(0), stringsAsFactors = FALSE
  )
  empty$choices <- list()
  empty$columns <- list()
  if (is.null(data) || ncol(data) == 0L) return(empty)

  nms <- names(data)
  labels <- if (isTRUE(skip_label_row) && nrow(data) > 0L) {
    vapply(data[1L, , drop = FALSE], function(x) as.character(x)[1], character(1))
  } else {
    stats::setNames(rep(NA_character_, length(nms)), nms)
  }
  body <- if (isTRUE(skip_label_row) && nrow(data) > 0L) {
    data[-1L, , drop = FALSE]
  } else {
    data
  }
  n <- nrow(body)

  has_value <- function(x) !is.na(x) & trimws(as.character(x)) != ""

  sep <- sm_separator %||% "/"
  is_child <- if (nzchar(sep)) grepl(sep, nms, fixed = TRUE) else rep(FALSE, length(nms))
  parent_of <- ifelse(
    is_child,
    vapply(strsplit(nms, sep, fixed = TRUE), `[`, character(1), 1L),
    NA_character_
  )
  sm_parents <- unique(parent_of[is_child])

  rows <- list()
  seen <- character(0)
  for (i in seq_along(nms)) {
    nm <- nms[i]
    variable <- if (is_child[i]) parent_of[i] else nm
    if (variable %in% seen) next
    seen <- c(seen, variable)

    if (variable %in% sm_parents) {
      kids <- nms[is_child & parent_of == variable]
      choices <- substring(kids, nchar(variable) + nchar(sep) + 1L)
      answered <- if (n == 0L) {
        0
      } else {
        mean(Reduce(`|`, lapply(body[kids], has_value)))
      }
      label <- if (variable %in% nms) labels[[variable]] else NA_character_
      rows[[length(rows) + 1L]] <- list(
        variable = variable, label = label, type = "select_multiple",
        choices = choices, answered = answered,
        columns = c(intersect(variable, nms), kids)
      )
      next
    }

    x <- body[[nm]]
    present <- has_value(x)
    values <- as.character(x[present])
    numeric_values <- suppressWarnings(as.numeric(values))
    type <- if (length(values) == 0L) {
      "empty"
    } else if (inherits(x, c("Date", "POSIXt"))) {
      "date"
    } else if (all(!is.na(numeric_values))) {
      "numeric"
    } else if (length(unique(values)) <= max_choices) {
      "select_one"
    } else {
      "text"
    }
    choices <- if (type == "select_one") sort(unique(values)) else character(0)

    rows[[length(rows) + 1L]] <- list(
      variable = variable, label = labels[[nm]], type = type,
      choices = choices, answered = if (n == 0L) 0 else mean(present),
      columns = nm
    )
  }

  out <- data.frame(
    variable = vapply(rows, `[[`, character(1), "variable"),
    label = vapply(rows, function(r) as.character(r$label %||% NA_character_)[1], character(1)),
    type = vapply(rows, `[[`, character(1), "type"),
    n_choices = vapply(rows, function(r) length(r$choices), integer(1)),
    answered = vapply(rows, `[[`, numeric(1), "answered"),
    stringsAsFactors = FALSE
  )
  out$choices <- lapply(rows, `[[`, "choices")
  out$columns <- lapply(rows, `[[`, "columns")
  # A label row that repeats the column name says nothing.
  out$label[!is.na(out$label) & out$label == out$variable] <- NA_character_
  out
}


#' The Analysis Type a Question Type Suggests
#' @param type Inferred types from ak_dataset_dictionary().
#' @return A character vector of analysis types; `NA` where none fits.
ak_suggest_analysis_type <- function(type) {
  unname(c(
    select_one = "prop_select_one",
    select_multiple = "prop_select_multiple",
    numeric = "mean",
    text = NA_character_,
    date = NA_character_,
    empty = NA_character_
  )[type])
}


#' Variables With Their Labels, for a Picker
#' @param dictionary From ak_dataset_dictionary().
#' @param types Keep only these types; `NULL` keeps all.
#' @return A named character vector: names are the display text, values the
#'   raw codes, as selectInput() takes them.
ak_variable_choices <- function(dictionary, types = NULL) {
  if (is.null(dictionary) || nrow(dictionary) == 0L) return(character(0))
  d <- dictionary
  if (!is.null(types)) d <- d[d$type %in% types, , drop = FALSE]
  shown <- ifelse(
    is.na(d$label) | d$label == "",
    d$variable,
    paste0(d$variable, " - ", ak_truncate(d$label, 70L))
  )
  stats::setNames(d$variable, shown)
}


#' Shorten Text for Display
#' @param x Character vector.
#' @param width Maximum characters.
#' @return Character vector.
ak_truncate <- function(x, width = 60L) {
  x <- as.character(x)
  long <- !is.na(x) & nchar(x) > width
  x[long] <- paste0(substr(x[long], 1L, width - 1L), "…")
  x
}


#' A Readable Output Name From a Question Label
#'
#' The suggested `new_name` for a disaggregation: the first few words of the
#' label, as an R-safe name. The analyst is expected to edit it.
#'
#' @param variable Raw code.
#' @param label Question label, may be `NA`.
#' @return A single string.
ak_suggest_new_name <- function(variable, label) {
  if (is.na(label) || !nzchar(trimws(label))) return(paste0(variable, "_group"))
  words <- strsplit(gsub("[^A-Za-z0-9 ]", " ", label), "\\s+")[[1]]
  words <- words[nzchar(words)]
  stop_words <- c("what", "is", "are", "your", "you", "the", "a", "an", "of",
                  "do", "did", "in", "which", "how", "to")
  keep <- words[!tolower(words) %in% stop_words]
  if (length(keep) == 0L) keep <- words
  name <- paste(utils::head(tools::toTitleCase(tolower(keep)), 4L), collapse = "_")
  if (!nzchar(name)) paste0(variable, "_group") else name
}


# -----------------------------------------------------------------------------
# Settings
# -----------------------------------------------------------------------------

#' Default Value of Every Setting
#'
#' Read from the pipeline's own formals, so the app never restates a default
#' the package owns. The few keys the pipeline does not take (the exporter's
#' and the route analysis's) carry the defaults documented in loa-schema.md.
#'
#' @return A named list, one element per setting in loa_settings_schema().
ak_settings_defaults <- function() {
  schema <- analysiskit::loa_settings_schema()
  formals_ <- formals(analysiskit::run_group_analysis_pipeline)

  fixed <- list(
    include_dataset = FALSE,
    transit_country = FALSE,
    places_stop = FALSE,
    dangerous_location = FALSE,
    route_gender_var = "Q27",
    route_top_countries = 10,
    route_top_locations = 4,
    route_full_sample_base = TRUE
  )
  labels <- eval(formals_$count_selections_labels)
  label_keys <- c(
    count_selections_label_none = 1L,
    count_selections_label_one = 2L,
    count_selections_label_many = 3L
  )

  out <- lapply(seq_len(nrow(schema)), function(i) {
    key <- schema$setting[i]
    if (key %in% names(fixed)) return(fixed[[key]])
    if (key %in% names(label_keys)) return(labels[[label_keys[[key]]]])
    value <- formals_[[schema$arg[i]]]
    if (is.null(value) || identical(value, quote(expr = ))) return(NULL)
    value <- eval(value)
    # An enum is written in the formals as its choices; the first one is the
    # default, as match.arg() would pick it.
    if (identical(schema$type[i], "enum")) value <- value[[1]]
    if (length(value) == 1L && is.na(value)) return(NULL)
    value
  })
  stats::setNames(out, schema$setting)
}


#' Read One Setting From a Settings Sheet
#'
#' @param sheet A `setting` / `value` data frame.
#' @param key The setting name.
#' @return The value as text, unwrapped if it was quoted, or `NULL` when the
#'   key is absent or blank.
ak_setting_text <- function(sheet, key) {
  if (ak_sheet_empty(sheet)) return(NULL)
  hit <- which(trimws(sheet$setting) == key)
  if (length(hit) == 0L) return(NULL)
  value <- sheet$value[[hit[1]]]
  if (is.na(value) || !nzchar(trimws(value))) return(NULL)
  value <- as.character(value)
  if (nchar(value) >= 2L && startsWith(value, "\"") && endsWith(value, "\"")) {
    value <- substr(value, 2L, nchar(value) - 1L)
  } else {
    value <- trimws(value)
  }
  value
}


#' Read One Setting as Its Type
#'
#' @param sheet A `setting` / `value` data frame.
#' @param key The setting name.
#' @param type `lgl`, `num`, `chr`, `chr[]` or `enum`.
#' @param default Returned when the key is absent.
#' @return The typed value.
ak_setting_value <- function(sheet, key, type, default = NULL) {
  text <- ak_setting_text(sheet, key)
  if (is.null(text)) return(default)
  switch(
    type,
    lgl = toupper(text) %in% c("TRUE", "T", "YES", "1"),
    num = suppressWarnings(as.numeric(text)),
    `chr[]` = trimws(strsplit(text, ",", fixed = TRUE)[[1]]),
    text
  )
}


#' Write a Value as a Settings Cell
#'
#' Text with a leading or trailing space is wrapped in double quotes, which
#' the reader unwraps: a spreadsheet would otherwise drop the space.
#'
#' @param value The value.
#' @param type The setting type.
#' @return A single string, or `NA` when the value is empty.
ak_format_setting <- function(value, type) {
  if (is.null(value) || length(value) == 0L) return(NA_character_)
  if (type == "chr[]") {
    value <- value[!is.na(value) & nzchar(value)]
    return(if (length(value) == 0L) NA_character_ else paste(value, collapse = ","))
  }
  if (length(value) == 1L && is.na(value)) return(NA_character_)
  if (type == "lgl") return(if (isTRUE(value)) "TRUE" else "FALSE")
  if (type == "num") return(format(value, scientific = FALSE, trim = TRUE))
  value <- as.character(value)
  if (!nzchar(value)) return(NA_character_)
  if (value != trimws(value)) value <- paste0("\"", value, "\"")
  value
}


#' Set or Clear One Setting
#'
#' A value equal to the default, or blank, removes the row: "blank means the
#' default", exactly as in the template, so the sheet only ever records what
#' the analyst changed.
#'
#' @param sheet A `setting` / `value` data frame.
#' @param key The setting name.
#' @param value The new value.
#' @param type The setting type.
#' @param default The default value.
#' @return The updated sheet.
ak_set_setting <- function(sheet, key, value, type, default = NULL) {
  if (is.null(sheet)) sheet <- ak_empty_frame(c("setting", "value"))
  cell <- ak_format_setting(value, type)
  is_default <- is.na(cell) ||
    (!is.null(default) && identical(cell, ak_format_setting(default, type)))

  hit <- which(trimws(sheet$setting) == key)
  if (is_default) {
    if (length(hit) > 0L) sheet <- sheet[-hit, , drop = FALSE]
  } else if (length(hit) > 0L) {
    sheet$value[hit[1]] <- cell
    if (length(hit) > 1L) sheet <- sheet[-hit[-1], , drop = FALSE]
  } else {
    row <- sheet[0, , drop = FALSE]
    row[1, ] <- NA
    row$setting <- key
    row$value <- cell
    sheet <- rbind(sheet, row)
  }
  rownames(sheet) <- NULL
  sheet
}


#' How the Settings Tab Groups the Keys
#'
#' The grouping of loa-schema.md section 7.1. A key the package adds later and
#' this map does not know lands in "Other", so it is still settable.
#'
#' @param keys Setting names.
#' @return A character vector of group names, one per key.
ak_setting_group <- function(keys) {
  groups <- list(
    "Data and design" = c(
      "sm_separator", "skip_label_row", "blank_to_na", "prepare_sm",
      "sm_child_style", "recreate_sm_parents", "weight_column", "strata_column"
    ),
    "Estimation" = c(
      "engine", "fallback_level", "min_group_n", "keep_missing_groups",
      "slim_design", "lonely_psu", "exclude_ignore_case"
    ),
    "Output shape" = c(
      "value_columns", "extra_columns", "missing_group_label", "use_group_prefix",
      "summary_value_label", "drop_empty_prop_rows", "add_analysis_type_label",
      "label_choices"
    ),
    "Results workbook" = "include_dataset"
  )
  vapply(keys, function(k) {
    for (g in names(groups)) if (k %in% groups[[g]]) return(g)
    if (startsWith(k, "count_selections")) return("Selection counts")
    if (startsWith(k, "count_exclusive") || k == "max_exclusive_choices") {
      return("Single choices")
    }
    if (startsWith(k, "count_combinations") || k == "max_combination_choices") {
      return("Choice combinations")
    }
    "Other"
  }, character(1), USE.NAMES = FALSE)
}


#' Settings Shown Only Under "Show Advanced"
#'
#' Keys the schema says to leave alone, or that only matter to the survey
#' engine's internals.
#' @return A character vector.
ak_advanced_settings <- function() {
  c("use_group_prefix", "slim_design", "lonely_psu", "fallback_level",
    "prepare_sm", "blank_to_na", "sm_child_style", "recreate_sm_parents",
    "skip_label_row", "drop_empty_prop_rows")
}


#' Help Text for a Setting
#' @param key The setting name.
#' @return A string, or `NULL`.
ak_setting_help <- function(key) {
  help <- c(
    sm_separator = "Separator between a multiple-choice question and its choice in the column names (ONA uses /).",
    skip_label_row = "Row 1 of the export holds the question labels.",
    blank_to_na = "Treat empty cells as missing.",
    prepare_sm = "Turn multiple-choice child columns into 0/1, keeping who was not asked.",
    sm_child_style = "How the child columns are written: choice labels, 0/1 dummies, or detect automatically.",
    recreate_sm_parents = "Rebuild the concatenated parent column. Needs the cleaningtools package and a column named uuid.",
    weight_column = "Column holding the survey weights. Leave empty for an unweighted analysis.",
    strata_column = "Strata column. Affects only confidence intervals.",
    engine = "auto sends only rows whose confidence interval is reported to the slower survey engine.",
    fallback_level = "Placeholder level for rows without one; the interval is blanked.",
    min_group_n = "Group levels with fewer interviews than this are set aside.",
    keep_missing_groups = "Report respondents with no value on a disaggregation as their own column.",
    slim_design = "Give the survey design only the columns it needs.",
    lonely_psu = "How the survey package treats a stratum with one PSU.",
    exclude_ignore_case = "Match excluded choices regardless of upper/lower case.",
    value_columns = "Columns written per group. Add n_w, n_w_total when weighting; stat_low, stat_upp for confidence intervals.",
    extra_columns = "Metadata columns of the analysis list (such as sector) to carry into the output.",
    missing_group_label = "Column heading for respondents missing on a disaggregation.",
    use_group_prefix = "Leave on. Off can make two disaggregations' columns collide.",
    summary_value_label = "Text written as the answer for mean, median and ratio rows.",
    drop_empty_prop_rows = "Drop the empty placeholder row proportions produce.",
    add_analysis_type_label = "Add a readable analysis-type column.",
    label_choices = "Relabel multiple-choice answers with their labels.",
    include_dataset = "Adds the dataset to the results workbook. It is personal data: handle and share the workbook as such.",
    transit_country = "Q47-Q54 pooled, with Q72 and QN8 analysed overall and within each country.",
    places_stop = "Five slots of country / place / reasons (Q56-Q70).",
    dangerous_location = "Five slots of country / place / incidents / perpetrators (Q98-Q117).",
    route_gender_var = "The gender variable every route table is broken down by, alongside the disaggregations.",
    route_top_countries = "Countries shown as columns, most named first. 0 shows every country.",
    route_top_locations = "Places shown per country, most named first.",
    route_full_sample_base = "Report the questions everyone is asked (country rankings, Q72) out of the full sample."
  )
  if (key %in% names(help)) help[[key]] else NULL
}


#' Is Each Route Block's Data in the Dataset
#'
#' A block whose questions are missing is skipped at run time with a warning;
#' this says so before the run.
#'
#' @param columns Dataset column names.
#' @param sm_separator Separator for select_multiple children.
#' @return A data frame with `block`, `title`, `found`, `total` and `missing`
#'   (a comma-separated list of absent codes).
ak_route_coverage <- function(columns, sm_separator = "/") {
  blocks <- analysiskit:::ck_route_blocks()
  present <- function(code) {
    code %in% columns || any(startsWith(columns, paste0(code, sm_separator)))
  }
  rows <- lapply(names(blocks), function(nm) {
    b <- blocks[[nm]]
    codes <- unique(c(unlist(b$slots, use.names = FALSE), b$respondent_follow_ups))
    ok <- vapply(codes, present, logical(1))
    data.frame(
      block = nm,
      title = b$title,
      found = sum(ok),
      total = length(codes),
      missing = paste(codes[!ok], collapse = ", "),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
