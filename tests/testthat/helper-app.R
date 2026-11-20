# Helpers for the tests that drive app.R through shiny::testServer().

# testthat runs with the working directory set to tests/testthat, but the app
# directory is the project root. Walk up rather than hard-coding "../..", so the
# tests still find it if they are run from somewhere else.
find_app_dir <- function(start = ".", levels = 4L) {
  path <- normalizePath(start, mustWork = FALSE)
  for (i in seq_len(levels)) {
    if (file.exists(file.path(path, "app.R"))) return(path)
    path <- dirname(path)
  }
  NA_character_
}

app_dir <- find_app_dir()

skip_without_app <- function() {
  skip_if_not_installed("shiny")
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  skip_if(is.na(app_dir), "app.R not found")
}

# A renderUI output arrives as a list of html plus dependencies, so flatten it
# before matching against the text.
ui_text <- function(x) paste(as.character(unlist(x)), collapse = " ")

# shinyDirButton reports its choice as a root name plus the path components
# below it; this is the shape parseDirPath() reads back.
#
# Home is stripped as a literal prefix, not a regex: on Windows outside RStudio
# path.expand("~") is "C:\\Users\\name", whose backslashes a regex reads as
# escapes, so the prefix never matched and Home was joined on twice.
pick_folder <- function(path) {
  home <- normalizePath(path.expand("~"), winslash = "/", mustWork = FALSE)
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  relative <- if (startsWith(path, home)) {
    sub("^/", "", substring(path, nchar(home) + 1L))
  } else {
    path
  }
  list(root = "Home", path = as.list(c("", strsplit(relative, "/", fixed = TRUE)[[1]])))
}

# One spelling of a folder, so two paths to the same place compare equal
# whichever separators Windows handed back.
as_dir <- function(path) normalizePath(path, winslash = "/", mustWork = FALSE)

# A fresh, writable folder under Home, so the picker has something real to
# point at and each test gets its own output directory.
#
# Kept inside one dotted folder rather than scattered across Home: the app is
# run locally, so Home is what the folder picker shows its user, and a test run
# should not leave a dozen directories sitting in it.
test_destination_root <- function() {
  file.path(path.expand("~"), ".analysiskit-tests")
}

temp_destination <- function() {
  path <- file.path(
    test_destination_root(), paste0("run_", as.integer(runif(1, 1, 1e9)))
  )
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

withr::defer(unlink(test_destination_root(), recursive = TRUE), teardown_env())

# fileInput hands the server a one-row data frame, so the fixtures must too.
upload <- function(path, name = basename(path)) {
  data.frame(
    name = name,
    size = file.size(path),
    type = "",
    datapath = path,
    stringsAsFactors = FALSE
  )
}

dataset_file <- function(dataset = fixture_dataset()) {
  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(dataset, path)
  path
}

loa_file <- function(sheets = fixture_sheets()) {
  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(sheets, path)
  path
}
