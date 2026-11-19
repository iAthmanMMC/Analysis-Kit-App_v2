# Analysis Kit App

Turns a 4Mi dataset and a **List of Analysis** workbook into an MMC-styled
results workbook. Upload two files, click one button.

## Open it

**Online:** [abu-analysiskit.share.connect.posit.cloud](https://abu-analysiskit.share.connect.posit.cloud/).
Nothing to install.

**Locally:** install the analysis package once, then start the app from
`analysiskitappv2.Rproj` in RStudio:

```r
pak::pkg_install("mixedmigrationcentre/analysiskit")   # once
shiny::runApp()
```

## Use it

1. **Upload the dataset** - a 4Mi export, `.csv` or `.xlsx`.
2. **Set up the analysis in the tabs** - no workbook to download, fill in and
   re-upload:
   - **Analysis variables** - pick questions from the dataset; the analysis
     type is suggested from the export. Add ratios, confidence levels,
     full-sample bases and metadata columns such as `sector`.
   - **Disaggregations** - the variables every analysis is broken down by,
     with the name each gets in the output.
   - **Multiple choice** - selection counts, choice combinations, single
     choices and excluded choices, ticked from the dataset's own labels.
   - **Settings** - every run-wide setting, starting at the package default.
   - **Route analysis** - the transit, places-stopped and dangerous-location
     blocks, each with a check that its questions are in the dataset.
3. **Read the Checks tab** - red must be fixed before you can run; amber is
   just worth reading. Problems also show on the tab that can fix them.
4. **Choose where to save** - locally, pick a folder. Online, the results come
   as a download.
5. **Run the analyses**, then download the results workbook.

**Reusing a setup:** *Download this setup* saves everything as a List of
Analysis workbook. Next round, *Import a List of Analysis* fills the tabs from
it (an older hand-filled workbook works too), ready to check or change. An
imported workbook left unchanged runs exactly as uploading it always did.

Tip: leave the confidence interval empty unless a row needs one. Those rows are
much slower, and the figures are the same either way.

## How it fits together

The analysis (reading, checks, the pipeline, the export and the template) lives
in the [analysiskit](https://github.com/mixedmigrationcentre/analysiskit)
package. This repository is only the Shiny interface and the deployment setup.

- **Changing the analysis?** Edit the package, push it to GitHub, then
  reinstall it here with `pak::pkg_install("mixedmigrationcentre/analysiskit")`.
- **Changing the interface?** `app.R` only lays out the page and wires the
  pieces. Each setup tab is a module in `R/mod_*.R`; the sheets they edit, the
  dataset dictionary and the settings defaults are in `R/setup_state.R`; the
  dataset, import, checks and run wiring is in `R/server_*.R`.
- **Deploying?** See [docs/deployment.md](docs/deployment.md).
