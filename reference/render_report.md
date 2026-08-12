# Generate report on Revision Analysis

Generate report on Revision Analysis

## Usage

``` r
render_report(
  rslt,
  output_file,
  output_dir,
  output_format = c("html_document", "pdf_document", "word_document"),
  plot_revisions = FALSE,
  open_report = TRUE,
  ...
)
```

## Arguments

- rslt:

  an object of class `"rjd3rev_rslts"` which is the output of the
  function
  [`revision_analysis()`](https://rjdverse.github.io/rjd3revisions/reference/revision_analysis.md)

- output_file:

  path or name of the output file containing the report

- output_dir:

  path of the dir containing the output file (Optional)

- output_format:

  either an HTML document (default), a PDF document or a Word document

- plot_revisions:

  Boolean. Default is FALSE meaning that a plot with the revisions will
  not be added to the report.

- open_report:

  Boolean. Default is TRUE meaning that the report will open
  automatically after being generated.

- ...:

  Arguments to be passed to
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html),
  for example:

  - `output_options` List of output options that can override the
    options specified in metadata

  - `...`

## See also

[`revision_analysis()`](https://rjdverse.github.io/rjd3revisions/reference/revision_analysis.md)
to create the input object

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)

## Simulated data
df_long <- simulate_long(
    n_period = 10L * 4L,
    n_revision = 5L,
    periodicity = 4L,
    start_period = as.Date("2010-01-01")
)

## Make analysis and generate the report

vintages <- create_vintages(df_long, periodicity = 4L, type = "long")
rslt <- revision_analysis(vintages, view = "diagonal")

if (FALSE) { # \dontrun{
render_report(
    rslt,
    output_file = "my_report",
    output_dir = "C:/Users/xxx",
    output_format = "pdf_document",
    plot_revisions = TRUE
)
} # }
}
```
