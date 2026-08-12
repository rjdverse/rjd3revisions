# Efficiency Model 2

Linear regression model of R_v on R\_{v-1}

## Usage

``` r
efficiencyModel2(vintages.view, gap = 1, na.zero = FALSE)
```

## Arguments

- vintages.view:

  mts object. Vertical or diagonal view of the
  [`create_vintages()`](https://rjdverse.github.io/rjd3revisions/reference/create_vintages.md)
  output

- gap:

  Integer. Gap to consider between each vintages. Default is 1 which
  means that revisions are calculated and tested for each vintages
  consecutively.

- na.zero:

  Boolean whether missing values should be considered as 0 or rather as
  data not (yet) available (the default).

## See also

[`revision_analysis()`](https://rjdverse.github.io/rjd3revisions/reference/revision_analysis.md),
[`render_report()`](https://rjdverse.github.io/rjd3revisions/reference/render_report.md)

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

## Create vintage and test
vintages <- create_vintages(df_long, periodicity = 4L)
efficiencyModel2(vintages[["diagonal_view"]])
}
```
