# Orthogonally Model 1

Linear regression model of R_v on R\_{v-1},...,R\_{v-p}. (p=nrevs)

## Usage

``` r
orthogonallyModel1(revisions.view, nrevs = 1, na.zero = FALSE)
```

## Arguments

- revisions.view:

  mts object. Vertical or diagonal view of the
  [`get_revisions()`](https://rjdverse.github.io/rjd3revisions/reference/get_revisions.md)
  output

- nrevs:

  Integer. Number of lags to consider.

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
revisions <- get_revisions(vintages, gap = 1)
orthogonallyModel1(revisions[["diagonal_view"]])
}
```
