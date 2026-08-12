# Descriptive statistics

Descriptive statistics

## Usage

``` r
descriptive_statistics(revisions.view, rounding = 3)
```

## Arguments

- revisions.view:

  mts object. Vertical or diagonal view of the
  [`get_revisions()`](https://rjdverse.github.io/rjd3revisions/reference/get_revisions.md)
  output

- rounding:

  number of decimals to display

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

## Create vintage and get descriptive statistics of revisions
vintages <- create_vintages(df_long, periodicity = 4)
revisions <- get_revisions(vintages, gap = 1)
descriptive_statistics(revisions[["diagonal_view"]], rounding = 1)
}
```
