# Vector error correction model (VECM)

Can lead to a better understanding of the nature of any nonstationary
process among the different component series.

## Usage

``` r
vecm(
  vintages.view,
  lag = 2,
  model = c("none", "cnt", "trend"),
  na.zero = FALSE
)
```

## Arguments

- vintages.view:

  mts object. Vertical or diagonal view of the
  [`create_vintages()`](https://rjdverse.github.io/rjd3revisions/reference/create_vintages.md)
  output

- lag:

  Number of lags

- model:

  Character. Must be "none" (the default), "cnt" or "trend".

- na.zero:

  Boolean whether missing values should be considered as 0 or rather as
  data not (yet) available (the default).

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
vecm(vintages[["diagonal_view"]])
}
```
