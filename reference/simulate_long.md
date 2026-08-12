# Simulate long datasets with revisions

Simulate long datasets with revisions

## Usage

``` r
simulate_long(
  n_period = 50,
  n_revision = 10,
  start_period = as.Date("2012-01-01"),
  periodicity = 12L
)
```

## Arguments

- n_period:

  Integer. Number of different time-period (length of the simulated
  series).

- n_revision:

  Integer. Number of different revision dates.

- start_period:

  Date. Start of the series.

- periodicity:

  Integer. Periodicity of the time period (12, 4 or 1 for resp. monthly,
  quarterly or annual data).

## Value

A dataset in the long format. See
[`create_vintages`](https://rjdverse.github.io/rjd3revisions/reference/create_vintages.md)
for more information about the different data formats.

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)

simulate_long(n_period = 100L, n_revision = 10L)
simulate_long(periodicity = 1L)
simulate_long(start_period = as.Date("2000-01-01"),
              n_period = 10L * 12L,
              periodicity = 12L)
simulate_long(periodicity = 4L, n_period = 5L * 4L)
}
```
