# Check long format

Check long format

## Usage

``` r
check_long(x, date_format = "%Y-%m-%d")
```

## Arguments

- x:

  a formatted `data.frame` containing the input in the long format

- date_format:

  `character` string corresponding to the format used in the input
  data.frame for the revision dates.

## Value

the same input but with column and date formatted

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)

long_format <- rjd3revisions:::simulate_long(
    start_period = as.Date("2020-01-01"),
    n_period = 24,
    n_revision = 6,
    periodicity = 12L
)
check_long(long_format)
}
```
