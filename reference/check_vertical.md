# Check vertical format

Check vertical format

## Usage

``` r
check_vertical(x, ...)

# S3 method for class 'mts'
check_vertical(x, periodicity, date_format = "%Y-%m-%d", ...)

# S3 method for class 'data.frame'
check_vertical(x, ...)

# S3 method for class 'matrix'
check_vertical(x, periodicity, date_format = "%Y-%m-%d", ...)

# Default S3 method
check_vertical(x, ...)
```

## Arguments

- x:

  a formatted `data.frame` containing the input in the vertical format

- ...:

  Arguments to be passed to `check_vertical` according to the class of
  the object `x`

- periodicity:

  Integer. Periodicity of the time period (12, 4 or 1 for resp. monthly,
  quarterly or annual data)

- date_format:

  `character` string corresponding to the format used in the input
  data.frame for the revision dates.

## Value

the same input but in a ts object and with revision date formatted

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)

long_format <- rjd3revisions:::simulate_long(
    start_period = as.Date("2020-01-01"),
    n_period = 24,
    n_revision = 6,
    periodicity = 12L
)
vertical_format <- rjd3revisions:::from_long_to_vertical(long_format, periodicity = 12L)
check_vertical(vertical_format)
}
```
