# Check horizontal format

Check horizontal format

## Usage

``` r
check_horizontal(x, ...)

# S3 method for class 'data.frame'
check_horizontal(x, ...)

# S3 method for class 'matrix'
check_horizontal(x, date_format = "%Y-%m-%d", ...)

# Default S3 method
check_horizontal(x, ...)
```

## Arguments

- x:

  a formatted `data.frame` containing the input in the horizontal format

- ...:

  Arguments to be passed to `check_horizontal` according to the class of
  the object `x`

- date_format:

  `character` string corresponding to the format used in the input
  data.frame for the revision dates.

## Value

the same input but with date formatted

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)

long_format <- rjd3revisions:::simulate_long(
    start_period = as.Date("2020-01-01"),
    n_period = 24,
    n_revision = 6,
    periodicity = 12L
)
horizontal_format <- rjd3revisions:::from_long_to_horizontal(long_format)
check_horizontal(horizontal_format)
}
```
