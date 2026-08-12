# Create vintages table from CSV or TXT files

Create vintages table from CSV or TXT files

## Usage

``` r
create_vintages_from_csv(
  file,
  type = c("long", "horizontal", "vertical"),
  periodicity,
  date_format = "%Y-%m-%d",
  ...
)
```

## Arguments

- file:

  character containing the name of the file which the data are to be
  read from.

- type:

  character specifying the type of representation of the input between
  `"long"`, `"horizontal"` and `"vertical"` approach.

- periodicity:

  Integer. Periodicity of the time period (12, 4 or 1 for resp. monthly,
  quarterly or annual data)

- date_format:

  `character` string corresponding to the format used in the input
  data.frame for the revision dates.

- ...:

  Arguments to be passed to
  [`read.csv()`](https://rdrr.io/r/utils/read.table.html), for example:

  - `sep` the field separator character

  - `dec` the character used in the file for decimal points.

  - `row.names` a vector of row names

  - `skip` integer, the number of lines of the data file to skip before
    beginning to read data.

  - `...`

## Value

an object of class `rjd3rev_vintages`

## See also

[`create_vintages_from_xlsx()`](https://rjdverse.github.io/rjd3revisions/reference/create_vintages_from_xlsx.md),
[`create_vintages()`](https://rjdverse.github.io/rjd3revisions/reference/create_vintages.md)
which this function wraps.

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
if (FALSE) { # \dontrun{
file_name <- "myinput.csv"
vintages <- create_vintages_from_csv(
    file = file_name,
    type = "vertical",
    periodicity = 12,
    date_format = "%Y-%m-%d",
    sep = ";"
)
} # }
}
```
