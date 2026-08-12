# Create vintages table from XLSX files

Create vintages table from XLSX files

## Usage

``` r
create_vintages_from_xlsx(
  file,
  type = c("long", "horizontal", "vertical"),
  periodicity,
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

- ...:

  Arguments to be passed to
  [`readxl::read_excel()`](https://readxl.tidyverse.org/reference/read_excel.html),
  for example:

  - `sheet` character containing the sheet to read

  - `range` A cell range to read from

  - `col_names` a boolean to use the first row as column names

  - `...`

## Value

an object of class `rjd3rev_vintages`

## See also

[`create_vintages_from_csv()`](https://rjdverse.github.io/rjd3revisions/reference/create_vintages_from_csv.md),
[`create_vintages()`](https://rjdverse.github.io/rjd3revisions/reference/create_vintages.md)
which this function wraps.

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
if (FALSE) { # \dontrun{
file_name <- "myinput.xlsx"
sheet_name <- "Sheet1"
vintages <- create_vintages_from_xlsx(
    file = file_name,
    type = "horizontal",
    periodicity = 12L,
    sheet = sheet_name
)
} # }
}
```
