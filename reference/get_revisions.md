# Calculate revisions from vintages

Calculate revisions from vintages

## Usage

``` r
get_revisions(vintages, gap = 1)
```

## Arguments

- vintages:

  an object of class `rjd3rev_vintages`

- gap:

  Integer. Gap to consider between each vintages to calculate revision.
  Default is 1 which means that revisions are calculated for each
  vintages consecutively.

## Value

an object of class `rjd3rev_revisions` which contains the three
different views of revisions

## See also

[`create_vintages()`](https://rjdverse.github.io/rjd3revisions/reference/create_vintages.md)

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
df <- data.frame(rev_date = c(rep("2022-07-31",4), rep("2022-08-31",4),
                            rep("2022-09-30",4), rep("2022-10-31",4),
                            rep("2022-11-30",4), rep("2022-12-31",4),
                            rep("2023-01-31",4), rep("2023-02-28",4)),
                 time_period = c(rep(c("2022Q1","2022Q2","2022Q3","2022Q4"),8)),
                 obs_values = c(.8,.2,NA,NA, .8,.1,NA,NA,
                                .7,.1,NA,NA, .7,.2,.5,NA,
                                .7,.2,.5,NA, .7,.3,.7,NA,
                                .7,.2,.7,.4, .7,.3,.7,.3))
vintages <- create_vintages(df, periodicity = 4)
revisions <- get_revisions(vintages, gap = 1)
}
```
