# rjd3revisions

## Installation

To get the current stable version (from the latest release):

``` r
# install.packages("remotes")
remotes::install_github("rjdemetra/rjd3toolkit@*release")
remotes::install_github("rjdemetra/rjd3revisions@*release")
```

To get the current development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("rjdemetra/rjd3revisions")
```

## Usage

First you need to get your input data set as a data.frame in R in a specific format as below.
| rev_date    | time_period | obs_values  |
| ----------- | ----------- | ----------- |
| 2022-07-31  | 2022Q1      | 0.8         |
| 2022-07-31  | 2022Q2      | 0.2         |
| 2022-07-31  | 2022Q3      | NA          |
| 2022-07-31  | 2022Q4      | NA          |
| 2022-08-31  | 2022Q1      | 0.8         |
| ...         | ...         | ...         |

``` r
# Example
df <- data.frame(
    rev_date = c(
        rep("2022-07-31", 4), rep("2022-08-31", 4),
        rep("2022-09-30", 4), rep("2022-10-31", 4),
        rep("2022-11-30", 4), rep("2022-12-31", 4),
        rep("2023-01-31", 4), rep("2023-02-28", 4)
    ),
    time_period = c(rep(c("2022Q1", "2022Q2", "2022Q3", "2022Q4"), 8)),
    obs_values = c(
        .8, .2, NA, NA, .8, .1, NA, NA,
        .7, .1, NA, NA, .7, .2, .5, NA,
        .7, .2, .5, NA, .7, .3, .7, NA,
        .7, .2, .7, .4, .7, .3, .7, .3
    )
)
```

Then you can create your vintages, inspect revisions if you want and make the analysis 
``` r
library("rjd3revisions")

vintages <- create_vintages(df, periodicity = 4)
# revisions<-get_revisions(vintages, gap = 2)
rslt <- revision_analysis(vintages, gap = 1, view = "diagonal", n.releases = 3)
```

Finally to create a report and get a summary of the results, you can use
``` r
get_report(rslt)

summary(rslt)
print(rslt)
plot(rslt)
```

## Additional information
This README.md file gives you a quick example of how to proceed. Consult the vignette (with `browseVignettes("rjd3revisions")`) and the documentation of each function (for example: `?create_vintages`, `?revision_analysis`) to see the current possibilities of the tool.
