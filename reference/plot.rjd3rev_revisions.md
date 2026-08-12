# Plot function for objects of class "rjd3rev_revisions"

Plot function for objects of class "rjd3rev_revisions"

## Usage

``` r
# S3 method for class 'rjd3rev_revisions'
plot(x, view = c("vertical", "diagonal"), n_rev = 2, ...)
```

## Arguments

- x:

  an object of class "rjd3rev_revisions"

- view:

  view to plot. By default, the vertical view is considered.

- n_rev:

  number of revision dates to consider. For the vertical view, the lasts
  n_rev revisions are plotted. For the diagonal view, the revisions
  between the first n_rev releases are plotted. The maximum number of
  n_rev is 5.

- ...:

  further arguments passed to ts.plot().
