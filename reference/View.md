# View function for objects of class "rjd3rev_vintages"

Display the different view in a different panel to visualize the data in
a table / matrix format

## Usage

``` r
View(x, ...)

# Default S3 method
View(x, ...)

# S3 method for class 'rjd3rev_vintages'
View(x, type = c("all", "long", "horizontal", "vertical", "diagonal"), ...)
```

## Arguments

- x:

  an object of class "rjd3rev_vintages".

- ...:

  further arguments passed to the `View` method.

- type:

  type of view to display

## Details

Generate the view of the vintages in different format. With the type
argument, you can choose the view to display. You can choose between the
long, horizontal, vertical and diagonal view.
