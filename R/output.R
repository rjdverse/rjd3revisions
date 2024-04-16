#' Print function for objects of class "rjd3rev_revision_analysis"
#'
#' @param x an object of class "rjd3rev_revision_analysis"
#' @param \dots further arguments passed to or from other methods.
#' @export
#'
print.rjd3rev_revision_analysis <- function(x, ...) {

  print(list(call=x$call,
             descriptive_statistics=x$descriptive.statistics,
             parametric_analysis=x$summary, ...))
}

#' Summary function for objects of class "rjd3rev_revision_analysis"
#'
#' @param x an object of class "rjd3rev_revision_analysis"
#' @export
#'
summary.rjd3rev_revision_analysis<- function(x) {

  if (! requireNamespace("formattable", quietly = TRUE)) {
    warning("Please install 'formattable': install.packages('formattable') to get more visual output")
    return(x$summary)
  } else {
    format_font <- function(x) {
      formattable::formatter("span",
                             style = x ~ formattable::style(color = ifelse(substr(x, 1, 1) == "G", "green",
                                                                    ifelse(substr(x, 1, 1) == "U", "orange", "red")),
                                                            font.weight = ifelse(substr(x, 1, 1) == "S", "bold", "plain")))
    }
    nc<-ncol(x$summary)
    return(list(formattable::formattable(x$summary, apply(x$summary[, 2:nc, drop=FALSE], 2, format_font))))
  }
}


