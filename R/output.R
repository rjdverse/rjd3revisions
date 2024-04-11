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

#' Plot function for objects of class "rjd3rev_revision_analysis"
#'
#' @param x an object of class "rjd3rev_revision_analysis"
#' @param \dots further arguments passed to or from other methods.
#' @export
#'
plot.rjd3rev_revision_analysis <- function(x, ...) {
  rev<-x$revisions
  nc<-ncol(rev)
  if (nc>4) {
    rev<-rev[, 1:4]
    nc<-4
  }

  stats::ts.plot(rev, gpars=list(xlab="", ylab="", col=c(1:nc), type="h", lwd=2, ...))
  graphics::legend("topleft", bty="n", lty=1, lwd=2, col=c(1:nc), legend=colnames(rev))
  graphics::title(main = "Size of Revisions")
}
