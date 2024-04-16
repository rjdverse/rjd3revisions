RAREVISIONS<-'JD3_RARevisions'

.get_revisions_view <- function(vt, gap) {
    n<-dim(vt)[2]

    idx1<- (gap+1):n
    idx0<-1:(n-gap)

    rev<-vt[, idx1, drop=FALSE]-vt[, idx0, drop=FALSE]

    w<-sapply(colnames(vt), function(s) paste0("[", s, "]"))
    rw<-mapply(FUN = function(a, b) paste(a, b, sep = "-"), w[idx1], w[idx0])

    rev<-`colnames<-`(rev, rw)
    return(rev)
}

#' Calculate revisions from vintages
#'
#' @param vintages an object of class `"rjd3rev_vintages"`
#' @param gap Integer. Gap to consider between each vintages to calculate
#'            revision. Default is 1 which means that revisions are
#'            calculated for each vintages consecutively.
#'
#' @seealso `create_vintages()`
#'
#' @return a list with revisions calculated for the three vintage views
#'
#' @export
#'
#' @examples
#' df<-data.frame(rev_date = c(rep("2022-07-31",4), rep("2022-08-31",4),
#'                             rep("2022-09-30",4), rep("2022-10-31",4),
#'                             rep("2022-11-30",4), rep("2022-12-31",4),
#'                             rep("2023-01-31",4), rep("2023-02-28",4)),
#'                  time_period = c(rep(c("2022Q1","2022Q2","2022Q3","2022Q4"),8)),
#'                  obs_values = c(.8,.2,NA,NA, .8,.1,NA,NA,
#'                                 .7,.1,NA,NA, .7,.2,.5,NA,
#'                                 .7,.2,.5,NA, .7,.3,.7,NA,
#'                                 .7,.2,.7,.4, .7,.3,.7,.3))
#' vintages<-create_vintages(df, periodicity = 4)
#' revisions<-get_revisions(vintages, gap=1)
#'
get_revisions<-function(vintages, gap=1) {
  if (!class(vintages)=="rjd3rev_vintages") {
    warning("Wrong input. vintages must be an object of class 'rjd3rev_vintages'.")
    return(NULL)
  }

  vv<-.get_revisions_view(vintages$vertical_view, gap)
  hv<-`colnames<-`(t(vv), colnames(vintages$horizontal_view))
  dv<-.get_revisions_view(vintages$diagonal_view, gap)

  return(structure(list(
      vertical_view=vv,
      horizontal_view=hv,
      diagonal_view=dv),
      class = RAREVISIONS))
}

#' Plot function for objects of class "JD3_RARevisions"
#'
#' @param x an object of class "JD3_RARevisions"
#' @param view view to plot. By default, the vertical view is considered.
#' @param n_rev number of revision dates to consider. For the vertical and
#'   horizontal view, the lasts n_rev revisions are plotted. For the diagonal
#'   view, the revisions between the first n_rev releases are plotted. The
#'   maximum number of n_rev is 5.
#' @param \dots further arguments passed to ts.plot().
#' @export
#'
plot.JD3_RARevisions <- function(x, view = c("vertical", "horizontal", "diagonal"), n_rev = 2, ...) {

    view <- match.arg(view)
    if(view == "horizontal") view <- "vertical"

    rev <- x[[paste0(view,"_view")]]
    nc <- ncol(rev)
    n_rev <- min(n_rev, nc, 5)
    if(view == "vertical"){
        rev <- rev[, (nc-n_rev+1):nc]
    }else{
        rev <- rev[, 1:n_rev]
    }

    ts.plot(rev, gpars=list(xlab="", ylab="", col=c(1:nc), type="h", lwd=2, ...))
    legend("topleft", bty="n", lty=1, lwd=2, col=c(1:nc), legend=colnames(rev))
    title(main = "Revisions size")
}


#' Print function for objects of class "JD3_RARevisions"
#'
#' @param x an object of class "JD3_RARevisions".
#' @param view view to print. By default, an extract of all views are printed.
#' @param n_row number of (last) rows to subset. For the horizontal view,
#'   corresponds to the number of columns to subset.
#' @param n_col number of columns to subset. Can be either the last n columns
#'   (verical view), the last n rows (horizontal view) or the first n columns
#'   (diagonal view).
#' @param \dots further arguments passed to or from other methods.
#' @export
#'
print.JD3_RARevisions <- function(x,
                                  view = c("all", "vertical", "horizontal",
                                              "diagonal"),
                                  n_row = 12,
                                  n_col = 3,
                                  ...) {

    view <- match.arg(view)

    n_col_tot <- ncol(x$vertical_view)
    n_row_tot <- nrow(x$vertical_view)
    n_col <- min(n_col_tot, n_col)
    n_row <- min(n_row_tot, n_row)
    freq <- frequency(x$vertical_view)
    end_period <- end(x$vertical_view)

    if(view %in% c("all", "vertical")){
        cat("Vertical view (extract) :\n")
        print(ts(x$vertical_view[(n_row_tot-n_row+1):n_row_tot,
                                 (n_col_tot-n_col+1):n_col_tot],
                 frequency = freq,
                 end = end_period))
    }

    if(view %in% c("all", "horizontal")){
        cat("\nHorizontal view (extract):\n")
        print(x$horizontal_view[(n_col_tot-n_col+1):n_col_tot,
                                (n_row_tot-n_row+1):n_row_tot])
    }


    if(view %in% c("all", "diagonal")){
        cat("\nDiagonal view (extract):\n")
        print(ts(x$diagonal_view[1:n_row,
                                 1:n_col],
                 frequency = freq,
                 end = end_period))
    }
}


#' Summary function for objects of class "JD3_RARevisions"
#'
#' @param x an object of class "JD3_RARevisions".
#' @param \dots further arguments passed to or from other methods.
#' @export
#'
summary.JD3_RARevisions <- function(x, ...) {
    cat("Number of releases: ", ncol(x$vertical_view))
    cat("\nCovered period:")
    cat("\n \tFrom: ", start(x$vertical_view))
    cat("\n \tTo: ", end(x$vertical_view))
    cat("\nFrequency: ", freq)
}
