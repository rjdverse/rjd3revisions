# get_revisions function -------------------------------------------------------

#' Calculate revisions from vintages
#'
#' @param vintages an object of class `rjd3rev_vintages`
#' @param gap Integer. Gap to consider between each vintages to calculate
#'            revision. Default is 1 which means that revisions are
#'            calculated for each vintages consecutively.
#'
#' @seealso `create_vintages()`
#'
#' @return an object of class `rjd3rev_revisions` which contains the three
#' different views of revisions
#'
#' @export
#'
#' @examples
#' df <- data.frame(rev_date = c(rep("2022-07-31",4), rep("2022-08-31",4),
#'                             rep("2022-09-30",4), rep("2022-10-31",4),
#'                             rep("2022-11-30",4), rep("2022-12-31",4),
#'                             rep("2023-01-31",4), rep("2023-02-28",4)),
#'                  time_period = c(rep(c("2022Q1","2022Q2","2022Q3","2022Q4"),8)),
#'                  obs_values = c(.8,.2,NA,NA, .8,.1,NA,NA,
#'                                 .7,.1,NA,NA, .7,.2,.5,NA,
#'                                 .7,.2,.5,NA, .7,.3,.7,NA,
#'                                 .7,.2,.7,.4, .7,.3,.7,.3))
#' vintages <- create_vintages(df, periodicity = 4)
#' revisions <- get_revisions(vintages, gap = 1)
#'
get_revisions <- function(vintages, gap = 1) {

    checkmate::assert_class(x = vintages, classes = "rjd3rev_vintages")
    checkmate::assert_count(x = gap, positive = TRUE, na.ok = FALSE, null.ok = FALSE, .var.name = "gap")

    dv <- get_revisions_view(vintages$diagonal_view, gap)
    vv <- get_revisions_view(vintages$vertical_view, gap)

    hv <- t(vv)
    colnames(hv) <- colnames(vintages$horizontal_view)

    return(structure(list(
        vertical_view = vv,
        horizontal_view = hv,
        diagonal_view = dv
    ), class = "rjd3rev_revisions"))
}


# compute revisions for a single view (vertical or horizontal)
get_revisions_view <- function(vt, gap) {
    n <- dim(vt)[2]

    idx1 <- (gap + 1):n
    idx0 <- 1:(n - gap)

    rev <- vt[, idx1, drop = FALSE] - vt[, idx0, drop = FALSE]

    w <- sapply(colnames(vt), function(s) paste0("[", s, "]"))
    rw <- mapply(FUN = function(a, b) paste(a, b, sep = "-"), w[idx1], w[idx0])

    colnames(rev) <- rw
    return(rev)
}


# Generic functions ------------------------------------------------------------

#' Plot function for objects of class "rjd3rev_revisions"
#'
#' @param x an object of class "rjd3rev_revisions"
#' @param view view to plot. By default, the vertical view is considered.
#' @param n_rev number of revision dates to consider. For the vertical view,
#' the lasts n_rev revisions are plotted. For the diagonal view, the revisions
#' between the first n_rev releases are plotted. The maximum number of n_rev is
#' 5.
#' @param \dots further arguments passed to ts.plot().
#' @exportS3Method plot rjd3rev_revisions
#' @export
#'
plot.rjd3rev_revisions <- function(x, view = c("vertical", "diagonal"), n_rev = 2, ...) {

    # Check type
    view <- match.arg(view)

    # Check counts
    checkmate::assert_count(x = n_rev, positive = TRUE, na.ok = FALSE, null.ok = FALSE)

    rev <- x[[paste0(view, "_view")]]
    nc <- ncol(rev)
    n_rev <- min(n_rev, nc, 5)
    if (view == "vertical") {
        rev <- rev[, (nc - n_rev + 1):nc, drop = FALSE]
    } else {
        rev <- rev[, 1:n_rev, drop = FALSE]
    }

    stats::ts.plot(rev, gpars = list(xlab = "", ylab = "", col = c(1:nc), type = "h", lwd = 2, ...))
    graphics::legend("topleft", bty = "n", lty = 1, lwd = 2, col = c(1:nc), legend = colnames(rev))
    graphics::title(main = "Revisions size")
}


#' Print function for objects of class `"rjd3rev_revisions"`
#'
#' @param x an object of class `"rjd3rev_revisions"`.
#' @param n_row number of last rows to display. For the horizontal view,
#'   corresponds to the number of columns.
#' @param n_col number of columns to display. Can be either the last n columns
#'   (verical view), the last n rows (horizontal view) or the first n columns
#'   (diagonal view).
#' @param \dots further arguments passed to the \code{\link{print}} function.
#' @exportS3Method print rjd3rev_revisions
#' @export
#'
print.rjd3rev_revisions <- function(x, n_row = 12, n_col = 3, ...) {

    # Check counts
    checkmate::assert_count(x = n_row, positive = TRUE, na.ok = FALSE, null.ok = FALSE, .var.name = "n_row")
    checkmate::assert_count(x = n_col, positive = TRUE, na.ok = FALSE, null.ok = FALSE, .var.name = "n_col")

    vv <- x$vertical_view
    n_col_tot <- ncol(vv)
    n_row_tot <- nrow(vv)
    n_col <- min(n_col_tot, n_col)
    n_row <- min(n_row_tot, n_row)
    freq <- stats::frequency(vv)
    end_period <- stats::end(vv)
    is_extract <- ifelse(n_col < n_col_tot || n_row < n_row_tot, TRUE, FALSE)

    extract_vv <- stats::ts(x$vertical_view[(n_row_tot - n_row + 1):n_row_tot, (n_col_tot - n_col + 1):n_col_tot],
                            frequency = freq,
                            end = end_period)
    extract_hv <- x$horizontal_view[(n_col_tot - n_col + 1):n_col_tot, (n_row_tot - n_row + 1):n_row_tot]
    extract_dv <- stats::ts(x$diagonal_view[(n_row_tot - n_row + 1):n_row_tot, 1:n_col],
                            frequency = freq,
                            end = end_period)

    print(list(extract = is_extract,
               vertical_view = extract_vv,
               horizontal_view = extract_hv,
               diagonal_view = extract_dv, ...))
}


#' Summary function for objects of class "rjd3rev_revisions"
#'
#' @param object an object of class "rjd3rev_revisions".
#' @param ... further arguments passed to or from other methods.
#' @exportS3Method summary rjd3rev_revisions
#' @export
#'
summary.rjd3rev_revisions <- function(object, ...) {
    x <- object
    vv <- x$vertical_view
    cat("Number of release revisions: ", ncol(vv))
    cat("\nCovered period:")
    cat("\n \tFrom: ", stats::start(vv))
    cat("\n \tTo: ", stats::end(vv))
    cat("\nFrequency: ", stats::frequency(vv))
}
