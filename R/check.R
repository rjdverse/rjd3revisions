
check_date_year <- function(x) {
    return(all(grepl(pattern = "^\\d{4}$", x = x)))
}

check_date_quarter <- function(x) {
    return(all(grepl(pattern = "^\\d{4}( ?[TtQq]?0?[1-4])?$", x = x)))
}

check_date_month <- function(x) {
    return(all(grepl(pattern = "^\\d{4}( ?[Mm]?(1[0-2]|0?[1-9]))?$", x = x)))
}

check_format_date <- function(x, date_format = "%Y-%m-%d") {
    for (format_n in date_format) {
        if (!(any(is.na(as.Date(x, format = format_n))))) {
            return(TRUE)
        }
    }
    return(FALSE)
}

periodicity <- function(x, date_format = "%Y-%m-%d") {

    # Check Date
    x_in_date <- assert_date(x = x, date_format = date_format)
    duration <- diff(sort(x_in_date))

    if (all(duration %in% 90:92)) {
        return(4L)
    } else if (all(duration %in% 28:31)) {
        return(12L)
    } else if (all(duration %in% 365:366)) {
        return(1L)
    } else {
        stop("The periodicity is not recognised.")
    }
}

assert_time_period <- function(x, date_format = "%Y-%m-%d") {

    checkmate::assert_atomic(unique(x), min.len = 2)
    if (!inherits(x = x, what = c("character", "integer", "Date", "POSIXt"))) {
        stop("The revdate column must be of type character, integer, Date or POSIXt.")
    }

    if (check_format_date(x = x, date_format = date_format)) {
        # Date au format ISO ou type date
        for (format_n in date_format) {
            if (!(any(is.na(as.Date(x, format = format_n))))) {
                return(as.Date(x, format = format_n))
            }
        }
    } else if (check_date_year(x)) {
        # Date au format année
        year <- x
        month <- 1
        return(as.Date(paste(year, month, "01", sep = "-")))

    } else if (check_date_month(x)) {
        # Date au format mois
        year <- regmatches(x, regexpr(pattern = "^\\d{4}", text = x))
        month <- regmatches(x, regexpr(pattern = "\\d{1,2}$", text = x))
        return(as.Date(paste(year, month, "01", sep = "-")))

    } else if (check_date_quarter(x)) {
        # Date au format trimestre
        year <- regmatches(x, regexpr("^\\d{4}", x))
        quarter <- regmatches(x, regexpr(pattern = "\\d{1,2}$", text = x))
        month <- 3 * as.integer(quarter) - 2
        return(as.Date(paste(year, month, "01", sep = "-")))

    } else {
        stop("Time periods not in a correct format. Examples of correct formats are 2023M1, 2023 M07 2023 Q1, 2023 m12, 2023q01, 2023 T2, 2023 or you can specify the format of your date with the argument `date_format`")
    }
}

assert_rev_date <- function(x, date_format = "%Y-%m-%d") {

    checkmate::assert_atomic(unique(x), min.len = 2)
    if (!inherits(x = x, what = c("character", "integer", "Date", "POSIXt"))) {
        stop("The revdate column must be of type character, integer, Date or POSIXt.")
    }
    if (check_format_date(x = x, date_format = date_format)) {
        # Date au format ISO ou type date
        for (format_n in date_format) {
            if (!(any(is.na(as.Date(x, format = format_n))))) {
                return(as.Date(x, format = format_n))
            }
        }
    } else {
        stop("Revisions date not in a correct format. You can specify the format of your date with the argument `date_format`")
    }
}


#' Check long format
#'
#' @param x a formatted \code{data.frame} containing the input in the long format
#' @param date_format \code{character} string corresponding to the format used in
#' the input data.frame for the revision dates.
#'
#' @return the same input but with column and date formatted
#' @export
#'
#' @examples
#'
#' long_format <- rjd3revisions:::simulate_long(
#'     start_period = as.Date("2020-01-01"),
#'     n_period = 24,
#'     n_revision = 6,
#'     periodicity = 12L
#' )
#' check_long(long_format)
#'
#' @rdname check_long
#'
check_long <- function(x, date_format = "%Y-%m-%d") {

    # Check input
    checkmate::assert_data_frame(x, ncols = 3L)
    checkmate::assert_numeric(x[, 3, drop = TRUE], .var.name = "The third column")

    rev_date <- assert_rev_date(x = x[, 1, drop = TRUE], date_format = date_format)
    time_period <- assert_time_period(x = x[, 2, drop = TRUE], date_format = date_format)

    # Long format
    long <- x

    colnames(long) <- c("revdate", "time", "obs_values")
    long$revdate <- rev_date
    long$time <- time_period
    long <- long[order(long$revdate, long$time), ]
    rownames(long) <- NULL

    return(long)
}

#' Check vertical format
#'
#' @param x a formatted \code{data.frame} containing the input in the vertical format
#' @param periodicity periodicity of the time period (12, 4 or 1 for resp.
#' monthly, quarterly or annual data)
#' @param date_format \code{character} string corresponding to the format used in
#' the input data.frame for the revision dates.
#' @param ... Arguments to be passed to `check_vertical` according to the class of the object `x`
#'
#' @return the same input but in a ts object and with revision date formatted
#' @export
#'
#' @examples
#'
#' long_format <- rjd3revisions:::simulate_long(
#'     start_period = as.Date("2020-01-01"),
#'     n_period = 24,
#'     n_revision = 6,
#'     periodicity = 12L
#' )
#' vertical_format <- rjd3revisions:::from_long_to_vertical(long_format, periodicity = 12L)
#' check_vertical(vertical_format)
#'
#' @rdname check_vertical
#'
check_vertical <- function(x, ...) {
    return(UseMethod("check_vertical", x))
}

#' @exportS3Method check_vertical mts
#' @method check_vertical mts
#'
#' @rdname check_vertical
#'
check_vertical.mts <- function(x,
                               periodicity,
                               date_format = "%Y-%m-%d",
                               ...) {
    # Check data type
    checkmate::assert_matrix(x, mode = "numeric")

    # Check frequency
    checkmate::assert_choice(x = stats::frequency(x), choices = c(1L, 4L, 12L))
    if (!missing(periodicity)) {
        # Check periodicity
        checkmate::assert_count(x = periodicity, positive = TRUE, na.ok = FALSE, null.ok = FALSE)
        checkmate::assert_choice(x = periodicity, choices = c(1L, 4L, 12L))
        checkmate::assert_set_equal(x = stats::frequency(x), y = periodicity)
    }

    # Vertical format
    vertical <- x
    colnames(vertical) <- as.character(assert_rev_date(x = colnames(vertical), date_format = date_format))

    return(vertical)
}

#' @exportS3Method check_vertical data.frame
#' @method check_vertical data.frame
#'
#' @rdname check_vertical
#'
check_vertical.data.frame <- function(x, ...) {
    return(UseMethod("check_vertical", as.matrix(x)))
}

#' @exportS3Method check_vertical matrix
#' @method check_vertical matrix
#'
#' @rdname check_vertical
#'
check_vertical.matrix <- function(
        x,
        periodicity,
        date_format = "%Y-%m-%d",
        ...) {

    # Check periodicity
    checkmate::assert_count(x = periodicity, positive = TRUE, na.ok = FALSE, null.ok = FALSE)
    checkmate::assert_choice(x = periodicity, choices = c(1L, 4L, 12L))

    # Check data type
    checkmate::assert_matrix(x, mode = "numeric")
    if (length(rownames(x)) == 0 || length(colnames(x)) == 0) {
        stop("Revisions dates or time periods are missing.")
    }

    # Vertical format
    vertical <- x

    # Check date periods
    real_time_period <- assert_time_period(x = rownames(vertical), date_format = date_format)

    start_year <- as.integer(format(x = min(real_time_period), format = "%Y"))
    start_month <- as.integer(format(x = min(real_time_period), format = "%m"))

    if (periodicity == 12L) {
        start <- c(start_year, start_month)
        theo_time_period <- seq.Date(
            from = min(real_time_period),
            by = "month",
            length.out = nrow(x)
        )
    } else if (periodicity == 4L) {
        start <- c(start_year, 1L + ((start_month - 1L) %/% 3L))
        theo_time_period <- seq.Date(
            from = min(real_time_period),
            by = "quarter",
            length.out = nrow(x)
        )
    } else if (periodicity == 1L) {
        start <- c(start_year, 1L + ((start_month - 1L) %/% 3L))
        theo_time_period <- seq.Date(
            from = min(real_time_period),
            by = "year",
            length.out = nrow(x)
        )
    }
    checkmate::assert_set_equal(x = real_time_period, y = theo_time_period)

    colnames(vertical) <- as.character(assert_rev_date(x = colnames(vertical), date_format = date_format))
    rownames(vertical) <- as.character(assert_time_period(x = rownames(vertical), date_format = date_format))

    vertical <- stats::ts(
        data = vertical[as.character(theo_time_period), ],
        start = start,
        frequency = periodicity
    )

    return(vertical)
}

#' @exportS3Method check_vertical default
#' @method check_vertical default
#'
#' @rdname check_vertical
#'
check_vertical.default <- function(x, ...) {
    stop("The function requires a matrix or a mts object!")
}

#' Check horizontal format
#'
#' @param x a formatted \code{data.frame} containing the input in the horizontal format
#' @param date_format \code{character} string corresponding to the format used in
#' the input data.frame for the revision dates.
#' @param ... Arguments to be passed to `check_horizontal` according to the class of the object `x`
#' @return the same input but with date formatted
#' @export
#'
#' @examples
#'
#' long_format <- rjd3revisions:::simulate_long(
#'     start_period = as.Date("2020-01-01"),
#'     n_period = 24,
#'     n_revision = 6,
#'     periodicity = 12L
#' )
#' horizontal_format <- rjd3revisions:::from_long_to_horizontal(long_format)
#' check_horizontal(horizontal_format)
#'
#' @rdname check_horizontal
#'
check_horizontal <- function(x, ...) {
    return(UseMethod("check_horizontal", x))
}

#' @exportS3Method check_horizontal data.frame
#' @method check_horizontal data.frame
#'
#' @rdname check_horizontal
#'
check_horizontal.data.frame <- function(x, ...) {
    return(UseMethod("check_horizontal", as.matrix(x)))
}

#' @exportS3Method check_horizontal matrix
#' @method check_horizontal matrix
#'
#' @rdname check_horizontal
#'
check_horizontal.matrix <- function(x, date_format = "%Y-%m-%d") {
    horizontal <- x
    colnames(horizontal) <- as.character(assert_time_period(x = colnames(horizontal), date_format = date_format))
    rownames(horizontal) <- as.character(assert_rev_date(x = rownames(horizontal), date_format = date_format))

    return(horizontal)
}

#' @exportS3Method check_horizontal default
#' @method check_horizontal default
#'
#' @rdname check_horizontal
#'
check_horizontal.default <- function(x, ...) {
    stop("The function requires a matrix or a mts object!")
}
