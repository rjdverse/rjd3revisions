
# Check functions --------------------------------------------------------------

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
check_long <- function(x, date_format = "%Y-%m-%d") {

    # Check input
    checkmate::assert_data_frame(x, ncols = 3L)
    checkmate::assert_numeric(x[, 3, drop = TRUE], .var.name = "The third column")

    rev_date <- convert_rev_date(x = x[, 1, drop = TRUE], date_format = date_format)
    time_period <- convert_time_period(x = x[, 2, drop = TRUE], date_format = date_format)

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
check_vertical <- function(x, ...) {
    return(UseMethod("check_vertical", x))
}

#' @exportS3Method check_vertical mts
check_vertical.mts <- function(
        x,
        periodicity,
        date_format = "%Y-%m-%d",
        ...
) {
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
    colnames(vertical) <- as.character(convert_rev_date(x = colnames(vertical), date_format = date_format))

    return(vertical)
}

#' @exportS3Method check_vertical data.frame
check_vertical.data.frame <- function(x, ...) {
    return(UseMethod("check_vertical", as.matrix(x)))
}

#' @exportS3Method check_vertical matrix
check_vertical.matrix <- function(
        x,
        periodicity,
        date_format = "%Y-%m-%d",
        ...
) {
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
    real_time_period <- convert_time_period(x = rownames(vertical), date_format = date_format)

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

    colnames(vertical) <- as.character(convert_rev_date(x = colnames(vertical), date_format = date_format))
    rownames(vertical) <- as.character(convert_time_period(x = rownames(vertical), date_format = date_format))

    vertical <- stats::ts(
        data = vertical[as.character(theo_time_period), ],
        start = start,
        frequency = periodicity
    )

    return(vertical)
}

#' @exportS3Method check_vertical default
check_vertical.default <- function(x, ...) {
    stop("The function requires a matrix or a mts object!")
}

#' Check horizontal format
#'
#' @param x a formatted \code{data.frame} containing the input in the horizontal format
#' @param date_format \code{character} string corresponding to the format used in
#' the input data.frame for the revision dates.
#'
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
check_horizontal <- function(x, ...) {
    return(UseMethod("check_horizontal", x))
}

#' @exportS3Method check_horizontal data.frame
check_horizontal.data.frame <- function(x, ...) {
    return(UseMethod("check_horizontal", as.matrix(x)))
}

#' @exportS3Method check_horizontal matrix
check_horizontal.matrix <- function(x, date_format = "%Y-%m-%d") {
    horizontal <- x
    colnames(horizontal) <- as.character(convert_time_period(x = colnames(horizontal), date_format = date_format))
    rownames(horizontal) <- as.character(convert_rev_date(x = rownames(horizontal), date_format = date_format))

    return(horizontal)
}

#' @exportS3Method check_horizontal default
check_horizontal.default <- function(x, ...) {
    stop("The function requires a matrix or a mts object!")
}


# Convert_functions ------------------------------------------------------------

convert_time_period <- function(x, date_format = "%Y-%m-%d") {
    check_quarter <- all(grepl(pattern = "^\\d{4}( ?[TtQq]?0?[1-4])?$", x = x))
    check_month <- all(grepl(pattern = "^\\d{4}( ?[Mm]?(1[0-2]|0?[1-9]))?$", x = x))
    check_date <- !(any(is.na(as.Date(x, format = date_format))))

    if (check_date) {
        return(as.Date(x, format = date_format))
    } else if (check_month) {
        year <- regmatches(x, regexpr(pattern = "^\\d{4}", text = x))
        month <- regmatches(x, regexpr(pattern = "\\d{1,2}$", text = x))
        return(as.Date(paste(year, month, "01", sep = "-")))
    } else if (check_quarter) {
        year <- regmatches(x, regexpr("^\\d{4}", x))
        quarter <- regmatches(x, regexpr(pattern = "\\d{1,2}$", text = x))
        month <- 3 * as.integer(quarter) - 2
        return(as.Date(paste(year, month, "01", sep = "-")))
    } else {
        stop("Time periods not in a correct format. Examples of correct formats are 2023M1, 2023 M07 2023 Q1, 2023 m12, 2023q01, 2023 T2, 2023 or you can specify the format of your date with the argument `format_date`")
    }
}

convert_rev_date <- function(x, date_format = "%Y-%m-%d") {

    checkmate::assert_atomic(unique(x), min.len = 2)
    if (!inherits(x = x, what = c("character", "integer", "Date", "POSIXt"))) {
        stop("The revdate column must be of type character, integer, Date or POSIXt.")
    }
    if (any(is.na(as.Date(x, format = date_format)))) {
        stop("Revisions date not in a correct format. You can specify the format of your date with the argument `format_date`")
    }
    return(as.Date(x, format = date_format))
}

from_long_to_vertical <- function(x, periodicity, date_format = "%Y-%m-%d") {
    # Check input
    x <- check_long(x = x, date_format = date_format)

    # Check periodicity
    checkmate::assert_count(x = periodicity, positive = TRUE, na.ok = FALSE, null.ok = FALSE)
    checkmate::assert_choice(x = periodicity, choices = c(1L, 4L, 12L))

    vertical <- stats::reshape(
        data = x,
        timevar = "revdate",
        idvar = "time",
        direction = "wide",
        varying = list(as.character(unique(x$revdate)))
    )

    time_periods <- vertical$time
    vertical <- as.matrix(vertical[, -1])
    rownames(vertical) <- as.character(time_periods)
    return(check_vertical(x = vertical, periodicity = periodicity, date_format = date_format))
}

from_long_to_horizontal <- function(x, date_format = "%Y-%m-%d") {
    # Check input
    x <- check_long(x = x, date_format = date_format)

    horizontal <- stats::reshape(
        data = x,
        timevar = "time",
        idvar = "revdate",
        direction = "wide",
        varying = list(as.character(unique(x$time)))
    )
    revdate <- horizontal$revdate
    horizontal <- as.matrix(horizontal[, -1])
    rownames(horizontal) <- as.character(revdate)
    return(horizontal)
}

from_long_to_diagonal <- function(x, periodicity, date_format = "%Y-%m-%d") {
    # Check input
    x <- check_long(x = x, date_format = date_format)

    # Check periodicity
    checkmate::assert_count(x = periodicity, positive = TRUE, na.ok = FALSE, null.ok = FALSE)
    checkmate::assert_choice(x = periodicity, choices = c(1L, 4L, 12L))

    horizontal <- from_long_to_horizontal(x = x)
    diagonal <- from_horizontal_to_diagonal(x = horizontal, periodicity = periodicity)
    return(diagonal)
}

from_vertical_to_long <- function(x, date_format = "%Y-%m-%d") {
    # Check input
    x <- check_vertical(x = x, date_format = date_format)

    vertical <- t(t(x))
    if (stats::frequency(x) == 12L) {
        time_period <- seq.Date(
            from = as.Date(paste0(stats::start(x)[1], "-", stats::start(x)[2], "-01")),
            by = "month",
            length.out = nrow(x)
        )
    } else if (stats::frequency(x) == 4L) {
        time_period <- seq.Date(
            from = as.Date(paste0(stats::start(x)[1], "-", 3 * stats::start(x)[2] - 2, "-01")),
            by = "quarter",
            length.out = nrow(x)
        )
    }
    rownames(vertical) <- as.character(time_period)

    long <- stats::reshape(
        data = data.frame(time = rownames(vertical), vertical, check.names = FALSE) ,
        direction = "long",
        varying = colnames(vertical),
        v.names = "obs_values",
        idvar = "time",
        ids = rownames(vertical),
        timevar = "revdate",
        times = colnames(vertical)
    )
    long <- long[, c("revdate", "time", "obs_values")]
    long$revdate <- convert_rev_date(long$revdate, date_format)
    long$time <- convert_time_period(long$time, date_format)
    long <- long[order(long$revdate, long$time), ]
    rownames(long) <- NULL

    return(long)
}

from_vertical_to_horizontal <- function(x, date_format = "%Y-%m-%d") {
    # Check input
    x <- check_vertical(x = x, date_format = date_format)

    horizontal <- t(x)
    if (stats::frequency(x) == 12L) {
        time_period <- seq.Date(
            from = as.Date(paste0(stats::start(x)[1], "-", stats::start(x)[2], "-01")),
            by = "month",
            length.out = nrow(x)
        )
    } else if (stats::frequency(x) == 4L) {
        time_period <- seq.Date(
            from = as.Date(paste0(stats::start(x)[1], "-", 3 * stats::start(x)[2] - 2, "-01")),
            by = "quarter",
            length.out = nrow(x)
        )
    }
    colnames(horizontal) <- as.character(time_period)
    return(horizontal)
}

from_vertical_to_diagonal <- function(x, date_format = "%Y-%m-%d") {
    # Check input
    x <- check_vertical(x, date_format)

    horizontal <- from_vertical_to_horizontal(x, date_format)
    diagonal <- from_horizontal_to_diagonal(x = horizontal, stats::frequency(x))
    return(diagonal)
}

from_horizontal_to_long <- function(x, date_format = "%Y-%m-%d") {

    horizontal <- check_horizontal(x, date_format = date_format)

    long <- stats::reshape(
        data = data.frame(revdate = rownames(horizontal), horizontal, check.names = FALSE) ,
        direction = "long",
        varying = colnames(horizontal),
        v.names = "obs_values",
        idvar = "revdate",
        ids = rownames(horizontal),
        timevar = "time",
        times = colnames(horizontal)
    )
    long <- long[, c("revdate", "time", "obs_values")]
    long$revdate <- convert_rev_date(long$revdate, date_format)
    long$time <- convert_time_period(long$time, date_format)
    long <- long[order(long$revdate, long$time), ]
    rownames(long) <- NULL

    return(long)
}

from_horizontal_to_vertical <- function(x, periodicity, date_format = "%Y-%m-%d") {
    # Check input
    horizontal <- check_horizontal(x = x, date_format = date_format)

    # Check periodicity
    checkmate::assert_count(x = periodicity, positive = TRUE, na.ok = FALSE, null.ok = FALSE)
    checkmate::assert_choice(x = periodicity, choices = c(1L, 4L, 12L))

    return(check_vertical(x = t(horizontal), periodicity = periodicity, date_format = date_format))
}

from_horizontal_to_diagonal <- function(x, periodicity, date_format = "%Y-%m-%d") {
    # Check input
    horizontal <- check_horizontal(x = x, date_format = date_format)

    # Check periodicity
    checkmate::assert_count(x = periodicity, positive = TRUE, na.ok = FALSE, null.ok = FALSE)
    checkmate::assert_choice(x = periodicity, choices = c(1L, 4L, 12L))

    diagonal <- apply(
        X = horizontal,
        MARGIN = 2,
        FUN = function(.x) .x[order(is.na(.x))],
        simplify  = FALSE
    )
    diagonal <- do.call(what = rbind, diagonal)
    colnames(diagonal) <- paste0("Release[", seq_len(ncol(diagonal)), "]")

    real_time_period <- convert_time_period(x = rownames(diagonal), date_format = date_format)

    start_year <- as.integer(format(min(real_time_period), format = "%Y"))
    start_month <- as.integer(format(min(real_time_period), format = "%m"))

    if (periodicity == 12L) {
        start <- c(start_year, start_month)
        theo_time_period <- seq.Date(
            from = min(real_time_period),
            by = "month",
            length.out = ncol(x)
        )
    } else if (periodicity == 4L) {
        start <- c(start_year, 1L + ((start_month - 1L) %/% 3L))
        theo_time_period <- seq.Date(
            from = min(real_time_period),
            by = "quarter",
            length.out = ncol(x)
        )
    } else if (periodicity == 1L) {
        start <- c(start_year, 1L + ((start_month - 1L) %/% 3L))
        theo_time_period <- seq.Date(
            from = min(real_time_period),
            by = "year",
            length.out = ncol(x)
        )
    }
    checkmate::assert_set_equal(x = real_time_period, y = theo_time_period)

    diagonal <- stats::ts(
        data = diagonal[as.character(theo_time_period), ],
        start = start,
        frequency = periodicity
    )

    return(diagonal)
}



# Create_vintages function ------------------------------------------------------

#' @title Create vintage tables
#' @rdname create_vintages
#'
#' @description
#' Create vintage tables from data.frame, matrix or mts object in R
#'
#' @details
#' From the input data.frame, the function displays vintages considering three
#' different data structures or views: vertical, horizontal and diagonal. See
#' the `details` section below for more information on the different views. The
#' function returns an object of class `rjd3rev_vintages` that can be used as
#' input in the main function `revision_analysis`.
#'
#' The are four different vintage views:
#'
#' 1. The vertical view shows the observed values at each time period by the
#' different vintages. This approach is robust to changes of base year and
#' data redefinition. A drawback of this approach is that for comparing the
#' same historical series for different vintages, we need to look at the
#' smallest common number of observations and consequently the number of
#' observations is in some circumstances very small. Moreover, it is often the
#' the case that most of the revision is about the last few points of the
#' series so that the number of observations is too small to test anything.
#'
#' 2. The horizontal view shows the observed values of the different vintages
#' by the period. A quick analysis can be performed by rows in order to see
#' how for the same data point (e.g. 2023Q1), figures are first estimated,
#' then forecasted and finally revised. The main findings are usually obvious:
#' in most cases the variance decreases, namely data converge towards the
#' 'true value'. Horizontal tables are just a transpose of vertical tables and
#' are not used in the tests in `revision_analysis`.
#'
#' 3. The diagonal view shows subsequent releases of a given time period,
#' without regard for the date of publication. The advantage of the diagonal
#' approach is that it gives a way to analyse the trade between the timing of
#' the release and the accuracy of the published figures. It is particularly
#' informative when regular estimation intervals exist for the data under
#' study. However, this approach requires to be particularly vigilant in case
#' there is a change in base year or data redefinition.
#'
#' 4. The long view is a representation of data that allows information to be
#' grouped together in order to facilitate their manipulation. With 3 columns
#' (1 column for the time period, 1 column for the publication / revision date
#' and one column for the data), this representation allows for efficient and
#' non-redundant storage of data.
#'
#' @param x a formatted object containing the input. It can be of type
#' `data.frame`, `matrix` or `mts` and must represent one of the multiple
#' vintage views (selected by the argument `type`.
#'
#' @return an object of class `rjd3rev_vintages` which contains the four
#' different view of a revision
#' @export
#' @examples
#' ## creating the input
#'
#' # Long format
#' long_view <- data.frame(
#'     rev_date = rep(x = c("2022-07-31", "2022-08-31", "2022-09-30", "2022-10-31",
#'                          "2022-11-30", "2022-12-31", "2023-01-31", "2023-02-28"),
#'                    each = 4L),
#'     time_period = rep(x = c("2022Q1", "2022Q2", "2022Q3", "2022Q4"), times = 8L),
#'     obs_values = c(
#'         .8, .2, NA, NA, .8, .1, NA, NA,
#'         .7, .1, NA, NA, .7, .2, .5, NA,
#'         .7, .2, .5, NA, .7, .3, .7, NA,
#'         .7, .2, .7, .4, .7, .3, .7, .3
#'     )
#' )
#'
#' vintages_1 <- create_vintages(x = long_view, type = "long", periodicity = 4)
#'
#' # Horizontal format
#' horizontal_view <- matrix(data = c(.8, .8, .7, .7, .7, .7, .7, .7, .2, .1,
#'                             .1, .2, .2, .3, .2, .3, NA, NA, NA, .5, .5, .7, .7,
#'                             .7, NA, NA, NA, NA, NA, NA, .4, .3),
#'                           ncol = 4)
#' colnames(horizontal_view) <- c("2022Q1", "2022Q2", "2022Q3", "2022Q4")
#' rownames(horizontal_view) <- c("2022-07-31", "2022-08-31", "2022-09-30", "2022-10-31",
#'                                "2022-11-30", "2022-12-31", "2023-01-31", "2023-02-28")
#'
#' vintages_2 <- create_vintages(x = horizontal_view, type = "horizontal", periodicity = 4)
#'
#' # Horizontal format
#' vertical_view <- matrix(data = c(.8, .2, NA, NA, .8, .1, NA, NA, .7, .1, NA,
#'                                  NA, .7, .2, .5, NA, .7, .2, .5, NA, .7, .3, .7, NA,
#'                                  .7, .2, .7, .4, .7, .3, .7, .3),
#'                           nrow = 4)
#' rownames(vertical_view) <- c("2022Q1", "2022Q2", "2022Q3", "2022Q4")
#' colnames(vertical_view) <- c("2022-07-31", "2022-08-31", "2022-09-30", "2022-10-31",
#'                                "2022-11-30", "2022-12-31", "2023-01-31", "2023-02-28")
#'
#' vintages_3 <- create_vintages(x = vertical_view, type = "vertical", periodicity = 4)
#'
#' ## specifying the format of revision dates
#' vintages <- create_vintages(
#'     x = long_view,
#'     type ="long",
#'     periodicity = 4L,
#'     date_format= "%Y-%m-%d"
#' )
#'
#' ## including vintage selection
#' vintages <- create_vintages(
#'     x = long_view,
#'     type ="long",
#'     periodicity = 4L,
#'     date_format= "%Y-%m-%d",
#'     vintage_selection = c(start="2022-10-31", end="2023-01-31")
#' )
#'
create_vintages <- function(x, ...) {
    return(UseMethod("create_vintages", x))
}

#' @rdname create_vintages
#' @inheritParams create_vintages
#'
#' @param type character specifying the type of representation of the input
#' between `"long"`, `"horizontal"` and `"vertical"` approach.
#' @param periodicity periodicity of the time period (12, 4 or 1 for resp.
#' monthly, quarterly or annual data)
#' @param date_format \code{character} string corresponding to the format used in
#' the input data.frame for the revision dates.
#' @param vintage_selection \code{Date} vector (or a character vector with the
#' same format as date_format) of length <= 2, specifying the range of revision
#' dates to retain. As an example:
#' c(start = "2022-02-02", end = "2022-08-05") or
#' c(start = as.Date("2022-02-02"), end = as.Date("2022-08-05")) would keep all
#' the vintages whose revision date is between 02 Feb. 2022 and 05 Aug. 2022.
#' If missing (by default), the whole range is selected.
#'
#' @exportS3Method create_vintages data.frame
create_vintages.data.frame <- function(
        x,
        type = c("long", "horizontal", "vertical"),
        periodicity,
        date_format = "%Y-%m-%d",
        vintage_selection,
        ...
) {

    # Check type
    type <- match.arg(type)

    # Check periodicity
    checkmate::assert_count(x = periodicity, positive = TRUE, na.ok = FALSE, null.ok = FALSE)
    checkmate::assert_choice(x = periodicity, choices = c(1L, 4L, 12L))

    if (type == "long") {

        # Check x input
        long <- check_long(x = x, date_format = date_format)

        if (!missing(vintage_selection)) {

            vintage_selection <- as.Date(vintage_selection, format = date_format)
            revdate <- long$revdate

            # Check vintage_selection
            checkmate::assert_date(vintage_selection, min.len = 0, max.len = 2, null.ok = FALSE, any.missing = FALSE)
            checkmate::assert_set_equal(x = length(unique(names(vintage_selection))), y = length(vintage_selection))
            sapply(X = names(vintage_selection), FUN = checkmate::assert_choice, choices = c("start", "end"))
            checkmate::assert_true(vintage_selection["start"] <= vintage_selection["end"], na.ok = TRUE)
            checkmate::assert_true(vintage_selection["start"] <= max(revdate), na.ok = TRUE)
            checkmate::assert_true(vintage_selection["end"] >= min(revdate), na.ok = TRUE)

            index <- ((is.na(vintage_selection["start"]) | revdate >= vintage_selection["start"])
                      & (is.na(vintage_selection["end"]) | revdate <= vintage_selection["end"]))
            long <- long[index, ]
        }

        # Horizontal format
        horizontal <- from_long_to_horizontal(x = long)
        # Vertical format
        vertical <- from_long_to_vertical(x = long, periodicity = periodicity, date_format = "%Y-%m-%d")
        # Diagonal format
        diagonal <- from_horizontal_to_diagonal(x = horizontal, periodicity = periodicity, date_format = "%Y-%m-%d")

        return(structure(
            list(
                vertical_view = vertical,
                horizontal_view = horizontal,
                diagonal_view = diagonal,
                long_view = long
            ),
            class = "rjd3rev_vintages"
        ))

    } else if (type %in% c("horizontal", "vertical")) {
        return(UseMethod("create_vintages", as.matrix(x)))
    }
}

#' @rdname create_vintages
#' @inheritParams create_vintages
#' @inheritParams create_vintages.data.frame
#' @exportS3Method create_vintages mts
create_vintages.mts <- function(
        x,
        type = c("long", "horizontal", "vertical"),
        periodicity,
        date_format = "%Y-%m-%d",
        vintage_selection,
        ...
) {

    # Check type
    type <- match.arg(type)

    if (type %in% c("horizontal", "long")) {
        stop("Wrong type for mts data.")
    } else if (type == "vertical") {

        # Vertical format
        vertical <- check_vertical(x = x, periodicity = periodicity, date_format = date_format)

        if (!missing(vintage_selection)) {

            vintage_selection <- as.Date(vintage_selection, format = date_format)
            revdate <- as.Date(colnames(vertical))

            # Check vintage_selection
            checkmate::assert_date(vintage_selection, min.len = 0, max.len = 2, null.ok = FALSE, any.missing = FALSE)
            checkmate::assert_set_equal(x = length(unique(names(vintage_selection))), y = length(vintage_selection))
            sapply(X = names(vintage_selection), FUN = checkmate::assert_choice, choices = c("start", "end"))
            checkmate::assert_true(vintage_selection["start"] <= vintage_selection["end"], na.ok = TRUE)
            checkmate::assert_true(vintage_selection["start"] <= max(revdate), na.ok = TRUE)
            checkmate::assert_true(vintage_selection["end"] >= min(revdate), na.ok = TRUE)

            index <- ((is.na(vintage_selection["start"]) | revdate >= vintage_selection["start"])
                      & (is.na(vintage_selection["end"]) | revdate <= vintage_selection["end"]))
            vertical <- vertical[, index, drop = FALSE]
            attr(vertical, "class") <- c("mts", "ts", "matrix", "array")

        }

        # Horizontal format
        horizontal <- from_vertical_to_horizontal(x = vertical, date_format = "%Y-%m-%d")
        # Long format
        long <- from_vertical_to_long(x = vertical, date_format = "%Y-%m-%d")
        # Diagonal format
        diagonal <- from_horizontal_to_diagonal(x = horizontal, periodicity = stats::frequency(vertical), date_format = "%Y-%m-%d")
    }

    return(structure(
        list(
            vertical_view = vertical,
            horizontal_view = horizontal,
            diagonal_view = diagonal,
            long_view = long
        ),
        class = "rjd3rev_vintages"
    ))
}

#' @rdname create_vintages
#' @inheritParams create_vintages
#' @inheritParams create_vintages.data.frame
#' @exportS3Method create_vintages matrix
create_vintages.matrix <- function(
        x,
        type = c("long", "horizontal", "vertical"),
        periodicity,
        date_format = "%Y-%m-%d",
        vintage_selection,
        ...
) {

    # Check type
    type <- match.arg(type)

    # Check periodicity
    checkmate::assert_count(x = periodicity, positive = TRUE, na.ok = FALSE, null.ok = FALSE)
    checkmate::assert_choice(x = periodicity, choices = c(1L, 4L, 12L))

    if (type == "long") {
        stop("Wrong type for mts data.")
    } else if (type == "horizontal") {

        # Check x input
        checkmate::assert_matrix(x, mode = "numeric")
        if (length(rownames(x)) == 0 || length(colnames(x)) == 0) {
            stop("Revisions dates or time periods are missing.")
        }

        # Horizontal format
        horizontal <- check_horizontal(x = x, date_format = date_format)

        # Check vintage_selection
        if (!missing(vintage_selection)) {

            vintage_selection <- as.Date(vintage_selection, format = date_format)
            revdate <- as.Date(rownames(horizontal))

            # Check vintage_selection
            checkmate::assert_date(vintage_selection, min.len = 0, max.len = 2, null.ok = FALSE, any.missing = FALSE)
            checkmate::assert_set_equal(x = length(unique(names(vintage_selection))), y = length(vintage_selection))
            sapply(X = names(vintage_selection), FUN = checkmate::assert_choice, choices = c("start", "end"))
            checkmate::assert_true(vintage_selection["start"] <= vintage_selection["end"], na.ok = TRUE)
            checkmate::assert_true(vintage_selection["start"] <= max(revdate), na.ok = TRUE)
            checkmate::assert_true(vintage_selection["end"] >= min(revdate), na.ok = TRUE)

            index <- ((is.na(vintage_selection["start"]) | revdate >= vintage_selection["start"])
                      & (is.na(vintage_selection["end"]) | revdate <= vintage_selection["end"]))
            horizontal <- horizontal[index, , drop = FALSE]

        }

        # Vertical format
        vertical <- from_horizontal_to_vertical(x = horizontal, periodicity = periodicity, date_format = "%Y-%m-%d")
        # Long format
        long <- from_horizontal_to_long(x = horizontal, date_format = "%Y-%m-%d")
        # Diagonal format
        diagonal <- from_horizontal_to_diagonal(x = horizontal, periodicity = stats::frequency(vertical), date_format = "%Y-%m-%d")

    } else if (type == "vertical") {

        # Vertical format
        vertical <- check_vertical(x = x, periodicity = periodicity, date_format = date_format)

        # Check vintage_selection
        if (!missing(vintage_selection)) {

            vintage_selection <- as.Date(vintage_selection, format = date_format)
            revdate <- as.Date(colnames(vertical))

            # Check vintage_selection
            checkmate::assert_date(vintage_selection, min.len = 0, max.len = 2, null.ok = FALSE, any.missing = FALSE)
            checkmate::assert_set_equal(x = length(unique(names(vintage_selection))), y = length(vintage_selection))
            sapply(X = names(vintage_selection), FUN = checkmate::assert_choice, choices = c("start", "end"))
            checkmate::assert_true(vintage_selection["start"] <= vintage_selection["end"], na.ok = TRUE)
            checkmate::assert_true(vintage_selection["start"] <= max(revdate), na.ok = TRUE)
            checkmate::assert_true(vintage_selection["end"] >= min(revdate), na.ok = TRUE)

            index <- ((is.na(vintage_selection["start"]) | revdate >= vintage_selection["start"])
                      & (is.na(vintage_selection["end"]) | revdate <= vintage_selection["end"]))
            vertical <- vertical[, index, drop = FALSE]

        }

        # Horizontal format
        horizontal <- from_vertical_to_horizontal(x = vertical, date_format = "%Y-%m-%d")
        # Long format
        long <- from_vertical_to_long(x = vertical, date_format = "%Y-%m-%d")
        # Diagonal format
        diagonal <- from_horizontal_to_diagonal(x = horizontal, periodicity = stats::frequency(vertical), date_format = "%Y-%m-%d")
    }

    return(structure(
        list(
            vertical_view = vertical,
            horizontal_view = horizontal,
            diagonal_view = diagonal,
            long_view = long
        ),
        class = "rjd3rev_vintages"
    ))
}

#' @rdname create_vintages
#' @inheritParams create_vintages
#' @exportS3Method create_vintages default
create_vintages.default <- function(x, ...) {
    stop("The function requires a data.frame, a matrix or a mts object!")
}

#' @title Create vintages table from CSV or TXT files
#'
#' @inheritParams create_vintages
#' @param file character containing the name of the file which the data are
#'             to be read from.
#' @inheritParams create_vintages.data.frame
#' @param ... Arguments to be passed to `read.csv()`, for example:
#' * `sep` the field separator character
#' * `dec` the character used in the file for decimal points.
#' * `row.names` a vector of row names
#' * `skip` integer, the number of lines of the data file to skip before beginning to read data.
#' * `...`
#'
#' @seealso [create_vintages_from_xlsx()], [create_vintages()] which this function wraps.
#'
#' @return an object of class `rjd3rev_vintages`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' file_name <- "myinput.csv"
#' vintages <- create_vintages_from_csv(
#'     file = file_name,
#'     type = "vertical",
#'     periodicity = 12,
#'     date_format = "%Y-%m-%d",
#'     sep = ";"
#' )
#' }
#'
create_vintages_from_csv <- function(file,
                                     type = c("long", "horizontal", "vertical"),
                                     periodicity,
                                     date_format = "%Y-%m-%d",
                                     ...) {
    # Check of inputs
    file <- normalizePath(file, mustWork = TRUE)

    # Check type
    type <- match.arg(type)

    # Check periodicity
    checkmate::assert_count(x = periodicity, positive = TRUE, na.ok = FALSE, null.ok = FALSE)
    checkmate::assert_choice(x = periodicity, choices = c(1L, 4L, 12L))

    df <- utils::read.csv(file = file, ...)

    return(create_vintages(
        x = df,
        type = type,
        periodicity = periodicity,
        date_format = date_format
    ))
}

#' Create vintages table from XLSX files
#'
#' @inheritParams create_vintages_from_csv
#' @param ... Arguments to be passed to `readxl::read_excel()`, for example:
#' * `sheet` character containing the sheet to read
#' * `range` A cell range to read from
#' * `col_names` a boolean to use the first row as column names
#' * `...`
#'
#' @seealso [create_vintages_from_csv()], [create_vintages()] which this function wraps.
#'
#' @return an object of class `rjd3rev_vintages`
#' @export
#'
#' @examples
#' \dontrun{
#' file_name <- "myinput.xlsx"
#' sheet_name <- "Sheet1"
#' vintages <- create_vintages_from_xlsx(
#'     file = file_name,
#'     type = "horizontal",
#'     periodicity = 12L,
#'     sheet = sheet_name
#' )
#' }
#'
create_vintages_from_xlsx<-function(file,
                                    type = c("long", "horizontal", "vertical"),
                                    periodicity,
                                    date_format = "%Y-%m-%d",
                                    ...) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("package 'readxl' must be installed to run the function 'create_vintages_from_xlsx'")
    }

    # Check of inputs
    file <- normalizePath(file, mustWork = TRUE)

    # Check type
    type <- match.arg(type)

    # Check periodicity
    checkmate::assert_count(x = periodicity, positive = TRUE, na.ok = FALSE, null.ok = FALSE)
    checkmate::assert_choice(x = periodicity, choices = c(1L, 4L, 12L))

    df <- as.data.frame(readxl::read_excel(path = file, ...))

    return(create_vintages(
        x = df,
        type = type,
        periodicity = periodicity,
        date_format = date_format
    ))
}
