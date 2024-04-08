
# Check functions --------------------------------------------------------------

check_long <- function(x, date_format = "%Y-%m-%d") {
    checkmate::assert_data_frame(x, ncols = 3L)
    checkmate::assert_numeric(x[, 3, drop = TRUE], .var.name = "The third column")
    rev_date <- convert_rev_date(x[, 1, drop = TRUE], date_format = date_format)
    time_period <- convert_time_period(x[, 2, drop = TRUE], date_format = date_format)

    # Long format
    long <- x

    colnames(long) <- c("revdate", "time", "obs_values")
    long$revdate <- rev_date
    long$time <- time_period
    long <- long[order(long$revdate, long$time), ]

    return(long)
}

check_vertical <- function(x, ...) {
    return(UseMethod("check_vertical", x))
}

#' @exportS3Method
check_vertical.mts <- function(
        x,
        date_format = "%Y-%m-%d",
        periodicity = NULL,
        ...
) {
    # Check data type
    checkmate::assert_matrix(x, mode = "numeric")

    # Check frequency
    checkmate::assert_choice(x = frequency(x), choices = c(4L, 12L, 1L))
    if (!is.null(periodicity)) {
        checkmate::assert_set_equal(x = frequency(x), y = periodicity)
    }

    # Vertical format
    vertical <- x
    colnames(vertical) <- as.character(convert_rev_date(colnames(vertical), date_format))

    return(vertical)
}

#' @exportS3Method
check_vertical.matrix <- function(
        x,
        date_format = "%Y-%m-%d",
        periodicity = c(4L, 12L, 1L),
        ...
) {
    # Check periodicity
    periodicity <- match.arg(arg = NULL, choices = periodicity)

    # Check data type
    checkmate::assert_matrix(x, mode = "numeric")
    if (length(rownames(x)) == 0 || length(colnames(x)) == 0) {
        stop("Revisions dates or time periods are missing.")
    }

    # Vertical format
    vertical <- x
    colnames(vertical) <- as.character(convert_rev_date(colnames(vertical), date_format))
    rownames(vertical) <- as.character(convert_time_period(rownames(vertical), date_format))

    # Check date periods
    real_time_period <- convert_time_period(rownames(vertical), date_format)

    start_year <- as.integer(format(min(real_time_period), format = "%Y"))
    start_month <- as.integer(format(min(real_time_period), format = "%m"))

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
    }
    checkmate::assert_set_equal(real_time_period, theo_time_period)

    vertical <- ts(
        data = vertical[as.character(theo_time_period), ],
        start = start,
        frequency = periodicity
    )

    return(vertical)
}

#' @exportS3Method
check_vertical.default <- function(x, ...) {
    stop("The function requires a matrix or a mts object!")
}

check_horizontal <- function(x, date_format = "%Y-%m-%d") {
    horizontal <- x
    colnames(horizontal) <- as.character(convert_time_period(colnames(horizontal), date_format))
    rownames(horizontal) <- as.character(convert_rev_date(rownames(horizontal), date_format))

    return(horizontal)
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
    checkmate::assert_choice(class(x), choices = c("character", "integer", "Date"))
    if (any(is.na(as.Date(x, format = date_format)))) {
        stop("Revisions date not in a correct format. You can specify the format of your date with the argument `format_date`")
    }
    return(as.Date(x, format = date_format))
}

from_long_to_vertical <- function(x, date_format = "%Y-%m-%d", periodicity) {
    x <- check_long(x)

    vertical <- reshape(
        data = x,
        timevar = "revdate",
        idvar = "time",
        direction = "wide",
        varying = list(as.character(unique(x$revdate)))
    )

    time_periods <- vertical$time
    vertical <- as.matrix(vertical[, -1])
    rownames(vertical) <- as.character(time_periods)
    return(check_vertical(vertical, date_format = date_format, periodicity = periodicity))
}

from_long_to_horizontal <- function(x) {
    x <- check_long(x)

    horizontal <- reshape(
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

from_long_to_diagonal <- function(x, periodicity) {
    x <- check_long(x)

    horizontal <- from_long_to_horizontal(x)
    diagonal <- from_horizontal_to_diagonal(horizontal, periodicity)
    return(diagonal)
}

from_vertical_to_long <- function(x, date_format = "%Y-%m-%d") {
    x <- check_vertical(x, date_format)
    vertical <- t(t(x))
    if (frequency(x) == 12L) {
        time_period <- seq.Date(
            from = as.Date(paste0(start(x)[1], "-", start(x)[2], "-01")),
            by = "month",
            length.out = nrow(x)
        )
    } else if (frequency(x) == 4L) {
        time_period <- seq.Date(
            from = as.Date(paste0(start(x)[1], "-", 3 * start(x)[2] - 2, "-01")),
            by = "quarter",
            length.out = nrow(x)
        )
    }
    rownames(vertical) <- as.character(time_period)

    long <- reshape(
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
    x <- check_vertical(x, date_format)
    horizontal <- t(x)
    if (frequency(x) == 12L) {
        time_period <- seq.Date(
            from = as.Date(paste0(start(x)[1], "-", start(x)[2], "-01")),
            by = "month",
            length.out = nrow(x)
        )
    } else if (frequency(x) == 4L) {
        time_period <- seq.Date(
            from = as.Date(paste0(start(x)[1], "-", 3 * start(x)[2] - 2, "-01")),
            by = "quarter",
            length.out = nrow(x)
        )
    }
    colnames(horizontal) <- as.character(time_period)
    return(horizontal)
}

from_vertical_to_diagonal <- function(x, date_format = "%Y-%m-%d") {
    x <- check_vertical(x, date_format)
    horizontal <- from_vertical_to_horizontal(x, date_format)
    diagonal <- from_horizontal_to_diagonal(horizontal, periodicity)
    return(diagonal)
}

from_horizontal_to_long <- function(x, date_format = "%Y-%m-%d") {

    horizontal <- check_horizontal(x, date_format = date_format)

    long <- reshape(
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

from_horizontal_to_vertical <- function(x, date_format = "%Y-%m-%d", periodicity) {
    horizontal <- check_horizontal(x, date_format = date_format)
    return(check_vertical(t(horizontal), date_format, periodicity))
}

from_horizontal_to_diagonal <- function(x, date_format = "%Y-%m-%d", periodicity) {
    horizontal <- check_horizontal(x, date_format = date_format)
    diagonal <- apply(
        X = horizontal,
        MARGIN = 2,
        FUN = function(.x) .x[order(is.na(.x))],
        simplify  = FALSE
    )
    diagonal <- do.call(what = rbind, diagonal)
    colnames(diagonal) <- paste0("Release[", seq_len(ncol(diagonal)), "]")

    real_time_period <- convert_time_period(rownames(diagonal), date_format)

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
    }
    checkmate::assert_set_equal(real_time_period, theo_time_period)

    diagonal <- ts(
        data = diagonal[as.character(theo_time_period), ],
        start = start,
        frequency = periodicity
    )

    return(diagonal)
}



# Create_vintages function ------------------------------------------------------

#' @title Create vintage tables
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
#' @param type character specifying the type of representation of the input
#' between `"long"`, `"horizontal"` and `"vertical"` approach.
#' @param periodicity periodicity of the time period (12, 4 or 1 for resp.
#' monthly,quarterly or annual data)
#' @param date_format character string corresponding to the format used in
#' the input data.frame for the revision dates.
#'
#' @return an object of class `rjd3rev_vintages` which contains the four
#' different view of a revision
#' @export
#' @examples
#' ## creating the input
#' df <- data.frame(rev_date = c(rep("2022-07-31",4), rep("2022-08-31",4),
#'                               rep("2022-09-30",4), rep("2022-10-31",4),
#'                               rep("2022-11-30",4), rep("2022-12-31",4),
#'                               rep("2023-01-31",4), rep("2023-02-28",4)),
#'                  time_period = c(rep(c("2022Q1","2022Q2","2022Q3","2022Q4"),8)),
#'                  obs_value = c(.8,.2,NA,NA, .8,.1,NA,NA,
#'                                .7,.1,NA,NA, .7,.2,.5,NA,
#'                                .7,.2,.5,NA, .7,.3,.7,NA,
#'                                .7,.2,.7,.4, .7,.3,.7,.3))
#' vintages <- create_vintages(x = df, type = "long", periodicity = 4L)
#'
#' ## specifying the format of revision dates
#' vintages <- create_vintages(
#'     x = df,
#'     type ="long",
#'     periodicity = 4L,
#'     date_format= "%Y-%m-%d"
#' )
#'
create_vintages <- function(x, ...) {
    return(UseMethod("create_vintages", x))
}

#' @export
create_vintages.data.frame <- function(
        x,
        type = c("long", "horizontal", "vertical"),
        periodicity = c(4L, 12L, 1L),
        date_format = "%Y-%m-%d",
        ...
) {

    # check type
    type <- match.arg(type)

    # check periodicity
    periodicity <- match.arg(arg = NULL, choices = periodicity)

    if (type == "long") {

        # check x input
        long <- check_long(x, date_format)

        # Horizontal format
        horizontal <- from_long_to_horizontal(long)
        # Vertical format
        vertical <- from_long_to_vertical(long, date_format = date_format, periodicity = periodicity)
        # Diagonal format
        diagonal <- from_horizontal_to_diagonal(horizontal, periodicity = periodicity)

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

#' @export
create_vintages.mts <- function(
        x,
        type = c("long", "horizontal", "vertical"),
        periodicity = NULL,
        date_format = "%Y-%m-%d",
        ...
) {

    # check type
    type <- match.arg(type)

    if (type %in% c("horizontal", "long")) {
        stop("Wrong type for mts data.")
    } else if (type == "vertical") {

        # Vertical format
        vertical <- check_vertical(x, date_format, periodicity)

        # Horizontal format
        horizontal <- from_vertical_to_horizontal(vertical)
        # Long format
        long <- from_vertical_to_long(vertical, date_format)
        # Diagonal format
        diagonal <- from_horizontal_to_diagonal(horizontal, periodicity = frequency(vertical))
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

#' @export
create_vintages.matrix <- function(
        x,
        type = c("long", "horizontal", "vertical"),
        periodicity = c(4L, 12L, 1L),
        date_format = "%Y-%m-%d",
        ...
) {

    # check type
    type <- match.arg(type)

    # check periodicity
    periodicity <- match.arg(arg = NULL, choices = periodicity)

    if (type == "long") {
        stop("Wrong type for mts data.")
    } else if (type == "horizontal") {

        # check x input
        checkmate::assert_matrix(x, mode = "numeric")
        if (length(rownames(x)) == 0 || length(colnames(x)) == 0) {
            stop("Revisions dates or time periods are missing.")
        }

        # Horizontal format
        horizontal <- check_horizontal(x, date_format = date_format)

        # Vertical format
        vertical <- from_horizontal_to_vertical(horizontal, date_format = date_format, periodicity = periodicity)
        # Long format
        long <- from_horizontal_to_long(horizontal, date_format)
        # Diagonal format
        diagonal <- from_horizontal_to_diagonal(horizontal, periodicity = frequency(vertical))

    } else if (type == "vertical") {

        # Vertical format
        vertical <- check_vertical(x, date_format, periodicity)

        # Horizontal format
        horizontal <- from_vertical_to_horizontal(vertical)
        # Long format
        long <- from_vertical_to_long(vertical, date_format)
        # Diagonal format
        diagonal <- from_horizontal_to_diagonal(horizontal, periodicity = frequency(vertical))
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

#' @export
create_vintages.default <- function(x, ...) {
    stop("The function requires a data.frame, a matrix or a mts object!")
}

#' Create vintages table from CSV or TXT files
#' @param file character containing the name of the file which the data are
#'             to be read from.
#' @inheritParams create_vintages
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
                                     periodicity = c(4L, 12L, 1L),
                                     date_format = "%Y-%m-%d",
                                     ...) {
    # check of inputs
    file <- normalizePath(file, mustWork = TRUE)

    # check type
    type <- match.arg(type)

    # check periodicity
    periodicity <- match.arg(arg = NULL, choices = periodicity)

    df <- read.csv(file, ...)

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
                                    periodicity = c(4L, 12L, 1L),
                                    date_format = "%Y-%m-%d",
                                    ...) {
    if (!require(readxl)) {
        stop("package 'readxl' must be installed to run the function 'create_vintages_from_xlsx'")
    }

    # check of inputs
    file <- normalizePath(file, mustWork = TRUE)

    # check type
    type <- match.arg(type)

    # check periodicity
    periodicity <- match.arg(arg = NULL, choices = periodicity)

    df <- readxl::read_excel(path = file, ...)

    return(create_vintages(
        x = df,
        type = type,
        periodicity = periodicity,
        date_format = date_format
    ))
}
