
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
#' @param type character specifying the type of representation of the input
#' between `"long"`, `"horizontal"` and `"vertical"` approach.
#' @param periodicity Integer. Periodicity of the time period (12, 4 or 1 for resp.
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
#' @param ... Arguments to be passed to `create_vintages` according to the class of the object `x`
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
#'
#' @exportS3Method create_vintages data.frame
#' @method create_vintages data.frame
#'
create_vintages.data.frame <- function(
        x,
        type = c("long", "horizontal", "vertical"),
        periodicity,
        date_format = "%Y-%m-%d",
        vintage_selection,
        ...) {

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
            revdate <- long[["revdate"]]

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

        output <- list(
            vertical_view = vertical,
            horizontal_view = horizontal,
            diagonal_view = diagonal,
            long_view = long
        )
        class(output) <- "rjd3rev_vintages"
        return(output)

    } else if (type %in% c("horizontal", "vertical")) {
        return(UseMethod("create_vintages", as.matrix(x)))
    }
}

#' @rdname create_vintages
#'
#' @exportS3Method create_vintages mts
#' @method create_vintages mts
#'
create_vintages.mts <- function(
        x,
        type = c("long", "horizontal", "vertical"),
        periodicity,
        date_format = "%Y-%m-%d",
        vintage_selection,
        ...) {

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

    output <- list(
        vertical_view = vertical,
        horizontal_view = horizontal,
        diagonal_view = diagonal,
        long_view = long
    )
    class(output) <- "rjd3rev_vintages"
    return(output)
}

#' @rdname create_vintages
#'
#' @exportS3Method create_vintages matrix
#' @method create_vintages matrix
#'
create_vintages.matrix <- function(
        x,
        type = c("long", "horizontal", "vertical"),
        periodicity,
        date_format = "%Y-%m-%d",
        vintage_selection,
        ...) {

    # Check type
    type <- match.arg(type)

    # Check periodicity
    checkmate::assert_count(x = periodicity, positive = TRUE, na.ok = FALSE, null.ok = FALSE)
    checkmate::assert_choice(x = periodicity, choices = c(1L, 4L, 12L))

    # Check if date in first column
    if (!is.numeric(x[, 1])) {
        rownames(x) <- x[, 1]
        x <- as.matrix(x[, -1])
    }

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

    output <- list(
        vertical_view = vertical,
        horizontal_view = horizontal,
        diagonal_view = diagonal,
        long_view = long
    )
    class(output) <- "rjd3rev_vintages"
    return(output)
}

#' @rdname create_vintages
#'
#' @exportS3Method create_vintages default
#' @method create_vintages default
#'
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
create_vintages_from_xlsx <- function(file,
                                      type = c("long", "horizontal", "vertical"),
                                      periodicity,
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
        periodicity = periodicity
    ))
}

# Generic functions ------------------------------------------------------------

#' Print function for objects of class `"rjd3rev_vintages"`
#'
#' @param x an object of class `"rjd3rev_vintages"`.
#' @param n_row number of last rows to display. For the horizontal view,
#'   corresponds to the number of columns.
#' @param n_col number of columns to display. Can be either the last n columns
#'   (verical view), the last n rows (horizontal view) or the first n columns
#'   (diagonal view). This argument is not used for the long view.
#' @param ... further arguments passed to the \code{\link{print}} function.
#'
#' @exportS3Method print rjd3rev_vintages
#' @method print rjd3rev_vintages
#'
#' @export
#'
print.rjd3rev_vintages <- function(x,
                                   n_row = 8,
                                   n_col = 3,
                                   ...) {

    # Check counts
    checkmate::assert_count(x = n_row, positive = TRUE, na.ok = FALSE, null.ok = FALSE, .var.name = "n_row")
    checkmate::assert_count(x = n_col, positive = TRUE, na.ok = FALSE, null.ok = FALSE, .var.name = "n_col")

    vv <- x[["vertical_view"]]
    n_col_tot <- ncol(vv)
    n_row_tot <- nrow(vv)
    n_row_long_tot <- nrow(x$long_view)
    n_col <- min(n_col_tot, n_col)
    n_row <- min(n_row_tot, n_row)
    n_row_long <- min(n_row_long_tot, n_row)
    freq <- stats::frequency(vv)
    end_period <- stats::end(vv)
    is_extract <- ifelse(n_col < n_col_tot || n_row < n_row_tot || n_row_long < n_row_long_tot, TRUE, FALSE)

    extract_lv <- x$long_view[(n_row_long_tot - n_row_long + 1):n_row_long_tot, ]
    rownames(extract_lv) <- NULL
    extract_vv <- stats::ts(x[["vertical_view"]][(n_row_tot - n_row + 1):n_row_tot, (n_col_tot - n_col + 1):n_col_tot],
                            frequency = freq,
                            end = end_period)
    extract_hv <- x$horizontal_view[(n_col_tot - n_col + 1):n_col_tot, (n_row_tot - n_row + 1):n_row_tot]
    extract_dv <- stats::ts(x[["diagonal_view"]][(n_row_tot - n_row + 1):n_row_tot, 1:n_col],
                            frequency = freq,
                            end = end_period)

    print(list(
        extract = is_extract,
        long_view = extract_lv,
        vertical_view = extract_vv,
        horizontal_view = extract_hv,
        diagonal_view = extract_dv,
        ...
    ))

    return(invisible(NULL))
}

#' Summary function for objects of class "rjd3rev_vintages"
#'
#' @param object an object of class "rjd3rev_vintages".
#' @param ... further arguments passed to or from other methods.
#'
#' @exportS3Method summary rjd3rev_vintages
#' @method summary rjd3rev_vintages
#' @export
#'
summary.rjd3rev_vintages <- function(object, ...) {

    x <- object
    vv <- x[["vertical_view"]]
    cat("Number of releases: ", ncol(vv))
    cat("\nCovered period:")
    cat("\n \tFrom: ", stats::start(vv))
    cat("\n \tTo: ", stats::end(vv))
    cat("\nFrequency: ", stats::frequency(vv))

    return(invisible(NULL))
}

#' @title View function for objects of class "rjd3rev_vintages"
#'
#' @description
#' Display the different view in a different panel to visualize the data in a table / matrix format
#'
#'
#' @param x an object of class "rjd3rev_vintages".
#' @param type type of view to display
#' @param ... further arguments passed to the `View` method.
#'
#' @details
#' Generate the view of the vintages in different format. With the type argument, you can choose the view to display. You can choose between the long, horizontal, vertical and diagonal view.
#'
#' @rdname View
#'
#' @exportS3Method View rjd3rev_vintages
#' @method View rjd3rev_vintages
#' @export
#'
View.rjd3rev_vintages <- function(
        x,
        type = c("all", "long", "horizontal", "vertical", "diagonal"),
        ...) {

    # Check type
    type <- match.arg(type)

    if (!hasArg(title)) {
        title <- ""
    }

    if (type %in% c("all", "long")) utils::View(x$long_view, title = paste(title, "Long view"))
    if (type %in% c("all", "horizontal")) utils::View(x$horizontal_view, title = paste(title, "Horizontal view"))
    if (type %in% c("all", "vertical")) utils::View(x[["vertical_view"]], title = paste(title, "Vertical view"))
    if (type %in% c("all", "diagonal")) utils::View(x[["diagonal_view"]], title = paste(title, "Diagonal view"))

    return(invisible(NULL))
}

#' @title Plot function for objects of class "rjd3rev_vintages"
#'
#' @param x an object of class "rjd3rev_vintages".
#' @param col a color vector of the same length as the number of releases
#' @param ... further arguments passed to or from other methods.
#'
#' @exportS3Method plot rjd3rev_vintages
#' @method plot rjd3rev_vintages
#' @export
#'
plot.rjd3rev_vintages <- function(x, col, ...) {
    if (missing(col)) {
        col <- grDevices::palette.colors(n = ncol(x[["vertical_view"]]))
    }
    graphics::par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)
    stats::ts.plot(x[["vertical_view"]], gpars = list(col = col, ...))
    graphics::legend(
        x = "topright",
        legend = colnames(x[["vertical_view"]]),
        pch = 16,
        col = col,
        xpd = TRUE,
        inset = c(-0.2, 0),
        bty = "n",
        title = "Release dates:"
    )

    return(invisible(NULL))
}
