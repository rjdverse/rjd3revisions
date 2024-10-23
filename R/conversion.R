
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
        varying = list(as.character(unique(x[["revdate"]])))
    )

    time_periods <- vertical[["time"]]
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
        varying = list(as.character(unique(x[["time"]])))
    )
    revdate <- horizontal[["revdate"]]
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
        data = data.frame(time = rownames(vertical), vertical, check.names = FALSE),
        direction = "long",
        varying = colnames(vertical),
        v.names = "obs_values",
        idvar = "time",
        ids = rownames(vertical),
        timevar = "revdate",
        times = colnames(vertical)
    )
    long <- long[, c("revdate", "time", "obs_values")]
    long[["revdate"]] <- assert_rev_date(long[["revdate"]], date_format)
    long[["time"]] <- assert_time_period(long[["time"]], date_format)
    long <- long[order(long[["revdate"]], long[["time"]]), ]
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
        data = data.frame(revdate = rownames(horizontal), horizontal, check.names = FALSE),
        direction = "long",
        varying = colnames(horizontal),
        v.names = "obs_values",
        idvar = "revdate",
        ids = rownames(horizontal),
        timevar = "time",
        times = colnames(horizontal)
    )
    long <- long[, c("revdate", "time", "obs_values")]
    long[["revdate"]] <- assert_rev_date(long[["revdate"]], date_format)
    long[["time"]] <- assert_time_period(long[["time"]], date_format)
    long <- long[order(long[["revdate"]], long[["time"]]), ]
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

    real_time_period <- assert_time_period(x = rownames(diagonal), date_format = date_format)

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
