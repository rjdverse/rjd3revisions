
simulate_series <- function(n, periodicity = 12L) {

    # Check n
    checkmate::assert_count(n, na.ok = FALSE, null.ok = FALSE)

    # Check periodicity
    checkmate::assert_number(x = periodicity, na.ok = FALSE, finite = TRUE, null.ok = FALSE)
    checkmate::assert_choice(x = periodicity, choices = c(1L, 4L, 12L))

    seed <- sample.int(n = .Machine$integer.max, size = 1L)
    coeff <- list(
        phi = NULL,
        d = 1,
        theta = 2.8,
        B_phi = NULL,
        B_D = 1,
        B_theta = -0.2
    )

    JD_model <- rjd3toolkit::sarima_model(
        phi = coeff$phi,
        d = coeff$d,
        theta = coeff$theta,
        bphi = coeff$B_phi,
        bd = coeff$B_D,
        btheta = coeff$B_theta,
        period = periodicity
    )

    return(rjd3toolkit::sarima_random(model = JD_model, length = n, seed = seed))
}

simulate_revision <- function(n, init = stats::rnorm(1, 0, 1)) {
    # Check n
    checkmate::assert_count(n, na.ok = FALSE, null.ok = FALSE)

    # Check init
    checkmate::assert_number(init, na.ok = FALSE, finite = TRUE, null.ok = FALSE)

    output <- init + stats::rnorm(n, mean = 0, sd = 2 ** (2 - seq_len(n)))
    return(output)
}

simulate_revision_dates <- function(n, time_period,
                                    periodicity, origin = "1970-01-01") {
    # Check n
    checkmate::assert_count(n, na.ok = FALSE, null.ok = FALSE)

    x <- min(time_period):(max(time_period) + 2L * 30L * 12L / periodicity) # On rajoute 2 périodes supplémentaires
    output <- as.Date(
        x = sort(sample(
            x = x,
            size = n,
            replace = FALSE
        )),
        origin = origin
    )

    return(output)
}

#' Simulate long datasets with revisions
#'
#' @param n_period Integer. Number of different time-period (length of the
#' simulated series).
#' @param n_revision Integer. Number of different revision dates.
#' @param start_period Date. Start of the series.
#' @param periodicity Integer. Periodicity of the time period (12, 4 or 1 for
#' resp. monthly, quarterly or annual data).
#'
#' @returns A dataset in the long format. See \code{\link{create_vintages}} for
#' more information about the different data formats.
#'
#' @export
#'
#' @examples
#'
#' simulate_long(n_period = 100L, n_revision = 10L)
#' simulate_long(periodicity = 1L)
#' simulate_long(start_period = as.Date("2000-01-01"),
#'               n_period = 10L * 12L,
#'               periodicity = 12L)
#' simulate_long(periodicity = 4L, n_period = 5L * 4L)
#'
simulate_long <- function(n_period = 50,
                          n_revision = 10,
                          start_period = as.Date("2012-01-01"),
                          periodicity = 12L) {
    # Check n_period
    checkmate::assert_count(n_period, positive = TRUE, na.ok = FALSE, null.ok = FALSE)

    # Check n_revision
    checkmate::assert_count(n_revision, positive = TRUE, na.ok = FALSE, null.ok = FALSE)

    # Check start_period
    checkmate::check_date(start_period, len = 1, null.ok = FALSE)

    # Check periodicity
    checkmate::assert_number(x = periodicity, na.ok = FALSE, finite = TRUE, null.ok = FALSE)
    checkmate::assert_choice(x = periodicity, choices = c(1L, 4L, 12L))

    if (periodicity == 12L) {
        by <- "month"
    } else if (periodicity == 4L) {
        by <- "quarter"
    } else if (periodicity == 1L) {
        by <- "year"
    }

    time_period <- seq.Date(from = start_period, by = by, length.out = n_period)
    rev_date <- simulate_revision_dates(n = n_revision, time_period, periodicity,
                                        origin = "1970-01-01")

    final_series <- simulate_series(n_period, periodicity = periodicity)

    long <- data.frame(
        rev_date = integer(),
        time_period = integer(),
        obs_values = double()
    )

    for (index_period in seq_along(time_period)) {
        period <- time_period[index_period]
        value <- final_series[index_period]
        nb_NA <- sum(rev_date < period)
        revised_series <- c(rep(NA_real_, nb_NA), simulate_revision(
            n = n_revision - nb_NA,
            init = value
        ))
        long <- rbind(long, data.frame(
            rev_date = rev_date,
            time_period = period,
            obs_values = revised_series
        ))
    }

    long <- long[order(long[["rev_date"]], long[["time_period"]]), ]
    rownames(long) <- NULL

    return(long)
}
