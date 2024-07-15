#' Set threshold values that can be customized by the user through global
#' options
#'
#' For each test, the function returns either default values or user-defined
#' values of the thresholds. User-defined values can be defined by the user
#' through the global options. The threshold values are used to make quality
#' assessment in the output of the functions `summary.rjd3rev_rslts()`
#' and `render_report()`. Default thresholds considered for residuals
#' diagnostics can also be changed if necessary. Options can be set via
#' `options()` and queried via `getOption()`.
#'
#' @examples
#' ## Simulated data
#' period_range <- seq(as.Date('2011-01-01'),as.Date('2020-10-01'),by='quarter')
#' qtr <- (as.numeric(substr(period_range,6,7))+2)/3
#' time_period <- rep(paste0(format(period_range, "%Y"), "Q", qtr),5)
#' np <- length(period_range)
#' rev_date <- c(rep("2021-06-30",np), rep("2021-12-31",np), rep("2022-06-30",np),
#'             rep("2022-12-31",np), rep("2023-06-30",np))
#' set.seed(1)
#' xt <- cumsum(sample(rnorm(1000,0,1), np, TRUE))
#' rev <- rnorm(np*4,0,.1)
#' obs_values <- xt
#' for(i in 1:4) {
#'   xt <- xt+rev[(1+(i-1)*np):(i*np)]
#'   obs_values <- c(obs_values,xt)
#' }
#' df <- data.frame(rev_date, time_period, obs_values)
#'
#' ## Create a `"rjd3rev_vintages"` object with the input
#' vintages <- create_vintages(x = df, periodicity = 4, date_format = "%Y-%m-%d")
#'
#' ## Define threshold
#' options(list(at_test_thresholds = c(severe = 0.005, bad = 0.05, uncertain = 0.1)),
#'              t_test_thresholds = c(severe = 0.005, bad = 0.05, uncertain = 0.1)),
#'              theil_u2_test_thresholds = c(uncertain = .5, bad = .75, severe = 1)))
#'
#' rslt1 <- revision_analysis(vintages)
#'
#' summary(rslt1)
#'
set_thresholds <- function(){

    # Thresholds values should be defined as a numeric vector.

    # We start from -Inf and each element of the vector should be understood as
    # the lower bound of the corresponding assessment. Furthermore, the
    # assessment "good" is always the not being mentioned.
    # As an example, assume I define the following bounds for my test
    # options(mytest = c(severe = 0.001, bad = 0.01, uncertain = 0.05))
    # This means that, if the assessment is based on the p-value, the situation
    # will be assessed as severe if the p-value < 0.001, as bad if between
    # 0.001 and 0.01, as uncertain if between 0.01 and 0.05 and as good if
    # higher than 0.05.

    return(list(

        # Here are the possible options the user can modified, together with the
        # default value

        # Threshold values for Theil’s Inequality Coefficient U1:
        theil_u1 = getOption("theil_u1", default = c(uncertain = .8, bad = .9, severe = .99)),

        # Threshold values for Theil’s Inequality Coefficient U2:
        theil_u2 = getOption("theil_u2", default = c(uncertain = .8, bad = .9, severe = 1)),

        # Threshold values for T-test
        t = getOption("t", default = c(severe = 0.001, bad = 0.01, uncertain = 0.05)),

        # Threshold values for Augmented T-test
        augmented_t = getOption("augmented_t", default = c(severe = 0.001, bad = 0.01, uncertain = 0.05)),

        # Threshold values for Slope and drift test
        slope_and_drift = getOption("slope_and_drift", default = c(severe = 0.001, bad = 0.01, uncertain = 0.05)),

        # Threshold values for Efficiency test (test 1)
        eff1 = getOption("eff1", default = c(severe = 0.001, bad = 0.01, uncertain = 0.05)),

        # Threshold values for Efficiency test (test 2)
        eff2 = getOption("eff2", default = c(severe = 0.001, bad = 0.01, uncertain = 0.05)),

        # Threshold values for Orthogonality test (test 1)
        orth1 = getOption("orth1", default = c(severe = 0.001, bad = 0.01, uncertain = 0.05)),

        # Threshold values for Orthogonality test (test 2)
        orth2 = getOption("orth2", default = c(severe = 0.001, bad = 0.01, uncertain = 0.05)),

        # Threshold values for Orthogonality test (test 3 on autocorrelation)
        autocorr = getOption("autocorr", default = c(severe = 0.001, bad = 0.01, uncertain = 0.05)),

        # Threshold values for Orthogonality test (test 4 on seasonality)
        seas = getOption("seas", default = c(severe = 0.001, bad = 0.01, uncertain = 0.05)),

        # Threshold values for Signal vs Noise test (test 1)
        signal_noise1 = getOption("signal_noise1", default = c(severe = 0.001, bad = 0.01, uncertain = 0.05)),

        # Threshold values for Signal vs Noise test (test 2)
        signal_noise2 = getOption("signal_noise2", default = c(uncertain = 0.05)),


        # Threshold values for residuals diagnostics

        ## Normality test: Jarque-Bera
        jb_res = getOption("jb_res", default = c(bad = 0.01, uncertain = 0.1)),

        ## Homoskedasticity test: Breusch-Pagan
        bp_res = getOption("bp_res", default = c(bad = 0.01, uncertain = 0.1)),

        ## Homoskedasticity test: White
        white_res = getOption("white_res", default = c(bad = 0.01, uncertain = 0.1)),

        ## Homoskedasticity test: ARCH
        arch_ress = getOption("arch_res", default = c(bad = 0.01, uncertain = 0.1))
        )
    )
}
