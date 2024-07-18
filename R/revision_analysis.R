# #' @include revisions.R
# #' @include options.R

#' Revision analysis through a battery of tests
#'
#' The function perform parametric tests which enable the users to detect
#' potential bias (both mean and regression bias) and sources of inefficiency in
#' preliminary estimates. We would conclude to inefficiency in the preliminary
#' estimates when revisions are predictable in some way. In the results,
#' parametric tests are divided into 5 categories: relevancy (check whether
#' preliminary estimates are even worth it), bias, efficiency, orthogonality
#' (correlation at higher lags), and signalVSnoise. Descriptive statistics on
#' revisions are also provided. For some of the parametric tests, prior
#' transformation of the vintage data may be important to avoid misleading
#' results. By default, the decision to differentiate the vintage data is
#' performed automatically based on unit root and co-integration tests whose
#' results can be found found in the results too (section 'varbased'). Finally,
#' running the function `render_report()` on the output of `revision_analysis()`
#' would give you both a formatted summary of the results and full explanations
#' about each tests.
#'
#' @param vintages an object of class `"rjd3rev_vintages"` which is the output
#'                 of the function `create_vintages()`
#' @param gap Integer. Gap to consider between each vintages. Default is 1
#'            which means that revisions are calculated and tested for each
#'            vintages consecutively.
#' @param view Selected view. Can be "vertical" (the default) or "diagonal".
#'             Vertical view shows the observed values at each time period by
#'             the different vintages. Diagonal view shows subsequent releases
#'             of a given time period, without regard for the date of
#'             publication, which can be particularly informative when regular
#'             estimation intervals exist. See `?create_vintages()` for more
#'             information about interests and drawbacks of each view.
#' @param n.releases only used when `view = "diagonal"`. Ignored otherwise.
#'                   Allow the user to limit the number of releases under
#'                   investigation). When `view = "vertical"`, the user is
#'                   invited to limit the number of vintages upstream through
#'                   the parameter `vintage_selection` in `create_vintages()`
#'                   whenever necessary.
#' @param transf.diff differentiation to apply to the data prior testing. Only
#'                    used for regressions including vintage data as regressor
#'                    and/or regressand. Regression including revision data only
#'                    are never differentiated even if `transf.diff = "forced"`.
#'                    Options are "automatic" (the default), "forced" and "none".
#' @param transf.log Boolean whether a log-transformation should first be
#'                   applied to the data. Default is FALSE.
#' @param descriptive.rounding Integer. Number of decimals to display for
#'                             descriptive statistics. Default is 3.
#' @param nrevs,ref Integer. Number of lags to consider for orthogonality tests
#'                  1 and 2 respectively.
#' @param na.zero Boolean whether missing values should be considered as 0 or
#'                rather as data not yet available (the default).
#' @import rJava rjd3toolkit
#'
#' @seealso `create_vintages()` to create the input object,
#'          `render_report()` to get a summary and information the tests
#'
#' @return an object of class 'rjd3rev_rslts'
#'
#' @export
#'
#' @examples
#'
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
#' # revisions <- get_revisions(vintages, gap = 1) # just to get a first insight of the revisions
#'
#' ## Call using all default parameters
#' rslt1 <- revision_analysis(vintages)
#' # render_report(rslt1)
#' # summary(rslt1) # formatted summary only
#'
#' ## Calls using diagonal view (suited in many situations such as to evaluate GDP estimates)
#' ## Note: when input are not growth rates but the gross series, differentiation is
#' ## performed automatically (if transf.diff is let to its default option) but `transf.log`
#' ## must be set to TRUE manually whenever a log-transformation of the data is necessary
#' rslt2 <- revision_analysis(vintages, gap = 1, view = "diagonal", n.releases = 3)
#' # render_report(rslt2)
#' # summary(rslt2)
#'
#' ## Call to evaluate revisions for a specific range of vintage periods
#' vintages <- create_vintages(
#'     x = df,
#'     periodicity = 4,
#'     vintage_selection = c(start="2021-12-31", end="2023-06-30")
#' )
#' rslt3 <- revision_analysis(vintages, gap=2, view = "vertical")
#' #render_report(rslt3)
#' #summary(rslt3)
#'
#' ## Note that it is possible to change thresholds values for quality
#' ## assessment using global options (see file `options.R` for details)
#' options(list(augmented_t = c(severe = 0.005, bad = 0.01, uncertain = 0.05),
#'              slope_and_drift = c(severe = 0.005, bad = 0.05, uncertain = 0.10),
#'              theil_u2 = c(uncertain = .5, bad = .7, severe = 1)))
#' rslt4 <- revision_analysis(vintages, gap = 1, view = "diagonal", n.releases = 3)
#' summary(rslt4)
#'
revision_analysis <- function(vintages,
                              gap = 1,
                              view = c("vertical", "diagonal"),
                              n.releases = 3,
                              transf.diff = c("auto", "forced", "none"),
                              transf.log = FALSE,
                              descriptive.rounding = 3,
                              nrevs = 1,
                              ref = 1,
                              na.zero = FALSE) {

    options(scipen = 999)

    cl <- match.call()
    view <- match.arg(view)
    transf.diff <- match.arg(transf.diff)

    if (is.null(vintages)) stop("No vintage found!")
    if (view == "diagonal") {
        if (ncol(vintages$diagonal_view) < (gap + 1)) stop("The number of releases must be >= (1+gap)")
        if (n.releases < (gap + 1)) stop("'n.releases' must be >= (1+gap)")
    } else {
        if (ncol(vintages$vertical_view) < (gap + 1)) stop("The number of vintages must be >= (1+gap)")
    }

    # Pre-treatment

    ## Vintages & revisions
    vintages_info <- get_vintages_view(vintages, transf.log, view, n.releases)
    vt <- vintages_info$vt
    rv <- get_revisions_view(vt, gap)
    is_log <- vintages_info$is_log
    if (is_log) rv_notrf <- get_revisions_view(exp(vt), gap) else rv_notrf <- rv

    ## Differentiation
    freq <- stats::frequency(vt)

    ur_test <- unitroot(vt, adfk = 1, na.zero) # p.m. H0: non-stationary
    is_stationary <- ur_test_intepretor(ur_test)

    coint_test <- cointegration(vt, adfk = 1, na.zero) # p.m. H0: no-cointegration
    is_cointegrated <- coint_test_interpretor(coint_test, is_stationary)

    seas_test <- apply(vt, 2, seasonality_test)
    is_seasonal <- ifelse(sum(seas_test) / ncol(vt) > .8, TRUE, FALSE)

    delta_diff <- ifelse(is_seasonal, freq, 1)
    if (transf.diff == "auto") {
        vts <- if (is_stationary) vt else diff(vt, delta_diff)
        vtc <- if (is_cointegrated) vt else diff(vt, delta_diff)
    } else if (transf.diff == "forced") {
        vts <- vtc <- diff(vt, delta_diff)
        is_stationary <- is_cointegrated <- FALSE
    } else if (transf.diff == "none") {
        vts <- vtc <- vt
        if (!is_stationary || !is_cointegrated) {
            warning("No differentiation considered even though stationarity and/or cointegration might not be present. This can lead to spurious regression.", call. = FALSE)
        }
        is_stationary <- is_cointegrated <- TRUE
    }

    # Descriptive statistics
    ds <- descriptive_statistics(rv_notrf, descriptive.rounding)

    # Parametric analysis

    quality_thresholds <- set_thresholds()
    quality_thresholds_res <- list(jb = quality_thresholds$jb_res,
                                   bp = quality_thresholds$bp_res,
                                   white = quality_thresholds$white_res,
                                   arch = quality_thresholds$arch_res)

    ## I. Relevancy

    ### Theil tests (U1 and U2)
    U1 <- theil(vt, gap, na.zero)
    U2 <- theil2(vt, gap, na.zero)
    theil_infos <- theil_test_evaluator(U1, U2, N = ds["N", ], n_test = ncol(rv),
                                        thr = list(u1 = quality_thresholds$theil_u1,
                                                   u2 = quality_thresholds$theil_u2))

    ## II. Bias (mean and regression bias)

    ### T-test and Augmented T-test
    tat_test <- bias(rv, na.zero)
    tat_infos <- tat_test_evaluator(tat_test, is_log, n_test = ncol(rv),
                                    thr = list(t = quality_thresholds$t,
                                               at = quality_thresholds$augmented_t))

    ### Slope and drift
    sd_test <- slope_and_drift(vtc, gap, na.zero)
    sd_infos <- sd_test_evaluator(sd_test, is_log, is_cointegrated, delta_diff,
                                  n_test = ncol(vtc[, -c(1:gap), drop = FALSE]),
                                  thr = quality_thresholds$slope_and_drift,
                                  thr_res = quality_thresholds_res)

    ## III. Efficiency

    ### Regression of revision on previous estimate (=noise)
    eff1_test <- efficiencyModel1(vts, gap, na.zero)
    eff1_infos <- eff1_test_evaluator(eff1_test, is_log, is_stationary,
                                      delta_diff, n_test = ncol(rv),
                                      thr = quality_thresholds$eff1,
                                      thr_res = quality_thresholds_res)

    ### Regression of latter revisions (Rv) on previous revisions (Rv_1)
    eff2_test <- efficiencyModel2(vt, gap, na.zero)
    eff2_infos <- eff2_test_evaluator(eff2_test, is_log, n_test = ncol(rv) - 1,
                                      thr = quality_thresholds$eff2,
                                      thr_res = quality_thresholds_res)

    ## IV. Orthogonality

    ### Regression of latter revisions (Rv) on previous revisions (Rv_1, Rv_2,...Rv_p)
    orth1_test <- orthogonallyModel1(rv, nrevs, na.zero)
    orth1_infos <- orth1_test_evaluator(orth1_test, is_log, nrevs, ncol_rv = ncol(rv),
                                        n_test = ncol(rv[, -c(1:nrevs), drop = FALSE]),
                                        thr = quality_thresholds$orth1,
                                        thr_res = quality_thresholds_res)

    ### Regression model of latter revisions (Rv) on previous revisions at a specific version (Rv_k)
    orth2_test <- orthogonallyModel2(rv, ref, na.zero)
    orth2_infos <- orth2_test_evaluator(orth2_test, is_log, ref, ncol_rv = ncol(rv),
                                        n_test = ncol(rv[, -c(1:ref), drop = FALSE]),
                                        thr = quality_thresholds$orth2,
                                        thr_res = quality_thresholds_res)

    ### Autocorrelation test
    ac_test <- try(apply(rv, 2, function(x) ljungbox(x[!is.na(x)], k = 2)), silent = TRUE) # Ljung-Box up to k
    ac_infos <- ac_test_evaluator(ac_test, is_log, cnames = colnames(rv), n_test = ncol(rv),
                                  thr = quality_thresholds$autocorr)

    ### Seasonality tests
    lb_test <- try(apply(diff(rv), 2, function(x) seasonality_qs(x, freq)), silent = TRUE) # Ljung-Box
    fd_test <- try(apply(diff(rv), 2, function(x) seasonality_friedman(x, freq)), silent = TRUE)  # Friedman non-parametric test
    seas_infos <- seas_tests_evaluator(lb_test, fd_test, is_log, cnames = colnames(rv),
                                       freq = freq, n_test = ncol(rv),
                                       thr = quality_thresholds$seas)

    ## V. Signal vs Noise
    sn_test <- signalnoise(vts, gap, na.zero)
    sn_infos <- sn_test_evaluator(sn_test, is_log, is_stationary,
                                  delta_diff, n_test = ncol(rv),
                                  thr = list(noise = quality_thresholds$signal_noise1,
                                             news = quality_thresholds$signal_noise2))

    # VAR-based Analysis
    vecm_test <- vecm(vt, lag = 2, model = "none", na.zero) ## VECM
    var_based_rslt <- list(unit_root_test = ur_test,
                           cointegration_test = coint_test,
                           vecm = vecm_test)

    # Output
    lbl <- c(paste("Relevancy - Theil", theil_infos$U_det), "Bias1 T-test", "Bias2 Augmented T-test",
             "Bias3 SlopeAndDrift (Ols L on P) - Mean", "Bias3 SlopeAndDrift (Ols L on P) - Reg.",
             "Efficiency1 (Ols R on P) - Mean", "Efficiency1 (Ols R on P) - Reg.",
             "Efficiency2 (Ols Rv on Rv_1) - Mean", "Efficiency2 (Ols Rv on Rv_1) - Reg.",
             "Orthogonality1 (Ols Rv on Rv_(1:p)) - Mean", "Orthogonality1 (Ols Rv on Rv_(1:p)) - Reg.",
             "Orthogonality2 (Ols Rv on Rv_k.) - Mean", "Orthogonality2 (Ols Rv on Rv_k) - Reg.",
             "Orthogonality3 AutoCorrelation (Ljung-Box)",
             "Orthogonality4 Seasonality (Ljung-Box)", "Orthogonality4 Seasonality (Friedman)",
             "SignalVsNoise1 - Noise (Ols R on P)", "SignalVsNoise2 - Signal (Ols R on L)")
    transformation <- c(theil_infos$theil_trf, rep(tat_infos$tat_trf, 2),
                        rep(sd_infos$sd_trf, 2), rep(eff1_infos$eff1_trf, 2),
                        rep(eff2_infos$eff2_trf, 2), rep(orth1_infos$orth1_trf, 2),
                        rep(orth2_infos$orth2_trf, 2), ac_infos$ac_trf,
                        rep(seas_infos$seas_trf, 2), rep(sn_infos$sn_trf, 2))
    evals <- rbind(theil_infos$theil_q, tat_infos$t_q, tat_infos$at_q, sd_infos$sd_m_q,
                   sd_infos$sd_r_q, eff1_infos$eff1_m_q, eff1_infos$eff1_r_q,
                   eff2_infos$eff2_m_q, eff2_infos$eff2_r_q, orth1_infos$orth1_m_q,
                   orth1_infos$orth1_r_q, orth2_infos$orth2_m_q, orth2_infos$orth2_r_q,
                   ac_infos$ac_q, seas_infos$seas_lb_q, seas_infos$seas_fd_q,
                   sn_infos$sn_noise_q, sn_infos$sn_news_q)
    summary_table <- data.frame(transformation, evals, row.names = lbl)
    colnames(summary_table) <- c("Transf.", colnames(rv))

    diagnostics_table <- list(slope_and_drift = sd_infos$sd_diagnostics,
                              efficiency1 = eff1_infos$eff1_diagnostics,
                              efficiency2 = eff2_infos$eff2_diagnostics,
                              orthogonality1 = orth1_infos$orth1_diagnostics,
                              orthogonality2 = orth2_infos$orth2_diagnostics)

    options(scipen = 0)

    return(
        structure(list(
            call = cl,
            revisions = rv_notrf,
            descriptive.statistics = ds,
            summary = summary_table,
            summary.residuals = diagnostics_table,
            relevancy = list(theil = theil_infos$theil_rslt),
            bias = list(t_ta_test = tat_infos$tat_rslt,
                        slope_and_drift = sd_infos$sd_rslt),
            efficiency = list(efficiency1 = eff1_infos$eff1_rslt,
                              efficiency2 = eff2_infos$eff2_rslt),
            orthogonality = list(orthogonality1 = orth1_infos$orth1_rslt,
                                 orthogonality2 = orth2_infos$orth2_rslt,
                                 autocorrelation_test = ac_infos$ac_rslt,
                                 seasonality_test = seas_infos$seas_rslt),
            signalnoise = list(signal_noise = sn_infos$sn_rslt),
            varbased = var_based_rslt
        ), class = "rjd3rev_rslts")
    )
}

# get_vintages_view function ---------------------------------------------------
get_vintages_view <- function(vintages, transf.log, view, n.releases) {

    if (view == "vertical") {
        vt <- vintages$vertical_view
    } else if (view == "diagonal") {
        n.releases <- min(n.releases, ncol(vintages$diagonal_view))
        vt <- vintages$diagonal_view[, 1:n.releases]
    }

    if (transf.log) {
        if (length(vt[vt[!is.na(vt)] < 0]) == 0) {
            is_log <- TRUE
            vt <- log(vt)
        } else {
            warning("Logarithm transformation incompatible with negative data. No transformation considered.", call. = FALSE)
            is_log <- FALSE
        }
    } else {
        is_log <- FALSE
    }

    return(list(is_log = is_log, vt = vt))
}


# test interpretors / evaluators -----------------------------------------------
ur_test_intepretor <- function(ur) {
    if (!is.null(ur)) {
        ur_ADFpvals <- ur[, "ADF.pvalue"]
        pc_signif_ur <- length(ur_ADFpvals[ur_ADFpvals < .05]) / length(ur_ADFpvals)
        is_stationary <- ifelse(pc_signif_ur > .8, TRUE, FALSE)
    } else {
        is_stationary <- TRUE
    }
    return(is_stationary)
}

coint_test_interpretor <- function(coint, is_stationary) {
    if (is_stationary) {
        is_cointegrated <- TRUE
    } else {
        if (!is.null(coint)) {
            coint_pvals <- coint[, "pvalue"]
            pc_signif_coint <- length(coint_pvals[coint_pvals < .05]) / length(coint_pvals)
            is_cointegrated <- ifelse(pc_signif_coint > .8, TRUE, FALSE)
        } else {
            is_cointegrated <- FALSE
        }
    }
    return(is_cointegrated)
}

seasonality_test <- function(x) {

    if (stats::frequency(x) > 1) {
        x_diff <- diff(x)
        lb_pval <- try(seasonality_qs(x_diff, stats::frequency(x))$pvalue, silent = TRUE) # Ljung-Box
        fd_pval <- try(seasonality_friedman(x_diff, stats::frequency(x))$pvalue, silent = TRUE) # Friedman non-parametric test

        test_succeeded <- c(!"try-error" %in% class(lb_pval), !"try-error" %in% class(fd_pval))
        if (all(test_succeeded)) {
            pvals <- c(lb_pval, fd_pval)
            seasonality <- ifelse(length(pvals[which(pvals < .05)]) == 2, TRUE, FALSE)
        } else if (any(test_succeeded)) {
            if (test_succeeded[1]) {
                seasonality <- ifelse(lb_pval < .01, TRUE, FALSE)
            } else if (test_succeeded[2]) {
                seasonality <- ifelse(fd_pval < .01, TRUE, FALSE)
            }
        } else {
            seasonality <- FALSE
        }
    } else {
        seasonality <- FALSE
    }
    return(seasonality)
}

theil_test_evaluator <- function(U1, U2, N, n_test, thr) {
    if (!is.null(U1)) {
        theil_rslt <- round(rbind(N, U1), 3)
        if (!is.null(U2) && !all(is.nan(U2)) == TRUE) {
            theil_rslt <- rbind(theil_rslt, U2 = round(U2, 3))
            theil_q <- eval_test(U2, threshold = thr$u2, ascending = FALSE)
            U_det <- "U2"
        } else {
            theil_rslt <- rbind(theil_rslt, U2 = rep(NA, length(U1)))
            theil_q <- eval_test(U1, threshold = thr$u1, ascending = FALSE)
            U_det <- "U1"
            warning("Theil U2 could not be calculated. Theil U1 is considered instead in summary().", call. = FALSE)
        }
    } else {
        theil_rslt <- NULL
        theil_q <- rep(NA, n_test)
        U_det <- ""
    }
    return(list(theil_rslt = theil_rslt, theil_q = theil_q, U_det = U_det, theil_trf = "None"))
}

tat_test_evaluator <- function(tat, is_log, n_test, thr) {
    if (!is.null(tat)) {
        tat_rslt <- t(round(tat, 3))
        t_q <- eval_test(tat[, "pvalue"], threshold = thr$t)
        at_q <- eval_test(tat[, "pvalue.adjusted"], threshold = thr$at)
    } else {
        tat_rslt <- NULL
        t_q <- at_q <- rep(NA, n_test)
    }

    tat_trf <- ifelse(is_log, "Log", "None")

    return(list(tat_rslt = tat_rslt, t_q = t_q, at_q = at_q, tat_trf = tat_trf))
}

sd_test_evaluator <- function(sd, is_log, is_cointegrated, delta_diff, n_test, thr, thr_res) {
    if (!is.null(sd)) {
        sd_rslt <- format_reg_output(sd, is_log, !is_cointegrated)
        sd_m_q <- eval_test(sd[, "intercept.pvalue"], threshold = thr)
        sd_r_q <- eval_test(sd[, "slope.pvalue"], threshold = thr)
        sd_diagnostics <- regression_diagnostics(sd, thr_res)
    } else {
        sd_rslt <- sd_diagnostics <- NULL
        sd_m_q <- sd_r_q <- rep(NA, n_test)
    }

    sd_trf <- if (is_cointegrated && !is_log) "None" else
        if (is_cointegrated && is_log) "Log" else
            if (!is_cointegrated && !is_log) paste("Delta", delta_diff) else
                paste("Delta-Log", delta_diff)

    return(list(sd_rslt = sd_rslt, sd_m_q = sd_m_q, sd_r_q = sd_r_q,
                sd_diagnostics = sd_diagnostics, sd_trf = sd_trf))
}

eff1_test_evaluator <- function(eff1, is_log, is_stationary, delta_diff, n_test, thr, thr_res) {

    if (!is.null(eff1)) {
        eff1_rslt <- format_reg_output(eff1, is_log, !is_stationary)
        eff1_m_q <- eval_test(eff1[, "intercept.pvalue"], threshold = thr)
        eff1_r_q <- eval_test(eff1[, "slope.pvalue"], threshold = thr)
        eff1_diagnostics <- regression_diagnostics(eff1, thr_res)
    } else {
        eff1_rslt <- eff1_diagnostics <- NULL
        eff1_m_q <- eff1_r_q <- rep(NA, n_test)
    }

    eff1_trf <- if (is_stationary && !is_log) "None" else
        if (is_stationary && is_log) "Log" else
            if (!is_stationary && !is_log) paste("Delta", delta_diff) else
                paste("Delta-Log", delta_diff)

    return(list(eff1_rslt = eff1_rslt, eff1_m_q = eff1_m_q, eff1_r_q = eff1_r_q,
                eff1_diagnostics = eff1_diagnostics, eff1_trf = eff1_trf))
}

eff2_test_evaluator <- function(eff2, is_log, n_test, thr, thr_res) {
    if (!is.null(eff2)) {
        eff2_rslt <- format_reg_output(eff2, is_log, FALSE)
        eff2_m_q <- c("", eval_test(eff2[, "intercept.pvalue"], threshold = thr))
        eff2_r_q <- c("", eval_test(eff2[, "slope.pvalue"], threshold = thr))
        eff2_diagnostics <- regression_diagnostics(eff2, thr_res)
    } else {
        eff2_rslt <- eff2_diagnostics <- NULL
        eff2_m_q <- eff2_r_q <- c("", rep(NA, n_test))
    }

    eff2_trf <- ifelse(is_log, "Log", "None")

    return(list(eff2_rslt = eff2_rslt, eff2_m_q = eff2_m_q, eff2_r_q = eff2_r_q,
                eff2_diagnostics = eff2_diagnostics, eff2_trf = eff2_trf))
}

orth1_test_evaluator <- function(orth1, is_log, nrevs, ncol_rv, n_test, thr, thr_res) {
    if (!is.null(orth1)) {
        orth1_rslt <- format_reg_output(orth1, is_log, FALSE)
        orth1_m_q <- c(rep("", nrevs), eval_test(orth1[, "intercept.pvalue"], threshold = thr))
        orth1_r_q <- c(rep("", nrevs), eval_test(stats::pf(orth1[, "F"], nrevs, orth1[, "N"] - nrevs - 1), threshold = thr))
        orth1_diagnostics <- regression_diagnostics(orth1, thr_res)
    } else {
        orth1_rslt <- orth1_diagnostics <- NULL
        if (ncol_rv > nrevs) orth1_m_q <- orth1_r_q <- c(rep("", nrevs), rep(NA, n_test))
        else orth1_m_q <- orth1_r_q <- rep("", ncol_rv)
    }

    orth1_trf <- ifelse(is_log, "Log", "None")

    return(list(orth1_rslt = orth1_rslt, orth1_m_q = orth1_m_q, orth1_r_q = orth1_r_q,
                orth1_diagnostics = orth1_diagnostics, orth1_trf = orth1_trf))
}

orth2_test_evaluator <- function(orth2, is_log, ref, ncol_rv, n_test, thr, thr_res) {
    if (!is.null(orth2)) {
        orth2_rslt <- format_reg_output(orth2, is_log, FALSE)
        orth2_m_q <- c(rep("", ref), eval_test(orth2[, "intercept.pvalue"], threshold = thr))
        orth2_r_q <- c(rep("", ref), eval_test(orth2[, "slope.pvalue"], threshold = thr))
        orth2_diagnostics <- regression_diagnostics(orth2, thr_res)
    } else {
        orth2_rslt <- orth2_diagnostics <- NULL
        if (ncol_rv > ref) orth2_m_q <- orth2_r_q <- c(rep("", ref), rep(NA, n_test))
        else orth2_m_q <- orth2_r_q <- rep("", ncol_rv)
    }

    orth2_trf <- ifelse(is_log, "Log", "None")

    return(list(orth2_rslt = orth2_rslt, orth2_m_q = orth2_m_q, orth2_r_q = orth2_r_q,
                orth2_diagnostics = orth2_diagnostics, orth2_trf = orth2_trf))
}

ac_test_evaluator <- function(ac, is_log, cnames, n_test, thr) {
    ac_trf <- ifelse(is_log, "Log", "None")
    ac_trf_str <- ifelse(ac_trf == "Log", get_info_transformation(TRUE, FALSE), get_info_transformation(FALSE, FALSE))

    if (!"try-error" %in% class(ac)) {
        pm_test_mat <- matrix(unlist(ac), ncol = 2, byrow = TRUE)[, , drop = FALSE]
        dimnames(pm_test_mat) <- list(cnames, c("value", "p.value"))
        ac_rslt <- list(info_transformation = ac_trf_str, estimates_ljungbox = pm_test_mat)
        ac_q <- eval_test(ac_rslt$estimates_ljungbox[, "p.value"], threshold = thr)
    } else {
        ac_rslt <- NULL
        ac_q <- rep(NA, n_test)
    }

    return(list(ac_rslt = ac_rslt, ac_q = ac_q, ac_trf = ac_trf))
}

seas_tests_evaluator <- function(lb_test, fd_test, is_log, cnames, freq, n_test, thr) {
    seas_trf <- ifelse(is_log, "Delta-Log 1", "Delta 1")
    seas_trf_str <- ifelse(seas_trf == "Delta-Log 1", get_info_transformation(TRUE, TRUE), get_info_transformation(FALSE, TRUE))

    if (!"try-error" %in% class(lb_test) && !"try-error" %in% class(fd_test) && freq > 1) {
        seas_rslt <- list(info_transformation = seas_trf_str,
                          estimates_ljungbox = matrix(unlist(lb_test), ncol = 2, byrow = TRUE, dimnames = list(cnames, c("value", "p.value"))),
                          estimates_friedman = matrix(unlist(fd_test), ncol = 2, byrow = TRUE, dimnames = list(cnames, c("value", "p.value"))))
        seas_lb_q <- eval_test(seas_rslt$estimates_ljungbox[, "p.value"], threshold = thr)
        seas_fd_q <- eval_test(seas_rslt$estimates_friedman[, "p.value"], threshold = thr)
    } else if (!"try-error" %in% class(lb_test) && freq > 1) {
        seas_rslt <- list(info_transformation = seas_trf_str,
                          estimates_ljungbox = matrix(unlist(lb_test), ncol = 2, byrow = TRUE, dimnames = list(cnames, c("value", "p.value"))),
                          estimates_friedman = NULL)
        seas_lb_q <- eval_test(seas_rslt$estimates_ljungbox[, "p.value"], threshold = thr)
        seas_fd_q <- rep(NA, n_test)
    } else if (!"try-error" %in% class(fd_test) && freq > 1) {
        seas_rslt <- list(info_transformation = seas_trf_str,
                          estimates_ljungbox = NULL,
                          estimates_friedman = matrix(unlist(fd_test), ncol = 2, byrow = TRUE, dimnames = list(cnames, c("value", "p.value"))))
        seas_lb_q <- rep(NA, n_test)
        seas_fd_q <- eval_test(seas_rslt$estimates_friedman[, "p.value"], threshold = thr)
    } else {
        seas_rslt <- NULL
        seas_lb_q <- seas_fd_q <- rep(NA, n_test)
    }

    return(list(seas_rslt = seas_rslt, seas_lb_q = seas_lb_q, seas_fd_q = seas_fd_q, seas_trf = seas_trf))
}

sn_test_evaluator <- function(sn, is_log, is_stationary, delta_diff, n_test, thr) {

    if (!is.null(sn)) {
        sn_rslt <- list(info_transformation = get_info_transformation(is_log, !is_stationary),
                        estimates = t(round(sn, 3)))
        sn_noise_q <- eval_test(sn[, "Noise.pvalue"], threshold = thr$noise)
        sn_news_q <- eval_test(sn[, "News.pvalue"], threshold = thr$news, ascending = FALSE)
    } else {
        sn_rslt <- NULL
        sn_noise_q <- sn_news_q <- rep(NA, n_test)
    }

    sn_trf <- if (is_stationary && !is_log) "None" else
        if (is_stationary && is_log) "Log" else
            if (!is_stationary && !is_log) paste("Delta", delta_diff) else
                paste("Delta-Log", delta_diff)

    return(list(sn_rslt = sn_rslt, sn_noise_q = sn_noise_q, sn_news_q = sn_news_q,
                sn_trf = sn_trf))
}


# p-values evaluators ----------------------------------------------------------

eval_test <- function(val,
                      threshold,
                      ascending = TRUE) {

    if(!all(tolower(names(threshold)) %in% c("good", "uncertain", "bad", "severe"))){
        stop("Possible values for quality assessment are 'good', 'uncertain', 'bad', 'severe'. Please check your user-defined options.", call. = FALSE)
    }
    if(is.unsorted(threshold)){
        stop("User-defined options must be defined in an ascending order. See doc or the file 'options.R' for more information.", call. = FALSE)
    }

    val <- as.numeric(val)
    n <- length(val)
    nt <- length(threshold)
    qualities <- c()

    for(i in 1:n){
        quality <- "good"

        if(!is.na(val[i])){
            if(ascending){
                for (k in 1:nt){
                    if(val[i] < threshold[k]){
                        quality <- names(threshold)[k]
                        break
                    }
                }
            }else{
                for (k in nt:1){
                    if(val[i] > threshold[k]){
                        quality <- names(threshold)[k]
                        break
                    }
                }
            }
        }

        quality_fmt <- paste0(paste0(toupper(substr(quality, 1, 1)), substr(quality, 2, nchar(quality))),
                              " (", trimws(format(round(val[i], 3), nsmall = 3)), ")")
        qualities <- c(qualities, quality_fmt)
    }

    return(qualities)
}

# eval_U <- function(U, U2 = TRUE) {
#     U <- as.numeric(U)
#
#     if (U2) {
#         quality <- ifelse(U >= 1, "Severe",
#                           ifelse(U > .9, "Bad",
#                                  ifelse(U > .8, "Uncertain", "Good")))
#     } else {
#         quality <- ifelse(U > .99, "Severe",
#                           ifelse(U > .9, "Bad",
#                                  ifelse(U > .8, "Uncertain", "Good")))
#     }
#     quality[is.na(quality)] <- "Good"
#
#     return(paste0(quality, " (", trimws(format(round(U, 3), nsmall = 3)), ")"))
# }


# other utility functions ------------------------------------------------------

format_reg_output <- function(x, is_log, is_diff) {
    info_transformation <- get_info_transformation(is_log, is_diff)

    x_df <- as.data.frame(t(round(x, 3)))
    n <- nrow(x_df)
    estim <- x_df[1:(n - 13), , drop = FALSE]

    norm_test <- list(Jarque_Bera_test = x_df[(n - 12):(n - 9), , drop = FALSE])
    hsk_test <- list(Breusch_Pagan_test = x_df[(n - 8):(n - 6), , drop = FALSE], White_test = x_df[(n - 5):(n - 3), , drop = FALSE], ARCH_test = x_df[(n - 2):n, , drop = FALSE])
    tests <- list(normality = norm_test, homoskedasticity = hsk_test)

    return(list(info_transformation = info_transformation, estimates = estim, residuals = tests))
}

get_info_transformation <- function(is_log, is_diff) {
    if (is_log) {
        info_transformation <- "Series have been log-transformed"
        if (is_diff) info_transformation <- paste(info_transformation, "and differentiated")
    } else {
        if (is_diff) {
            info_transformation <- "Series have been differentiated"
        } else {
            info_transformation <- "No transformation"
        }
    }
    return(info_transformation)
}

regression_diagnostics <- function(reg_output, thr) {

    jb <- eval_test(reg_output[, "JarqueBera.pvalue"], threshold = thr$jb)
    bp <- eval_test(reg_output[, "BreuschPagan.pvalue"], threshold = thr$bp)
    wh <- eval_test(reg_output[, "White.pvalue"], threshold = thr$white)
    arch <- eval_test(reg_output[, "arch.pvalue"], threshold = thr$arch)

    lbl <- c("Jarque-Bera", "Breusch-Pagan", "White", "ARCH")
    tests <- c("Normality", rep("Homoskedasticity", 3))
    tests_rslts <- rbind(jb, bp, wh, arch)
    colnames(tests_rslts) <- rownames(reg_output)

    rslt <- data.frame(Test = tests, tests_rslts, row.names = lbl)
    colnames(rslt) <- c("Test category", colnames(tests_rslts))
    return(rslt)
}


# Generic functions ------------------------------------------------------------

#' Print function for objects of class "rjd3rev_rslts"
#'
#' @param x an object of class "rjd3rev_rslts"
#' @param \dots further arguments passed to the print() function.
#' @export
#' @exportS3Method print rjd3rev_rslts
#'
print.rjd3rev_rslts <- function(x, ...) {

    print(list(call = x$call,
               descriptive_statistics = x$descriptive.statistics,
               parametric_analysis = x$summary, ...))
}

#' Summary function for objects of class "rjd3rev_rslts"
#'
#' @param object an object of class "rjd3rev_rslts"
#' @param ... further arguments passed to or from other methods.
#' @exportS3Method summary rjd3rev_rslts
#' @export
#'
summary.rjd3rev_rslts <- function(object, ...) {

    x <- object
    if (!requireNamespace("formattable", quietly = TRUE)) {
        warning("Please install 'formattable': install.packages('formattable') to get more visual output")
        return(x$summary)
    } else {
        format_font <- function(x) {
            formattable::formatter("span",
                                   style = x ~ formattable::style(color = ifelse(substr(x, 1, 1) == "G", "green",
                                                                                 ifelse(substr(x, 1, 1) == "U", "orange", "red")),
                                                                  font.weight = ifelse(substr(x, 1, 1) == "S", "bold", "plain")))
        }
        nc <- ncol(x$summary)
        return(list(formattable::formattable(x$summary, apply(x$summary[, 2:nc, drop = FALSE], 2, format_font))))
    }
}
