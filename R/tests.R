descriptiveStatNames <- c("N", "mean revision", "st.dev.", "min", "q.10", "median", "q.90", "max", "% positive", "% zero", "% negative", "mean absolute revision", "root mean square revision")

biasNames <- c("N", "estimate", "stderr", "tstat", "pvalue", "ar(1)", "stderr.adjusted", "tstat.adjusted", "pvalue.adjusted")

OlsNames <- c("N", "R2", "F", "intercept.estimate", "intercept.stderr", "intercept.pvalue",
              "slope.estimate", "slope.stderr", "slope.pvalue",
              "skewness", "kurtosis", "JarqueBera.value", "JarqueBera.pvalue",
              "BreuschPagan.R2", "BreuschPagan.value", "BreuschPagan.pvalue",
              "White.R2", "White.value", "White.pvalue",
              "arch.R2", "arch.value", "arch.pvalue")

OlsTestNames <- c("skewness", "kurtosis", "JarqueBera.value", "JarqueBera.pvalue",
                  "BreuschPagan.R2", "BreuschPagan.value", "BreuschPagan.pvalue",
                  "White.R2", "White.value", "White.pvalue",
                  "arch.R2", "arch.value", "arch.pvalue")

OlsAdjNames <- c("N", "R2", "F")

acNames <- c("BreuschGodfrey.R2", "BreuschGodfrey.value", "BreuschGodfrey.pvalue",
             "LungBox.value", "LungBox.pvalue")

urNames <- c("DF.value", "DF.stderr", "DF.statistic", "DF.pvalue",
             "ADF.value", "ADF.stderr", "ADF.statistic", "ADF.pvalue",
             "DFCT.value", "DFCT.stderr", "DFCT.statistic", "DFCT.pvalue",
             "PP.value", "PP.stderr", "PP.statistic", "PP.pvalue")

egNames <- c("value", "stderr", "statistic", "pvalue")

snNames <- c("News.R2", "News.F", "News.pvalue", "Noise.R2", "Noise.F", "Noise.pvalue")


OlsCNames <- function(nregs) {
    n <- c("intercept.estimate", "intercept.stderr", "intercept.pvalue")
    for (i in 1:nregs) {
        cur <- paste0("x(", i, ")")
        n <- c(n, paste0(cur, ".estimate"), paste0(cur, ".stderr"), paste0(cur, ".pvalue"))
    }
    return(n)
}

OlsAllNames <- function(nregs) {
    return(c(OlsAdjNames, OlsCNames(nregs), OlsTestNames))
}

vecmAllNames <- function(lag) {
    t <- m <- c()
    for (i in lag:1) {
        t <- c(t, paste0("trace(", i, ")"))
        m <- c(m, paste0("max(", i, ")"))
    }
    return(c(t, m))
}

matrix_jd2r <- function(s) {
    if (is.jnull(s)) {
        return(NULL)
    }
    nr <- .jcall(s, "I", "getRowsCount")
    nc <- .jcall(s, "I", "getColumnsCount")
    d <- .jcall(s, "[D", "toArray")
    return(array(d, dim = c(nr, nc)))
}

matrix_r2jd <- function(s) {
    if (is.null(s))
        return(.jnull("jdplus/toolkit/base/api/math/matrices/Matrix"))
    if (!is.matrix(s)) {
        s <- matrix(s, nrow = length(s), ncol = 1)
    }
    sdim <- dim(s)
    return(.jcall("jdplus/toolkit/base/api/math/matrices/Matrix", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "of", as.double(s), as.integer(sdim[1]), as.integer(sdim[2])))
}


#' Descriptive statistics
#'
#' @param revisions.view mts object. Vertical or diagonal view of the
#'                       `get_revisions()` output
#' @param rounding number of decimals to display
#'
#' @export
#' @examples
#'
#' ## Simulated data
#' df_long <- simulate_long(
#'     n_period = 10L * 4L,
#'     n_revision = 5L,
#'     periodicity = 4L,
#'     start_period = as.Date("2010-01-01")
#' )
#'
#' ## Create vintage and get descriptive statistics of revisions
#' vintages <- create_vintages(df_long, periodicity = 4)
#' revisions <- get_revisions(vintages, gap = 1)
#' descriptive_statistics(revisions$diagonal_view, rounding = 1)
#'
descriptive_statistics <- function(revisions.view, rounding = 3) {

    descriptive_statistics_one <- function(r) {
        rc <- as.numeric(r[!is.na(r)])

        n <- length(rc)
        mn <- mean(rc)
        sd <- sd(rc)
        min <- min(rc)
        q10 <- stats::quantile(rc, 0.1)
        q50 <- stats::median(rc)
        q90 <- stats::quantile(rc, 0.9)
        max <- max(rc)
        pp <- length(rc[rc > 0]) / n
        p0 <- length(rc[rc == 0]) / n
        pn <- 1 - pp - p0
        mar <- mean(abs(rc))
        rmsr <- sqrt(mean(rc^2))

        return(c(n, mn, sd, min, q10, q50, q90, max, pp, p0, pn, mar, rmsr))
    }

    ds <- apply(revisions.view, 2, descriptive_statistics_one)

    output <- round(ds, rounding)
    rownames(output) <- descriptiveStatNames

    return(output)
}

#' Theil's Inequality Coefficient U1
#'
#' @param vintages.view mts object. Vertical or diagonal view of the
#'                      `create_vintages()` output
#' @param gap Integer. Gap to consider between each vintages. Default is 1
#'            which means that revisions are calculated and tested for each
#'            vintages consecutively.
#' @param na.zero Boolean whether missing values should be considered as 0 or
#'                rather as data not (yet) available (the default).
#'
#' @seealso `revision_analysis()`, `render_report()`
#'
#' @export
#'
#' @examples
#'
#' ## Simulated data
#' df_long <- simulate_long(
#'     n_period = 10L * 4L,
#'     n_revision = 5L,
#'     periodicity = 4L,
#'     start_period = as.Date("2010-01-01")
#' )
#'
#' ## Create vintage and test
#' vintages <- create_vintages(df_long, periodicity = 4)
#' theil(vintages$diagonal_view)
#'
theil <- function(vintages.view, gap = 1, na.zero = FALSE) {
    q <- vintages.view
    if (na.zero) q[is.na(q)] <- 0
    jq <- matrix_r2jd(q)
    theil <- try(.jcall("jdplus/revisions/base/r/Utility", "[D", "theil", jq, as.integer(gap)), silent = TRUE)
    if (inherits(theil, "try-error")) {
        warning("theil could not be performed", call. = FALSE)
        return(NULL)
    }
    return(theil)
}

#' Theil's Inequality Coefficient U2
#'
#' @param vintages.view mts object. Vertical or diagonal view of the
#'                      `create_vintages()` output
#' @param gap Integer. Gap to consider between each vintages. Default is 1
#'            which means that revisions are calculated and tested for each
#'            vintages consecutively..
#' @param na.zero Boolean whether missing values should be considered as 0 or
#'                rather as data not (yet) available (the default).
#' @seealso `revision_analysis()`, `render_report()`
#'
#' @export
#'
#' @examples
#'
#' ## Simulated data
#' df_long <- simulate_long(
#'     n_period = 10L * 4L,
#'     n_revision = 5L,
#'     periodicity = 4L,
#'     start_period = as.Date("2010-01-01")
#' )
#'
#' ## Create vintage and test
#' vintages <- create_vintages(df_long, periodicity = 4)
#' theil2(vintages$diagonal_view)
#'
theil2 <- function(vintages.view, gap = 1, na.zero = FALSE) {
    q <- vintages.view
    if (na.zero) q[is.na(q)] <- 0
    jq <- matrix_r2jd(q)
    theil2 <- try(.jcall("jdplus/revisions/base/r/Utility", "[D", "theil2", jq, as.integer(gap)), silent = TRUE)
    if (inherits(theil2, "try-error")) {
        warning("theil2 could not be performed", call. = FALSE)
        return(NULL)
    }
    return(theil2)
}

#' Estimate bias using t-test and augmented t-test
#'
#' @param revisions.view mts object. Vertical or diagonal view of the
#'                       `get_revisions()` output
#' @param na.zero Boolean whether missing values should be considered as 0 or
#'                rather as data not (yet) available (the default).
#' @seealso `revision_analysis()`, `render_report()`
#'
#' @export
#'
#' @examples
#'
#' ## Simulated data
#' df_long <- simulate_long(
#'     n_period = 10L * 4L,
#'     n_revision = 5L,
#'     periodicity = 4L,
#'     start_period = as.Date("2010-01-01")
#' )
#'
#' ## Create vintage and test
#' vintages <- create_vintages(df_long, periodicity = 4)
#' revisions <- get_revisions(vintages, gap = 1)
#' bias(revisions$diagonal_view)
#'

bias <- function(revisions.view, na.zero = FALSE) {
    r <- revisions.view
    if (na.zero) r[is.na(r)] <- 0
    jrevs <- matrix_r2jd(r)
    jbias <- try(.jcall("jdplus/revisions/base/r/Utility", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "bias", jrevs), silent = TRUE)
    if (inherits(bias, "try-error")) {
        warning("bias could not be performed", call. = FALSE)
        return(NULL)
    }
    bias <- matrix_jd2r(jbias)
    bias[rowSums(bias[]) == 0, ] <- NaN # fix non-calculable cases
    if (all(is.nan(bias))) {
        warning("bias could not be performed", call. = FALSE)
        return(NULL)
    }
    colnames(bias) <- biasNames
    rownames(bias) <- colnames(r)
    return(bias)
}


#' Slope and Drift
#'
#' Linear regression model of a latter vintage (L) on a preliminary vintage (P)
#'
#' @param vintages.view mts object. Vertical or diagonal view of the
#'                      `create_vintages()` output
#' @param gap Integer. Gap to consider between each vintages. Default is 1
#'            which means that revisions are calculated and tested for each
#'            vintages consecutively.
#' @param na.zero Boolean whether missing values should be considered as 0 or
#'                rather as data not (yet) available (the default).
#'
#' @seealso `revision_analysis()`, `render_report()`
#'
#' @export
#'
#' @examples
#'
#' ## Simulated data
#' df_long <- simulate_long(
#'     n_period = 10L * 4L,
#'     n_revision = 5L,
#'     periodicity = 4L,
#'     start_period = as.Date("2010-01-01")
#' )
#'
#' ## Create vintage and test
#' vintages <- create_vintages(df_long, periodicity = 4L)
#' slope_and_drift(vintages$diagonal_view)
#'
slope_and_drift <- function(vintages.view, gap = 1, na.zero = FALSE) {
    q <- vintages.view
    if (na.zero) q[is.na(q)] <- 0
    jq <- matrix_r2jd(q)
    jsd <- try(.jcall("jdplus/revisions/base/r/Utility", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "slopeAndDrift", jq, as.integer(gap)), silent = TRUE)
    if (inherits(jsd, "try-error")) {
        warning("Slope and drift could not be performed", call. = FALSE)
        return(NULL)
    }
    slope_and_drift <- matrix_jd2r(jsd)
    slope_and_drift[rowSums(slope_and_drift[]) == 0, ] <- NaN # fix non-calculable cases
    if (all(is.nan(slope_and_drift))) {
        warning("slope_and_drift could not be performed", call. = FALSE)
        return(NULL)
    }
    colnames(slope_and_drift) <- OlsNames
    rownames(slope_and_drift) <- colnames(q)[-(1:gap)]
    return(slope_and_drift)
}


#' Efficiency Model 1
#'
#' Linear regression model of the revisions (R) on a preliminary vintage (P)
#'
#' @param vintages.view mts object. Vertical or diagonal view of the
#'                      `create_vintages()` output
#' @param gap Integer. Gap to consider between each vintages. Default is 1
#'            which means that revisions are calculated and tested for each
#'            vintages consecutively.
#' @param na.zero Boolean whether missing values should be considered as 0 or
#'                rather as data not (yet) available (the default).
#'
#' @seealso `revision_analysis()`, `render_report()`
#'
#' @export
#'
#' @examples
#'
#' ## Simulated data
#' df_long <- simulate_long(
#'     n_period = 10L * 4L,
#'     n_revision = 5L,
#'     periodicity = 4L,
#'     start_period = as.Date("2010-01-01")
#' )
#'
#' ## Create vintage and test
#' vintages <- create_vintages(df_long, periodicity = 4L)
#' efficiencyModel1(vintages$diagonal_view)
#'
efficiencyModel1 <- function(vintages.view, gap = 1, na.zero = FALSE) {
    q <- vintages.view
    if (na.zero) q[is.na(q)] <- 0
    jq <- matrix_r2jd(q)
    jef1 <- try(.jcall("jdplus/revisions/base/r/Utility", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "efficiencyModel1", jq, as.integer(gap)), silent = TRUE)
    if (inherits(jef1, "try-error")) {
        warning("efficiencyModel1 could not be performed", call. = FALSE)
        return(NULL)
    }
    efficiencyModel1 <- matrix_jd2r(jef1)
    efficiencyModel1[rowSums(efficiencyModel1[]) == 0, ] <- NaN # fix non-calculable cases
    if (all(is.nan(efficiencyModel1))) {
        warning("efficiencyModel1 could not be performed", call. = FALSE)
        return(NULL)
    }
    colnames(efficiencyModel1) <- OlsNames
    n <- dim(q)[2]
    w <- sapply(colnames(q), function(s) paste0("[", s, "]"))
    rw <- mapply(function(a, b) paste(a, b, sep = "-"), w[(gap + 1):n], w[1:(n - gap)])
    rownames(efficiencyModel1) <- rw
    return(efficiencyModel1)
}

#' Efficiency Model 2
#'
#' Linear regression model of R_v on R_\{v-1\}
#'
#' @param vintages.view mts object. Vertical or diagonal view of the
#'                      `create_vintages()` output
#' @param gap Integer. Gap to consider between each vintages. Default is 1
#'            which means that revisions are calculated and tested for each
#'            vintages consecutively.
#' @param na.zero Boolean whether missing values should be considered as 0 or
#'                rather as data not (yet) available (the default).
#' @seealso `revision_analysis()`, `render_report()`
#'
#' @export
#'
#' @examples
#'
#' ## Simulated data
#' df_long <- simulate_long(
#'     n_period = 10L * 4L,
#'     n_revision = 5L,
#'     periodicity = 4L,
#'     start_period = as.Date("2010-01-01")
#' )
#'
#' ## Create vintage and test
#' vintages <- create_vintages(df_long, periodicity = 4L)
#' efficiencyModel2(vintages$diagonal_view)
#'
efficiencyModel2 <- function(vintages.view, gap = 1, na.zero = FALSE) {
    q <- vintages.view
    if (na.zero) q[is.na(q)] <- 0
    jq <- matrix_r2jd(q)
    jef2 <- try(.jcall("jdplus/revisions/base/r/Utility", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "efficiencyModel2", jq, as.integer(gap)), silent = TRUE)
    if (inherits(jef2, "try-error")) {
        warning("efficiencyModel2 could not be performed", call. = FALSE)
        return(NULL)
    }
    efficiencyModel2 <- matrix_jd2r(jef2)
    if (length(efficiencyModel2) == 0) {
        warning("efficiencyModel2 could not be performed: Too few number of vintages", call. = FALSE)
        return(NULL)
    }
    efficiencyModel2[rowSums(efficiencyModel2[]) == 0, ] <- NaN # fix non-calculable cases
    if (all(is.nan(efficiencyModel2))) {
        warning("efficiencyModel2 could not be performed", call. = FALSE)
        return(NULL)
    }
    colnames(efficiencyModel2) <- OlsNames
    n <- dim(q)[2]
    w <- sapply(colnames(q), function(s) paste0("[", s, "]"))
    rw <- mapply(function(a, b) paste(a, b, sep = "-"), w[(gap + 1):n], w[1:(n - gap)])
    rownames(efficiencyModel2) <- rw[-1]
    return(efficiencyModel2)
}

#' Orthogonally Model 1
#'
#' Linear regression model of R_v on R_\{v-1\},...,R_\{v-p\}. (p=nrevs)
#'
#' @param revisions.view mts object. Vertical or diagonal view of the
#'                       `get_revisions()` output
#' @param nrevs Integer. Number of lags to consider.
#' @param na.zero Boolean whether missing values should be considered as 0 or
#'                rather as data not (yet) available (the default).
#' @seealso `revision_analysis()`, `render_report()`
#'
#' @export
#'
#' @examples
#'
#' ## Simulated data
#' df_long <- simulate_long(
#'     n_period = 10L * 4L,
#'     n_revision = 5L,
#'     periodicity = 4L,
#'     start_period = as.Date("2010-01-01")
#' )
#'
#' ## Create vintage and test
#' vintages <- create_vintages(df_long, periodicity = 4L)
#' revisions <- get_revisions(vintages, gap = 1)
#' orthogonallyModel1(revisions$diagonal_view)
#'
orthogonallyModel1 <- function(revisions.view, nrevs = 1, na.zero = FALSE) {
    r <- revisions.view
    if (na.zero) r[is.na(r)] <- 0
    jr <- matrix_r2jd(as.matrix(r))
    jom <- try(.jcall("jdplus/revisions/base/r/Utility", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "orthogonallyModel1", jr, as.integer(nrevs)), silent = TRUE)
    if (inherits(jom, "try-error")) {
        warning("orthogonallyModel1 could not be performed", call. = FALSE)
        return(NULL)
    }
    om <- matrix_jd2r(jom)
    if (length(om) == 0) {
        warning("orthogonallyModel1 could not be performed: Too few number of vintages", call. = FALSE)
        return(NULL)
    }
    om[rowSums(om[]) == 0, ] <- NaN # fix non-calculable cases
    if (all(is.nan(om))) {
        warning("orthogonallyModel1 could not be performed", call. = FALSE)
        return(NULL)
    }
    colnames(om) <- OlsAllNames(nrevs)
    rownames(om) <- colnames(r)[-c(1:nrevs)]
    return(om)
}

#' Orthogonally Model 2
#'
#' Linear regression model of R_v on R_\{v-k\} (k = reference)
#'
#' @param revisions.view mts object. Vertical or diagonal view of the
#'                       `get_revisions()` output
#' @param reference Integer. Number of lags to consider.
#' @param na.zero Boolean whether missing values should be considered as 0 or
#'                rather as data not (yet) available (the default).
#' @seealso `revision_analysis()`, `render_report()`
#'
#' @export
#'
#' @examples
#'
#' ## Simulated data
#' df_long <- simulate_long(
#'     n_period = 10L * 4L,
#'     n_revision = 5L,
#'     periodicity = 4L,
#'     start_period = as.Date("2010-01-01")
#' )
#'
#' ## Create vintage and test
#' vintages <- create_vintages(df_long, periodicity = 4L)
#' revisions <- get_revisions(vintages, gap = 1)
#' orthogonallyModel2(revisions$diagonal_view)
#'
orthogonallyModel2 <- function(revisions.view, reference = 1, na.zero = FALSE) {
    r <- revisions.view
    if (na.zero) r[is.na(r)] <- 0
    jr <- matrix_r2jd(as.matrix(r))
    jom <- try(.jcall("jdplus/revisions/base/r/Utility", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "orthogonallyModel2", jr, as.integer(reference)), silent = TRUE)
    if (inherits(jom, "try-error")) {
        warning("orthogonallyModel2 could not be performed", call. = FALSE)
        return(NULL)
    }
    om <- matrix_jd2r(jom)
    if (length(om) == 0) {
        warning("orthogonallyModel2 could not be performed: Too few number of vintages", call. = FALSE)
        return(NULL)
    }
    om[rowSums(om[]) == 0, ] <- NaN # fix non-calculable cases
    if (all(is.nan(om))) {
        warning("orthogonallyModel2 could not be performed", call. = FALSE)
        return(NULL)
    }
    colnames(om) <- OlsNames
    rownames(om) <- colnames(r)[-c(1:reference)]
    return(om)
}

#' Signal VS Noise
#'
#' Linear regression models to determine whether revisions are ‘news’ or ‘noise’.
#' For 'noise': R (revisions) on P (preliminary estimate).
#' For 'news': R on L (latter estimate).
#'
#' @param vintages.view mts object. Vertical or diagonal view of the
#'                      `create_vintages()` output
#' @param gap Integer. Gap to consider between each vintages. Default is 1
#'            which means that revisions are calculated and tested for each
#'            vintages consecutively.
#' @param na.zero Boolean whether missing values should be considered as 0 or
#'                rather as data not (yet) available (the default).
#' @seealso `revision_analysis()`, `render_report()`
#'
#' @export
#'
#' @examples
#' ## Simulated data
#' df_long <- simulate_long(
#'     n_period = 10L * 4L,
#'     n_revision = 5L,
#'     periodicity = 4L,
#'     start_period = as.Date("2010-01-01")
#' )
#'
#' ## Create vintage and test
#' vintages <- create_vintages(df_long, periodicity = 4L)
#' signalnoise(vintages$diagonal_view)
#'
signalnoise <- function(vintages.view, gap = 1, na.zero = FALSE) {
    q <- vintages.view
    if (na.zero) q[is.na(q)] <- 0
    jq <- matrix_r2jd(q)
    jsd <- try(.jcall("jdplus/revisions/base/r/Utility", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "signalNoise", jq, as.integer(gap)), silent = TRUE)
    if (inherits(jsd, "try-error")) {
        warning("signalnoise could not be performed", call. = FALSE)
        return(NULL)
    }
    sn <- matrix_jd2r(jsd)
    sn[rowSums(sn[]) == 0, ] <- NaN # fix non-calculable cases
    if (all(is.nan(sn))) {
        warning("SignalNoise could not be performed", call. = FALSE)
        return(NULL)
    }
    colnames(sn) <- snNames
    n <- dim(q)[2]
    w <- sapply(colnames(q), function(s) paste0("[", s, "]"))
    rw <- mapply(function(a, b) paste(a, b, sep = "-"), w[(gap + 1):n], w[1:(n - gap)])
    rownames(sn) <- rw
    return(sn)
}

#' Unit root test
#'
#' @param vintages.view mts object. Vertical or diagonal view of the
#'                      `create_vintages()` output
#' @param adfk Number of lags to consider for Augmented Dicky-Fuller (ADF) test
#' @param na.zero Boolean whether missing values should be considered as 0 or
#'                rather as data not (yet) available (the default).
#' @seealso `revision_analysis()`, `render_report()`
#'
#' @export
#'
#' @examples
#' ## Simulated data
#' df_long <- simulate_long(
#'     n_period = 10L * 4L,
#'     n_revision = 5L,
#'     periodicity = 4L,
#'     start_period = as.Date("2010-01-01")
#' )
#'
#' ## Create vintage and test
#' vintages <- create_vintages(df_long, periodicity = 4L)
#' unitroot(vintages$diagonal_view)
#'
unitroot <- function(vintages.view, adfk = 1, na.zero = FALSE) {
    q <- vintages.view
    if (na.zero) q[is.na(q)] <- 0
    jq <- matrix_r2jd(q)
    jsd <- try(.jcall("jdplus/revisions/base/r/Utility", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "unitroot", jq, as.integer(adfk)), silent = TRUE)
    if (inherits(jsd, "try-error")) {
        warning("unit root test could not be performed", call. = FALSE)
        return(NULL)
    }
    ur <- matrix_jd2r(jsd)
    colnames(ur) <- urNames
    rownames(ur) <- colnames(q)
    return(ur)
}

#' Cointegration tests (Engle-Granger)
#'
#' @param vintages.view mts object. Vertical or diagonal view of the
#'                      `create_vintages()` output
#' @param adfk Number of lags to consider for ADF
#' @param na.zero Boolean whether missing values should be considered as 0 or
#'                rather as data not (yet) available (the default).
#' @seealso `revision_analysis()`, `render_report()`
#'
#' @export
#'
#' @examples
#'
#' ## Simulated data
#' df_long <- simulate_long(
#'     n_period = 10L * 4L,
#'     n_revision = 5L,
#'     periodicity = 4L,
#'     start_period = as.Date("2010-01-01")
#' )
#'
#' ## Create vintage and test
#' vintages <- create_vintages(df_long, periodicity = 4L)
#' cointegration(vintages$diagonal_view)
#'
cointegration <- function(vintages.view, adfk = 1, na.zero = FALSE) {
    q <- vintages.view
    if (na.zero) q[is.na(q)] <- 0
    jq <- matrix_r2jd(q)
    jsd <- try(.jcall("jdplus/revisions/base/r/Utility", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "cointegration", jq, as.integer(adfk)), silent = TRUE)
    if (inherits(jsd, "try-error")) {
        warning("cointegration test could not be performed", call. = FALSE)
        return(NULL)
    }
    eg <- matrix_jd2r(jsd)
    colnames(eg) <- egNames
    rownames(eg) <- get_rownames_diag(q, adfk)
    return(eg)
}

get_rownames_diag <- function(vt, gap) {
    # Example for n=4: v1_v2, v1_v3, v1_v4, v2_v3, v2_v4, v3_v4
    n <- ncol(vt)
    idx1 <- rep(1:(n - gap), (n - gap):1)
    idx0 <- sequence((n - gap):1) + rep(1:(n - gap), (n - gap):1)
    w <- sapply(colnames(vt), function(s) paste0("[", s, "]"))
    rw <- mapply(function(a, b) paste(a, b, sep = "_"), w[idx1], w[idx0])
    return(rw)
}

#' Vector error correction model (VECM)
#'
#' Can lead to a better understanding of the nature of any nonstationary process
#' among the different component series.
#'
#' @param vintages.view mts object. Vertical or diagonal view of the
#'                      `create_vintages()` output
#' @param lag Number of lags
#' @param model Character. Must be "none" (the default), "cnt" or "trend".
#' @param na.zero Boolean whether missing values should be considered as 0 or
#'                rather as data not (yet) available (the default).
#' @export
#'
#' @examples
#' ## Simulated data
#' df_long <- simulate_long(
#'     n_period = 10L * 4L,
#'     n_revision = 5L,
#'     periodicity = 4L,
#'     start_period = as.Date("2010-01-01")
#' )
#'
#' ## Create vintage and test
#' vintages <- create_vintages(df_long, periodicity = 4L)
#' vecm(vintages$diagonal_view)
#'
vecm <- function(vintages.view, lag = 2, model = c("none", "cnt", "trend"), na.zero = FALSE) {
    model <- match.arg(model)
    q <- vintages.view
    if (na.zero) q[is.na(q)] <- 0
    jq <- matrix_r2jd(q)
    jsd <- try(.jcall("jdplus/revisions/base/r/Utility", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "vecm", jq, as.integer(lag), model), silent = TRUE)
    if (inherits(jsd, "try-error")) {
        warning("vecm could not be performed", call. = FALSE)
        return(NULL)
    }
    vecm <- matrix_jd2r(jsd)
    colnames(vecm) <- vecmAllNames(lag)
    rownames(vecm) <- get_rownames_diag(q, 1)
    return(vecm)
}

# Auto-correlation tests on OLS residuals
#
# auto_correlation <- function(vintages.view, nbreuschgodfrey=1, nljungbox=1, na.zero = FALSE) {
#   q <- vintages.view
#   if (na.zero) q[is.na(q)] <- 0
#   jq <- matrix_r2jd(q)
#   jsd <- try(.jcall("jdplus/revisions/base/r/Utility", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "autoCorrelation", jq
#                   , as.integer(nbreuschgodfrey), as.integer(nljungbox)), silent = TRUE)
#   if ("try-error" %in% class(jsd)) {
#     warning("auto_correlation test on OLS residuals could not be performed", call. = FALSE)
#     return(NULL)
#   }
#   ac <- matrix_jd2r(jsd)
#   colnames(ac) <- acNames
#   rownames(ac) <- get_rownames_diag(q, 1)
#   return(ac)
# }
