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
#' running the function `get_report()` on the output of `revision_analysis()`
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
#'                   the parameter `vintage.selection` in `create_vintages()`
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
#'
#' @import rJava rjd3toolkit
#'
#' @seealso `create_vintages()` to create the input object,
#'          `get_report()` to get a summary and information the tests
#'
#' @return an object of class `"rjd3rev_revision_analysis"`
#'
#' @export
#'
#' @examples
#'
#' ## Simulated data
#' period_range<-seq(as.Date('2011-01-01'),as.Date('2020-10-01'),by='quarter')
#' qtr<-(as.numeric(substr(period_range,6,7))+2)/3
#' time_period<-rep(paste0(format(period_range, "%Y"), "Q", qtr),5)
#' np<-length(period_range)
#' rev_date<-c(rep("2021-06-30",np), rep("2021-12-31",np), rep("2022-06-30",np),
#'             rep("2022-12-31",np), rep("2023-06-30",np))
#' set.seed(1)
#' xt<-cumsum(sample(rnorm(1000,0,1), np, TRUE))
#' rev<-rnorm(np*4,0,.1)
#' obs_values<-xt
#' for(i in 1:4) {
#'   xt<-xt+rev[(1+(i-1)*np):(i*np)]
#'   obs_values<-c(obs_values,xt)
#' }
#' df<-data.frame(rev_date, time_period, obs_values)
#'
#' ## Create a `"rjd3rev_vintages"` object with the input
#' vintages<-create_vintages(df, periodicity = 4, revdate.format= "%Y-%m-%d")
#' # revisions<-get_revisions(vintages, gap = 1) # just to get a first insight of the revisions
#'
#' ## Call using all default parameters
#' rslt1<-revision_analysis(vintages)
#' #get_report(rslt1)
#' #summary(rslt1) # formatted summary only
#'
#' ## Calls using diagonal view (suited in many situations such as to evaluate GDP estimates)
#' ## Note: when input are not growth rates but the gross series, differentiation is
#' ## performed automatically (if transf.diff is let to its default option) but `transf.log`
#' ## must be set to TRUE manually whenever a log-transformation of the data is necessary
#' rslt2<-revision_analysis(vintages, gap = 1, view = "diagonal", n.releases = 3)
#' #get_report(rslt2)
#' #summary(rslt2)
#'
#' ## Call to evaluate revisions for a specific range of vintage periods
#' vintages<-create_vintages(df, periodicity = 4, vintage.selection = list(start="2021-12-31", end="2023-06-30"))
#' rslt3<-revision_analysis(vintages, gap=2, view = "vertical")
#' #get_report(rslt3)
#' #summary(rslt3)
#'
revision_analysis<-function(vintages,
                            gap = 1,
                            view = c("vertical", "diagonal"),
                            n.releases = 3,
                            transf.diff = c("auto", "forced", "none"),
                            transf.log = FALSE,
                            descriptive.rounding = 3,
                            nrevs = 1,
                            ref = 1,
                            na.zero=FALSE) {

  options(scipen = 999)

  cl<-match.call()

  out <- list()
  class(out) <- "rjd3rev_revision_analysis"

  view<-match.arg(view)
  transf.diff<-match.arg(transf.diff)

  if(is.null(vintages)) stop("No vintage found!")
  if(n.releases<(1+gap)) stop("'n.releases' must be >= (1+gap)")

  # Input selection

  ## Select vintage view
  if(view == "vertical") {
    vt<-vintages$vertical_view
  }else if (view == "diagonal") {
    vt<-vintages$diagonal_view[, 1:n.releases]
  }

  ## Revisions and Vintages Transformation
  rv_notrf<-get_vd_rev(vt, gap)
  freq<-frequency(vt)

  ### Log transformation
  if(transf.log) {
    if(length(vt[vt[!is.na(vt)] < 0])>0) {
      warning("Logarithm transformation incompatible with negative data. No transformation considered.", call.=FALSE)
      rv<-rv_notrf
      is_log<-FALSE
    } else {
      vt<-log(vt)
      rv<-get_vd_rev(vt, gap)
      is_log<-TRUE
    }
  } else {
    rv<-rv_notrf
    is_log<-FALSE
  }

  ### Differentiation
  ur<-unitroot(vt, adfk=1, na.zero) # H0: non-stationary
  if(!is.null(ur)) {
    ur_rslt<-t(round(ur, 3))
    ur_ADFpvals<-ur[, "ADF.pvalue"]
    pc_signif_ur<-length(ur_ADFpvals[ur_ADFpvals<.05])/length(ur_ADFpvals)
    is_stationary<-ifelse(pc_signif_ur>.8, TRUE, FALSE)
  } else {
    ur_rslt<-NULL
    is_stationary<-TRUE
  }

  coint<-cointegration(vt, adfk=1, na.zero) # H0: no-cointegration
  if(is_stationary) {
    is_cointegrated<-TRUE
  } else {
    if(!is.null(coint)) {
      coint_pvals<-coint[, "pvalue"]
      pc_signif_coint<-length(coint_pvals[coint_pvals<.05])/length(coint_pvals)
      is_cointegrated<-ifelse(pc_signif_coint>.8, TRUE, FALSE)
    } else {
      is_cointegrated<-FALSE
    }
  }

  seasonality<-apply(vt, 2, check_seasonality)
  nt<-sum(seasonality)
  is_seasonal<-ifelse(nt/ncol(vt)>.8, TRUE, FALSE)
  if(is_seasonal) {
    vt_diff<-diff(vt, freq)
    delta_diff<-freq
  } else {
    vt_diff<-diff(vt)
    delta_diff<-1
  }

  if(transf.diff == "auto") {
    vts<-if(is_stationary) vt else vt_diff
    vtc<-if(is_cointegrated) vt else vt_diff
  }else if(transf.diff == "forced") {
    vts<-vtc<-vt_diff
    is_stationary<-is_cointegrated<-FALSE
  }else if(transf.diff == "none") {
    vts<-vtc<-vt
    if(!is_stationary || !is_cointegrated) {
      warning("No differentiation considered even though stationarity and/or cointegration might not be present. This can lead to spurious regression.", call.=FALSE)
    }
    is_stationary<-is_cointegrated<-TRUE
  }

  # Descriptive statistics
  ds<-descriptive_statistics(rv_notrf, descriptive.rounding)

  # Parametric analysis

  ## I. Relevancy

  ### Theil U1 and U2
  N<-apply(rv_notrf, 2, function(x) length(x[!is.na(x)]))
  U_det<-"U"
  theil_trf<-"None"
  U1<-theil(vt, gap, na.zero)
  U2<-theil2(vt, gap, na.zero)
  if(!is.null(U1)) {
    theil_rslt<-round(rbind(N, U1), 3)
    if(!is.null(U2) && !all(is.nan(U2))==TRUE) {
      theil_rslt<-rbind(theil_rslt, U2=round(U2, 3))
      theil_q<-eval_U(U2)
      U_det<-"U2"
    } else {
      theil_rslt<-rbind(theil_rslt, U2=rep(NA, length(U1)))
      theil_q<-eval_U(U1, U2=FALSE)
      U_det<-"U1"
      warning("Theil U2 could not be calculated. Theil U1 is considered instead in summary().", call.=FALSE)
    }
  } else {
    theil_rslt<-NULL
    theil_q<-rep(NA, length(colnames(rv)))
  }

  ## II. Bias (mean and regression bias)

  ### II.1_2. T-test and Augmented T-test
  ta_trf<-ifelse(is_log, "Log", "None")
  ta<-bias(rv, na.zero)
  if(!is.null(ta)) {
    ta_rslt<-t(round(ta, 3))
    t_q<-eval_pvals(ta[, "pvalue"], h0_good=TRUE)
    ta_q<-eval_pvals(ta[, "pvalue.adjusted"], h0_good=TRUE)
  } else {
    ta_rslt<-NULL
    t_q<-ta_q<-rep(NA, length(colnames(rv)))
  }

  ### II.3. Slope and drift
  sd_trf<-if (is_cointegrated && !is_log) "None" else
    if (is_cointegrated && is_log) "Log" else
      if (!is_cointegrated && !is_log) paste("Delta", delta_diff) else
        paste("Delta-Log", delta_diff)
  sd<-slope_and_drift(vtc, gap, na.zero)
  if(!is.null(sd)) {
    sd_rslt<-format_reg_output(sd, is_log, !is_cointegrated)
    sd_m_q<-eval_pvals(sd[, "intercept.pvalue"], h0_good=TRUE)
    sd_r_q<-eval_pvals(sd[, "slope.pvalue"], h0_good=TRUE)
    sd_diagnostics<-regression_diagnostics(sd)
  } else {
    sd_rslt<-sd_diagnostics<-NULL
    sd_m_q<-sd_r_q<-rep(NA, length(colnames(vtc)[-c(1:gap)]))
  }

  ## III. Efficiency

  ### III.1. Regression of revision on previous estimate (=noise)
  eff1_trf<-if (is_stationary && !is_log) "None" else
    if (is_stationary && is_log) "Log" else
      if (!is_stationary && !is_log) paste("Delta", delta_diff) else
        paste("Delta-Log", delta_diff)
  eff1<-efficiencyModel1(vts, gap, na.zero)
  if(!is.null(eff1)) {
    eff1_rslt<-format_reg_output(eff1, is_log, !is_stationary)
    eff1_m_q<-eval_pvals(eff1[, "intercept.pvalue"], h0_good=TRUE)
    eff1_r_q<-eval_pvals(eff1[, "slope.pvalue"], h0_good=TRUE)
    eff1_diagnostics<-regression_diagnostics(eff1)
  } else {
    eff1_rslt<-eff1_diagnostics<-NULL
    eff1_m_q<-eff1_r_q<-rep(NA, length(colnames(rv)))
  }

  ### III.2. Regression of latter revisions (Rv) on previous revisions (Rv_1)
  eff2_trf<-ifelse(is_log, "Log", "None")
  eff2<-efficiencyModel2(vt, gap, na.zero)
  if(!is.null(eff2)) {
    eff2_rslt<-format_reg_output(eff2, is_log, FALSE)
    eff2_m_q<-c("", eval_pvals(eff2[, "intercept.pvalue"], h0_good=TRUE))
    eff2_r_q<-c("", eval_pvals(eff2[, "slope.pvalue"], h0_good=TRUE))
    eff2_diagnostics<-regression_diagnostics(eff2)
  } else {
    eff2_rslt<-eff2_diagnostics<-NULL
    eff2_m_q<-eff2_r_q<-c("", rep(NA, length(colnames(rv)[-1])))
  }

  ## IV. Orthogonality

  ### IV.1. Regression of latter revisions (Rv) on previous revisions (Rv_1, Rv_2,...Rv_p)
  orth1_trf<-ifelse(is_log, "Log", "None")
  orth1<-orthogonallyModel1(rv, nrevs, na.zero)
  if(!is.null(orth1)) {
    orth1_rslt<-format_reg_output(orth1, is_log, FALSE)
    orth1_m_q<-c(rep("", nrevs), eval_pvals(orth1[, "intercept.pvalue"], h0_good=TRUE))
    orth1_r_q<-c(rep("", nrevs), eval_pvals(pf(orth1[, "F"], nrevs, orth1[, "N"]-nrevs-1), h0_good=TRUE))
    orth1_diagnostics<-regression_diagnostics(orth1)
  } else {
    orth1_rslt<-orth1_diagnostics<-NULL
    if(ncol(rv)>nrevs) orth1_m_q<-orth1_r_q<-c(rep("", nrevs), rep(NA, length(colnames(rv)[-c(1:nrevs)])))
    else orth1_m_q<-orth1_r_q<-rep("", nrevs)
  }

  ### IV.2. Regression model of latter revisions (Rv) on previous revisions at a specific version (Rv_k)
  orth2_trf<-ifelse(is_log, "Log", "None")
  orth2<-orthogonallyModel2(rv, ref, na.zero)
  if(!is.null(orth2)) {
    orth2_rslt<-format_reg_output(orth2, is_log, FALSE)
    orth2_m_q<-c(rep("", ref), eval_pvals(orth2[, "intercept.pvalue"], h0_good=TRUE))
    orth2_r_q<-c(rep("", ref), eval_pvals(orth2[, "slope.pvalue"], h0_good=TRUE))
    orth2_diagnostics<-regression_diagnostics(orth2)
  } else {
    orth2_rslt<-orth2_diagnostics<-NULL
    if(ncol(rv)>ref) orth2_m_q<-orth2_r_q<-c(rep("", ref), rep(NA, length(colnames(rv)[-c(1:ref)])))
    else orth2_m_q<-orth2_r_q<-rep("", ref)
  }

  ### IV.3. Autocorrelation test
  ac_trf<-ifelse(is_log, "Log", "None")
  ac_trf_str<-ifelse(ac_trf == "Log", get_info_transformation(TRUE, FALSE), get_info_transformation(FALSE, FALSE))
  pm_test<-try(apply(rv, 2, function(x) ljungbox(x[!is.na(x)], k=2)), silent=TRUE) # Ljung-Box up to k

  if(!"try-error" %in% class(pm_test)) {
    pm_test_mat<-matrix(unlist(pm_test), ncol=2, byrow=TRUE)[, , drop=FALSE]
    dimnames(pm_test_mat)<-list(colnames(rv), c("value", "p.value"))
    ac_rslt<-list(info_transformation=ac_trf_str, estimates_ljungbox=pm_test_mat)
    ac_q<-eval_pvals(ac_rslt$estimates_ljungbox[, "p.value"], h0_good=TRUE)
  } else {
    ac_rslt<-NULL
    ac_q<-rep(NA, length(colnames(rv)))
  }

  ### IV.4. Seasonality test
  seas_trf<- ifelse(is_log, "Delta-Log 1", "Delta 1")
  seas_trf_str<-ifelse(seas_trf == "Delta-Log 1", get_info_transformation(TRUE, TRUE), get_info_transformation(FALSE, TRUE))
  lb_test<-try(apply(diff(rv), 2, function(x) seasonality_qs(x, freq)), silent=TRUE) # Ljung-Box
  fd_test<-try(apply(diff(rv), 2, function(x) seasonality_friedman(x, freq)), silent=TRUE)  # Friedman non-parametric test

  if(!"try-error" %in% class(lb_test) && !"try-error" %in% class(fd_test) && freq>1) {
    seas_rslt<-list(info_transformation=seas_trf_str,
                    estimates_ljungbox=matrix(unlist(lb_test), ncol=2, byrow = TRUE, dimnames = list(colnames(rv), c("value", "p.value"))),
                    estimates_friedman=matrix(unlist(fd_test), ncol=2, byrow = TRUE, dimnames = list(colnames(rv), c("value", "p.value"))))
    seas_lb_q<-eval_pvals(seas_rslt$estimates_ljungbox[, "p.value"], h0_good=TRUE)
    seas_fd_q<-eval_pvals(seas_rslt$estimates_friedman[, "p.value"], h0_good=TRUE)
  }else if(!"try-error" %in% class(lb_test) && freq>1) {
    seas_rslt<-list(info_transformation=seas_trf_str,
                    estimates_ljungbox=matrix(unlist(lb_test), ncol=2, byrow = TRUE, dimnames = list(colnames(rv), c("value", "p.value"))),
                    estimates_friedman=NULL)
    seas_lb_q<-eval_pvals(seas_rslt$estimates_ljungbox[, "p.value"], h0_good=TRUE)
    seas_fd_q<-rep(NA, length(colnames(rv)))
  }else if(!"try-error" %in% class(fd_test) && freq>1) {
    seas_rslt<-list(info_transformation=seas_trf_str,
                    estimates_ljungbox=NULL,
                    estimates_friedman=matrix(unlist(fd_test), ncol=2, byrow = TRUE, dimnames = list(colnames(rv), c("value", "p.value"))))
    seas_lb_q<-rep(NA, length(colnames(rv)))
    seas_fd_q<-eval_pvals(seas_rslt$estimates_friedman[, "p.value"], h0_good=TRUE)
  } else {
    seas_rslt<-NULL
    seas_lb_q<-seas_fd_q<-rep(NA, length(colnames(rv)))
  }

  ## V. Signal vs Noise
  sn_trf<-if(is_stationary && !is_log) "None" else
    if(is_stationary && is_log) "Log" else
      if(!is_stationary && !is_log) paste("Delta", delta_diff) else
        paste("Delta-Log", delta_diff)
  sn<-signalnoise(vts, gap, na.zero)
  if(!is.null(sn)) {
    sn_rslt<-list(info_transformation=get_info_transformation(is_log, !is_stationary),
                  estimates=t(round(sn, 3)))
    sn_noise_q<-eval_pvals(sn[, "Noise.pvalue"], h0_good=TRUE)
    sn_news_q<-eval_pvals(sn[, "News.pvalue"], h0_good=FALSE)
  } else {
    sn_rslt<-NULL
    sn_noise_q<-sn_news_q<-rep(NA, colnames(rv))
  }


  # VAR-based Analysis

  ## 1. Unit root test
  ## see before

  ## 2. Cointegration test
  if(!is.null(coint)) coint_rslt<-round(coint, 3)

  ## 3. VECM
  errcorr<-vecm(vt, lag=2, model="none", na.zero)
  if(!is.null(errcorr)) {
    vecm_rslt<-round(errcorr, 3)
  } else {
    vecm_rslt<-NULL
  }

  var_based_rslt<-list(unit_root_test = ur_rslt,
                       cointegration_test = coint_rslt,
                       vecm = vecm_rslt)


  # Output
  lbl<-c(paste("Relevancy - Theil", U_det), "Bias1 T-test", "Bias2 Augmented T-test",
         "Bias3 SlopeAndDrift (Ols L on P) - Mean", "Bias3 SlopeAndDrift (Ols L on P) - Reg.",
         "Efficiency1 (Ols R on P) - Mean", "Efficiency1 (Ols R on P) - Reg.",
         "Efficiency2 (Ols Rv on Rv_1) - Mean", "Efficiency2 (Ols Rv on Rv_1) - Reg.",
         "Orthogonality1 (Ols Rv on Rv_(1:p)) - Mean", "Orthogonality1 (Ols Rv on Rv_(1:p)) - Reg.",
         "Orthogonality2 (Ols Rv on Rv_k.) - Mean", "Orthogonality2 (Ols Rv on Rv_k) - Reg.",
         "Orthogonality3 AutoCorrelation (Ljung-Box)",
         "Orthogonality4 Seasonality (Ljung-Box)", "Orthogonality4 Seasonality (Friedman)",
         "SignalVsNoise1 - Noise (Ols R on P)", "SignalVsNoise2 - Signal (Ols R on L)")
  transformation<-c(theil_trf, rep(ta_trf, 2), rep(sd_trf, 2), rep(eff1_trf, 2),
                    rep(eff2_trf, 2), rep(orth1_trf, 2), rep(orth2_trf, 2),
                    ac_trf, rep(seas_trf, 2), rep(sn_trf, 2))
  evals<-rbind(theil_q, t_q, ta_q, sd_m_q, sd_r_q, eff1_m_q, eff1_r_q, eff2_m_q,
               eff2_r_q, orth1_m_q, orth1_r_q, orth2_m_q, orth2_r_q,
               ac_q, seas_lb_q, seas_fd_q, sn_noise_q, sn_news_q)
  summary_table<-data.frame(transformation, evals, row.names = lbl)
  colnames(summary_table)<- c("Transf.", colnames(rv))

  diagnostics_table <- list(slope_and_drift = sd_diagnostics, efficiency1 = eff1_diagnostics,
                            efficiency2 = eff2_diagnostics, orthogonality1 = orth1_diagnostics,
                            orthogonality2 = orth2_diagnostics)

  out$call <- cl
  out$revisions <- rv_notrf
  out$descriptive.statistics <- ds
  out$summary <- summary_table
  out$summary.diagnostics <- diagnostics_table
  out$relevancy <- list(theil=theil_rslt)
  out$bias <- list(t_ta_test=ta_rslt, slope_and_drift=sd_rslt)
  out$efficiency <- list(efficiency1=eff1_rslt, efficiency2=eff2_rslt)
  out$orthogonality <- list(orthogonality1=orth1_rslt, orthogonality2=orth2_rslt,
                            autocorrelation_test=ac_rslt, seasonality_test=seas_rslt)
  out$signalnoise <- list(signal_noise=sn_rslt)
  out$varbased<-var_based_rslt

  options(scipen = 0)

  return(out)
}



get_rownames_diag <- function(vt, gap) {
  # Example for n=4: v1_v2, v1_v3, v1_v4, v2_v3, v2_v4, v3_v4
  n<-ncol(vt)
  idx1<-rep(1:(n-gap), (n-gap):1)
  idx0<-sequence((n-gap):1)+rep(1:(n-gap), (n-gap):1)
  w<-sapply(colnames(vt), function(s) paste0("[", s, "]"))
  rw<-mapply(function(a, b) paste(a, b, sep="_"), w[idx1], w[idx0])
  return(rw)
}


eval_pvals<-function(pval, h0_good = TRUE, residuals = FALSE) {
  pval<-as.numeric(pval)

  if(! residuals) {
    if(h0_good) {
      quality<-ifelse(pval>.05, "Good",
                      ifelse(pval>.01, "Uncertain",
                             ifelse(pval>.001, "Bad", "Severe")))
    } else {
      quality<-ifelse(pval<.05, "Good", "Uncertain")
    }
  } else {
    if(h0_good) {
      quality<-ifelse(pval>.1, "Good",
                      ifelse(pval>.01, "Uncertain", "Bad"))
    } else {
      quality<-ifelse(pval<.05, "Good", "Uncertain")
    }
  }
  quality[is.na(quality)]<-"Good"

  return(paste0(quality, " (", trimws(format(round(pval, 3), nsmall=3)), ")"))
}

eval_U<-function(U, U2=TRUE) {
  U<-as.numeric(U)

  if(U2) {
    quality<-ifelse(U>=1, "Severe",
                    ifelse(U>.9, "Bad",
                           ifelse(U>.8, "Uncertain", "Good")))
  } else {
    quality<-ifelse(U>.99, "Severe",
                    ifelse(U>.9, "Bad",
                           ifelse(U>.8, "Uncertain", "Good")))
  }
  quality[is.na(quality)]<-"Good"

  return(paste0(quality, " (", trimws(format(round(U, 3), nsmall=3)), ")"))
}

format_reg_output<-function(x, is_log, is_diff) {
  info_transformation<-get_info_transformation(is_log, is_diff)

  x_df<-as.data.frame(t(round(x, 3)))
  n<-nrow(x_df)
  estim<-x_df[1:(n-13), , drop=FALSE]

  norm_test<-list(Jarque_Bera_test=x_df[(n-12):(n-9), , drop=FALSE])
  hsk_test<-list(Breusch_Pagan_test=x_df[(n-8):(n-6), , drop=FALSE], White_test=x_df[(n-5):(n-3), , drop=FALSE], ARCH_test=x_df[(n-2):n, , drop=FALSE])
  tests<-list(normality=norm_test, homoskedasticity=hsk_test)

  return(list(info_transformation=info_transformation, estimates=estim, residuals=tests))
}

get_info_transformation<-function(is_log, is_diff) {
  if(is_log) {
    info_transformation<-"Series have been log-transformed"
    if(is_diff) info_transformation<-paste(info_transformation, "and differentiated")
  } else {
    if(is_diff) {
      info_transformation<-"Series have been differentiated"
    } else {
      info_transformation<-"No transformation"
    }
  }
  return(info_transformation)
}

regression_diagnostics<-function(reg_output) {

  jb<-eval_pvals(reg_output[, "JarqueBera.pvalue"], residuals=TRUE)
  bp<-eval_pvals(reg_output[, "BreuschPagan.pvalue"], residuals=TRUE)
  wh<-eval_pvals(reg_output[, "White.pvalue"], residuals=TRUE)
  arch<-eval_pvals(reg_output[, "arch.pvalue"], residuals=TRUE)

  lbl<-c("Jarque-Bera", "Breusch-Pagan", "White", "ARCH")
  tests<-c("Normality", rep("Homoskedasticity", 3))
  tests_rslts<-rbind(jb, bp, wh, arch)

  rslt<-`colnames<-`(data.frame(tests, tests_rslts, row.names = lbl), c("Test", rownames(reg_output)))

  return(rslt)
}

get_vd_rev <- function(vt, gap) {
  n<-dim(vt)[2]

  idx1<-(gap+1):n
  idx0<-1:(n-gap)

  rev<-vt[, idx1, drop=FALSE]-vt[, idx0, drop=FALSE]

  w<-sapply(colnames(vt), function(s) paste0("[", s, "]"))
  rw<-mapply(function(a, b) paste(a, b, sep="-"), w[idx1], w[idx0])

  rev<-`colnames<-`(rev, rw)
  return(rev)
}

check_seasonality <- function(x) {

  if(frequency(x)>1) {
    x_diff<-diff(x)
    lb_pval<-try(seasonality_qs(x_diff, frequency(x))$pvalue, silent=TRUE) # Ljung-Box
    fd_pval<-try(seasonality_friedman(x_diff, frequency(x))$pvalue, silent=TRUE) # Friedman non-parametric test

    test_succeeded<-c(!"try-error" %in% class(lb_pval), !"try-error" %in% class(fd_pval))
    if(all(test_succeeded)) {
      pvals<-c(lb_pval, fd_pval)
      seasonality<-ifelse(length(pvals[which(pvals<.05)])==2, TRUE, FALSE)
    }else if(any(test_succeeded)) {
      if(test_succeeded[1]) {
        seasonality<-ifelse(lb_pval<.01, TRUE, FALSE)
      }else if(test_succeeded[2]) {
        seasonality<-ifelse(fd_pval<.01, TRUE, FALSE)
      }
    } else {
      seasonality<-FALSE
    }
  } else {
    seasonality<-FALSE
  }
  return(seasonality)
}
