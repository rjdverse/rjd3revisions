#' Set all test thresholds to their default values
#'
#' @param diagnostic_tests Boolean. Whether or not to reset thresholds for
#'   diagnostics tests on residuals as well in addition to parametric tests.
#' @export
#' @examples
#'
#' set_all_thresholds_to_default()
#'
set_all_thresholds_to_default <- function(diagnostic_tests = TRUE) {

    threshold_option_names <- c("theil_u1_threshold", "theil_u2_threshold",
                                "t_threshold", "augmented_t_threshold",
                                "slope_and_drift_threshold", "eff1_threshold",
                                "eff2_threshold", "orth1_threshold",
                                "orth2_threshold", "autocorr_threshold",
                                "seas_threshold", "signal_noise1_threshold",
                                "signal_noise2_threshold")

    if (diagnostic_tests) {
        threshold_option_names <- c(threshold_option_names, "jb_res_threshold",
                                    "bp_res_threshold", "white_res_threshold",
                                    "arch_res_threshold")
    }

    for (threshold_option_name in threshold_option_names) {
        set_thresholds_to_default(threshold_option_name)
    }

    return(invisible(NULL))
}


#' Set thresholds of a given test to their default values
#'
#' @param threshold_option_name Boolean. Whether or not to reset thresholds for
#'   diagnostics tests on residuals as well in addition to parametric tests.
#' @export
#' @examples
#'
#' set_thresholds_to_default("t_threshold")
#'
set_thresholds_to_default <- function(threshold_option_name) {
    switch(
        threshold_option_name,
        theil_u1_threshold = { options(theil_u1_threshold = c(uncertain = .8, bad = .9, severe = .99)) },
        theil_u2_threshold = { options(theil_u2_threshold = c(uncertain = .8, bad = .9, severe = 1)) },
        t_threshold = { options(t_threshold = c(severe = 0.001, bad = 0.01, uncertain = 0.05)) },
        augmented_t_threshold = { options(augmented_t_threshold = c(severe = 0.001, bad = 0.01, uncertain = 0.05)) },
        slope_and_drift_threshold = { options(slope_and_drift_threshold = c(severe = 0.001, bad = 0.01, uncertain = 0.05)) },
        eff1_threshold = { options(eff1_threshold = c(severe = 0.001, bad = 0.01, uncertain = 0.05)) },
        eff2_threshold = { options(eff2_threshold = c(severe = 0.001, bad = 0.01, uncertain = 0.05)) },
        orth1_threshold = { options(orth1_threshold = c(severe = 0.001, bad = 0.01, uncertain = 0.05)) },
        orth2_threshold = { options(orth2_threshold = c(severe = 0.001, bad = 0.01, uncertain = 0.05)) },
        autocorr_threshold = { options(autocorr_threshold = c(severe = 0.001, bad = 0.01, uncertain = 0.05)) },
        seas_threshold = { options(seas_threshold = c(severe = 0.001, bad = 0.01, uncertain = 0.05)) },
        signal_noise1_threshold = { options(signal_noise1_threshold = c(severe = 0.001, bad = 0.01, uncertain = 0.05)) },
        signal_noise2_threshold = { options(signal_noise2_threshold = c(uncertain = 0.05)) },
        jb_res_threshold = { options(jb_res_threshold = c(bad = 0.01, uncertain = 0.1)) },
        bp_res_threshold = { options(bp_res_threshold = c(bad = 0.01, uncertain = 0.1)) },
        white_res_threshold = { options(white_res_threshold = c(bad = 0.01, uncertain = 0.1)) },
        arch_res_threshold = { options(arch_res_threshold = c(bad = 0.01, uncertain = 0.1)) },
        stop("Test not found")
    )
}
