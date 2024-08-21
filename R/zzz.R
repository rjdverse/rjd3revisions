#' @import rJava
NULL

.onLoad <- function(libname, pkgname) {
    if (!requireNamespace("rjd3toolkit", quietly = TRUE)) stop("Loading rjd3 libraries failed")

    result <- rJava::.jpackage(pkgname, lib.loc = libname)
    if (!result) stop("Loading java packages failed")

    # reload extractors
    .jcall("jdplus/toolkit/base/api/information/InformationExtractors", "V", "reloadExtractors")

    # options initialization
    if (is.null(getOption("theil_u1_threshold"))) {
        set_thresholds_to_default("theil_u1_threshold")
    }
    if (is.null(getOption("theil_u2_threshold"))) {
        set_thresholds_to_default("theil_u2_threshold")
    }
    if (is.null(getOption("t_threshold"))) {
        set_thresholds_to_default("t_threshold")
    }
    if (is.null(getOption("augmented_t_threshold"))) {
        set_thresholds_to_default("augmented_t_threshold")
    }
    if (is.null(getOption("slope_and_drift_threshold"))) {
        set_thresholds_to_default("slope_and_drift_threshold")
    }
    if (is.null(getOption("eff1_threshold"))) {
        set_thresholds_to_default("eff1_threshold")
    }
    if (is.null(getOption("eff2_threshold"))) {
        set_thresholds_to_default("eff2_threshold")
    }
    if (is.null(getOption("orth1_threshold"))) {
        set_thresholds_to_default("orth1_threshold")
    }
    if (is.null(getOption("orth2_threshold"))) {
        set_thresholds_to_default("orth2_threshold")
    }
    if (is.null(getOption("autocorr_threshold"))) {
        set_thresholds_to_default("autocorr_threshold")
    }
    if (is.null(getOption("seas_threshold"))) {
        set_thresholds_to_default("seas_threshold")
    }
    if (is.null(getOption("signal_noise1_threshold"))) {
        set_thresholds_to_default("signal_noise1_threshold")
    }
    if (is.null(getOption("signal_noise2_threshold"))) {
        set_thresholds_to_default("signal_noise2_threshold")
    }
    if (is.null(getOption("jb_res_threshold"))) {
        set_thresholds_to_default("jb_res_threshold")
    }
    if (is.null(getOption("bp_res_threshold"))) {
        set_thresholds_to_default("bp_res_threshold")
    }
    if (is.null(getOption("white_res_threshold"))) {
        set_thresholds_to_default("white_res_threshold")
    }
    if (is.null(getOption("arch_res_threshold"))) {
        set_thresholds_to_default("arch_res_threshold")
    }
}
