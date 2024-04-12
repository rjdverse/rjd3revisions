#' Generate report on Revision Analysis
#'
#' @param rslt an object of class `"rjd3rev_vintages"` which is the output
#'             of the function `revision_analysis()`
#' @param output_path path or name of the output file containing the report
#' @param open_report Boolean. Default is TRUE meaning that the report will
#'                    open automatically after being generated.
#' @param ... Arguments to be passed to `rmarkdown::render()`, for example:
#' * `output_options` List of output options that can override the options specified in metadata
#' * `...`
#'
#' @seealso `revision_analysis()` to create the input object
#'
#' @export
#'
#' @examples
#' ## Simulated data
#' period_range <- seq(as.Date("2011-01-01"), as.Date("2020-10-01"), by = "quarter")
#' qtr <- (as.numeric(substr(period_range, 6, 7)) + 2) / 3
#' time_period <- rep(paste0(format(period_range, "%Y"), "Q", qtr), 5)
#' np <- length(period_range)
#' rev_date <- c(
#'     rep("2021-06-30", np), rep("2021-12-31", np), rep("2022-06-30", np),
#'     rep("2022-12-31", np), rep("2023-06-30", np)
#' )
#' set.seed(1)
#' xt <- cumsum(sample(rnorm(1000, 0, 1), np, TRUE))
#' rev <- rnorm(np * 4, 0, .1)
#' obs_values <- xt
#' for (i in 1:4) {
#'     xt <- xt + rev[(1 + (i - 1) * np):(i * np)]
#'     obs_values <- c(obs_values, xt)
#' }
#' df <- data.frame(rev_date, time_period, obs_values)
#'
#' ## Make analysis and generate the report
#' vintages <- create_vintages(df, periodicity = 4)
#' rslt <- revision_analysis(vintages, view = "diagonal")
#' \dontrun{
#' render_report(rslt)
#' }
#'
render_report <- function(rslt,
                          output_path,
                          output_format = c("html_document", "pdf_document"),
                          open_report = TRUE,
                          ...) {

    # Check input
    checkmate::assert_class(rslt, "rjd3rev_revision_analysis")

    # Check output_format
    output_format <- match.arg(output_format)

    template_file <- system.file("templates/report.Rmd", package = "rjd3revisions")

    # Check path
    output_path <- normalizePath(output_path, mustWork = FALSE)
    message("The report will be rendered to ", output_path, " and in ", output_format, " format.")

    e <- list2env(list(
        descriptive_statistics = rslt$descriptive.statistics,
        main_results = rslt$summary
    ))

    rmarkdown::render(input = template_file,
                      output_file = output_path,
                      output_format = output_format,
                      output_dir = NULL,
                      envir = e,
                      ...)

    if (open_report) utils::browseURL(output_path)

    return(invisible(NULL))
}
