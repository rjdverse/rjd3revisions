#' Generate report on Revision Analysis
#'
#' @param rslt an object of class `"rjd3rev_vintages"` which is the output
#'             of the function `revision_analysis()`
#' @param output_file path or name of the output file containing the report
#' @param output_dir path of the dir containing the output file (Optional)
#' @param output_format either an HTML document (default) or a PDF document
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
#'
#' ## Simulated data
#'
#' long_format <- rjd3revisions:::simulate_long(
#'     start_period = as.Date("2020-01-01"),
#'     n_period = 24,
#'     n_revision = 6,
#'     periodicity = 12L
#' )
#'
#' ## Make analysis and generate the report
#'
#' vintages <- create_vintages(long_format, periodicity = 12L)
#' rslt <- revision_analysis(vintages, view = "diagonal")
#' \dontrun{
#' render_report(
#'     rslt,
#'     output_file = "my_report",
#'     output_dir = "C:/Users/xxx",
#'     output_format = "pdf_document"
#' )
#' }
#'
render_report <- function(rslt,
                          output_file,
                          output_dir,
                          output_format = c("html_document", "pdf_document"),
                          open_report = TRUE,
                          ...) {

    # Check input
    checkmate::assert_class(rslt, "rjd3rev_rslts")

    # Check output_format
    output_format <- match.arg(output_format)

    template_file <- system.file("templates/report.Rmd", package = "rjd3revisions")

    # Check dir
    print("gi")
    if (missing(output_dir)) {
        output_dir <- dirname(output_file)
    }
    output_dir <- normalizePath(output_dir, mustWork = TRUE)
    output_file <- basename(output_file)

    # Check extension
    ext <- tools::file_ext(output_file)
    if (nchar(ext) == 0) {
        if (output_format == "html_document") {
            ext <- "html"
        } else if (output_format == "pdf_document") {
            ext <- "pdf"
        }
    }
    output_file <- tools::file_path_sans_ext(output_file)
    output_path <- paste0(normalizePath(file.path(output_dir, output_file), mustWork = FALSE), ".", ext)

    # Check path
    checkmate::assert_path_for_output(output_path, overwrite = TRUE)

    message("The report will be rendered to ", output_path, " in ", output_format, " format.")
    if (file.exists(output_path)) {
        message("The file already exists and will be overwritten.")
    }

    e <- list2env(list(
        descriptive_statistics = rslt$descriptive.statistics,
        main_results = rslt$summary
    ))

    rmarkdown::render(
        input = template_file,
        output_file = output_file,
        output_format = output_format,
        output_dir = output_dir,
        envir = e,
        ...
    )

    if (open_report) {
        utils::browseURL(output_path)
    }

    return(invisible(NULL))
}
