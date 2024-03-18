#' Generate report on Revision Analysis
#'
#' @param rslt an object of class `"rjd3rev_vintages"` which is the output
#'             of the function `revision_analysis()`
#' @param path.out path of the output HTML file containing the report. Default
#'                 is NULL meaning that the report will be saved in the working
#'                 directory with a generic name.
#' @param open.report Boolean. Default is TRUE meaning that the report will
#'                    open automatically after being generated.
#' @seealso `revision_analysis()` to create the input object
#'
#' @export
#'
#' @examples
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
#' for(i in 1:4){
#'   xt<-xt+rev[(1+(i-1)*np):(i*np)]
#'   obs_values<-c(obs_values,xt)
#' }
#' df<-data.frame(rev_date, time_period, obs_values)
#'
#' ## Make analysis and generate the report
#' vintages<-create_vintages(df, periodicity = 4)
#' rslt<-revision_analysis(vintages, view = "diagonal")
#' \dontrun{
#' get_report(rslt)
#' }
#'
get_report <- function(rslt,
                       path.out = NULL,
                       open.report=TRUE) {

  #template_file<-"report.Rmd"
  template_file<-system.file("templates/report.Rmd", package = "rjd3revisions")

  if (is.null(path.out)) {
    path.out<-paste0(getwd(), "/", "revisions_report.html")
  } else {
    path.out<-ifelse(substr(path.out, length(path.out)-5, length(path.out)) != ".html",
                     paste0(path.out, ".html"),
                     path.out)
  }

  e<-new.env(parent = baseenv())
  assign("descriptive_statistics", rslt$descriptive.statistics, envir = e)
  assign("main_results", rslt$summary, envir = e)

  rmarkdown::render(input = template_file,
                    output_file = path.out,
                    output_options = list(self_contained = FALSE, mathjax = "local"),
                    envir = e)

  if (open.report) utils::browseURL(path.out)
}
