#' Calculate revisions from vintages
#'
#' @param vintages an object of class `"rjd3rev_vintages"`
#' @param gap Integer. Gap to consider between each vintages to calculate
#'            revision. Default is 1 which means that revisions are
#'            calculated for each vintages consecutively.
#'
#' @seealso `create_vintages()`
#'
#' @return a list with revisions calculated for the three vintage views
#'
#' @export
#'
#' @examples
#' df<-data.frame(rev_date = c(rep("2022-07-31",4), rep("2022-08-31",4),
#'                             rep("2022-09-30",4), rep("2022-10-31",4),
#'                             rep("2022-11-30",4), rep("2022-12-31",4),
#'                             rep("2023-01-31",4), rep("2023-02-28",4)),
#'                  time_period = c(rep(c("2022Q1","2022Q2","2022Q3","2022Q4"),8)),
#'                  obs_values = c(.8,.2,NA,NA, .8,.1,NA,NA,
#'                                 .7,.1,NA,NA, .7,.2,.5,NA,
#'                                 .7,.2,.5,NA, .7,.3,.7,NA,
#'                                 .7,.2,.7,.4, .7,.3,.7,.3))
#' vintages<-create_vintages(df, periodicity = 4)
#' revisions<-get_revisions(vintages, gap=1)
#'
get_revisions<-function(vintages, gap=1) {
  if(!class(vintages)=="rjd3rev_vintages") {
    warning("Wrong input. vintages must be an object of class 'rjd3rev_vintages'.")
    return(NULL)
  }

  get_vd_rev <- function(vt, gap) {
    n<-dim(vt)[2]

    idx1<-(gap+1):n
    idx0<-1:(n-gap)

    rev<-vt[, idx1, drop=FALSE]-vt[, idx0, drop=FALSE]

    w<-sapply(colnames(vt), function(s) paste0("[", s, "]"))
    rw<-mapply(FUN = function(a, b) paste(a, b, sep = "-"), w[idx1], w[idx0])

    rev<-`colnames<-`(rev, rw)
    return (rev)
  }

  vr<-get_vd_rev(vintages$vertical_view, gap)
  hr<-`colnames<-`(t(vr), colnames(vintages$horizontal_view))
  dr<-get_vd_rev(vintages$diagonal_view, gap)

  return (list(vertical_view=vr, horizontal_view=hr, diagonal_view=dr))
}
