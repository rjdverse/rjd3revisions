#' Create vintage tables from data.frame
#'
#' From the input data.frame, the function displays vintages considering three
#' different data structures or views: vertical, horizontal and diagonal. See
#' the `details` section below for more information on the different views. The
#' function returns an object of class `rjd3rev_vintages` that can be used as
#' input in the main function `revision_analysis`.
#'
#' The are three different vintage views:
#'
#' 1. The vertical view shows the observed values at each time period by the
#' different vintages. This approach is robust to changes of base year and
#' data redefinition. A drawback of this approach is that for comparing the
#' same historical series for different vintages, we need to look at the
#' smallest common number of observations and consequently the number of
#' observations is in some circumstances very small. Moreover, it is often the
#' the case that most of the revision is about the last few points of the
#' series so that the number of observations is too small to test anything.
#'
#' 2. The horizontal view shows the observed values of the different vintages
#' by the period. A quick analysis can be performed by rows in order to see
#' how for the same data point (e.g. 2023Q1), figures are first estimated,
#' then forecasted and finally revised. The main findings are usually obvious:
#' in most cases the variance decreases, namely data converge towards the
#' 'true value'. Horizontal tables are just a transpose of vertical tables and
#' are not used in the tests in `revision_analysis`.
#'
#' 3. The diagonal view shows subsequent releases of a given time period,
#' without regard for the date of publication. The advantage of the diagonal
#' approach is that it gives a way to analyse the trade between the timing of
#' the release and the accuracy of the published figures. It is particularly
#' informative when regular estimation intervals exist for the data under
#' study. However, this approach requires to be particularly vigilant in case
#' there is a change in base year or data redefinition.
#'
#' @param df a formatted data.frame containing the input. It must be composed of
#'           three columns in this order: revision date, time period and value:
#'           Revision date should be in a Date format specified in
#'           `revdate.format` argument. Time period should be mentioned in
#'           YYM., YYQ. or YEAR. format. Examples of correct formats are
#'           2023M01, 2023Q1 or 2023. Finally, the values should be numeric.
#' @param periodicity periodicity of the time period (12, 4 or 1 for resp.
#'                    monthly,quarterly or annual data)
#' @param vintage.selection a list specifying the range of revision dates to
#'                          retain. As an example: list(start="2022-02-02",
#'                          end="2022-08-05") would keep all the vintages whose
#'                          revision date is between 02Feb.2022 and 05Aug.2022.
#'                          If NULL (the default), the whole range is
#'                          selected.
#' @param revdate.format character string corresponding to the format used in
#'                       the input data.frame for the revision dates.
#' @import rJava
#'
#' @return an object of class `rjd3rev_vintages`
#' @export
#' @examples
#' df<-data.frame(rev_date = c(rep("2022-07-31",4), rep("2022-08-31",4),
#'                             rep("2022-09-30",4), rep("2022-10-31",4),
#'                             rep("2022-11-30",4), rep("2022-12-31",4),
#'                             rep("2023-01-31",4), rep("2023-02-28",4)),
#'                time_period = c(rep(c("2022Q1","2022Q2","2022Q3","2022Q4"),8)),
#'                obs_values = c(.8,.2,NA,NA, .8,.1,NA,NA,
#'                                 .7,.1,NA,NA, .7,.2,.5,NA,
#'                                 .7,.2,.5,NA, .7,.3,.7,NA,
#'                                 .7,.2,.7,.4, .7,.3,.7,.3))
#' vintages<-create_vintages(df, periodicity = 4)
#'
#' ## including vintage selection
#' vintages<-create_vintages(df, periodicity = 4,
#'                           vintage.selection = list(start="2022-10-31", end="2023-01-31"))
#'
#' ## specifying the format of revision dates
#' vintages<-create_vintages(df, periodicity = 4, revdate.format= "%Y-%m-%d")
#'
create_vintages<- function(df, periodicity, vintage.selection = NULL, revdate.format= "%Y-%m-%d") {

  jfac<-.jnew("jdplus/revisions/base/r/VintagesFactory", as.integer(periodicity))

  # check input
  if(! is.data.frame(df)){
    warning("Wrong input type. Must be a data.frame.")
    return(NULL)
  }
  if (ncol(df)!=3) {
    warning("Wrong input. Number of columns of df must be 3.")
    return(NULL)
  }

  colnames(df)<-c("revdate","time","obs_value")
  df$revdate<-as.Date(as.character(df$revdate), revdate.format)

  if (any(is.na(df$revdate))){
    warning("Wrong input in first column. Revision dates not in a correct format. The parameter revdate.format might be mispecified.")
    return(NULL)
  }
  if(is.null(yp(df$time))){
    warning("Wrong input in second column. Time periods not in a correct format. Examples of correct formats are 2023M01, 2023Q1 or 2023.")
    return(NULL)
  }else{
    df$time<-yp(df$time)
  }
  if(! is.numeric(df$obs_value)){
    warning("Wrong input in third column. Obsevation values must be numeric.")
    return(NULL)
  }

  # Vintages selection
  if(!is.null(vintage.selection)){
    df<-subset(df, revdate >= as.Date(vintage.selection[[1]]) & revdate <= as.Date(vintage.selection[[2]]))
    if(nrow(df) == 0){
      warning("Vintage selection out of range!")
      return(NULL)
    }
  }
  if(length(unique(df$revdate))<2){
    warning("Too few number of vintages. Number of unique revision dates must be >= 2.")
    return(NULL)
  }

  # format input
  input<-split(df, seq(nrow(df)))

  add_to_JD3revisions <- function(x) {
    revdate<-x[[1]]
    timedate<-x[[2]]
    val<-x[[3]]
    .jcall(jfac, "V", "add", as.character(timedate), as.character(revdate), val)
    return (NULL)
  }
  lapply(input, FUN=add_to_JD3revisions)

  # output
  ## vertical view
  vv<-vintageTableFromFactory(jfac)

  ## horizontal view
  periods_name<-as.character(d2t(as.vector(time(vv)), periodicity))
  vh<-`colnames<-`(t(vv), periods_name)

  ## diagonal view
  vd_mat<-t(apply(t(vv), 2, function(x) x[order(is.na(x))]))
  releases_name<-sprintf("Release[%s]",seq(1:ncol(vd_mat)))
  vd<-`colnames<-`(ts(vd_mat, start = start(vv), frequency = periodicity),releases_name)

  vintages <- list(vertical_view = vv, horizontal_view = vh, diagonal_view = vd)
  class(vintages) <- "rjd3rev_vintages"

  return (vintages)
}

#' Create vintages table from CSV or TXT files
#'
#' @param file character containing the name of the file which the data are
#'             to be read from.
#' @param periodicity periodicity of the time period (12, 4 or 1 for resp.
#'                    monthly,quarterly or annual data)
#' @param separator the field separator character in the input file (default ',')
#' @param vintage.selection a list specifying the range of revision dates to
#'                          retain. As an example: list(start='2022-02-02',
#'                          end='2022-08-05') would keep all the vintages whose
#'                          revision date is between 02Feb.2022 and 05Aug.2022.
#'                          If NULL (the default), the whole range is selected.
#' @param revdate.format character string corresponding to the format used in
#'                       the input data.frame for the revision dates.
#'                       The default is \%Y-\%m-\%d.
#'
#' @import rJava
#'
#' @seealso [create_vintages_from_xlsx()], [create_vintages()] which this function wraps.
#'
#' @return an object of class `rjd3rev_vintages`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' file.name<-"myinput.csv"
#' vintages<-create_vintages_from_csv(file.name, 12, separator = ";", revdate.format="%d/%m/%Y")
#' }
create_vintages_from_csv<-function(file, periodicity, separator = ",", vintage.selection=NULL, revdate.format="%Y.%m.%d"){

  df<-as.data.frame(read.csv(file, sep = separator, stringsAsFactors = F))

  return(create_vintages(df, periodicity, vintage.selection, revdate.format))
}

#' Create vintages table from XLSX files
#'
#' @param file character containing the name of the file which the data are
#'             to be read from.
#' @param sheetname character containing the sheet to read
#' @param periodicity periodicity of the time period (12, 4 or 1 for resp.
#'                    monthly,quarterly or annual data)
#' @param vintage.selection a list specifying the range of revision dates to
#'                          retain. As an example: list(start='2022-02-02',
#'                          end='2022-08-05') would keep all the vintages whose
#'                          revision date is between 02Feb.2022 and 05Aug.2022.
#'                          If NULL (the default), the whole range is selected.
#' @param revdate.format character string corresponding to the format used in
#'                       the input data.frame for the revision dates.
#'                       The default is \%Y-\%m-\%d.
#'
#' @import rJava
#'
#' @seealso [create_vintages_from_csv()], [create_vintages()] which this function wraps.
#'
#' @return an object of class `rjd3rev_vintages`
#' @export
#'
#' @examples
#' \dontrun{
#' file.name<-"myinput.xlsx"
#' vintages<-create_vintages_from_xlsx(file.name, 12, separator = ";", revdate.format="%d/%m/%Y")
#' }
create_vintages_from_xlsx<-function(file, sheetname, periodicity, vintage.selection, revdate.format= "%Y.%m.%d"){

  if(! require(readxl)){stop("package 'readxl' must be installed to run the function 'create_vintages_from_xlsx'")}

  df<-as.data.frame(read_excel(file, sheet = sheetname))

  return(create_vintages(df, periodicity, vintage.selection, revdate.format))
}



vintageTableFromFactory<-function(jfac){

  jvn<-.jcall(jfac, "Ljdplus/revisions/base/r/Vintages;", "build")
  jmat<-.jcall(jvn, "Ljdplus/revisions/base/api/timeseries/TsMatrix;", "vtable")
  data<-matrix_jd2r(.jcall(jmat, "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "getMatrix"))
  cols<-.jcall(jmat, "[S", "getFields")
  jstart<-.jcall(jmat, "Ljdplus/toolkit/base/api/timeseries/TsPeriod;", "getStart")
  pstart<-.jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "[I", "of", jstart)

  data[is.nan(data)]<-NA
  tsm<-ts(data, frequency = pstart[1], start = pstart[-1])
  tsm<-`colnames<-`(tsm, cols)
  return (tsm)
}


ymd<-function(y, m, d=1){
  return (as.Date(sprintf("%04i-%02i-%02i", y, m, d)))
}

yq<-function(y, q){
  return (as.Date(sprintf("%04i-%02i-%02i", y, q*3-2, 1)))
}

yp<-function(s){
  y<-as.integer(substr(s, 1, 4))
  if(nchar(as.character(s))[1] == 4){
    return (ymd(y,1))
  }else{
    p=substr(s,5,5)
    if (all(p == 'Q') | all(p == 'q')){
      q<-as.integer(substr(s, 6, 6))
      return (yq(y,q))
    }
    if (all(p == 'M') | all(p == 'm')){
      m<-as.integer(substr(s, 6, length(s)))
      return (ymd(y,m))
    }
  }
  return (NULL)
}

d2t<-function(d, periodicity){
  y<-floor(d)
  frac<-d%%1
  date<-ymd(y,1,1)+(ymd(y+1,1,1)-ymd(y,1,1))*frac
  if (periodicity == 12){
    return (paste0(y,"M",format(date,"%m")))
  }else if (periodicity == 4){
    return (paste0(y,"Q",(as.numeric(format(date,"%m"))+2)/3))
  }else if (periodicity == 1){
    return (y)
  }
  return(NULL)
}
