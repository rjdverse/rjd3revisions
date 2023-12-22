#' @import rJava
NULL

.onLoad <- function(libname, pkgname) {
  if (! requireNamespace("rjd3toolkit", quietly = TRUE)) stop("Loading rjd3 libraries failed")

  result <- rJava::.jpackage(pkgname, lib.loc=libname)
  if (!result) stop("Loading java packages failed")

  # reload extractors
  .jcall("jdplus/toolkit/base/api/information/InformationExtractors", "V", "reloadExtractors")
}
