ts_r2jd<-function(s){
  if (is.null(s)){
    return (NULL)
  }
  freq<-frequency(s)
  start<-start(s)
  .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/r/timeseries/TsData;", "of", 
         as.integer(freq), as.integer(start[1]), as.integer(start[2]), as.double(s))
  }

tsdomain_r2jd<-function(period, startYear, startPeriod, length){
  .jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "Ljdplus/toolkit/base/r/timeseries/TsDomain;", "of", 
         as.integer(period), as.integer(startYear), as.integer(startPeriod), as.integer(length))
}


ts_jd2r<-function(s){
  if (is.null(s)){
    return (NULL)
  }
  pstart<-.jcall("jdplus/toolkit/base/r/timeseries/TsUtility", "[I", "startPeriod", s)
  jx<-.jcall(s, "Ljdplus/toolkit/base/api/data/DoubleSeq;", "getValues")
  x<-.jcall(jx, "[D", "toArray")
  ts(x,start=pstart[2:3], frequency=pstart[1])
}

matrix_jd2r<-function(s){
  if (is.jnull(s)){
    return (NULL)
  }
  nr<-.jcall(s, "I", "getRowsCount")
  nc<-.jcall(s, "I", "getColumnsCount")
  d<-.jcall(s, "[D", "toArray")
  return (array(d, dim=c(nr, nc)))
}

matrix_r2jd<-function(s){
  if (is.null(s))
    return (.jnull("jdplus/toolkit/base/api/math/matrices/Matrix"))
  if (!is.matrix(s)){
    s<-matrix(s, nrow=length(s), ncol=1)
  }
  sdim<-dim(s)
  return (.jcall("jdplus/toolkit/base/api/math/matrices/Matrix","Ljdplus/toolkit/base/api/math/matrices/Matrix;", "of", as.double(s), as.integer(sdim[1]), as.integer(sdim[2])))
}
