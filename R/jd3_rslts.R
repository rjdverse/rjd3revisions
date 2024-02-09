#' @include jd3_ts.R

proc_numeric<-function(rslt, name){
  s<-.jcall(rslt, "Ljava/lang/Object;", "getData", name, jd_clobj)
  if (!is.jnull(s))
    .jcall(s, "D", "doubleValue")
  else
    return (NaN)
}

proc_vector<-function(rslt, name){
  s<-.jcall(rslt, "Ljava/lang/Object;", "getData", name, jd_clobj)
  if (is.jnull(s))
    return(NULL)
  .jevalArray(s)
}

proc_int<-function(rslt, name){
  s<-.jcall(rslt, "Ljava/lang/Object;", "getData", name, jd_clobj)
  if (is.jnull(s))
    return(-1)
  .jcall(s, "I", "intValue")
}

proc_bool<-function(rslt, name){
  s<-.jcall(rslt, "Ljava/lang/Object;", "getData", name, jd_clobj)
  if (is.jnull(s))
    return(FALSE)
  .jcall(s, "Z", "booleanValue")
}

proc_ts<-function(rslt, name){
  s<-.jcall(rslt, "Ljava/lang/Object;", "getData", name, jd_clobj)
  if (is.jnull(s))
    return (NULL)
  if (.jinstanceof(s, "demetra/timeseries/TsData"))
    return(ts_jd2r(.jcast(s,"demetra/timeseries/TsData")))
  else
    return (NULL)
}

proc_str<-function(rslt, name){
  s<-.jcall(rslt, "Ljava/lang/Object;", "getData", name, jd_clobj)
  if (is.jnull(s))
    return(NULL)
  .jcall(s, "S", "toString")
}

proc_desc<-function(rslt, name){
  s<-.jcall(rslt, "Ljava/lang/Object;", "getData", name, jd_clobj)
  if (is.jnull(s))
    return(NULL)
  .jevalArray(s)
}

proc_test<-function(rslt, name){
  s<-.jcall(rslt, "Ljava/lang/Object;", "getData", name, jd_clobj)
  if (is.jnull(s))
    return(NULL)
  desc<-.jcall(s, "S", "getDescription")
  val<-.jcall(s, "D", "getValue")
  pval<-.jcall(s, "D", "getPvalue")
  all<-c(val, pval)
  attr(all, "description")<-desc
  all
}

proc_parameter<-function(rslt, name){
  s<-.jcall(rslt, "Ljava/lang/Object;", "getData", name, jd_clobj)
  if (is.jnull(s))
    return(NULL)
  val<-.jcall(s, "D", "getValue")
  return (val)
}

proc_parameters<-function(rslt, name){
  jd_p<-.jcall(rslt, "Ljava/lang/Object;", "getData", name, jd_clobj)
  if (is.jnull(jd_p))
    return(NULL)
  p<-.jcastToArray(jd_p)
  len<-length(p)
  all<-array(0, dim=c(len))
  for (i in 1:len){
    all[i]<-.jcall(p[[i]], "D", "getValue")
  }
  all
}

proc_matrix<-function(rslt, name){
  s<-.jcall(rslt, "Ljava/lang/Object;", "getData", name, jd_clobj)
  if (is.jnull(s))
    return(NULL)
  return (matrix_jd2r(s))
}

proc_data<-function(rslt, name){
  s<-.jcall(rslt, "Ljava/lang/Object;", "getData", name, jd_clobj)
  if (is.jnull(s))
    return (NULL)
  if (.jinstanceof(s, "demetra/timeseries/TsData"))
    return(ts_jd2r(.jcast(s,"demetra/timeseries/TsData")))
  else if (.jinstanceof(s, "java/lang/Number"))
    return (.jcall(s, "D", "doubleValue"))
  else if (.jinstanceof(s, "demetra/math/matrices/MatrixType"))
    return(matrix_jd2r(.jcast(s,"demetra/math/matrices/MatrixType")))
  else if (.jinstanceof(s, "demetra/data/Parameter")){
    val<-.jcall(s, "D", "getValue")
     return (c(val))
  }
  else if (.jinstanceof(s, "[Ldemetra/data/Parameter;")){
    p<-.jcastToArray(s)
    len<-length(p)
    all<-array(0, dim=c(len))
    for (i in 1:len){
      all[i]<-.jcall(p[[i]], "D", "getValue")
    }
    return (all)
  }
  else if (.jcall(.jcall(s, "Ljava/lang/Class;", "getClass"), "Z", "isArray"))
    return (.jevalArray(s, silent=TRUE))
  else
    return (.jcall(s, "S", "toString"))
}

proc_dictionary<-function(name){
  jmapping<-.jcall(name, "Ldemetra/information/InformationMapping;", "getMapping")
  jmap<-.jnew("java/util/LinkedHashMap")
  .jcall(jmapping, "V", "fillDictionary", .jnull("java/lang/String"), .jcast(jmap, "java/util/Map"), TRUE)
  jkeys<-.jcall(jmap, "Ljava/util/Set;", "keySet")
  size<-.jcall(jkeys, "I", "size")
  keys<-array(dim=size)
  jiter<-.jcall(jkeys, "Ljava/util/Iterator;", "iterator")
  for (i in 1:size){
    keys[i] <- .jcall(.jcall(jiter, "Ljava/lang/Object;", "next"), "Ljava/lang/String;", "toString")
  }
  return (keys)
}

proc_likelihood<-function(jrslt, prefix){
  return (list(
    ll=proc_numeric(jrslt, paste(prefix,"ll", sep="")),
    ssq=proc_numeric(jrslt, paste(prefix,"ssq", sep="")),
    ser=proc_numeric(jrslt, paste(prefix,"ser", sep="")),
    nobs=proc_int(jrslt, paste(prefix,"nobs", sep="")),
    neffective=proc_int(jrslt, paste(prefix,"neffective", sep="")),
    nparams=proc_int(jrslt, paste(prefix,"nparams", sep="")),
    df=proc_int(jrslt, paste(prefix,"df", sep="")),
    aic=proc_numeric(jrslt, paste(prefix,"aic", sep="")),
    aicc=proc_numeric(jrslt, paste(prefix,"aicc", sep="")),
    bic=proc_numeric(jrslt, paste(prefix,"bic", sep="")),
    bic2=proc_numeric(jrslt, paste(prefix,"bic2", sep="")),
    bicc=proc_numeric(jrslt, paste(prefix,"bicc", sep="")),
    hannanquinn=proc_numeric(jrslt, paste(prefix,"hannanquinn", sep="")))
  )
}
