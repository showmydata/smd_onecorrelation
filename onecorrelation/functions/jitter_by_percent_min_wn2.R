jitter_by_percent_min_wn2 <- function(data, perc_jitter, ranked=FALSE) {
  v=colnames(data)
  if (is.vector(data)) {
    x=data
    xjprop=(perc_jitter-1)/200                               # compute proportion of min dot diff to jitter
    xuniq=unique(x)                                          # find unique values
    minxd=min(diff(xuniq[order(xuniq)]),na.rm=TRUE);         # find minimum difference b/w ordered unique values
    if(xjprop>=0) {                                          # if percent jitter is greater than zero then do jitter
      if(ranked) x1=jitter(x,amount=xjprop*10) 
      else x1=jitter(x,amount=xjprop*minxd) 
    } 
    else x1=x
    data=x1
  }
  else {
    for (i in 1:ncol(data)) {
      x=data[,i]                                               # grab a column of data
      xjprop=(perc_jitter-1)/200                               # compute proportion of min dot diff to jitter
      xuniq=unique(x)                                          # find unique values
      minxd=min(diff(xuniq[order(xuniq)]),na.rm=TRUE);         # find minimum difference b/w ordered unique values
      if(xjprop>=0) {                                          # if percent jitter is greater than zero then do jitter
        if(ranked) x1=jitter(x,amount=xjprop*10) 
        else x1=jitter(x,amount=xjprop*minxd) 
      } 
      else x1=x
      data[,i]=x1
    }
  }
  return(data)
}