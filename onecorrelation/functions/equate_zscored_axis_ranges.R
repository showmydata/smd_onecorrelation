equate_zscored_axis_ranges <- function(data, cushion=.1, ...){
  # Selects axis ranges that all span the same z range 
  mins=apply(data,2,min,na.rm=TRUE); 
  maxs=apply(data,2,max,na.rm=TRUE); 
  means=apply(data,2,mean,na.rm=TRUE); 
  sds=apply(data,2,sd,na.rm=TRUE);
  zranges=(maxs-mins)/sds
  maxzrange=max(zranges)
  midranges=apply(cbind(mins,maxs),1,mean)
  ranges=cbind(midranges-(maxzrange/2+cushion)*sds,midranges+(maxzrange/2+cushion)*sds) 
  return(ranges)
}