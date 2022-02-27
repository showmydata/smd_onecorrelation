process_label <- function(typedlabel, currentlabel) {

if(typedlabel=="") rng2=currentlabel
else {
  rng=typedlabel
  rng=unlist(strsplit(rng,","))
  if (length(rng) < length(currentlabel)) {
    rng2=rng
    rng2[(length(rng)+1):length(currentlabel)]=currentlabel[(length(rng)+1):length(currentlabel)]
  } 
  else {
    rng2=rng[1:length(currentlabel)]
  }
}
return(rng2)
}