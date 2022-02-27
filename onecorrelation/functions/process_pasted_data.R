process_pasted_data <- function(data,delete_casewise=FALSE) {
  a=gsub("\t\n", "\tNA\n", data) # replace blank end-of-lines (except the last line) with NA as placeholder so don't lose characters
  if(substr(a,nchar(a),nchar(a))=="\t") a=paste(a,"NA"); # do the same for the last line (b/c line just above doesn't manage to do that)
  if(substr(a,1,1)=="\n") a=paste("NA",a); # do the same for the last line (b/c line just above doesn't manage to do that)
  a1=gsub(" ","",a) # remove spaces
  b=unlist(strsplit(a1,"\n")) # split data by rows
  c=unlist(strsplit(b,"\t")) # split resulting data by cells
  options(warn=-1); d=as.numeric(c); options(warn=0) # Turn off warnings for just this line b/c warned-about coercion is the intention here
  nrows=length(b)               # n rows
  ncells=length(d)              # n total cells (10/20/18 changed from length(c) to length(d) 
  ncols=ncells/nrows            # n cells / n rows
  v=unlist(strsplit(b[1],"\t")) # split 1st row into putative variable names by tabs
  nvars=length(v)               # find number of variables
  if (nvars==ncols) {           # if have the right number of variable names for the data (i.e. life is simple)
    f=t(matrix(d,nrow=ncols,ncol=nrows)) # create data matrix by filling in by column then transposing
    if (sum(is.na(as.numeric(v)))==0) {
      for (i in 1:ncols) v[i]=paste0("[variable ",as.character(i),"]")
    }
  } else {                      # else (i.e. life is not so simple)
    # create data matrix that excludes the first row
    d=d[(nvars+1):length(d)]      # remove first row worth of cells from the beginning of the data vector
    nrows=nrows-1               # recompute nrows for first row being removed
    ncells=length(d)            # recompute ncells as length of new d
    ncols=ncells/nrows          # recompute ncols
    f=t(matrix(d,nrow=ncols,ncol=nrows)) # create a data matrix (as above) with the remaining data (minus the first row)
    # a second try at variable names
    b1=unlist(strsplit(a,"\n")) # split data by rows
    v1=gsub(" ","\t",b1[1])     # create version of first row that replaces spaces with tabs
    v=unlist(strsplit(v1,"\t")) # split 1st row into putative variable names by tabs
    nvars=length(v)             # find number of variables
    if (!(nvars==ncols)) {      # if still don't have the right number of variable names & there are some repeated tabs
      while (grepl("\t\t", v1, fixed=TRUE)) v1=gsub("\t\t","\t",v1) # remove all double tabs (which probably come from double spaces)
      v=unlist(strsplit(v1,"\t")) # split 1st row into putative variable names by tabs
      nvars=length(v)             # find number of variables
    }
    if (!(nvars==ncols)) {        # if still don't have the right number of variable names
      if (nvars<ncols) {          # if too few variable names, then make some up
        if (v[1]=="NA") v[1]="[variable 1]"
        for (i in (nvars+1):ncols) v[i]=paste0("[variable ",as.character(i),"]")
      } else if (nvars>ncols) {   # else if too many then just take the first few
        v=v[1:ncols]
      }
    }
  }
  colnames(f)=v               # set variable names
  processeddata=f             # save data
  if (delete_casewise) {
    f=f[complete.cases(f), ]   # remove rows with one or more NA if requested
  }
  return(processeddata)
}