perc_rank <- function(data,...){
  a=dim(data)             # finds dimensions of data (must have at least two columns for this function to make sense)
  v=a[2]                  # find number of columns
  for(i in 1:v) {
    ranked_data=rank(data[,i],na.last="keep");      # find ranks with NA's kept as NA's and not figured into the ranking
    vector_length=length(rank(data[,i],na.last=NA)) # find the number of rows that were given a rank
    data[,i]=(ranked_data/vector_length)*100        # compute percentile ranks and add them back into original data frame
  }
  return(data)
}