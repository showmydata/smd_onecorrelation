get_data_from_url <- function (data,session,datalink) { 
  
  data2return=data;
  
  urlparameters <- parseQueryString(session$clientData$url_search)        # Get any passed parameters
  if (!is.null(urlparameters[['datalink']])) {                            # If there's a datalink in the passed parameters
    datalink2read=urlparameters["datalink"]
    datalink2read=gsub("goosheet", "docs.google.com/spreadsheets", datalink2read, fixed=TRUE); # Change goosheet back to full google URL
    if (grepl("google.com/spreadsheets", datalink2read, fixed = TRUE)) {  # If there's google sheet info somewhere in the passed parameters
      hash <- parseQueryString(session$clientData$url_hash)               # Search for hash in url
      if (!is.null(hash[['#gid']])) {                                     # If there is a #gid in the hash portion of the url
        url_hash=session$clientData$url_hash                              # then get the hash
        datalink2read=paste0(datalink2read,url_hash)                      # and add the hash at the end of the url
      }
      data2return <-gsheet2tbl(datalink2read) 
    } else {
      if (grepl("dropbox", datalink2read, fixed = TRUE) & !grepl("raw=1", datalink2read, fixed = TRUE)) datalink2read = paste0(datalink2read, "&raw=1") 
      data2return <-read_csv(as.character(datalink2read))
    }
  }
  if (datalink!="") {
    datalink2read=datalink; 
    if (grepl("google.com/spreadsheets", datalink2read, fixed = TRUE)) data2return <-gsheet2tbl(datalink2read)
    else if (grepl(".csv", datalink2read, fixed = TRUE)) {
      if (grepl("dropbox", datalink2read, fixed = TRUE) & !grepl("raw=1", datalink2read, fixed = TRUE)) datalink2read = paste0(datalink2read, "&raw=1") 
      data2return <-read.csv(datalink2read)}
    else if (grepl(".xlsx", datalink2read, fixed = TRUE)) data2return <- read_excel(datalink2read)
    else data2return=data}
  
  return(data2return)
}