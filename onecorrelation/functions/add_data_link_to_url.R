add_data_link_to_url <- function(theurl, session,...){
 url2pass=theurl
  urlparameters <- parseQueryString(session$clientData$url_search)        # Get any passed parameters
  if (!is.null(urlparameters[['datalink']])) {                            # If there's a datalink in the passed parameters
    datalink2add=urlparameters["datalink"]
    datalink2add=gsub("goosheet", "docs.google.com/spreadsheets", datalink2add, fixed=TRUE); # Change goosheet back to full google URL
    if (grepl("google.com/spreadsheets", datalink2add, fixed = TRUE)) {  # If there's google sheet info somewhere in the passed parameters
      hash <- parseQueryString(session$clientData$url_hash)               # Search for hash in url
      if (!is.null(hash[['#gid']])) {                                     # If there is a #gid in the hash portion of the url
        url_hash=session$clientData$url_hash                              # then get the hash
        datalink2add=paste0(datalink2add,url_hash)                      # and add the hash at the end of the url
      }
      url2pass = paste0(theurl,"&","datalink=",datalink2add)
    } else {
      if (grepl("dropbox", datalink2add, fixed = TRUE) & !grepl("raw=1", datalink2add, fixed = TRUE)) datalink2add = paste0(datalink, "&raw=1") 
      url2pass = paste0(theurl,"&","datalink=",datalink2add)
    }
  }
  return(url2pass)
} 
