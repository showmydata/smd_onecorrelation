make_url <- function(settings = settings, get_all = FALSE, datalink = datalink, appurl = "[URL goes here]",...){ #%#%#%#
  
  settings <- lapply(settings, function(settings) {
    settings=gsub("#", "haash", settings, fixed=TRUE);    # Replace 1st w/ 2nd in 3rd
    settings=gsub(" ", "%20", settings, fixed=TRUE);    
    settings=gsub("&", "%amp", settings, fixed=TRUE);
    settings=gsub("\r", "%return", settings, fixed=TRUE);
    settings=gsub("\\n","\n", settings, fixed=TRUE); 
    settings=gsub("\n","newline", settings, fixed=TRUE);
    settings=gsub("▲", "triangle", settings,fixed=TRUE);
    settings=gsub("Δ", "delta1", settings, fixed=TRUE);
    settings=gsub("△", "delta2", settings, fixed=TRUE);
    settings=gsub("(","o_par",settings,fixed=TRUE);
    settings=gsub(")","c_par",settings,fixed=TRUE);
  })
  
  theurl=paste0(appurl,"/?")
  none_yet=TRUE
  if (get_all) {                    # Writes the full list of the parameters to the URL (use at beginning)
    for (i in 1:length(settings)) {
      if (names(settings)[i]=="create_url" | names(settings)[i]=="clipbtn" | names(settings)[i]=="myData" | names(settings)[i]=="datalink") 1==1 #%#%#%#
      else if (none_yet) {theurl=paste0(theurl,names(settings)[i],"=",settings[i]); none_yet=FALSE}
      else if (!none_yet) theurl=paste0(theurl,"&",names(settings)[i],"=",settings[i])
    }
  } else {
    for (i in 1:length(settings)) { # Writes only non-"" parameters to the URL (for general use)
      if (names(settings)[i]=="create_url" | names(settings)[i]=="clipbtn" | names(settings)[i]=="myData" | names(settings)[i]=="datalink") 1==1 #%#%#%#
      else if (settings[i]!="" & none_yet) {theurl=paste0(theurl,names(settings)[i],"=",settings[i]); none_yet=FALSE}
      else if (settings[i]!="" & !none_yet) theurl=paste0(theurl,"&",names(settings)[i],"=",settings[i])
    }}
  
  if (datalink !="") {
    datalink=gsub("docs.google.com/spreadsheets","goosheet",datalink,fixed=TRUE); 
    theurl=paste0(theurl,"&datalink=",datalink) 
  }
  
  return(theurl)
}
