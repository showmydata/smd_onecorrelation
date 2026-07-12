parse_url <- function(urlstring, session = session, ...){
  
  urlstring=sub('.', '', urlstring)
  urlstring=sub('resourcekey=&', '', urlstring)
  urlstring=strsplit(urlstring,split="&"); urlstring=urlstring[[1]]
  keys=NA; values=NA
  for (i in 1:length(urlstring)) {
    a=urlstring[i]
    a=strsplit(a,split="="); a=a[[1]]
    keys[i]=a[1]
    values[i]=a[2]
  }
  updatethese=cbind(keys,values)
  updatethese=gsub("%20", " ", updatethese, fixed=TRUE);    # Replace 1st w/ 2nd in 3rd
  updatethese=gsub("haash", "#", updatethese, fixed=TRUE); 
  updatethese=gsub("%amp", "&", updatethese, fixed=TRUE); 
  updatethese=gsub("%return", "\r", updatethese, fixed=TRUE); 
  updatethese=gsub("newline", "\n", updatethese, fixed=TRUE); 
  updatethese=gsub("triangle", "▲", updatethese, fixed=TRUE);
  updatethese=gsub("delta1", "Δ", updatethese, fixed=TRUE);
  updatethese=gsub("delta2", "△", updatethese, fixed=TRUE);
  updatethese=gsub("o_par", "(", updatethese, fixed=TRUE); 
  updatethese=gsub("c_par", ")", updatethese, fixed=TRUE); 
  updatethese=gsub("goosheet", "docs.google.com/spreadsheets", updatethese, fixed=TRUE); 
  
  info=read.table(file="functions/info.txt",header=TRUE)
  for (i in 1:length(updatethese[,1])) {
    a=c(updatethese[i,1],updatethese[i,2])
    b=info[info$name==a[[1]],2]
    if (b=="checkboxInput") updateCheckboxInput(session, inputId=a[[1]], value=as.logical(a[[2]]))
    if (b=="colourInput") updateColourInput(session, inputId=a[[1]], value=a[[2]])
    if (b=="radioButtons") updateRadioButtons(session, inputId=a[[1]], selected=a[[2]])
    if (b=="selectInput") updateSelectInput(session, inputId=a[[1]], selected=a[[2]])
    if (b=="sliderInput") updateSliderInput(session, inputId=a[[1]], value=as.numeric(a[[2]]))
    if (b=="textInput") updateTextInput(session, inputId=a[[1]], value=a[[2]])
    if (b=="textAreaInput") updateTextAreaInput(session, inputId=a[[1]], value=a[[2]])
    if (b=="numericInput") updateNumericInput(session, inputId=a[[1]], value=a[[2]])
    if (b=="linkInput") {urlhash <- parseQueryString(session$clientData$url_hash); if (!is.null(urlhash[['#gid']])) updateTextInput(session, inputId=a[[1]], value=paste0(a[[2]],"#gid=",urlhash)) else updateTextInput(session, inputId=a[[1]], value=a[[2]])}
  }
  
  return(session)
}