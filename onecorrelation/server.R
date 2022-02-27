# one correlation shiny app

# Load libraries
library(shiny)
library(stringr)
library(psychometric)
source("functions/equate_zscored_axis_ranges.R") 
source("functions/perc_rank.R") 
source("functions/jitter_by_percent_min_wn2.R") 
source("functions/process_label.R")
# consider implementing something like one of these...
# https://github.com/sbihorel/rclipboard for copy-to-clipboard
# or this: library(clipr)

# Begin the function to plot (everything below is within this function)
shinyServer(
  function(input, output) { 
# Grabs user-specified height and width
    output$ui_plot <- renderUI({plotOutput("contents", width = input$plotwidth*8*3, height = input$plotheight*8*3)})
    output$contents <- renderPlot( { # Call Shiny function that makes the plot

###############################
# GET DEFAULT OR PASTED DATA
###############################
      # # Process any pasted data (new)
      # if(input$myData>"") {
      #   tt <- read.table(text = input$myData, sep = '\t', header = FALSE)
      #   tt2=lapply(tt, as.numeric); 
      #   tt2=as.data.frame(tt2);
      #   if (all(is.na(tt2[1,]))) t <- read.table(text = input$myData, sep = '\t', header = TRUE)
      #   else t <- read.table(text = input$myData, sep = '\t', header = FALSE)
      #   v=colnames(t)
      # } else {
      #   t=cars[,1:2]; v=c("car speed","car stopping distance") 
      # }
      # t=as.data.frame(t); 
      # v=gsub("\\.", " ", as.character(v))
      # v=strtrim(v, 48)
      # d=t; cc=complete.cases(d)
      # 
      # if (input$spearman==TRUE) d[,1:2]=perc_rank(d);
      # x=d[cc,1]; y=d[cc,2];
      

      
      ### DATA INPUT ###
      if(input$myData>"") {
        d <- read.table(text = input$myData, sep = '\t', header = FALSE)
        d2 <- read.table(text = input$myData, sep = '\t', header = TRUE); 
        v=as.character(colnames(d2))
        v=gsub("\\.", " ", v)
        v=strtrim(v, 45)
      } else {
        d=cars[,1:2]; v=c("car speed","car stopping distance")
      }
      ## DEAL WITH >2 COLUMNS OF DATA ##
      isdata=FALSE;
      dkeep=d;          # keep this copy of d for its labels
      for (i in 1:length(d)) { # for each column, set to numeric then see if it is at least half numbers
        d[,i]=as.numeric(d[,i]); isdata[i]=FALSE; if(sum(!is.na(d[,i]))/length(d[,i])>.5) {isdata[i]=TRUE}
      }
      dolabels=FALSE      # set default to no labels
      if (length(d)>2) {  # if more than two columns
        dolabels=TRUE     # plan to assign one to labels
        w1=which(isdata)  # find the columns with data
        w2=which(!isdata) # find the columns without data
        dd=d[,w1[1:2]]    # get the first two data columns
        v=v[w1[1:2]]      # get the labels for the first two data columns
        if (length(w2)==0) thelabels=dkeep[,w1[3]] else thelabels=dkeep[,w2[1]] # get the labels column from one of two places
        thelabels[1]=NA
      } else {
        dd=d[,1:2]; # if column number wasn't greater than 2, just graph the two columns
      }
      d=dd;
      cc=complete.cases(d)
      if (input$spearman==TRUE) d[,1:2]=perc_rank(d);
      x=d[cc,1]; y=d[cc,2];
      d=as.data.frame(d);
      
      
          
#######################################
# PROCESS DATA AND COMPUTE STATISTICS
#######################################
# Jitter data percent of minimum difference between points for each column
    x1=jitter_by_percent_min_wn2(x,input$xjitter,ranked=input$spearman)
    y1=jitter_by_percent_min_wn2(y,input$yjitter,ranked=input$spearman)
# Choose axis ranges
    ranges=equate_zscored_axis_ranges(d,cushion=.1)
    xspan=ranges[1,2]-ranges[1,1]
    if(input$xaxisrange=="") {xlim=ranges[1,]} else {rng=input$xaxisrange; rng=unlist(strsplit(rng,",")); rng=as.numeric(rng); xlim=rng;}
    if(input$yaxisrange=="") {ylim=ranges[2,]} else {rng=input$yaxisrange; rng=unlist(strsplit(rng,",")); rng=as.numeric(rng); ylim=rng;}
# Get variable labels
    v1=process_label(input$xvariablelabel,v[1]); xlabel=v1; 
    v2=process_label(input$yvariablelabel,v[2]); ylabel=v2
    extra_margin=max(str_count(v1,"\n"),str_count(v2,"\n")) # max carriage returns in a label (to adjust plot margins)
# Prepare data for download and plotting of residuals
    data=d                                        # initiate data set to download using original data with all missing values
    xynames=c(v1,v2)                              # current name of x and y variables
    vnames=xynames                                # start a list of variable names, labeling the original data with the current axis labels
    if (input$lsline) {
      lslinemodel <- lm(y ~ poly(x,1))            # fit lsline model
      lslineresiduals=residuals(lslinemodel)      # compute residuals
      data=cbind(data,NA)                         # add NA column to downloadable data
      data[cc,ncol(data)]=lslineresiduals         # add lsline residuals to (original) data to download
      vnames=c(vnames,paste(xynames[2],"_ controlling for _ ",xynames[1],"_ via least-squares line")) # name lsline residuals
      lsline_rsq=round(1-((sd(lslineresiduals)^2)/(sd(y)^2)),2); # compute variance explained
      output$lsline <- renderText(paste("least squares line = ", lsline_rsq, "<br>", sep="")) # generate text for variance explained
    } else {output$lsline <- renderText(paste("", sep=""))}
   if (input$polynomial & length(unique(x))>input$polynomial_degree+2) { # poly requires a few more unique x data points than degree
      polynomialmodel <- lm(y ~ poly(x,input$polynomial_degree))         # fit polynomial model
      polynomialresiduals=residuals(polynomialmodel)                     # compute residuals of polynomial model
      data=cbind(data,NA)                                                # add NA column to downloadable data
      data[cc,ncol(data)]=polynomialresiduals                            # add polynomial residuals to (original) data to download
      vnames=c(vnames,paste(xynames[2],"_ controlling for _",xynames[1],"_ via polynomial of degree",input$polynomial_degree)) # name polynomial residuals
      polynomial_rsq=round(1-((sd(polynomialresiduals)^2)/(sd(y)^2)),2); # compute variance explained
      output$polynomial <- renderText(paste("     polynomial = ", polynomial_rsq, "<br>", sep="")) # generate text for variance explained
    } else {output$polynomial <- renderText(paste("", sep=""))}
    if (input$spline & length(unique(x))>4) {                           # if doing spline and x and y data both of reasonable length
      smoothingSpline = smooth.spline(x, y, spar=input$spline_smoothness/100) # fit spline model
      splineresiduals=residuals(smoothingSpline)                        # compute residuals of spline model
      data=cbind(data,NA)                                               # add NA column to downloadable data
      data[cc,ncol(data)]=splineresiduals                               # add polynomial residuals to (original) data to download
      vnames=c(vnames,paste(xynames[2],"_ controlling for _",xynames[1],"_ via spline with smoothness parameter (spar) of",input$spline_smoothness)) # name spline residuals
      spline_rsq=round(1-((sd(splineresiduals)^2)/(sd(y)^2)),2);        # compute variance explained
      output$spline <- renderText(paste("     spline = ", spline_rsq, "<br>", sep="")) # generate text for variance explained
    } else {output$spline <- renderText(paste("", sep=""))}
    colnames(data)=vnames # put names in data file
# Compute correlation and generate text to display
    r=round(cor(x,y),3)
    n=length(x)
    ci=round(CIr(r,n=n,level=.95),3)
    if(input$spearman==FALSE) {outputtext <- paste("r = ", r, ", n = ", n, ", 95% CI [", ci[1], ", ", ci[2], "]", sep=""); output$text=renderText(paste(outputtext,"<br>",sep=""))}
    if(input$spearman==TRUE) {outputtext <- paste("rho = ", r, ", n = ", n, ", 95% CI [", ci[1], ", ", ci[2], "]", sep=""); output$text=renderText(paste(outputtext,"<br>",sep=""))}
    
##################################
# DRAW PLOT IN WINDOW
##################################
    c=col2rgb(input$color_dot)/255 # Find rgb for selected dot color
    if(input$plotwidth == input$plotheight) dosquare=TRUE else dosquare=FALSE
    makemyplot <- function(dosquare=TRUE) {
      if (dosquare) par(pty="s") # Forces the scatterplot to be square
      par(mar = c(5+extra_margin*2,4+extra_margin*2,4,2) + 0.1) # adjust default axis margin for multiline labels: par(mar = c(lower,left,top,right)) as par(mar = c(5,4,4,2) + 0.1)
      plot(x1, y1, 
           main=input$graphtitle, xlab="", ylab=ylabel, cex.main=3, cex.axis=1.5,   # title & axis labels & font sizes
           xlim=xlim, ylim=ylim, frame=FALSE, cex.lab=2,                            # axis ranges & no frame
           pch=as.numeric(input$dottype), cex=input$dotsize/20,                     # dot type and size
           col=rgb(red=c[1], green=c[2], blue=c[3], alpha=input$dotopacity/100)     # opacity
           )
      mtext(v1, side = 1, line = str_count(v1,"\n")*2+3, cex=2)
      if(input$xyline) {
        abline(0,1, col=input$color4, lwd=input$lw_xyline/10)                                                            # draws x=y line
      }
      if(input$lsline) {
        abline(lm(y~x), col=input$color1, lwd=input$lw_lsline/10)                                                            # draws least-squares line
        if (input$showresiduals) apply(cbind(x1,x1,y1,y1-lslineresiduals),1,function(coords){lines(coords[1:2],coords[3:4],col=input$color1)}) # plots lsline residual lines
      }
      if (input$polynomial & length(unique(x))>input$polynomial_degree+2 & length(unique(y))>1) { # poly requires a few more unique x data points than degree
        t=cbind(x,y); t=t[order(t[,1]),]                                # create ordered x & y
        model1 <- lm(t[,2] ~ poly(t[,1],input$polynomial_degree))       # fit model
        z1 <- predict(model1)                                           # find the model predictions
        lines(t[,1], z1, col=input$color2, lw=input$lw_polynomial/10)              # draw model predictions
        if (input$showresiduals) apply(cbind(x1,x1,y1,y1-polynomialresiduals),1,function(coords){lines(coords[1:2]+xspan/500,coords[3:4],col=input$color2)}) # plot polynomial residual lines
      }
      if (input$spline & length(unique(x))>4 & length(unique(y))>4) {
        smoothingSpline = smooth.spline(x, y, spar=input$spline_smoothness/100)
        lines(smoothingSpline,lw=input$lw_spline/10,col=input$color3)
        if (input$showresiduals) apply(cbind(x1,x1,y1,y1-splineresiduals),1,function(coords){lines(coords[1:2]-xspan/500,coords[3:4],col=input$color3)})     # plot spline residual lines
      }
      if (input$rug) {rug(x1,side=1,quiet=TRUE,ticksize=.01); rug(y1,side=2,quiet=TRUE,ticksize=.01)}
      if (input$showstats) mtext(text=paste("     ",outputtext,sep=""), side=3, cex=1.25, adj=0, line=-1, font=2)
      if (dolabels) {text(x1, y1, labels=thelabels, cex=0.9, pos=3, col=rgb(0,0,0,.5))}
    }
    makemyplot(dosquare)
    
##################################
# DOWNLOAD PLOT AND DATA
##################################
# Download plot
    output$down <- downloadHandler(
      filename =  function() {
        paste("myplot", input$filetype, sep=".")
      },
      # the below is a function that writes a plot to a type of file
      content = function(file) {
        if(input$filetype == "png")
          png(file, units="in", width=input$plotwidth/3.333, height=input$plotheight/3.333, res=500) # make png file
        else
          pdf(file, width=input$plotwidth/3.33, height=input$plotheight/3.33) # open the pdf device
        makemyplot(dosquare)
        dev.off()  # turn the device off
      })
# Download data
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("mydata", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data, file, row.names = FALSE)
      }
    )
  })
  })