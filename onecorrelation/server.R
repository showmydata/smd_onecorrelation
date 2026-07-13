# one correlation shiny app
# Git info: https://carpentries.github.io/sandpaper-docs/github-pat.html

# Load libraries
library(shiny)
library(stringr)
library(psych)
library(readr)
library(MASS)
library(gsheet)

source("functions/equate_zscored_axis_ranges.R") 
source("functions/perc_rank.R") 
source("functions/jitter_by_percent_min_wn2.R") 
source("functions/process_label.R")
source("functions/make_url.R") 
source("functions/parse_url.R") 
source("functions/add_data_link_to_url.R")
source("functions/get_data_from_url.R")

shinyServer(  # Initiate the shiny server
function(input, output, session) { # Create the function -- added 'session' for URL project 3/22/24

# Re-render UI with user-specified height and width
  output$ui_plot <- renderUI({plotOutput("contents", width = input$plotwidth*8*3, height = input$plotheight*8*3)})
    
# Run function that makes the plot
  output$contents <- renderPlot( { # Call Shiny function that makes the plot

###############################
# GET DEFAULT OR PASTED DATA
###############################

      ### DATA INPUT ###
      if(input$myData>"") {
        # Next 3 lines added 8/15/23
        v=unlist(strsplit(input$myData,"\n")); v=unlist(strsplit(v[1],"\t")); # Read 'header' exactly, regardless of characters
        if(!all(is.na(as.numeric(v)))) for (i in 1:length(v)) v[i]=paste("column ",i); # If 'header' has any numbers (is not all words), replace with "column i"
        d0=gsub(",","",input$myData); d0=gsub("'","",d0); d0=gsub("‘","",d0); d0=gsub("’","",d0); d0=gsub('"',"",d0); d0=gsub("“","",d0); d0=gsub("”","",d0) # Replace various characters that produce errors
        for (i in 1:length(v)) { vv=v[i]; # For each variable label
          if (nchar(vv)>20) { # If the variable label is >20 length, add a carriage return at the last space before the 20th character
            b=unlist(gregexpr(' ', vv)); c=max(b[b<20]); vv=paste(substr(vv,1,c-1), "\n", substr(vv,c+1,nchar(vv)), sep=""); v[i]=vv
        }}
        
        d <- read.table(text = d0, sep = '\t', header = FALSE); # Get data assuming first row has data
        if (v[1]=="column 1") 
          
        d2 <- read.table(text = d0, sep = '\t', header = TRUE); # Get data assuming first row has column label
      } else {
        d=cars[,1:2]; v=c("car speed","car stopping distance"); colnames(d) <- v
        d=as.data.frame(get_data_from_url(d,session,input$datalink)); 
        v=colnames(d)
      }
    # 3/27/24 -- copy-pasted from TMB version of app -- hoping it will deal with periods and other characters in google sheet
    v=gsub(".", " ", v, fixed=TRUE); v=gsub(",","",v); v=gsub("'","",v); v=gsub("‘","",v); v=gsub("’","",v); v=gsub('"',"",v); v=gsub("“","",v); v=gsub("”","",v) # Replace various characters that produce errors
    for (i in 1:length(v)) { vv=v[i]; # For each variable label
    if (nchar(vv)>20) { # If the variable label is >20 length, add a carriage return at the last space before the 20th character
      b=unlist(gregexpr(' ', vv)); c=max(b[b<20]); vv=paste(substr(vv,1,c-1), "\n", substr(vv,c+1,nchar(vv)), sep=""); v[i]=vv
    }}
    
      ## DEAL WITH >2 COLUMNS OF DATA ##
      d=d[rowSums(d=="") != ncol(d), ] # Exclude completely blank rows from data set
      isdata=FALSE;
        dkeep=d;          # keep this copy of d for its labels
      for (i in 1:length(d)) { # for each column, set to numeric then see if it is at least 20% numbers
        d[,i]=as.numeric(d[,i]); isdata[i]=FALSE; if(sum(!is.na(d[,i]))/length(d[,i])>.2) {isdata[i]=TRUE}
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
      if (length(d)>2) havelabels=TRUE else havelabels=FALSE
      d=dd;
      cc=complete.cases(d) # find the complete cases (excludes NAs in first row left by labels)
      if (havelabels) thelabels=thelabels[cc] # If there are labels, exclude incomplete rows
      if (input$spearman==TRUE) d[,1:2]=perc_rank(d); # If spearman, do percentile rank
      x=d[cc,1]; y=d[cc,2]; # Exclude incomplete rows in data
      d=as.data.frame(d); # This is kept for download of residuals with original data (including missing values)
      # Deal with polychoric correlations
      expand_contingency <- function(a, b, c, d) {
        A <- rep(c(1,1,0,0), times = c(a, b, c, d))
        B <- rep(c(1,0,1,0), times = c(a, b, c, d))
        data.frame(A, B)
      }
      do_poly = FALSE
      if (input$poly) {
        if (input$poly_type=='data') {
          if (input$poly & sum(complete.cases(unique(d[,1])))<=input$cats & sum(complete.cases(unique(d[,2]))) ) {a=polychoric(d); poly_r=round(a$rho[2,1],digits=3); do_poly=TRUE} else do_poly=FALSE
        }
        if (input$poly_type=='table') {
          a=c(input$yesyes, input$yesno, input$noyes, input$nono)
          if (all(sapply(a, function(x) length(x) == 1 && !is.na(suppressWarnings(as.numeric(x)))))) { # If all valid numbers
            a=round(as.numeric(a)) # Convert strings to numbers and round if necessary
            d = expand_contingency(a[1], a[2], a[3], a[4]); b=polychoric(d); poly_r=round(b$rho[2,1],digits=3)
            x=d[,1]; y=d[,2]; cc=complete.cases(d)
            do_poly = TRUE
            v[1]="x"; v[2]="y";
          } else do_poly = FALSE
        }
        if (input$poly_type=='concordance') {
          a=c(input$concordant, input$discordant, input$prevalence)
          if (all(sapply(a, function(x) length(x) == 1 && !is.na(suppressWarnings(as.numeric(x))) && as.numeric(a[3])<=1 && as.numeric(a[3])>=0 ))) { # If all valid numbers and prevalence 0-1
            a=as.numeric(a); a[1:2]=round(a[1:2]) # Convert strings to numbers and round if necessary
            nono=(a[1] + a[2] - a[3]*a[1] - 2*a[3]*a[2]) / a[3]
            a=c(a[1], a[2], a[2], nono)
            d = expand_contingency(a[1], a[2], a[3], a[4]); b=polychoric(d); poly_r=round(b$rho[2,1],digits=3)
            x=d[,1]; y=d[,2]; cc=complete.cases(d)
            do_poly = TRUE
            v[1]="x"; v[2]="y";
        } else do_poly = FALSE
        }
      }
      drawlines=FALSE
      if (do_poly & input$poly_sim & input$poly_type!='data') {
        n <- nrow(d); rho <- poly_r; 
        p <- (1:n)/(n+1)
        x <- qnorm(p); z <- qnorm(sample(p)) # x and z have same marginal distribution but different order
        x <- x - mean(x); z <- z - mean(z) # center
        z <- z - sum(x*z)/sum(x^2) * x # orthogonalize
        x <- x / sqrt(mean(x^2)); z <- z / sqrt(mean(z^2)) # scale
        y <- rho * x + sqrt(1 - rho^2) * z # mix
        prevalence=sum(a[1],a[2])/sum(a); xline=quantile(x,1-prevalence); yline=quantile(y,1-prevalence); drawlines=TRUE # draw cutoffs
      }
        

          
#######################################
# PROCESS DATA AND COMPUTE STATISTICS
#######################################
# Jitter data percent of minimum difference between points for each column
    x1=jitter_by_percent_min_wn2(x,input$xjitter,ranked=input$spearman)
    y1=jitter_by_percent_min_wn2(y,input$yjitter,ranked=input$spearman)
# Choose axis ranges
    cushion=input$cushion/100
    ranges=equate_zscored_axis_ranges(cbind(x,y),cushion=cushion)
    xspan=ranges[1,2]-ranges[1,1]
    xlim=ranges[1,]
    ylim=ranges[2,]
    xaxs="r"; yaxs="r";   # Confirm the default of 4% cushioning on axes, then change below if user specifies axis range
    if (input$xaxisrange!="") {                                         # If y axis range given
      rng=as.numeric(unlist(strsplit(input$xaxisrange,",")));           # Replace only the y axis value(s) given so far
      xlim[1:length(rng)]=rng; xaxs="i";                                # Input only the values given so far
    }
    if (input$yaxisrange!="") {                                         # If y axis range given
      rng=as.numeric(unlist(strsplit(input$yaxisrange,",")));           # Replace only the y axis value(s) given so far
      ylim[1:length(rng)]=rng; yaxs="i";                                # Input only the values given so far
    }
    
# Get variable labels
    v1=process_label(input$xvariablelabel,v[1]); xlabel=v1; 
    v2=process_label(input$yvariablelabel,v[2]); ylabel=v2
    extra_margin=max(str_count(v1,"\n"),str_count(v2,"\n")) # max carriage returns in a label (to adjust plot margins)
    title_extra=str_count(input$graphtitle,"\n") # carriage returns in the title
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
    ct = cor.test(x,y); 
    ci = round(ct$conf.int,3)
    pv = signif(ct$p.value,3)
    thelm = lm(y ~ x, data = as.data.frame(c(x,y)))
    rawslope = round(thelm$coefficients[[2]],2)
    lmci = round(confint(thelm),2)
    if(input$spearman==FALSE) {
      outputtext <- paste("r = ", r, ", n = ", n, ", 95% CI [", ci[1], ", ", ci[2], "]", sep="")
      moretext <- paste("<br>mean x = ", round(mean(x),2), ", mean y = ", round(mean(y),2), ", p = ", pv)
      stillmoretext <- paste("<br>raw slope = ", rawslope, ", 95% CI [", lmci[2,1], ", ", lmci[2,2], "]", sep="")
      }
    if(input$spearman==TRUE) {
      outputtext <- paste("rho = ", r, ", n = ", n, ", 95% CI [", ci[1], ", ", ci[2], "]", sep="")
      moretext <- paste("<br>p = ", pv)
      stillmoretext <- ""
      }
    if(do_poly) {
      outputtext <- paste("rho (polychoric) = ", poly_r[1], ", n = ", n, "<br><br><small>When a polychoric correlation has two categories, it is called tetrachoric.</small>", sep="")
      if(input$poly_type=='concordance') moretext <- paste("<br><small>Assumes true discordance is double the observed, because discordant pairs are twice as hard to detect.</small>") else moretext <- paste("<br><small>Assumes direct observation of all four counts in a random, population-based sample.</small>")
      if(input$poly_sim) stillmoretext <- paste("<br><small>Blue lines show cutoffs that on average yield the given numbers.</small>") else stillmoretext <- paste("")
    }
# Give intervals around y for given x
    if (input$xvalue != "") {a = as.numeric(input$xvalue); if (is.finite(a)) {
      yhat = round(thelm$coefficients[2]*a + thelm$coefficients[1],2)
      ytext = paste("predicted y when x = ", a, " is ", yhat, sep="")
      a = data.frame(x=a)
      if (input$cix) {cix=predict(thelm, newdata = a, interval = 'confidence'); cixtext = paste(", 95% CI [", round(cix[1,2],2), ", ", round(cix[1,3],2), "]", sep="")} else cixtext=""
      if (input$pix) {pix=predict(thelm, newdata = a, interval = 'prediction'); pixtext = paste(", 95% PI [", round(pix[1,2],2), ", ", round(pix[1,3],2), "]", sep="")} else pixtext=""
      intervaltext = paste(ytext, cixtext, pixtext, sep="")
    } else intervaltext = ""} else intervaltext = ""
    
    output$text=renderText(paste(outputtext,moretext,stillmoretext,"<br>",intervaltext,"<br><br>",sep=""));
    
##################################
# DRAW PLOT IN WINDOW
##################################
    c=col2rgb(input$color_dot)/255 # Find rgb for selected dot color
    if(input$plotwidth == input$plotheight) dosquare=TRUE else dosquare=FALSE
    makemyplot <- function(dosquare=TRUE) {
      if (dosquare) par(pty="s") # Forces the scatterplot to be square
      par(mar = c(4.5 + extra_margin*2, 4.5 + extra_margin*2, 4 + title_extra*3, 4) + 0.1) # adjust default axis margin for multiline labels: par(mar = c(lower,left,top,right)) as par(mar = c(5,4,4,2) + 0.1)
      if (input$xaxisnums!="") {xaxisnums=as.numeric(unlist(strsplit(input$xaxisnums,","))); xaxt="n"} else xaxt="s"
      if (input$yaxisnums!="") {yaxisnums=as.numeric(unlist(strsplit(input$yaxisnums,","))); yaxt="n"} else yaxt="s"
      plot(x1, y1, xaxt = xaxt, yaxt = yaxt,
           main=input$graphtitle, xlab="", ylab=ylabel, cex.main=3, cex.axis=1.5,   # title & axis labels & font sizes
           xlim=xlim, ylim=ylim, frame=FALSE, cex.lab=2, xaxs=xaxs, yaxs=yaxs,       # axis ranges & no frame
           pch=as.numeric(input$dottype), cex=input$dotsize/20,                     # dot type and size
           col=rgb(red=c[1], green=c[2], blue=c[3], alpha=input$dotopacity/100)     # opacity
           )
      if (input$xaxisnums!="") axis(1, at = xaxisnums, cex.axis=1.5)
      if (input$yaxisnums!="") axis(2, at = yaxisnums, cex.axis=1.5)
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
      
      if (drawlines) {abline(v = xline, col = "blue", lwd = 1.5); abline(h = yline, col = "blue", lwd = 1.5)}
    }
    makemyplot(dosquare)
      settings=reactiveValuesToList(input);
      theurl=make_url(settings, get_all=FALSE, 
                      datalink=input$datalink, 
                      appurl="https://showmydata.shinyapps.io/onecorrelation"); 
      theurl=gsub("\\n","\n",theurl,fixed=TRUE); theurl=gsub("\n","newline",theurl,fixed=TRUE); 
      output$clip <- renderUI({ rclipButton(inputId = "clipbtn", icon = icon("clipboard"), 
                                            label = "Copy link with current settings", 
                                            clipText = theurl)}) 
    
    
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
        if (all(is.na(data[1,]))) data_to_download=data[-1,] else data_to_download=data # Slight hack to eliminate initial unintended row of NAs when no column labels are given
        write.csv(data_to_download, file, row.names = FALSE)
      }
    )
  })
  
  # Get link, Make link, Add URL
  observe({ urlstring=session$clientData$url_search; if (urlstring!="") session <- parse_url(urlstring, session) }) # updates session
  
  })