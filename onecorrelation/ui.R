# one correlation shiny app

# Load libraries
library(shiny)
library(colourpicker)

# Begin UI function (everything below is within this function)
shinyUI(fluidPage(
  titlePanel("Correlation: 2 measures"),
    sidebarLayout(
    sidebarPanel(
      
# Data input
      textAreaInput("myData", "DATA", "", width = 200, height = 200, placeholder = "[Paste, from spreadsheet, 2 columns of data with non-number labels in top row; optionally, add a 3rd column containing datapoint labels]"),
      
# Hacks
      tags$style("input[type='checkbox']:checked+span{font-weight:bold;}"), # Hack to get checkboxes to show up bold when unchecked
      tags$style("input[type='checkbox']:not(:checked)+span{font-weight:bold;}"), # Hack to get checkboxes to show up bold when unchecked
      tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # hack to remove minor ticks on sliders

# Options to select from (each of these has a section below)
      selectInput(inputId="options", label="OPTIONS:",
                  choices=c("*** select ***" = "select",
                    "Manage data point visibility" = "dotvisibility",
                    "Transform data" = "perc",
                    "Fit lines/curves" = "fit",
                    "Tweak axes" = "axes",
                    "Adjust labels & plot size" = "labels"),
                  selected = NULL),

# Manage data point visibility
conditionalPanel(condition="input.options=='dotvisibility'",
                 radioButtons("dottype", label = "point type", choiceNames = list("dot", "ring"), choiceValues = list("16", "1")),
                 colourInput(inputId="color_dot", label=NULL, value = "black", showColour = c("both"), palette = c("square"), allowedCols = NULL, allowTransparent = TRUE, returnName = TRUE),
                 sliderInput(inputId = "dotsize",
                             label = "point size",
                             min = 1,
                             max = 100,
                             value = 75),
                 sliderInput(inputId = "xjitter",
                             label = "x jitter",
                             min = 0,
                             max = 100,
                             value = 0),
                 sliderInput(inputId = "yjitter",
                             label = "y jitter",
                             min = 0,
                             max = 100,
                             value = 0),
                 sliderInput(inputId = "dotopacity",
                             label = "point opacity",
                             min = 0,
                             max = 100,
                             value = 50)
),

# Transform data
      conditionalPanel(condition="input.options=='perc'",
      checkboxInput('spearman', 'percentile ranks', FALSE)
      ),

# Fit lines/curves
      conditionalPanel(condition="input.options=='fit'",

                       checkboxInput('lsline', 'line: least-squares', TRUE),
                       conditionalPanel(condition="input.lsline",
                       #checkboxInput('lsline_showresiduals', 'show residuals', FALSE),
                       colourInput(inputId="color1", label=NULL, value = "black", showColour = c("both"), palette = c("square"), allowedCols = NULL, allowTransparent = TRUE, returnName = TRUE),
                       sliderInput(inputId = "lw_lsline",
                                   label = "line width",
                                   min = 0,
                                   max = 100,
                                   value = 40)
                       ),
                       
                       hr(style = "margin: 0px 30px 10px 30px; border: .5px solid #a6a6a6"),
                       
                       checkboxInput('xyline', 'line: x=y line', FALSE),
                       conditionalPanel(condition="input.xyline",
                       colourInput(inputId="color4", label=NULL, value = "gray", showColour = c("both"), palette = c("square"), allowedCols = NULL, allowTransparent = TRUE, returnName = TRUE),
                       sliderInput(inputId = "lw_xyline",
                                    label = "line width",
                                    min = 0,
                                    max = 100,
                                    value = 40)
                       ),
                       
                       hr(style = "margin: 0px 30px 10px 30px; border: .5px solid #a6a6a6"),
                       
                       checkboxInput("polynomial", "curve: least-squares", FALSE),
                       conditionalPanel(condition="input.polynomial",
                       colourInput(inputId="color2", label=NULL, value = "blue", showColour = c("both"), palette = c("square"), allowedCols = NULL, allowTransparent = TRUE, returnName = TRUE),
                       sliderInput(inputId = "polynomial_degree",
                                   label = "degree",
                                   min = 2,
                                   max = 7,
                                   value = 2),
                       sliderInput(inputId = "lw_polynomial",
                                   label = "line width",
                                   min = 0,
                                   max = 100,
                                   value = 40)
                       ),
                       
                       hr(style = "margin: 0px 30px 10px 30px; border: .5px solid #a6a6a6"),
                       
                       checkboxInput("spline", "curve: spline", FALSE),
                       conditionalPanel(condition="input.spline",
                       colourInput(inputId="color3", label=NULL, value = "red", showColour = c("both"), palette = c("square"), allowedCols = NULL, allowTransparent = TRUE, returnName = TRUE),
                       sliderInput(inputId = "spline_smoothness",
                                   label = "smoothness",
                                   min = 1,
                                   max = 100,
                                   value = 50),
                       sliderInput(inputId = "lw_spline",
                                   label = "line width",
                                   min = 0,
                                   max = 100,
                                   value = 40)
                       )
                       
      ),

# Tweak axes
        conditionalPanel(condition="input.options=='axes'",
                      checkboxInput('rug', 'project data onto axes', FALSE),
                      textInput("xaxisrange", label = "x range", value = "", width = "50%", placeholder = "min,max"),
                      textInput("yaxisrange", label = "y range", value = "", width = "50%", placeholder = "min,max")
      ),

# Adjust labels & plot size
      conditionalPanel(condition="input.options=='labels'",
      checkboxInput('showstats', 'show stats on plot', TRUE),
          #conditionalPanel(condition="input.options=='labels'",
      textInput("graphtitle", label = "title", width = "50%", placeholder = "[title]"), 
      textAreaInput("xvariablelabel", label = "x variable label", value = "", width = "100%", rows = "2", placeholder = "Use [return] to split label"),
      textAreaInput("yvariablelabel", label = "y variable label", value = "", width = "100%", rows = "2", placeholder = "Use [return] to split label"),
      sliderInput(inputId = "plotwidth",
                  label = "plot width",
                  min = 0,
                  max = 100,
                  value = 25),
      sliderInput(inputId = "plotheight",
                  label = "plot height",
                  min = 0,
                  max = 100,
                  value = 25)
      )

      ),

# Contents of main panel
    mainPanel(
      uiOutput('ui_plot'), # Plot itself (defined in server file)
      
      # Further contents of main panel...
      hr(style = "margin: 0px 30px 10px 30px; border: .5px solid #a6a6a6"),
      downloadButton(outputId = "down", label = "Download graph as..."),
      radioButtons("filetype", label = NULL, choices = list("pdf", "png")),
      hr(style = "margin: 0px 30px 10px 30px; border: .5px solid #a6a6a6"),
      tags$h4("Statistics"),
      htmlOutput("text"),
      conditionalPanel(condition="input.lsline || input.polynomial || input.spline",
        tags$span(style="font-size: 11pt; font-weight:normal; text-decoration: underline", "Proportion variance explained by..."),
        htmlOutput("lsline"),
        htmlOutput("polynomial"),
        htmlOutput("spline"),
        tags$b(HTML("<br>"))
      ),
      hr(style = "margin: 0px 30px 10px 30px; border: .5px solid #a6a6a6"),
      downloadButton("downloadData", "Download original data and residuals"), 
      checkboxInput("showresiduals", HTML("<span style='font-weight:normal;'>show residuals</span>"), FALSE),
      hr(style = "margin: 0px 30px 10px 30px; border: .5px solid #a6a6a6"),
      tags$b("Notes..."),
      tags$h6("1. Line slope: By default, the physical slope of a fitted least-squares line equals the correlation (r or rho) value due to x and y axis ranges being chosen to span an equal number of standard deviations."),
      tags$h6("2. Spline: Spline is fit via R's smooth.spline function, with smoothness set via 'spar' argument."),
      tags$h6("3. Primacy of non-jittered data: All lines/curves are fit, and all stats and residuals are computed, using non-jittered data; since shown residuals follow jittered data, they do not exactly touch the line/curve."),
      tags$h6("4. Jitter units: For raw data, unit is percentage of smallest distance between two dots, calculated separately for each variable; for ranked data, unit is percentage of 10 percentile units; each point is randomly jittered over a range equal to this unit."),
      tags$style(type="text/css",
         ".shiny-output-error { visibility: hidden; }",
         ".shiny-output-error:before { visibility: hidden; }")
      )
  )
))