# one correlation shiny app 

# Load libraries
library(shiny)
library(colourpicker)
library(rclipboard) # added for URL project 3/22/24

# Begin UI function (everything below is within this function)
shinyUI(fluidPage(
  
  rclipboardSetup(), # added for URL project 3/22/24
  
  # Application title
  titlePanel("Correlation: 2 measures"),
 
  # Sidebar 
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
                    "Tweak axes/stats" = "axes",
                    "Adjust labels & plot size" = "labels",
                    "Data import" = "dataimport"),
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
      checkboxInput('spearman', 'percentile ranks', FALSE),
      checkboxInput('poly', 'polychoric correlations', FALSE),
      conditionalPanel(condition="input.poly",
         radioButtons('poly_type', label = "type", choiceNames = list("pasted data", "2 x 2 table", "concordance"), choiceValues = list("data", "table", "concordance")),
         conditionalPanel(condition="input.poly_type=='table' | input.poly_type=='concordance'",
         checkboxInput('poly_sim', 'simulate underlying data', FALSE)),
         conditionalPanel(condition="input.poly_type=='data'",
             sliderInput(inputId = "cats", label = "use when both variables have this # of categories or less", min = 2, max = 7, value = 2)),
         conditionalPanel(condition="input.poly_type=='table'",
                textInput("yesyes", label = "both yes", width = "50%", value = 10, placeholder = "10"),
                textInput("nono", label = "both no", width = "50%", value = 10, placeholder = "10"),
                textInput("yesno", label = "1st yes, 2nd no", width = "50%", value = 10, placeholder = "10"),
                textInput("noyes", label = "1st no, 2nd yes", width = "50%", value = 10, placeholder = "10")
         ),
         conditionalPanel(condition="input.poly_type=='concordance'",
                textInput("concordant", label = "# concordant", width = "50%", value = 10, placeholder = "10"),
                textInput("discordant", label = "# discordant", width = "50%", value = 10, placeholder = "20"),
                textInput("prevalence", label = "prevalence (proportion)", width = "50%", value = .5, placeholder = ".5")
         )
      )
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
                                            value = 40),
                                      checkboxInput('int', 'predicted y given x', FALSE),
                                      conditionalPanel(condition="input.int",
                                                      textInput("xvalue", label = "x value", value = "", width = "50%", placeholder = ""),
                                                      checkboxInput('cix', 'confidence interval', FALSE),
                                                      checkboxInput('pix', 'prediction interval', FALSE))
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
                      checkboxInput('showstats', 'show stats on plot', FALSE),
                      sliderInput(inputId = "cushion",
                                  label = "add white space around data",
                                  min = 0,
                                  max = 100,
                                  value = 10),
                      textInput("xaxisrange", label = "x range", value = "", width = "50%", placeholder = "min,max"),
                      textInput("yaxisrange", label = "y range", value = "", width = "50%", placeholder = "min,max"),
                      checkboxInput('specifyaxisnums', 'specify axis numbers', FALSE),
                      conditionalPanel(condition="input.specifyaxisnums",
                                       textInput("xaxisnums", label = "x axis numbers", value = "", width = "75%", placeholder = "number 1,number 2..."),
                                       textInput("yaxisnums", label = "y axis numbers", value = "", width = "75%", placeholder = "number 1,number 2..."))
    ),

# Adjust labels & plot size
      conditionalPanel(condition="input.options=='labels'",
      textAreaInput("graphtitle", label = "title", value = "", width = "100%", placeholder = "Use [return] to split title"),
      textAreaInput("xvariablelabel", label = "x variable label", value = "", width = "100%", rows = "2", placeholder = "Use [return] to split label"),
      textAreaInput("yvariablelabel", label = "y variable label", value = "", width = "100%", rows = "2", placeholder = "Use [return] to split label"),
      sliderInput(inputId = "plotwidth",
                  label = "plot width",
                  min = 0,
                  max = 100,
                  value = 25,
                  step = 0.1),
      sliderInput(inputId = "plotheight",
                  label = "plot height",
                  min = 0,
                  max = 100,
                  value = 25,
                  step = 0.1)
      ),

# Data import
conditionalPanel(condition="input.options=='dataimport'",
                 textInput("datalink", 
                           label = HTML("paste shared google sheets link<h6><strong style='font-weight:normal'>
                                 Linked file must contain <i>only</i> the data you wish to plot, with a top row of column labels, 2 columns of numbers, and, optionally, a 3rd column containing datapoint labels. Column and datapoint labels must be text, not numbers.</strong></h6>"), 
                           value = "", width = "85%", placeholder = "https://docs.google.com/spread...")
)


      ),

# Contents of main panel
    mainPanel(
      uiOutput('ui_plot'), # Plot itself (defined in server file)
      
      # Further contents of main panel...
      hr(style = "margin: 0px 30px 10px 30px; border: .5px solid #a6a6a6"),
      downloadButton(outputId = "down", label = "Download graph as..."),
      radioButtons("filetype", label = NULL, choices = list("png", "pdf")),
      
      # added for URL project 3/22/24
      uiOutput("clip"), 
      tags$h6(HTML(" ")),
      
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
      tags$h6("1. Line slope: By default, the physical slope of the fitted least-squares line equals the correlation (r or rho) value because the graph is square and the x and y axis ranges are chosen to span an equal number of standard deviations. This feature is retained when the 'white space' slider is adjusted but not when the 'x/y range' or the graph 'height/width' are adjusted."),
      tags$h6("2. Spline: Spline is fit via R's smooth.spline function, with smoothness set via 'spar' argument."),
      tags$h6("3. Primacy of non-jittered data: All lines/curves are fit, and all stats and residuals are computed, using non-jittered data; since shown residuals follow jittered data, they do not exactly touch the line/curve."),
      tags$h6("4. Jitter units: For raw data, unit is percentage of smallest distance between two dots, calculated separately for each variable; for ranked data, unit is percentage of 10 percentile units; each point is randomly jittered over a range equal to this unit."),
      tags$style(type="text/css",
         ".shiny-output-error { visibility: hidden; }",
         ".shiny-output-error:before { visibility: hidden; }")
      )
  )
))