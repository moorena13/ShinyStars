#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Stars"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("xaxis", h3("X-Axis Variable"), choices = c("RA"), selected = "RA"),
        selectInput("yaxis", h3("Y-Axis Variable"), choices = c("RA"), selected = "RA"),
        sliderInput("xslider", h3("X-Axis Range"),
                    min = 0, max = 100, value = c(0, 100)),
        sliderInput("yslider", h3("Y-Axis Range"),
                    min = 0, max = 100, value = c(0, 100)),
        actionButton("presetButton", "Preset Inputs"),
        actionButton("prettyButton", "Pretty Inputs"),
        h4("Data retrieved from ", a(href="http://www.astronexus.com/hyg", "the HYG database."))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("graph")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
  starData <- read.csv("hygfull.csv")
  regex <- "\\S"
  namedStars <- subset(starData, grepl(regex, ProperName))
  
  
  axisfont <- list(family = "Linux Biolinum G, sans-serif",
                   size = 15,
                   color = "010d21")
  tickfont <- list(family = "Linux Biolinum G, sans-serif",
                   size = 10,
                   color = "010d21")
  
  names <- colnames(namedStars)
  names <- names[! names %in% c("StarID", "Hip", "HD", "HR", "Gliese", "BayerFlamsteed", "ProperName", "Spectrum")]
  
  updateSelectInput(session, inputId = "xaxis", label = "X-Axis Variable", choices = names, selected = "Distance")
  updateSelectInput(session, inputId = "yaxis", label = "Y-Axis Variable", choices = names, selected = "Mag")

  output$graph <- renderPlotly({
    testdata <- namedStars
    testdata$xx <- testdata[[input$xaxis]]
    testdata$yy <- testdata[[input$yaxis]]
    
    sliderXMin <- min(testdata$xx)
    sliderXMax <- max(testdata$xx)
    
    sliderYMin <- min(testdata$yy)
    sliderYMax <- max(testdata$yy)
    
    plot <- plot_ly(data = testdata,
                    source = "stars",
                    x = ~xx,
                    y = ~yy,
                    type = "scatter",
                    mode = "markers",
                    marker = list(sizeref = 1.5, sizemode = "Mag"),
                    size = ~AbsMag,
                    color = ~ColorIndex,
                    colors = c("#77cdff", "#fff67f", "#ba4141"),
                    hoverinfo = "text",
                    text = ~ProperName) %>%
      layout(plot_bgcolor="010d21", 
             xaxis = list(color = "white",
                          tickfont = tickfont,
                          title = input$xaxis,
                          titlefont = axisfont,
                          range = input$xslider),
             yaxis = list(color = "white",
                          tickfont = tickfont,
                          title = input$yaxis,
                          titlefont = axisfont,
                          range = input$yslider))
    plot
    })
  
  observe({
    
    xaxisvar <- namedStars[input$xaxis]
    xmin <- min(xaxisvar)
    xmax <- max(xaxisvar)
    xstep <- (xmax - xmin) / 10
    xmin <- xmin - xstep
    xmax <- xmax + xstep
    
    yaxisvar <- namedStars[input$yaxis]
    ymin <- min(yaxisvar)
    ymax <- max(yaxisvar)
    ystep <- (ymax - ymin) / 10
    ymin <- ymin - ystep
    ymax <- ymax + ystep
    
    updateSliderInput(session, inputId = "xslider", label = "X-Axis Range", min = xmin, max = xmax, step = xstep, value = c(xmin, xmax))
    updateSliderInput(session, inputId = "yslider", label = "Y-Axis Range", min = ymin, max = ymax, step = ystep, value = c(ymin, ymax))
  
    clicky <- event_data("plotly_click", source = "stars")
    if (!is.null(clicky))
    {
      starFriend <- namedStars[clicky$pointNumber + 1,]
      starFriendName <- starFriend$ProperName
      starFriendWebsite <- paste("https://en.wikipedia.org/wiki/", starFriendName, sep = "")
      browseURL(starFriendWebsite)
    }
  })
  
  observeEvent(input$presetButton,
               {
                 updateSelectInput(session, inputId = "xaxis", label = "X-Axis Variable", choices = names, selected = "Distance")
                 updateSelectInput(session, inputId = "yaxis", label = "Y-Axis Variable", choices = names, selected = "Mag")
               })
  
  observeEvent(input$prettyButton,
               {
                 updateSelectInput(session, inputId = "xaxis", label = "X-Axis Variable", choices = names, selected = "ColorIndex")
                 updateSelectInput(session, inputId = "yaxis", label = "Y-Axis Variable", choices = names, selected = "Mag")
               })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

