library(plotly)

starData <- read.csv("hygfull.csv\\hygfull.csv")
regex <- "\\S"
namedStars <- subset(starData, grepl(regex, ProperName))

names <- colnames(namedStars)
names <- names[! names %in% c("StarID", "Hip", "HD", "HR", "Gliese", "BayerFlamsteed")]

axisfont <- list(family = "Arial, sans-serif",
                 size = 15,
                 color = "010d21")

testdata <- namedStars
testdata$xx <- testdata["Mag"]
testdata$yy <- testdata["Distance"]
testdata$xx

graph <- plot_ly(data = namedStars, 
                 x = ~Distance, 
                 y = ~Mag, 
                 type = "scatter",
                 mode = "markers",
                 marker = list(sizeref = 1.5, sizemode = "Mag"),
                 size = ~Mag,
                 color = ~ColorIndex,
                 colors = c("#77cdff", "#fff67f", "#ba4141"),
                 hoverinfo = "text",
                 text = ~ProperName) %>%
  layout(plot_bgcolor="010d21",
         xaxis = list(color = "white",
                      title = "Distance (Parsecs)",
                      titlefont = axisfont),
         yaxis = list(color = "white",
                      title = "Magnitude",
                      titlefont = axisfont))
graph


