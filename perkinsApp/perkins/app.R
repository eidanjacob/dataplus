library(shiny)
library(readr)
library(ggplot2)
library(png)
library(dplyr)
library(plotly)
library(lubridate)

ap_xy <- read_csv("../perkins_aps")
walls <- read_csv("../perkins_walls")
events <- read_csv("../../perkinsOnly.csv")
aps <- data.frame("ap" = unique(events$ap))
aps <- merge(aps, ap_xy)

start.time <- min(events$`_time`)
time.windowStart <- start.time
end.time <- max(events$`_time`)
timeStep <- 3600 * 4
apBinned <- data.frame("ap" = NULL, "pop" = NULL, "timeFrame" = NULL, "X" = NULL, "Y" = NULL, "f" = NULL)

while(end.time > time.windowStart){
  # Filter for time interval
  selInt = interval(time.windowStart, time.windowStart + timeStep)
  thisStep <- events %>%
    filter(`_time` %within% selInt)
  
  # Bin to AP
  pop <- sapply(aps$ap, function(x){
    return(length(unique(thisStep$macaddr[thisStep$ap == x])))
    })
  
  blah <- data.frame("ap" = aps$ap, "population" = pop, "timeFrame" = time.windowStart,
                  "X" = aps$transX, "Y" = aps$transY, "f" = aps$floor)
  apBinned <- rbind(apBinned, blah)
  time.windowStart = time.windowStart + timeStep
}
ui <- fluidPage(

  titlePanel("Perkins Library"),
  
  sliderInput("time", "Time", min = start.time, max = end.time,
              value = start.time, animate = animationOptions(interval=1500),
              step = dseconds(timeStep)),

  plotlyOutput("plot", height = "900px", width = "1500px")

)

server <- function(input, output) {
  
  maxpop <- max(apBinned$population)
  
  output$plot <- renderPlotly({
    apBinned %>%
      filter(timeFrame == input$time) %>%
      plot_ly(x = ~X, 
              y = ~Y, 
              z = ~f,
              hoverinfo = "text",
              text = ~paste("AP name:", ap, "\n Unique MACs:", population),
              marker = list(
                color = ~population, 
                colorscale = c(0, "#d3d3d3", maxpop, "#ff0000"),
                cauto = F,
                cmin = 0,
                cmax = maxpop,
                showscale = TRUE),
              type = "scatter3d",
              mode = "markers") %>%
      add_markers() %>%
      layout(showlegend = FALSE)
  })
}

shinyApp(ui = ui, server = server)

