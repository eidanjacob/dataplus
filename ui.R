library(shiny) # package for creating interactive visualizations

# Setting ranges for slider, though there is probably a neater way to do this (ideally the server would handle all data)
me <- read_csv("./me.csv")
start.time = (min(me$time))
end.time = (max(me$time))

# Creates interface
shinyUI(fluidPage(
  titlePanel("Duke Wireless Data"),

    sidebarLayout(
      sidebarPanel(
        # input a time to show chronologically nearby records on map
        sliderInput("time", "Time", min = start.time, max = end.time, value = start.time)
      ),
    
      mainPanel(
        leafletOutput("map")
      )
    )
  )
)