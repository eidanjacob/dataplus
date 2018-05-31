require(shiny)
require(readr)
require(ggplot2)
require(ggmap)
require(maps)

me <- read_csv("../../me.csv")
start.time = (min(me$time))
end.time = (max(me$time))

ui <- fluidPage(
   
   titlePanel("Duke Wireless Data"),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("time",
                     "Time",
                     min = start.time,
                     max = end.time,
                     value = start.time)
      ),
      
      mainPanel(
         plotOutput("ggMap")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

