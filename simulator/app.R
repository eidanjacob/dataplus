library(shiny)
library(readr)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Duke Wireless Network Data Simulator"), 
  
  sidebarLayout(
     
     sidebarPanel(
       fileInput("eventData", "Event Data"),
       fileInput("locationData", "Access Point Location Data"),
       fileInput("ouiData", "Organization Unique Identifier Data"),
       numericInput("tsamp", "Sample Time (hrs)", value = 24, min = 1, max = 168),
       textInput("outName", "Output Name", value = "simulations.csv")
     ),
     
     mainPanel(
       uiOutput("goButton"),
       tableOutput("sampleTable")
     )
   )
)

matchOUI <- function(data, lookup){
  sapply(data$macaddr, function(mac){
    MAC <- substr(gsub(":", "-", toupper(mac)), 1, 8)
    return(lookup$org[lookup$prefix == MAC])
  })
}

server <- function(input, output) {
  
  options(shiny.maxRequestSize=1024^6)
  
   output$goButton <- renderUI({
     req(input$eventData)
     req(input$locationData)
     req(input$ouiData)
     req(input$tsamp)
     req(input$outName)
     actionButton("runSim", "Sample")
   })
   
   observeEvent(input$runSim, {
     eventData <- read_csv(input$eventData$datapath)
     locationData <- read_csv(input$locationData$datapath)
     ouiData <- read_csv(input$ouiData$datapath)
     
     addressInfo <- summarize(group_by(eventData, macaddr),n())
     addressInfo$org <- matchOUI(addressInfo, ouiData)
     allData <- merge(eventData, locationData)
     allData <- merge(allData, addressInfo)
     
     # Build model(s)
     # Do we model from perspective of devices arriving at aps, or from aps connecting to devices? hmm...
     # Devices arriving at aps: For each ap, model wait time + properties of 'visiting' devices. Store parameters in a table.
     
     output$sampleTable <- renderTable({
       # Render a few sims
     })
   })
}

shinyApp(ui = ui, server = server)