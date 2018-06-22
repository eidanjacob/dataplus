library(shiny)
library(readr)

wallsdf <- NULL
apsdf <- NULL

ui <- fluidPage(
   
   navbarPage("Single Building Wireless", id = "tabs",
              tabPanel("File Upload", 
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("shape", "Floor Shapes"),
                           fileInput("aps", "Access Point Positions"),
                           fileInput("events", "Event Data"),
                           actionButton("read", "Go")
                         ),
                         
                         mainPanel(
                           textOutput("diagnostic")
                         )
                       )
              ),
              tabPanel("Visualizations") # To Do: port polygon-generating code from previous iteration
   )
)

server <- function(input, output) {
  options(shiny.maxRequestSize=1024^4)
  observeEvent(input$read, {
    # The user has indicated they are ready to proceed. Read the submitted data frames and output diagnostics.
    req(input$shape)
    req(input$aps)
    req(input$events)
    tryCatch(
      {
        wallsdf <- read_csv(input$shape$datapath)
        apsdf <- read_csv(input$aps$datapath)
        eventsdf <- read_csv(input$events$datapath)
      },
      error = function(e) {stop(safeError(e))}
    )
    
    # Null or non-csv inputs will not make it past this point. Check that columns name match:
    if(!all(c("transX", "transY", "floor") %in% names(wallsdf)) | 
       !all(c("transX", "transY", "floor", "ap") %in% names(apsdf))){
      warning("Invalid Input! Fix before Proceeding.")
    } else {
      # Passed basic sanity check, run more advanced?
      eventsdf <- eventsdf %>% filter(ap %in% apsdf$ap)
      floorsw <- sort(unique(wallsdf$floor))
      floorsa <- sort(unique(apsdf$floor))
      if(any(floorsw != floorsa)){
        warning("Files contain information on different floors.")
      }
      output$diagnostic <- renderText(paste(as.character(nrow(apsdf)), "APs on", 
                                 as.character(length(floorsa)), "floors.",
                                 "Events:", as.character(nrow(eventsdf))))
    }
    
  })
     
}

shinyApp(ui = ui, server = server)