# Density map of Duke's campus for public usage.

# Load packages:
library(readr) # for reading csv files
library(leaflet) # for mapping
library(dplyr) # for data wrangling
library(shiny) # for interactive maps
library(deldir) # for voronoi cell calculations
library(sp) # for drawing polygons
library(rgdal) # for drawing polygons
library(lubridate) # for easy handling of times and dates
library(geosphere) # for haversine formula (calculate distance on sphere)

# reading in data (project folder is working directory)
coord <- read_csv("../locationsToCoordinates.csv") # locations to coordinates look up table
coord <- coord[order(coord$location),] # alphabetize location - coordinate dictionary
dukeShape <- read_csv("../dukeShape.txt", col_names = FALSE) # shape of duke's campus

# reading in event data from directory
df <- NULL
directory <- "../data" # name of directory with data
files <- list.files(directory, full.names = TRUE)
lapply(files, function(fname) {
  df <<- rbind(df, read_csv(paste0(fname)))
})

df$`_time` <- force_tz(ymd_hms(df$`_time`), "EST")

# draw duke border
p = Polygon(dukeShape)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))

# remove out-of-bounds locations
inBounds <- sapply(1:nrow(coord), function(x) {
  point.in.polygon(coord$long[x], coord$lat[x], unlist(dukeShape[,1]), unlist(dukeShape[,2]))
})
coord <- coord[inBounds == 1,]
N = nrow(coord)
# calculating voronoi cells and converting to polygons to plot on map
z <- deldir(coord$long, coord$lat) # computes cells
# convert cell info to spatial data frame (polygons)
w <- tile.list(z)
polys <- vector(mode="list", length=length(w))
for (i in seq(along=polys)) {
  pcrds <- cbind(w[[i]]$x, w[[i]]$y)
  pcrds <- rbind(pcrds, pcrds[1,])
  polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
}
SP <- SpatialPolygons(polys)
SP <- intersect(SP, sps)
for(x in 1:nrow(coord)){
  SP@polygons[[x]]@ID <- as.character(x)
}
SPDF <- SpatialPolygonsDataFrame(SP, data=data.frame(x=coord[,2], y=coord[,3]))
# tag polygons with location name
SPDF@data$ID = coord$location
sapply(1:length(coord$location), function(x){
  SPDF@polygons[[x]]@ID <- coord$location[x]
  SPDF <<- SPDF
})

# Default coordinates that provide overview of entire campus
defLong <- -78.9272544 # default longitude
defLati <- 36.0042458 # default latitude
zm <- 15 # default zoom level

# Areas of polygons were calculated in original units (degrees). The code below approximates a sq. meter measure to a square degree (In Durham)
p1 <- c(defLong, defLati)
degScale = -3
p2 <- c(defLong + 10 ^ degScale, defLati)
p3 <- c(defLong, defLati + 10 ^ degScale)
# The Haversine formula calculates distances along a spherical surface.
areaConvert = distHaversine(p1, p2) * distHaversine(p1, p3) # = square meters per 10^degScale square degrees (in Durham)
areaConvert = areaConvert / 10^(2 * degScale) # square meters per square degree

# Calculate population density 
end.time = (max(df$`_time`))

# Color palette for polygons
colorPal <- "Purples"

# Calculate population densities
locationBinnedPop <- data.frame("location" = coord$location, "pop" = c(0), "dens" = c(0))
# For each location, count the number of unique devices (MAC addresses) that are present during the time time.window.
locationBinnedPop$pop <- sapply(locationBinnedPop$location, function(x) {length(unique(df$macaddr[df$`location.y` == x]))})
densities_area  <- sapply(1:N, function(x) {100 * locationBinnedPop$pop[x] / (SPDF@polygons[[x]]@area * areaConvert)})
locationBinnedPop$dens <- densities_area

# Setting up palette for choropleth
palette_area <- colorNumeric(colorPal, locationBinnedPop$dens)

# App User Interface
ui <- fluidPage(
  leafletOutput("map", height = 850)
)

# App Server
server <- function(input, output, session) {
  
  # Getting the marker color depending on its population density
  getColor <- function(step) {
    sapply(step$dens, function(val) {
      if(val < mean(step$dens)) {
        "green"
      } else if(val > mean(step$dens)) {
        "red" 
      } else {
        "yellow"
      } 
    })
  }
  # Icon image
  icons <- awesomeIcons(
    icon = 'wifi',
    iconColor = 'black',
    library = 'fa',
    markerColor = getColor(locationBinnedPop)
  )
  
  # Creating map
  m <- leaflet() %>%
    setView("map", lng = defLong, lat = defLati, zoom = zm) %>% # sets initial map zoom & center
    addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
    addPolygons(data = SPDF, 
                layerId = locationBinnedPop$location,
                weight = 1,
                color = "black",
                fillOpacity = 0.5,
                fillColor = ~palette_area(densities_area)) %>% 
    addAwesomeMarkers(lng = coord$long, 
                      lat = coord$lat, 
                      icon = icons,
                      options = markerOptions())
  
  # Drawing map
  output$map <- renderLeaflet({ 
    m
  })
}

# Run the application 
shinyApp(ui = ui, server = server)