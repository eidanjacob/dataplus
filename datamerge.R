# Script to add coordinates of events to data and aggregate
require(readr)
require(dplyr)

# Parameters to play with
path.data <- "./eventData.csv"
path.coordinates <- "./locationsToCoordinates.csv"
path.locations <- "./APtoLocations.csv"
cell_size <- 60 # Minutes

# Data in
splunkData <- read_csv(path.data)
coordinates <- read_csv(path.coordinates)
locations <- read_csv(path.locations)

# Adding Location labels
splunkLocations <- splunkData %>%
  mutate(location = case_when(
    # If location is not given, (may need to expand condition) find the corresponding location in AP lookup table.
    location %in% c("nan", "default", NA) ~ locations$location[location$ap == ap, ],
    # Possibly need to clean locations variable (typos, capitalization/abbreviations, etc)
    location == "some trash" ~ "some not-trash")) 

# Match coordinates to locations. Resulting data frame has coordinates and standard location names for each event.
splunkCoordinates <- merge(splunkLocations, coordinates, by = location)

# Now produce a data frame with one row per (location, timestep) pair. 
# Count the number of unique MAC addresses associated with that location in that time step (determined by cell_size).

start.time <- min(splunkCoordinates$time)
end.time <- min(splunkCoordinates$time)

df_out <- NULL

current.time <- start.time
while(current.time < end.time){
  df_step <- coordinates
  events_step <- splunkLocations %<%
    filter(time > current.time) %<%
    filter(time <= current.time + cell_size * 60)
  df_step$MACs <- sapply(df_step$locations, function(x) {
    return( length( unique ( events_step$macaddr[events_step$location == x] ) ) )
  })
  df_out <- rbind(df_out, df_step)
  current.time <- current.time + cell_size * 60
}