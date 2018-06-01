# Script to add coordinates of events to data and aggregate
require(readr)
require(dplyr)

# Parameters to play with
path.data <- "./eventData.csv"
path.coordinates <- "./locationsToCoordinates.csv"
path.locations <- "./APlist2.txt"
cell_size <- 60 # Minutes

# Data in
splunkData <- read_csv(path.data)
coordinates <- read_csv(path.coordinates)
locations <- read_csv(path.locations, col_names = FALSE)
names(locations) <- c("APname","APnum","location")
locations$location[c(2:227, 732:733)] = "Kilgo"
locations$location[228:235] = "Chapel"
locations$location[236:257] = "Gray"
locations$location[c(258:272, 634:649)] = "Divinity"
locations$location[273:371] = "Bostock"
locations$location[372:507] = "Perkins"
locations$location[c(508:578,1018:1036)] = "Rubenstein"
locations$location[579:597] = "Languages"
locations$location[598:633] = "OldChem"
locations$location[650:694] = "SocPsy"
locations$location[695:727] = "SocSci" # Possibly merge SocPsy/SocSci due to label confusion
locations$location[728:729] = "Craven"
locations$location[730:731] = "Crowell"
locations$location[734:803] = "WestUnion"
locations$location[813:815] = "Page"
locations$location[816:824] = "Card" # Wilson Gym (not called Wilson bcz of Wilson Dorm on East)
locations$location[825:855] = "Sanford"
locations$location[856:864] = "CAE" # Center for Athletic Excellence
locations$location[865:931] = "Ciemas"
locations$location[932:1017] = "French"
locations$location[:] = ""


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
# Is there any other information we would like to aggregate in this way? I don't think so.

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
  df_step$time <- c(current.time)
  df_out <- rbind(df_out, df_step)
  current.time <- current.time + cell_size * 60
}