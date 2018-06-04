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
locations$location[c(258:272, 634:649, 7247:7248)] = "Divinity"
locations$location[273:371] = "Bostock"
locations$location[372:507] = "Perkins"
locations$location[c(508:578,1018:1036)] = "Rubenstein"
locations$location[579:597] = "Languages"
locations$location[c(598:633, 7403:7403)] = "OldChem"
locations$location[650:694] = "SocPsy"
locations$location[695:727] = "SocSci" # Possibly merge SocPsy/SocSci due to label confusion
locations$location[728:729] = "Craven"
locations$location[730:731] = "Crowell"
locations$location[734:803] = "WestUnion"
locations$location[813:815] = "Page"
locations$location[c(816:824, 7387:7388, 7234:7236, 7223:7226, 6124:6146)] = "Card" # Wilson Gym (not called Wilson bcz of Wilson Dorm on East)
locations$location[825:855] = "Sanford"
locations$location[c(856:864, 7233:7233)] = "CAE" # Center for Athletic Excellence
locations$location[c(865:931, 7483:7487, 7244:7244)] = "Ciemas"
locations$location[c(932:1017, 7211:7211)] = "French"
locations$location[1037:1039] = "Schwartz"
locations$location[1040:1336] = "Few"
locations$location[c(1337:1423, 7207:7207)] = "Cameron"
locations$location[1424:1440] = "Telcom"
locations$location[1441:1500] = "Hudson"
locations$location[1501:1560] = "Physics"
locations$location[1561:1635] = "Allen"
locations$location[1636:1763] = "Wannamaker"
locations$location[1764:1799] = "North"
locations$location[1800:1897] = "BioSci"
locations$location[1898:1980] = "Law"
locations$location[1981:2105] = "Fuqua"
locations$location[2106:2112] = "FacultyClub"
locations$location[2114:2129] = "Teer"
locations$location[2130:2143] = "TUNL"
locations$location[2144:2370] = "LSRC"
locations$location[c(2394:2395, 6147:6148)] = "Yoh"
locations$location[c(2396:2412, 6149:6165)] = "FELL" # Free Electron Laser Laboratory


locations$location[6393:6756] = "Edens"
locations$location[c(7208:7209, 6167:6392)] = "Wallace"
locations$location[c(7212:7222, 7210:7210)] = "NA" # Completely Unknown
locations$location[7227:7231] = "Baseball" # Includes Stadium and Pressbox
locations$location[c(7232:7232, 6757:6856)] = "Bryan"
locations$location[7237:7237] = "NA" # Cell Headend-Bld
locations$location[7238:7238] = "ChillerPlant1"
locations$location[7239:7243] = "ChillerPlant2"
locations$location[7245:7246] = "Medctr"
locations$location[7249:7249] = "CarlPenthouse"
locations$location[7293:7293] = "Erwin" # Erwin Road Const Trailer
locations$location[7294:7302] = "Penn"
locations$location[c(7303:7303, 7251:7292)] = "Enviro"
locations$location[7304:7306] = "GolfShed"
locations$location[7307:7368] = "Gross"
locations$location[7369:7371] = "HalfTimeHouse"
locations$location[7372:7372] = "Intramural"
locations$location[7373:7380] = "Kennedy" # Kennedy Tower
locations$location[c(7381:7381, 6857:7206)] = "Keohane"
locations$location[c(7384:7386, 6166:6166)] = "Koskinen" 
locations$location[7389:7390] = "Fence" # Kville Fence
locations$location[c(7392:7393, 5897:6123)] = "LSRC"
locations$location[7394:7399] = "PascalField"
locations$location[7400:7401] = "Murray" # William David Murray Building - Athletics
locations$location[7402:7402] = "NPavilion" # On Medical Campus
locations$location[7404:7415] = "NA" # Unknown
locations$location[7416:7419] = "Phytotron"
locations$location[7419:7419] = "NA" # Practice Hut
locations$location[7420:7427] = "Sbutters"
locations$location[7428:7467] = "ScottFam"
locations$location[7468:7468] = "Skanska" # Skanska steamplant
locations$location[7469:7470] = "NA" # Substation-Joe's Office & Garage
locations$location[7471:7477] = "Tennis"
locations$location[7478:7481] = "VarsityGolf"
locations$location[7488:7490] = "Grounds" # West Grounds

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
