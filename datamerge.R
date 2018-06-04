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
<<<<<<< HEAD
locations$location[c(1, 3457,3459:3469, 3484, 3496:3497, 3616:3617, 3651:3662,
                     3666, 3716:3717, 3737, 3754)] = NA
locations$location[c(2:227, 732:733, 3629:3630, 3755:3980, 4485:4486)] = "Kilgo"
locations$location[c(228:235, 3981:3988)] = "Chapel"
locations$location[c(236:272, 634:649, 3494:3495, 3989:4025, 4387:4402)] = "Divinity"
locations$location[c(273:371, 4026:4124)] = "Bostock"
locations$location[c(372:507, 4125:4260)] = "Perkins"
locations$location[c(508:578, 1018:1036, 4261:4331, 4771:4789)] = "Rubenstein"
locations$location[c(579:597, 3638, 4332:4350)] = "Languages"
locations$location[c(598:633, 3650, 4351:4386)] = "OldChem"
locations$location[c(650:694, 4403:4447)] = "SocPsy"
locations$location[c(695:727, 4448:4480)] = "SocSci" # Possibly merge SocPsy/SocSci due to label confusion
locations$location[c(728:729, 4481:4482)] = "Craven"
locations$location[c(730:731, 4483:4484)] = "Crowell"
locations$location[c(734:803, 4487:4556)] = "WestUnion"
locations$location[c(813:815, 4566:4568)] = "Page"
locations$location[c(816:824, 2371:2393, 3481:3483, 3634:3637, 4569:4577)] = "Card" # Wilson Gym (not called Wilson bcz of Wilson Dorm on East)
locations$location[c(825:855, 4578:4608)] = "Sanford"
locations$location[c(856:864, 3480, 4609:4617)] = "CAE" # Center for Athletic Excellence
locations$location[c(865:931, 3491, 3730:3734, 4618:4684)] = "Ciemas"
locations$location[c(932:1017, 4686:4770)] = "French"
locations$location[c(1037:1039, 3667:3674, 4790:4792)] = "Schwartz"
locations$location[c(1040:1336, 3618, 4793:5089)] = "Few"
locations$location[c(1337:1423, 5090:5176)] = "Cameron"
locations$location[c(1424:1440, 3619, 5177:5193)] = "Telcom"
locations$location[c(1441:1500, 5194:5253)] = "Hudson"
locations$location[c(1501:1560, 5254:5313)] = "Physics"
locations$location[c(1561:1635, 5314:5386)] = "Allen"
locations$location[c(1636:1763, 5389:5516)] = "Wannamaker"
locations$location[c(1764:1799, 5517:5552)] = "North"
locations$location[c(1800:1897, 3663:3665, 5553:5650)] = "BioSci" # Including greenhouse and phytotron
locations$location[c(1898:1980, 5651:5733)] = "Law"
locations$location[c(1981:2105, 5734:5858)] = "Fuqua"
locations$location[c(2106:2112, 5859:5865)] = "FacultyClub"
locations$location[c(2114:2129, 5867:5882)] = "Teer"
locations$location[c(2130:2143, 5883:5896)] = "TUNL"
locations$location[c(2144:2370, 3639:3640)] = "LSRC"
locations$location[c(2394:2395, 3738:3753)] = "Yoh"
locations$location[2396:2412] = "FELL"
locations$location[c(2413, 3621:3627, 3631:3633)] = "Koskinen"
locations$location[c(2414:2639, 3455:3466, 3729)] = "WallaceWade"
locations$location[2640:3003] = "Edens"
locations$location[c(3004:3103, 3479)] = "BryanCenter"
locations$location[c(3104:3453, 3628)] = "Keohane"
locations$location[3454] = "Cameron"
locations$location[3458] = "French"
locations$location[3470:3473] = "Aquatic"
locations$location[3474:3478] = "Baseball"
locations$location[3485] = "Chiller1"
locations$location[3486:3490] = "Chiller2"
locations$location[c(3492:3493, 3649)] = "MedCenter"
locations$location[c(3498:3539, 3550)] = "EnviromentalHall"
locations$location[3541:3549] = "PennPav"
locations$location[c(3551:3553, 3725:3728)] = "Golf"
locations$location[c(3554:3615, 5866)] = "Gross"
locations$location[3641:3646] = "Pascal"
locations$location[c(3647:3648, 3676:3714)] = "Murray" # Includes Scott Pav
locations$location[3715] = "Steamplant"
locations$location[3718:3724] = "Tennis"
locations$location[4557:4565] = "Flowers"
=======
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
>>>>>>> 0c1bccdce58a5f89a26a110fe140539f9814d826


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
