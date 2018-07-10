require(dplyr)
library(stringr)
library(ggplot2)
library(readr)

# This is the bit that merges one day worth of byte data
##############
# # Reading in data and cleaning it.
# # This merges all of the data within one day and assigns time stamps to it.
# df0419 <- read_csv("./data-plus-2018_wireless-ap-logs_20180419.csv")
# 
# # match aps to locations, merge for coordinates
# df0419 <- df0419[!is.na(df0419$ap),] # remove observations with no ap
# 
# # Some aps are in splunk data with name, some with number - code below matches location using whichever is available
# nameMatch = which(validLocations$APname %in% df0419$ap) # find which aps have their name in the data
# numMatch = which(validLocations$APnum %in% df0419$ap) # find which aps have their number in the data
# validLocations$ap = c(NA) # new "flexible" column to store either name or number
# validLocations$ap[nameMatch] = validLocations$APname[nameMatch]
# validLocations$ap[numMatch] = validLocations$APnum[numMatch]
# 
# validLocations <- merge(coord, validLocations) # link coordinates to locations
# # use the new "flexible" ap variable to merge coordinates onto df
# df0419 <- merge(df0419, validLocations, by = "ap") # this is the slow step
# 
# bytesData <- NULL
# 
# nums <- seq(0, 5500, 500)
# for(i in seq(15500, 235500, 10000)) { # since the end file numbers jump at 1000 increments -- the numbers represent minutes
#   nums <- c(nums, seq(i-5500, i, 500))
# }
# nums[121:length(nums)] <- format(nums[121:length(nums)], scientific = FALSE)
# nums <- str_pad(nums, 6, pad = "0")
# 
# currTime <- as.POSIXct("2018-04-19 00:00:00 UTC", tz = "UTC") # data starts at midnight while Splunk data starts at 4am
# incre <- 0
# for(i in nums){ # combining each minutes' data
#   dataRaw <- read.csv(paste0("./data-plus-2018_wireless-bytes_20180419-",
#                              as.character(i)))
#   dataRaw$time <- currTime + incre
#   bytesData <- rbind(bytesData, dataRaw)
#   incre <- incre + 60 * 5 # since each file represents 5 minutes
# }
# 
# write.csv(bytesData, "./bytesData0419.csv", row.names = F)
##############

# This is the bit that merges the bytes data with Splunk data
##############
bytesData <- read_csv("./bytesData0419.csv")

currTime <- min(bytesData$time)
end.time <- max(bytesData$time)
cleanedDF <- NULL

while(currTime < end.time) { # labeling time frames accordingly/matching bytesData to Splunk data
  timeInt <- interval(currTime, currTime + 60 * 5)
  thisStep <- df0419 %>% 
    filter(`_time` %within% timeInt)
  ipStep <- bytesData %>% 
    filter(time == currTime)
  # making new dataframe
  temp <- merge(thisStep, ipStep, by.x = "ipaddr", by.y = "SrcAddr")
  
  #############
  # we will assume each ip matches to one mac during each time frame for simplicity, 
  # this is a check to see if our assumption is true
  # it only checks it; it doesn't deal with it
  if(length(thisStep$asa_code) != 0) {
    ipsToMacs <- aggregate(macaddr ~ ipaddr, temp, unique) # matches each ip to one (or more) macaddrs, this is here as a future ref
    ipsToMacs$dups <- lapply(ipsToMacs$macaddr, function(x) if(length(x) > 1) return(TRUE) else return(FALSE))
    if(TRUE %in% ipsToMacs$dups) { # our assumption was false :(
      print("oh no!")
      print(currTime)
    }
  }
  #############
  
  cleanedDF <- rbind(cleanedDF, temp) 
  currTime <- currTime + 60 * 5
}

# Remove duplicates based on totBytes, time and ipaddr
# Since the bytes data is in chunks, when the data is merged it might create
# duplicate rows for each minute in the chunk, if an event was registered in that minute
temp <- cleanedDF %>% 
  distinct(TotBytes, time, ipaddr, .keep_all = TRUE)
##############

# seeing total bytes by location
locsDF <- temp %>% 
  group_by(location.y, long, lat, time) %>% 
  summarise(tot = sum(as.numeric(TotBytes)))

# seeing total bytes by ip and mac
ipMacDF <- temp %>% 
  group_by(ipaddr, macaddr, time) %>% 
  summarise(tot = sum(as.numeric(TotBytes)))



