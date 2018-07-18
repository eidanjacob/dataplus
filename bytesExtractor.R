require(dplyr)
library(stringr)
library(ggplot2)
library(readr)

# Note to self: test that it still works after function-ifying it

# Takes the raw bytes filename, the date, and the start digits, either 000500 or 000000
# and then writes it to a csv.
writeBytes <- function(raw="./data-plus-2018_wireless-bytes_20180419-", startTime=0, startDate ="0419") {
  bytesData <- NULL

  nums <- seq(startTime, 5500, 500)
  for(i in seq(15500, 235500, 10000)) { # since the end file numbers jump at 1000 increments -- the numbers hours:minutes:seconds
    nums <- c(nums, seq(i-5500, i, 500))
  }
  nums[121:length(nums)] <- format(nums[121:length(nums)], scientific = FALSE)
  nums <- str_pad(nums, 6, pad = "0")

  if(startTime == 0) {
    currTime <- as.POSIXct("2018-04-19 00:00:00 EST", tz = "EST") 
  } else {
    currTime <- as.POSIXct("2018-04-19 00:05:00 EST", tz = "EST")
  }
  
  incre <- 0
  for(i in nums){ # combining each minutes' data
    dataRaw <- read_csv(paste0(raw, as.character(i)), col_types = "cn")
    dataRaw$time <- currTime + incre
    bytesData <- rbind(bytesData, dataRaw)
    incre <- incre + 60 * 5 # since each file represents 5 minutes
  }
  
  write.csv(bytesData, paste0("./bytesData", startDate, ".csv"), row.names = F) 
  
  return(bytesData)
}

# Merging the bytes data with Splunk data
mergeBytesSplunk <- function(bData, sData) {
  currTime <- min(bData$time)
  end.time <- max(bData$time)
  cleanedDF <- NULL
  
  while(currTime < end.time) { # labeling time frames accordingly/matching bytesData to Splunk data
    timeInt <- interval(currTime, currTime + 60 * 5)
    thisStep <- sData %>% 
      filter(`_time` %within% timeInt)
    ipStep <- bData %>% 
      filter(time == currTime)
    # making new dataframe
    temp <- merge(thisStep, ipStep, by.x = "ipaddr", by.y = "SrcAddr")
    
    #############
    # we will assume each ip matches to one mac during each time frame for simplicity, 
    # this is a check to see if our assumption is true
    # it only checks it and stops the whole program; it doesn't deal with it
    if(length(thisStep$asa_code) != 0) {
      ipsToMacs <- aggregate(macaddr ~ ipaddr, temp, unique) # matches each ip to one (or more) macaddrs, this is here as a future ref
      ipsToMacs$dups <- lapply(ipsToMacs$macaddr, function(x) if(length(x) > 1) return(TRUE) else return(FALSE))
      if(TRUE %in% ipsToMacs$dups) { # our assumption was false :(
        stop("The assumption was false. Write code to handle these cases.")
        # sample handling:
        # ipsToMacs <- ipsToMacs[!ipsToMacs$dups, ] # delete duplicates
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
  temp$TotBytes <- as.numeric(temp$TotBytes)
  return(temp)
}


