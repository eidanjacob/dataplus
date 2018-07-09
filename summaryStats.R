# Script that calculates summary/interesting stats.
# Stats calculated:
# Avg time taken to get to certain places
# Most common paths

library(dplyr)
library(lubridate)

# # Mess with these numbers if you want.
# timeSteps = c("1hr" = 60*60, "2hr" = 2*60*60, "4hr" = 4*60*60) # in seconds
# # timeSteps = c("4 hr" = 4*60*60)
# delay = 2700 # in milliseconds
# # ------------------------------
# 
# start.time = (min(df$`_time`))
# end.time = (max(df$`_time`))
# 
# macsToLocList <- list()
# 
# end.times <- rep(end.time, length(timeSteps))
# 
# for(i in 1){ # didn't feel like replacing i -> 1
#   timeStep <- timeSteps[i]
#   # Bin populations, calculate densities at each timestep, and cache for future plotting
#   time.windowStart = start.time # time.window for selection
#   macsToLoc <- NULL
# 
#   while(end.time > time.windowStart){
# 
#     # Filter for time interval
#     selInt = interval(time.windowStart, time.windowStart + timeStep)
#     thisStep <- df %>%
#       filter(`_time` %within% selInt)
# 
#     # For each macaddr, keep track of where it currently is
#     macs <- data.frame("macaddr" = thisStep$macaddr,
#                        "location" = thisStep$location.y,
#                        "long" = thisStep$long,
#                        "lat" = thisStep$lat,
#                        "time.window" = c(time.windowStart),
#                        "realTime" = c(thisStep$`_time`))
#     macs <- macs[order(macs$realTime), ]
#     macsToLoc <- rbind(macsToLoc, macs)
# 
#     end.times[i] <- time.windowStart
#     time.windowStart = time.windowStart + timeStep
#   }
# 
#   # Cache these guys away for later
#   macsToLocList[[i]] <- macsToLoc
# }
# 
# # mac data -- to be used for point plotting
# macData <- macsToLocList[[1]]

source("functions.R") # import findIndex function

# Calculating avg time to get to certain places
#############
from <- "Perkins"
to <- "WestUnion"
uniqMacs <- macData %>% # initial filtering
  filter(location %in% c(from, to))
uniqMacs <- unique(uniqMacs$macaddr)

n <- 1
breakNum <- 500 # how many macaddrs to search through
times <- vector(mode = mode(macData$realTime[[1]]), length = breakNum)

macDF <- sapply(sample(uniqMacs,breakNum), FUN = findIndex, macdf=macData, fromLoc=from, toLoc=to, betweenInte=60 * 10, distInte=1)
macDFNonNull <- macDF[-which(sapply(macDF, is.null))] # getting rid of null values
times <- sapply(macDFNonNull, FUN = function(x) { # calculating the travel times
  macLocs <- x$orig[x$indexFrom:x$indexTo, ]
  diff <- difftime(macLocs$startTimeReal[[length(macLocs$startTimeReal)]], macLocs$endTimeReal[[1]])
  return(as.numeric(diff, units = "secs"))
})

times2 <- sapply(macDFNonNull, FUN = function(x) { # calculating the number of locations visited
  diff <- x$orig[x$indexFrom:x$indexTo, ]
  return(diff$id[[length(diff$startTimeReal)]] - diff$id[[1]])
})

cat("Calculated using a sample size of", length(times$t), "macaddrs.", "\n")
cat("Average time from", from, "to", to, "is", mean(times$t), "seconds.", "\n")
cat("Average number of buildings visited", mean(times$n), "\n")
# it seems to take between 3-5 minutes from Perk to WU
#############

