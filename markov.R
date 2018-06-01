# Markov.R functions for markov modeling of movement around campus

transition_prob <- function(dataset, minTime = 0, maxTime = 60){
  # Returns a table with probabilities for transitions between locations (states) of appearance of a MAC address
  # MinTime and MaxTime set limits for how temporally distant records can be (minutes)
  location_list <- unique(dataset$location)
  n <- length(location_list)
  res <- matrix(data = rep(0, n*n), nrow = n, ncol = n, dimnames = list("rows" = location_list, "cols" = location_list))
  macs <- unique(dataset$macaddr)
  for(mac in macs){
    # Generate timeline for this device (location, time), add valid transitions to table
    timeline <- dataset[dataset$macaddr == mac, ]
    if(nrow(timeline) < 2){
      next
    }
    timeline <- timeline[order(timeline$time), ]
    for(i in 2:length(timeline$time)){
      timeDiff <- timeline$time[i] - timeline$time[i-1]
      if(timeDiff < minTime*60 | timeDiff > maxTime*60){
        next
      } else {
        start <- timeline$location[i-1]
        end <- timeline$location[i]
        res[start][end] <- 1 + res[start][end]
      }
    }
  }
  s <- sum(res)
  if(s == 0){
    print("No valid transitions.")
    return(NULL)
  } else{
    return(res / s)
  }
}

summarize_traffic <- function(A, B, dataset, minTime = 0, maxTime = 60){
  # Returns statistics about travel between locations A and B (can be same location!)
  require(dplyr)
  AandB <- dataset %<%
    filter(location == A | location == B)
  macs = unique(AandB$macaddr)
  AtoB.trips = 0
  AtoB.time = 0
  BtoA.trips = 0
  BtoA.time = 0
  for(mac in macs){
    timeline <- dataset[dataset$macaddr == mac, ]
    if(nrow(timeline) < 2){
      next
    }
    timeline <- timeline[order(timeline$time), ]
    for(i in 2:nrow(timeline)){
      if(timeline$location[i-1] == A & timeline$location[i] == B){
        AtoB.trips = AtoB.trips + 1
        AtoB.time = double(AtoB.time + timeline$time[i] - timeline$time[i-1])
      } else if(timeline$location[i-1] == B & timeline$location[i] == A){
        BtoA.trips = BtoA.trips + 1
        BtoA.time = double(BtoA.time + timeline$time[i] - timeline$time[i-1])
      }
    }
  }
  if(A == B){
    return(
      list(
        "trips" = AtoB.trips, "avgtime" = AtoB.time / AtoB.trips
      )
    )
  } else {
    return(
      list(
        paste("Trips from", A,"to",B) = AtoB.trips,
        paste("AvgTime from", A,"to",B) = AtoB.time / AtoB.trips,
        paste("Trips from", B,"to",A) = BtoA.trips,
        paste("AvgTime from", B,"to",A) = BtoA.time / BtoA.trips,
        "Combined Trips" = AtoB.trips + BtoA.trips,
        "Combined avgTime" = (AtoB.time + BtoA.time) / (AtoB.trips + BtoA.trips)
      )
    )
  }

}