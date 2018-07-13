# Mainly to make fun plots.

library(readr)
library(dplyr)
library(ggplot2)
library(data.table)

# source("bytesExtractor.R")
# source("functions.R")

# fname <- "./data-plus-2018_wireless-ap-logs_20180419.csv"
# df0419 <- cleanSplunk(fname)
# bytesData <- read_csv("./bytesData0419.csv")
# mergeDF <- mergeBytesSplunk(bytesData, df0419)

dfssid <- df0419 %>% # seeing if ssid events change by campus
  group_by(ssid, campus) %>% 
  summarise(n = n())
dfVO <- df0419 %>% # seeing if ssid events change by locations off campus
  filter(campus == "Off") %>% 
  group_by(ssid, location.y) %>%
  summarise(n = n())
dfssidsum <- df0419 %>% # seeing what percentage of ssids are na
  group_by(ssid) %>% 
  summarise(n = n())

mainSSID <- c("Dukeblue", "DukeVisitor", "DukeOpen", "eduroam")

ggplot(data = dfssid %>% 
         filter(ssid %in% mainSSID), aes(x=ssid,y=n)) + 
  geom_col(aes(fill = campus)) +
  labs(title = "0419 ssid(truncat) usage by campus") + 
  facet_grid(.~campus)+
  theme_bw()
ggplot(data = dfssid, aes(x=ssid,y=n)) + 
  geom_col(aes(fill = campus)) +
  labs(title = "0419 ssid usage by campus") + 
  facet_grid(.~campus)+
  theme_bw()
ggplot(data = dfVO %>% 
         filter(ssid %in% mainSSID), aes(x=ssid, y= n)) + 
  geom_col(aes(fill = location.y)) +
  facet_grid(.~location.y) + 
  labs(title = "0419 ssid usage by offcampus locations")+
  theme_bw()
ggplot(data = dfssidsum[order(dfssidsum$n, decreasing = TRUE), ][1:5,], aes(x="", y= n, fill = ssid)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start= 0) + 
  labs(title = "0419 percentage of trun ssid events") +
  theme_bw()

dfOrgs <- df0419 %>% 
  group_by(macaddr, org) %>% 
  summatise(n=n())
dfOrgs <- dfOrgs %>%
  group_by(org) %>% 
  summarise(n = n())
dfOrgs <- dfOrgs[order(df0419Orgs$n, decreasing = TRUE), ] # Most common manufacturers.
dforgslot <- df0419 %>% # which manufacturers can only view which slots ?
  group_by(org, slotnum) %>% 
  summarise(n = n()) 
dfapslots <- df0419 %>% # are there aps that are lacking in certain slots?
  group_by(ap, slotnum) %>% 
  summarise(n = n()) 
dfslots <- df0419 %>% # most common slotnums
  group_by(slotnum) %>% 
  summarise(n = n())

dfapslotsno2 <- dfapslots[dfapslots$slotnum != 2, ] # filtering out slotnum 2 because it's much rarer
dfapslotsno2 <- merge.data.frame(dfapslotsno2[dfapslotsno2$slotnum == 0, ], 
                                 dfapslotsno2[dfapslotsno2$slotnum == 1, ], 
                                 by = "ap")
dfapslotsno2 <- dfapslotsno2 %>% # view the ratio of slot 0 / slot 1 events
  mutate(ratio = n.x / n.y)
mean(dfapslotsno2$ratio) # mean = 1.531007
ggplot(data = dfapslotsno2, aes(x=n.x, y= n.y)) + 
  geom_point() + 
  labs(x="num of slot 0 events", y= "num of slot 1 events")

dfnaap <- df0419 %>% # want to see if certain aps generate nas more often than others
  group_by(ssid, ap) %>% 
  summarise(n = n())

dfap <- df0419 %>% # seeing top events by location by ap
  group_by(ap, location.y, campus) %>%
  summarise(n = n())
dfaptrun <- dfap %>%
  group_by(location.y) %>%
  top_n(n=5, wt = n)
locs <- dfaptrun %>% 
  group_by(location.y) %>% 
  summarise(n = n())
locs <- locs[locs$n == 5, ]
dfaptrun <- dfaptrun[dfaptrun$location.y %in% locs$location.y, ] # getting rid of locations that don't have at least 5 aps
dfaptrun <- dfaptrun[order(dfaptrun$n,dfaptrun$location.y),]

ggplot(dfaptrun, aes(x=ap,y=n)) + 
  geom_col(aes(fill=campus)) + 
  labs(title = "top events by location by ap") +
  theme_bw() + 
  coord_flip()


laptops <- c("Intel Corporate")

dflaptops <- df0419 %>% # examining laptops closely by calculating time diff between aps
  filter(org %in% laptops)
dflaptops <- dflaptops[order(dflaptops$`_time`), ]
dflaptops$timeEnd <- lead(dflaptops$`_time`, 1)
dflaptops <- dflaptops %>% 
  mutate(timeDiff = as.numeric(difftime(dflaptops$timeEnd, dflaptops$`_time`, units = "secs")))
col <- rleid(dflaptops$ap)
dflaptops$id <- col
timeStayed <- aggregate(timeDiff ~ id, data = dflaptops, sum)

 
# # merging _time and time columns
# mergeDF$time[which(is.na(mergeDF$time))] <- mergeDF$`_time`[which(is.na(mergeDF$time))]
# mergeDF <- mergeDF[order(mergeDF$`_time`), ]
# 
# delete <- mergeDF
# delete <- delete %>% 
#   rename(realTime = `_time`, location = location.y)
# deleteHowLong <- NULL
# macs <- unique(delete$macaddr)
# 
# lapply(macs, function(mac) {
#   oneMac <- howLong(mac, delete)
#   deleteHowLong <<- rbind(deleteHowLong, oneMac)
# })
# 
# delete$id <- rleid(delete$location)
# delete <- delete %>% # summing up the number of bytes for each location chunk
#   group_by(id) %>% 
#   mutate(tot = sum(as.numeric(TotBytes)))
# delete <- delete[!duplicated(delete$id),] # condensing delete by id
# delete2 <- merge.data.frame(delete, deleteHowLong, by.x = c("realTime","macaddr"), by.y = c("startTimeReal","macaddr"))
# 
# # testing because when i plotted delete2 it did not look right
# deleteHowLongSub <- deleteHowLong %>% filter(macaddr %in% unique(deleteHowLong$macaddr))
# deleteSub <- temp %>% filter(macaddr %in% unique(deleteHowLong$macaddr))
# deleteHowLongSub <- deleteHowLongSub[order(deleteHowLongSub$macaddr), ]
# deleteSub <- deleteSub[order(deleteSub$macaddr), ]
# 
# # there's some negative difftimes which i still have yet to investigate
# # the negatives are caused by things not being ordered when being passed in
# deleteSub$id <- rleid(deleteSub$location.y)
# deleteSub <- deleteSub %>% # summing up the number of bytes for each location chunk
#   group_by(id) %>% 
#   mutate(tot = sum(as.numeric(TotBytes)))
# deleteSub <- deleteSub[!duplicated(deleteSub$id),] # condensing delete by id
# 
# delete2Sub <- merge.data.frame(deleteSub, deleteHowLongSub, by.x = c("macaddr", "_time"), by.y = c("macaddr", "startTimeReal"))
# 
# # 37 mb per sec is upload/download speed that i just tested my computer for
# ggplot(data = delete2Sub, aes(x=totalTime, y=tot)) + geom_point()
# 
# # this plot looks like a triangle bc say, if your start time is 0, then you can have 100 tot. but if start = 100, tot =0.
# ggplot(data = delete2Sub, aes(x=`_time`, y=totalTime)) + geom_point()
# 
# delete2Sub$condensedTotTime <- difftime(delete2Sub$endTimeReal,delete2Sub$`_time`, units="secs")
# delete2Sub$condensedTotTime <- as.numeric(delete2Sub$condensedTotTime)
# 
# speed <- (delete2Sub$tot / 1000000) / delete2Sub$totalTime # test if any is > 40 mb/s
# speeders <- cbind(delete2Sub, speed = speed)
# speeders <- speeders[which(speeders$totalTime != 0), ]
# speeders <- speeders[which(speeders$speed > 40), ]

