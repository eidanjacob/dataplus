require(readr)
require(dplyr)

dataRaw <- read_csv("./data-plus-2018_wireless-ap-logs_20180501.csv")

perkinsData <- subset(dataRaw, grepl("7704", dataRaw$ap))
perkinsData <- subset(perkinsData, !grepl("bostock", perkinsData$ap))

for(i in 18:30){
  dataRaw <- read_csv(paste0("./data-plus-2018_wireless-ap-logs_201804",
                             as.character(i),
                             ".csv"))
  p <- subset(dataRaw, grepl("7704", dataRaw$ap))
  p <- subset(p, !grepl("bostock", p$ap))
  perkinsData <- rbind(perkinsData, p)
}

write.csv(perkinsData, "./perkinsOnly.csv", row.names = F)