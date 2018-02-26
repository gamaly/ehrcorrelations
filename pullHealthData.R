library(googlesheets)
healthData <- gs_url("https://docs.google.com/spreadsheets/d/1eMz98RVYkRLEgwIygZfmTSckwZequpTkD2qos1c87zs/edit#gid=0")
gs_ws_ls(healthData)
allHealthData <- gs_read(ss=healthData, ws = "Tests and Medications", skip = 1)
allHealthData$Date <- as.Date(allHealthData$Date, "%m/%d/%Y")
save(allHealthData, file="allHealthData.RData")

