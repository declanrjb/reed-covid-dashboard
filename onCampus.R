#Building dates on campus
cf <- gf
cf <- cbind(cf, startDate=NA)
cf <- cbind(cf, endDate=NA)
colnames(cf) <- c("Notification","Role","Dates","startDate","endDate")
cf <- cf %>% filter(Dates != "none")
cf$Dates <- gsub(", ","-",cf$Dates)

testDf <- as.data.frame(strsplit(cf$Dates,"-"))
transpose <- t(testDf)
transpose <- as.data.frame(transpose)
testDf <- rev(transpose)
colnames(testDf) <- c("endDate","startDate")
cf$startDate <- testDf$startDate
cf$endDate <- testDf$endDate
cf$startDate <- mdy(cf$startDate)
cf$endDate <- mdy(cf$endDate)

for (i in 1:length(cf$Dates)) {
  if (!is.na(cf$startDate[i]) & !is.na(cf$endDate[i]) & !(cf$startDate[i] %within% interval(ydm("2000-01-01"),cf$endDate[i]))) {
    start <- cf$endDate[i]
    end <- cf$startDate[i]
    cf$startDate[i] <- start
    cf$endDate[i] <- end
  }
}

countDf <- data.frame(matrix(ncol = 1, nrow = 0))
roleDf <- data.frame(matrix(ncol = 1, nrow = 0))
for (i in 1:length(cf$Dates)) {
  if (!is.na(cf$startDate[i]) & !is.na(cf$endDate[i])) {
    tempDf <- as.data.frame(seq(cf$startDate[i],cf$endDate[i], by = 'days'))
    colnames(tempDf) <- c("Date")
    
    tempRoleDf <- as.data.frame(matrix(ncol = 1, nrow = length(tempDf$Date)))
    colnames(tempRoleDf) <- c("Role")
    tempRoleDf$Role <- cf$Role[i]
    
    countDf <- rbind(countDf,tempDf)
    roleDf <- rbind(roleDf,tempRoleDf)
  }
}

countDf <- cbind(countDf,Role=roleDf$Role)

