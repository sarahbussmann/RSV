UseRunSubset <- read.csv(file.choose(), header = TRUE)
Time0 <- subset(UseRunSubset, time == 0, select = -c(time))
colnames(Time0)[colnames(Time0) == "CS"] <- "CS1"
colnames(Time0)[colnames(Time0) == "R"] <- "R1"
colnames(Time0)[colnames(Time0) == "WEL"] <- "WEL1"
colnames(Time0)[colnames(Time0) == "INSLT"] <- "INSLT1"

Time12 <- subset(UseRunSubset, time == 12, select = CS:INSLT)
colnames(Time12)[colnames(Time12) == "CS"] <- "CS2"
colnames(Time12)[colnames(Time12) == "R"] <- "R2"
colnames(Time12)[colnames(Time12) == "WEL"] <- "WEL2"
colnames(Time12)[colnames(Time12) == "INSLT"] <- "INSLT2"

Time24 <- subset(UseRunSubset, time == 24, select = CS:INSLT)
colnames(Time24)[colnames(Time24) == "CS"] <- "CS3"
colnames(Time24)[colnames(Time24) == "R"] <- "R3"
colnames(Time24)[colnames(Time24) == "WEL"] <- "WEL3"
colnames(Time24)[colnames(Time24) == "INSLT"] <- "INSLT3"

Time36 <- subset(UseRunSubset, time == 36, select = c(CS:INSLT))
colnames(Time36)[colnames(Time36) == "CS"] <- "CS4"
colnames(Time36)[colnames(Time36) == "R"] <- "R4"
colnames(Time36)[colnames(Time36) == "WEL"] <- "WEL4"
colnames(Time36)[colnames(Time36) == "INSLT"] <- "INSLT4" 

Time45 <- subset(UseRunSubset, time == 45, select = c(CS, R, INSLT))
colnames(Time45)[colnames(Time45) == "CS"] <- "CS5"
colnames(Time45)[colnames(Time45) == "R"] <- "R5"
colnames(Time45)[colnames(Time45) == "INSLT"] <- "INSLT5" 

SortByTime <- cbind.data.frame(Time0, Time12, Time24, Time36, Time45)
write.csv(SortByTime, file="SortedByTime5Years.csv", row.names=FALSE)