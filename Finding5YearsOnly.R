# PopData <- read.csv(file = "PopData.csv")
PopData <- read.csv(file.choose(), header = TRUE)   # reads in PopData from Population.R
PopDataRun <- table(PopData$Run)                    # reads the Run column from PopData, tells how many times
# each run.id appears (the frequency)
PopData_frame <- as.data.frame(PopDataRun)          # turns that into a data frame
FiveYears <- subset(PopData_frame, PopData_frame$Freq == 5)   # gets subset of run.id's that appears 4 times
RunSubset <- subset(PopData, PopData$Run %in% FiveYears$Var1) # then creates the subset of each run.id for 4 years
write.csv(RunSubset, file="PopDataFiveYears.csv", row.names=FALSE)
