PopData <- read.csv(file.choose(), header = TRUE)             # reads in PopData from Population.R
RunSubset <- read.csv(file.choose(), header = TRUE)           # reads in RunSubset from Finding5YearsOnly.R
# Ymatrix <- subset(PopData, select = -c(Enrollment.Year:time))  
Ymatrix <- subset(PopData, select = c(Age,CS,R,WEL,INSLT))     # only uses attribute columns Age, CS, R, WEL
# used later for creating quantiles
m = 44400    # the number of examples (rows) in SortByTime_ThreeQuantiles.R
n = 16            # the number of attributes (columns)
RSV_X <- matrix(0,nrow=m,ncol=n)   # creates a matrix of zeros that is the size of mxn
colnames(RSV_X) <- c("Age_25","CS1_25","R1_25","WEL1_25","INSLT1_25","Age_50","CS1_50","R1_50","WEL1_50","INSLT1_50","Age_75",
                     "CS1_75","R1_75","WEL1_75","INSLT1_75", "WEL_TreeSplit")

quantileAge <- quantile(Ymatrix$Age,probs = seq(0,1,0.25))   # Age quantile that separates into 25,50,75 percentiles
quantileCS <- quantile(Ymatrix$CS, probs = seq(0,1,0.25))    # CS ""
quantileR <- quantile(Ymatrix$R, probs = seq(0,1,0.25))     # R ""
quantileWEL <- quantile(Ymatrix$WEL, probs = seq(0,1,0.25))  # WEL ""
quantileINSLT <- quantile(Ymatrix$INSLT, probs = seq(0,1,0.25))  # INSLT ""

quantileRSV <- cbind(quantileAge, quantileCS, quantileR, quantileWEL,quantileINSLT)  # binds the quantiles into one matrix
quantileRSV25 <- t(quantileRSV[2,]) # only takes the 25% row
quantileRSV50 <- t(quantileRSV[3,]) # only takes the 50% row
quantileRSV75 <- t(quantileRSV[4,]) # only takes the 75% row
# quantileRSV100 <- t(quantileRSV[5,])

SortByTime <- read.csv(file.choose(), header = TRUE)   
Sorted <- subset(SortByTime, select = c(Age,CS1,R1,WEL1,INSLT1))   # takes the attributes from SortByTime (from SortingWithTime5Years.R)
# attributes from all 5 years

for(i in 1:m){                                             # goes through each row
  if(Sorted[i,"Age"] <= quantileRSV25[1,"quantileAge"]){     # makes comparisons of each attribute to the quantile and assigns Booleans
    RSV_X[i,"Age_25"] <- 0
  } else {
    RSV_X[i,"Age_25"] <- 1
  }
  if(Sorted[i,"CS1"] <= quantileRSV25[1,"quantileCS"]){
    RSV_X[i,"CS1_25"] <- 0
  } else {
    RSV_X[i,"CS1_25"] <- 1
  }
  if(Sorted[i,"R1"] <= quantileRSV25[1,"quantileR"]){
    RSV_X[i,"R1_25"] <- 0
  } else {
    RSV_X[i,"R1_25"] <- 1
  }
  if(Sorted[i,"WEL1"] <= quantileRSV25[1,"quantileWEL"]){
    RSV_X[i,"WEL1_25"] <- 0
  } else {
    RSV_X[i,"WEL1_25"] <- 1
  }
  if(Sorted[i,"INSLT1"] <= quantileRSV25[1,"quantileINSLT"]){
    RSV_X[i,"INSLT1_25"] <- 0
  } else {
    RSV_X[i,"INSLT1_25"] <- 1
  }
}
for(i in 1:m){
  if(Sorted[i,"Age"] <= quantileRSV50[1,"quantileAge"]){     # makes comparisons of each attribute to the quantile and assigns Booleans
    RSV_X[i,"Age_50"] <- 0
  } else {
    RSV_X[i,"Age_50"] <- 1
  }
  if(Sorted[i,"CS1"] <= quantileRSV50[1,"quantileCS"]){
    RSV_X[i,"CS1_50"] <- 0
  } else {
    RSV_X[i,"CS1_50"] <- 1
  }
  if(Sorted[i,"R1"] <= quantileRSV50[1,"quantileR"]){
    RSV_X[i,"R1_50"] <- 0
  } else {
    RSV_X[i,"R1_50"] <- 1
  }
  if(Sorted[i,"WEL1"] <= quantileRSV50[1,"quantileWEL"]){
    RSV_X[i,"WEL1_50"] <- 0
  } else {
    RSV_X[i,"WEL1_50"] <- 1
  }
  if(Sorted[i,"INSLT1"] <= quantileRSV50[1,"quantileINSLT"]){
    RSV_X[i,"INSLT1_50"] <- 0
  } else {
    RSV_X[i,"INSLT1_50"] <- 1
  }
}
for(i in 1:m){
  if(Sorted[i,"Age"] <= quantileRSV75[1,"quantileAge"]){     # makes comparisons of each attribute to the quantile and assigns Booleans
    RSV_X[i,"Age_75"] <- 0
  } else {
    RSV_X[i,"Age_75"] <- 1
  }
  if(Sorted[i,"CS1"] <= quantileRSV75[1,"quantileCS"]){
    RSV_X[i,"CS1_75"] <- 0
  } else {
    RSV_X[i,"CS1_75"] <- 1
  }
  if(Sorted[i,"R1"] <= quantileRSV75[1,"quantileR"]){
    RSV_X[i,"R1_75"] <- 0
  } else {
    RSV_X[i,"R1_75"] <- 1
  }
  if(Sorted[i,"WEL1"] <= quantileRSV75[1,"quantileWEL"]){
    RSV_X[i,"WEL1_75"] <- 0
  } else {
    RSV_X[i,"WEL1_75"] <- 1
  }
  if(Sorted[i,"INSLT1"] <= quantileRSV75[1,"quantileINSLT"]){
    RSV_X[i,"INSLT1_75"] <- 0
  } else {
    RSV_X[i,"INSLT1_75"] <- 1
  }
  if(Sorted[i,"WEL1"] <= 1.996555){
    RSV_X[i,"WEL_TreeSplit"] <- 0
  } else {
    RSV_X[i,"WEL_TreeSplit"] <- 1
  }
}

sid <- subset(SortByTime, select = student.ID)      # taking student.id from SortByTime.csv to put into RSV_X matrix
gender <- subset(SortByTime, select = Gender)
RSV_X <- cbind(sid,gender,RSV_X)                           # creates RSV_X with student.id
# write.csv(RSV_X, file="C:/Users/Carley/Documents/csvFiles/RSV_X_ThreeQuantiles_FiveAttributes.csv", row.names=FALSE)

WELfinal <- subset(RunSubset, time == 45, select = c(WEL))   # takes WEL for each student's final assessment
Sorted_reals <- subset(SortByTime, select = c(CS2, R2, WEL2, INSLT2))
attributes <- cbind(RSV_X, Sorted, Sorted_reals, WELfinal)                 # puts it in the attributes matrix, since it wasn't included earlier

attributes$WEL1 <- log(attributes$WEL1)
attributes$WEL2 <- log(attributes$WEL2)

attributes$Num <- c(rep(c(1,2,3,4),11100))
attributes <- subset(attributes, Num==1 | Num==2)

attributes2 <- subset(attributes, Num==2)$WEL
attributes1 <- subset(attributes, Num==1)$WEL
Difference <- log(attributes2) - log(attributes1)

Difference <- as.data.frame(Difference)

write.csv(Difference, file="difference_ThreeQuantiles_New9RealAttributes_tx13_ExtraTreeSplit.csv", row.names=FALSE)
write.csv(attributes, file="attributes_ThreeQuantiles_New9RealAttributes_tx13_ExtraTreeSplit.csv", row.names=FALSE)