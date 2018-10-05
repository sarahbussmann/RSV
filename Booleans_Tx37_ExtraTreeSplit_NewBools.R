SortByTime <- read.csv(file.choose(), header = TRUE)   
PopData5Years <- read.csv(file.choose(), header = TRUE) 
newYmatrix <- subset(SortByTime, select = c(Age,CS4,R4,WEL4,INSLT4)) #PopDataFiveYears.csv

# used later for creating quantiles
m = 44400    # the number of examples (rows) in SortByTime_ThreeQuantiles.R
n = 16            # the number of attributes (columns)
RSV_X <- matrix(0,nrow=m,ncol=n)   # creates a matrix of zeros that is the size of mxn
colnames(RSV_X) <- c("Age_25","CS4_25","R4_25","WEL4_25","INSLT4_25","Age_50","CS4_50","R4_50","WEL4_50","INSLT4_50","Age_75",
                     "CS4_75","R4_75","WEL4_75","INSLT4_75", "WEL_TreeSplit")

quantileAge <- quantile(newYmatrix$Age,probs = seq(0,1,0.25))   # Age quantile that separates into 25,50,75 percentiles
quantileCS_4 <- quantile(newYmatrix$CS4, probs = seq(0,1,0.25))    # CS ""
quantileR_4 <- quantile(newYmatrix$R4, probs = seq(0,1,0.25))     # R ""
quantileWEL_4 <- quantile(newYmatrix$WEL4, probs = seq(0,1,0.25))  # WEL ""
quantileINSLT_4 <- quantile(newYmatrix$INSLT4, probs = seq(0,1,0.25))  # INSLT ""

quantileRSV <- cbind(quantileAge, quantileCS_4, quantileR_4,quantileWEL_4,quantileINSLT_4)  # binds the quantiles into one matrix
quantileRSV25 <- t(quantileRSV[2,]) # only takes the 25% row
quantileRSV50 <- t(quantileRSV[3,]) # only takes the 50% row
quantileRSV75 <- t(quantileRSV[4,]) # only takes the 75% row
# quantileRSV100 <- t(quantileRSV[5,])

Sorted <- subset(SortByTime, select = c(Age,CS1,R1,WEL1,INSLT1,CS2,R2,WEL2,INSLT2,CS3,R3,WEL3,INSLT3,CS4,R4,WEL4,INSLT4))   # takes the attributes from SortByTime (from SortingWithTime5Years.R)

for(i in 1:m){                                             # goes through each row
  if(Sorted[i,"Age"] <= quantileRSV25[1,"quantileAge"]){     # makes comparisons of each attribute to the quantile and assigns Booleans
    RSV_X[i,"Age_25"] <- 0
  } else {
    RSV_X[i,"Age_25"] <- 1
  }
  if(Sorted[i,"CS4"] <= quantileRSV25[1,"quantileCS_4"]){
    RSV_X[i,"CS4_25"] <- 0
  } else {
    RSV_X[i,"CS4_25"] <- 1
  }
  if(Sorted[i,"R4"] <= quantileRSV25[1,"quantileR_4"]){
    RSV_X[i,"R4_25"] <- 0
  } else {
    RSV_X[i,"R4_25"] <- 1
  }
  if(Sorted[i,"WEL4"] <= quantileRSV25[1,"quantileWEL_4"]){
    RSV_X[i,"WEL4_25"] <- 0
  } else {
    RSV_X[i,"WEL4_25"] <- 1
  }
  if(Sorted[i,"INSLT4"] <= quantileRSV25[1,"quantileINSLT_4"]){
    RSV_X[i,"INSLT4_25"] <- 0
  } else {
    RSV_X[i,"INSLT4_25"] <- 1
  }
}
for(i in 1:m){
  if(Sorted[i,"Age"] <= quantileRSV50[1,"quantileAge"]){     # makes comparisons of each attribute to the quantile and assigns Booleans
    RSV_X[i,"Age_50"] <- 0
  } else {
    RSV_X[i,"Age_50"] <- 1
  }
  if(Sorted[i,"CS4"] <= quantileRSV50[1,"quantileCS_4"]){
    RSV_X[i,"CS4_50"] <- 0
  } else {
    RSV_X[i,"CS4_50"] <- 1
  }
  if(Sorted[i,"R4"] <= quantileRSV50[1,"quantileR_4"]){
    RSV_X[i,"R4_50"] <- 0
  } else {
    RSV_X[i,"R4_50"] <- 1
  }
  if(Sorted[i,"WEL4"] <= quantileRSV50[1,"quantileWEL_4"]){
    RSV_X[i,"WEL4_50"] <- 0
  } else {
    RSV_X[i,"WEL4_50"] <- 1
  }
  if(Sorted[i,"INSLT4"] <= quantileRSV50[1,"quantileINSLT_4"]){
    RSV_X[i,"INSLT4_50"] <- 0
  } else {
    RSV_X[i,"INSLT4_50"] <- 1
  }
}
for(i in 1:m){
  if(Sorted[i,"Age"] <= quantileRSV75[1,"quantileAge"]){     # makes comparisons of each attribute to the quantile and assigns Booleans
    RSV_X[i,"Age_75"] <- 0
  } else {
    RSV_X[i,"Age_75"] <- 1
  }
  if(Sorted[i,"CS4"] <= quantileRSV75[1,"quantileCS_4"]){
    RSV_X[i,"CS4_75"] <- 0
  } else {
    RSV_X[i,"CS4_75"] <- 1
  }
  if(Sorted[i,"R4"] <= quantileRSV75[1,"quantileR_4"]){
    RSV_X[i,"R4_75"] <- 0
  } else {
    RSV_X[i,"R4_75"] <- 1
  }
  if(Sorted[i,"WEL4"] <= quantileRSV75[1,"quantileWEL_4"]){
    RSV_X[i,"WEL4_75"] <- 0
  } else {
    RSV_X[i,"WEL4_75"] <- 1
  }
  if(Sorted[i,"INSLT4"] <= quantileRSV75[1,"quantileINSLT_4"]){
    RSV_X[i,"INSLT4_75"] <- 0
  } else {
    RSV_X[i,"INSLT4_75"] <- 1
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

WELfinal <- subset(PopData5Years, time == 45, select = c(WEL))   # takes WEL for each student's final assessment
#Sorted_reals <- subset(SortByTime, select = c(CS2, R2, WEL2, INSLT2))
attributes <- cbind(RSV_X, Sorted, WELfinal)                 # puts it in the attributes matrix, since it wasn't included earlier

attributes$WEL1 <- log(attributes$WEL1)
attributes$WEL4 <- log(attributes$WEL4)

attributes$Num <- c(rep(c(1,2,3,4),11100))
attributes <- subset(attributes, Num==1 | Num==4)

attributes4 <- subset(attributes, Num==4)$WEL
attributes1 <- subset(attributes, Num==1)$WEL
Difference <- log(attributes4) - log(attributes1)

Difference <- as.data.frame(Difference)

write.csv(Difference, file="difference__tx37_ExtraTreeSplit_NewBools.csv", row.names=FALSE)
write.csv(attributes, file="attributes__tx37_ExtraTreeSplit_NewBools.csv", row.names=FALSE)