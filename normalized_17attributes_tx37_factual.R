normalized <- read.csv(file.choose(), header = FALSE)
normalized_xy <- as.data.frame(normalized[,1:33])
normalized_xy$Num <- c(rep(c(1,2),11100))
normalized_xy <- subset(normalized_xy, Num==1)
normalized_z <- read.csv(file.choose(), header = TRUE)
normalized_z <- normalized_z[1:11100,]
normalized_final <- cbind(normalized_xy,normalized_z)
normalized_final <- subset(normalized_final, select = -c(Num))
write.csv(normalized_final, file="normalized_17attributes_tx37_factual.csv", row.names=FALSE)
