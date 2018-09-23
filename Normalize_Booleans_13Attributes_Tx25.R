boolean_attributes <- read.csv(file.choose(), header = FALSE)
boolean_attributes_yz <- boolean_attributes[, 17:30]
boolean_attributes_x <- boolean_attributes[, 1:16]
for (i in 1:14) { 
  boolean_attributes_yz[,i] <- (boolean_attributes_yz[,i] - min(boolean_attributes_yz[,i]))/(max(boolean_attributes_yz[,i])- min(boolean_attributes_yz[,i]))
}
boolean_attributes_new <- cbind(boolean_attributes_x, boolean_attributes_yz)

write.csv(boolean_attributes_new, file="normalized_New13Attributes_tx25.csv", row.names=FALSE)




## Normalize one column at a time! Or just divide all wellness values by 2.