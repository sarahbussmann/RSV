boolean_attributes <- read.csv(file.choose(), header = FALSE)
boolean_attributes_yz <- boolean_attributes[, 18:31]
boolean_attributes_x <- boolean_attributes[, 1:17]
for (i in 1:14) { 
  boolean_attributes_yz[,i] <- (boolean_attributes_yz[,i] - min(boolean_attributes_yz[,i]))/(max(boolean_attributes_yz[,i])- min(boolean_attributes_yz[,i]))
}
boolean_attributes_new <- cbind(boolean_attributes_x, boolean_attributes_yz)

write.csv(boolean_attributes_new, file="normalized_New13Attributes_tx25_ExtraTreeSplit.csv", row.names=FALSE)





