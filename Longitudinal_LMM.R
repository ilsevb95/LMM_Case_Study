#### Longitudinal Linear Mixed Model ####

#### libraries ####



#### Load in data ###
data <- read.delim("Data/Data_Chickens.txt", sep = "")



#### Set up ####
summary(data)
str(data)
data$Group <- as.factor(data$Group)
data$Time <- as.numeric(data$Time)
data$Department <- as.factor(data$Department)
data$Pen <- as.factor(data$Pen)
data$ID <-  as.factor(data$ID)
summary(data)
