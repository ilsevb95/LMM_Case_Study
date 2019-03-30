#### Data preparation ####

#### Library ####
library(tidyverse)
library(reshape2)
library(plyr)

#### Load in data ###
data <- read.delim("Data/Data_Chickens.txt", sep = "")
colnames(data) <- c("Department", "Pen", "Group", "Animal_nr", "Time", 
                    "Weight_change")

#### Set up ####
summary(data)
str(data)
data$Group <- as.factor(data$Group)
data$Animal_nr <- as.factor(data$Animal_nr)
data$Time <- as.numeric(data$Time)
data$Department <- as.factor(data$Department)
data$Pen <- as.factor(data$Pen)
summary(data)

# Creating unique IDs
data <- data %>% 
  mutate(ID = group_indices_(data, .dots=list("Animal_nr", "Pen", "Department", 
                                              "Group"))) %>%
  select(-Animal_nr) %>%
  mutate(ID = as.factor(ID))


# save data
write.table(data, file = "Data/Data_chickens.txt", row.names = F)
############################# End ##############################################