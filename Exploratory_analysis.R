#### Library ####
library(tidyverse)
library(reshape2)
library(plyr)

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



### First exploratory plots ####
# Set up for plots
theme <- theme(axis.text=element_text(size=20),
              axis.title=element_text(size=22),
              plot.title = element_text(size = 22),
              strip.text = element_text(size = 20))

p1 <- ggplot2::ggplot(data = data, aes(x = Time, y = Weight_change, col = ID)) +
  geom_point(size = 4) + geom_line() + facet_grid(~ Group) + theme(legend.position="none") +
  xlab("Time (days)") + ylab("Weight change (gr)") + theme +
  xlim(c(0,35))

plot(p1)


# Frequency tables
# Group by Groups and Departments
df_freq <- data %>%
  count(Time, Group, Department) %>%
  group_by(Group, Department)

# Group by: Group
df_freq2 <- data %>%
  count(Time, Group) %>%
  group_by(Group)


#### Summarize data ####
df_summ = dplyr::select(data, ID, Weight_change, Group)
df_summ = melt(data = df_summ, id.vars=c("ID", "Group"))


df_summ = data.frame(ddply(df_summ, c("Group", "variable"), summarise,
                                 Mean = round(mean(value), digits = 2),
                                 SD = round(sd(value), digits = 2),
                                 SEM = round(sd(value)/sqrt(length(value)), digits = 2),
                                 LowerQuantile = round(quantile(value, probs = 0.025), 
                                                       digits=2),
                                 UpperQuantile = round(quantile(value, probs = 0.975), 
                                                       digits = 2),
                                 N = length(value)))


#### Save files and plots ####
write.table(df_summ, file = paste("Data/Summary_statistics_",Sys.Date(),".csv", 
                                  sep = ""), row.names=FALSE, na = "", 
            col.names=T, sep=",")

write.table(df_freq, file = paste("Data/Frequency_Group_Time_Departm_",Sys.Date(),".csv", 
                                  sep = ""), row.names=FALSE, na = "", 
            col.names=T, sep=",")

write.table(df_freq2, file = paste("Data/Frequencly_Group_Time_",Sys.Date(),".csv", 
                                  sep = ""), row.names=FALSE, na = "", 
            col.names=T, sep=",")


png('Plots/Weight_Time_profiles.png', width = 15, height = 7, units = 'in', res = 600)
plot(p1)
dev.off()