#### Classical Linear Mixed Model ####

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


#### Polynomial model ####
plot(data$Time,data$Weight_change, xlim=c(0,40), ylim=c(1,2500))


fit1 <- lm(Weight_change ~poly(Time,1), data =  data )
fit2 <- lm(Weight_change ~poly(Time,2), data =  data )
fit3 <- lm(Weight_change ~poly(Time,3), data =  data )
fit4 <- lm(Weight_change ~poly(Time,4), data =  data )

xx <- seq(0,40, length.out=250)

lines(xx, predict(fit1, data.frame(Time=xx)), col='blue')
lines(xx, predict(fit2, data.frame(Time=xx)), col='green')
lines(xx, predict(fit3, data.frame(Time=xx)), col='red')
lines(xx, predict(fit4, data.frame(Time=xx)), col='orange')




######################## End ###################################################