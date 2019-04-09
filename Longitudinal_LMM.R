#### Longitudinal Linear Mixed Model ####

#### libraries ####
library(lme4)
library(lmerTest)
library(car)
library(carData)
library(MASS)

#### Load in data ###
data <- read.delim("Data/Data_Chickens.txt", sep = "")


#### Set up ####
summary(data)
str(data)
data$Group <- as.factor(data$Group)
data$Time <- as.factor(data$Time)
data$Department <- as.factor(data$Department)
data$Pen <- as.factor(data$Pen)
data$ID <-  as.factor(data$ID)
summary(data)

##### Set up analysis ####
# 1) Start with comparing different fixed effect parts 
#           - REML = F
#           - For each model check the assumptions (norm residuals, cooks distance, QQplot)
#           - Only add the random intercept
#           - compare models with the LRT/logLik -> models are nested
# 
# 2) Start adding more random effects and compare
#           - Add random slope and random pens
#           - REML = T
#           - Check assumptions (norm residuals, cooks distance, QQplot)
#           - Add different correlation structures
#           - Compare models with AIC/BIC

#### Comparing fixed effects ####
#### Minimal model ####
# Start with only Time and Group as fixed effects. Make sure both are
lmm_min <- lmer(Weight_change ~ Time + Group + (1| ID), REML = F, data = data)
summary(lmm_min)

#### Intermediate models ####
lmm_int <- lmer(Weight_change ~ Time + Group + Department + (1| ID), REML = F, 
                data = data)
summary(lmm_int)

lmm_int2 <- lmer(Weight_change ~ Time + Group + Time:Group + (1| ID), REML = F, 
                data = data)


# compare models
anova(lmm_min, lmm_int)
anova(lmm_min, lmm_int2)

#### Maximum model ####
lmm_max <- lmer(Weight_change ~ Time + Group + Department + Time:Group + (1| ID), 
                REML = F, data = data)
summary(lmm_max)

# Compare models
# The maximum model has a better fit -> loglik is higher (from -5190 to -5156)
# Although change is very small, doesn't make big difference?
anova(lmm_int, lmm_max)
anova(lmm_int2, lmm_max)



#### Assess the assumptions ####
# Each model needs to be assessed for:
#     1. Homogeneity of variance
#     2. Normal distribution of residuals
#     3. Infuence of outliers


# Homogeneity of variance check
std_resid = resid(lmm_max)/ sqrt(var(resid(lmm_max)))
plot(std_resid)



# Check normal distribution
qqPlot(std_resid)

# Check influence of outliers
infl1 <- influence(lmm_max,obs = T)
cooksd <-  cooks.distance(infl1) 
plot(cooksd, pch="*", cex=2) + 
  abline(h = 4*mean(cooksd, na.rm=T), col="red") +
  text(x = 1:length(cooksd) , y=cooksd, 
       labels=ifelse(cooksd>4*mean(cooksd), 1:length(cooksd),""), 
       col="blue", pos = 2)

# Have closer look at the most influencial points
data[c(24,529),]




#### Adding random effects ####

