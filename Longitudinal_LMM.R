#### Longitudinal Linear Mixed Model ####
rm(list = ls())

#### libraries ####
library(lme4)
library(lmerTest)
library(car)
library(carData)
library(MASS)
library(nlme)
library(tidyverse)
library(emmeans)

#### Load in data ###
data <- read.delim("Data/Data_Chickens.txt", sep = "")


#### Set up ####
summary(data)
str(data)
data$Group <- as.factor(data$Group)
#data$Time <- as.factor(data$Time)
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
lmm_min <- lme(Weight_change ~ Time + Group, 
               random = ~1|ID, method = "ML",data = data)
summary(lmm_min)

#### Intermediate models ####
# Add department
# Add interaction Time and groups
lmm_int <- lme(Weight_change ~ Time + Group + Department, 
               random = ~1|ID, method = "ML",data = data)
summary(lmm_int)

lmm_int2 <- lme(Weight_change ~ Time + Group + Time:Group, 
               random = ~1|ID, method = "ML",data = data)
summary(lmm_int2)


# compare models
anova(lmm_min, lmm_int)
anova(lmm_min, lmm_int2)

#### Maximum model ####
lmm_max <- lme(Weight_change ~ Time + Group + Department, 
                random = ~1|ID, method = "ML",data = data)
summary(lmm_max)

# Compare models
# The maximum model has a better fit -> loglik is higher (from -5190 to -5156)
# Although change is very small, doesn't make big difference?
anova(lmm_int, lmm_max)
anova(lmm_int2, lmm_max)

# We add the normalized residuals to the data
data <- data %>%
  mutate(Norm_resid_max = resid(lmm_max, type = "n"), 
         Norm_resid_int = resid(lmm_int, type = "n")) 




#### Assess the assumptions ####
# Each model needs to be assessed for:
#     1. Homogeneity of variance
#     2. Normal distribution of residuals
#     3. Infuence of outliers


# Homogeneity of variance check
pd <- position_dodge(0.8)

ggplot2::ggplot(data = data, aes(x = Time, y = Norm_resid_max)) + 
  geom_point(position = pd, aes(col = Department), size = 4) + 
  ylab("Normalized residuals") + 
  facet_grid(~Group)


# Check normal distribution
qqPlot(resid(lmm_max, type = "n"), ylab = "Normalized residuals") 
qqnorm(lmm_max, ~ resid(., type = "n"), abline = c(0, 1))


# Check influence of outliers
infl1 <- influence(lmm_max,obs = T)
cooksd <-  cooks.distance(infl1) 
plot(cooksd, pch="*", cex=2) + 
  abline(h = 4*mean(cooksd, na.rm=T), col="red") +
  text(x = 1:length(cooksd) , y=cooksd, 
       labels=ifelse(cooksd>4*mean(cooksd), 1:length(cooksd),""), 
       col="blue", pos = 2)

# Have closer look at the most influencial points
data[c(24,529),] # have very low Norm_resid




#### Adding random effects ####
### Random intercept only ####
lmm_max_r1 <- lme(Weight_change ~ Time + Group + Department + Time:Group, 
               random = ~1|ID, method = "REML",data = data)
summary(lmm_max_r1)



#### Random intercept and slope together ####
# Adding random intercept and slope together is not possible
# The model is too complex for the data -> no convergence
# Error: number of observations (=809) <= number of random effects (=810) for 
# term (Time | ID); the random-effects parameters and the residual variance 
# (or scale parameter) are probably unidentifiable
lmm_max_r2.1 <- nlme::lme(Weight_change ~ Time + Group + Department + Time:Group, 
                  random = ~1+Time|ID, method = "REML",data = data)

lmm_max_r2.2 <- lme4::lmer(Weight_change ~ Time + Group + Department + Time:Group + 
                             (1+Time|ID), data = data, REML = T)
summary(lmm_max_r2.2)


#### Random intercept and slope added independently ####
# Random intercept and slope -> they are added independently
# We then assume different levels of hierarchy. One level for measurements within ID
# Other level of dependence at the level of Pens
lmm_max_r2.3 <- lme(Weight_change ~ Time + Group + Department + Time:Group, 
                  random = list(ID = pdDiag(form = ~ 1),
                                Time = pdDiag(form = ~ 1)), 
                                method = "REML",data = data)

summary(lmm_max_r2.3)

#### Only random slope model ####
# No convergence either
lmm_max_r2.4 <- nlme::lme(Weight_change ~ Time + Group + Department + Time:Group, 
                          random = ~Time|ID, method = "REML",data = data)


#### Random intercept and Pens ####
# We then assume different levels of hierarchy. One level for measurements within ID
# Other level of dependence at the level of Pens
lmm_max_r2.5 <- lme(Weight_change ~ Time + Group + Department + Time:Group, 
                    random = list(ID = pdDiag(form = ~ 1),
                                  Pen = pdDiag(form = ~ 1)), 
                    method = "REML",data = data)
summary(lmm_max_r2.5)

# Compare different random effect structures
# The model with only a random intercept fits better
anova(lmm_max_r1, lmm_max_r2.3)
anova(lmm_max_r1, lmm_max_r2.5)



##### Different correlation structures ####
# We continue with the random intercept only model: lmm_max_r1

# First have a look at the covar-var matrix
getVarCov(lmm_max_r1, individuals = 1)


##### General: unstructured correlation ####
lmm_max_unstr <- gls(Weight_change ~ Time + Group + Department + Time:Group, 
                  method = "REML", 
                  correlation = corSymm(form = ~ 1 | ID),
                  data = data)

summary(lmm_max_unstr)


##### Compound symmetry correlation ####
lmm_max_cs <- gls(Weight_change ~ Time + Group + Department + Time:Group, 
                     method = "REML", 
                     correlation = corCompSymm(form = ~ 1 | ID),
                     data = data)

summary(lmm_max_cs)


##### ARMA correlation ####
lmm_max_arma <- gls(Weight_change ~ Time + Group + Department + Time:Group, 
                     method = "REML", 
                     correlation = corARMA(form = ~ 1 | ID, p = 1, q = 2),
                     data = data)

summary(lmm_max_arma)


##### AR1 correlation ####
lmm_max_ar <- gls(Weight_change ~ Time + Group + Department + Time:Group, 
                     method = "REML", 
                     correlation = corAR1(form = ~ 1 | ID),
                     data = data)

summary(lmm_max_ar)


# Compare different correlation structures
anova(lmm_max_cs, lmm_max_ar, lmm_max_unstr)
anova(lmm_max_unstr, lmm_max_r1)

LRT <- 2*(logLik(lmm_max_r1) - logLik(lmm_max_unstr))
pchisq(LRT, 9,lower.tail = T)




#### Adding weights ####
# Continue with the unstructed correlation model
lmm_max_unstr_weight <- gls(Weight_change ~ Time + Group + Department + Time:Group, 
                     method = "REML", 
                     weights = varIdent(~ 1|Timef),
                     correlation = corSymm(form = ~ 1 | ID),
                     data = data)

anova(lmm_max_unstr, lmm_max_unstr_weight)



#### EMMEANS ####
# The random intercept model fits best. Next we calculate the estimated marginal means
Model_final <- lmm_max_r1

# Construct reference grid
refgrid = ref_grid(Model_final)
df_emmeans = data.frame(summary(refgrid))
refgrid

# Compute Estimated Marginal Means over all timepoints for the different Groups
data_emmeans = as.data.frame(emmeans(refgrid, specs = c('Group')))

# Contrast is calculated between the two groups
data_contr = contrast(data_emmeans,method = 'pairwise')

#### Find model estimates per timepoint ####
data$Time <- as.factor(data$Time)
Model_final2 <- lme(Weight_change ~ Time + Group + Department + Time:Group, 
                  random = ~1|ID, method = "REML",data = data)


refgrid2 = ref_grid(Model_final2)
df_emmeans2 = data.frame(summary(refgrid2))
refgrid2

# Plot estimated model timepoints 
pd <- position_dodge(0.8)

df_emmeans2$Time <- as.numeric(as.character(df_emmeans2$Time))

ggplot2::ggplot(data = df_emmeans2, aes(x = Time, y = prediction)) + 
  geom_line(size = 2, aes(col = Department)) + geom_point(data = data, aes(x = Time, y = Weight_change)) + 
  ylab("Weight change") + 
  facet_grid(~Group)
