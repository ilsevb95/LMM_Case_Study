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
library(ggplot2)
library(lattice)
library(xtable)

data <- read.delim("Data/Data_Chickens.txt", sep = "")

###### Settings data plots #########
theme <-  theme(panel.background = 
                  element_rect(fill = "lightgrey", colour = "white", size = 4),
              axis.text=element_text(size=20),
              axis.title=element_text(size=22),
              plot.title = element_text(size = 22),
              strip.text = element_text(size = 20),
              legend.position="bottom",
              legend.text=element_text(size=20),
              legend.title=element_blank()) 

#### Set up ####
summary(data)
str(data)
data$Group <- as.factor(data$Group)
data$Time_factor <- as.factor(data$Time) # we want a new column with time as factor
data$Department_factor <- as.factor(data$Department) # same for department
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
# Start with only Time and Group as fixed effects.
# Fit response with and without a transformation 
# log transformation results in better fit -> amount of var explained by random intercept bigger
lmm_min_log <- lme(log(Weight_change) ~ Time + Group, 
               random = ~1|ID, method = "ML",data = data)
summary(lmm_min_log)

lmm_min <- lme(Weight_change ~ Time + Group, 
               random = ~1|ID, method = "ML",data = data)
summary(lmm_min)

#### Intermediate models ####
# Add department
# Add interaction Time and groups
lmm_int <- lme(log(Weight_change) ~ Time + Group + Department_factor, 
               random = ~1|ID, method = "ML",data = data)
summary(lmm_int)

lmm_int2 <- lme(log(Weight_change) ~ Time + Group + Time:Group, 
               random = ~1|ID, method = "ML",data = data)
summary(lmm_int2)


# compare models
anova(lmm_min, lmm_int)
anova(lmm_min, lmm_int2)

#### Maximum model ####
lmm_max <- lme(log(Weight_change) ~ Time + Group + Department_factor + Time:Group, 
                random = ~1|ID, method = "ML",data = data)
summary(lmm_max)

# Compare models
# The maximum model has a better fit -> loglik is higher (from -5190 to -5156)
# Although change is very small, doesn't make big difference?
anova(lmm_int, lmm_max)
anova(lmm_int2, lmm_max)

# We add the normalized residuals to the data
data <- data %>%
  mutate(Norm_resid_max = resid(lmm_max, type = "n"))




#### Assess the assumptions ####
# Each model needs to be assessed for:
#     1. Homogeneity of variance
#     2. Normal distribution of residuals
#     3. Infuence of outliers


# Homogeneity of variance check
pd <- position_dodge(0.8)

p1 <- ggplot2::ggplot(data = data, aes(x = Time, y = Norm_resid_max)) + 
  geom_point(position = pd, aes(col = Department_factor), size = 4) + 
  ylab("Normalized residuals") + theme + 
  facet_grid(~Group)

plot(p1)

# black, red, green, blue and light blue
par(mfrow = c(1,1))
plot(data$Norm_resid_max, col = data$Time_factor, 
             ylab = "Normalized residuals") 


# Check normal distribution
plot(qqnorm(lmm_max, ~ resid(., type = "n"), abline = c(0, 1)))


# Check influence of outliers
infl1 <- influence(lmm_max,obs = T)
cooksd <-  cooks.distance(infl1) 
plot(cooksd, pch="*", cex=2) + 
  abline(h = 4*mean(cooksd, na.rm=T), col="red") +
  text(x = 1:length(cooksd) , y=cooksd, 
       labels=ifelse(cooksd>4*mean(cooksd), 1:length(cooksd),""), 
       col="blue", pos = 2)

# Have closer look at the most influencial points
data[c(24,529, 525),] 




#### Adding random effects ####
### Random intercept only ####
# Note: method = REML
lmm_max_r1 <- lme(log(Weight_change) ~ Time + Group + Department_factor + Time:Group, 
                   random = ~1|ID, method= "REML",data = data)

summary(lmm_max_r1)
xtable(coef(summary(lmm_max_r1)))




#Visualize random intercept
rr1 <- ranef(lmm_max_r1, condVar = TRUE)
dotplot(rr1, scales = list(x = list(relation = 'free')))


# Have a look at the Var Covar matrix
getVarCov(lmm_max_r1, individual = 20, type = "conditional")
getVarCov(lmm_max_r1, individual = 20, type = "marginal")
getVarCov(lmm_max_r1) # Var explained by random intercept

getVarCov(lmm_max, individual = 20, type = "conditional")



#### Random intercept and slope together ####
# Adding random intercept and slope together is not possible
# The model is too complex for the data -> no convergence
# Error: number of observations (=809) <= number of random effects (=810) for 
# term (Time | ID); the random-effects parameters and the residual variance 
# (or scale parameter) are probably unidentifiable
lmm_max_r2.1 <- nlme::lme(Weight_change ~ Time + Group + Department_factor + Time:Group, 
                  random = ~1+Time|ID, method = "REML",data = data)

lmm_max_r2.2 <- lme4::lmer(log(Weight_change) ~ Time + Group + Department_factor + 
                             Time:Group + 
                             (1+Time|ID), data = data, REML = T)
summary(lmm_max_r2.2)

# Visualize random effects
plot(ranef(lmm_max_r1, condVar = T))
dotplot(ranef(lmm_max_r2.2, condVar = T))


#### Test 1 and 2 random effects: mixture of chi-squared ####
LRT <- 2*(logLik(lmm_max_r1) - logLik(lmm_max_r2.2))
0.5*pchisq(LRT, 1,lower.tail = F) + 0.5*pchisq(LRT, 2,lower.tail = F)


#### Random intercept and slope added independently ####
# Random intercept and slope -> they are added independently
# We then assume different levels of hierarchy. One level for measurements within ID
# Other level of dependence at the level of Pens
lmm_max_r2.3 <- lme(Weight_change ~ Time + Group + Department_factor + Time:Group, 
                  random = list(ID = pdDiag(form = ~ 1),
                                Time = pdDiag(form = ~ 1)), 
                                method = "REML",data = data)

summary(lmm_max_r2.3)

#### Only random slope model ####
# No convergence either
lmm_max_r2.4 <- nlme::lme(Weight_change ~ Time + Group + Department_factor + Time:Group, 
                          random = ~Time|ID, method = "REML",data = data)


#### Random intercept and Pens ####
# We then assume different levels of hierarchy. One level for measurements within ID
# Other level of dependence at the level of Pens
lmm_max_r2.5 <- lme(Weight_change ~ Time + Group + Department_factor + Time:Group, 
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
getVarCov(lmm_max_r1, individual = 20, type = "conditional") # random effect modl
getVarCov(lmm_max_r1, individual = 20, type = "marginal") # marginal model
getVarCov(lmm_max_r1, individual = 20) # explained var of random intercept


##### General: unstructured correlation #
# Doesn't work -> no convergence
lmm_max_unstr <- lme(log(Weight_change) ~ Time + Group + Department_factor + Time:Group, 
                  random = ~1|ID, 
                  correlation = corSymm(form = ~ 1 | ID),
                  method = "REML",data = data)

summary(lmm_max_unstr)



##### Compound symmetry correlation #
# same correlation and var over all timepoints
lmm_max_cs <- lme(log(Weight_change) ~ Time + Group + Department_factor + Time:Group, 
                     method = "REML", 
                     random = ~1|ID,
                     correlation = corCompSymm(form = ~ 1 | ID),
                     data = data)

summary(lmm_max_cs)


##### ARMA correlation #
lmm_max_arma <- lme(log(Weight_change) ~ Time + Group + Department_factor + Time:Group, 
                     method = "REML", 
                     random = ~1|ID,
                     correlation = corARMA(form = ~ 1 | ID, p = 4, q = 1),
                     data = data)

summary(lmm_max_arma)


##### AR1 correlation #
# Pairwse Correlations decrease with time -> most likely for a clinical trial
lmm_max_ar <- lme(log(Weight_change) ~ Time + Group + Department_factor + Time:Group, 
                     method = "REML", 
                     random = ~1|ID,
                     correlation = corAR1(form = ~ 1 | ID),
                     data = data)

summary(lmm_max_ar)


# Compare different correlation structures with the LRT
anova(lmm_max_cs, lmm_max_ar, lmm_max_arma, lmm_max_r1)
anova(lmm_max_r1, lmm_max_cs)
anova(lmm_max_r1, lmm_max_arma)
anova(lmm_max_r1, lmm_max_ar)
anova(lmm_max_arma, lmm_max_r1)

LRT <- 2*(logLik(lmm_max_arma) - logLik(lmm_max_r1))
pchisq(LRT, 3,lower.tail = T)


# Have look at var-covar matrix
getVarCov(lmm_max_arma, individuals = 20, type = "conditional")
getVarCov(lmm_max_r1, individuals = 20, type = "conditional")


#### Model diagnostics ####
# We add the normalized residuals to the data
data <- data %>%
  mutate(Norm_resid_final = resid(lmm_max_arma, type = "n"))



#### Assess the assumptions ####
# Each model needs to be assessed for:
#     1. Homogeneity of variance
#     2. Normal distribution of residuals
#     3. Infuence of outliers


# Homogeneity of variance check
pd <- position_dodge(0.8)

p2 <- ggplot2::ggplot(data = data, aes(x = Time, y = Norm_resid_final)) + 
  geom_point(position = pd, aes(col = Department_factor), size = 4) + 
  ylab("Normalized residuals") + theme + 
  facet_grid(~Group)

plot(p2)


#### Checking dependence ####
lmm_gls_cs <- gls(log(Weight_change) ~ Time + Group + Department_factor + 
                    Time:Group, 
                  method = "REML", 
                  correlation = corCompSymm(form = ~ 1 | ID),
                  data = data)

summary(lmm_gls_cs)

lmm_gls <- gls(log(Weight_change) ~ Time + Group + Department_factor + Time:Group, 
                  method = "REML", 
                  correlation = NULL,
                  data = data)

summary(lmm_gls)

# significant effect of the correlation structure
# Conclusion: there is dependence of data over time -> constant var
anova(lmm_gls, lmm_gls_cs)
getVarCov(lmm_gls_cs, type = "marginal")

#### Adding weights ####
# Continue with the unstructed correlation model
# 
lmm_max_r1_wght <- lme(Weight_change ~ Time + Group + Department_factor + Time:Group, 
                     method = "REML", 
                     random = ~1|ID,
                     weights = varIdent(~ 1|Time), # also not possible with ID
                     data = data)

summary(lmm_max_r1_wght)

# Adding weights doesn't improve the model
anova(lmm_max_r1, lmm_max_r1_wght)


#### EMMEANS ####
# The random intercept model fits best. Next we calculate the estimated marginal means
# We want to average over the departments -> needs to be numeric
Model_final <- lme(log(Weight_change) ~ Time + Group + Department + Time:Group, 
                  random = ~1|ID, 
                  correlation = corARMA(form = ~ 1 | ID, p = 4, q = 1),
                  method = "REML",data = data)
summary(Model_final)


# Construct reference grid
refgrid <-  ref_grid(Model_final)
refgrid
df_emmeans = data.frame(summary(refgrid))
data_emmeans <-  emmeans(refgrid, specs = c('Group', "Time"))
xtable(data_emmeans)

# Contrast and p-value is calculated between the two groups
contr <-  contrast(data_emmeans)



#### Find model estimates per timepoint ####
Model_final2 <- lme(log(Weight_change) ~ Time_factor + Group + Department + 
                      Time_factor:Group,
                    correlation = corARMA(form = ~ 1 | ID, p = 4, q = 1),
                  random = ~1|ID, method = "REML",data = data)

summary(Model_final2)

refgrid2 = ref_grid(Model_final2)
df_emmeans2 = data.frame(summary(refgrid2))
refgrid2


# Plot estimated model timepoints
pd <- position_dodge(0.2)

df_emmeans2$Group <- as.factor(df_emmeans2$Group)
df_emmeans2$Time <- as.numeric(as.character(df_emmeans2$Time))
df_emmeans2$SD <- df_emmeans2$SE * sqrt(162)

# Plot predictions with SD as errorbars
p3 <- ggplot2::ggplot(data = df_emmeans2, aes(x = Time, y = prediction)) + 
  geom_line(position = pd, aes(col = Group), size = 1) + 
  geom_point(position = pd, aes(shape = Group, col = Group), size = 4) + 
  geom_errorbar(position = pd, aes(ymin = prediction - SD, 
                                   ymax = prediction + SD, 
                                   x = Time, col = Group), size = 1, width= 2) +
  scale_color_manual(values = c('blue',  'orange')) + theme + 
  ylab("Weight change")

plot(p3)

#### Save result summary plots and tables ####
#### Save files and plots ####
write.table(df_summ, file = paste("Data/Summary_statistics_",Sys.Date(),".csv", 
                                  sep = ""), row.names=FALSE, na = "", 
            col.names=T, sep=",")



png('Plots/Residuals_arma.png', width = 15, height = 7, units = 'in', res = 600)
plot(p2)
dev.off()
#################################### End #######################################