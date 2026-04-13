############# Loading necessary packages #########
library(lme4)
library(lmerTest)
library(readxl)
library(glmmTMB)
library(MuMIn)
library(ggplot2)
library(ggeffects)
library(car)
library(emmeans)
library(tidyverse)
library(Hmisc)
library(performance)
library(DHARMa)
library(sf)
library(boot)
library(dplyr)

###############data import #################
CompR_met <- read_excel("Emink_HR.xlsx")
CompR_met$Individual<-as.factor(CompR_met$Individual)
CompR_met$Individual_year<-as.factor(CompR_met$Individual_year)
CompR_met$Sex<-as.factor(CompR_met$Sex)
CompR_met$Method <- factor(CompR_met$Method)
CompR_met$Landscape_type<-as.factor(CompR_met$Landscape_type)
CompR_met$Period<-as.factor(CompR_met$Period)
summary(CompR_met)

options(na.action = "na.fail") #Essential for the dredge

############HR0W : proportion of home range outside the wetland###########
# Generalized Linear Mixed Model (GLMM) with beta distribution 
modhr0w <- glmmTMB(HR0W ~ Method*Landscape_type + (1 | Individual_year), family = beta_family(link = "logit"), data = CompR_met)
#Model selection
results <- dredge(modhr0w)
print(results)
#Model selection supported an additive model including both estimation method and landscape type as explanatory variables
newmodhr0w <- glmmTMB(HR0W ~ Method + Landscape_type + (1 | Individual_year), family = beta_family(link = "logit"), data = CompR_met)
summary(newmodhr0w)
Anova(newmodhr0w, type = "II") #Method and Landscape type have both a significant effect on HR0W.
#Pairwise comparison of methods
emm1 <- emmeans(newmodhr0w, ~ Method)
pairs(emm1) #Significant differences occur between EHR and Kernel, GAM and Kernel, GAM and LoCoH
#Pairwise comparison of landscapes
emm2 <- emmeans(newmodhr0w, ~ Landscape_type)
pairs(emm2) #There is a significant difference between Marsh and River

############W0HR : proportion of wetland not covered by home range################
#GLMM with beta distribution 
modw0hr <- glmmTMB(W0HR ~ Method*Landscape_type + (1 | Individual_year), family = beta_family(link = "logit"), data = CompR_met)
#Model selection
results <- dredge(modw0hr)
print(results)
# Model selection supported the previous model, we get back to the summary
summary(modw0hr)
Anova(modw0hr, type = "II") #The methods don't have the same effect depending the landscape type. 
emm1 <- emmeans(modw0hr, ~ Method | Landscape_type)
pairs(emm1) #In marsh there is a significant difference only between Kernel and LoCoH. In river, all values are significantly different, except between GAM and Kernel.

#############WCI : wetland conformity index############################
# Generalized Linear Mixed Model (GLMM) with gamma distribution 
modwci <- glmmTMB(WCI ~ Method * Landscape_type + (1 | Individual_year), family = Gamma(link = "log"), data = CompR_met)
#Model selection
results <- dredge(modwci)
print(results) #Two models are supported: additive model including method and landscape type, additive model+interaction
avg_mod <- model.avg(results, subset = delta < 2)
summary(avg_mod)
#Conditional average show interaction not significant so support an additive model including both estimation method and landscape type as explanatory variables.
newmodwci <- glmmTMB(WCI ~ Method + Landscape_type + (1 | Individual_year),family = Gamma(link = "log"), data = CompR_met)
summary(newmodwci)
Anova(newmodwci, type = "II") #Method and Landscape type have both a significant effect on HR0W.
#Pairwise comparison of method
emm1 <- emmeans(newmodwci, ~ Method)
pairs(emm1) #Significant differences occur between EHR and Kernel, GAM and Kernel, Kernel and LoCoH
#Pairwise comparison of landscapes
emm2 <- emmeans(newmodwci, ~ Landscape_type)
pairs(emm2) #There is a significant difference between Marsh and River

#############Home range and core area size using the GAM method############################
#######Creation of dataset
gam_data <- CompR_met %>%
  filter(Method == "GAM")
summary(gam_data)

#######Home range
# Linear Mixed-Effects Model (LMM) 
modgamsurf <- lmer(Area_95_ha ~ Landscape_type + Sex + Period + Landscape_type:Sex + Period:Sex + (1 | Individual), data = gam_data, REML = FALSE)
summary(modgamsurf)
#Model selection
results <- dredge(modgamsurf)
print(results) #Three models are supported: additive model including sex and landscape type, a model with simple effect of sex and the null model
avg_mod <- model.avg(results, subset = delta < 2)
summary(avg_mod)
#Conditional average show Landscape type and sex are significant so support an additive model including both estimation method and landscape type as explanatory variables.
newmodgamsurf <- lmer(Area_95_ha ~ Landscape_type + Sex + (1 | Individual), data = gam_data, REML = FALSE)
summary(newmodgamsurf)
Anova(newmodgamsurf, type = "II") #Sex and Landscape type have both a significant effect on HR0W.
#Pairwise comparison of sex
emm1 <- emmeans(newmodgamsurf, ~ Sex)
pairs(emm1) #Significant differences occur between EHR and Kernel, GAM and Kernel, Kernel and LoCoH
#Pairwise comparison of landscapes
emm2 <- emmeans(newmodgamsurf, ~ Landscape_type)
pairs(emm2) #There is a significant difference between Marsh and River

#######Core area
# Linear Mixed-Effects Model (LMM) 
modgamsurf50 <- lm(Area_50_ha ~ Landscape_type + Sex + Period + Landscape_type:Sex + Period:Sex, data = gam_data)
summary(modgamsurf50)
#Model selection
results <- dredge(modgamsurf50)
print(results) #Three models are supported: null model, model with simple effect of Sex and model with simple effect of Period. 
avg_mod <- model.avg(results, subset = delta < 2)
summary(avg_mod)
#Conditional average show no significant variables so core area size is not explained by our variables. 