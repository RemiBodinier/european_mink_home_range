# Charger les packages nécessaires
library(lme4)
library(lmerTest)
library(readxl)
library(glmmTMB)
library(MuMIn)

###############data import#################
CompR_met <- read_excel("Data/Dataset_Emink_HR.xlsx")
CompR_met
summary(CompR_met)
CompR_met$Individual<-as.factor(CompR_met$Individual)
CompR_met$Sex<-as.factor(CompR_met$Sex)
CompR_met$Method <- as.factor(CompR_met$Method)
CompR_met$Landscape_type<-as.factor(CompR_met$Landscape_type)
CompR_met$Period<-as.factor(CompR_met$Period)
summary(CompR_met)

############HR0W : proportion of home range outside the wetland###########
# Generalized Linear Mixed Model (GLMM) with beta distribution 
modhr0w <- glmmTMB(HR0W ~ Method*Landscape_type + (1 | Individual), family = beta_family(link = "logit"), data = CompR_met)
summary(modhr0w)

#Model selection
options(na.action = "na.fail")
results <- dredge(modhr0w)
print(results)
#Model selection supported an additive model including both estimation method and landscape type as explanatory variables

modpropdvadd <- glmmTMB(HR0W ~ Method + Landscape_type + (1 | Individual), family = beta_family(link = "logit"), data = CompR_met)
summary(modpropdvadd)

############W0HR : proportion of wetland not cover by home range################
#GLMM with beta distribution 
modw0hr <- glmmTMB(W0HR ~ Method*Landscape_type + (1 | Individual), family = beta_family(link = "logit"), data = CompR_met)
summary(modw0hr)

#Model selection
resultats <- dredge(modw0hr)
print(resultats)
# Model selection supported the previous model, we get back to the summary
summary(modw0hr)

#############WCI : wetland conformity index############################
# linear mixed-effects model (LMM) 
modwci <- lmer(WCI ~ Method * Landscape_type + (1|Individual) , data = CompR_met, REML = FALSE)

#Model selection
resultats <- dredge(modwci)
print(resultats)
#Model selection supported the previous model, we get back to the summary.
summary(modwci)
