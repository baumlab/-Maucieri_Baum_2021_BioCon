# Impacts of heat stress on soft corals, an overlooked and highly vulnerable component of coral reef ecosystems, at a central equatorial Pacific atoll

# Authors: Dominique G. Maucieri[1], and Julia K. Baum[1]
# Institution: [1] Department of Biology, University of Victoria, Victoria, British Columbia, V8P 5C2, Canada
# Corresponding Author: Julia K. Baum, Email: baum@uvic.ca

# Script to fit generalized linear mixed-effects models for soft coral cover prior to the heatwave

##############################

## load packages 
library(dplyr)
library(qpcR)
library(glmmTMB)
library(scales)
library(MuMIn)
library(xlsx)

## Set your working directory
# Make sure that this contains all the file folders
setwd("C:/Users/...") # If on a PC
setwd("/Users/...") # If on a Mac

load("data/raw_soft_coral.Rdata")

##############################

## check the effect of year

year_check <- raw_soft_coral %>% filter(TimeBlock == "before") %>% group_by(UniqueID) %>% 
  mutate(SoftCoral = sum(Sarcophyton, Cladiella, Sinularia, Lobophytum)) %>% ungroup() %>% 
  group_by(Site, Year) %>% summarise(meanSoftCoral = mean(SoftCoral) / 100)

year_check_glm <- glm(meanSoftCoral ~ Year, data = year_check, family = quasibinomial)

summary(year_check_glm) #no effect of year

##############################

## total soft coral cover models

cover_before <- raw_soft_coral %>% filter(TimeBlock == "before") %>% group_by(UniqueID) %>% 
  mutate(SoftCoral = sum(Sarcophyton, Cladiella, Sinularia, Lobophytum)) %>% ungroup() %>% 
  group_by(Site, HD_Cat, HD_Cont, Region, NPP, WE, WaveEnergy) %>% summarise(meanSoftCoral = mean(SoftCoral) / 100, meanLobo = mean(Lobophytum) / 100, meanSinu = mean(Sinularia) / 100, meanSarco = mean(Sarcophyton) / 100, meanClad = mean(Cladiella) / 100)

## standardizing continuous variables
cover_before$NPP_std <- rescale(cover_before$NPP)
cover_before$HD_Cont_std <- rescale(cover_before$HD_Cont)
cover_before$WaveEnergy_std <- rescale(cover_before$WaveEnergy)

## All models
cover_before$WE <- factor(cover_before$WE, levels = c("windward","sheltered"))

model1 <- glmmTMB(meanSoftCoral ~ HD_Cont_std , data = cover_before, ziformula = ~1, family = beta_family(link = "logit"))

model2 <- glmmTMB(meanSoftCoral ~ WE, data = cover_before, ziformula = ~1, family = beta_family(link = "logit"))

model3 <- glmmTMB(meanSoftCoral ~ NPP_std , data = cover_before, ziformula = ~1, family = beta_family(link = "logit"))

model4 <- glmmTMB(meanSoftCoral ~ HD_Cont_std + WE + NPP_std , data = cover_before, ziformula = ~1, family = beta_family(link = "logit"))

model5 <- glmmTMB(meanSoftCoral ~ HD_Cont_std + NPP_std , data = cover_before, ziformula = ~1, family = beta_family(link = "logit"))

model6 <- glmmTMB(meanSoftCoral ~ HD_Cont_std + WE , data = cover_before, ziformula = ~1, family = beta_family(link = "logit"))

model7 <- glmmTMB(meanSoftCoral ~ HD_Cont_std * WE, data = cover_before, ziformula = ~1, family = beta_family(link = "logit"))


## AIC model selection
AIC_table <- model.sel(model1, model2, model3, model4, model5, model6, model7)

## AIC summary table 
names(AIC_table) <- c("(Int)", "zi(Int)", "dsp(Int)", "Human HD_Cont", "Wind Side", "NPP", "HD_Cont_std * WindSide", "Family", "df", "LL", "AICc", "delta", "weight")
AIC_table$weight <- c(akaike.weights(AIC_table$AICc)$weights)

## Save as Table 1 & S2
write.xlsx(AIC_table, "tables_figures/Table_1_S2.xlsx", row.names = TRUE)

## Calling top three models
summary(model4)
summary(model7)
summary(model6)



##############################

## genera specific models
soft_coral_genera <- raw_soft_coral %>% dplyr::filter(TimeBlock == "before") %>% group_by(UniqueID, Lobophytum, Sinularia, Sarcophyton, Cladiella) %>% mutate(SoftCoral = sum(Lobophytum, Sinularia, Sarcophyton, Cladiella)) %>% ungroup() %>% summarise(totalLoph = mean(Lobophytum), seLoph = (sd(Lobophytum))/(sqrt(length(Lobophytum))), totalSinu = mean(Sinularia), seSinu = (sd(Sinularia))/(sqrt(length(Sinularia))), totalSarco = mean(Sarcophyton), seSarc = (sd(Sarcophyton))/(sqrt(length(Sarcophyton))), totalClad = mean(Cladiella), seClad = (sd(Cladiella))/(sqrt(length(Cladiella))), totalSoft = mean(SoftCoral), seSoftCoral = (sd(SoftCoral))/(sqrt(length(SoftCoral))))
## Cover order: Loph, Sinu, Sarco, Clad (greatest to least)


Lobo_Model <- glmmTMB(meanLobo ~ HD_Cont_std + WE + NPP_std , data = cover_before, ziformula = ~1, family = beta_family(link = "logit"))
summary(Lobo_Model)

Sinu_Model <- glmmTMB(meanSinu ~ HD_Cont_std + WE + NPP_std , data = cover_before, ziformula = ~1, family = beta_family(link = "logit"))
summary(Sinu_Model)

Sarco_Model <- glmmTMB(meanSarco ~ HD_Cont_std + WE + NPP_std , data = cover_before, ziformula = ~1, family = beta_family(link = "logit"))
summary(Sarco_Model)

Clad_Model <- glmmTMB(meanClad ~ HD_Cont_std + WE + NPP_std , data = cover_before, ziformula = ~1, family = beta_family(link = "logit"))
summary(Clad_Model)

##############################

## Wave Energy Models

model_WaveEnergy <- glmmTMB(meanSoftCoral ~ WaveEnergy_std , data = cover_before, ziformula = ~1, family = beta_family(link = "logit"))
summary(model_WaveEnergy)

