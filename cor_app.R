library(ggplot2)
library(reshape2)
library(dplyr)


Rheology_shear <- Rheology[, c(-2, -5)] %>%
  dcast(STABILITYSTUDYID + batchid + conditionset + TIMEPOINT ~ Shear,
        value.var = "Labvalue")

Rheology_shear <- Rheology_shear[!(is.na(Rheology_shear[5:10])),] 


# Rheology_shear <- dcast(STABILITYSTUDYID + batchid + conditionset)
  
Rheology_apperance <- merge(
  Rheology_shear, 
  Appearance_2[, c("STABILITYSTUDYID","batchid", "conditionset", "max_time","stabilityapp")], 
  by = c("STABILITYSTUDYID","batchid", "conditionset")
) %>%
  filter(stabilityapp %in% c("PASS", "FAIL") & conditionset == 25)

library(ggpubr)

p1 <- ggplot(Rheology_apperance[which(Rheology_apperance$TIMEPOINT %in% c(0, 1, 2, 4,8, 12)), ],
       aes(x = as.factor(TIMEPOINT), y=as.numeric(`0.05`),fill = stabilityapp)) +
  geom_boxplot() + 
  scale_y_log10()


p2 <- ggplot(Rheology_apperance[which(Rheology_apperance$TIMEPOINT %in% c(0, 1, 2, 4,8, 12)), ],
       aes(x = as.factor(TIMEPOINT), y=as.numeric(`0.2`),fill = stabilityapp)) +
  geom_boxplot() + 
  scale_y_log10()

p3 <- ggplot(Rheology_apperance[which(Rheology_apperance$TIMEPOINT %in% c(0, 1, 2, 4,8, 12)), ],
       aes(x = as.factor(TIMEPOINT), y=as.numeric(`0.5`),fill = stabilityapp)) +
  geom_boxplot() + 
  scale_y_log10()

p4 <- ggplot(Rheology_apperance[which(Rheology_apperance$TIMEPOINT %in% c(0, 1, 2, 4,8, 12)), ],
       aes(x = as.factor(TIMEPOINT), y=as.numeric(`1`),fill = stabilityapp)) +
  geom_boxplot() + 
  scale_y_log10()

p5 <- ggplot(Rheology_apperance[which(Rheology_apperance$TIMEPOINT %in% c(0, 1, 2, 4,8, 12)), ],
       aes(x = as.factor(TIMEPOINT), y=as.numeric(`20`),fill = stabilityapp)) +
  geom_boxplot() + 
  scale_y_log10()

p6 <- ggplot(Rheology_apperance[which(Rheology_apperance$TIMEPOINT %in% c(0, 1, 2, 4,8, 12)), ],
       aes(x = as.factor(TIMEPOINT), y=as.numeric(`100`),fill = stabilityapp)) +
  geom_boxplot() + 
  scale_y_log10()

ggarrange(p1, p2, p3, p4, p5, p6, labels = c(0.05, 0.2, 0.5, 1, 20, 100), common.legend = T) 

# merge with PH ----
Rhe_ph_app <- merge(Appearance_2,
                    PH[, c(0,1, 4, 6, 7, 11)],
                    by = c(colnames(Rheology_apperance)[1:3]))


ggplot(Rhe_ph_app[which(Rhe_ph_app$TIMEPOINT %in% c(0,1, 2, 4, 8)),], 
       aes(y = as.numeric(deltapH), x = as.factor(TIMEPOINT), color = stabilityapp)) +
  geom_boxplot() + 
  ylim(-1,0.5)
##
Rhe_ph_app <- merge(Appearance_2,
                    PH[, c(0,1, 4, 6, 7, 8)],
                    by = c(colnames(Rheology_apperance)[1:3]))


ggplot(Rhe_ph_app[which(Rhe_ph_app$TIMEPOINT %in% c(0,1, 2, 4, 8)),], 
       aes(y = as.numeric(Labvalue), x = as.factor(TIMEPOINT), color = stabilityapp)) +
  geom_boxplot() + ylim(0,12)

# merge with Viscosity_B
  
VB_app <- merge(
  Viscosity[, c(1, 4, 6, 7, 8 )],
  Appearance_2,
  by = colnames(Appearance_2)[1:3]
)

ggplot(VB_app[which(VB_app$TIMEPOINT %in% c(0,1, 2, 4, 8)),], 
       aes(x= as.factor(TIMEPOINT), y = as.numeric(zeroviscosity), color = stabilityapp)) +
  geom_boxplot() +
  scale_y_log10()

# merge with Arctic ----
Arcric_app <- merge(
  Arctic[, c(1, 4, 6, 7, 8)], 
  Appearance_2,
  by = colnames(Appearance_2)[1:3]
)
ggplot(Arcric_app[which(Arcric_app$TIMEPOINT %in% c(0,1, 2, 4, 8)),], 
       aes(x= as.factor(TIMEPOINT), y = as.numeric(Labvalue), color = stabilityapp)) +
  geom_boxplot() +
  scale_y_log10()

Arcric_app <- merge(
  Arctic[, c(1, 4, 6, 7, 10)], 
  Appearance_2,
  by = colnames(Appearance_2)[1:3]
)
ggplot(Arcric_app[which(Arcric_app$TIMEPOINT %in% c(0,1, 2, 4, 8, 12)),], 
       aes(x= as.factor(TIMEPOINT), y = as.numeric(activitypct), color = stabilityapp)) +
  geom_boxplot()

# merge with enzyme ----
enzyme <- rbind(Mannanase, Preferenz_P280, Preferenz_P283,
                Vitalenz_P290, Xyloglucanase)

enzyme_app <- merge(
  enzyme[, c(1, 4, 6, 7, 8, 9, 10)],
  Appearance_2,
  by = colnames(Appearance_2)[1:3]
)

ggplot(enzyme_app[which(enzyme_app$TIMEPOINT %in% c(0,1, 2, 4, 8, 12)),], 
       aes(x= as.factor(TIMEPOINT), y = as.numeric(activitypct), color = stabilityapp)) +
  geom_boxplot() + 
  facet_wrap(conditionset~ methodID)


ggplot(enzyme_app[which(enzyme_app$TIMEPOINT %in% c(0,1, 2, 4, 8, 12)),], 
       aes(x= as.factor(TIMEPOINT), y = as.numeric(Labvalue), color = stabilityapp)) +
  geom_boxplot() + scale_y_log10()+
  facet_wrap(~ methodID, scale= "free_y")

# merge with Color ----
Color_app <- merge(
  Color[, c(1, 4, 6, 7,17)],
  Appearance_2,
  by = colnames(Appearance_2)[1:3]
)
ggplot(Color_app[which(Color_app$TIMEPOINT %in% c(0,1, 2, 4, 8, 12)),], 
       aes(x= as.factor(TIMEPOINT), y = as.numeric(deltaE), color = stabilityapp)) +
  geom_boxplot()+
  scale_y_continuous(trans = "log2")

