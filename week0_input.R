library(reshape2)
library(dplyr)

# input data ---
# data both contains week 0 and week 12


Rheology_1 <- Rheology %>%
  mutate(zeroviscosity = as.numeric(zeroviscosity)) %>%
  filter( TIMEPOINT == 0 & STABILITYSTUDYID != "Analytical")%>%
  select(1, 7, 8) %>%
  distinct(STABILITYSTUDYID, Shear, .keep_all = T) %>%
  dcast(STABILITYSTUDYID ~ Shear)

colnames(Rheology_1)[2:7] <- paste0("Rhe_", colnames(Rheology_1)[2:7])


Viscosity_1 <- Viscosity %>%
  filter(TIMEPOINT ==0 & STABILITYSTUDYID != "Analytical") %>%
  select(1,7) %>%
  distinct(STABILITYSTUDYID, .keep_all = T)


enzyme_1 <- enzyme %>%
  mutate(Labvalue = as.numeric(Labvalue)) %>%
  filter(TIMEPOINT ==0 & STABILITYSTUDYID != "Analytical",
         methodID != "Mannanase"
         ) %>%
  select(1,8) %>%
  distinct(STABILITYSTUDYID, .keep_all = T) 
colnames(enzyme_1)[2] <- "activity_pct"


Mannanase_1 <- Mannanase %>%
  mutate(Labvalue = as.numeric(Labvalue)) %>%
  filter(TIMEPOINT ==0 & STABILITYSTUDYID != "Analytical"
         ) %>%
  select(1, 8) %>%
  distinct(STABILITYSTUDYID, .keep_all = T)
  
colnames(Mannanase_1)[2] <- "Mannanase"

Arctic_1 <- Arctic %>%
  mutate(Labvalue = as.numeric(Labvalue)) %>%
  filter(TIMEPOINT ==0 & STABILITYSTUDYID != "Analytical"
        
         ) %>%
  select(1, 8) %>%
  distinct(STABILITYSTUDYID, .keep_all = T)
colnames(Arctic_1)[2] <- "Arctic_act"


PH_1 <- PH %>%
  mutate(Labvalue = as.numeric(Labvalue)) %>%
  filter(TIMEPOINT ==0 & STABILITYSTUDYID != "Analytical"
        
  ) %>%
  select(1, 8) %>%
  distinct(STABILITYSTUDYID, .keep_all = T)
colnames(PH_1)[2] <- "PH"

Density_1 <- Density %>%
  mutate(Labvalue = as.numeric(Labvalue)) %>%
  filter(TIMEPOINT ==0 & STABILITYSTUDYID != "Analytical"
         # & conditionset %in% c(25)
  ) %>%
  select(1,  8) %>%
  distinct(STABILITYSTUDYID, .keep_all = T)

names(Density_1)[2] <- "density"



Odor_1 <- Odor %>%
  mutate(Labvalue = as.numeric(Labvalue)) %>%
  filter(TIMEPOINT ==0 & STABILITYSTUDYID != "Analytical"
         # & conditionset %in% c(25)
  ) %>%
  select(1, 8) %>%
  distinct(STABILITYSTUDYID, .keep_all = T)
names(Odor_1)[2] <- "odor"


res_i <- merge(
  Rheology_1, PH_1, by = "STABILITYSTUDYID"
) %>% 
  merge(
  enzyme_1, by = "STABILITYSTUDYID"
) %>% 
  merge(
    Arctic_1, by = "STABILITYSTUDYID") %>%
  merge(
    Density_1, by =  "STABILITYSTUDYID"
  ) %>%
  merge(
    Appearance_2[,c(1, 4)] %>% distinct(STABILITYSTUDYID, stabilityapp),  by =  "STABILITYSTUDYID"
  )

rownames(res_i)<- res_i$STABILITYSTUDYID

input <- res_i[, 2:11]
# output <- data.frame(
#   row.names = rownames(res_i),
#   output = res_i$stabilityapp
# )

output <- as.factor(res_i[, 12])

