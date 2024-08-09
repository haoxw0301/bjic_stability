

Rheology_5_2 <- dcast(Rheology_5, 
                      STABILITYSTUDYID  + batchid ~ TIMEPOINT,
                      value.var = "Labvalue", first)
colnames(Rheology_5_2)[3:ncol(Rheology_5_2)] <-
  paste0("Rheology_", colnames(Rheology_5_2)[3:ncol(Rheology_5_2)])

Viscosity_5_2 <- dcast(Viscosity_5,
                      STABILITYSTUDYID  + batchid ~ TIMEPOINT,
                       value.var = "Labvalue", first)
colnames(Viscosity_5_2)[3:ncol(Viscosity_5_2)] <-
  paste0("Viscosity_", colnames(Viscosity_5_2)[3:ncol(Viscosity_5_2)])

PH_5_2 <-  dcast(PH_5,
                 STABILITYSTUDYID  + batchid ~ TIMEPOINT,
                 value.var = "deltapH")
colnames(PH_5_2)[3:ncol(PH_5_2)] <-
  paste0("deltaPH_", colnames(PH_5_2)[3:ncol(PH_5_2)] )

enzyme_5_2 <- dcast(enzyme_5,
                    STABILITYSTUDYID  + batchid + methodID ~ TIMEPOINT,
                   value.var = "LNActivity", )
colnames(enzyme_5_2)[4:ncol(enzyme_5_2)]<-
  paste0("enzyme_", colnames(enzyme_5_2)[4:ncol(enzyme_5_2)])


Arctic_5_2 <- dcast(Arctic_5,
                    STABILITYSTUDYID  + batchid ~ TIMEPOINT,
                    value.var = "LNActivity")
colnames(Arctic_5_2)[3:ncol(Arctic_5_2)] <-
  paste0("Arctic_", colnames(Arctic_5_2)[3:ncol(Arctic_5_2)])

Odor_5_2 <- dcast(Odor_5,
                  STABILITYSTUDYID  + batchid ~ TIMEPOINT,
                  value.var = "Labvalue")
colnames(Odor_5_2)[3:ncol(Odor_5_2)] <-
  paste0("Odor_", colnames(Odor_5_2)[3:ncol(Odor_5_2)])

Color_5_2 <- dcast(Color_5,
                   STABILITYSTUDYID  + batchid ~ TIMEPOINT,
                   value.var = "deltaE")
colnames(Color_5_2)[3:ncol(Color_5_2)] <-
  paste0("Color_", colnames(Color_5_2)[3:ncol(Color_5_2)])

Appearance_5_2 <- dcast(Appearance_5,
                        STABILITYSTUDYID  + batchid + initialappearance ~ TIMEPOINT ,
                        value.var = "stabilityapp")
colnames(Appearance_5_2)[4:ncol(Appearance_5_2)] <-
  paste0("Appearance_", colnames(Appearance_5_2)[4:ncol(Appearance_5_2)])



# merge data ----
res_2 <- merge(Rheology_5_2, Viscosity_5_2,
               by = c("STABILITYSTUDYID", "batchid"), all = T) %>%
  merge(PH_5_2, by = c("STABILITYSTUDYID", "batchid"), all = T) %>%
  merge(enzyme_5_2, by = c("STABILITYSTUDYID", "batchid"), all = T) %>%
  merge(Arctic_5_2, by = c("STABILITYSTUDYID", "batchid"), all = T) %>%
  merge(Color_5_2, by = c("STABILITYSTUDYID", "batchid"), all = T) %>%
  merge(Appearance_5_2, by = c("STABILITYSTUDYID", "batchid")) %>%
  unite("ID", STABILITYSTUDYID,batchid, methodID)


input <- res_2[,1:113]

output <- res_2[, c(1, 114:146)]

output_num <- data.frame(
  timepoint = "n",
  num = 0
)
for (i in 3:ncol(output)){
  timepoint = length(output[, i][which(output[,i] %in% c("PASS", "FAIL"))])
  num = colnames(output)[i]
  dt <- data.frame(
    timepoint=timepoint,
    num = num
  )
}
