library(ggplot2)
library(reshape2)
library(dplyr)


visc_ph <- merge(
  Viscosity[, c(1,4, 6, 8, 9, 10)],
  PH[, c(1, 4, 6, 7, 8, 11)], 
  by = colnames(PH)[c(1, 4, 6, 7)],
  suffixes = c("_Visc", "_PH")
)


# ggplot(visc_ph, aes(x = as.factor(TIMEPOINT), y = as.numeric(Labvalue_Visc), color = as.numeric(Labvalue_PH))) + 
#   geom_point() +
#   scale_y_continuous(trans = "log10") +
#   scale_color_gradient(low = "blue",  high = "gold" )

plot_theme <- function(p){
  p <- p +
    theme(
      axis.title = element_text(size =20, color = "black"),
      axis.text = element_text(size = 18, color = "black"),
      legend.text = element_text(size = 18, color= "black"),
      strip.text = element_text(size = 20, color = "black")
    )
  return(p)
}

week_12 <- visc_ph[which(visc_ph$TIMEPOINT == "12"),] %>% distinct(STABILITYSTUDYID)
week_4 <- visc_ph[which(visc_ph$TIMEPOINT == "4"),] %>% distinct(STABILITYSTUDYID)

plot_theme(
  ggplot(visc_ph, aes(x = deltapH, y=as.numeric(Labvalue_Visc))) +
  geom_point() +
  scale_y_log10() +
  facet_wrap(~conditionset) +
  geom_density2d())
  

plot_theme(
  ggplot(visc_ph[which(visc_ph$STABILITYSTUDYID %in% week_12$STABILITYSTUDYID & 
                         visc_ph$TIMEPOINT %in% c("0", "12")),],
         aes(x = as.numeric(Labvalue_PH),y = as.numeric(Labvalue_Visc), color = STABILITYSTUDYID)) +
  geom_point() +
  scale_y_log10() +
  # geom_density2d() +
  facet_grid(conditionset~TIMEPOINT)) +
  theme(
    legend.position = "none"
  ) + labs(x = "PH", y = "Visc")


plot_theme(
  ggplot(visc_ph[which(visc_ph$STABILITYSTUDYID %in% week_12$STABILITYSTUDYID & 
                         visc_ph$TIMEPOINT %in% c("0", "4")),],
         aes(x = as.numeric(Labvalue_PH),y = as.numeric(Labvalue_Visc), color = STABILITYSTUDYID)) +
    geom_point() +
    scale_y_log10() +
    # geom_density2d() +
    facet_grid(conditionset~TIMEPOINT)) +
  theme(
    legend.position = "none"
  )+ labs(x = "PH", y = "Visc")


plot_theme(ggplot(visc_ph, aes(x = as.numeric(Labvalue_PH),y = deltaviscosity)) +
  geom_point() +
  # scale_y_log10() +
  geom_density2d()) 
  

### 
  
visc_ph_2 <- merge(
  visc_ph[which(visc_ph$TIMEPOINT == "0"),],
  visc_ph[which(visc_ph$TIMEPOINT == "4"),],
  by = colnames(visc_ph)[1:3], suffixes = c("_0", "_4")
) %>%
  merge(Appearance[which(Appearance$TIMEPOINT == 4), c(1, 4,6, 7, 13)],
        by = colnames(visc_ph)[1:3])


plot_theme(
  ggplot(visc_ph_2,
         aes(x = as.numeric(Labvalue_PH_4) -  as.numeric(Labvalue_PH_0),
             y = as.numeric(Labvalue_Visc_4) -  as.numeric(Labvalue_Visc_0),
             color = stabilityapp)) +
    # geom_point()+
    geom_density2d()+ 
    labs(x = "PH(week4 - week0)", y = "Visc(week4 - week0)")
)


visc_ph_3 <- merge(
  visc_ph[which(visc_ph$TIMEPOINT == "0"),],
  visc_ph[which(visc_ph$TIMEPOINT == "12"),],
  by = colnames(visc_ph)[1:3], suffixes = c("_0", "_12")
) %>%
  merge(Appearance[which(Appearance$TIMEPOINT == 4), c(1, 4,6, 7, 13)],
        by = colnames(visc_ph)[1:3])



plot_theme(
  ggplot(visc_ph_3,
         aes(x = as.numeric(Labvalue_PH_12) -  as.numeric(Labvalue_PH_0),
             y = as.numeric(Labvalue_Visc_12) -  as.numeric(Labvalue_Visc_0),
             color = stabilityapp)) +
    # geom_point()+
    geom_density2d()+ 
    # facet_wrap(~conditionset) +
    labs(x = "PH(week12 - week0)", y = "Visc(week12 - week0)")
)



