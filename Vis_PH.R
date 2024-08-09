

f1 <- y~x
f2 <- y ~ x + I(x^2) #定义回归方程

plot_theme(
  ggplot(Viscosity[which(Viscosity$TIMEPOINT %in% c(0:72) &
                          Viscosity$conditionset %in% c("25", "40") 
                        # & Viscosity$zeropH <= 8.9 & Viscosity$zeropH >= 8 & Viscosity$conditionset != 20
                        # & Viscosity$STABILITYSTUDYID %in% week_12$STABILITYSTUDYID
  ),],
  aes(x = as.numeric(TIMEPOINT), y = as.numeric(Labvalue) )) +
    geom_point() + 
    # geom_boxplot() +
    facet_wrap(~conditionset) +
    # geom_vline(xintercept = 8) +
    labs(x = "TIMEPOINT", color = "") +
    scale_y_log10() +
    geom_smooth(method = c('loess'), se=FALSE, col = 'black', span=0.5) + ##不规则拟合
    geom_smooth(method = "lm", formula = y ~ poly(x,3),color='blue',size = 2,se = T)+  ##二次拟合
    stat_poly_eq(formula = f2,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE,label.x.npc = "right", label.y.npc = "top",size = 5) #添加注释文字
)





Vis_ph <- merge(
  Viscosity[, c(1, 4,5, 6, 7, 8, 9)],
  PH[, c(1, 4, 6, 7, 8, 10,11)], 
  by = colnames(PH)[c(1, 4, 6, 7)],
  suffixes = c("_Vis", "_PH")
)


filters <- Vis_ph %>%
  group_by(STABILITYSTUDYID, conditionset) %>%
  summarise(n = n()) %>%
  filter(n > 3)

plot_theme(
  ggplot(Vis_ph[which(Vis_ph$TIMEPOINT %in% c(0:72) 
                      # Viscosity$conditionset %in% c("25", "40") 
                      & Vis_ph$zeropH <= 8.9 & Vis_ph$zeropH >= 8
                      & Vis_ph$STABILITYSTUDYID %in% filters$STABILITYSTUDYID
  ),],
  aes(x = as.numeric(TIMEPOINT), y = as.numeric(Labvalue_Vis) )) +
    geom_point() + 
    # geom_boxplot() +
    facet_wrap(~conditionset) +
    # geom_vline(xintercept = 8) +
    labs(x = "TIMEPOINT", color = "") +
    scale_y_log10() +
    geom_smooth(method = c('loess'), se=FALSE, col = 'black', span=0.5) + ##不规则拟合
    geom_smooth(method = "lm", formula = y ~ poly(x,3),color='blue',size = 2,se = T)+  ##二次拟合
    stat_poly_eq(formula = f2,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE,label.x.npc = "right", label.y.npc = "top",size = 5) #添加注释文字
)




Vis_ph <- Vis_ph %>% mutate(
  PH = ifelse(zeropH < 3, "<3",
              ifelse(zeropH < 6, "3-6",
                     ifelse(zeropH < 8, "6-8",
                            ifelse(zeropH < 8.5, "8-8.5",
                                   ifelse(
                                     zeropH < 9, "8.5-9", "> 9"
                                   )))))
  
)

Vis_ph$PH <- factor(
  Vis_ph$PH,
  levels = c("<3", "3-6", "6-8", "8-8.5", "8.5-9", "> 9")
)

plot_theme(
  ggplot(Vis_ph[which(Vis_ph$TIMEPOINT %in% c(0:72) 
                      # Viscosity$conditionset %in% c("25", "40") 
                      # & Vis_ph$zeropH <= 8.9 & Vis_ph$zeropH >= 8
                      & Vis_ph$STABILITYSTUDYID %in% filters$STABILITYSTUDYID
  ),],
  aes(x = as.numeric(TIMEPOINT), y = as.numeric(Labvalue_Vis))) +
    geom_point() + 
    theme_bw(base_rect_size = 1.3 ) +
    facet_grid(PH~conditionset) +
    # geom_vline(xintercept = 8) +
    labs(x = "TIMEPOINT", color = "") +
    scale_y_log10()
)


plot_theme(
  ggplot(Vis_ph[which(Vis_ph$TIMEPOINT %in% c(0:72)
                          & Vis_ph$conditionset %in% c("25", "40")
                         # & Viscosity$zeropH <= 8.9 & Viscosity$zeropH >= 8 & Viscosity$conditionset != 20
                         # & Viscosity$STABILITYSTUDYID %in% week_12$STABILITYSTUDYID
  ),],
  aes(x = as.numeric(TIMEPOINT), y = as.numeric(Labvalue_Vis), color= PH )) +
    geom_point() + 
    # geom_boxplot() +
    facet_wrap(~conditionset) +
    # geom_vline(xintercept = 8) +
    labs(x = "TIMEPOINT", color = "") +
    scale_y_log10() +
    theme_bw(base_rect_size = 1.3) +
    scale_color_igv()
)

