

f1 <- y~x
f2 <- y ~ x + I(x^2) #定义回归方程

plot_theme(
  ggplot(Rheology[which(Rheology$TIMEPOINT %in% c(0:72) &
                        Rheology$conditionset %in% c("25", "40") 
                       # & Rheology$zeropH <= 8.9 & Rheology$zeropH >= 8 & Rheology$conditionset != 20
                         # & Rheology$STABILITYSTUDYID %in% week_12$STABILITYSTUDYID
                       ),],
         aes(x = as.numeric(TIMEPOINT), y = as.numeric(Labvalue) )) +
    geom_point() + 
    # geom_boxplot() +
    facet_grid(Shear~conditionset) +
    # geom_vline(xintercept = 8) +
    labs(x = "TIMEPOINT", color = "") +
    scale_y_log10() +
    geom_smooth(method = c('loess'), se=FALSE, col = 'black', span=0.5) + ##不规则拟合
    geom_smooth(method = "lm", formula = y ~ poly(x,3),color='blue',size = 2,se = T)+  ##二次拟合
    stat_poly_eq(formula = f2,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE,label.x.npc = "right", label.y.npc = "top",size = 5) #添加注释文字
)

Rhe_ph <- merge(
  Rheology[,c(1, 4, 6:11)],
  PH[, c(1, 4, 6:8, 10 ,11)],
  by = colnames(PH)[c(1,4, 6,7)],
  suffixes = c("_RHe","_PH")
)


Rhe_ph <- Rhe_ph %>% mutate(
  PH = ifelse(zeropH < 3, "<3",
              ifelse(zeropH < 6, "3-6",
                     ifelse(zeropH < 8, "6-8",
                            ifelse(zeropH < 8.5, "8-8.5",
                                   ifelse(
                                     zeropH < 9, "8.5-9", "> 9"
                                   )))))
  
)

Rhe_ph$PH <- factor(
  Rhe_ph$PH,
  levels = c("<3", "3-6", "6-8", "8-8.5", "8.5-9", "> 9")
)

plot_theme(
  ggplot(Rhe_ph[which(Rhe_ph$TIMEPOINT %in% c(0:72) &
                          Rhe_ph$conditionset %in% c("25", "40") 
                        # & Rheology$zeropH <= 8.9 & Rheology$zeropH >= 8 & Rheology$conditionset != 20
                        # & Rheology$STABILITYSTUDYID %in% week_12$STABILITYSTUDYID
  ),],
  aes(x = as.numeric(TIMEPOINT), y = as.numeric(Labvalue_RHe) )) +
    geom_point() + 
    # geom_boxplot() +
    facet_grid(Shear~conditionset) +
    # geom_vline(xintercept = 8) +
    labs(x = "TIMEPOINT", color = "") +
    scale_y_log10() +
    geom_smooth(method = c('loess'), se=FALSE, col = 'black', span=0.5) + ##不规则拟合
    geom_smooth(method = "lm", formula = y ~ poly(x,3),color='blue',size = 2,se = T)+  ##二次拟合
    stat_poly_eq(formula = f2,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE,label.x.npc = "right", label.y.npc = "top",size = 5) #添加注释文字
)


plot_theme(x`` %in% c("25", "40") 
                      # & Rheology$zeropH <= 8.9 & Rheology$zeropH >= 8 & Rheology$conditionset != 20
                      # & Rheology$STABILITYSTUDYID %in% week_12$STABILITYSTUDYID
  ),],
  aes(x = as.numeric(TIMEPOINT), y = as.numeric(deltaviscosity), color = PH )) +
    geom_point() + 
    # geom_boxplot() +
    facet_grid(Shear~conditionset) +
    scale_color_igv()+
    labs(x = "TIMEPOINT", color = "") +
    scale_y_log10() +
    theme_bw(base_rect_size = 1.3)
)
