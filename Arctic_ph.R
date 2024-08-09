
plot_theme(
  ggplot(Arctic[which(Arctic$TIMEPOINT %in% c(0:72) 
                         & Arctic$conditionset %in% c("25", "30", "35")
                        # & ARctic$zeropH <= 8.9 & Rheology$zeropH >= 8 & Rheology$conditionset != 20
                        # & Rheology$STABILITYSTUDYID %in% week_12$STABILITYSTUDYID
  ),],
  aes(x = as.numeric(TIMEPOINT), y = as.numeric(activitypct) )) +
    geom_point() + 
    # geom_boxplot() +
    facet_wrap(~conditionset, scales = "free_x") +
    # geom_vline(xintercept = 8) +
    labs(x = "TIMEPOINT", color = "") +
    # scale_y_log10() +
    geom_smooth(method = c('loess'), se=FALSE, col = 'black', span=0.5) + ##不规则拟合
    geom_smooth(method = "lm", formula = y ~ poly(x,3),color='blue',size = 2,se = T)+  ##二次拟合
    stat_poly_eq(formula = f2,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE,label.x.npc = "right", label.y.npc = "top",size = 3) #添加注释文字
)


Arctic_ph <- merge(
  Arctic[, c(1, 4, 6, 7,10)],
  PH[, c(1, 4, 6, 7, 8, 10)],
  by = colnames(PH)[c(1, 4, 6, 7)]
)


plot_theme(
  ggplot(Arctic_ph[which(Arctic_ph$TIMEPOINT %in% c(0:72) 
                      & Arctic_ph$conditionset %in% c("25", "30", "35")
                      & Arctic_ph$zeropH <= 8.5 & Arctic_ph$zeropH >= 8.2 
                      # & Rheology$STABILITYSTUDYID %in% week_12$STABILITYSTUDYID
  ),],
  aes(x = as.numeric(TIMEPOINT), y = as.numeric(activitypct) )) +
    geom_point() + 
    # geom_boxplot() +
    facet_wrap(~conditionset, scales = "free_x") +
    # geom_vline(xintercept = 8) +
    labs(x = "TIMEPOINT", color = "") +
    # scale_y_log10() +
    geom_smooth(method = c('loess'), se=FALSE, col = 'black', span=0.5) + ##不规则拟合
    geom_smooth(method = "lm", formula = y ~ poly(x,3),color='blue',size = 2,se = T)+  ##二次拟合
    stat_poly_eq(formula = f2,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE,label.x.npc = "right", label.y.npc = "top",size = 3) #添加注释文字
)



Arctic_ph <- Arctic_ph %>% mutate(
  PH = ifelse(zeropH < 6, "<6",
              ifelse(zeropH < 7, "6-7",
                     ifelse(zeropH < 8, "7-8",
                            ifelse(zeropH < 8.2, "8-8.2",
                                   ifelse(
                                     zeropH < 8.5, "8.2-8.5",
                                     ifelse(zeropH <9, "8.5-9", ">9")
                                   )))))
  
)

Arctic_ph$PH <- factor(
  Arctic_ph$PH,
  levels = c("<6", "6-7", "7-8", "8-8.2", "8.2-8.5", "8.5-9", "> 9")
)

plot_theme(
  ggplot(Arctic_ph[which(Arctic_ph$TIMEPOINT %in% c(0:72) 
                         & Arctic_ph$conditionset %in% c("25", "30", "35")
                         # & ARctic$zeropH <= 8.9 & Rheology$zeropH >= 8 & Rheology$conditionset != 20
                         # & Rheology$STABILITYSTUDYID %in% week_12$STABILITYSTUDYID
  ),],
  aes(x = as.numeric(TIMEPOINT), y = as.numeric(activitypct), color = PH )) +
    geom_point() + 
    # geom_boxplot() +
    facet_grid(~conditionset, scales = "free_x") +
    scale_color_igv() + 
    labs(x = "TIMEPOINT", color = "") +
    theme_bw(base_rect_size = 1.3)
)


Arctic_ph_app <- merge(
  Arctic_ph, 
  Appearance[,c(1,4,6,7,13)],
  by = colnames(Arctic_ph)[1:4]
)


plot_theme(
  ggplot(Arctic_ph_app[which(Arctic_ph_app$TIMEPOINT %in% c(0:72) 
                         & Arctic_ph_app$conditionset %in% c("25", "30", "35")
                         & Arctic_ph_app$zeropH <= 8.5 & Arctic_ph_app$zeropH >= 8.2 
                         # & Rheology$STABILITYSTUDYID %in% week_12$STABILITYSTUDYID
  ),],
  aes(x = as.numeric(TIMEPOINT), y = as.numeric(activitypct) )) +
    geom_point() + 
    # geom_boxplot() +
    facet_grid(stabilityapp~conditionset, scales = "free_x") +
    # geom_vline(xintercept = 8) +
    labs(x = "TIMEPOINT", color = "") +
    # scale_y_log10() +
    geom_smooth(method = c('loess'), se=FALSE, col = 'black', span=0.5) + ##不规则拟合
    geom_smooth(method = "lm", formula = y ~ poly(x,3),color='blue',size = 2,se = T)+  ##二次拟合
    stat_poly_eq(formula = f2,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE,label.x.npc = "right", label.y.npc = "top",size = 5) #添加注释文字
)

