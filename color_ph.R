
plot_theme(
  ggplot(Color[which(Color$TIMEPOINT %in% c(0:72) 
                      & Color$conditionset %in% c("25", "30", "35")
                      # & Color$zeropH <= 8.9 & Rheology$zeropH >= 8 & Rheology$conditionset != 20
                      # & Rheology$STABILITYSTUDYID %in% week_12$STABILITYSTUDYID
  ),],
  aes(x = as.numeric(TIMEPOINT), y = as.numeric(deltaE) )) +
    geom_point() + 
    # geom_boxplot() +
    facet_wrap(~conditionset) +
    # geom_vline(xintercept = 8) +
    labs(x = "TIMEPOINT", color = "") +
    scale_y_continuous(trans = "log2") +
    geom_smooth(method = c('loess'), se=FALSE, col = 'black', span=0.5) + ##不规则拟合
    geom_smooth(method = "lm", formula = y ~ poly(x,3),color='blue',size = 2,se = T)+  ##二次拟合
    stat_poly_eq(formula = f2,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE,label.x.npc = "right", label.y.npc = "top",size = 5) #添加注释文字
)

Color_ph <- merge(
  Color[, c(1, 4, 6, 7,17)],
  PH[, c(1,4,6, 7, 8,9 , 10)], 
  by = colnames(PH)[c(1, 4, 6, 7)]
) %>%
  mutate(
    PH = ifelse(zeropH < 3, "< 3",
                ifelse(zeropH < 5, "3-5",
                       ifelse(zeropH < 7, "5-7",
                              ifelse(zeropH < 8, "7-8",
                                     ifelse(zeropH < 8.5, "8-8.5",
                                            ifelse(zeropH <9, "8.5-9", "> 9"))))))
  ) %>%
  mutate(PH = factor(PH,
                     levels = c("< 3", "3-5", "5-7", "7-8", "8-8.5", "8.5-9", "> 9")))




plot_theme(
  ggplot(Color_ph[which(Color_ph$TIMEPOINT %in% c(0:72) 
                     & Color_ph$conditionset %in% c("25", "30", "35")
                     # & Color$zeropH <= 8.9 & Rheology$zeropH >= 8 & Rheology$conditionset != 20
                     # & Rheology$STABILITYSTUDYID %in% week_12$STABILITYSTUDYID
  ),],
  aes(x = as.numeric(TIMEPOINT), y = as.numeric(deltaE), color = PH )) +
    geom_point() + 
    # geom_boxplot() +
    facet_grid(~conditionset) +
    theme_bw(base_rect_size = 1.3) +
    # geom_vline(xintercept = 8) +
    labs(x = "TIMEPOINT", color = "") +
    scale_y_continuous(trans = "log2") +
    scale_color_igv()
)

plot_theme(
  ggplot(Color_ph[which(Color_ph$TIMEPOINT %in% c(0:72) 
                        & Color_ph$conditionset %in% c("25", "30", "35")
                        & Color_ph$zeropH <= 8.5 & Color_ph$zeropH >= 8
                        # & Rheology$STABILITYSTUDYID %in% week_12$STABILITYSTUDYID
  ),],
  aes(x = as.numeric(TIMEPOINT), y = as.numeric(deltaE))) +
    geom_point() + 
    # geom_boxplot() +
    facet_grid(~conditionset) +
    theme_bw(base_rect_size = 1.3) +
    # geom_vline(xintercept = 8) +
    labs(x = "TIMEPOINT", color = "") +
    scale_y_continuous(trans = "log2") +
    # scale_color_igv() +
    geom_smooth(method = c('loess'), se=FALSE, col = 'black', span=0.5) + ##不规则拟合
    geom_smooth(method = "lm", formula = y ~ poly(x,3),color='blue',size = 2,se = T)+  ##二次拟合
    stat_poly_eq(formula = f2,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE,label.x.npc = "right", label.y.npc = "top",size = 5) #添加注释文字
)
