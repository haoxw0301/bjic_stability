

color_ph <- merge(
  Color[, c(1,4, 6, 7, 17)],
  PH[, c(1, 4, 6, 7, 8, 11)], 
  by = colnames(PH)[c(1, 4, 6, 7)]
)


week_12 <- visc_ph[which(color_ph$TIMEPOINT == "12"),] %>% distinct(STABILITYSTUDYID)
week_4 <- visc_ph[which(color_ph$TIMEPOINT == "4"),] %>% distinct(STABILITYSTUDYID)


plot_theme(
  ggplot(color_ph[which(color_ph$TIMEPOINT %in% c(1:72) ),], aes(x = as.numeric(deltapH), y = deltaE, color = STABILITYSTUDYID)) +
    geom_point() + 
    # geom_boxplot() +
    facet_grid(conditionset~TIMEPOINT, scales = "free_x") +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 1) +
    scale_y_continuous(trans = "log2") +
    labs(x = "deltapH", color = "")
) + theme(
  legend.position = "none"
)


plot_theme(
  ggplot(color_ph[which(color_ph$TIMEPOINT %in% c(1:72) ),], aes(x = as.numeric(deltapH), y = deltaE, color = STABILITYSTUDYID)) +
    geom_point() + 
    geom_boxplot() +
    facet_grid(conditionset~TIMEPOINT, scales = "free_x") +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 1) +
    scale_y_continuous(trans = "log2") +
    labs(x = "deltapH", color = "")
) + theme(
  legend.position = "none",
  axis.text.x = element_text(size =12)
)

color_ph_app <- merge(
  color_ph, 
  Appearance[, c(1, 4, 6, 7,13)],
  by = colnames(color_ph)[1:4]
)


plot_theme(
  ggplot(color_ph_app[which(color_ph_app$TIMEPOINT %in% c(1:72) ),], 
         aes(x = as.numeric(deltapH), y = deltaE, color = stabilityapp)) +
    geom_point() + 
    geom_boxplot(alpha = 0.5) +
    facet_grid(conditionset~TIMEPOINT, scales = "free_x") +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 1) +
    scale_y_continuous(trans = "log2") +
    labs(x = "deltapH", color = "")
) + scale_color_npg() +
  theme(
    axis.text.x = element_text(size =12)
  )
