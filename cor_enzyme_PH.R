library(ggplot2)
library(reshape2)
library(dplyr)
library(ggsci)
library(PerformanceAnalytics)#加载包
library(ggpmisc)
enzyme <- rbind(Mannanase, Vitalenz_P290, Preferenz_P283,Preferenz_P280, Xyloglucanase)


enzy_ph <- merge(
  enzyme[, c(1,4, 6,7, 8, 9, 10, 11)],
  PH[, c(1, 4, 6, 7, 8, 10,11)], 
  by = colnames(PH)[c(1, 4, 6, 7)],
  suffixes = c("_enzy", "_PH")
)

week_12 <- visc_ph[which(enzy_ph$TIMEPOINT == "12"),] %>% distinct(STABILITYSTUDYID)
week_4 <- visc_ph[which(enzy_ph$TIMEPOINT == "4"),] %>% distinct(STABILITYSTUDYID)


plot_theme(
  ggplot(enzy_ph[which(enzy_ph$TIMEPOINT %in% c(1:72) ),], aes(x = as.numeric(deltapH), y = activitypct, color = methodID)) +
    geom_point() + 
    # geom_boxplot() +
    facet_grid(methodID~TIMEPOINT) +
    geom_vline(xintercept = 0) +
    labs(x = "deltapH", color = "")
)
# enzy_ph_283 <- subset(enzy_ph, methodID == "Preferenz P283")
# plot_theme(
#   ggplot(enzy_ph_283[which(enzy_ph$TIMEPOINT %in% c(1:72) ),], aes(x = as.numeric(deltapH), y = activitypct, color = conditionset)) +
#     geom_point() + 
#     geom_boxplot() +
#     facet_wrap(~TIMEPOINT) +
#     geom_vline(xintercept = 0) +
#     labs(x = "deltapH", color = "")
# )




enzy_ph_app <- merge(
  enzy_ph, Appearance[, c(1, 4, 6, 7,13)],
  by  = colnames(enzy_ph)[1:4]
)

plot_theme(
  ggplot(enzy_ph_app[which(enzy_ph_app$TIMEPOINT %in% c(0:72)& enzy_ph_app$methodID == "Preferenz P283"),],
         aes(x = as.numeric(Labvalue_PH), y = activitypct, color = stabilityapp)) +
    geom_point() + 
    # geom_boxplot() +
    facet_grid(conditionset~TIMEPOINT, scales = "free_x") +
    geom_vline(xintercept = 8) +
    labs(x = "PH", color = "") +
    scale_color_npg()
)


plot_theme(
  ggplot(enzy_ph_app[which(enzy_ph_app$TIMEPOINT %in% c(0:72)& enzy_ph_app$methodID == "Preferenz P283"),],
         aes(x = as.numeric(Labvalue_PH), y = activitypct, color = stabilityapp)) +
    geom_point() + 
    geom_boxplot() +
    facet_grid(conditionset~TIMEPOINT) +
    geom_vline(xintercept = 8) +
    labs(x = "PH", color = "") +
    scale_color_npg()
)

enzy_ph$Labvalue_PH <- as.numeric(enzy_ph$Labvalue_PH)

## 选择在一个条件下至少有两个检测的实验 ----

filters <- enzy_ph %>%
  group_by(STABILITYSTUDYID, conditionset, methodID) %>%
  summarise(n = n())%>%
  filter(n > 3)


f1 <- y~x
f2 <- y ~ x + I(x^2) #定义回归方程
# f3 <- y ~ a * exp(b*x)
plot_theme(
  ggplot(enzy_ph[which(enzy_ph$TIMEPOINT %in% c(0:72)
                       & enzy_ph$zeropH <= 8.9 & enzy_ph$zeropH >= 8 & enzy_ph$conditionset != 20 &
                         enzy_ph$STABILITYSTUDYID %in% c(filters$STABILITYSTUDYID, week_12$STABILITYSTUDYID)),],
         aes(x = as.numeric(TIMEPOINT), y = activitypct)) +
    geom_point() + 
    # geom_boxplot() +
    facet_grid(methodID~conditionset, scales ="free_x") +
    # geom_vline(xintercept = 8) +
    labs(x = "TIMEPOINT", color = "") +
    # scale_color_npg() + 
    geom_smooth(method = c('loess'), se=FALSE, col = 'black', span=0.5) + ##不规则拟合
    geom_smooth( method = "lm", formula = y ~ poly(x,2),color='blue',size = 2,se = T)+  ##二次拟合
    stat_poly_eq(formula = f2,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE,label.x.npc = "right", label.y.npc = "top",size = 3) #添加注释文字
)



enzy_ph <- enzy_ph %>% mutate(
  PH = ifelse(zeropH < 3, "<3",
              ifelse(zeropH < 6, "3-6",
                     ifelse(zeropH < 8, "6-8",
                            ifelse(zeropH < 8.5, "8-8.5",
                                   ifelse(
                                     zeropH < 9, "8.5-9", "> 9"
                                   )))))
  
)

enzy_ph$PH <- factor(
  enzy_ph$PH,
  levels = c("<3", "3-6", "6-8", "8-8.5", "8.5-9", "> 9")
)

plot_theme(
  ggplot(enzy_ph[which(enzy_ph$TIMEPOINT %in% c(0:72)
                       & enzy_ph$conditionset != 20 &
                         enzy_ph$STABILITYSTUDYID %in% c(filters$STABILITYSTUDYID, week_12$STABILITYSTUDYID)),],
         aes(x = as.numeric(TIMEPOINT), y = activitypct, color = PH)) +
    geom_point() + 
    # geom_boxplot() +
    facet_grid(methodID~conditionset, scales = "free_x") +
    scale_color_npg() + 
    labs(x = "TIMEPOINT", color = "") +
    theme_bw(base_rect_size = 1.3)
)




filters <- enzy_ph_app %>%
  group_by(STABILITYSTUDYID, methodID, conditionset, stabilityapp) %>%
  summarise(n = n()) %>%
  filter(n > 3)

plot_theme(
  ggplot(enzy_ph_app[which(enzy_ph_app$TIMEPOINT %in% c(0:72)
                           & enzy_ph_app$STABILITYSTUDYID %in% c(filters$STABILITYSTUDYID, week_12$STABILITYSTUDYID)
                           & enzy_ph_app$methodID == "Preferenz P283"
                       & enzy_ph_app$zeropH <= 8.5 & enzy_ph_app$zeropH >= 8),],
         aes(x = as.numeric(TIMEPOINT), y = activitypct)) +
    geom_point() +
    # geom_boxplot() +
    facet_grid(stabilityapp~conditionset,scales =  "free_x") +
    # geom_vline(xintercept = 8) +
    labs(x = "TIMEPOINT", color = "") +
    # scale_color_npg() +
    geom_smooth(method = c('loess'), se=FALSE, col = 'black', span=0.5) + ##不规则拟合
    geom_smooth(method = "lm", formula = y ~ poly(x,3),color='blue',size = 2,se = T)+  ##二次拟合
    stat_poly_eq(formula = f2,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE,label.x.npc = "right", label.y.npc = "top",size = 3) #添加注释文字
)


enzy_ph_p283 <- enzy_ph_app[which(enzy_ph_app$TIMEPOINT %in% c(0:72)
                                  & enzy_ph_app$STABILITYSTUDYID %in% c(filters$STABILITYSTUDYID, week_12$STABILITYSTUDYID)
                                  & enzy_ph_app$methodID == "Preferenz P283"
                                  & enzy_ph_app$zeropH <= 8.5 & enzy_ph_app$zeropH >= 8),] %>%
  filter(conditionset == 25)
x  <- as.numeric(enzy_ph_p283$TIMEPOINT)
y <- as.numeric(enzy_ph_p283$activitypct)

model <- nls(y ~ a *exp(b *x), start = list(a = 1, b = 1))

plot_theme(
  ggplot(enzy_ph_app[which(enzy_ph_app$TIMEPOINT %in% c(0:72)
                           & enzy_ph_app$STABILITYSTUDYID %in% c(filters$STABILITYSTUDYID, week_12$STABILITYSTUDYID)
                           & enzy_ph_app$methodID == "Mannanase"
                           & enzy_ph_app$zeropH <= 8.5 & enzy_ph_app$zeropH >= 8),],
         aes(x = as.numeric(TIMEPOINT), y = activitypct)) +
    geom_point() +
    # geom_boxplot() +
    facet_grid(stabilityapp~conditionset,scales = "free_x") +
    # geom_vline(xintercept = 8) +
    labs(x = "TIMEPOINT", color = "") +
    # scale_color_npg() +
    geom_smooth(method = c('loess'), se=FALSE, col = 'black', span=0.5) + ##不规则拟合
    geom_smooth(method = "lm", formula = y ~ poly(x,3),color='blue',size = 2,se = T)+  ##二次拟合
    stat_poly_eq(formula = f2,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE,label.x.npc = "right", label.y.npc = "top",size = 5) #添加注释文字
)



plot_theme(
  ggplot(enzy_ph_app[which(enzy_ph_app$TIMEPOINT %in% c(0:72)
                           & enzy_ph_app$STABILITYSTUDYID %in% c(filters$STABILITYSTUDYID, week_12$STABILITYSTUDYID)
                           & enzy_ph_app$methodID == "Vitalenz P290"
                           & enzy_ph_app$zeropH <= 8.5 & enzy_ph_app$zeropH >= 8),],
         aes(x = as.numeric(TIMEPOINT), y = activitypct)) +
    geom_point() +
    # geom_boxplot() +
    facet_grid(stabilityapp~conditionset) +
    # geom_vline(xintercept = 8) +
    labs(x = "TIMEPOINT", color = "") +
    # scale_color_npg() +
    geom_smooth(method = c('loess'), se=FALSE, col = 'black', span=0.5) + ##不规则拟合
    geom_smooth(method = "lm", formula = y ~ poly(x,3),color='blue',size = 2,se = T)+  ##二次拟合
    stat_poly_eq(formula = f2,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE,label.x.npc = "right", label.y.npc = "top",size = 5) #添加注释文字
)

## correlate the Viscosity ----

enzy_ph_vis <- merge(
  enzy_ph, Viscosity[, c(1,4, 6, 7, 8,9, 10)],
  by = colnames(enzy_ph)[1:4]
)


plot_theme(
  ggplot(enzy_ph_vis[which(enzy_ph_vis$TIMEPOINT %in% c(0:72)
                           # & enzy_ph_vis$STABILITYSTUDYID %in% c(filters$STABILITYSTUDYID, week_12$STABILITYSTUDYID)
                           & enzy_ph_vis$methodID == "Mannanase"
                           & enzy_ph_vis$zeropH <= 9 & enzy_ph_vis$zeropH >= 8),],
         aes(x = as.numeric(TIMEPOINT), y = activitypct)) +
    geom_point() +
    # geom_boxplot() +
    facet_grid(~conditionset) +
    # geom_vline(xintercept = 8) +
    labs(x = "TIMEPOINT", color = "") +
    # scale_color_npg() +
    geom_smooth(method = c('loess'), se=FALSE, col = 'black', span=0.5) + ##不规则拟合
    geom_smooth(method = "lm", formula = y ~ poly(x,3),color='blue',size = 2,se = T)+  ##二次拟合
    stat_poly_eq(formula = f2,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE,label.x.npc = "right", label.y.npc = "top",size = 5) #添加注释文字
)

exp_model <- function(x, a, b, c){
  a -bx + log(x, base = c)
}

fit <- nls(enzy_ph[which(enzy_ph$TIMEPOINT %in% c(0:72) & enzy_ph$methodID == "Mannanase"
                             & enzy_ph$zeropH <= 9 & enzy_ph$zeropH >= 8),]$activitypct ~ 
             exp_model(enzy_ph[which(enzy_ph$TIMEPOINT %in% c(0:72)
                               & enzy_ph$methodID == "Mannanase"
                               & enzy_ph$zeropH <= 9 & enzy_ph$zeropH >= 8)]$TIMEPOINT, a, b, c))
