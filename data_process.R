library(dplyr)
# split the data by methodid ---- 

methods <- distinct(df, methodID)



## 去掉全是缺失值的列, conditionset == 25
dele_NA <- function(df){
  missing_value <- colSums(is.na(df))
  
  columns_to_delete <- which(missing_value == nrow(df))
  
  df <- subset(df, select = -columns_to_delete)
  
  # df <- subset(df, conditionset == 25)
  
  return(df)
}

##删除完全相同的列


Rheology <- df[which(df$methodID == "Rheology"), ] %>%
    dele_NA() 

Viscosity <- df[which(df$methodID == "Viscosity Brookfield"),] %>%
  dele_NA() 

PH <- df[which(df$methodID == "pH10%"),] %>%
  dele_NA() 

Vitalenz_P290<- df[which(df$methodID == methods[4,1]), ] %>%
  dele_NA() # enzyme ?


Xyloglucanase <- df[which(df$methodID == methods[5,1]), ] %>%
  dele_NA() # enzyme

Mannanase <-  df[which(df$methodID == methods[6,1]), ] %>%
  dele_NA() # enzyme

Preferenz_P283 <-  df[which(df$methodID == methods[7,1]), ] %>%
  dele_NA()  # enzyme


Arctic<- df[which(df$methodID == methods[8,1]), ] %>%
  dele_NA() ## 是酶吗？

Preferenz_P280 <- df[which(df$methodID == methods[9,1]), ] %>%
  dele_NA() # 345

Odor <- df[which(df$methodID == methods[10,1]), ] %>%
  dele_NA() # 气味

Density <- df[which(df$methodID == methods[11,1]), ] %>%
  dele_NA()

Color <- df[which(df$methodID == "Color"),] %>%
  dele_NA()

Appearance <- df[which(df$methodID == "Appearance"),] %>%
  dele_NA()

Dioxane <- df[which(df$methodID == "Dioxane"),] %>%
  dele_NA() ## 二氧六环

Dynamic_Concentration_Rheometry <- df[which(df$methodID == "Dynamic Concentration Rheometry"),] %>%
  dele_NA() # 680 删掉

# 选择可以使用的实验数据 ----
# 即每个 STABILITYSTUDYID,  sampleid, batchid 有在连续时间点的数据

time_points <- function(df, id){
  if (id == "STABILITYSTUDYID"){
    stabilityid <- df %>%
      distinct(STABILITYSTUDYID, TIMEPOINT) %>%
      group_by(STABILITYSTUDYID) %>%
      summarise(times = n()) %>%
      filter(times > 1)
    return(stabilityid)
  } else if (id == "sampleid") {
    sampleid <- df %>%
      distinct(sampleid, TIMEPOINT) %>%
      group_by(sampleid) %>%
      summarise(times = n()) %>%
      filter(times > 1)
    return(sampleid)
  } else if (id == "batchid"){
    batchid <- df %>%
      distinct(batchid, TIMEPOINT) %>%
      group_by(batchid) %>%
      summarise(times = n()) %>%
      filter(times > 1)
    return(batchid)
  }
  
} 
Rheology_sampleid <- time_points(Rheology, "sampleid") # 0
Rheology_stabilityid<- time_points(Rheology, "STABILITYSTUDYID")
Rheology_batchid <- time_points(Rheology, "batchid")

## sampleid 是唯一的

# only 0 week  ----

only_0_week <- function(df){
  dt <- df %>%
    group_by(STABILITYSTUDYID, TIMEPOINT) %>%
    summarise(times = n()) %>%
    filter(times == 1)
  
  df_2 <- df[which(df$STABILITYSTUDYID %in% dt$STABILITYSTUDYID),] %>%
    filter(TIMEPOINT == 0)
  return(df_2)
}


Rheology_0_week <- only_0_week(Rheology)

Viscosity_0_week <- only_0_week(Viscosity)


# experiments have more than 5 timepoints ----

multi_timepoints <- function(df, t = 5){
  dt <- df %>%
    distinct(STABILITYSTUDYID, TIMEPOINT) %>%
    group_by(STABILITYSTUDYID) %>%
    summarise(times = n()) %>%
    filter(times >= t)
  
  df_2 <- df[which(df$STABILITYSTUDYID %in% dt$STABILITYSTUDYID),]
  return(df_2)
}

Rheology_5 <- multi_timepoints(Rheology, 5)

# ggplot(Rheology_5, aes(x = TIMEPOINT, y = as.numeric(Labvalue), shape = conditionset)) + 
#   geom_point() +
#   # scale_x_log10() +
#   scale_y_log10() +
#   facet_grid(conditionset ~ Shear)

Viscosity_5 <- multi_timepoints(Viscosity, 5) 
  # dcast(STABILITYSTUDYID  ~ TIMEPOINT, value.var = "Labvalue")

PH_5 <- multi_timepoints(PH)

enzyme_5 <- multi_timepoints(
  rbind(Vitalenz_P290, Xyloglucanase, Mannanase, Preferenz_P283, Preferenz_P280)
)

Arctic_5 <- multi_timepoints(Arctic) ## 暂时没用

Odor_5 <- multi_timepoints(Odor) # delete

Density_5 <- multi_timepoints(Density)  # delete

Color_5 <- multi_timepoints(Color)

Appearance_5 <- multi_timepoints(Appearance)

Dioxane_5 <- multi_timepoints(Dioxane)

## 每个值对结果的影响 ----
Rheology_5$Labvalue = as.numeric(Rheology_5$Labvalue)


plot_theme(
  ggplot(Rheology_5, aes(x = as.numeric(TIMEPOINT), y = as.numeric(Labvalue), color = STABILITYSTUDYID)) +
             geom_point() +
              # geom_boxplot() +
             facet_grid(conditionset~Shear, scales = "free_y") +
             theme_bw() +
             # scale_y_log10() +
             geom_smooth()  +
              ylim(0,1000) +
             theme(legend.position = "none")
) 

plot_theme(
  ggplot(Viscosity_5, aes(x = as.numeric(TIMEPOINT), y = deltaviscosity, color = STABILITYSTUDYID)) +
    geom_point() + 
    # facet_wrap(~Shear) +
    theme_bw() +
    # scale_y_log10() + 
    geom_smooth() + 
    theme(legend.position = "none")
)

plot_theme(
  ggplot(PH_5, aes(x = as.numeric(TIMEPOINT), y = deltapH, color = STABILITYSTUDYID)) +
    geom_point() + 
    geom_smooth() +
    ylim(-2, 1) + 
    theme_classic(base_rect_size = 1.5) +
    geom_smooth() + theme(legend.position = "none")
)

plot_theme(ggplot(enzyme_5, aes(x = as.numeric(TIMEPOINT), y = activitypct, color = STABILITYSTUDYID)) +
             geom_point() + 
             geom_smooth() +
             ylim(0,100) +
             theme_classic(base_rect_size = 1.5) +
             facet_wrap(~methodID, scales = "free") +
             theme(legend.position = "none"))


ggplot(Arctic_5, aes(x = as.numeric(TIMEPOINT), y = LNActivity, color = STABILITYSTUDYID)) +
  geom_point() +
  geom_smooth() +
  theme_classic(base_rect_size = 1.5) +
  theme(legend.position = "none")

ggplot(Color_5, aes(x = as.numeric(TIMEPOINT), y = deltaE, color = STABILITYSTUDYID)) +
  geom_point() + 
  geom_smooth() +
  theme_classic(base_rect_size = 1.5) +
  theme(legend.position = "none")

ggplot(Dioxane_5, aes(x = as.numeric(TIMEPOINT), y = deltadioxane, color = STABILITYSTUDYID)) +
  geom_point() + 
  geom_smooth() +
  theme_classic(base_rect_size = 1.5) +
  ylim(-0.5,1) + theme(legend.position = "none")

# merge data ----

## labvalue ----
## all.x = T
res_1 <- merge(Rheology_5[, c("STABILITYSTUDYID", "Shear", "TIMEPOINT", "Labvalue")],
               PH_5[, c("STABILITYSTUDYID", "TIMEPOINT", "Labvalue")],
              by = c("STABILITYSTUDYID", "TIMEPOINT"),
              suffixes = c("_Rheology", "_PH10%"), all.x = T) %>%
  merge(
        Viscosity_5[, c("STABILITYSTUDYID", "TIMEPOINT", "Labvalue")],
        by = c("STABILITYSTUDYID","TIMEPOINT"), all.x = T) %>%
  merge(
    enzyme_5[, c("STABILITYSTUDYID", "TIMEPOINT", "Labvalue", "methodID", 
                 "Activity18months", "Activity15months")],
    by = c("STABILITYSTUDYID","TIMEPOINT"),
    suffixes = c("_ViscosityB", "_Enzyme"), all.x = T) %>%
  merge(
    Arctic_5[, c("STABILITYSTUDYID", "TIMEPOINT", "Labvalue",
                 "Activity18months", "Activity15months")],
    by = c("STABILITYSTUDYID","TIMEPOINT"),
    suffixes = c("_enzyme", "_Arctic"),
    all.x = T
  ) %>%
  merge(Odor_5[, c("STABILITYSTUDYID", "TIMEPOINT", "Labvalue")],
        by = c("STABILITYSTUDYID","TIMEPOINT"),
        suffixes = c("_Arctic", "_Odor"),
        all.x = T) %>%
  merge(
    Color_5[, c("STABILITYSTUDYID", "TIMEPOINT", "deltaE")],
    by = c("STABILITYSTUDYID","TIMEPOINT"),
    all.x = T
  ) %>%
  merge(
    Dioxane_5[, c("STABILITYSTUDYID", "TIMEPOINT", "deltadioxane")],
    by =  c("STABILITYSTUDYID","TIMEPOINT"),
    all.x = T
  ) %>%
  merge(
    Appearance_5[,c("STABILITYSTUDYID", "TIMEPOINT",
                    "initialappearance", "stabilityapp")],
    by =  c("STABILITYSTUDYID","TIMEPOINT"),
    all.x = T
  )




## all = T
res_2 <- merge(Rheology_5[, c("STABILITYSTUDYID", "Shear", "TIMEPOINT", "Labvalue")],
               PH_5[, c("STABILITYSTUDYID", "TIMEPOINT", "Labvalue")],
               by = c("STABILITYSTUDYID", "TIMEPOINT"),
               suffixes = c("_Rheology", "_PH10%"), all = T) %>%
  merge(
    Viscosity_5[, c("STABILITYSTUDYID", "TIMEPOINT", "Labvalue")],
    by = c("STABILITYSTUDYID","TIMEPOINT"), all = T) %>%
  merge(
    enzyme_5[, c("STABILITYSTUDYID", "TIMEPOINT", "Labvalue", "methodID",
                 "Activity18months", "Activity15months")],
    by = c("STABILITYSTUDYID","TIMEPOINT"),
    suffixes = c("_ViscosityB", "_Enzyme"), all = T) %>%
  merge(
    Arctic_5[, c("STABILITYSTUDYID", "TIMEPOINT", "Labvalue",
                 "Activity18months", "Activity15months")],
    by = c("STABILITYSTUDYID","TIMEPOINT"),
    suffixes = c("_enzyme", "_Arctic"),
    all = T
  ) %>%
  merge(Odor_5[, c("STABILITYSTUDYID", "TIMEPOINT", "Labvalue")],
        by = c("STABILITYSTUDYID","TIMEPOINT"),
        suffixes = c("_Arctic", "_Odor"),
        all.x = T) %>%
  merge(
    Color_5[, c("STABILITYSTUDYID", "TIMEPOINT", "deltaE")],
    by = c("STABILITYSTUDYID","TIMEPOINT"),
    all = T
  ) %>%
  merge(
    Dioxane_5[, c("STABILITYSTUDYID", "TIMEPOINT", "deltadioxane")],
    by =  c("STABILITYSTUDYID","TIMEPOINT"),
    all = T
  ) %>%
  merge(
    Appearance_5[,c("STABILITYSTUDYID", "TIMEPOINT",
                    "initialappearance", "stabilityapp")],
    by =  c("STABILITYSTUDYID","TIMEPOINT"),
    all = T
  )

###
# res_3 <- merge(Rheology[, c("STABILITYSTUDYID", "Shear", "TIMEPOINT", "Labvalue")],
#                PH_5[, c("STABILITYSTUDYID", "TIMEPOINT", "Labvalue")],
#                by = c("STABILITYSTUDYID", "TIMEPOINT"),
#                suffixes = c("_Rheology", "_PH10%"), all.x = T) %>%
#   merge(
#     Viscosity[, c("STABILITYSTUDYID", "TIMEPOINT", "Labvalue")],
#     by = c("STABILITYSTUDYID","TIMEPOINT"), all.x = T) %>%
#   merge(
#     rbind(Vitalenz_P290, Xyloglucanase, Mannanase, Preferenz_P283, Preferenz_P280)[, c("STABILITYSTUDYID", "TIMEPOINT", "Labvalue", "methodID", 
#                  "Activity18months", "Activity15months")],
#     by = c("STABILITYSTUDYID","TIMEPOINT"),
#     suffixes = c("_ViscosityB", "_Enzyme"), all.x = T) %>%
#   merge(
#     Arctic[, c("STABILITYSTUDYID", "TIMEPOINT", "Labvalue",
#                  "Activity18months", "Activity15months")],
#     by = c("STABILITYSTUDYID","TIMEPOINT"),
#     suffixes = c("_enzyme", "_Arctic"),
#     all.x = T
#   ) %>%
#   merge(Odor[, c("STABILITYSTUDYID", "TIMEPOINT", "Labvalue")],
#         by = c("STABILITYSTUDYID","TIMEPOINT"),
#         suffixes = c("_Arctic", "_Odor"),
#         all.x = T) %>%
#   merge(
#     Color[, c("STABILITYSTUDYID", "TIMEPOINT", "deltaE")],
#     by = c("STABILITYSTUDYID","TIMEPOINT"),
#     all.x = T
#   ) %>%
#   merge(
#     Dioxane[, c("STABILITYSTUDYID", "TIMEPOINT", "deltadioxane")],
#     by =  c("STABILITYSTUDYID","TIMEPOINT"),
#     all.x = T
#   ) %>%
#   merge(
#     Appearance[,c("STABILITYSTUDYID", "TIMEPOINT",
#                     "initialappearance", "stabilityapp")],
#     by =  c("STABILITYSTUDYID","TIMEPOINT"),
#     all.x = T
#   )

# res_2 <- dcast(res_1,
#                STABILITYSTUDYID + Shear + `Labvalue_PH10%` + Labvalue_ViscosityB +
#                  Labvalue_Enzyme + methodID + Activity18months_enzyme + 
#                  Activity15months_enzyme + Labvalue_Arctic + Activity18months_Arctic +
#                  Activity15months_Arctic + Labvalue_Odor + deltaE + deltadioxane +
#                  initialappearance + stabilityapp ~ TIMEPOINT,
#                value.var = "Labvalue_Rheology") %>%
#   



ggplot(res_1, aes(x = Activity15months_enzyme, y = Activity15months_Arctic )) +
        geom_point() 


ggplot(Appearance_5, aes(x = initialappearance, fill = stabilityapp)) + 
  geom_bar(position = "fill") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, color = "black"))
## delta value


## 补全空值算法 ----

batch_ids <- rbind(
  Rheology[,c(1,4)],
  enzyme[, c(1,4)],
  PH[, c(1,4)],
  Viscosity[, c(1,4)],
  Arctic[, c(1,4)],
  Color[, c(1,4)],
  Appearance[, c(1,4)]
  ) %>%
  distinct(batchid)

batch_id <- df %>% distinct(batchid, .keep_all = F)


write.csv(batch_id, "batch_id_stability.csv", row.names = F)
