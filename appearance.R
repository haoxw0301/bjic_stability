library(dplyr)

Appearance_2 <- Appearance %>%
    group_by(STABILITYSTUDYID, batchid,conditionset,stabilityapp) %>%
    summarise(min_time = min(TIMEPOINT),
              max_time = max(TIMEPOINT))
# %>%
#     group_by(STABILITYSTUDYID, batchid,conditionset, max_time) %>%
#     summarise(max_time = max(max_time)) %>%
#   merge(Appearance[, c("STABILITYSTUDYID","batchid", "conditionset", "TIMEPOINT", "stabilityapp")],
#         by.x = c("STABILITYSTUDYID","batchid", "conditionset","max_time"),
#         by.y = c("STABILITYSTUDYID","batchid", "conditionset", "TIMEPOINT")) 
# 

Appearance_2_P <- Appearance_2 %>%
  filter(stabilityapp == "PASS" & max_time >= 72)
Appearance_2_F <- Appearance_2 %>%
  filter(! batchid %in% Appearance_2_P$batchid  &
           stabilityapp == "FAIL" & 
           min_time >=0)


Appearance_2 <- rbind(Appearance_2_P, Appearance_2_F)

plot_theme(
  ggplot(Appearance_2, aes(x = STABILITYSTUDYID, color = stabilityapp)) + 
    geom_bar(position = "fill") + 
    theme_bw(base_rect_size = 1.3) +
    theme(
      axis.text.x = element_blank()
    )
)

write.csv(Appearance_2_F, "Failed_experiments_week72.csv")
write.csv(Appearance_2_P, "PASS_experiments_week72.csv")
