library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(openxlsx)
# load data ----
# data from Alberto

# df <- read.csv("download-stability-010724.csv",fill = "NA")
# df <- read.table("download-stability-010724.txt", header = T, sep= "\t", fill = "NA")
df <- read.xlsx("download-stability-010724.xlsx", sheet = 1)

## colnames modification 
colnames(df) <- gsub("_", "", colnames(df))

# removeRowsAllNA <- function(x){
#   x[apply(x, 1, function(y) any(!is.na(y))),]
# }
# removeColsAllNA <- function(x){
#   x[, apply(x, 2, function(y) any(! is.na(y)))]
# }
# 
# df_2 <- removeColsAllNA(df)

## statistics of raw data ----
cols <- colnames(df)
# 
# nonNArowNum <- function(df, colname){
#   rowNum <- length(df[which(df[,colname] != "NA"), colname])
#   uniq_rowNum <- length(
#     unique(df[which(df[,colname] != "NA"), colname])
#   )
#   return(list(colname = colname, rowNum = rowNum, uniq_rowNum = uniq_rowNum))
# }
# 
# res <- data.frame(
#   colname = "colname",
#   rowNum = 0,
#   uniq_rowNum = 0
# )
# for (i in 1:length(cols)){
#   t <- nonNArowNum(df, cols[i])
#   res <- rbind(res, data.frame(t))
# }
# res <- res[-1, ]
# 
# res$colname = factor(res$colname, levels = cols)
# 
# 
# p1 <- ggplot(res, aes(x = colname, y = rowNum, fill = colname)) + 
#   geom_col() +
#   theme_bw(base_rect_size = 1.3) + 
#   # scale_y_log10() + 
#   theme(
#     # axis.text.x = element_text(angle = 90,size = 10, color= "black"),
#     axis.text.x = element_blank(),
#     axis.text.y = element_text(size =15, color = "black"),
#     axis.title = element_text(size = 15),
#     axis.title.x = element_blank(),
#     legend.position = "none"
#   )
# p2 <- ggplot(res, aes(x = colname, y = uniq_rowNum, fill = colname)) + 
#   geom_col() +
#   theme_bw(base_rect_size = 1.3) + 
#   # scale_y_log10() + 
#   theme(
#     axis.text.x = element_text(angle = 90,size = 15, color= "black"),
#     axis.text.y = element_text(size =15, color = "black"),
#     axis.title = element_text(size = 15),
#     legend.position = "none"
#   )
# library(ggpubr)
# 
# ggarrange(p1, p2, nrow = 2, heights = c(1, 2))
# 
# ## method 
# 
# methods <- distinct(df, methodID)
# 
#  
# Rhe <- df[which(df$methodID == "Rheology"), ]
#   
# ## 去掉全是缺失值的列