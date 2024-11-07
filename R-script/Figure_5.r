##############################Dx SNP distribution###########################################
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

setwd("F:/HMW/35-Dx-Dy-calling-SNP/02-5k-Dx-5k&5k-Dy-5k")

# 读取SNP位置文件
snp_positions <- read.csv("Dx-hap-pos.csv", header = T)

# 确保Position列是数值型
snp_positions <- snp_positions %>%
  mutate(across(everything(), as.numeric))

# 创建一个数据框，表示12K基因序列的矩形
gene_sequence1 <- data.frame(
  Position = 1:4999
)

gene_sequence2 <- data.frame(
  Position = 5000:7565
)

gene_sequence3 <- data.frame(
  Position = 7566:12565
)
# 将数据转换为长格式，便于分面绘图
snp_long <- snp_positions %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "Position")%>%
  filter(!is.na(Position))


# 自定义列的顺序
custom_order <- c("hap3","hap9","hap2","hap1","hap8","hap5","hap6","hap7","hap4")
snp_long$Column <- factor(snp_long$Column, levels = custom_order)

# 使用ggplot2绘制图形
ggplot() +
  # 绘制表示基因序列的矩形
  geom_rect(data = gene_sequence1, aes(xmin = Position - 0.5, xmax = Position + 0.5, ymin = 0, ymax = 0.9), fill = "white", color = "grey90") +
  #加深基因区的颜色
  geom_rect(data = gene_sequence2, aes(xmin = Position - 0.5, xmax = Position + 0.5, ymin = 0, ymax = 0.9), fill = "white", color = "grey80") +
  geom_rect(data = gene_sequence3, aes(xmin = Position - 0.5, xmax = Position + 0.5, ymin = 0, ymax = 0.9), fill = "white", color = "grey90") +
  # 在SNP位置上绘制紫色条形
  geom_rect(data = snp_long, aes(xmin = Position - 5, xmax = Position + 5, ymin = 0, ymax = 0.9), fill = "purple") +
  # 添加标签
  geom_text(data = snp_long %>% group_by(Column) %>% summarise(Position = max(Position)), 
            aes(x = -10, y = 0.2, label = Column), hjust = 1, vjust = 0.5, size = 3) +
  # 设置坐标轴范围和标签
  scale_x_continuous(name = "Position in Gene Sequence (bp)", limits = c(-100, 12565), expand = c(0, 0),breaks = seq(0, 12565, by = 500)) +
  scale_y_continuous(name = "Hap", limits = c(0, 1), expand = c(0, 0)) +
  # 分面绘图
  facet_wrap(~ Column, ncol = 1) +
  # 设置主题
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "black", size = 0.5),  # 显示x轴刻度线
    panel.grid.major = element_blank(),  # 取消主要网格线
    panel.grid.minor = element_blank(),  # 取消次要网格线
    strip.background = element_blank(),  # 取消分面标签背景
    strip.text.x = element_blank(),  # 取消分面标签文本
    axis.line.x = element_line(color = "black", size = 0.5)
  )



##############################Dy SNP distribution###########################################
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

# 读取SNP位置文件
snp_positions <- read.csv("Dy-hap-pos.csv", header = T)

# 确保Position列是数值型
snp_positions <- snp_positions %>%
  mutate(across(everything(), as.numeric))

# 创建一个数据框，表示12K基因序列的矩形
gene_sequence1 <- data.frame(
  Position = 1:4999
)

gene_sequence2 <- data.frame(
  Position = 5000:7565
)

gene_sequence3 <- data.frame(
  Position = 7566:12565
)
# 将数据转换为长格式，便于分面绘图
snp_long <- snp_positions %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "Position")%>%
  filter(!is.na(Position))

# 自定义列的顺序
custom_order <- c("hap3","hap9","hap2","hap1","hap8","hap5","hap6","hap7","hap4")
snp_long$Column <- factor(snp_long$Column, levels = custom_order)

# 使用ggplot2绘制图形
ggplot() +
  # 绘制表示基因序列的矩形
  geom_rect(data = gene_sequence1, aes(xmin = Position - 0.5, xmax = Position + 0.5, ymin = 0, ymax = 0.9), fill = "white", color = "grey90") +
  #加深基因区的颜色
  geom_rect(data = gene_sequence2, aes(xmin = Position - 0.5, xmax = Position + 0.5, ymin = 0, ymax = 0.9), fill = "white", color = "grey80") +
  geom_rect(data = gene_sequence3, aes(xmin = Position - 0.5, xmax = Position + 0.5, ymin = 0, ymax = 0.9), fill = "white", color = "grey90") +
  # 在SNP位置上绘制紫色条形
  geom_rect(data = snp_long, aes(xmin = Position - 5, xmax = Position + 5, ymin = 0, ymax = 0.9), fill = "purple") +
  # 添加标签
  geom_text(data = snp_long %>% group_by(Column) %>% summarise(Position = max(Position)), 
            aes(x = -10, y = 0.2, label = Column), hjust = 1, vjust = 0.5, size = 3) +
  # 设置坐标轴范围和标签
  scale_x_continuous(name = "Position in Gene Sequence (bp)", limits = c(-100, 12565), expand = c(0, 0),breaks = seq(0, 12565, by = 500)) +
  scale_y_continuous(name = "Hap", limits = c(0, 1), expand = c(0, 0)) +
  # 分面绘图
  facet_wrap(~ Column, ncol = 1) +
  # 设置主题
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "black", size = 0.5),  # 显示x轴刻度线
    panel.grid.major = element_blank(),  # 取消主要网格线
    panel.grid.minor = element_blank(),  # 取消次要网格线
    strip.background = element_blank(),  # 取消分面标签背景
    strip.text.x = element_blank(),  # 取消分面标签文本
    axis.line.x = element_line(color = "black", size = 0.5)
  )
