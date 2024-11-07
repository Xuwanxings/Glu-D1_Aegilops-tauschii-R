########################Dx-Dy-len##############################################
library(ggplot2)
library(dplyr)
library(ggbreak)
setwd("E:/HMW/64-draw-picture-for-paper/Figure2_len_PI散点图")
data <- read.table("Dx-Dy-PI.txt", header = TRUE, sep = "\t")
order <- read.delim("order.txt", sep="\t", header = F)
data$seq_name <- factor(data$seq_name, levels = order$V1)
# 创建两个数据框，分别表示Dx-len和Dy-len
data_dx <- data %>%
  select(seq_name, Clade, len = Dx.PI) %>%
  mutate(type = "Dx-PI")

data_dy <- data %>%
  select(seq_name, Clade, len = Dy.PI) %>%
  mutate(type = "Dy-PI")
clade_colors <- c("L1E" = "#faa2c1",
                  "L1W" = "#EDB11A",
                  "L2E-1" = "#c25160",
                  "L2E-2" = "#41a96e",
                  "L2W-1" = "#2E59A7",
                  "L2W-2" = "#A67EB7")
# 合并数据框
data_combined <- bind_rows(data_dx, data_dy)
pdf("Dx-Dy-PI-5.pdf",width=6.29,height = 3)
ggplot(data_combined, aes(x = seq_name, y = len, color = Clade, shape = type)) +
  geom_point(size = 1.5) +
  scale_color_manual(values = clade_colors) +
  scale_shape_manual(values = c("Dx-PI" = 17, "Dy-PI" = 19)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8),
        #axis.text.x = element_text(size = 8),  # 调整x轴字体大小
        axis.text.y = element_text(size = 8),  # 调整y轴字体大小
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y =element_blank(),
        plot.title = element_text(hjust = 0.5)# 将标题居中对齐 
  )+
  labs(title = "Scatter Plot of len by seq_name and Clade",
       x = "seq_name",
       y = "len",
       color = "Clade",
       shape = "Type")+
  ylim(5,8)
dev.off()

##################################The PI of Dx and Dy########################################
data <- read.table("Dx-Dy-PI.txt", header = TRUE, sep = "\t")
order <- read.delim("order.txt", sep="\t", header = F)
data$seq_name <- factor(data$seq_name, levels = order$V1)
# 创建两个数据框，分别表示Dx-len和Dy-len
data_dx <- data %>%
  select(seq_name, Clade, len = Dx.PI) %>%
  mutate(type = "Dx-PI")

data_dy <- data %>%
  select(seq_name, Clade, len = Dy.PI) %>%
  mutate(type = "Dy-PI")
clade_colors <- c("L1E" = "#faa2c1",
                  "L1W" = "#EDB11A",
                  "L2E-1" = "#c25160",
                  "L2E-2" = "#41a96e",
                  "L2W-1" = "#2E59A7",
                  "L2W-2" = "#A67EB7")
# 合并数据框
data_combined <- bind_rows(data_dx, data_dy)

background_colors <- data.frame(
  Clade = c("L1E", "L1W", "L2E-1", "L2E-2", "L2W-1", "L2W-2"),
  ymin = 5,  # y 轴的下限
  ymax = 8,  # y 轴的上限
  xmin = -Inf,  # x 轴的下限
  xmax = Inf   # x 轴的上限
)

# 创建一个 ggplot
#pdf("Dx-Dy-PI-5.pdf", width = 6.29, height = 3)
ggplot(data_combined, aes(x =seq_name, y = len, color = Clade, shape = type)) +
  # 添加背景色
  geom_rect(data = background_colors, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Clade), alpha = 0.2) +
  geom_point(size = 1.5) +
  scale_color_manual(values = clade_colors) +
  scale_fill_manual(values = clade_colors) +  # 为填充颜色添加相同的颜色
  scale_shape_manual(values = c("Dx-PI" = 17, "Dy-PI" = 19)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Scatter Plot of len by seq_name and Clade",
       x = "seq_name",
       y = "len",
       color = "Clade",
       shape = "Type") +
  ylim(5, 8)
