###################The amnio acid classify of Dx########################
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggbreak)
# 读取数据
data_wide <- read.table("Dx-Aet-aa-classify.txt", header = TRUE, sep = "\t")

# 宽数据转换为长数据
data_long <- gather(data_wide, key = "Age_Category", value = "Age", -Clade)

# 数据因子化 调整顺序 
data_long$Age_Category <- factor(data_long$Age_Category,
                                 levels = c('Acidic', 'Basic', 'Hydrophobic', 'Hydrophilic'))

data_long$ID <- factor(data_long$Clade,
                       levels = c("L1E", "L1W", "L2E-1", "L2E-2", "L2W-1", "L2W-2"))

# 计算每个 Age_Category 和 ID 组合的平均值和标准差
summary_data <- data_long %>%
  group_by(Age_Category, ID) %>%
  summarise(
    mean_age = mean(Age, na.rm = TRUE),
    sd_age = sd(Age, na.rm = TRUE),
    .groups = 'drop'
  )

# 绘制图形
ggplot(data = summary_data, aes(x = Age_Category, y = mean_age, fill = ID,)) +
  geom_col(position = position_dodge(0.8), width = 0.8, show.legend = F, color = "black",linewidth = 0.3) +
  geom_errorbar(aes(ymin = mean_age - sd_age, ymax = mean_age + sd_age),
                position = position_dodge(0.8), width = 0.6, linewidth = 0.3) +
  #geom_jitter(data = data_long, aes(x = Age_Category, y = Age, color = ID), 
  #            position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), 
  #            size = 2, alpha = 0.5, color = "grey") +
  labs(x = "Dy Amino acid classify", y = "Number",size =1) +
  scale_fill_manual(values = c("L1W" = "#CBB47B", "L1E" = "#CA8687", "L2E-1" = "#c55659", 
                               "L2E-2" = "#54ac75", "L2W-1" = "#4878b9", "L2W-2" = "#7572b5")) +
  scale_y_break(breaks = c(40, 260), space = 0.1) +  # 添加断点
  coord_cartesian(ylim = c(0, 430)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 30,50, 260, 300, 340,380,420)) +
  theme_bw() +
  theme(legend.direction = "vertical",
        #legend.position = "",  # 设置图例位置为左上角
        #legend.justification = c(0, 1),  # 确保图例从左上角开始
        legend.title = element_text(colour = "white",size = 8, family = "Helvetica"),
                                    axis.text.x = element_text(size = 8, family = "Helvetica"),  # 调整x轴字体大小
                                    axis.text.y = element_text(size = 8, family = "Helvetica"),  # 调整y轴字体大小
                                    axis.title.x = element_text(size = 8, family = "Helvetica"),  # 调整x轴标题字体大小
                                    axis.title.y = element_text(size = 8, family = "Helvetica"),  # 调整y轴标题字体大小
                                    plot.title = element_text(size = 8, family = "Helvetica"),    # 调整标题字体大小
                                    panel.grid.minor.x = element_blank(),
                                    panel.grid.minor.y = element_blank(),
                                    panel.grid.major.x = element_blank(),
                                    panel.grid.major.y = element_blank()
                                    #legend.text = element_text(size = 8, family = "Helvetica")  # 调整图例字体大小
        )

###################The amnio acid classify of Dy########################

library(ggplot2)
library(tidyr)
library(dplyr)
library(ggbreak)
# 读取数据
data_wide <- read.table("Dy-Aet-aa-classify.txt", header = TRUE, sep = "\t")

# 宽数据转换为长数据
data_long <- gather(data_wide, key = "Age_Category", value = "Age", -Clade)

# 数据因子化 调整顺序 
data_long$Age_Category <- factor(data_long$Age_Category,
                                 levels = c('Acidic', 'Basic', 'Hydrophobic', 'Hydrophilic'))

data_long$ID <- factor(data_long$Clade,
                       levels = c("L1E", "L1W", "L2E-1", "L2E-2", "L2W-1", "L2W-2"))

# 计算每个 Age_Category 和 ID 组合的平均值和标准差
summary_data <- data_long %>%
  group_by(Age_Category, ID) %>%
  summarise(
    mean_age = mean(Age, na.rm = TRUE),
    sd_age = sd(Age, na.rm = TRUE),
    .groups = 'drop'
  )


# 绘制图形
ggplot(data = summary_data, aes(x = Age_Category, y = mean_age, fill = ID,)) +
  geom_col(position = position_dodge(0.8), width = 0.8, show.legend = F, color = "black",linewidth = 0.3) +
  geom_errorbar(aes(ymin = mean_age - sd_age, ymax = mean_age + sd_age),
                position = position_dodge(0.8), width = 0.6, linewidth = 0.3) +
  #geom_jitter(data = data_long, aes(x = Age_Category, y = Age, color = ID), 
  #            position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), 
  #            size = 2, alpha = 0.5, color = "grey") +
  labs(x = "Dy Amino acid classify", y = "Number",size =1) +
  scale_fill_manual(values = c("L1W" = "#CBB47B", "L1E" = "#CA8687", "L2E-1" = "#c55659", 
                               "L2E-2" = "#54ac75", "L2W-1" = "#4878b9", "L2W-2" = "#7572b5")) +
  scale_y_break(breaks = c(40, 260), space = 0.1) +  # 添加断点
  coord_cartesian(ylim = c(0, 430)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 30,50, 260, 300, 340,380,420)) +
  theme_bw() +
  theme(legend.direction = "vertical",
        #legend.position = "",  # 设置图例位置为左上角
        #legend.justification = c(0, 1),  # 确保图例从左上角开始
        legend.title = element_text(colour = "white",size = 8, family = "Helvetica"),
                                    axis.text.x = element_text(size = 8, family = "Helvetica"),  # 调整x轴字体大小
                                    axis.text.y = element_text(size = 8, family = "Helvetica"),  # 调整y轴字体大小
                                    axis.title.x = element_text(size = 8, family = "Helvetica"),  # 调整x轴标题字体大小
                                    axis.title.y = element_text(size = 8, family = "Helvetica"),  # 调整y轴标题字体大小
                                    plot.title = element_text(size = 8, family = "Helvetica"),    # 调整标题字体大小
                                    panel.grid.minor.x = element_blank(),
                                    panel.grid.minor.y = element_blank(),
                                    panel.grid.major.x = element_blank(),
                                    panel.grid.major.y = element_blank()
                                    #legend.text = element_text(size = 8, family = "Helvetica")  # 调整图例字体大小
        )


#############################The celiac number of Dx#################
library(ggplot2)
library(multcompView)
library(dplyr)

# 读取数据
data <- read.table("2024-10-20-Dx-Tae-Aet.txt", header = TRUE, stringsAsFactors = TRUE)

# 替换 Clade 列中的连字符为下划线
data$Clade <- gsub("-", "_", data$Clade)

# 计算每个 Clade 的均值和标准差
summary_data <- data %>%
  group_by(Clade) %>%
  summarise(
    Mean = mean(Dx, na.rm = TRUE),  # 添加 na.rm 参数以处理缺失值
    SD = sd(Dx, na.rm = TRUE)
  )

# 进行 ANOVA 分析
model <- aov(Dx ~ Clade, data = data)

# 进行 Tukey HSD 事后检验
tukey_result <- TukeyHSD(model)

# 提取 Tukey HSD 的结果并转换为数据框
tukey_letters <- multcompLetters(tukey_result$Clade[, "p adj"])
tukey_letters <- as.data.frame(tukey_letters$Letters)
colnames(tukey_letters) <- "Letters"

# 将 Clade 列的名称添加到 tukey_letters 中
tukey_letters$Clade <- rownames(tukey_letters)

# 合并均值、标准差和 Tukey HSD 结果
summary_data <- merge(summary_data, tukey_letters, by = "Clade")

# 检查 summary_data
print(summary_data)

pdf("20241002-Dx-乳糜泻.pdf",width  =8)
# 绘制图表
ggplot(summary_data, aes(x = Clade, y = Mean, fill = Clade)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = Letters, y = Mean + SD + 2), position = position_dodge(0.9), vjust = 0, size = 6) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  scale_fill_manual(values = c(
    "L1W" = "#CBB47B",
    "L1E" = "#CA8687",
    "L2E_1" = "#c55659",
    "L2E_2" = "#54ac75",
    "L2W_1" = "#4878b9",
    "L2W_2" = "#7572b5",
    "Tae_L2E_1" = "#768696",
    "Tae_L2W-2" = "#3DA6AE")) +
  labs(title = " ", x = "Clade", y = "The Number of Dx celiac", size = 10) +
  scale_y_continuous(breaks = c(0, 25, 50,75,100), limits = c(0, 100))

#######################The celiac number of Dy####################################
library(ggplot2)
library(multcompView)
library(dplyr)

# 读取数据
data <- read.table("2024-10-20-Dy-Tae-Aet.txt", header = TRUE, stringsAsFactors = TRUE)

# 替换 Clade 列中的连字符为下划线
data$Clade <- gsub("-", "_", data$Clade)

# 计算每个 Clade 的均值和标准差
summary_data <- data %>%
  group_by(Clade) %>%
  summarise(
    Mean = mean(Dy, na.rm = TRUE),  # 添加 na.rm 参数以处理缺失值
    SD = sd(Dy, na.rm = TRUE)
  )

# 进行 ANOVA 分析
model <- aov(Dy ~ Clade, data = data)

# 进行 Tukey HSD 事后检验
tukey_result <- TukeyHSD(model)

# 提取 Tukey HSD 的结果并转换为数据框
tukey_letters <- multcompLetters(tukey_result$Clade[, "p adj"])
tukey_letters <- as.data.frame(tukey_letters$Letters)
colnames(tukey_letters) <- "Letters"

# 将 Clade 列的名称添加到 tukey_letters 中
tukey_letters$Clade <- rownames(tukey_letters)

# 合并均值、标准差和 Tukey HSD 结果
summary_data <- merge(summary_data, tukey_letters, by = "Clade")

# 检查 summary_data
print(summary_data)

# 绘制图表
ggplot(summary_data, aes(x = Clade, y = Mean, fill = Clade)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = Letters, y = Mean + SD + 2), position = position_dodge(0.9), vjust = 0, size = 6) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  scale_fill_manual(values = c(
    "L1W" = "#CBB47B",
    "L1E" = "#CA8687",
    "L2E_1" = "#c55659",
    "L2E_2" = "#54ac75",
    "L2W_1" = "#4878b9",
    "L2W_2" = "#7572b5",
    "Tae_L2E_1" = "#768696",
    "Tae_L2W-2" = "#3DA6AE")) +
  labs(title = " ", x = "Clade", y = "The Number of Dy celiac", size = 10) +
  scale_y_continuous(breaks = c(0, 30, 50,75, 100), limits = c(0, 100))

