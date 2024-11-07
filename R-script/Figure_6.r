##########################################Figure_6a###################################
#加载绘图所需的包
library(ggtree)
library(ggplot2)
library(ape)
library (ggstance)
#设置工作路径

setwd("E:/HMW/33-align-result/all-Dy-scaffold-refseq-By8")
#打开nwk文件
tree <- read.tree("new-scaffold-Dy-refseq-and-By8-Align-without-value.nwk")

# 检查并处理 edge.length 中的 NA 值
#if (any(is.na(tree$edge.length))) {
#  tree$edge.length <- NULL
#}
#指定外群ID
outgroup <- c("AEV53351.1-By8")

# 使用 ape 包的 root 函数来指定外群
rooted_tree <- root(tree, outgroup = outgroup, resolve.root = TRUE)

#规定图例颜色
clade_colors <- c(
  "CladeL1E" = "#fCDFE5",
  "CladeL1W" = "#FFF8B3",
  "CladeL2E-1" = "#F1B7B3",
  "CladeL2E-2" = "#D3E2B7",
  "CladeL2W-1" = "#E2F0FF",
  "CladeL2W-2" = "#F0ECFB"
)
#绘图
#绘图
#pdf ("2024-10-24-Dx-scaffold-refseq-Bx7.pdf",width = 3.15,height = 3.15)
pdf ("2024-10-27-Dy-scaffold-refseq-Bx7-1.pdf",width = 3.15,height = 3.15)
ggtree(rooted_tree,
       layout = "circular", ## 'rectangular'为默认形状,"circular"为环形
       branch.length = "none", #不显示长度会使分支长度一致
       size = 0.2 #设置分支线条粗细
) +
  
  #添加分类标签及弧线
  geom_strip("T054-L1E-X", "ENT336-L2W-1", # label范围
             label = "Clade L1E", # 添加分组标签 
             offset = 18,  # 颜色条距离中心的相对位置，需要和xlim相互参照
             offset.text = 1, # 分组标签距离中心的相对位置
             barsize = 0.8,  # 调整颜色条的宽度
             extend = 0.4, # 调整颜色条间的距离，一般设置为0.5左右比较好
             fontsize = 1, # 调整分组标签的字体大小
             angle = 0, # 调整分组标签的旋转角度
             hjust = 1, # 调整分组标签的位置
             color = "#faa2c1" #颜色条和分组标签的颜色
  ) +
  geom_strip("T202-L1W", "T243-MIX", label = "Clade L1W", offset = 18,  offset.text = 1, 
             barsize = 0.8, extend = 0.4, fontsize = 1, 
             angle = 0, hjust = 1,
             color = "#EDB11A") +
  geom_strip ("AEV53351.1-By8", "T123-L2W-1", label = "Clade L2E-2", offset = 18,  offset.text = 1, 
              barsize = 0.8, extend = 0.4, 
              fontsize = 1,
              angle = 0, hjust = 0.7,
              color = "#41a96e") +
  geom_strip("AAU04841.1-Dy10.1-Tae", "T050-L2W-1", label = "Clade L2W-1", offset = 18,  offset.text = 1, 
             barsize = 0.8, extend = 0.4, 
             fontsize = 1,
             angle = 0, hjust = 0.5,
             color = "#2E59A7") +
  geom_strip("AHC72161.1-Dy-Tae","T191-L2E-1",label = "Clade L2E-1", offset = 18,  offset.text = 1, 
             barsize =0.8, extend = 0.4,fontsize = 1, 
             angle = 0, hjust = 1,
             color = "#c25160") +
  geom_strip("ACD37913.1-Dy12*-Tae", "Yao-Dy12-Tae", label = "Clade L2W-2", offset = 18,  offset.text = 1, 
             barsize = 0.8, extend = 0.4, fontsize = 1, 
             angle = 0, hjust = 1,
             color = "#A67EB7") +
  # xlim(0,40) +
  #将色条覆盖在基因ID上
  geom_strip("T054-L1E-X", "ENT336-L2W-1", # label范围
             #label = "Clade1-1", #添加分组标签 
             offset =9, # 颜色条距离中心的相对位置，需要和xlim相互参照
             #offset.text = 2, # 分组标签距离中心的相对位置
             barsize = 20, # 调整颜色条的宽度
             extend = 0.5, # 调整颜色条间的距离，一般设置为0.5左右比较好
             fontsize = 2, # 调整分组标签的字体大小
             angle = 0, # 调整分组标签的旋转角度
             #hjust = 1, #调整分组标签的位置
             color = clade_colors["CladeL1E"] #颜色条和分组标签的颜色
  ) +
  geom_strip("T202-L1W", "T243-MIX",  offset = 9, 
             barsize = 20, extend = 0.3, fontsize = 2, 
             angle = 0, 
             
             color = clade_colors["CladeL1W"]) +
  geom_strip("AHC72161.1-Dy-Tae","T191-L2E-1",offset = 9, 
             barsize = 20, extend = 0.3, fontsize = 2, 
             angle = 0,
             color = clade_colors["CladeL2E-1"]) +
  geom_strip("AAU04841.1-Dy10.1-Tae", "T050-L2W-1", offset = 9,  
             barsize = 20, extend = 0.3, fontsize = 2,
             angle = 0, 
             color = clade_colors["CladeL2W-1"]) +
  geom_strip( "ACD37913.1-Dy12*-Tae","Yao-Dy12-Tae", offset = 9,   
              barsize = 20, extend = 0.3, fontsize = 2,
              angle = 0, 
              color = clade_colors["CladeL2W-2"]) +
  geom_strip("AEV53351.1-By8", "T123-L2W-1",  offset = 9,  
             barsize = 20, extend = 0.3, fontsize = 2, 
             angle = 0, 
             color = clade_colors["CladeL2E-2"]) +
  
  geom_tiplab(color = "grey20", #修改基因ID的颜色
              hjust = -0.1, #调节基因ID的位置
              size =1.2, #修改基因ID大小
              align = F,
              offset = 1 #修改分支到基因ID的距离,
  ) +
  #将bootstrap value四舍五入为两位小数再乘以100变成整数
  geom_nodelab(aes(label = sprintf("%.0f", as.numeric(label)*100)), hjust = 1.5, vjust = -0.5, color = "darkblue", size = 0.5) 

dev.off()
##########################################Figure_6b###################################
library(trackViewer)
library(grid)
library(GenomicRanges)

# 读取数据
order_data <- read.delim("5k-Dx-5K-gene-pos-order.txt", sep = "\t", header = TRUE)

# 假设species列名为"species"
#species_id <- unique(data$species)

# 获取当前species的数据
#species_data <- data

# 获取当前species的order数据
species_order_data <- order_data

# 处理数据
#SNP <- species_data$pos
#Name <- species_data$codon
#Type <- species_data$type 
#sample.gr <- GRanges("AL878", IRanges(SNP, width=1, names=paste0("     ",SNP," " , Name)))

# 使用order.txt文件中的数据创建features
features <- GRanges("AL878r", IRanges(start = species_order_data$pos,
                                  width = species_order_data$width,
                                  names = species_order_data$names))

# 定义block的颜色
features$fill <- species_order_data$fill

# 自定义棒棒糖颜色
#sample.gr$color <- species_data$color
#sample.gr$border <- sample(c("black"), length(SNP), replace=TRUE)
#sample.gr$alpha <- sample(200:250, length(SNP), replace = TRUE)/255 # 设置sample.gr透明度
features$height <- species_order_data$height

# 设置节点位置
#sample.gr$SNPsideID <- species_data$y

# 设置线型
#sample.gr$lty <- species_data$lty

## shape must be "circle", "square", "diamond", "triangle_point_up", or "triangle_point_down"
#sample.gr$shape <- species_data$type
#sample.gr$legend <- paste0("legend", as.numeric(factor(sample.gr$shape)))

# 设置标题和横坐标轴标签
#title <- paste0(species_id)
xaxis <- c(0, 2500,5000,7500,10000)

# 绘制图表
lolliplot(sample.gr, features, xaxis = xaxis,# main = title, 
          #type = "circle", # 设置棒棒糖形状
          #cex = 1, # 设置棒棒糖节点大小
          #rescale = TRUE, # 一些节点移动得离原始坐标太远。要重新缩放，可以按如下方式重置 x 轴
          legend = legend,
          lty = "dashed" # 设置线型
)
