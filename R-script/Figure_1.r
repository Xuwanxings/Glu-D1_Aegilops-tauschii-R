#加载绘图所需的包
library(ggtree)
library(ggplot2)
library(ape)
library (ggstance)

#打开nwk文件
tree <- read.tree("Figure1-Dy-outgroup-tree-without-value")

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
P<-ggtree(rooted_tree,
          layout = "circular", ## 'rectangular'为默认形状,"circular"为环形
          branch.length = "none", #不显示长度会使分支长度一致
          size = 0.2 #设置分支线条粗细
          
) +
  # geom_tippoint(color = "#0b5394", size = 1) +
  #添加分类标签及弧线
  geom_strip("T236-L1E-X", "T212-L1E-X", # label范围
             label = "Clade L1E", # 添加分组标签 
             offset = 11,  # 颜色条距离中心的相对位置，需要和xlim相互参照
             offset.text = 2, # 分组标签距离中心的相对位置
             barsize = 0.5,  # 调整颜色条的宽度
             extend = 0.4, # 调整颜色条间的距离，一般设置为0.5左右比较好
             fontsize = 1, # 调整分组标签的字体大小
             angle = 0, # 调整分组标签的旋转角度
             hjust = 0.5, # 调整分组标签的位置
             color = "#faa2c1" #颜色条和分组标签的颜色
  ) +
  geom_strip("T243-MIX", "T202-L1W", label = "Clade L1W", offset = 11,  offset.text = 2, 
             barsize = 0.5, extend = 0.4, fontsize = 1, angle = 0, hjust = 0.5,
             color = "#EDB11A") +
  geom_strip("TA2576-L3", "T142-L2E-2", label = "Clade L2E-2", offset = 11,  offset.text = 2, 
             barsize = 0.5, extend = 0.4, fontsize = 1, angle = 0.5, hjust = 1,
             color = "#41a96e") +
  geom_strip("TA1618-L2E-2", "T050-L2W-1", label = "Clade L2W-1", offset = 11,  offset.text = 2, 
             barsize = 0.5, extend = 0.4, fontsize = 1, angle = 0, hjust = 0,
             color = "#2E59A7") +
  geom_strip("T047-L2E-1", "RL5271-L2E-2", label = "Clade L2E-1", offset = 11,  offset.text = 2, 
             barsize = 0.5, extend = 0.4, fontsize = 1, angle = 0, hjust = 1,
             color = "#c25160") +
  geom_strip("T088-L2W-2",  "TA1675-L2W-2", label = "Clade L2W-2", offset = 11,  offset.text = 2, 
             barsize = 0.5, extend = 0.4, fontsize = 1, angle = 0, hjust = 0.5,
             color = "#A67EB7") +
  xlim(0,45) +#将色条覆盖在基因ID上
  geom_strip("T236-L1E-X", "T212-L1E-X", # label范围
             #label = "Clade1-1", #添加分组标签 
             offset = 5.5, # 颜色条距离中心的相对位置，需要和xlim相互参照
             #offset.text = 2, # 分组标签距离中心的相对位置
             barsize = 8, # 调整颜色条的宽度
             extend = 0.3, # 调整颜色条间的距离，一般设置为0.5左右比较好
             fontsize = 1, # 调整分组标签的字体大小
             angle = 0,# 调整分组标签的旋转角度
             #size = 0.1,
             #hjust = 1, #调整分组标签的位置
             color = clade_colors["CladeL1E"] #颜色条和分组标签的颜色
  ) +
  geom_strip("T243-MIX", "T202-L1W",  offset = 5.5, 
             barsize = 8, extend = 0.3, fontsize = 1, angle = 0, 
             color = clade_colors["CladeL1W"]) +
  geom_strip("T047-L2E-1", "RL5271-L2E-2",  offset = 5.5, 
             barsize = 8, extend = 0.3, fontsize = 1, angle = 0,
             color = clade_colors["CladeL2E-1"]) +
  geom_strip("TA1618-L2E-2", "T050-L2W-1", offset = 5.5,  
             barsize = 8, extend = 0.3, fontsize = 1, angle = 0, 
             color = clade_colors["CladeL2W-1"]) +
  geom_strip("T088-L2W-2", "TA1675-L2W-2", offset = 5.5,   
             barsize = 8, extend = 0.3, fontsize = 1, angle = 0, 
             color = clade_colors["CladeL2W-2"]) +
  geom_strip("TA2576-L3", "T142-L2E-2",  offset = 5.5,  
             barsize = 8, extend = 0.3, fontsize = 1, angle = 0, 
             color = clade_colors["CladeL2E-2"]) +
  
  geom_tiplab(color = "grey20", #修改基因ID的颜色
              hjust = 0, #调节基因ID的位置
              size = 0.8, #修改基因ID大小
              align = F,
              offset = 2,#修改分支到基因ID的距离,
              aes(angle=angel) #指定弧形所占角度
  )  +
  geom_nodelab(aes(label = sprintf("%.0f", as.numeric(label)*100)), hjust = 1.5, vjust = -0.5, color = "darkblue", size = 1.5) +
  theme(text = element_text(family = "Arial")) 
ggsave("2024-11-04-Figure1-Dy-outgroup-tree.pdf", plot = P, width = 4.52, height = 3.15,dpi = 600)
