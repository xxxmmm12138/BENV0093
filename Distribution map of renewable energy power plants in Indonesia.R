# 安装必要的包
install.packages("readxl")
install.packages("ggplot2")
install.packages("sf")
install.packages("ggrepel")

# 加载库
library(readxl)
library(ggplot2)
library(sf)
library(ggrepel)

# 导入数据
data <- read_excel("C:/Users/于嘉曦/Desktop/renewable.xlsx")

# 转换经纬度为数值型
data$Longitude <- as.numeric(data$longitude)
data$Latitude <- as.numeric(data$latitude)

# 检查数据结构
str(data)

# 加载 Shapefile
indonesia_map <- st_read("C:/Users/于嘉曦/Desktop/path_to_your_fileindonesia_boundary.shp")

# 检查地图数据
print(indonesia_map)

# 绘制地图
ggplot() +
  geom_sf(data = indonesia_map, fill = "gray90", color = "gray50") +  # 绘制印尼边界
  geom_point(data = data, aes(x = longitude, y = latitude, 
                              size = capacity_mw, color = type,shape = status), alpha = 0.8) + 
  scale_size(range = c(2, 8), name = "Capacity (MW)") +  # 调整点大小范围
  scale_color_manual(values = c("Hydro" = "green", "solar" = "blue", "Geothermal" = "orange","wind"="purple")) + 
  scale_shape_manual(values = c("existing" = 16, "planned" = 17, "shelved" = 15)) + # 状态形状
  theme_minimal() +
  labs(title = "Distribution map of renewable energy power plants in Indonesia",
       x = "Longitude", y = "Latitude") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggrepel::geom_text_repel(data = data, aes(x = longitude, y = latitude, label = plant),
                           size = 3, max.overlaps = 10)
