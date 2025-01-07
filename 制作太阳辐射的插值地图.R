install.packages(c("terra", "gstat", "sf", "tmap"))
library(terra)
library(gstat)
library(sf)
library(tmap)

# 1. 读取数据并转换为sf对象
data <- read.csv("C:/Users/于嘉曦/Desktop/ssrd1.csv")

set.seed(123)  # 设置随机种子，确保结果可重复
sampled_data <- data[sample(nrow(data), size = 1000), ]  # 随机抽取1000个点

# 2. 转换为sf对象，使用适合印度尼西亚区域的设置
library(sf)
points_sf <- st_as_sf(data, 
                      coords = c("longitude", "latitude"),
                      crs = 4326)

# 3. 创建栅格模板，使用更适合你研究区域的分辨率
library(raster)
raster_template <- raster(
  xmn = min(data$longitude) - 0.1, 
  xmx = max(data$longitude) + 0.1,
  ymn = min(data$latitude) - 0.1, 
  ymx = max(data$latitude) + 0.1,
  resolution = 0.05,  # 对于印度尼西亚这个区域可以用更细的分辨率
  crs = CRS("+proj=longlat +datum=WGS84")
)

# 4. IDW模型
library(gstat)
idw_model <- gstat(
  formula = ssrd ~ 1,
  locations = points_sf,
  nmax = 8  # 增加了搜索点数，因为是沿海区域
)

# 5. 插值
interpolated_raster <- interpolate(raster_template, idw_model)

# 6. 绘图
library(tmap)
tmap_mode("view")  # 设置为交互模式
tm_shape(interpolated_raster) +
  tm_raster(style = "cont", 
            palette = "-Blues",  # 或者用 "-RdYlGn" 等其他配色
            title = "Solar Radiation") +
  tm_shape(points_sf) +
  tm_dots(size = 0.01, col = "red", alpha = 0.5) +
  tm_layout(legend.position = c("right", "bottom"))

library(gstat)
library(sf)

# 设置参数
n_idp <- 20  # 要验证的 idp 参数范围，从 1 到 20
n_fold <- 10  # 交叉验证的折数
set.seed(7713)  # 固定随机种子，确保结果可重复

# 数据准备
kf <- sample(rep(1:n_fold, length.out = nrow(points_sf)))  # 创建折数分组
va <- data.frame(idp = 1:n_idp, rmse = NA)  # 初始化结果存储表

# 定义 RMSE 函数
RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm = TRUE))
}

# K 折交叉验证
for (j in 1:n_idp) {  # 遍历 idp 参数
  rmse <- numeric(n_fold)  # 存储每折的 RMSE 值
  
  for (i in 1:n_fold) {  # 遍历每一折
    # 划分训练集和测试集
    test <- points_sf[kf == i, ]  # 当前测试集
    train <- points_sf[kf != i, ]  # 当前训练集
    
    # 创建 IDW 模型
    gs <- gstat(formula = ssrd ~ 1, locations = train, nmax = Inf, set = list(idp = j))
    
    # 对测试集进行预测
    predictions <- predict(gs, test)
    
    # 计算 RMSE
    rmse[i] <- RMSE(test$ssrd, predictions$var1.pred)
  }
  
  # 计算当前 idp 的平均 RMSE
  va$rmse[j] <- mean(rmse)
}

# 输出结果
print(va)

# 可视化 RMSE 随 idp 变化的趋势
plot(va$idp, va$rmse, type = "b", col = "blue", pch = 19,
     xlab = "IDW Power Parameter (idp)",
     ylab = "Mean RMSE",
     main = "RMSE vs IDP (Power Parameter)")


