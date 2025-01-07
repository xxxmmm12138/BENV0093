install.packages(c("terra", "ncdf4", "ggplot2"))
library(terra)  # 用于处理地理栅格数据
library(ncdf4)  # 用于读取 NetCDF 文件
library(ggplot2)  # 用于可视化

# 导入 NetCDF 文件
ssrd_file <- "C:/Users/于嘉曦/Desktop/solar_data.nc"
ssrd_raster <- rast(ssrd_file)  # 读取为栅格对象

# 检查文件内容
print(ssrd_raster)

# 设置太阳能板参数
area <- 1.2*10^7       # 面积，单位：m²
yield_r <- 0.175   # 板效率
p <- 0.6  # 性能比
hours <- 1

# 定义太阳能发电厂的功率
radiation_to_power <- function(radiation, area, yield_r, p, hours) {
  power <- radiation * area * yield_r * p * (hours/3600) / 1000
  return(power)
}

# 对每个像素太阳能发电厂的功率
power_raster_KWh <- radiation_to_power(ssrd_raster, area, yield_r, p, hours)

print(power_raster_KWh)

# 转换为数据框
power_df <- as.data.frame(power_raster_KW, xy = TRUE)

colnames(power_df) <- c("Longitude", "Latitude", "Power")

# 使用 ggplot2 绘制地图
ggplot(data = power_df, aes(x = Longitude, y = Latitude, fill = Power)) +
  geom_raster() +
  scale_fill_viridis_c(name = "Power (kWh)") +
  labs(title = "Solar Power Distribution", x = "Longitude", y = "Latitude") +
  theme_minimal()


# 设置太阳能板参数
area <- 1.2e+7        # 面积，单位：m²
yield_r <- 0.175   # 板效率
p <- 0.6  # 性能比
hours <- 1
ssrd_raster <- 24593502

# 定义太阳能发电厂的发电量
radiation_to_energy <- function(radiation, area, yield_r, p, hours) {
  power <- radiation * area * yield_r * p * (hours/3600) / 1000
  return(power)
}

# 对每个像素太阳能发电厂的发电量
energy_raster_KWh <- radiation_to_energy(ssrd_raster, area, yield_r, p, hours)
#容量
energy_raster_KW <-energy_raster_KWh/1

print(energy_raster_KW)




