#加载需要的包
library(tidyr)
library(dplyr)
library(ggplot2)
#1.数据清洗与整理

#载入数据，数据不准确，且有大量丢失
DXYArea <- read.csv("D:/Chrome downloads/DXYArea.csv", na = "NA",encoding = "UTF-8")
#转tibble表
DXYArea<-as_tibble(DXYArea)

#对数据执行一系列操作
nation <- DXYArea %>% 
  #从全球数据筛选中国数据，筛选确诊，治愈，死亡数据
  filter(countryName=="中国") %>% 
  select(updateTime, provinceName, cityName,city_confirmedCount,
         city_curedCount,city_deadCount) %>% 
  #每天有好几个数据，通过切片得到日期和时分秒，
  mutate(Date=substr(updateTime,1,10)) %>% 
  mutate(time= as.numeric(paste(substr(updateTime,12,13),
         substr(updateTime,15,16),substr(updateTime,18,19),sep = ""))) %>% 
  #以日期省市为分组，筛选最大时分秒，以此得到每日最新数据
  group_by(Date, provinceName, cityName) %>%
  filter(time == max(time)) %>% 
  #剔除国内没有数据的日期
  filter(Date!="2020-01-22" ) %>% 
  filter(Date!="2020-01-23" ) %>%
  #再以日期为组，将各省市数据加起来得到全国数据
  group_by(Date) %>% 
  summarise(confirmedCount = sum(city_confirmedCount,na.rm=TRUE), 
            curedCount = sum(city_curedCount,na.rm=TRUE), 
            deadCount = sum(city_deadCount,na.rm=TRUE)) %>% 
  #数据不完整，只有前39行较准确
  head(39)

province <- DXYArea %>% 
  #从全球数据筛选中国数据，筛选确诊，治愈，死亡数据
  filter(countryName=="中国") %>% 
  select(updateTime, provinceName, cityName,city_confirmedCount,city_curedCount
         ,city_deadCount) %>% 
  #每天有好几个数据，通过切片得到日期和时分秒，
  mutate(Date=substr(updateTime,1,10)) %>% 
  mutate(time= as.numeric(paste(substr(updateTime,12,13),substr(updateTime,15,16),
                                substr(updateTime,18,19),sep = ""))) %>% 
  #以日期省市为分组，筛选最大时分秒，以此得到每日最新数据
  group_by(Date, provinceName, cityName) %>%
  filter(time == max(time)) %>% 
  #选择数据较完整的最新日期2020年2月23日
  filter(Date=="2020-02-23" ) %>%
  #再以省份为组，将各市数据加起来得到各省数据
  group_by(provinceName) %>% 
  summarise(confirmedCount = sum(city_confirmedCount,na.rm=TRUE), 
            curedCount = sum(city_curedCount,na.rm=TRUE), 
            deadCount = sum(city_deadCount,na.rm=TRUE)) %>% 
  mutate(level=1)

city <- DXYArea %>% 
  #从全球数据筛选中国数据，筛选确诊，治愈，死亡数据
  filter(countryName=="中国") %>% 
  select(updateTime, provinceName, cityName,city_confirmedCount,city_curedCount
         ,city_deadCount) %>% 
  #每天有好几个数据，通过切片得到日期和时分秒，
  mutate(Date=substr(updateTime,1,10)) %>% 
  mutate(time= as.numeric(paste(substr(updateTime,12,13),substr(updateTime,15,16),
                                substr(updateTime,18,19),sep = ""))) %>% 
  #以日期省市为分组，筛选最大时分秒，以此得到每日最新数据
  group_by(Date, provinceName, cityName) %>%
  filter(time == max(time)) %>% 
  #选择数据较完整的最新日期2020年2月23日
  filter(Date=="2020-02-23" ) %>%
  filter(provinceName=="湖北省")
  
#手动添加台湾，香港数据
province$provinceName[27] <- "台湾省"
province$provinceName[30] <- "香港特别行政区"
province$confirmedCount[27] <- 25
province$confirmedCount[30] <- 52

#为各省确诊人数设置级别
attach(province)
province[confirmedCount <= 9, ]$level <- 1
province[confirmedCount > 9 & confirmedCount <= 99, ]$level <- 2
province[confirmedCount > 99 & confirmedCount <= 499, ]$level <- 3
province[confirmedCount > 499 & confirmedCount <= 999, ]$level <- 4
province[confirmedCount > 999 & confirmedCount <= 10000, ]$level <- 5
province[confirmedCount > 10000, ]$level <- 6
province<-province %>% 
  mutate(level = factor(level, ordered = TRUE))


#2.可视化

#全国数据 ：条形图
library(reshape2)
nation %>% 
  melt(id.vars = "Date", 
       measure.vars = c("confirmedCount", "curedCount", "deadCount")) %>% 
  ggplot() + 
  geom_bar(aes(x = Date, y = value, fill = variable, alpha = I(0.8)), 
           stat = "identity", position = "stack") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1)) + 
  labs(title = "全国疫情发展", x = "日期", y = "人数") + 
  guides(fill = guide_legend(title = NULL)) +
  scale_x_discrete(breaks = c("2020-01-24", "2020-02-08", 
                              "2020-02-20", "2020-03-02"))+
  scale_fill_discrete(labels = c("确诊数", "治愈数", "死亡数"))

#全国数据：wrap分开显示
nation %>% 
  melt(id.vars = "Date", 
       measure.vars = c("confirmedCount", "curedCount", "deadCount")) %>% 
  ggplot() + 
  geom_bar(aes(x = Date, y = value, fill = variable, alpha = I(0.8)), 
           stat = "identity", position = "dodge") + 
  facet_wrap(facets = ~ variable, scales = "free") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), 
        legend.position = "none") + 
  scale_x_discrete(breaks = c("2020-01-24", "2020-02-08", 
                              "2020-02-20", "2020-03-02")) + 
  labs(title = "疫情发展", x = "日期", y = "人数")

#全国数据：折线图
nation %>% 
  melt(id.vars = "Date", 
       measure.vars = c("confirmedCount", "curedCount", "deadCount")) %>% 
  ggplot() + 
  geom_point(aes(x = Date, y = value, group = variable, color = variable)) + 
  geom_line(aes(x = Date, y = value, group = variable, color = variable)) + 
  facet_wrap(facets = ~ variable, scales = "free") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none") + 
  scale_x_discrete(breaks = c("2020-01-24", "2020-02-08", 
                              "2020-02-20", "2020-03-02")) + 
  labs(title = "全国疫情", x = "日期", y = "人数")

#各省份确诊人数占比
province = province[order(province$confirmedCount, decreasing = TRUE),]
myLabel = as.vector(province$provinceName)   
myLabel = paste(myLabel, "(", round(province$confirmedCount / sum(province$confirmedCount) * 100, 2), "%)",
                sep = "")   
province %>% 
  ggplot() + 
  geom_bar(aes(x = 1, y = confirmedCount, fill = provinceName, color = I("grey40"),
               size = I(0.7), alpha = I(0.8)), stat = "identity") + 
  coord_polar(theta = "y") + 
  labs(x = '', y = '', title = '') + 
  theme(axis.text = element_blank(), axis.ticks = element_blank())+
  geom_text(stat="identity",aes(x=1,y=confirmedCount, label = myLabel))+
  scale_fill_discrete(breaks = province$provinceName, labels = myLabel)

#湖北各市确诊人数占比
city = city[order(city$city_confirmedCount, decreasing = TRUE),]
myLabel = as.vector(city$cityName)   
myLabel = paste(myLabel, "(", round(city$city_confirmedCount / sum(city$city_confirmedCount) * 100, 2), "%)",
                sep = "")   
city %>% 
  ggplot() + 
  geom_bar(aes(x = 1, y = city_confirmedCount, fill = cityName, color = I("grey40"),
               size = I(0.7), alpha = I(0.8)), stat = "identity") + 
  coord_polar(theta = "y") + 
  labs(x = '', y = '', title = '') + 
  theme(axis.text = element_blank(), axis.ticks = element_blank())+
  geom_text(stat="identity",aes(x=1,y=city_confirmedCount, label = myLabel))+
  scale_fill_discrete(breaks = city$cityName, labels = myLabel)

#画全国疫情地图

#加载相关的包
library(mapproj) 
library(maps)
library(sp)
library(rgdal)

#加载中国地图
China <- readOGR("China_map", "bou2_4p")
#获取省份与经纬度信息
region <- China@data
region <-data.frame(region, id = as.character(seq(0:924) - 1), stringsAsFactors = F)
region <- subset(region, id != 898)
china_map <- fortify(China)
#将省份，经纬度和疫情信息关联
china_map_data <- full_join(region, china_map, by = "id")
cov_data <- full_join(china_map_data, province, 
                         by = c("NAME" = "provinceName"))

#画出2月23号的疫情图
ggplot(cov_data, aes(x = long, y = lat, group = group, fill = factor(level))) + 
  geom_polygon(color = "grey40") + 
  scale_fill_manual(values = c("#ffddd3", "#fd987e", "#e94b2f", "#da0000", "#8b0000", "#4c221b"), 
          labels = c("0 - 9", "10 - 99", "100 - 499", "500 - 999", "1000 - 10000", "> 10000")) + 
  labs(title = "中国疫情图", x = "", y = "", fill = "", subtitle = "2020-02-23") + 
  coord_map("polyconic")+
  guides(fill = guide_legend(reverse = T)) + 
  theme(axis.text = element_blank(), axis.ticks = element_blank()) 


#3.时间序列分析与预测

library(forecast)

#筛选前45天较平稳符合实际的数据
day_45<-DXYArea %>% 
  #从全球数据筛选中国数据，筛选确诊，治愈，死亡数据
  filter(countryName=="中国") %>% 
  select(updateTime, provinceName, cityName,city_confirmedCount,city_curedCount
         ,city_deadCount) %>% 
  #每天有好几个数据，通过切片得到日期和时分秒，
  mutate(Date=substr(updateTime,1,10)) %>% 
  mutate(time= as.numeric(paste(substr(updateTime,12,13),substr(updateTime,15,16),
                                substr(updateTime,18,19),sep = ""))) %>% 
  #以日期省市为分组，筛选最大时分秒，以此得到每日最新数据
  group_by(Date, provinceName, cityName) %>%
  filter(time == max(time)) %>% 
  #剔除国内没有数据的日期
  filter(Date!="2020-01-22" ) %>% 
  filter(Date!="2020-01-23" ) %>%
  #再以日期为组，将各省市数据加起来得到全国数据
  group_by(Date) %>% 
  summarise(confirmedCount = sum(city_confirmedCount,na.rm=TRUE), 
            curedCount = sum(city_curedCount,na.rm=TRUE), 
            deadCount = sum(city_deadCount,na.rm=TRUE)) %>% 
  head(45)

#将确诊人数转化成时间序列
confirmed<-day_45$confirmedCount
confirmed_ts <- ts(confirmed)

#画出前45天确诊人数图
plot(confirmed_ts, type="l")

#建模,并进行acf、diff与pacf检验，
trModel <- lm(confirmed_ts ~ c(1:length(confirmed_ts)))
plot(resid(trModel), type="l")
plot(diff(resid(trModel)),type="l")
plot(diff(diff(resid(trModel))),type="l")
acf(resid(trModel))
pacf(resid(trModel))
#p=0，d并不平稳，q=1

#应用arima自动建模
model <- auto.arima(confirmed_ts)
#总览模型
summary(model)

#预测10天后
forecast_10 <- forecast(model, 10)
plot(forecast_10)
forecast_10$lower[10,2]
forecast_10$upper[10,2]
#预测270天后
forecast_270 <- forecast(model, 270)
plot(forecast_270)
forecast_270$lower[270,2]
forecast_270$upper[270,2]
#可以从结果看出，10和270天的预测基本水平，即控制住了疫情，但是置信区间特别大
#所以需要有更多更准确的数据才能完成较准确的预测（数据越多越准确是数据分析，预测的王道）