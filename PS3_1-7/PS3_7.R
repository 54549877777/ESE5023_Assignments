library(tidyr)
library(dplyr)
library(ggplot2)

#1.t检验
#问题：水面和水底的大肠杆菌浓度是否一样？
waterdata <- read.csv(file = "water_quality.csv", header = T,encoding = "UTF-8")
water<-as_tibble(waterdata)
ecoli_surface<-water %>% 
  filter(Depth=="Surface Water") %>% 
  pull(E..coli..cfu.100mL.)
ecoli_surface<-as.numeric(ecoli_surface)
ecoli_bottom<-water %>% 
  filter(Depth=="Bottom Water") %>% 
  pull(E..coli..cfu.100mL.)
ecoli_bottom<-as.numeric(ecoli_bottom)
diff<-t.test(ecoli_bottom, ecoli_surface)
#根据结果可得，p值远小于0.05，所以有信心说两组之间存在统计差异
#即水面和水底的大肠杆菌浓度不一样。

#2.方差分析
#问题：是否水底，水中以及水底位置的不同，会影响大肠杆菌的浓度变化？
water_depth<-water %>% 
  mutate(Depth_factor = factor(Depth, ordered = TRUE)) %>%
  mutate(ecoli=as.numeric(E..coli..cfu.100mL.)) %>% 
  select(Depth_factor,ecoli)
  
anova_one_way <- aov(ecoli ~ Depth_factor, data =water_depth)
summary(anova_one_way)
#根据结果可得，p值远小于0.05，所以有信心说大肠杆菌的浓度有变化
#即水底，水中以及水底位置的不同，会影响大肠杆菌的浓度变化

#3
#问题：使用线性拟合来拟合近30年来水中的ph值，观察ph的改变
water_ph<-water %>% 
  filter(Depth=="Bottom Water") %>% 
  mutate(date=as.Date(Dates)) %>% 
  mutate(ph=as.numeric(pH)) %>% 
  select(date,ph)
  

model_ph <- lm( ph ~ date, data=water_ph )
summary(model_ph)
#可以从拟合结果中看到，近30年来，水中的ph值呈现出微弱的下降趋势
