library(tidyr)
library(dplyr)
library(ggplot2)

high<-c(180,305,381,488,549,640,762,883)
high<-0.001*high
temp<-c(13.3,12.2,13.3,10.0,8.3,9.4,8.3,7.2)

list<-cbind(high,temp)
hike<-as_tibble(list)

hike %>% 
  ggplot( aes(x=high, y=temp) ) + 
  geom_point() + 
  geom_smooth()
  
model <- lm(temp ~ high, data = hike)
coef(model)
summary(model)
plot(high,temp)
abline(model,col="red")
#拟合直线斜率为-9.3，近似于-9.8