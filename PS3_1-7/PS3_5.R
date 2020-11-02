library(tidyr)
library(dplyr)
library(ggplot2)

Nebula<-c("S. Mag","L. Mag","NGC 6822","NGC 598","NGC 221","NGC 224","NGC 5457","NGC 4736",
          "NGC 5194","NGC 4449","NGC 4214","NGC 3031","NGC 3627","NGC 4626","NGC 5236",
          "NGC 1068","NGC 5055","NGC 7331","NGC 4258","NGC 4151","NGC 4382","NGC 4472",
          "NGC 4486","NGC 4649")

Velocity<-c(170,290,-130,-70,-185,-220,200,290,270,200,300,-30,650,150,500,920,450,500,500,
            960,500,850,800,1090)

Distance<-c(0.032,0.034,0.214,0.263,0.275,0.275,0.450,0.500,0.500,0.630,0.800,0.900,0.900,
            0.900,0.900,1.000,1.100,1.100,1.400,1.700,2.000,2.000,2.000,2.000)
Distance<- 30.9e18*Distance
list<-data.frame(Nebula,Velocity,Distance)
bigbang<-as_tibble(list)

#1
plot(Velocity,Distance)

#2
model <- lm(Distance ~ Velocity, data = bigbang)
abline(model,col="red")

#3
#lm模型中加-1可以令截距等于0
model_new <- lm(Distance ~ Velocity-1, data = bigbang)
summary(model_new)
#如果严格遵循大爆炸理论，当宇宙刚刚诞生时，没有距离，红移应该为0,
#且随着时间的推移，红移越来越大，通过红移可以测量过去时间的多少
age<-(model_new$coefficients/60/60/24/365)
print(paste("宇宙的年龄为：",age,"岁"))

#4
#首先截距不为0，明显是有误差的，然后数据中，不同的红移，有着相同的距离，
#也是不对的，所以提升测量精度是可以得到更好的回归系数，也就是更精确的宇宙年龄