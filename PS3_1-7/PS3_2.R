library(tidyr)
library(dplyr)
library(ggplot2)
bone_mean<-c(11.10,11.22,11.29,11.49,11.32,11.40,11.71,11.60,11.78,12.05
          ,10.61,10.88,11.12,11.24,11.43,10.92,11.20,11.30,11.62,11.70
          ,11.70,11.79,11.91,12.15
          ,11.33,11.41,11.62,12.15,12.30
          ,11.32,11.65,11.96,12.15
          ,11.54,11.89,12.04
          ,10.93,11.01,11.08,11.12,11.28,11.37
          ,11.35,11.43,11.50,11.57,11.92
          ,11.95,12.01,12.25,12.30,12.39)

bone<-c("Rib","Rib","Rib","Rib","Gastralia","Gastralia","Gastralia","Gastralia","Gastralia",
           "Gastralia","Dorsal_vertebra","Dorsal_vertebra","Dorsal_vertebra","Dorsal_vertebra",
           "Dorsal_vertebra","Dorsal_vertebra","Dorsal_vertebra","Dorsal_vertebra",
           "Dorsal_vertebra","Dorsal_vertebra","Femur","Femur","Femur","Femur",
           "Tibia","Tibia","Tibia","Tibia","Tibia","Metatarsal","Metatarsal","Metatarsal","Metatarsal",
           "Phalange", "Phalange", "Phalange","Proximal_caudal","Proximal_caudal","Proximal_caudal",
           "Proximal_caudal","Proximal_caudal","Proximal_caudal",
           "Mid_caudal", "Mid_caudal", "Mid_caudal", "Mid_caudal", "Mid_caudal",
           "Distal_caudal","Distal_caudal","Distal_caudal","Distal_caudal","Distal_caudal")

list<-cbind(bone,bone_mean)
tyran<-as_tibble(list)
tyran_tbl<-tyran %>% 
  mutate(bone_factor = factor(bone, ordered = TRUE)) %>% 
  mutate(bone_value=as.numeric(bone_mean)) 


anova_one_way <- aov(bone_value ~ bone_factor, data =tyran_tbl)
summary(anova_one_way)

#P值非常的小，所以有信心说两组之间存在统计差异，即霸王龙不是温血动物。
