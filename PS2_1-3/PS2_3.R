library(tidyr)
library(dplyr)
library(ggplot2)

waterdata <- read.csv(file = "water_quality.csv", header = T,encoding = "UTF-8")
waterdata<-as_tibble(waterdata)
waterdata %>% 
  mutate(elico=ifelse(E..coli..cfu.100mL.>9999,9999,E..coli..cfu.100mL.)) %>% 
  mutate(date=as.Date(waterdata$Dates)) %>% 
  select(date,elico) %>% 
  ggplot(aes(x=date,y=elico))+
  geom_line()