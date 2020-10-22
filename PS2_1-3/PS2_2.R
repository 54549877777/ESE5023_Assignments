library(tidyr)
library(dplyr)
library(ggplot2)
#过去10年一起画
data = read.csv(file="2281305.csv", header = T)
data <- as_tibble(data)
data %>% 
  select(WND,DATE)%>% 
  filter(substr(WND,1,3)!="999") %>% 
  filter(substr(WND,9,12)!="9999") %>% 
  filter(substr(WND,5,7)=="1,N")%>%
  filter(substr(WND,14,14)=="1")%>%
  mutate(monthly=as.numeric(paste(substr(DATE,1,4),substr(DATE,6,7),sep = ""))) %>%
  mutate(ws=as.numeric(substr(WND,9,12)))%>%
  select(monthly,ws) %>% 
  group_by(monthly) %>% 
  summarise(ws_month=mean(ws,na.rm = T)) %>% 
  ggplot(aes(x=monthly, y=ws_month)) + 
  geom_line()

#每年画一个
data = read.csv(file="2281305.csv", header = T)
data <- as_tibble(data)
data %>% 
  select(WND,DATE)%>% 
  filter(substr(WND,1,3)!="999") %>% 
  filter(substr(WND,9,12)!="9999") %>% 
  filter(substr(WND,5,7)=="1,N")%>%
  filter(substr(WND,14,14)=="1")%>%
  mutate(ws=as.numeric(substr(WND,9,12)))%>%
  mutate(date=as.numeric(paste(substr(DATE,1,4),substr(DATE,6,7),sep = ""))) %>%
  group_by(date) %>% 
  summarise(month_mean=mean(ws),na.rm=T) %>%
  mutate(year=as.numeric(substr(date,1,4))) %>%
  mutate(month=as.numeric(substr(date,5,6)))%>% 
  ggplot(aes(x=month, y=month_mean)) + 
  geom_line()+
  facet_wrap(~ year)
  
    
           
  
  
  
  
  

