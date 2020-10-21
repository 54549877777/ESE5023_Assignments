library(tidyr)
library(dplyr)
library(ggplot2)
#1.1
data = read.delim(file="signif.txt", header = T)
Sig_Eqs=as_tibble(data)


#1.2
Sig_Eqs %>% 
  select(COUNTRY,TOTAL_DEATHS) %>% 
  group_by(COUNTRY) %>% 
  summarise(country_total_death = sum(TOTAL_DEATHS,na.rm = T)) %>% 
  arrange(desc(country_total_death)) 


#1.3
Sig_Eqs %>%
  select(EQ_PRIMARY,YEAR) %>% 
  filter(EQ_PRIMARY>6.0)%>% 
  group_by(YEAR) %>% 
  summarise(EQ_PRIMARY_YEAR=sum(EQ_PRIMARY,na.rm = T)) %>% 
  
  ggplot(aes(x=YEAR,y=EQ_PRIMARY_YEAR)) + 
  geom_line()
#分析：
#从曲线图中可以看出，6级以上的地震发生频率不断增加，公元1800年以后更是
#成指数增长。我觉得原因有2个：首先是古时的数据不完整，很多数据没有被记录
#或数据丢失；第二是近几百年来地壳变得更活跃了。

#1.4
#函数
CountEq_LargestEq<-function(country){
  num_eq<-Sig_Eqs %>% 
    filter(COUNTRY==country)%>% 
    nrow()
  max_eq<-Sig_Eqs %>%
    filter(COUNTRY==country)%>%
    filter(EQ_PRIMARY == max(EQ_PRIMARY,na.rm = T))%>% 
    mutate(date=paste(YEAR,MONTH,DAY,sep = "-"))%>% 
    pull(date)
  list<-list(num_eq,max_eq)
  return(list)
}

#将数据输入数据框，再转换成tibble表
country<-unique(Sig_Eqs$COUNTRY)
number_eq<-c()
country_eq<-c()
date_max_eq<-c()
for(i in country){
  a<-as.numeric(CountEq_LargestEq(i)[1])
  b<-i
  c<-CountEq_LargestEq(i)[2]
  number_eq <-c(number_eq,a)
  country_eq<-c(country_eq,b)
  date_max_eq<-c(date_max_eq,c)
}
df1<-data.frame(country_eq,number_eq,date_max_eq)

#对新列表进行降序
tbl_new<-as_tibble(df1)
tbl_new %>% 
  arrange(desc(number_eq))
