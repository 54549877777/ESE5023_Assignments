#(1)
data<- read.csv(file = "2281305.csv", header = T)
vis<-data$VIS
vis1<-substr(vis,1,6)
vis3<-substr(vis,8,12)
vis2<-as.numeric(vis1)
vis2[which(vis2>160000)]<-NA
vis2[which(vis3!="1,N,1")]<-NA
time<-as.Date(data$DATE)
plot(time, vis2, lwd=0.05, type="l", col="blue")
#图中2013年后的数据不合要求，之前的数据过于密集，无法得出能见距离是否变远
#(2)
vis_max <- c()
for(iday in unique(time)){
  Thisday<- which(time == iday)
  Thisday_max <- max(vis2[Thisday], na.rm=T)
  vis_max <- c(vis_max, Thisday_max)
}
Year <-substr(unique(time),1,4)
Year1<-as.numeric(Year)
Year2<-unique(Year1)
for (iyear in Year2){
  Year_Vis <- vis_max[which(Year1==iyear)]
  hist(Year_Vis,breaks=c(0,5000,10000,15000,20000,25000,30000))
}
