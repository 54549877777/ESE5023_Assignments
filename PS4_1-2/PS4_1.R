library(ggplot2)

#1.Boxplot --  ToothGrowth
library(ggpubr)
data(ToothGrowth)
data1 <- ToothGrowth
ggboxplot(data1, x="dose", y="len", fill="dose", palette = "npg", 
          width = 0.5,size = 0.8,xlab = FALSE,ylab = "Length",
          font.y=20,font.xtickslab = 20,font.ytickslab = 20)+
  scale_x_discrete(limits=c("0.5", "1", "2")) +
  labs(title = "Comparison of different doses")+
  theme(legend.position='none',plot.title = element_text(size=25,hjust = 0.5))+
  stat_compare_means(comparisons = list(c("0.5", "1"), c("1", "2"), 
                                      c("0.5", "2")),label= "p.signif")

#2.Time series --  Economics
ggplot(economics,aes(x=date,y=unemploy))+ 
  geom_line(colour='green')+
  geom_line(colour='green') + 
  geom_area(colour='green',alpha=0.3)+
  labs(title = "The Economics")+
  xlab('The Time Series of Date')+ 
  ylab('The Time Series of unemploy')+
  geom_smooth(method = "loess")+
  theme_bw()

#3.Histogram
ggplot(d, aes(x, fill = cut(x, 100)))+
  geom_histogram(bins = 50,show.legend = FALSE)+
  scale_fill_discrete(h = c(240, 10), c = 120, l = 70)+
  ggtitle("Histogram of X")+
  labs(x = "Variable X", y = "Frequency")+
  geom_density(color = "red",alpha=.2, fill="#FF6666")+
  theme_bw()
#??拟合曲线没有出现 


#4.Scatter plot  --  iris
library(ggthemes)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point(size = 2.0, shape = 16) +
  facet_wrap( ~ Species) +
  labs(title="Sepal.Length ~ Sepal.Width") +
  geom_smooth(method = "loess")+
  theme_solarized()

#5.The Surface Air Temperature Monthly Unnomaly 
library(fields)
library(maps)
library(RNetCDF)
ex.nc     <- open.nc("air.mon.anom.nc")
print.nc(ex.nc)
Lat       <- var.get.nc(ex.nc, "lat")
Lon       <- var.get.nc(ex.nc, "lon")
Air_T     <- var.get.nc(ex.nc, "air") 
close.nc(ex.nc)
Lat <- rev(Lat)
Air_T_Jan <- Air_T[,,1]
for(row in 1:length(Lat)){
  Air_T_Jan[,row] <- Air_T_Jan[, (length(Lat)+1-row) ]
}
image.plot(Lon, Lat, Air_T_Jan,
           horizontal=T, useRaster=T,
           legend.shrink=0.75, axis.args=list(cex.axis = 1.25), 
           legend.width=1, legend.mar=2,
           legend.args=list(text="Surface Temperature [k]",cex=1.25),           
           xlab='',ylab='',midpoint=T, axes=F, ann=F
)
title(xlab="",cex.lab=1.25,font.lab=2)
axis(1,at=pretty(Lon),tck=-0.015,lwd=2,cex.axis=1.25,font=1)
title(ylab="",cex.lab=1.25,font.lab=2)
axis(2,at=pretty(Lat),tck=-0.015,lwd=2,cex.axis=1.25,font=1,las=1)
title(main=paste("unusually."),
      cex.main=1,font.main=2)
map('world',add=T,lwd=0.75,col="black")
