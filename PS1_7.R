#1
waterdata <- read.csv(file = "water_quality.csv", header = T,encoding = "UTF-8")
#2
ecoli<-waterdata$E..coli..cfu.100mL.
ecoli[which(ecoli>9999)]<-9999
date<-as.Date(waterdata$Dates)
plot(date,ecoli,lwd=0.05, type="l", col="blue")
#3
min(ecoli)
max(ecoli)
mean(ecoli)
median(ecoli)
var(ecoli)
sd(ecoli)
hist(ecoli)
summary(ecoli)
plot(date,ecoli,lwd=0.05, type="o", col="blue")