library(tidyr)
library(dplyr)
library(ggplot2)

#1.1
Unseeded<-c(1202.6, 830.1, 372.4, 345.5, 321.2, 244.3, 163.0, 147.8, 95.0, 87.0, 81.2, 68.5, 47.3, 41.1, 36.6, 29.0, 28.6, 26.3, 26.0, 24.4, 21.4, 17.3, 11.5, 4.9, 4.9, 1.0)
Seeded<-c(2745.6, 1697.1, 1656.4, 978.0, 703.4, 489.1, 430.0, 334.1, 302.8, 274.7, 274.7, 255.0, 242.5, 200.7, 198.6, 129.6, 119.0, 118.3, 115.3, 92.4, 40.6, 32.7, 31.4, 17.5, 7.7, 4.1)
Rainfall<-cbind(Unseeded,Seeded)
rf<-as_tibble(Rainfall)
boxplot(rf$Unseeded,rf$Seeded,
        xlab = "UNSEED_OR_SEED",
        ylab = "rainfall",
        main = "rainfall",
        cex = 1,
        col = "orange",
        border = "darkgreen")

#1.2
diff<-t.test(rf$Unseeded, rf$Seeded)
diff
print(paste("偏差是：",diff$statistic))
#p值为0.054，可以认为在0.05的置信水平下无明显差异
#但在0.1的置信水平下可以认为有差异