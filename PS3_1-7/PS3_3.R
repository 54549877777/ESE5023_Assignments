library(tidyr)
library(dplyr)
library(ggplot2)


non_vegetarian_preg<-c(185,197,189,181,150,176)
vegetarian_preg<-c(171,174,202,171,207,125,189,179,163,174,184,186)
non_vegetarian<-c(210,139,172,198,177)

diff1<-t.test(non_vegetarian, non_vegetarian_preg)
diff2<-t.test(non_vegetarian_preg, vegetarian_preg)
diff3<-t.test(vegetarian_preg,non_vegetarian)

print(diff1)
print(diff2)
print(diff3)

#以上三组均无统计上的差异显著性
#所以不支持素食主义者比非素食主义者的锌含量更低