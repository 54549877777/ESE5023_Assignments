library(tidyr)
library(dplyr)
library(ggplot2)

#1
library(MASS)
data(cpus)
sample_index <- sample(nrow(cpus),nrow(cpus)*0.8)
cpus_train <- cpus[sample_index,]
cpus_test  <- cpus[-sample_index,]

library(leaps)
subset_result <- regsubsets(perf~ syct+mmin+mmax+cach+chmin+chmax, data=cpus_train,
                            nbest=2, nvmax = 6)
plot(subset_result, scale="bic")

#2
model_cpus <- lm( perf ~ syct+mmin+mmax+cach+chmax, data=cpus_train )
cpus_predict <- predict(model_cpus,cpus_test)
plot(cpus_test$perf, cpus_predict)

#Relative mean bias
mean_bias<-(mean(cpus_predict) - mean(cpus_test$perf))/
  mean(cpus_test$perf)*100
print(paste("相对平均偏差是",mean_bias,"%"))