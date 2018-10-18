###computer age statistical inference chapter 2
#by honground - suckwon Hong

#install and load required package
install.packages("psych")
library(psych)

#load data
setwd("~/Desktop/CASI/chapter2") # check your working directory if error occurs!
data<-read.table("gfr.txt",header=F)

#Figure 2.1
hist(data$V1,breaks=30)

#mean and standard error (Table 2.1)
mean<-mean(data$V1) 
s.error<-function(x){
  n<-length(x)
  return (sqrt(sum((x-mean(x))^2)/(n*(n-1))))
}
paste("mean: ", mean)
paste("standard error: ",s.error(data$V1))

#winsorized mean
mean.winsorized<-winsor.mean(data$V1, trim = 0.25, na.rm = TRUE)
paste("winsorized mean: ", mean.winsorized)

#bootstrap mean
mean.bootstrap<-c()
for(i in 1:1000){
  data.boot<-data$V1[sample(1:length(data$V1),replace=T)]
  mean.bootstrap<-c(mean.bootstrap, winsor.mean(data.boot, trim = 0.25, na.rm = FALSE))
}
s.error.bootmean<-sd(mean.bootstrap)

median.bootstrap<-c()
for(i in 1:1000){
  data.median<-data$V1[sample(1:length(data$V1),replace=T)]
  median.bootstrap<-c(median.bootstrap,median(data.median))
}
s.error.bootstrap<-sd(median.bootstrap)
paste("bootstrap mean_standard deviation: ",s.error.bootmean)
paste("bootstrap median_standard deviation: ",s.error.bootstrap)


#Figure 2.2
n<-10
result<-c()
for(c in seq(-6,6,0.2)){
  alpha<-c()
  beta<-c()
  for(i in 1:1000){
    x<-rnorm(1,0,1/sqrt(n))
    f1x<-dnorm(x,0.5,1/sqrt(n))
    f0x<-dnorm(x,0,1/sqrt(n))
    Lx<-f1x/f0x
    Tx<-log(Lx)
    a<-isTRUE(Tx>=c)
    alpha<-c(alpha,a)
    
    x<-rnorm(1,0.5,1/sqrt(n))
    f1x<-dnorm(x,0.5,1/sqrt(n))
    f0x<-dnorm(x,0,1/sqrt(n))
    Lx<-f1x/f0x
    Tx<-log(Lx)
    b<-isTRUE(Tx<c)
    beta<-c(beta,b)
  }
  result<-rbind(result,data.frame(alpha=mean(alpha),beta=mean(beta),c))
}
plot(result$alpha,result$beta,type="l")