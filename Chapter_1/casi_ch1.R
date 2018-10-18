###computer age statistical inference chapter 1
#by honground - suckwon Hong

#load data
setwd("~/Desktop/casi/CASI_local/chapter1") #check your working directory
kidney<-read.table("kidney.txt",header=T)

#Regression
mod.regression<-lm(tot ~ age,data=kidney)
predict.mod<-predict(mod.regression,newdata=data.frame(age=seq(20,80,10)),se.fit=T)

#Figure 1.1
plot(kidney$age,kidney$tot,pch=4, col="blue")
abline(mod.regression, col="green")

#Lowess
mod.lowess<-lowess(kidney$age,kidney$tot,1/3)
predict.mod.lowess<-approx(mod.lowess$x,mod.lowess$y,xout=seq(20,80,10))

#Figure 1.2
plot(mod.lowess,type="l",lwd=3,col="green")
points(kidney$age,kidney$tot,pch=3,col="blue")

#Figure 1.3 bootstrap
data_temp<-c()
for(i in 1:50){
  bootstrap_kidney<-kidney[sample(1:nrow(kidney),replace=T),]
  lowess.bootstrap<-lowess(bootstrap_kidney$age,bootstrap_kidney$tot,1/3)
  lines(lowess.bootstrap,col="red")
  data_temp<-rbind(data_temp,approx(lowess.bootstrap$x,lowess.bootstrap$y,xout=seq(20,80,10))$y)
}