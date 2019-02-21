######################################################################
if(1) {cat("###load data and libs\n")
#load knn library (need to have installed this with install.packages("kknn"))
library(kknn)
#get Boston data
#library(MASS) ## a library of example datasets
#attach(Boston)
}
######################################################################
kvec = c(4,40,150)
nsim = 30
fit1=rep(0,nsim)
fit2=rep(0,nsim)
fit3=rep(0,nsim)
print('gets here a')

train = data.frame(lstat,medv)
test = data.frame(lstat=sort(lstat))
print('gets here b')
#par(mfrow=c(2,3))
ntrain = 200

fitind = 400
ylm = c(10,25)
print('gets here c')
#get good one
png('other.png',type="cairo")

gknn = kknn(medv~lstat,train,data.frame(lstat=test[fitind,1]),k=40,kernel = "rectangular")

plot(lstat,medv,col="lightgray")
points(lstat[ii],medv[ii],col="blue",pch=16)
lines(test$lstat,kfit1$fitted,col="red",lwd=2)
points(test[fitind,1],kfit1$fitted[fitind],col="green",pch=17,cex=2)

set.seed(99)
browser()
for(i in 1:nsim) {
   ii = sample(1:nrow(train),ntrain)
   kfit1 = kknn(medv~lstat,train[ii,],test,k=kvec[1],kernel = "rectangular") # what even is lstat?
   kfit2 = kknn(medv~lstat,train[ii,],test,k=kvec[2],kernel = "rectangular") # what even is medv?
   kfit3 = kknn(medv~lstat,train[ii,],test,k=kvec[3],kernel = "rectangular") #
   png('dirty.png',type="cairo")

   plot(lstat,medv,col="lightgray")
   points(lstat[ii],medv[ii],col="blue",pch=16)
   lines(test$lstat,kfit1$fitted,col="red",lwd=2)
   points(test[fitind,1],kfit1$fitted[fitind],col="green",pch=17,cex=2)

   plot(lstat,medv,col="lightgray")
   lines(test$lstat,kfit2$fitted,col="red",lwd=2)
   points(test[fitind,1],kfit2$fitted[fitind],col="green",pch=17,cex=2)

   plot(lstat,medv,col="lightgray")
   lines(test$lstat,kfit3$fitted,col="red",lwd=2)
   points(test[fitind,1],kfit3$fitted[fitind],col="green",pch=17,cex=2)

   fit1[i]=kfit1$fitted[fitind]
   boxplot(fit1[1:i],ylim=ylm)
   abline(h=gknn$fitted,col="red")

   fit2[i]=kfit2$fitted[fitind]
   boxplot(fit2[1:i],ylim=ylm)
   abline(h=gknn$fitted,col="red")

   fit3[i]=kfit3$fitted[fitind]
   boxplot(fit3[1:i],ylim=ylm)
   abline(h=gknn$fitted,col="red")

   #readline("go?")
   Sys.sleep(.4)
}
