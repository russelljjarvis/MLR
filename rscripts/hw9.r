#=
library(h2o)
x11(width = 8, height = 8)

library(MASS)
## a library of example datasets
attach(Boston)## standardize
#lstat
rg =range(Boston$lstat)
lstats = (Boston$lstat-rg[1])/(rg[2]-rg[1])
##make data frame with standardized lstat values sorted for plotting
ddf =data.frame(lstats,medv=Boston$medv)
oo =order(ddf$lstats)#order the data by x, convenient for plotting
ddf = ddf[oo,]
head(ddf)
plot(ddf)

library(nnet)
set.seed(14)
nn1 =nnet(medv~lstats,ddf,size=5,decay=.1,linout=T,maxit=1000)
summary(nn1)
yhat1 =predict(nn1,ddf)
plot(ddf)
lines(ddf$lstats,yhat1,lty=1,col="red",lwd=3)
browser()
