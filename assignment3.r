#setContentType("image/png")
if("kknn" %in% rownames(installed.packages()) == FALSE) {
  update.packages(checkBuilt=TRUE, ask=FALSE,repos = "http://cran.us.r-project.org", dependencies = TRUE)
  install.packages("devtools",repos = "http://cran.us.r-project.org", dependencies = TRUE)
  # install.packages("devtools")
  devtools::install_github("igraph/rigraph")
  devtools::install_github("KlausVigo/kknn")
  install.packages("igraph",repos = "http://cran.us.r-project.org", INSTALL_opts = "--no-clean-on-error")
}
# install.packages("igraph",repos = "http://cran.us.r-project.org", dependencies = TRUE)
# install.packages("kknn",repos = "http://cran.us.r-project.org", dependencies = TRUE)
# install.packages("igraph",repos = "http://cran.us.r-project.org", dependencies = TRUE)
# install.packages("kknn",repos = "http://cran.us.r-project.org", dependencies = TRUE)

# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
# Clean workspace
rm(list=ls())
# Set working directory
# setwd("~/git/R_code_assignments")

destfile = "usedcars.csv"
if (!file.exists(destfile)) {
  usedcars = read.csv("http://www.rob-mcculloch.org/data/usedcars.csv")
  write.csv(usedcars, file = destfile)

}
usedcars = read.csv("usedcars.csv")
library("compiler")

library(doParallel)

sessionInfo() #see what packages were loaded
registerDoParallel(cores=8)
cat("number of workers is: ",getDoParWorkers(),"\n")



require("kknn")
library(kknn)

cmpfun(kknn)
cmpfun(lines)
cmpfun(abline)
cmpfun(points)

price = usedcars['price']
mileage = usedcars['mileage']
#print(price[1,:])
y = price[,1]
x = mileage[,1]
#print(price[1,1])
#plot(price[,1],mileage[,1])

kvec = c(4,40,150)
nsim = 30
fit1 = rep(0,nsim)
fit2 = rep(0,nsim)
fit3 = rep(0,nsim)
train = data.frame(x,y)
test = data.frame(x=sort(x))
#With the par( ) function, you can include the option mfrow=c(nrows, ncols)
#to create a matrix of nrows x ncols plots that are filled in by row.
#mfcol=c(nrows, ncols) fills in the matrix by columns.
ntrain=200

fitind = 400
kvec = c(1,2,3,4,5,6,7,8,9,10,20,40,75,150,200,300,400,500,600)
nsim=30
fit1=rep(0,nsim)
fit2=rep(0,nsim)
fit3=rep(0,nsim)
medv = x
lstat = y
train = data.frame(lstat,medv)
test = data.frame(lstat=sort(lstat))
#par(mfrow=c(2,3))
ntrain = 200
fitind = 400 #why 400?

kfit1 = kknn(medv~lstat,train,test,k=600,kernel = "rectangular")
fit1 = kfit1$fitted[fitind]
print(paste('iteration number: ',k))
png('compare_fits.png',type="cairo")
par()
plot(lstat,medv,col="lightgray")
points(lstat,medv,col="blue",pch=16)
lines(test$lstat,kfit1$fitted,col="red",lwd=2)
print(paste('the price at 10000 is: ',kfit1$fitted[10000]))
print('28237.75')
#browser()
points(test[fitind,1],kfit1$fitted[fitind],col="green",pch=17,cex=2)
relation <- lm(medv~lstat)
print(relation$fitted.values[10000])
print('76402.16')

#plot(lstat,medv,col="lightgray")
abline(relation,col="green",pch=17,cex=2)
dev.off()
set.seed(99)

##
# At k = 300 everything looks good.
##
#for (k in kvec){
#resP = foreach(k in kvev) %dopar% {



matrix<-foreach(i=1:length(kvec),.combine=cbind) %do% {
#matrix<-foreach(i=1:.combine=cbind) %do% {
   k = kvec[i]
   print(paste('iteration number: ', kvec[i]))

   fname = paste('k_number_kmeans',k,'.png')
   png(fname,type="cairo")
   #par(mfrow=c(1,3))
   par()
   #ii = sample(1:nrow(train),ntrain)
   kfit1 = kknn(medv~lstat,train,test,k=k,kernel = "rectangular")
   #kfit2 = kknn(medv~lstat,train[ii,],test,k=kvec[2],kernel = "rectangular")
   #kfit3 = kknn(medv~lstat,train[ii,],test,k=kvec[3],kernel = "rectangular")
   fit1[i] = kfit1$fitted[fitind]
   #fit2[i] = kfit2$fitted[fitind]
   #fit3[i] = kfit3$fitted[fitind]

   #browser()
   plot(lstat,medv,col="lightgray")
   points(lstat,medv,col="blue",pch=16)
   lines(test$lstat,kfit1$fitted,col="red",lwd=2)
   points(test[fitind,1],kfit1$fitted[fitind],col="green",pch=17,cex=2)
   relation <- lm(medv~lstat)
   plot(lstat,medv,col="lightgray")
   abline(relation,col="green",pch=17,cex=2)
   dev.off()

   #fname = paste('frame_linear_regression.png')
   #png(fname,type="cairo")
   #par()


  # dev.off()

   #browser()
   #plot(lstat,medv,col="lightgray")
   #points(lstat,medv,col="blue",pch=16)
   #lines(test$lstat,kfit1$fitted,col="red",lwd=2)
   #points(test[fitind,1],kfit1$fitted[fitind],col="green",pch=17,cex=2)

   #plot(lstat,medv,col="lightgray")
   #lines(test$lstat,kfit2$fitted,col="red",lwd=2)
   #points(test[fitind,1],kfit2$fitted[fitind],col="green",pch=17,cex=2)

   #plot(lstat,medv,col="lightgray")
   #lines(test$lstat,kfit3$fitted,col="red",lwd=2)
   #points(test[fitind,1],kfit3$fitted[fitind],col="green",pch=17,cex=2)
   #dev.off()


   #fname = paste('frame',i,'.png')
   #png(fname,type="cairo")
   fname = paste('the_box_plots',k,'.png')
   png(fname,type="cairo")
   #par(mfrow=c(1,3))
   boxplot(fit1)#,ylim=ylm)
   abline(h = gknn$fitted,col="red")
   dev.off()

   # boxplot(fit2[1:i])#,ylim=ylm)
   # abline(h = gknn$fitted,col="red")
   # boxplot(fit3[1:i])#,ylim=ylm)
   # abline(h = gknn$fitted,col="red")
   # dev.off()

 }

#}




#cat('yes, the relationship makes sense cars that are driven less on average cost more.')
#cat('People will pay more for reduced mileage, as this relates to less mechanical failure.')

#
#Get the susedcars.csv data set from the webpage. Plot x=mileage versus y=price. (price is the price of a used car.)

#Does the relationship between mileage and price make sense?

#  Add the fit from a linear regression to the plot. Add the fit from kNN for various values of k to the plot. For what value of k does the plot look nice?
#  Using your “nice” value of k, what is the predicted price of a car with 100,000 miles on it?
#  What is the prediction from a linear fit?
