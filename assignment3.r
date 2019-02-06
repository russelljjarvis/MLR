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

#load(destfile)
library(doParallel)
#cmpfun(doParallel)

sessionInfo() #see what packages were loaded
registerDoParallel(cores=8)
cat("number of workers is: ",getDoParWorkers(),"\n")

resP = foreach(i=1:100,.combine=rbind) %dopar% {
  print(i)
}


require("kknn")
library(kknn)

cmpfun(kknn)
cmpfun(lines)
cmpfun(abline)
cmpfun(points)
print('yes')
#cmpfun(boxplot)
if(!is.null(dev.list()))
png('dirty.png',type="cairo")

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
ylm = c(10,25)

#get good one
gknn = kknn(x~y,train,data.frame(x=test[fitind,1]),k=40,kernel = "rectangular")
set.seed(99)
#if(!is.null(dev.list()))
#png('price_versus_mileage.png',type="cairo")

#plot(price[,1],mileage[,1])
#dev.off()


stopifnot(1==1)
png('movie.png',type="cairo")

par(mfrow=c(2,3))

for(i in 1:nsim) {
  ii = sample(1:nrow(train),ntrain)
  kfit1 = kknn(x~y,train[ii,],test,k=kvec[1],kernel = "rectangular")
  kfit2 = kknn(x~y,train[ii,],test,k=kvec[2],kernel = "rectangular")
  kfit3 = kknn(x~y,train[ii,],test,k=kvec[3],kernel = "rectangular")
  #png('movie.png',type="cairo")
  #
  #
  plot(x,y,col="lightgray")
  points(x[ii],y[ii],col="blue",pch=16)
  lines(test$x,kfit1$fitted,col="red",lwd=2)
  points(test[fitind,1],kfit1$fitted[fitind],col="green",pch=17,cex=2)

  plot(x,y,col="lightgray")
  lines(test$x,kfit2$fitted,col="red",lwd=2)
  points(test[fitind,1],kfit2$fitted[fitind],col="green",pch=17,cex=2)

  plot(x,y,col="lightgray")
  lines(test$x,kfit3$fitted,col="red",lwd=2)
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
  #Sys.sleep(.4)
}

dev.off()

cat('yes, the relationship makes sense cars that are driven less on average cost more.')
cat('People will pay more for reduced mileage, as this relates to less mechanical failure.')

#
#Get the susedcars.csv data set from the webpage. Plot x=mileage versus y=price. (price is the price of a used car.)

#Does the relationship between mileage and price make sense?

#  Add the fit from a linear regression to the plot. Add the fit from kNN for various values of k to the plot. For what value of k does the plot look nice?
#  Using your “nice” value of k, what is the predicted price of a car with 100,000 miles on it?
#  What is the prediction from a linear fit?
