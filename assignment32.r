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
options(error=recover,keep.source = TRUE, show.error.locations = TRUE,
keep.source.pkgs = TRUE)

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

price = price[,1]
mileage = mileage[,1]

#price <- rev(price)
png('simple_plot_3.png',type="cairo")
plot(mileage,price,col='blue')
dev.off()
nsim=30
fit1 = rep(0,nsim)
train = data.frame(mileage,price)
test = data.frame(price=sort(price))
#With the par( ) function, you can include the option mfrow=c(nrows, ncols)
#to create a matrix of nrows x ncols plots that are filled in by row.
#mfcol=c(nrows, ncols) fills in the matrix by columns.
ntrain=200

fitind = 400
kvec = c(1,2,3,4,5,6,7,8,9,10,20,40,75,150,200,300,400,500,600)
#fit1=rep(0,nsim)
ntrain = 200
fitind = 400 #why 400?

kfit1 = kknn(mileage~price,train,test,k=700,kernel = "rectangular")
fit1 = kfit1$fitted[fitind]
png('compare_fits.png',type="cairo")
par()
plot(mileage,price,col="lightgray")
points(mileage,price,col="blue",pch=16)
lines(test$price,kfit1$fitted,col="red",lwd=2)
print(paste('the price at 10000 is: ',kfit1$fitted[10000]))
#print('28237.75')
#browser()
points(test[fitind,1],kfit1$fitted[fitind],col="green",pch=17,cex=2)
relation <- lm(mileage~price)
print(relation$fitted.values[10000])
#print('76402.16')

#plot(mileage,price,col="lightgray")
abline(relation,col="green",pch=17,cex=2)
dev.off()
set.seed(99)
browser()
compare <- TRUE
if (compare==TRUE){
  kfito = kknn(price~mileage,train,test,k=80,kernel = "optimal")
  fito = kfito$fitted[fitind]

  png('compare_fits.png',type="cairo")
  par()
  kfitr = kknn(price~mileage,train,test,k=80,kernel = "rectangular")
  fitr = kfitr$fitted[fitind]
  lines(test$price,kfitr$fitted,col="red",lwd=2)
  lines(test$price,kfito$fitted,col="red",lwd=2)
  #abline(relation,col="green",pch=17,cex=2)
  dev.off()
}

  #plot(mileage,price,col="lightgray")
  #points(mileage,price,col="blue",pch=16)
  #print(paste('the price at 10000 is: ',kfit1$fitted[10000]))
  #print('28237.75')
  #browser()
  #points(test[fitind,1],kfit1$fitted[fitind],col="green",pch=17,cex=2)
  #relation <- lm(price~mileage)
  #print(relation$fitted.values[10000])
  #print('76402.16')

  #plot(mileage,price,col="lightgray")
##
# At k = 300 everything looks good.
##
#for (k in kvec){


matrix<-foreach(i=1:length(kvec),.combine=cbind) %do% {
#matrix<-foreach(i=1:.combine=cbind) %do% {
   k <- kvec[i]
   thing_to_print <-paste('iteration number: ', kvec[i])
   print(thing_to_print)
   fname = paste('k_number_kmeans',kvec[i],'.png')
   png(fname,type="cairo")
   par()
   gknn = kfit1 = kknn(price~mileage,train,test,k=k,kernel = "rectangular")
   fit1[i] = kfit1$fitted[fitind]

   plot(mileage,price,col="lightgray")
   points(mileage,price,col="blue",pch=16)
   lines(test$price,kfit1$fitted,col="red",lwd=2)
   points(test[fitind,1],kfit1$fitted[fitind],col="green",pch=17,cex=2)
   relation <- lm(price~mileage)
   plot(mileage,price,col="lightgray")
   abline(relation,col="green",pch=17,cex=2)
   dev.off()

   fname = paste('the_box_plots',k,'.png')
   png(fname,type="cairo")
   boxplot(fit1[i])
   abline(h = gknn$fitted,col="red")
   dev.off()
 }

# the data makes sense when only cost can be zero. Mileage can't be zero.
cat('controlling for initial price, cost ')
#cat('depcrecation, yes, the relationship makes sense cars that are driven less on average cost more.')
#cat('People will pay more for reduced mileage, as this relates to less mechanical failure.')

#
#Get the susedcars.csv data set from the webpage. Plot x=mileage versus y=price. (price is the price of a used car.)

#Does the relationship between mileage and price make sense?

#  Add the fit from a linear regression to the plot. Add the fit from kNN for various values of k to the plot. For what value of k does the plot look nice?
#  Using your “nice” value of k, what is the predicted price of a car with 100,000 miles on it?
#  What is the prediction from a linear fit?
