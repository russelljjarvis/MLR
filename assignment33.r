


library("compiler")
mse = function(y,yhat) {
  return(sum((y-yhat)^2))
}
doknn = function(x,y,xp,k) {
   kdo=k[1]
   train = data.frame(x,y=y)
   test = data.frame(xp); names(test) = names(train)[1:(ncol(train)-1)]
   near  = kknn(y~.,train,test,k=kdo,kernel='rectangular')
   return(near$fitted)
}
#--------------------------------------------------
docv = function(x,y,set,predfun,loss,nfold=10,doran=TRUE,verbose=TRUE,...)
{
    #x,y training data
    #set each row gives settings for predfun
    #predfun predicts on xp given (x,y)
    #loss: measure of fit
    #nfold: number of folds (e.g. 5 or 10)
    #doran: should you shuffle the data
   #a little error checking
   if(!(is.matrix(x) | is.data.frame(x))) {cat('error in docv: x is not a matrix or data frame\n'); return(0)}
   if(!(is.vector(y))) {cat('error in docv: y is not a vector\n'); return(0)}
   if(!(length(y)==nrow(x))) {cat('error in docv: length(y) != nrow(x)\n'); return(0)}

   #shuffle the data
   nset = nrow(set); n=length(y) #get dimensions
   if(n==nfold) doran=FALSE #no need to shuffle if you are doing them all.
   cat('in docv: nset,n,nfold: ',nset,n,nfold,'\n')
   lossv = rep(0,nset) #return values
   if(doran) {ii = sample(1:n,n); y=y[ii]; x=x[ii,,drop=FALSE]} #shuffle rows

   #loop over folds and settings
   fs = round(n/nfold) # fold size
   for(i in 1:nfold) { #fold loop
      bot=(i-1)*fs+1; top=ifelse(i==nfold,n,i*fs); ii =bot:top
      if(verbose) cat('on fold: ',i,', range: ',bot,':',top,'\n')
      xin = x[-ii,,drop=FALSE]; yin=y[-ii]; xout=x[ii,,drop=FALSE]; yout=y[ii]
      for(k in 1:nset) { #setting loop
         yhat = predfun(xin,yin,xout,set[k,],...)
         lossv[k]=lossv[k]+loss(yout,yhat)
      }
   }

   return(lossv)
}
docvknn = function(x,y,k,nfold=10,doran=TRUE,verbose=TRUE) {
  return(docv(x,y,matrix(k,ncol=1),doknn,mse,nfold=nfold,doran=doran,verbose=verbose))
}

#docvknn <- dget("cross_validation.R")
#docv <- dget("cross_validation.R")
cmpfun(docv)

cmpfun(docvknn)
#print(docvknn)
#print(docv)
#--------------------------------------------------
#cv version for knn


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
#if(!is.null(dev.list())) dev.off()
# Clear console
# Clean workspace
#rm(list=ls())
# Set working directory
# setwd("~/git/R_code_assignments")

destfile = "usedcars.csv"
if (!file.exists(destfile)) {
  usedcars = read.csv("http://www.rob-mcculloch.org/data/usedcars.csv")
  write.csv(usedcars, file = destfile)

}
usedcars = read.csv("usedcars.csv")
library(doParallel)

#sessionInfo() #see what packages were loaded
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

docvknn = function(x,y,k,nfold=10,doran=TRUE,verbose=TRUE) {
  return(docv(x,y,matrix(k,ncol=1),doknn,mse,nfold=nfold,doran=doran,verbose=verbose))
}
cmpfun(docvknn)

#browser()

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


x = price
y = mileage
price <- y
mileage <- x
kfit1 = kknn(mileage~price,train,test,k=450,kernel = "rectangular")
#fit1 = kfit1$fitted[fitind]
png('compare_fits.png',type="cairo")
par()
plot(mileage,price,col="lightgray")
points(mileage,price,col="blue",pch=16)
lines(test$price,kfit1$fitted,col="red",lwd=2)
print(paste('the price at 10000 from kmeans is: ',kfit1$fitted[10000]))
#print('28237.75')
#browser()
points(test[fitind,1],kfit1$fitted[fitind],col="green",pch=17,cex=2)
relation <- lm(price~mileage)

print(paste('the price at 10000 from linear regression is: ',relation$fitted.values[10000]))
contents = docvknn(data.frame(mileage),price,150,nfold=5,doran=TRUE,verbose=TRUE)

#plot(mileage,price,col="lightgray")
abline(relation,col="green",pch=17,cex=2)
dev.off()
set.seed(99)
compare <- TRUE
if (compare==TRUE){
  kfito = kknn(mileage~price,train,test,k=80,kernel = "optimal")
  #fito = kfito$fitted[fitind]

  png('compare_fits.png',type="cairo")
  par()
  plot(mileage,price,col="lightgray")

  kfitr = kknn(mileage~price,train,test,k=80,kernel = "rectangular")
  #fitr = kfitr$fitted[fitind]
  lines(test$price,kfitr$fitted,col="red",lwd=2)
  lines(test$price,kfito$fitted,col="red",lwd=2)
  abline(relation,col="green",pch=17,cex=2)
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

browser()

matrix<-foreach(i=1:length(kvec),.combine=cbind) %do% {
#matrix<-foreach(i=1:.combine=cbind) %do% {
   k <- kvec[i]
   thing_to_print <-paste('iteration number: ', kvec[i])
   print(thing_to_print)
   fname = paste('k_number_kmeans',kvec[i],'.png')
   png(fname,type="cairo")
   par()
   kfit1 = kknn(price~mileage,train,test,k=k,kernel = "rectangular")
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
   abline(h = kfit1$fitted,col="red")
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
