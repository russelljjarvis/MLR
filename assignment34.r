


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
         print(yhat)
         lossv[k]=lossv[k]+loss(yout,yhat)
      }
   }

   return(lossv)
}
do_cv_knn = function(x,y,k,nfold=10,doran=TRUE,verbose=TRUE) {
  return(docv(x,y,matrix(k,ncol=1),doknn,mse,nfold=nfold,doran=doran,verbose=verbose))
}

cmpfun(docv)
cmpfun(do_cv_knn)


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

#temp <- mileage
#mileage <- price
#%price <- temp
plot(mileage,price,col='blue')

#do_cvk_nn = function(x,y,k,nfold=10,doran=TRUE,verbose=TRUE) {#
#  return(docv(x,y,matrix(k,ncol=1),doknn,mse,nfold=nfold,doran=doran,verbose=verbose))
#}
#cmpfun(do_cvk_nn)

#browser()

#price <- rev(price)
#png('simple_plot_3.png',type="cairo")
#dev.off()
nsim=30
fit1 = rep(0,nsim)
#With the par( ) function, you can include the option mfrow=c(nrows, ncols)
#to create a matrix of nrows x ncols plots that are filled in by row.
#mfcol=c(nrows, ncols) fills in the matrix by columns.
ntrain=200

fitind = 400
kvec = c(1,2,3,4,5,6,7,8,9,10,20,40,75,150,200,300,400,500,600)
#fit1=rep(0,nsim)
ntrain = 200
fitind = 10000 #why 400?


#x = price
#y = mileage
#price <- y
#mileage <- x
train = data.frame(price,mileage)
test = data.frame(mileage=sort(mileage))

kfit1 = kknn(price~mileage,train,test,k=450,kernel = "rectangular")
#kfit1 = kknn(mileage~price,train,test,k=450,kernel = "rectangular")

par()
plot(mileage,price,col="lightgray")
#points(mileage,price,col="lightgray",pch=16)
lines(test$mileage,kfit1$fitted,col="red",lwd=2)
print(paste('the price at 10000 from kmeans is: ',kfit1$fitted[10000]))
points(test[fitind,1],kfit1$fitted[fitind],col="blue",pch=17,cex=2)
relation <- lm(price~mileage)
#abline(relation,col="green",pch=17,cex=2)

print(paste('the price at 10000 from linear regression is: ',relation$fitted.values[10000]))

#for i in folds{
#resP = foreach(k in kvev) %dopar% {
m<-0
matrix <- foreach(k=145:250 ,.combine=cbind) %dopar% { do_cv_knn(data.frame(mileage),price,k,nfold=5,doran=TRUE,verbose=TRUE) }
plot_against = rep(1:length(matrix))
plot(plot_against,matrix)
browser()


using_all_neighbors <- do_cv_knn(data.frame(mileage),price,length(mileage),nfold=5,doran=TRUE,verbose=TRUE) 

#browser()
#plot(mileage,price,col="lightgray")
#dev.off()
set.seed(99)
compare <- TRUE
#if (compare==TRUE){
kfito = kknn(price~mileage,train,test,k=15,kernel = "optimal")
#fito = kfito$fitted[fitind]

#png('compare_fits.png',type="cairo")
#par()

kfitr = kknn(price~mileage,train,test,k=15,kernel = "rectangular")
#fitr = kfitr$fitted[fitind]
plot(mileage,price,col="lightgray")
lines(test$mileage,kfitr$fitted,col="red",lwd=2,xlab="rectangular")
lines(test$mileage,kfito$fitted,col="green",lwd=2,xlab='optimal')
#+   main = "Eruptions of Old Faithful",
#+   xlab = "Eruption time (min)",
#+   ylab = "Waiting time to next eruption (min)")
#abline(relation,col="green",pch=17,cex=2)
  #dev.off()
#}




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
#resP = foreach(k in kvev) %dopar% {

#browser()

# }

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
