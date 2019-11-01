##################################################
### get out of sample rmse for susedcars data

## read in the data
cd = read.csv("http://www.rob-mcculloch.org/data/susedcars.csv")
n = nrow(cd)

## pull off (price,mileage,year) and divide price and mileage by 1000
ddf = cd[,c(1,4,5)]
ddf$mileage = ddf$mileage/1000
ddf$price = ddf$price/1000
print(head(ddf))

## train/test split
nsamp = 500 # number of train/test plots
trainfrac = .75 # percent of data in train, rest is test
ntrain = floor(trainfrac*n)
ntest = n-ntrain
cat("nsamp, n,ntrain,ntest: ",nsamp,n,ntrain,ntest,"\n")

## rmse function
rmsef = function(y,yhat) {
   return( sqrt(mean((y-yhat)^2)) )
}

## store results
resv = rep(0.0,nsamp) # store results
ysM = matrix(0.0,ntest,nsamp)
yhatM = matrix(0.0,ntest,nsamp)

## loop over random train/test splits
set.seed(99)
for(i in 1:nsamp) {
   if((i %% 5) == 0) cat("on sample: ",i,"\n")

   #draws train subset
   ii = sample(1:n,ntrain)

   # train/test
   ddftr = ddf[ii,]; ddfte = ddf[-ii,]

   #fit on train
   lmmod = lm(price~.,ddftr)

   #predict on test
   yhatte = predict(lmmod,ddfte)

   #record loss
   resv[i] = rmsef(ddfte$price,yhatte)

   # keep sampled y and yhat
   ysM[,i] = ddfte$price; yhatM[,i] = yhatte
}

##plot results
par(mfrow=c(1,2))
plot(resv)
boxplot(resv)

par(mfrow=c(1,1))
plim = range(ysM)
plot(as.double(ysM),as.double(yhatM),xlab="y",ylab="yhat",xlim=plim,ylim=plim)
abline(0,1,col="red",lwd=2,lty=2)

## write resv to file
write.csv(data.frame(resv),file="resv-R.csv",row.names=FALSE)
