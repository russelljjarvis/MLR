
#setContentType("image/png")
#t <- tempfile()
#png(t,type="cairo")
#plot(rnorm(10))
#install.packages("randomForest")
#install.packages("gbm")
if("JuliaCall" %in% rownames(installed.packages()) == FALSE) {

   install.packages("JuliaCall",repos = "http://cran.us.r-project.org", dependencies = TRUE)
   library("JuliaCall")
   browser()
   library("compiler")
   library(JuliaCall)
   julia <- julia_setup()
}
#install.packages("debugr",repos = "http://cran.us.r-project.org", dependencies = TRUE)
#library(debugr)
#library(debugr)

#debugr_switchOn()

x11(width = 8, height = 8)
destfile="kaggle-del-train.csv"
if(!file.exists(destfile)){
   ktr=read.csv("http://www.rob-mcculloch.org/data/kaggle-del-train.csv")#read in the train
   kte=read.csv("http://www.rob-mcculloch.org/data/kaggle-del-test.csv")#read in the test data

}else{
   ktr=read.csv("kaggle-del-train.csv")#read in the train
   kte=read.csv("kaggle-del-test.csv")#read in the test data
}
#load(destfile)
library(rpart)

ktr$DelIn2Yr =as.factor(ktr$DelIn2Yr)
kte$DelIn2Yr =as.factor(kte$DelIn2Yr)
print(names(ktr))
set.seed(99)
big.tree =rpart(DelIn2Yr~.,data=ktr, control=rpart.control(cp=.0001))
nbig =length(unique(big.tree$where))
cat("size of big tree: ",nbig,"\n")


#Boosting
library(gbm)#also xgboost is supposed to be good## Loaded gbm 2.1.4# first gbm needs a numeric y, weirdtr
DB = ktr;

trDB = ktr;
trDB$DelIn2Yr =as.numeric(trDB$DelIn2Yr)-1
teDB = kte;
teDB$DelIn2Yr =as.numeric(teDB$DelIn2Yr)

#trDB$DelIn2Yr =as.numeric(trDB$DelIn2Yr)-1
#teDB = kte; teDB$DelIn2Yr =as.numeric(teDB$DelIn2Yr)-1# check the new y's make sensetable(trDB$DelIn2Yr,ktr$DelIn2Yr)####         0     1##   0 69971     0##   1     0  5029table(teDB$DelIn2Yr,kte$DelIn2Yr)####         0     1##   0 70003     0##   1     0  4997#fit boostingbfit =gbm(DelIn2Yr~.,trDB, distribution="bernoulli",n.trees=500,interaction.depth=3,shrinkage=.05)byhattest =predict(bfit,newdata=teDB,n.trees=500,type="response")

library(randomForest)## randomForest 4.6-14##
# Type rfNews() #to see new features/changes/bug
#fixes.set.seed(99)
rffit =randomForest(DelIn2Yr~.,data=ktr,mtry=3,ntree=500)
plot(rffit)


plotcp(big.tree)
rfyhattest =predict(rffit,newdata=kte,type="prob")[,2]
#again, second column is p(y=1|x)
summary(rfyhattest)##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.## 0.00000 0.00200 0.01200 0.06103 0.05000 0.99400
source("http://www.rob-mcculloch.org/2019_ml/webpage/notes/lift-loss.R")
#lift.plot(yhattest,kte$DelIn2Yr,cex.lab=1.2)
lift.plot(rfyhattest,kte$DelIn2Yr,cex.lab=1.2)

varImpPlot(rffit)

#Boosting
library(gbm)#also xgboost is supposed to be good#
# Loaded gbm 2.1.4# first gbm needs a numeric y, weird
trDB = ktr; trDB$DelIn2Yr =as.numeric(trDB$DelIn2Yr)-1
teDB = kte; teDB$DelIn2Yr =as.numeric(teDB$DelIn2Yr)-1# check the new y's make sensetable(trDB$DelIn2Yr,ktr$DelIn2Yr)####         0     1##   0 69971     0##   1     0  5029
table(teDB$DelIn2Yr,kte$DelIn2Yr)
#fit
#boosting
bfit =gbm(DelIn2Yr~.,trDB, distribution="bernoulli",n.trees=500,interaction.depth=3,shrinkage=.05)
byhattest =predict(bfit,newdata=teDB,n.trees=500,type="response")
browser()
#fit boosting
#bfit =gbm(DelIn2Yr~.,trDB, distribution="bernoulli",n.trees=500,interaction.depth=3,shrinkage=.05)
#byhattest =predict(bfit,newdata=teDB,n.trees=500,type="response")


#yhatL =list(yhattest,rfyhattest,byhattest)
#lift.many.plot(yhatL,kte$DelIn2Yr)
#legend("topleft",legend=c("tree","random forests","boosting"),lwd=rep(3,1),col=1:3,bty="n",cex=.8)
#Homework Problem
#Try changingmtry
#in random forests to something bigger than the 3
#I used in the code above.Try a different setting
#for the boosting fit.
#Your choice.Use plots (e.g lift)
# to compare to new random forests and boosting fits with the old ones.
#Is it any better?

#dev.off()

## size of big tree:  376head(big.tree$where)## 1 2 3 4 5 6## 5 5 5 5 5 5
