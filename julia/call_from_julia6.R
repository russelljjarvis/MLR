# Machine Learning Homework 6, 494
# Rob McCulloch
# 3/25/2019
# Contents
# 1. Lasso on the Used Cars Data
# Problem . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 2
# 6
# 2. Logistic Regression with the Kaggle Delinquency Data
# Code for Tabloid Data . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 6
# 8
# 3. Multinomial Logit with the Survey Data 12
# 4. Logistic glmnet 14
# 11. Lasso on the Used Cars Data
# We have looked at the used cars data before.
# We used the file susedcars.csv which has 1,000 observation.
# This time let’s use usedcars.csv which has 20,000 observations and more x variables.
library("compiler")
library(glmnet)

cmpfun(coef)
cmpfun(plot)
cmpfun(lm)
cmpfun(glmnet.cv)

#cd = read.csv("usedcars.csv")
cd = read.csv("http://www.rob-mcculloch.org/data/usedcars.csv")
if(!file.exists("usedcars.csv")){
  res <- tryCatch(download.file("http://www.rob-mcculloch.org/data/usedcars.csv",
                            destfile="./usedcars.csv",
                            method="auto"),
              error=function(e) 1)
  cd = read.csv("usedcars.csv")
  if(dat!=1){ cd = read.csv("usedcars.csv") }
}
dim(cd)
## [1] 1000
#7
summary(cd)
cd$displacement = as.factor(cd$displacement)
summary(cd$displacement)
tdf = data.frame(x1 = as.factor(rep(1:3,3)),x2=rnorm(9))
X = model.matrix(~.,tdf)
#attr(,"assign")
#attr(,"contrasts")
#z`attr(,"contrasts")$x1
x = model.matrix(price~.,cd)[,-1] #drop the one vector
dim(x)
colnames(x)
head(x)
lm1 = lm(price~.,cd)
lm2 = lm(price~.,data.frame(price=cd$price,x))
summary(lm1$fitted-lm2$fitted)
cvglcd = cv.glmnet(x,cd$price,family="gaussian")



glcd = cvglcd$glmnet.fit
plot(cvglcd) #see cross validation
log(Lambda)

plot(glcd) #see Lasso on all the data
# L1 Norm
minlam = cvglcd$lambda.min
minlam1 = cvglcd$lambda.lse
bhatL = coef(glcd,s=minlam)[,1] #coefficents at min lambda, [,1] gets rid of sparse matrix format
bhatL1 = coef(glcd,s=minlam1)[,1] #coefficients at 1se, [,1] gets rid of sparse matrix format
cat("lasso min coefficents\n") #none of them are 0!
# ## lasso min coefficents
# print(bhatL)
# ##
# (Intercept)
# trim500
# trim550
# trimother
# ##
# -4.436170e+06
# 1.503945e+03
# 3.704763e+03
# 9.795653e+03
# ##
# isOneOwnert
# mileage
# year
# colorother
# ##
# 7.146933e+02
# -1.258718e-01
# 2.232864e+03
# -4.604070e+02
# ##
# colorSilver
# colorWhite
# displacement5.5 displacementother
# ##
# -1.144896e+03
# 2.067333e+03
# -9.788869e+03
# -1.122679e+04
# cat("lassa 1se /lasso min coefficient\n")
# ## lassa 1se /lasso min coefficient
# print(bhatL1/bhatL) #two of the 1se coefficients are 0!
# ##
# (Intercept)
# trim500
# trim550
# trimother
# ##
# 1.0638117
# 0.0000000
# 0.2590802
# 0.6813400
# ##
# isOneOwnert
# mileage
# year
# colorother
# ##
# 0.3547576
# 1.0368357
# 1.0630736
# 0.0000000
# ##
# colorSilver
# colorWhite
# displacement5.5 displacementother
# ##
# 0.4432157
# 0.7105184
# 0.6295144
# 0.7203852
# yhat = predict(glcd,newx=x,s=minlam)
# yhat1 = predict(glcd,newx=x,s=minlam1)
# fmat = cbind(lm1$fitted.values,yhat,yhat1) ; colnames(fmat) = c("ols","lassomin","lasso1se")
# pairs(fmat)
# 520000
# 60000
# 0
# ols
# lasso1se
# lassomin
# 0
# 20000
# 60000
# 0
# 20000 40000 60000
# Problem
# Try the elastic net!!
# How do the elastic net results compare with Lasso?
# For example to get the elastic net we use the alpha option:
# cvglcd = cv.glmnet(x,cd$price,family="gaussian",alpha=.5)
# 2. Logistic Regression with the Kaggle Delinquency Data
# Let’s use the data kaggle-del-test.csv and kaggle-del-train.csv from the data webpage. The first file is 75,000
# test data observations and the second file is 75,000 train observations.
# ktr=read.csv("http://www.rob-mcculloch.org/data/kaggle-del-train.csv") #read in the train data
# kte=read.csv("http://www.rob-mcculloch.org/data/kaggle-del-test.csv") #read in the test data
# ktr$DelIn2Yr = as.factor(ktr$DelIn2Yr)
# kte$DelIn2Yr = as.factor(kte$DelIn2Yr)
# names(ktr)
# ## [1] "RevolvingUtilizationOfUnsecuredLines"
# ## [2] "age"
# ## [3] "NumberOfTime30.59DaysPastDueNotWorse"
# ## [4] "DebtRatio"
# ## [5] "NumberOfOpenCreditLinesAndLoans"
# ## [6] "NumberOfTimes90DaysLate"
# ## [7] "NumberRealEstateLoansOrLines"
# ## [8] "NumberOfTime60.89DaysPastDueNotWorse"
# ## [9] "DelIn2Yr"
# dim(ktr)
# ## [1] 75000
# 9
# dim(kte)
# ## [1] 75000
# 9
# table(ktr$DelIn2Yr)/length(ktr$DelIn2Yr)
# 6##
# ##
# 0
# 1
# ## 0.93294667 0.06705333
# table(kte$DelIn2Yr)/length(kte$DelIn2Yr)
# ##
# ##
# 0
# 1
# ## 0.93337333 0.06662667
# This is a simpified version of a Kaggle data set.
# Each observation corresponds to a customer account.
# The response is DelIn2yr, it is a binary indicator indicating whether the account is in default 2 years after
# the time of the explanatory variable measurement.
# The explanantory variable describe the account 2 years ago.
# The names of the variable are (somewhat) self-explanatory.
# 7Problem
# •
# •
# •
# •
# •
# plot DelIn2Yr against age
# (using ktr)run the logistic regression of DelIn2Yr on age and plot P (DelIn2Y r = 1|age) vs. age.
# according to AIC, does it help to use age and NumberOfTimes90DaysLate in the logit?
# plot the out-of-sample lift curves for the logit with and without NumberOfTimes90DaysLate
# plot the ROC and get the AUC out-of-sample with and without NumberOfTimes90DaysLate
# Code for Tabloid Data
# read in data
# ttr=read.csv("http://www.rob-mcculloch.org/data/td1.csv") #read in the train data
# tte=read.csv("http://www.rob-mcculloch.org/data/td2.csv") #read in the test data
# ttr$purchase = as.factor(ttr$purchase)
# tte$purchase = as.factor(tte$purchase)
# names(ttr)
# ## [1] "purchase" "nTab"
# "moCbook" "iRecMer1" "llDol"
# dim(ttr)
# ## [1] 10000
# 5
# dim(tte)
# ## [1] 5000
# 5
# plot y=purchase vs. llDol
# boxplot(llDol~purchase,ttr)
# 0
# 1
# run logit using llDol
# lf = glm(purchase~llDol,ttr,family=binomial)
# print(summary(lf))
# ##
# ## Call:
# ## glm(formula = purchase ~ llDol, family = binomial, data = ttr)
# ##
# ## Deviance Residuals:
# ##
# Min
# 1Q
# Median
# 3Q
# Max
# ## -0.5981 -0.1794 -0.1794 -0.1794
# 2.8768
# ##
# ## Coefficients:
# ##
# Estimate Std. Error z value Pr(>|z|)
# ## (Intercept) -3.51568
# 0.06543 -53.73
# <2e-16 ***
# ## llDol
# 0.26324
# 0.01848
# 14.24
# <2e-16 ***
# 8##
# ##
# ##
# ##
# ##
# ##
# ##
# ##
# ##
# ##
# ---
# Signif. codes:
# 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# (Dispersion parameter for binomial family taken to be 1)
# Null deviance: 2396.5
# Residual deviance: 2221.8
# AIC: 2225.8
# on 9999
# on 9998
# degrees of freedom
# degrees of freedom
# Number of Fisher Scoring iterations: 7
# 9plot in-sample phat
# 0.10
# phatin = predict(lf,ttr,type="response") #without type="response" you get X betahat
# plot(ttr$llDol,phatin)
# −2
# 0
# 2
# ttr$llDol
# add in nTab
# lf1 = glm(purchase~llDol+nTab,ttr,family=binomial)
# print(summary(lf1))
# ##
# ## Call:
# ## glm(formula = purchase ~ llDol + nTab, family = binomial, data = ttr)
# ##
# ## Deviance Residuals:
# ##
# Min
# 1Q
# Median
# 3Q
# Max
# ## -1.7662 -0.1879 -0.1696 -0.1696
# 2.9150
# ##
# ## Coefficients:
# ##
# Estimate Std. Error z value Pr(>|z|)
# ## (Intercept) -3.92792
# 0.08478 -46.332 < 2e-16 ***
# ## llDol
# 0.13309
# 0.02486
# 5.354 8.63e-08 ***
# ## nTab
# 0.10314
# 0.01111
# 9.286 < 2e-16 ***
# ## ---
# ## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ##
# ## (Dispersion parameter for binomial family taken to be 1)
# ##
# ##
# Null deviance: 2396.5 on 9999 degrees of freedom
# ## Residual deviance: 2132.0 on 9997 degrees of freedom
# ## AIC: 2138
# ##
# ## Number of Fisher Scoring iterations: 7
#out of sample lift and ROC/AUC
phat = predict(lf,tte,type="response")
phat1 = predict(lf1,tte,type="response")
source("lift-loss.R")
lift.many.plot(list(phat,phat1),tte$purchase)

library(pROC)
## Type 'citation("pROC")' for a citation.
##
## Attaching package: 'pROC'
## The following object is masked from 'package:glmnet':
##
##
auc
## The following objects are masked from 'package:stats':
##
##
cov, smooth, var
par(mfrow=c(1,2))
par(mai=c(1,1,.5,.5))
rocR = roc(response=tte$purchase,predictor=phat)
AUC = auc(rocR)
plot(rocR)
title(main=paste("log model AUC= ",round(AUC,2)))
rocR = roc(response=tte$purchase,predictor=phat1)
AUC = auc(rocR)
plot(rocR)
title(main=paste("llDol model AUC= ",round(AUC,2)))
#llDol model
AUC= 0.75
#log model
AUC= 0.65
#1.0 0.8 0.6 0.4 0.2 0.0 1.0 0.8 0.6 0.4 0.2 0.0
#Specificity Specificity
#113. Multinomial Logit with the Survey Data
#For this data use the file Ayx.csv:
sd = read.csv("http://www.rob-mcculloch.org/data/Ayx.csv")
facv = c(1,2,3,5,6)
for(i in facv) sd[[i]]=as.factor(sd[[i]]) #convert categoricals to factors
summary(sd)
## yr
yc
gender
age
empstat yrres
## 1: 16
#8
#:233
#1:393
#Min.
#:0.0000
#1:573
#1:178
## 2: 75
#7
#:130
#2:405
#1st Qu.:0.2105
#2:149
#2:620
## 3:236
#6
#:109
#Median :0.3860
#3: 76
## 4:343
#9
#: 86
#Mean
#:0.4518
## 5:128
#10
#: 58
#3rd Qu.:0.7018
##
#11
#: 57
#Max.
#:1.0000
##
#(Other):125
##
#numdep
## Min.
#:0.00000
## 1st Qu.:0.09091
## Median :0.18182
## Mean
#:0.19537
## 3rd Qu.:0.27273
## Max.
#:1.00000
##
dim(sd)
## [1] 798
#7
#The data comes from a survey in which residents of an town were asked how much they like the town. Our
#“y” is yr is which is home much they like it on a 5 point scale (1 don’t like it, 5 like it).
#We won’t use yc which is the same thing on an 11 point scale.
#The other variables are “demographics” describing the respondents.
#demographics:
#The demographic variables capturing respondent characteristics. gender: 1- Male,
#2- Female. age in years. Employment status: 1- salaried employee or army or self-employed,
#2- unemployed or retired, 3- student. years of residence in the city: 1- less than 10 years,
#2-more than 10 years. number of dependents.
#L#et’s run the multinomial logit:
library(nnet)
mfit = multinom(yr~gender+age,data=sd)
## # weights: 20 (12 variable)
## initial value 1284.331454
## iter 10 value 1057.505739
## iter 20 value 1043.415184
## final value 1043.413678
## converged
summary(mfit)
## Call:
## multinom(formula = yr ~ gender + age, data = sd)
##
## Coefficients:
##
#(Intercept)
gender2
age
## 2
#2.739666 -0.1157771 -2.340599
## 3
#4.005943 -0.5856735 -1.996925
### 4
#4.092785 -0.5920310 -1.346938
## 5
#3.061896 -0.5497707 -1.295499
##
## Std. Errors:
##
#(Intercept)
gender2
age
#12##
##
##
##
##
##
##
#2
#3
#4
#5
#0.7322306
#0.6985675
#0.6943810
#0.7126850
#0.5708817
#0.5348835
#0.5291321
#0.5473207
#1.0402684
#0.9705207
#0.9586739
#0.9926548
#Residual Deviance: 2086.827
#AIC: 2110.827
#Now let’s get the fits:
yhat = predict(mfit,sd) #most likely
phat = predict(mfit,sd,type="probs")
yhat[1:5]
## [1] 4 4 4 4 4
## Levels: 1 2 3 4 5
phat[1:5,]
##
#1
#2
#3
## 1 0.03260134 0.09442852 0.2633120
## 2 0.02444743 0.05493901 0.2587548
## 3 0.03260134 0.09442852 0.2633120
## 4 0.03836950 0.08686660 0.2511490
## 5 0.04504509 0.07971008 0.2389478
#confusion matrix
table(yhat,sd$yr)
##
## yhat
#1
#2
#3
#4#
#5
##
#1#
#0
#0
#0
#0
#0
##
#2
#0
#0
#0
#0
#0
##
#3
#0
#0
#0
#0
#0
##
#4 16 75 236 343 128
##
#5
#0
#0
#0
#0
#0
# probs
#4#
#0.4401648
#0.4823537
#0.4401648
#0.4495628
#0.4580113
#5
#0.1694933
#0.1795051
#0.1694933
#0.1740521
#0.1782857
#Problem
#• can you interpret the coefficients from yr~gender+age? Do older people like the town better?
#• run the multinomial logit of yr on all the x variables. What does AIC say about the model compared
#to yr~gender+age?
#• get the in sample confusion matrix for yr on all the x variables. Is it any better than the confusion
#matrix for yr~gender+age?
#134. Logistic glmnet
#Let’s try glmnet on the tabloid data.
#4
#4
#3
#2
#2
#−7
#−6
#−5
#0 3 4 4
#0.0 0.5 1.0 1.5
#4
#4
#4
#4
library(glmnet)
xtr = as.matrix(ttr[,2:5])
ytr = ttr[,1]
set.seed(14)
cvtfit = cv.glmnet(x=xtr, ytr, family="binomial", nfolds=10)
par(mfrow=c(2,2))
plot(cvtfit)
plot(cvtfit$glmnet.fit)
#−4
log(Lambda)
#L1 Norm
#The minlamda solution uses all the variables and the 1se lambda uses 2.
#Let’s have a look.
bhatL = coef(cvtfit$glmnet.fit,s=cvtfit$lambda.min)[,1]
cat("min coefficients:\n")
## min coefficients:
bhatL
## (Intercept)
nTab
moCbook
iRecMer1
llDol
## -2.61835191 0.05585313 -0.03227268 1.66909745 0.07381539
bhatL1 = coef(cvtfit$glmnet.fit,s=cvtfit$lambda.lse)[,1]
cat("1se coefficients:\n")
## 1se coefficients:
bhatL1
## (Intercept)
nTab
moCbook
iRecMer1
llDol
## -3.15753284 0.05738781 -0.01342321 0.00000000 0.00000000
Let’s get the test phats from both models.
xte = as.matrix(tte[,2:5])
probL = predict(cvtfit$glmnet.fit, xte, type="response", s=cvtfit$lambda.min)
probL1 = predict(cvtfit$glmnet.fit, xte, type="response", s=cvtfit$lambda.lse)
plot(probL,probL1)
abline(0,1,col="red")
#140.25
#0.0
#0.1
#0.2
#0.3
# 0.4
# 0.5
 #0.6
# 0.7
#probL
#Let’s have a look at the out of sample lifts.
source("lift-loss.R")
phatL = list(probL,probL1)
lift.many.plot(phatL,tte$purchase)
#0.0
#0.2
#0.4
#0.6
#0.8
#1.0
# % tried
#Problem
# • get the lambda.min and lambda.1se coefficients for the delinquincy data. How do they compare?
#• get the out of sample lift curves for the lambda.min and lambda.1se coefficients for the delinquincy
# data. How do they compare?
#15
