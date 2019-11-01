# install.packages("glmnet",dependencies=TRUE)
library(glmnet)
library(compiler)
library(JuliaCall)

cd =read.csv("http://www.rob-mcculloch.org/data/usedcars.csv")
dim(cd)
summary(cd)

cd$displacement =as.factor(cd$displacement)
summary(cd$displacement)

x =model.matrix(price~.,cd)
dim(x)
colnames(x)
head(x)
lm1 =lm(price~.,cd)
lm2 =lm(price~.,data.frame(price=cd$price,x))

summary(lm1$fitted-lm2$fitted)
price_frame = data.frame(price=cd$price,x)
browser()
#lm3 =glmnet(price~.,price_frame)
#print(lm3)
# summary(lm2$fitted-lm3$fitted)

# glmnet(x, y, family=c("gaussian","binomial","poisson","multinomial","cox","mgaussian"),
# weights, offset=NULL, alpha = 1, nlambda = 100,
# lambda.min.ratio = ifelse(nobs
x=matrix(rnorm(100*20),100,20)
y=rnorm(100)
g2=sample(1:2,100,replace=TRUE)
g4=sample(1:4,100,replace=TRUE)
fit1=glmnet(x,y)
predict(fit1,newx=x[1:5,],s=c(0.01,0.005))
predict(fit1,type="coef")
plot(fit1,xvar="lambda")
fit2=glmnet(x,g2,family="binomial")
predict(fit2,type="response",newx=x[2:5,])
predict(fit2,type="nonzero")
fit3=glmnet(x,g4,family="multinomial")
predict(fit3,newx=x[1:3,],type="response",s=0.01)
