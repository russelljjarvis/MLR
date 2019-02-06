#setContentType("image/png")
if("tm" %in% rownames(installed.packages()) == FALSE) {

  update.packages(checkBuilt=TRUE, ask=FALSE,repos = "http://cran.us.r-project.org", dependencies = TRUE)
  install.packages("tm",repos = "http://cran.us.r-project.org", dependencies = TRUE)
  # install.packages("devtools")
  #devtools::install_github("igraph/rigraph")
  #devtools::install_github("KlausVigo/kknn")
  install.packages("SnowballC",repos = "http://cran.us.r-project.org", INSTALL_opts = "--no-clean-on-error")
  install.packages("e1071",repos = "http://cran.us.r-project.org", INSTALL_opts = "--no-clean-on-error")
  install.packages("doParallel",repos = "http://cran.us.r-project.org", INSTALL_opts = "--no-clean-on-error")
  install.packages("txtplot",repos = "http://cran.us.r-project.org", INSTALL_opts = "--no-clean-on-error")

}
if("compiler" %in% rownames(installed.packages()) == FALSE) {
  install.packages("compiler",repos = "http://cran.us.r-project.org", INSTALL_opts = "--no-clean-on-error")

}


mysummary <- function(x) {

  center <- median(x); spread <- mad(x)
  result <- list(center=center,spread=spread)
  return(result)
}


library(tm)
library(SnowballC)
library(doParallel)
library(e1071)
library(txtplot)
library("compiler")

cmpfun(findFreqTerms)
cmpfun(table)
cmpfun(naiveBayes)
cmpfun(predict)

#dev.off()

# install.packages("igraph",repos = "http://cran.us.r-project.org", dependencies = TRUE)
# install.packages("kknn",repos = "http://cran.us.r-project.org", dependencies = TRUE)
# install.packages("igraph",repos = "http://cran.us.r-project.org", dependencies = TRUE)
# install.packages("kknn",repos = "http://cran.us.r-project.org", dependencies = TRUE)

# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014")
# Clean workspace
rm(list=ls())
# Set working directory
# setwd("~/git/R_code_assignments")

destfile = "sms_spam.csv"
if (!file.exists(destfile)) {
  smsRaw = read.csv("http://www.rob-mcculloch.org/data/sms_spam.csv", stringsAsFactors = FALSE)

  #usedcars = read.csv("http://www.rob-mcculloch.org/data/usedcars.csv")
  write.csv(smsRaw, file = destfile)

}
smsRaw = read.csv("sms_spam.csv")

destfile = "oosloop.RData"
if (file.exists(destfile)) {
  load("oosloop.RData")
  #for ( obj in ls() ) { cat('---',obj,'---\n'); print(get(obj)) }
  #browser()
}

##################################################
### read in date, make y=ham/spam a factor
# convert spam/ham to factor.
smsRaw$type = factor(smsRaw$type)


##################################################
### get and clean corpus
# build a corpus using the text mining (tm) package
#volatile (in memory corpus from vector of text in R
smsC = VCorpus(VectorSource(smsRaw$text))
# clean up the corpus using tm_map()
smsCC = tm_map(smsC, content_transformer(tolower)) #upper -> lower
smsCC = tm_map(smsCC, removeNumbers) # remove numbers
smsCC = tm_map(smsCC, removeWords, stopwords()) # remove stop words
smsCC = tm_map(smsCC, removePunctuation) # remove punctuation
smsCC = tm_map(smsCC, stemDocument) #stemming
smsCC = tm_map(smsCC, stripWhitespace) # eliminate unneeded whitespace

##################################################
### create Document Term Matrix
smsDtm = DocumentTermMatrix(smsCC)
dim(smsDtm)


##################################################
### tuning parameter choices

##################################################
### train/test loop
#convert counts to if(count>0) (yes,no)
convertCounts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

nsamp= 16 #number of random train/test splits
trainfrac = .75 #percent of data to put in train


set.seed(99)
n = nrow(smsDtm) #total sample size



##################################################
### use for each to run the loop in parallel
sessionInfo() #see what packages were loaded
registerDoParallel(cores=8)
cat("number of workers is: ",getDoParWorkers(),"\n")

set.seed(99)


ii = sample(1:n,floor(trainfrac*n))
smsTrain = smsDtm[ii, ]
smsTest  = smsDtm[-ii, ]
smsTrainy = smsRaw[ii, ]$type
smsTesty  = smsRaw[-ii, ]$type

wfreqv = c(4,5,7,8,9,10,11,12)
nfr = length(wfreqv)
resv = rep(0,nfr)
resM = matrix(0.0,nsamp,nfr) #store out of sample missclassifcation rates here

ptm <- proc.time()
wfreqv = c(4,5,7,8,9,10,11,12)


resP = foreach(i=1:nsamp,.combine=rbind) %dopar% {
   if( (i%%1)==0) cat("on sample ",i,"\n")

   ii = sample(1:n,floor(trainfrac*n))
   smsTrain = smsDtm[ii, ]
   smsTest  = smsDtm[-ii, ]
   smsTrainy = smsRaw[ii, ]$type
   smsTesty  = smsRaw[-ii, ]$type

   resv=rep(0,nfr)
   for(j in 1:nfr) {
      #print('cutoff',wfreqv[j])
      #print(summary(wfreqv[j]))
      #print(mean(wfreqv[j]))
      #pull off columns with frequent words and then convert count to binary
      smsFreqWords = findFreqTerms(smsTrain, wfreqv[j])
      print(smsFreqWords)
      #print(length(smsFreqWords[1]), 'the length of smsFreqWords')
      len = length(smsFreqWords)
      print(len)
      stopifnot(len>2)

      smsFreqTrain = smsTrain[ , smsFreqWords]
      smsFreqTest = smsTest[ , smsFreqWords]
      smsTrainB = apply(smsFreqTrain, MARGIN = 2, convertCounts)
      smsTestB  = apply(smsFreqTest, MARGIN = 2, convertCounts)
      # fit NM on train
      smsNB = naiveBayes(smsTrainB, smsTrainy)
      # predict on test
      yhat = predict(smsNB,smsTestB)
      # store oos missclass
      ctab = table(yhat,smsTesty)
      misclass = (sum(ctab)-sum(diag(ctab)))/sum(ctab)
      resv[j] = misclass
      resM[j] = misclass
      #print('gets b4 save',j)

      #print('gets after save',j)


      #print(summary(resM))
   }
   save(resM,file="oosloop.RData")
   #resv
}




##################################################
### pull off age, adult tables
ii= 1:4169
smsTrain = smsDtm[ii, ]
smsTrainy = smsRaw[ii, ]$type
#cutoff = c(5,10)
#for c in cutoff
smsFreqWords = findFreqTerms(smsTrain, 5) #words that appear at leat 5 times
smsFreqTrain = smsTrain[ , smsFreqWords]
smsTrainB = apply(smsFreqTrain, MARGIN = 2, convertCounts)

iiy1 = (smsTrainy=="ham")
smsAge1 = smsTrainB[iiy1,"age"]
smsAdult1 = smsTrainB[iiy1,"adult"]
smsAge0 = smsTrainB[!iiy1,"age"]
smsAdult0 = smsTrainB[!iiy1,"adult"]

sink("age-adult-tables.txt")
png('tables1.png',type="cairo")

table(smsAge1,smsAdult1)
png('tables2.png',type="cairo")

table(smsAge0,smsAdult0)
#dev.off()

#sink()


if (1==2){
  # comment out
  # Start the clock!
  ptm <- proc.time()
  for(i in 1:nsamp) {
     if( (i%%1)==0) cat("on sample ",i,"\n")

     ii = sample(1:n,floor(trainfrac*n))
     smsTrain = smsDtm[ii, ]
     smsTest  = smsDtm[-ii, ]
     smsTrainy = smsRaw[ii, ]$type
     smsTesty  = smsRaw[-ii, ]$type

     for(j in 1:nfr) {
        #pull off columns with frequent words and then convert count to binary
        smsFreqWords = findFreqTerms(smsTrain, wfreqv[j])
        smsFreqTrain = smsTrain[ , smsFreqWords]
        smsFreqTest = smsTest[ , smsFreqWords]
        smsTrainB = apply(smsFreqTrain, MARGIN = 2, convertCounts)
        smsTestB  = apply(smsFreqTest, MARGIN = 2, convertCounts)
        # fit NM on train
        smsNB = naiveBayes(smsTrainB, smsTrainy)
        # predict on test
        yhat = predict(smsNB,smsTestB)
        # store oos missclass
        ctab = table(yhat,smsTesty)
        misclass = (sum(ctab)-sum(diag(ctab)))/sum(ctab)
        resM[i,j]=misclass
     }
  }
  # Stop the clock
  tottime = proc.time() - ptm
  cat("time:\n")
  print(tottime)
}
