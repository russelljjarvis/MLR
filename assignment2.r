#setContentType("image/png")
if("tm" %in% rownames(installed.packages()) == FALSE) {

  update.packages(checkBuilt=TRUE, ask=FALSE,repos = "http://cran.us.r-project.org", dependencies = TRUE)
  install.packages("tm",repos = "http://cran.us.r-project.org", dependencies = TRUE)
  install.packages("SnowballC",repos = "http://cran.us.r-project.org", INSTALL_opts = "--no-clean-on-error")
  install.packages("e1071",repos = "http://cran.us.r-project.org", INSTALL_opts = "--no-clean-on-error")
  install.packages("doParallel",repos = "http://cran.us.r-project.org", INSTALL_opts = "--no-clean-on-error")
  install.packages("txtplot",repos = "http://cran.us.r-project.org", INSTALL_opts = "--no-clean-on-error")

}
if("compiler" %in% rownames(installed.packages()) == FALSE) {
  install.packages("compiler",repos = "http://cran.us.r-project.org", INSTALL_opts = "--no-clean-on-error")

}

mysummary <- function(x) {
  # an example of a function that can be mapped (applied in R).
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


destfile = "sms_spam.csv"
if (!file.exists(destfile)) {
  smsRaw = read.csv("http://www.rob-mcculloch.org/data/sms_spam.csv", stringsAsFactors = FALSE)
  write.csv(smsRaw, file = destfile)

}
smsRaw = read.csv("sms_spam.csv")

destfile = "oosloop.RData"
if (file.exists(destfile)) {
  load("oosloop.RData")
  # un comment to dir() on data types:
  #for ( obj in ls() ) { cat('---',obj,'---\n'); print(get(obj)) }
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

nsamp= 8 #number of random train/test splits
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
residues = rep(0,nfr)
resM = matrix(0.0,nsamp,nfr) #store out of sample missclassifcation rates here

ptm <- proc.time()
#wfreqv = c(4,5,7,8,9,10,11,12)

smsNB = naiveBayes(smsTrain, smsTrainy, laplace=1)
yhat = predict(smsNB,smsTest)
ctab = table(yhat,smsTesty)
ctab
misclass = (sum(ctab)-sum(diag(ctab)))/sum(ctab)
perspam = ctab[2,2]/sum(ctab[,2])
cat("misclass,perspam: ", misclass,perspam,"\n")
#resP = foreach(i=1:nsamp,.combine=rbind) %dopar% {
i = 1
#if( (i%%1)==0) cat("on sample ",i,"\n")

ii = sample(1:n,floor(trainfrac*n)) # randomly choose a subsample.

# use this random sub sample to make train and test splits.
smsTrain = smsDtm[ii, ]
smsTest  = smsDtm[-ii, ]
smsTrainy = smsRaw[ii, ]$type
smsTesty  = smsRaw[-ii, ]$type

resv=rep(0,nfr)
for(j in 1:nfr) {
  #pull off columns with frequent words and then convert count to binary
  smsFreqWords = findFreqTerms(smsTrain, 8)
  print(smsFreqWords)
  len = length(smsFreqWords)
  #print(len)
  #stopifnot(len>2)

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
  residues[j] = misclass

}
sink("age-adult-tables.txt")

table(smsAge1,smsAdult1)
fname = paste('tables',i,'1.png')
png(fname,type="cairo")
table(smsAge0,smsAdult0)
fname = paste('tables',i,'2.png')
png(fname,type="cairo")
save(residues,file="oosloop.RData")
#}

#browser()
i = 1
#if( (i%%1)==0) cat("on sample ",i,"\n")

ii = sample(1:n,floor(trainfrac*n)) # randomly choose a subsample.

# use this random sub sample to make train and test splits.
smsTrain = smsDtm[ii, ]
smsTest  = smsDtm[-ii, ]
smsTrainy = smsRaw[ii, ]$type
smsTesty  = smsRaw[-ii, ]$type

resv=rep(0,nfr)
for(j in 1:nfr) {
  #pull off columns with frequent words and then convert count to binary
  smsFreqWords = findFreqTerms(smsTrain, 8)
  print(smsFreqWords)
  len = length(smsFreqWords)
  #print(len)
  #stopifnot(len>2)

  smsFreqTrain = smsTrain[ , smsFreqWords]
  smsFreqTest = smsTest[ , smsFreqWords]
  smsTrainB = apply(smsFreqTrain, MARGIN = 2, convertCounts)
  smsTestB  = apply(smsFreqTest, MARGIN = 2, convertCounts)
  # fit NM on train
  smsNB = naiveBayes(smsTrainB, smsTrainy,laplace=2)
  # predict on test
  yhat = predict(smsNB,smsTestB)
  # store oos missclass
  ctab = table(yhat,smsTesty)
  misclass = (sum(ctab)-sum(diag(ctab)))/sum(ctab)
  residues[j] = misclass

}
sink("age-adult-tables.txt")

table(smsAge1,smsAdult1)
fname = paste('tables_laplace2',i,'1.png')
png(fname,type="cairo")
table(smsAge0,smsAdult0)
fname = paste('tables_laplace2',i,'2.png')
png(fname,type="cairo")
save(residues,file="oosloop.RData")

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
