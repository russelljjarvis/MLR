##################################################
### read in date, make y=ham/spam a factor
smsRaw = read.csv("http://www.rob-mcculloch.org/data/sms_spam.csv", stringsAsFactors = FALSE)
# convert spam/ham to factor.
smsRaw$type = factor(smsRaw$type)
#install.packages("e1071")
#install.packages("tm",dependencies=TRUE)
install.packages("doParallel")
install.packages("doSNOW")
install.packages("wordcloud")

install.packages("doParallel") 
install.packages("remotes")
remotes::install_github("centerforopenscience/osfr")
install.packages("compiler")
library(osfr)
osf_retrieve_file("xfzy6") %>% 
  osf_download()
#cr_project <- osf_retrieve_node("https://osf.io/xfzy6")di
#neuronunit_project <- osf_retrieve_node("bxc3g") #neuronunit
#wc_project <- osf_retrieve_node("yng5u") #word complexity
#wc_project <- wc_project.osf_download()
#print(wc_project)

library(compiler)
f <- function(n, x) for (i in 1:n) x = (1 + x)^(-1)
g <- cmpfun(f)
system('wget https://osf.io/xfzy6/download')
aaply(seq(1,10000,100), function(x) rnorm(1, mean=x+(1:100), .parallel=TRUE)


      #osf_download('https://osf.io/xfzy6', path = outfile)

# create an external data file
write.csv(mtcars, "mtcars.csv")

osf_create_project(title = "Motor Trend Car Road Tests") %>%
  osf_create_component("Car Data") %>%
  osf_mkdir("rawdata") %>%
  osf_upload("mtcars.csv") %>%
  osf_open()
osf_ls_files(cr_project)

install.packages("doMPI")
library(doParallel)


##################################################
### get and clean corpus
# build a corpus using the text mining (tm) package
library(tm)
#install.packages("tm",dependencies=TRUE)
library(SnowballC)
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
wfreqv = c(5,10,50)
nfr = length(wfreqv)

##################################################
### train/test loop
#convert counts to if(count>0) (yes,no)
convertCounts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

nsamp= 16 #number of random train/test splits
trainfrac = .75 #percent of data to put in train

# residuals?
resM = matrix(0.0,nsamp,nfr) #store out of sample missclassifcation rates here
data_store0 = matrix(0.0,nsamp,nfr) #store out of sample missclassifcation rates here
data_store1 = matrix(0.0,nsamp,nfr) 

set.seed(99)
n = nrow(smsDtm) #total sample size
library(e1071)

# Start the clock!
ptm <- proc.time()

##################################################
### use for each to run the loop in parallel
library(doParallel)
sessionInfo() #see what packages were loaded
registerDoParallel(cores=8)
cat("number of workers is: ",getDoParWorkers(),"\n")


set.seed(99)
# Start the clock!
ptm <- proc.time()
#foreach(i=1:nsamp,.combine=rbind) %dopar% { cat("on sample ",i,"\n") }
foreach(i=1:10) %dopar% {cat(i)}
set.seed(99)
# Start the clock!
ptm <- proc.time()
cat(nsamp)
resP = foreach(i=1:nsamp) %dopar% { cat(i) }  
# In this file we randomly do several train, test splits for various choices of the tuning parameter "frequent words"
# for(i in 1:nsamp) {
# resP = foreach(i=1:nsamp) %dopar% {
resP = foreach(i=1:nsamp,.combine=rbind) %dopar% { 
  cat("on sample ",i,"\n") 
  
  if( (i%%1)==0) cat("on sample ",i,"\n")
  ii = sample(1:n,floor(trainfrac*n))
  smsTrain = smsDtm[ii, ]
  smsTest  = smsDtm[-ii, ]
  smsTrainy = smsRaw[ii, ]$type
  smsTesty  = smsRaw[-ii, ]$type
  
  for(j in 1:nfr) {
    #pull off columns with frequent words and then convert count to binary
    cat("cut off word frequency ", j, , wfreqv[j])
    smsFreqWords = findFreqTerms(smsTrain, wfreqv[j])
    smsFreqTrain = smsTrain[ , smsFreqWords]
    smsFreqTest = smsTest[ , smsFreqWords]
    smsTrainB = apply(smsFreqTrain, MARGIN = 2, convertCounts)
    smsTestB  = apply(smsFreqTest, MARGIN = 2, convertCounts)
    # fit NM on train
    smsNB = naiveBayes(smsTrainB, smsTrainy)
    
    #smsNB2$tables[1:3]
    
    # predict on test
    yhat = predict(smsNB,smsTestB)
    # store oos missclass
    ctab = table(yhat,smsTesty)
    # misclassification
    misclass = (sum(ctab)-sum(diag(ctab)))/sum(ctab)
    resM[i,j] = misclass
    
  }
}
resP = foreach(i=1:nsamp,.combine=rbind) %dopar% {
  if( (i%%1)==0) cat("on sample ",i,"\n")
  ii = sample(1:n,floor(trainfrac*n))
  smsTrain = smsDtm[ii, ]
  smsTest  = smsDtm[-ii, ]
  smsTrainy = smsRaw[ii, ]$type
  smsTesty  = smsRaw[-ii, ]$type
  
  for(j in 1:nfr) {
    #pull off columns with frequent words and then convert count to binary
    cat("cut off word frequency ", j, , wfreqv[j])
    smsFreqWords = findFreqTerms(smsTrain, wfreqv[j])
    smsFreqTrain = smsTrain[ , smsFreqWords]
    smsFreqTest = smsTest[ , smsFreqWords]
    smsTrainB = apply(smsFreqTrain, MARGIN = 2, convertCounts)
    smsTestB  = apply(smsFreqTest, MARGIN = 2, convertCounts)
    # fit NM on train
    #smsNB = naiveBayes(smsTrainB, smsTrainy)
    
    smsNB2 = naiveBayes(smsTrainB, smsTrainy,laplace=2)
    cat(smsNB2)
    yhat2 = predict(smsNB2,smsTestB)
    cat(yhat2)
    # store oos missclass
    ctab2 = table(yhat2,smsTesty)
    misclass2 = (sum(ctab2)-sum(diag(ctab2)))/sum(ctab2)
    cat(misclass2)
    data_store0[i,j] = misclass2
  }
}

resP = foreach(i=1:nsamp,.combine=rbind) %dopar% {
  if( (i%%1)==0) cat("on sample ",i,"\n")
  ii = sample(1:n,floor(trainfrac*n))
  smsTrain = smsDtm[ii, ]
  smsTest  = smsDtm[-ii, ]
  smsTrainy = smsRaw[ii, ]$type
  smsTesty  = smsRaw[-ii, ]$type
  
  for(j in 1:nfr) {
    #pull off columns with frequent words and then convert count to binary
    cat("cut off word frequency ", j, , wfreqv[j])
    smsFreqWords = findFreqTerms(smsTrain, wfreqv[j])
    smsFreqTrain = smsTrain[ , smsFreqWords]
    smsFreqTest = smsTest[ , smsFreqWords]
    smsTrainB = apply(smsFreqTrain, MARGIN = 2, convertCounts)
    smsTestB  = apply(smsFreqTest, MARGIN = 2, convertCounts)
    # fit NM on train
    #smsNB = naiveBayes(smsTrainB, smsTrainy)
    
    smsNB3 = naiveBayes(smsTrainB, smsTrainy,laplace=3)
    cat(smsNB3)
    yhat3 = predict(smsNB2,smsTestB)
    cat(yhat3)
    # store oos missclass
    ctab2 = table(yhat3,smsTesty)
    misclass3 = (sum(ctab3)-sum(diag(ctab3)))/sum(ctab3)
    cat(misclass3)
    data_store1[i,j] = misclass2
  }
}
# Stop the clock
tottime = proc.time() - ptm
cat("time:\n")
print(tottime)

#graph results
boxplot(resM)


##################################################
### use for each to run the loop in parallel
library(doParallel)
sessionInfo() #see what packages were loaded
resregisterDoParallel(cores=8)
cat("number of workers is: ",getDoParWorkers(),"\n")

set.seed(99)
# Start the clock!
ptm <- proc.time()

# In this file we randomly do several train, test splits for various choices of the tuning parameter "frequent words"

resP = foreach(i=1:nsamp,.combine=rbind) %dopar% {
  if( (i%%1)==0) cat("on sample ",i,"\n")
  
  ii = sample(1:n,floor(trainfrac*n))
  smsTrain  = smsDtm[ii, ]
  cat("iterator looks like",ii,"\n")
  smsTest   = smsDtm[-ii, ]
  smsTrainy = smsRaw[ii, ]$type
  smsTesty  = smsRaw[-ii, ]$type
  
  resv = rep(0,nfr)
  for(j in 1:nfr) {
    #pull off columns with frequent words and then convert count to binary
    # this is difernet for each iteration of the loop.
    smsFreqWords = findFreqTerms(smsTrain, wfreqv[j])
    
    smsFreqTrain = smsTrain[ , smsFreqWords] # Train split
    smsFreqTest = smsTest[ , smsFreqWords] # Test split
    smsTrainB = apply(smsFreqTrain, MARGIN = 2, convertCounts)
    smsTestB  = apply(smsFreqTest, MARGIN = 2, convertCounts)
    # fit NM on train
    smsNB = naiveBayes(smsTrainB, smsTrainy)
    # predict on test
    yhat = predict(smsNB,smsTestB)
    # store oos missclass
    ctab = table(yhat,smsTesty)
    misclass = (sum(ctab)-sum(diag(ctab)))/sum(ctab)
    resv[j]=misclass
  }
  resv
}
# Stop the clock
tottimeP = proc.time() - ptm
cat("time:\n")
print(tottimeP)

##plot results
boxplot(resP)


## compare results
qqplot(as.double(resM),as.double(resP))
abline(0,1)

save(resM,tottime,resP,tottimeP,file="oosloop.RData")


##################################################
### pull off age, adult tables
ii= 1:4169
smsTrain = smsDtm[ii, ]
smsTrainy = smsRaw[ii, ]$type
smsFreqWords = findFreqTerms(smsTrain, 5) #words that appear at leat 5 times
smsFreqTrain = smsTrain[ , smsFreqWords]
smsTrainB = apply(smsFreqTrain, MARGIN = 2, convertCounts)

iiy1 = (smsTrainy=="ham")
smsAge1 = smsTrainB[iiy1,"age"]
smsAdult1 = smsTrainB[iiy1,"adult"]
smsAge0 = smsTrainB[!iiy1,"age"]
smsAdult0 = smsTrainB[!iiy1,"adult"]

sink("age-adult-tables.txt")
table(smsAge1,smsAdult1)
table(smsAge0,smsAdult0)
sink()

