source('clutter.R')
########################################
# save frequently-appearing terms to a character vector and then drop infrequent terms (cols of DTM)
smsFreqWords = findFreqTerms(smsTrain, 5) #words that appear at leat 5 times
str(smsFreqWords)
length(smsFreqWords)

# create DTMs with only the frequent terms
smsFreqTrain = smsTrain[ , smsFreqWords]
smsFreqTest = smsTest[ , smsFreqWords]

########################################
#convert counts to if(count>0) (yes,no)
convertCounts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}
# apply() convert_counts() to columns of train/test data
# these are just matrices
smsTrain = apply(smsFreqTrain, MARGIN = 2, convertCounts)
smsTest  <- apply(smsFreqTest, MARGIN = 2, convertCounts)
#check out smsTrain

is.matrix(smsTrain)
typeof(smsTrain)
str(smsTrain)

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



#wfreqv = c(4,5,7,8,9,10,11,12)
#nfr = length(wfreqv)
#residues = rep(0,nfr)
#resM = matrix(0.0,nsamp,nfr) #store out of sample missclassifcation rates here

ptm <- proc.time()
#wfreqv = c(4,5,7,8,9,10,11,12)
# use this random sub sample to make train and test splits.
smsTrain = smsDtm[ii, ]
smsTest  = smsDtm[-ii, ]
smsTrainy = smsRaw[ii, ]$type
smsTesty  = smsRaw[-ii, ]$type

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

#ii = sample(1:n,floor(trainfrac*n)) # randomly choose a subsample.
#ii = sample(1:n,floor(trainfrac*n))
#smsTrain = smsDtm[ii, ]
#smsTest  = smsDtm[-ii, ]
#smsTrainy = smsRaw[ii, ]$type
#smsTesty  = smsRaw[-ii, ]$type


########################################
## finally! do NB
# use e1071 package (note also klaR package).

library(e1071)

smsNB = naiveBayes(smsTrain, smsTrainy)
smsNB$tables[1:3]

#pred out of sample
yhat = predict(smsNB,smsTest)

#simple table
table(yhat,smsTesty)
browser()
resv=rep(0,nfr)
for(j in 1:nfr) {
  #pull off columns with frequent words and then convert count to binary
  smsFreqWords = findFreqTerms(smsTrain, 8)
  print(smsFreqWords)
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
