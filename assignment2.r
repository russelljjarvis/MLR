source('clutter.R')

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
