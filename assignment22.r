######################################################################
### See Machine Learning in R by Brett Lantz, Chapter 4
######################################################################

########################################
## read in data
# if the file is in your directory:
smsRaw = read.csv("sms_spam.csv", stringsAsFactors = FALSE)
# or read it off the web:
smsRaw = read.csv("http://www.rob-mcculloch.org/data/sms_spam.csv", stringsAsFactors = FALSE)

## examine the structure of the sms data
is.data.frame(smsRaw)
dim(smsRaw)
names(smsRaw)
str(smsRaw) #has type: ham or spam, text: the text message, goal is to guess type from text

########################################
## convert spam/ham to factor.
smsRaw$type = factor(smsRaw$type)
print(table(smsRaw$type))

########################################
# word cloud visualization
library(wordcloud)
wordcloud(smsRaw$text,max.words = 40, scale = c(3, 0.5))

########################################
## build a corpus using the text mining (tm) package
library(tm)
#volatile (in memory corpus from vector of text in R
smsC = VCorpus(VectorSource(smsRaw$text))
print(smsC)
#smsC is a list of lists where smsC[[i]] is a doc
# and doc[[1]] is content, [[2]] is ``meta'' 
is.list(smsC)
is.list(smsC[[1]])
names(smsC[[1]])

########################################
##inspect is a function from tm
inspect(smsC[1])
inspect(smsC[1:2])

##to see a doc use as.character
# x1 and x4
as.character(smsC[[1]])
as.character(smsC[[4]])
#y1 and y4
smsRaw$type[1]
smsRaw$type[4]

#see first two observations
lapply(smsC[1:2], as.character) #x
smsRaw$type[1:2] #y

#or just pull of content
smsC[[1]]$content


########################################
## clean up the corpus using tm_map()
# tolower changes upper case to lower case
tolower("Rob")
smsCC = tm_map(smsC, content_transformer(tolower))

# show the difference between sms_corpus and corpus_clean
as.character(smsC[[1]])
as.character(smsCC[[1]])

# tm package has a set of transformations
#> getTransformations()
#[1] "removeNumbers"     "removePunctuation" "removeWords"      
#[4] "stemDocument"      "stripWhitespace"

# stopwords are common words that you want to delete because they
#   have no information
stopwords()[1:10]
# [1] "i"         "me"        "my"        "myself"    "we"        "our"      
# [7] "ours"      "ourselves" "you"       "your"

# a bunch more cleanup 
# note that for the functions from tm, you don't have to wrap the function with
#       content_transformer
smsCC = tm_map(smsCC, removeNumbers) # remove numbers
smsCC = tm_map(smsCC, removeWords, stopwords()) # remove stop words
smsCC = tm_map(smsCC, removePunctuation) # remove punctuation
as.character(smsCC[[1]])
as.character(smsCC[[2]])

#note:
#could use:
replacePunctuation <- function(x) { gsub("[[:punct:]]+", " ", x) }
#in place of removePunctuation
#example:
removePunctuation("hello...world")
replacePunctuation("hello...world")

#next is stemming
## use SnowballC for stemming
library(SnowballC)
wordStem(c("learn", "learned", "learning", "learns"))
wordStem(c("help", "helped", "helping", "helps"))

smsCC = tm_map(smsCC, stemDocument)
smsCC <- tm_map(smsCC, stripWhitespace) # eliminate unneeded whitespace


# examine the final clean corpus
lapply(smsC[1:3], as.character)
lapply(smsCC[1:3], as.character)

########################################
## we will be making the DTM (Document Term Matrix), which is a sparse matrix (mostly 0's).
#note on sparse matrices
library(Matrix)
N=10000
sp = sparseMatrix(1:N,1:N,x=1) # NxN identity
m = diag(1,N,N) # non sparse identity
library(pryr) # library of R tools
pryr::object_size(sp)
pryr::object_size(m)

#note that you mask the function inspect from tm when you library(Matrix)
# try
pryr::inspect
tm::inspect

inspect(smsC[1])
tm::inspect(smsC[1])

## create a document-term sparse matrix, rows are docs and columns are terms (words, tokens)
# i,j element is number of times term j is in doc i
if(1) {
  smsDtm <- DocumentTermMatrix(smsCC) #function from tm package
} else { # alternative solution: create a document-term sparse matrix directly from the SMS corpus
  smsDtm2 <- DocumentTermMatrix(smsC, control = list(
    tolower = TRUE,
    removeNumbers = TRUE,
    stopwords = TRUE,
    removePunctuation = TRUE,
    stemming = TRUE
  ))
}

#look at dtm a bit
print(dim(smsDtm))
is.matrix(smsDtm)
is.data.frame(smsDtm)
is.list(smsDtm)
str(smsDtm)

library(slam) #for col_sums, slam: Sparse Lightweight Arrays and Matrices
tcnt = col_sums(smsDtm) #number of times each term is used, summed over documents
summary(tcnt) #summarize total time a term is used.

terms = smsDtm$dimnames$Terms #the column names which are the words=terms
nterm = length(terms)
set.seed(14)
ii = sample(1:nterm,50)
terms[ii] #random selection of the terms

#> names(smsDtm)
#[1] "i"        "j"        "v"        "nrow"     "ncol"     "dimnames"
#the nonzero elements of have count v at row i and column j
#check
# the (ii,jj) element of the DTM matrix is v
ii=smsDtm$i[1]
jj=smsDtm$j[1]
smsDtm$v[1]
terms[jj]
for(i in 1:5) print(terms[smsDtm$j[i]])
smsCC[[1]][1] #or smsCC[[1]]$content 

########################################
#train and test
# creating training and test datasets
smsTrain = smsDtm[1:4169, ]
smsTest  = smsDtm[4170:5559, ]

# also save the labels
smsTrainy = smsRaw[1:4169, ]$type
smsTesty  = smsRaw[4170:5559, ]$type

# check that the proportion of spam is similar
prop.table(table(smsTrainy))
prop.table(table(smsTesty))

########################################
# word cloud visualization
#can call wordcloud using the corpus
wordcloud(smsCC, min.freq = 50, random.order = FALSE)
#can call wordcloud using the raw text
wordcloud(smsRaw$text,max.words = 40, scale = c(3, 0.5))


# subset the training data into spam and ham groups
spamSS  = subset(smsRaw, type == "spam")
hamSS  = subset(smsRaw, type == "ham")

par(mfrow=c(1,2))
wordcloud(spamSS$text, max.words = 40, scale = c(3, 0.5))
wordcloud(hamSS$text, max.words = 40, scale = c(3, 0.5))

#with some color
par(mfrow=c(1,2))
pal = brewer.pal(6,"Dark2")
pal = pal[-(1)]
wordcloud(spamSS$text, max.words = 40, scale = c(3, 0.5),colors=pal)
wordcloud(hamSS$text, max.words = 40, scale = c(3, 0.5),colors=pal)

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

#nicer table
library(gmodels)
CrossTable(yhat, smsTesty,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))




# try it with laplace
smsNB2 = naiveBayes(smsTrain, smsTrainy,laplace=1)
smsNB2$tables[1:3]
yhat2 = predict(smsNB2,smsTest)
CrossTable(yhat2, smsTesty,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))


## not much different
#> (5+28)/1390
#[1] 0.02374101
#> length(yhat)
#[1] 1390
#> (30+6)/1390
#[1] 0.02589928

##################################################
## get tables for (age,adult) from train
# marginal joint of (age,adult)
mtab = table(smsTrain[,"age"],smsTrain[,"adult"])
iiy1 = (smsTrainy=="ham")
c1tab =  table(smsTrain[iiy1,"age"],smsTrain[iiy1,"adult"])
c0tab =  table(smsTrain[!iiy1,"age"],smsTrain[!iiy1,"adult"])

prop.table(mtab)
prop.table(c1tab)
prop.table(c0tab)

CrossTable(smsTrain[,"age"],smsTrain[,"adult"],
           prop.chisq = FALSE, prop.t = FALSE, prop.r = TRUE,prop.c=FALSE,
           dnn = c('age', 'adult'),digits=4)

CrossTable(smsTrain[iiy1,"age"],smsTrain[iiy1,"adult"],
           prop.chisq = FALSE, prop.t = FALSE, prop.r = TRUE,prop.c=FALSE,
           dnn = c('age', 'adult'),digits=4)

CrossTable(smsTrain[!iiy1,"age"],smsTrain[!iiy1,"adult"],
           prop.chisq = FALSE, prop.t = FALSE, prop.r = TRUE,prop.c=FALSE,
           dnn = c('age', 'adult'),digits=4)

#check p(age=yes|ham) (.00138 in notes)
#5/3605
#check p(age=yes|spam) (.02127 in notes)
#12/564


#5/3605

#11
#1

