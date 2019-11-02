##################################################
### get out of sample rmse for susedcars data using linear regression

## import
import math
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn.linear_model import LinearRegression

## read in data
cd = pd.read_csv("http://www.rob-mcculloch.org/data/susedcars.csv") #cd for car data
n = cd.values.shape[0]

## pull off (price,mileage,year) and divide price and mileage by 1000
cd = cd[['price','mileage','year']]
cd['price'] = cd['price']/1000
cd['mileage'] = cd['mileage']/1000
print(cd.head()) # head just prints out the first few rows

## get numpy X and y
X = cd[['mileage','year']].values  #mileage and year columns as a numpy array
y = cd['price'].values #price as a numpy vector

## train/test split
nsamp = 500 # number of train/test plots
trainfrac = .75 # percent of data in train, rest is test
ntrain = math.floor(trainfrac*n)
ntest = n - ntrain
print("nsamp,n,ntrain,ntest: ",nsamp,n,ntrain,ntest)

## helper functions
def rmsef(y,yhat):
   'compute root mean squared error'
   return np.sqrt(np.mean((y-yhat)**2))

def trteind(n,ntrain):
   'gets indices for a train test split'
   itr = list(np.random.choice(n,ntrain,replace=False))
   alli = list(range(n))
   ite = list(set(alli) - set(itr))
   return {'tr':itr,'te':ite}

## store results
resv = np.zeros(nsamp)
ysM = np.zeros((ntest,nsamp))
yhatM = np.zeros((ntest,nsamp))

## loop over train/test
np.random.seed(99)
for i in np.arange(nsamp):
   if(not(i%5)): print("on sample: ",i)
   
   #train/test
   ii = trteind(n,ntrain)
   Xtr = X[ii['tr'],:]; ytr = y[ii['tr']]
   Xte = X[ii['te'],:]; yte = y[ii['te']]

   #fit on train
   lmmod = LinearRegression(fit_intercept=True)
   lmmod.fit(Xtr,ytr)

   #predict on test
   yhatte = lmmod.predict(Xte)

   #record loss
   resv[i] = rmsef(yte,yhatte)

   # keep sampled y and yhat
   ysM[:,i] = yte
   yhatM[:,i] = yhatte


##plot results
plt.scatter(range(nsamp),resv)
plt.scatter(ysM.flatten(),yhatM.flatten())
plt.show()
## write resv to file
resvDf = pd.DataFrame(resv,columns=['resv'])
resvDf.to_csv('resv-P.csv',index=False)
