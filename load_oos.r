destfile = "oosloop.RData"
if (file.exists(destfile)) {
  load("oosloop.RData")
  for ( obj in ls() ) { cat('---',obj,'---\n'); print(get(obj)) }

}
library("compiler")

cmpfun(png)
cmpfun(boxplot)
cmpfun(plot)

browser()

for(i in 1:nrow(resM)) { for(j in 1:ncol(resM)) { print(i,j) } }
# Stop the clock
#graph results
#setContentType("image/png")
#t <- tempfile()
png('resv0.png',type="cairo")

#x11(width = 8, height = 8)
#plot(t[1:24], type = "l")
for(i in resM[,1]){
  boxplot(i)
}
#plot(rnorm(10))
dev.off()
#tottimeP = proc.time() - ptm
#cat("time:\n")
#print(tottimeP)

##plot results
#setContentType("image/png")
#t <- tempfile()
png('resv1.png',type="cairo")

for(i in resM[,1]){
  boxplot(resM[,1])
}

dev.off()

png('final.png',type="cairo")
plot(resM[,1])
dev.off()

#setContentType("image/png")
#t <- tempfile()
png(type="cairo")

## compare results
#qqplot(as.double(resM),as.double(resP))
#abline(0,1)
