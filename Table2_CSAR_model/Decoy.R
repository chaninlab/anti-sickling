#######Load package
library(RWeka)
library(caret)
library(randomForest)
library(kernlab)
library(e1071)
library(corrplot)
library(C50)
library(nnet)
library(GA)
library(cvTools) 
library(Metrics)
library(MASS)
library(pls)
## AtomPairs2DFingerprintCount.csv
## AtomPairs2DFingerprinter.csv
## EStateFingerprinter.csv
## ExtendedFingerprinter.csv
## Fingerprinter.csv
## GraphOnlyFingerprinter.csv
## KlekotaRothFingerprintCount.csv
## KlekotaRothFingerprinter.csv
## MACCSFingerprinter.csv
## PubchemFingerprinter.csv
## SubstructureFingerprintCount.csv
## SubstructureFingerprinter.csv
df1 = read.csv("AtomPairs2DFingerprintCount.csv", header = TRUE)
df = df1[,-1]
data = Filter(function(x) sd(x) > 0.1, df)
datadecoy = read.csv("AtomPairs2DFingerprintCountdecoy.csv", header = TRUE)
decoy1 <- datadecoy[,-1]
Decoy = Filter(function(x) sd(x) > 0.1, df)
Pos = subset(data, Activity == 'active')
Neg = subset(data, Activity == 'inactive')
Dneg = subset(Decoy, Activity == 'inactive')

nPos = nrow(Pos)
nNeg = nrow(Neg)
nDneg = nrow(Dneg)
m= 100
ACCts  <- matrix(nrow = m, ncol = 1)
error  <- matrix(nrow = 10, ncol = 1)

for (i in 1:m){
  #######  Dividing Training and Testing sets on positive and negative classes
  sample <- c(sample(1:nrow(Neg) ,32))
  BNeg <- Neg[sample,]
  nBNeg = nrow(BNeg)
  sample1 <- c(sample(1:nPos,24))
  sample2 <- c(sample(1:nBNeg,24 ))
  train1  <- Pos[sample1,] ####Positive set for training
  train2  <- BNeg[sample2,] ####Negative set for training
  internal <- rbind(train1,train2) ####combining for internal set
  external <- Decoy    ####combining for external set
  
  
  ######### Optimized parameter
  model <- tuneRF(internal[,-ncol(internal)], internal[,ncol(internal)], stepFactor=1.5)
  index <- c(100,200,300,400,500,600,700,800,900,1000)
  for(p in 1:length(index)){
    ntree <- randomForest(Activity ~ ., internal, ntree= index[p],mtry = model[order(model[,2]),][1],orm.votes=TRUE,keep.forest=TRUE, importance=TRUE)
    error[p,] <- sum(ntree $ confusion[,3])
  }
  ntr = cbind(c(1:10),error)
  ntr2 = ntr[order(ntr[,2]),][1]
  
  ################### External validation
  RF = randomForest(Activity ~ ., internal, ntree= index[ntr2],mtry = model[order(model[,2]),][1],orm.votes=TRUE,keep.forest=TRUE, importance=TRUE) ## Building RF on internal with the optimized parameter
  Resultext = table(external$Activity, predict(RF, external))  ###### Prediction on external set
  
  
  ################### Performance report
  data = Resultext
  ACCts[i,] = (data[1]+data[4])/(data[1]+data[2]+data[3]+data[4])*100
}
result = data.frame(ACCts)

write.csv(result, "Decoy_AtomPairs2DFingerprintCount.csv", row.names=TRUE, na="")
