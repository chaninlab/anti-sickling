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

########## building model
df1 = read.csv("AtomPairs2DFingerprinter.csv", header = TRUE)
df = df1[,-1]
data = Filter(function(x) sd(x) > 0.1, df)
Pos = subset(data, Activity == 'active')
Neg = subset(data, Activity == 'inactive')

nPos = nrow(Pos)
nNeg = nrow(Neg)

m= 100
ACCtr  <- matrix(nrow = m, ncol = 1)
SENStr  <- matrix(nrow = m, ncol = 1)
SPECtr  <- matrix(nrow = m, ncol = 1)
MCCtr  <- matrix(nrow = m, ncol = 1)
ACC5cv  <- matrix(nrow = m, ncol = 1)
SENS5cv  <- matrix(nrow = m, ncol = 1)
SPEC5cv  <- matrix(nrow = m, ncol = 1)
MCC5cv  <- matrix(nrow = m, ncol = 1)
ACCts  <- matrix(nrow = m, ncol = 1)
SENSts  <- matrix(nrow = m, ncol = 1)
SPECts  <- matrix(nrow = m, ncol = 1)
MCCts  <- matrix(nrow = m, ncol = 1)
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
  test1 <-   Pos[-sample1,]    ####Positive set for testing
  test2 <-   BNeg[-sample2,]    ####Negative set for testing 
  internal <- rbind(train1,train2) ####combining for internal set
  external <- rbind(test1,test2)    ####combining for external set
  
  ######### Optimized parameter
  customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
  customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
  customRF$grid <- function(x, y, len = NULL, search = "grid") {}
  customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
  }
  customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, newdata)
  customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, newdata, type = "prob")
  customRF$sort <- function(x) x[order(x[,1]),]
  customRF$levels <- function(x) x$classes
  
  tunegrid <- expand.grid(.mtry=c(1:10), .ntree=seq(100,1000,100))
  RFmodel <- train(Activity~., data=internal , method=customRF, metric=c("Accuracy"), tuneGrid=tunegrid, trControl=trainControl(method= "repeatedcv", number=5,repeats=3))
  Resultcv <- RFmodel$ finalModel $ confusion[1:2,1:2]
  
  ################### Performance report
  data = Resultcv
  ACC5cv[i,] = (data[1]+data[4])/(data[1]+data[2]+data[3]+data[4])*100
  SENS5cv[i,]  =  (data[1]/(data[1]+data[2]))*100
  SPEC5cv[i,] = (data[4])/(data[3]+data[4])*100
  MCC1      = (data[1]*data[4]) - (data[2]*data[3])
  MCC2      =  (data[4]+data[2])*(data[4]+data[3])*(data[1]+data[2])*(data[1]+data[3])	
  MCC3	=  sqrt(MCC2)
  MCC5cv[i,]  = MCC1/MCC3
}
result = data.frame(ACC5cv,SENS5cv,SPEC5cv,MCC5cv)

write.csv(result, "RF_AtomPairs2DFingerprinter.csv", row.names=TRUE, na="")
