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
library(prospectr)

df1 = read.csv("SubstructureFingerprintCount.csv", header = TRUE)
df = df1[,-1]
data = Filter(function(x) sd(x) > 0.1, df)
datanew_comp = read.csv("new_compounds.csv", header = TRUE)
new_comp <- datanew_comp[,-1]
Pos = subset(data, Activity == 'active')
Neg = subset(data, Activity == 'inactive')
active_ken <- Pos[, 1:ncol(Pos)-1]
inactive_ken <- Neg[, 1:ncol(Neg)-1]
DPos = subset(new_comp, Activity == 'Active')

nDPos = nrow(DPos)
m=1
ACCts  <- matrix(nrow = 1, ncol = 1)
error  <- matrix(nrow = 10, ncol = 1)

for (i in 1:m){
  #######  Dividing Training and Testing sets on positive and negative classes
  sample <- kenStone(inactive_ken, k = 32, metric = "mahal", pc = 2 )
  Binactive <- Neg[sample$model, ]
  Binactive_ken <- Binactive[, 1:ncol(Binactive)-1]
  sample1 <- kenStone(active_ken, k = 24, metric = "mahal", pc = 2 )
  sample2 <- kenStone(Binactive_ken, k = 24, metric = "mahal", pc = 2 )
  train1 <- Pos[sample1$model, ]
  train2 <- Neg[sample2$model, ]
  internal <- rbind(train1,train2) ####combining for internal set
  external <- new_comp    ####combining for external set
  
  
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

external[ ,(ncol(external)+1)] <- predict(RF, external)

result = data.frame(ACCts)

