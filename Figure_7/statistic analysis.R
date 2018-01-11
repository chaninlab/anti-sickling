df = read.csv("feature_SubstructureFingerprintCount.csv", header = TRUE)
data = df[,-1]
dataRac = data[,-ncol(data)]
newclass = as.numeric(data[,ncol(data)])
datanew = data.frame(data[,-ncol(data)], Activity = newclass)
active = subset(datanew, Activity == '1')
inactive = subset(datanew, Activity == '2')
m = ncol(data)-1
KS_test <- matrix(nrow = m, ncol = 1)
name = data.frame(names(dataRac))

for (i in 1:m){
  KS_test[i, ] <- ks.test(active[,i],inactive[,i])$p.value
}
result_KS_test = cbind(name, KS_test)
result_KS_test

T_test <- matrix(nrow =m, ncol = 1)

for (i in 1:m){
  T_test[i, ] <- t.test(active[,i],inactive[,i])$p.value
}


wilcox <- matrix(nrow =m, ncol = 1)
for (i in 1:m){
  wilcox[i,]  <- wilcox.test(active[,i],inactive[,i])$p.value
}
result_W_test = cbind(name,wilcox)
result_W_test 
